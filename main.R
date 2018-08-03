library(tidyverse)
library(yaml)
library(mongolite)
library(cli)
library(plumber)

# Helper Function ---------------------------------------------------------

est_mongo_conn <- function(conn) {
  
  # make sure file exists
  if (!file.exists("dbconfig.yaml")) {
    stop("Database credential not found.")
  }
  
  # read config
  d <- yaml::read_yaml("dbconfig.yaml")
  
  # check if db is specified
  if( "db" %in% names(d[[conn]]) ){
    db = d[[conn]][['db']]
  }else{
    db = conn
  }
  
  # check if the credentials are specified
  if(!all(c("host", "port", "collection") %in% names(d[[conn]]))){
    stop("One or more database parameters is incorrect.")
  }
  
  # est conn
  c <- mongo(
    collection = d[[conn]][["collection"]],
    url = with(d[[conn]], 
               # mongodb://username:password@host:port
               sprintf("mongodb://%s:%s@%s:%d/", user, password, host, port)),
    db = d[[conn]][["db"]]
  )
  
  # return connection
  c
  
}


# Main --------------------------------------------------------------------

do_validate <- function(df) {

  wrt <- est_mongo_conn("Write")
  
  # extract coefficients
  coefs <- lm(ts ~ x, data = df)$coefficients %>% round(digits = 5)
  
  # save result to db
  list(
    timestamp = Sys.time(),
    intercept = coefs[["(Intercept)"]],
    slope = coefs[["x"]]
  ) %>% wrt$insert()
  
  
  cat_boxx("*** ALL TEST PASSED ***", col = "green")
  
}

make_plot <- function(df) {
  
  df %>% 
    bind_rows() %>% 
    mutate(ts = as.numeric(ts)) %>% 
    ggplot(aes(x, ts)) +
    geom_line(col = "salmon") + 
    labs(x = "N", y = "Time Spent")
  
}

# GET -----------------------------------------------------------------


#* @get /status
function(req) {
  
  cat_rule("Receive trigger. Prepare to validate.")
  
  # establish a db connection
  rd <- est_mongo_conn("Read")
  
  # extract all data
  dat <- rd$find()
  
  # test: if all status = true
  if(all(dat$status)) {
    
    do_validate(dat)
    # return simple feedback
    list(
      status = 1,
      remark = "All passed"
    )
    
  }else {
    
    msg = "Not all results return true. Please check."
    cat_boxx(msg)
    
    list(
      status = 0,
      remark = msg
    )
  }
  
}


#' @png (width = 600, height = 400)
#* @get /graph
function(req) {
  
  cat_rule("Receive trigger. Ready to process result.")
  
  # establish a db connection
  rd <- est_mongo_conn("Read")
  
  # extract all data
  dat <- rd$find()
  
  # test: if all status = true
  if(all(dat$status)) {
    
    do_validate(dat)
    # return plot
    make_plot(dat) %>% print()
    
  }else {
    
    # report 
    cat_boxx("Not all results return true. Please check.", col = "red")
    stop("Require further attention.")
    
  }
  
}



