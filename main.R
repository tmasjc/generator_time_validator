library(tidyverse)
library(yaml)
library(mongolite)
library(cli)

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


# establish a db connection
rd <- est_mongo_conn("Read")
wrt <- est_mongo_conn("Write")

# extract all
dat <- rd$find()

# test: if all status = true
if(all(dat$status)) {
  
  # extract coefficients
  coefs <- lm(ts ~ x, data = dat)$coefficients %>% round(digits = 5)
  
  # save result to db
  list(
    timestamp = Sys.time(),
    intercept = coefs[["(Intercept)"]],
    slope = coefs[["x"]]
  ) %>% wrt$insert()
  
  
  cat_boxx("All test passed.", col = "green")
  
}else {
  
  # report 
  cat_boxx("Not all results return true. Please check.", col = "red")
  stop("Require further attention.")
  
}




