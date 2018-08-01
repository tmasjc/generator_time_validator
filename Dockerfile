FROM rocker/tidyverse

RUN apt-get update -y \
	&& apt-get -y --no-install-recommends install \
		libssl-dev \
		libsasl2-dev \
		libjpeg-dev \
	&& install2.r --error \
		--deps TRUE \
		-r "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"	\
		yaml \
		mongolite \
		cli \
		plumber

COPY . /validator

WORKDIR /validator
