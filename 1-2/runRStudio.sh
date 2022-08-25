#!/bin/sh
docker run -v /home/tazakfvw/work/20220825:/home/rstudio/projects/SCRUM12 \
	--rm \
	-p 8765:8787 \
	-e PASSWORD=1234 \
	rocker/tidyverse
