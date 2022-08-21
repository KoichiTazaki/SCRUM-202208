#!/bin/sh
docker run -v /home/ubuntu/work/OASIS/SCRUM-202208:/home/rstudio/projects/SCRUM-202208 \
	--rm \
	-p 8787:8787 \
	-e PASSWORD=1234 \
	tazakfvw/rss

