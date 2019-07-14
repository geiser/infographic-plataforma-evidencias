FROM geiser/r-shiny:v0.09 AS prod
MAINTAINER Geiser Chalco <geiser@usp.br>

LABEL org.label-schema.license="GPL-3.0" \
      org.label-schema.vcs-url="https://github.com/geiser/docker" \
      org.label-schema.vendor="Infographic for Evidence-based Assessement Platform through R-shiny" \
      maintainer="Geiser Chalco <geiser@usp.br>"

# Install R packages
RUN install2.r -s pivottabler \
	&& install2.r -s readr

