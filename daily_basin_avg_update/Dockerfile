FROM rocker/r-ver:4.2.2

RUN mkdir -p /home
RUN mkdir -p /home/code
RUN mkdir -p /home/gis

WORKDIR /home

COPY gis/ /home/gis/
COPY code/.Renviron /home
COPY code/install_packages.R /home/code/install_packages.R
COPY code/daily_basin_avg_update.R /home/code/daily_basin_avg_update.R

CMD ["Rscript", "/home/code/daily_basin_avg_update.R"]