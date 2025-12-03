FROM rocker/r-ver:4.2.2

RUN mkdir -p /home
RUN mkdir -p /home/code
RUN mkdir -p /home/data

WORKDIR /home

COPY data/basin_stats_month_day_combo_2002_2024.csv /home/data/
COPY code/install_packages.R /home/code/install_packages.R
COPY code/write_daily_data_and_publish_hyetograph.R /home/code/write_daily_data_and_publish_hyetograph.R

RUN Rscript /home/code/install_packages.R

CMD ["Rscript", "/home/code/write_daily_data_and_publish_hyetograph.R"]