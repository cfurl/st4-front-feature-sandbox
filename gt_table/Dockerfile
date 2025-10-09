FROM rocker/r-ver:4.2.2

# --- System deps (Chrome, fonts, image + geospatial libs, text shaping) ---
RUN apt-get update && apt-get install -y --no-install-recommends \
    wget gnupg ca-certificates \
    fonts-dejavu fonts-liberation \
    libnss3 libx11-6 libxcomposite1 libxdamage1 libxext6 libxfixes3 libxrandr2 \
    libxkbcommon0 libgbm1 libasound2 libatk1.0-0 libatk-bridge2.0-0 \
    libcairo2 libpangocairo-1.0-0 libpango-1.0-0 libgtk-3-0 libcups2 \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libgdal-dev gdal-bin libgeos-dev libproj-dev proj-bin libudunits2-dev \
    libmagick++-dev imagemagick \
    libfreetype6-dev libpng-dev libjpeg-dev libharfbuzz-dev libfribidi-dev \
 && rm -rf /var/lib/apt/lists/*

# --- Google Chrome (headless) repo & install ---
RUN wget -qO- https://dl.google.com/linux/linux_signing_key.pub \
      | gpg --dearmor -o /usr/share/keyrings/google-linux-signing-keyring.gpg \
 && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-linux-signing-keyring.gpg] http://dl.google.com/linux/chrome/deb/ stable main" \
      > /etc/apt/sources.list.d/google-chrome.list \
 && apt-get update \
 && apt-get install -y --no-install-recommends google-chrome-stable \
 && rm -rf /var/lib/apt/lists/*

# Let chromote/webshot2 find Chrome
ENV CHROMOTE_CHROME=/usr/bin/google-chrome

RUN mkdir -p /home
RUN mkdir -p /home/code
RUN mkdir -p /home/gis
RUN mkdir -p /home/data
RUN mkdir -p /home/rosm.cache/cartolight

WORKDIR /home

# copy <host machine> to <docker image>

COPY rosm.cache/cartolight/ /home/rosm.cache/cartolight/  
COPY gis/ /home/gis/ 

#COPY code/.Renviron /home
COPY code/install_packages.R /home/code/install_packages.R
COPY code/make_gt.R /home/code/make_gt.R
COPY code/map_plus_gt.R /home/code/map_plus_gt.R

RUN Rscript /home/code/install_packages.R
# RUN Rscript /home/code/make_gt.R - you need this at runtime not build

# CMD ["Rscript", "/home/code/map_plus_gt.R"] 
# --- Default runtime: render GT, then render/composite map ---
CMD ["bash","-lc","Rscript /home/code/make_gt.R && Rscript /home/code/map_plus_gt.R"]