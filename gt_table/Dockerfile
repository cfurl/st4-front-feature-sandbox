FROM rocker/r-ver:4.2.2

ENV DEBIAN_FRONTEND=noninteractive
# Tame threading to avoid IM/BLAS stack-smashers
ENV MAGICK_THREAD_LIMIT=1 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1

# --- System deps: image/geo/text + headers ---
RUN apt-get update && apt-get install -y --no-install-recommends \
    wget gnupg ca-certificates \
    # fonts + X bits some libs expect
    fonts-dejavu fonts-liberation \
    libnss3 libx11-6 libxcomposite1 libxdamage1 libxext6 libxfixes3 libxrandr2 \
    libxkbcommon0 libgbm1 libasound2 libatk1.0-0 libatk-bridge2.0-0 \
    libcups2 \
    # curl/ssl/xml for R pkgs
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    # geospatial (sf)
    gdal-bin libgdal-dev libgeos-dev libproj-dev proj-bin libudunits2-dev \
    # ImageMagick + headers
    imagemagick libmagick++-dev libmagickwand-dev \
    # cairo/pango + dev for text shaping
    libcairo2 libcairo2-dev libpango-1.0-0 libpango1.0-dev libpangocairo-1.0-0 \
    # raster/graphics dev headers
    libfontconfig1 libfontconfig1-dev \
    libfreetype6-dev libpng-dev libjpeg-dev libtiff5-dev \
    libharfbuzz-dev libfribidi-dev \
    librsvg2-2 librsvg2-dev \
    pkg-config \
 && rm -rf /var/lib/apt/lists/*

# --- Google Chrome (headless) for webshot/chromote ---
RUN wget -qO- https://dl.google.com/linux/linux_signing_key.pub \
      | gpg --dearmor -o /usr/share/keyrings/google-linux-signing-keyring.gpg \
 && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-linux-signing-keyring.gpg] http://dl.google.com/linux/chrome/deb/ stable main" \
      > /etc/apt/sources.list.d/google-chrome.list \
 && apt-get update && apt-get install -y --no-install-recommends google-chrome-stable \
 && rm -rf /var/lib/apt/lists/*
ENV CHROMOTE_CHROME=/usr/bin/google-chrome

# Optional: disable flaky HEIC delegate in IM6
RUN sed -i 's/<policy domain="coder" rights="read|write" pattern="HEIC"/<policy domain="coder" rights="none" pattern="HEIC"/' /etc/ImageMagick-6/policy.xml || true

# --- Build critical R pkgs *after* system libs are present ---
# Link magick against the IM we just installed
RUN R -q -e "install.packages('magick', type='source', repos='https://cloud.r-project.org')"
# Text + graphics stack compiled correctly; sf links to system GDAL/GEOS/PROJ
RUN R -q -e "install.packages(c('ragg','systemfonts','textshaping','sf'), repos='https://cloud.r-project.org')"

# Smoke test so CI fails early if IM/magick mismatch
RUN R -q -e "library(magick); print(magick::magick_config()); image_blank(100,100) |> image_annotate('ok', size=12) -> x; print(x)"

# --- Your app layout ---
WORKDIR /home
RUN mkdir -p /home/code /home/gis /home/data /home/rosm.cache/cartolight

COPY rosm.cache/cartolight/ /home/rosm.cache/cartolight/
COPY gis/ /home/gis/
COPY code/install_packages.R /home/code/install_packages.R
COPY code/make_gt.R /home/code/make_gt.R
COPY code/map_plus_gt.R /home/code/map_plus_gt.R

# Install the rest of your R deps (project-specific)
RUN Rscript /home/code/install_packages.R

# Runtime: render GT, then render/composite map
CMD ["bash","-lc","Rscript /home/code/make_gt.R && Rscript /home/code/map_plus_gt.R"]