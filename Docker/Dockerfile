FROM rocker/shiny
# Download and install library
RUN apt-get update &&\
    sudo apt install -y libssl-dev \
                        libcurl4-openssl-dev \
                        libudunits2-dev \
                        gdal-bin \
                        libgdal-dev \
                        libcairo2-dev \
                        libfontconfig1-dev \
                        libproj-dev \
                        git \
                        libgeos-dev &&\
    mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install packages

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('hadley/devtools')"
RUN R -e "install.packages('leaftlet')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('mapview')"
RUN R -e "install.packages('shiny')"
RUN R -e "remotes::install_github('r-spatial/mapview')"
RUN R -e "install.packages(c('shinydashboard', 'shinyjs', 'V8'))"
RUN cd /root && git clone https://github.com/class-euproject/pollution-visualization
WORKDIR /root/pollution-visualization/R
CMD ["Rscript","dashboard-Modena.r"]

