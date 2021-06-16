
FROM jupyter/r-notebook
USER root
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
USER $NB_USER
RUN conda install -v --yes \
    'r-mapview' \
    'r-leaflet'  \
    'r-sf'  \
    'r-readr' && \
    conda clean --all -f -y && \
    fix-permissions "${CONDA_DIR}" && \
    fix-permissions "/home/${NB_USER}"

    
# RUN R -e "install.packages('devtools',repos = 'http://cran.us.r-project.org')"
# RUN R -e "devtools::install_github('hadley/devtools')"

# RUN R -e "install.packages('sf', configure.args = '--with-proj-lib=/usr/local/lib/',repos = 'http://cr$
# RUN R -e "install.packages('mapview',repos = 'http://cran.us.r-project.org')"
# #RUN R -e "remotes::install_github('r-spatial/mapview')"
# RUN R -e "install.packages('leaftlet',repos = 'http://cran.us.r-project.org')"
# RUN R -e "install.packages('readr',repos = 'http://cran.us.r-project.org')"
# RUN R -e "install.packages('units',repos = 'http://cran.us.r-project.org')"
# RUN R -e "install.packages(c('shinydashboard', 'shinyjs', 'V8'),repos = 'http://cran.us.r-project.org'$
#RUN cd /root && git clone https://github.com/class-euproject/pollution-visualization
#WORKDIR /root/pollution-visualization/R
#CMD ["Rscript","dashboard-Modena.r"]

