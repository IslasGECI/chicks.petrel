FROM islasgeci/base:1.0.0
COPY . /workdir
RUN apt-get update && apt-get install --yes \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    netcdf-bin
RUN R -e "install.packages(c('comprehenr', 'multimark', 'optparse'), repos='http://cran.rstudio.com')"
RUN R -e "remotes::install_github('IslasGECI/optparse', ref='latest')"
RUN R -e "remotes::install_github('IslasGECI/testtools')"
RUN R -e "remotes::install_github('mikemeredith/IPMbook', build_vignettes=FALSE, upgrade = 'always')"
