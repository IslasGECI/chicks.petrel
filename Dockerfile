FROM islasgeci/base:1.0.0
COPY . /workdir
RUN R -e "install.packages(c('comprehenr', 'multimark', 'optparse'), repos='http://cran.rstudio.com')"
RUN R -e "remotes::install_github('IslasGECI/optparse', ref='latest')"
RUN R -e "remotes::install_github('IslasGECI/testtools')"
RUN R -e "remotes::install_github('mikemeredith/IPMbook', build_vignettes=FALSE, upgrade = 'always')"
