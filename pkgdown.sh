#!/bin/bash

# install necessary packages
Rscript -e 'install.packages("pkgdown")'

# generate package documentation
Rscript -e 'pkgdown::build_site()'
