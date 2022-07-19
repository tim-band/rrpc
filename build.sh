#!/bin/sh
# Build and install this R package
name=$(basename $(pwd))
Rscript -e "devtools::document()"
cd ..
package=$(R CMD build ${name} | grep -Po '(?<=^\* building .).*(?=.$)')
echo "Installing ${package}..."
Rscript -e "install.packages('${package}', repos=NULL)"
