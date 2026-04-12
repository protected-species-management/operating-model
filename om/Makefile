
PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`


all: documentation dependencies install

dependencies: 
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes", contriburl = "https://cran.ma.imperial.ac.uk/bin/windows/contrib/4.5/")' \
	-e 'remotes::install_deps(dependencies = TRUE)' 

documentation:
	Rscript \
	-e 'if (!requireNamespace("roxygen2")) install.packages("roxygen2")' \
	-e 'roxygen2::roxygenize()'

install: 
	R CMD build --no-build-vignettes .
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz
	rm $(PKGNAME)_$(PKGVERS).tar.gz
