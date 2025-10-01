
PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`


all: document dependencies install

dependencies: 
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_github("protected-species-management/life-history-module/lhm")' \
	-e 'remotes::install_github("protected-species-management/empirical-data-module/edm")' \
	-e 'remotes::install_deps(dependencies = TRUE)' 

document:
	Rscript \
	-e 'if (!requireNamespace("roxygen2")) install.packages("roxygen2")' \
	-e 'roxygen2::roxygenize()'

install: 
	R CMD build .
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz
	rm $(PKGNAME)_$(PKGVERS).tar.gz
