all:
	Rscript -e "rmarkdown::render('appendix.Rmd')"
	mkdir -p site
	mv appendix.html ./site/index.html
