default: README.md

README.md: vignettes/rgraph6.Rmd
		cp $< $(@:.md=.Rmd)
		mkdir -p tools
		Rscript -e "rmarkdown::render('$(@:.md=.Rmd)', output_format='github_document', output_file='$@', params=list(figpath='tools/rgraph6-'))"
		rm $(@:.md=.Rmd)

.PHONY: default
