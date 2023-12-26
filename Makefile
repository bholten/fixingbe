RMD_FILES := $(wildcard *.Rmd)
HTML_FILES := $(RMD_FILES:.Rmd=.html)

all: $(HTML_FILES)

%.html: %.Rmd install_packages.R
	Rscript install_packages.R
	Rscript -e "rmarkdown::render('$<', output_format = 'html_document', output_dir = 'html')"

clean:
	rm -f $(HTML_FILES)
