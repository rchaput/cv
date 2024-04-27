SRC=contact_functions.R custom.css cv_functions.R dd_cv.css DESCRIPTION template.html data/**
OUT_DIR=results

all: ${OUT_DIR}/research/cv.html ${OUT_DIR}/research/cv.pdf ${OUT_DIR}/industry/cv.html ${OUT_DIR}/industry/cv.pdf


${OUT_DIR}/research/cv.html: cv_research.Rmd ${SRC}
	mkdir -p ${OUT_DIR}/research
	R -q -e 'rmarkdown::render("cv_research.Rmd", output_file="${OUT_DIR}/research/cv.html")'

${OUT_DIR}/research/cv.pdf: ${OUT_DIR}/research/cv.html
	R -q -e 'pagedown::chrome_print("${OUT_DIR}/research/cv.html", format="pdf")'


${OUT_DIR}/industry/cv.html: cv_industry.Rmd ${SRC}
	mkdir -p ${OUT_DIR}/industry
	R -q -e 'rmarkdown::render("cv_industry.Rmd", output_file="${OUT_DIR}/industry/cv.html")'

${OUT_DIR}/industry/cv.pdf: ${OUT_DIR}/industry/cv.html
	R -q -e 'pagedown::chrome_print("${OUT_DIR}/industry/cv.html", format="pdf")'


.PHONY: clean

clean:
	rm cv_research.html cv_research.pdf cv_industry.html cv_industry.pdf
