# CV - RChaput

> Author: rchaput

## Description

This is the source code for my CVs. They use [rmarkdown] and [pagedown]
so that both an HTML and PDF version can be produced automatically from
the same data.


## How to build

Install `R`, then launch a R console to install the dependencies:

```r
install.packages("remotes")
remotes::install_deps()
```

(You can also install all dependencies listed in the [DESCRIPTION](./DESCRIPTION) file) manually.)

You must then install the icons packs:

```r
icons::download_fontawesome()
icons::download_simple_icons()
```

Then, use `make` to build the documents by using one of the following commands,
depending on the document you want to build:

- `make results/industry/cv.html`
- `make results/industry/cv.pdf`
- `make results/research/cv.html`
- `make results/research/cv.pdf`
- `make all`

The `make all` rule builds all 4 others.

You can use `make clean` to delete the rendered documents.


[rmarkdown]: https://rmarkdown.rstudio.com/
[pagedown]: https://pagedown.rbind.io/
