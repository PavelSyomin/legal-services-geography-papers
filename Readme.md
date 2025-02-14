# Papers (mainly) on the geography of legal services in Russia

## Summary

This repository contains the source code for a number of academic papers. All but two deal with different aspects of the geography of legal services in Russia. The remaining two papers focus on relevant data processing issues. Some are conference papers, the others are journal articles. Some are in English, others in Russian, and the one paper is in both languages. All the papers accompany my PhD thesis on the geography of legal services in Russia. The thesis has not yet been defended, but almost all the papers have already been published.

## Papers details

In the following tables, the links point to the respective home folders of the papers with the paper code, text, published PDF (if available), and citation information. Papers are sorted by conference/publication date. An unpublished paper is not listed.

Some journal articles were first presented at conferences and then revised and published in the journals without separate publication in the conference proceedings. In this case, they are placed in the “Journal articles” section without a record in “Conference papers”, and the relevant conference details can be found on the paper page.

*InterCatro. InterGIS* is a conference proceedings, but is usually referred to as a journal, so the paper is placed in the “Journal articles” section.

### Conference papers

| Paper title | Conference information | Text language |
| -- | -- | -- |
| [Migration of Russian Legal Companies in 2016–23](legal-companies-migration) | First Belarus Geographical Congress (Belarus, Minsk, April 9–13, 2024) | English |
| [Legal Services Market in Russia: Regional Specialization](legal-companies-regional-specialization) | Geographical problems of development of countries and regions (Russia, Stavropol, April 17–19, 2024) | English |
| [The “Rank-Size” Rule in the Distribution of Russian Cities by the Count of Legal Companies and their Employees](legal-companies-zipf-law) | 10th Maksakov's Readings (Russia, Moscow, May 16–17, 2024) | Russian |
| [A Python CLI application to generate a geo-referenced dataset of small and medium-sized businesses in Russia based on Federal Tax Service open data](fts-open-data-cli-app) | XV Scientific Assembly of the Association of Russian Social Geographers (ARGO) (Russia, Krasnodar, September 29 – October 8, 2024) | Russian |

### Journal articles

| Paper title | Journal name | Text language |
| -- | -- | -- |
| [Relocation of Russian Legal Companies in 2016–23](legal-companies-migration) | Regional geosystems | Russian |
| [Mapping Russian Small and Medium-Sized Businesses Using Tax Service Open Data](law-firms-mapping) | Pskov State University Herald (Natural and Physics&Math Sciences series) | English, Russian |
| [Application of Tax Service Open Data for Analysis in Economic Geography](fts-open-data-in-economic-geography) | Geographical Bulletin | Russian |
| [Legal Business Geography in Russia: Regional Analysis and Mapping Using Tax Service Open Data](law-firms-geography) | InterCarto. InterGIS | English |
| [Spatial and regression analysis of provision of commercial legal services in Russian cities](legal-services-provision) | Journal of Geography and Environmental Management | English |

Paper's home folders have a consistent structure with a similar naming of subfolders and files.

1. `assets` — auxiliary data files used by the code to build the paper.
2. `helpers` — one or more R scripts containing the main data analysis code.
3. `Paper.Rmd` — the main source file for the paper text.
4. `Paper.pdf` — file of the published paper (if avaialble).
5. `renv`, `renv.lock`, `.Rpofile` — utilites for reproducibility.
6. `*.Rproj` — R project file to open with RStudio.
7. `Readme.md` — paper title, abstract, and sometimes additional notes such as conference name.
8. `citation.bib` — citation information in BibTeX.
9. `Slides.qmd` — optional source file for conference slides, if the paper was presented at the conference.
10. `Paper_*.Rmd` — optional source files for the paper text or metadata (author information and references) in a language other that the main language of the paper.

If a folder contains both a conference paper and a journal article, there are three subfolders: `common`, `conference-paper`, `journal-article`. Their names should be self-explanatory. The internal structure of the `conference-paper` and `journal-article` folders is similar to that outlined above, except that `renv` utilites, project file, and readme are located in the home folder of the paper.

## Reproducibility

All papers are designed to follow the principles of open science and reproducible research. In particular, they are based on open data (mainly [open administrative data published by the Federal Tax Service of Russia](https://www.nalog.gov.ru/opendata/)), written in [RMarkdown](https://rmarkdown.rstudio.com/), and use [renv](https://rstudio.github.io/renv/index.html) to manage information about R packages and environment. All the code used to generate the papers is available in this repository, and the auxiliary datasets are also published. I hope that the steps outlined below will be sufficient to fully reproduce the main results of the research as well as the full text of the papers.

### Getting the data

All the basic datasets, as well as the auxiliary data used by the code of the papers, are stored either in the root folder `datasets` or in `assets` or `common` subfolders inside a paper's home folder. Cloning this repository is sufficient to obtain all the data required to reproduce the paper.

### Building the manuscript

1. Install git, [R](https://www.r-project.org/) 4.0 or higher, [RStudio](https://posit.co/download/rstudio-desktop/). Also install [Quarto](https://quarto.org/) if you want to render conference slides.
2. Clone this repository.
3. Navigate to the home folder of the paper of interest and open its project file.
4. If prompted, install `renv` and any packages it suggests.
5. Open a `Paper.Rmd` and click the Knit button.

### Troubleshooting

1. If `renv` is not installed and does not attempt to install itself automatically, install it with `install.packages("renv")`.
2. If `renv` does not attempt to install the required packages automatically, call `renv::restore()` after opening the project.
3. If some of the required packages fail to install, try to read the error messages and fix the problem. For example, the `sf` package requires some GDAL development libraries to be installed on your system. See [sf package installation notes](https://r-spatial.github.io/sf/#installing) for details.
