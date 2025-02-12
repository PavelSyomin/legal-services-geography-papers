# Papers about legal services geography in Russia

## Summary

This repository contains source code for a number of papers based on open data about small and medium-sized enterprises in Russia. Some of them are conference papers, the other are journal articles. Some are in English, some are in Russian. Almost all the papers deals with the various aspects of the geography of legal services in Russia. All the papers accompany my PhD thesis on legal services geography in Russia. The thesis has not been defended yet, but almost all the papers have already been published.

## Papers details

In the following tables, links reference the particular folders with the paper's code, text, published PDF (if any), and citation info. Papers are sorted in the order of conference/publication date. Unpublished papers (currently, there is one unpublished paper) are not listed.

Some journal articles were first presented on conferences, and then enhanced and published in the journals without a separate publication in conference proceedings. In this case, they are placed in the “Journal articles” section without a record in “Conference papers”, and the relevant conference details can be found on the particular paper's page.

*InterCatro. InterGIS* is a conference proceedings, but usually it is referred to as a journal, thus the respective paper is placed in the “Journal articles”.

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

## Projects structure

Each paper (or a pair of a conference paper with a journal article) has its own project located in a separate folder. Folders share a common structure with a similar naming of subfolders and files.

1. `assets` — auxiliary data files used by the code to build the paper.
2. `helpers` — one or many R scripts with the main code for data analysis.
3. `Paper.Rmd` — the main source file of the paper.
4. `Paper.pdf` — file of the published paper.
5. `renv`, `renv.lock`, `.Rpofile` — helpers used primarily to facilitate reproducibility.
6. `*.Rproj` — R project file to be opened with RStudio.
7. `Readme.md` — general paper info.

If a folder contains both a conference paper and a journal article, it has three subfolders: `common`, `conference-paper`, `journal-article`. Their names are likely to be self-explanatory. The internal structure of `conference-paper` and `journal-article` folders is similar to those outlined above, expect for `renv`, project file, and readme that are situated in the main folder of the paper.

## Reproducibility

All the papers have been designed to follow the principles of open science and reproducible research. In particular, they are based on open data (primarily [open administrative data published by Federal Tax Service of Russia](https://www.nalog.gov.ru/opendata/)), written with [RMarkdown](https://rmarkdown.rstudio.com/), and use [renv](https://rstudio.github.io/renv/index.html) to manage information about R packages and environment. All the code used to generate the papers is available in this repository, and the auxiliary datasets are also published. I hope that the steps outlined below will be enough to fully reproduce the main results of the research as well as the entire texts of the papers.

### Getting the data

All the foundation datasets as well as auxiliary data used by the papers' code is stored either in the root `datasets` folder or in `assets` or `common` subfolders inside individual papers' folders. Cloning this repository is enough to obtain all the necessary data.

### Building the manuscript

1. Install R 4.0 or higher, RStudio.
2. Clone this repository.
3. Open a project of a paper of interest.
4. If prompted, install `renv` and all the packages suggested by `renv`.
5. Open a `*.Rmd` file (usually `Paper.Rmd`) and click the Knit button.

### Troubleshooting

1. If `renv` is not instelled and is not being installed automatically, install it with `install.packages("renv")`.
2. If `renv` does not attempt to automatically install the required packages, call `renv::restore()` after opening the project.
3. If some of the required packages fail to install, try reading the error message and fix the issue. For example, `sf` package required a few GDAL development libraries to be installed on your system. See [notes on sf package installation](https://r-spatial.github.io/sf/#installing) on various operating systems for details.


