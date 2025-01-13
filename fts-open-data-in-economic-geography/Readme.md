# Paper about FTS data application for human geography

The paper about an algorithm and a tool for working with a few FTS open datasets in socio-economic geographical analysis was published in “Географический вестник = Geographical bulletin”. The publication file is `Paper.pdf`. A Bibtex entry is below.

```
@article{Syomin_fts_open_data_2024,
    address={Moscow},
    title = {Application of {Tax} {Service} {Open} {Data} for {Analysis} in {Economic} {Geography}},
    abstract = {This paper presents a methodology for the creation of a geocoded tabular dataset of small and medium-sized enterprises (SMEs) in Russia based on open data provided by the Federal Tax Service (FTS) of Russia. The resulting dataset encompasses the entire territory of the country. The data is provided at the level of individual SMEs. The dataset is structured as a CSV file comprising the following fields: tax number, registration number, legal status (juridical person, sole trader, head of peasant (farm) enterprise), SME category (microbusiness, small-sized business, medium-sized business), name, address (region, district, city, settlement), main activity code according to NACE, income, expenses, and average list number of employees. The dataset includes revenue, expenses, and employee data from 2018 onwards, with yearly granularity, and all other variables from August 2016 onwards, with monthly granularity. The article presents a reproducible methodology for the processing of raw FTS data and illustrates its application in the generation and exploratory data analysis of a dataset comprising firms in the agriculture, forestry and fisheries sectors. A reference implementation of the described technology is provided in the form of an open-source Python command-line tool. The paper demonstrates that the proposed technique enables the utilisation of FTS open data to address a range of analytical and academic tasks in the field of economic geography, particularly those benefiting from disaggregated information or requiring spatial resolution at the settlement level. Furthermore, the incorporation of geographic coordinates into the dataset facilitates direct mapping without the necessity for additional processing. The inclusion of municipal codes allows for the seamless integration with official statistical information.},
    language = {ru},
    journal = {Geographical Bulletin},
    author = {Syomin, Pavel Olegovich},
    year = {2024},
    issue={4 (71)},
    doi={10.17072/2079-7877-2024-4-54-66},
    pages = {54--66}
}
```
