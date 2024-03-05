# PiCo contactsurvey

This repository contains the code developed and used for the analysis of the PIENTER3 and PiCo contactsurveys study in the Netherlands, as described in ["Contact behaviour before, during and after the COVID-19 pandemic in the Netherlands: evidence from contact surveys in 2016-2017 and 2020-2023"](https://www.medrxiv.org/content/10.1101/??), by [Jantien A Backer](mailto:jantien.backer@rivm.nl), Eric Vos, Gerco den Hartog, Cheyenne van Hagen, Hester de Melker, Fiona van der Klis and Jacco Wallinga. 

The repository consists of:

* `scripts`: code for two scripts (data cleaning and analysis)

* `R`: functions used in analysis script

* `figures`: figures produced by analysis script

All code has been written in the programming language [R](https://www.r-project.org/about.html); see [Requirements](#requirements) for detailed specification.

## Data

The raw data used in the data cleaning script is not publicly available due to GDPR constraints.

All data used for the analysis is publicly available and automatically downloaded in the analyis script:

* Cleaned data from the [Zenodo repository](https://dx.doi.org/10.5281/zenodo.10370353) in the [socialcontactdata.org](https://www.socialcontactdata.org) format.

* Population distribution by age and sex in the Netherlands at the time of the survey rounds from [CBS](https://opendata.cbs.nl/#/CBS/nl/dataset/83482NED/table?dl=98643) (Statistics Netherlands)

* Population distribution by education level, age group and sex in the Netherlands in 2021 from  [CBS](https://opendata.cbs.nl/statline/#/CBS/nl/dataset/85184NED/table?dl=984E1) (Statistics Netherlands)


## Usage

`scripts` contains the scripts for data cleaning and analysis, and `R` contains functions that are loaded at the start of the analysis script. Each script consists of several numbered R files that reproduce the results when executed in order, as in `00_Main.R`. While running the analysis script an additional `results` folder will be created to store results in `.rds` format.

Note that the data cleaning script can't be run for lack of raw data.


## <a name = "requirements"></a> Requirements

The code has been developed and runs under the RIVM R-Studio servers.

```
R version 4.2.3 (2023-10-31) Eye Holes
Platform: x86_64-redhat-linux-gnu (64-bit)
Running under: Red Hat Cloud Infrastructure
```

Next to the R base packages, the following packages(_versions) need to be installed

```
cbsodataR_1.0.1
RColorBrewer_1.1-3
ISOweek_0.6-2
patchwork_1.2.0
cowplot_1.1.2
lubridate_1.9.3
forcats_1.0.0
stringr_1.5.1
dplyr_1.1.2
purrr_1.0.2
readr_2.1.5
tidyr_1.3.0       
tibble_3.2.1
ggplot2_3.4.4
tidyverse_2.0.0 
```

## Funding

This study was funded by the Ministry of Health, Welfare and Sport (VWS) in the Netherlands. The authors of this study (JB and JW) received funding from European Union’s Horizon 2020 research and innovation programme - projects EpiPose (Grant agreement number 101003688) and ESCAPE (Grant agreement number 101095619).

## Feedback

If you encounter a clear bug, please file an issue with a minimal reproducible example on GitHub.

