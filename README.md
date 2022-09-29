# Replication package for “Uncovering the Dynamics of the Wealth Distribution” (Blanchet, 2022)

Overview
--------

The codes in this replication package replicate all the results in [Blanchet (2022)](https://thomasblanchet.github.io/wealth-tax/Blanchet2022_JMP.pdf), starting from the preprocessing of the raw data to the final results. All the data that can be redistributed and that does not exceed GitHub's size constraints is directly included in the package. The rest can be accessed relatively easily, as described in this README. Replicators should expect the entire code to run for several days, but many parts can be run independently.

The paper has a companion website (<https://thomasblanchet.github.io/wealth-tax/>) which can be used to apply the paper's formulas directly. The full text of the paper is accessible [here](https://thomasblanchet.github.io/wealth-tax/Blanchet2022_JMP.pdf).

Data Availability and Provenance Statements
-------------------------------------------

### Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. 

### License for Data

![Creative Commons Attribution 4.0 International Public License](https://img.shields.io/badge/License%20-CC%20BY%204.0-lightgrey.svg)

The data, tables and figures are licensed under the [Creative Commons Attribution 4.0 International (CC BY 4.0) license](https://creativecommons.org/licenses/by/4.0/). See [LICENSE.txt](LICENSE.txt) for details.

### Summary of Availability

- [ ] All data **are** publicly available.
- [x] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.

### Details on each Data Source

#### Distributional National Accounts (DINA) Data

The income and wealth data primarily comes from the distributional national accounts data [(Piketty, Saez and Zucman, 2018)](https://gabriel-zucman.eu/files/PSZ2018QJE.pdf) which have been updated by the same authors [(Saez and Zucman, 2020)](https://gabriel-zucman.eu/files/SaezZucman2020JEP.pdf). This data contains a macroeconomic part (included in the folder `raw-data/dina-macro`) and microdata part. The microdata is based on the public-use tax microdata, which can be purchased from the NBER but cannot be redistributed directly. A degraded version of these files is available at <https://gabriel-zucman.eu/usdina/>. While in principle this should provide similar results, there can be no guarantee.

#### Survey of Consumer Finances

The data from the Survey of Consumer Finances (SCF) is publicly available from the Federal Reserve. The data is automatically downloaded by the code into the folder `raw-data/scf`.

#### SCF+ 

The SCF+ data is an historical extension of the SCF created by [Kuhn, Schularick and Steins (2020)](https://www.journals.uchicago.edu/doi/abs/10.1086/708815) from archives. The data can be downloaded from the paper's supplemental material. It is included in the folder `raw-data/scf-plus`.

#### Survey of Income and Program Participation (SIPP)

The SIPP data is publicly available from the US Census Bureau. It is automatically downloaded by the code into the folder `raw-data/sipp`.

#### Panel Study of Income Dynamics

The PSID data is publicly available from <https://psidonline.isr.umich.edu/>. The relevant data extract is provided under `raw-data/psid`. This folder contains a small Stata code that extract the data into a format more easily readable using R.

#### Census microdata from (IPUMS USA)

The paper uses census microdata from [IPUMS USA](https://usa.ipums.org/usa/). The data is publicly available but cannot be redistributed directly for both size and licensing reasons.

#### Population Tables from the US Census Bureau

The population tables from the US Census Bureau are publicly available at <https://www.census.gov/programs-surveys/popest/data/tables.html> and are included in the folder `raw-data/uscb`.

#### Human Mortality Database

Historical life tables from the Human Mortality Database are publicly available from <https://mortality.org/> (free registration required). The relevant data extract in included in the folder `raw-data/hmd`.

#### UN World Population Prospects (2019)

The data from the UN World Population Prospects (2019) is publicly available from <https://population.un.org/wpp/>. The relevant files are included under `raw-data/wpp`.

#### Human Life Table Database

The Human Life Table Database is an extension of the Human Mortality Database which has more historical depth (but with lower quality). The data is publicly available at <https://www.lifetable.de/>, and included in the folder `raw-data/hltd`.

#### Haines (1998) Life Tables

The earliest life tables for the United States come from estimates by [Haines (1998)](https://www.tandfonline.com/doi/abs/10.1080/01615449809601197). The relevant data was digitized manually and is included in the folder `raw-data/haines`.

#### Human Fertility Database

The data from the Human Fertility Database is publicly available at <https://www.humanfertility.org/> and the relevant extracts are included in the folder `raw-data/hfd`.

#### Human Fertility Collection

The Human Fertility Collection is an extension of the Human Fertility Database which includes earlier historical data (with a lower quality). It is publicly available at <https://www.fertilitydata.org/cgi-bin/index.php> and the relevant data extract are included in the folder `raw-data/hfc`.

#### Gapminder

[Gapminder](https://www.gapminder.org/) provides the earliest evidence on fertility rates in the United States, in the form a a single series for the total fertility rate of women. I use this series to extrapolates the age-specific fertility rates before the 20th century.

#### Marriage and Divorce Rates from the US Census Bureau

The US Census Bureau provides the earliest evidence on divorce and marriage rates. This data is publicly available from <https://www.census.gov/library/publications/2006/compendia/statab/126ed/vital-statistics.html> and the relevant extract is included under `raw-data/uscb-marital`.

#### National Vital Statistics System

The more recent data on marriage and divorce rates come from the CDC's National Vital Statistics System. This data is publicly available at <https://www.cdc.gov/nchs/nvss/marriage-divorce.htm> and the relevant data extract is included in `raw-data/nvss`.

#### Estate Tax Schedules

The statutory schedules of the estate tax were collected manually from historical IRS forms and other historical. The data and its sources is included in the folder `raw-data/estate-tax`.

#### Forbes 400

The Forbes 400 panel is taken from [Gomez (2022)](https://www.matthieugomez.com/files/forbes400.csv).

Dataset list
------------

| Data file or directory | Source | Notes | Provided |
|---|---|---|---|
| `raw-data/dina-macro` | [Piketty, Saez and Zucman (2018)](https://gabriel-zucman.eu/files/PSZ2018QJE.pdf), updated by the same authors. | Available at <https://gabriel-zucman.eu/usdina/>. | Yes |
| `raw-data/dina-micro` | [Piketty, Saez and Zucman (2018)](https://gabriel-zucman.eu/files/PSZ2018QJE.pdf), updated by the same authors. | Complete data is based on the public-use tax microdata, which can be obtained from the NBER but cannot be redistributed. A degraded version of this data is available from <https://gabriel-zucman.eu/usdina/>. | No |
| `raw-data/scf` | Survey of Consumer Finances, Federal Reserve Board | Data not provided but automatically downloaded by `R/02-import-scf.R` and `R/02-import-scf-panel.R`. | No |
| `raw-data/scf-plus` | SCF+ [(Kuhn, Schularick and Steins, 2020)](https://www.journals.uchicago.edu/doi/abs/10.1086/708815) | Data not provided but available from <https://doi.org/10.1086/708815>. | No |
| `raw-data/sipp` | Survey of Income and Program Participation | Data not provided but automatically downloaded by `R/02-import-sipp.R`. | No |
| `raw-data/psid` | Panel Study of Income Dynamics | Relevant data extract provided in `raw-data/psid`. | Yes |
| `raw-data/census` | Census microdata (from IPUMS USA). | Data not provided dues to size and licensing issues, but available from <https://usa.ipums.org/usa/>. | No |
| `raw-data/ucsb` | Census population tables. | Data provided and if necessary downloaded by `R/02-import-population.R`. | Yes |
| `raw-data/hmd` | Human Mortality Database. | Data provided in `raw-data/hmd`. | Yes |
| `raw-data/wpp` | World Population Prospects (2019). | Data provided and if necessary downloaded by `R/02-import-population.R`. | Yes |
| `raw-data/hltd` | Human Life Table Database. | Data provided in `raw-data/hltd`. | Yes |
| `raw-data/haines` | Haines (1998) life tables. | Data provided in `raw-data/haines`. | Yes |
| `raw-data/hfd` | Human Fertility Database. | Data provided in `raw-data/hfd`. | Yes |
| `raw-data/hfc` | Human Fertility Collection. | Data provided in `raw-data/hfc`. | Yes |
| `raw-data/gapminder` | Total female fertility rates from [Gapminder](https://www.gapminder.org/). | Data provided in `raw-data/gapminder`. | Yes |
| `raw-data/uscb-marital` | Marriage and divorce data from the census bureau. | Data provided in `raw-data/uscb-marital`. | Yes |
| `raw-data/nvss` | Marriage and divorce data from the National Vital Statistics System. | Data provided in `raw-data/nvss`. | Yes |
| `raw-data/estate-tax` | Data on the estate tax schedule. | Data provided in `raw-data/estate-tax`. | Yes |
| `raw-data/forbes400` | Panel data of the Forbes 400 (from Gomez, 2022). | Data provided in `raw-data/forbes400`. | Yes |


Computational requirements
---------------------------

### Software Requirements

- R 4.0.1
  - I recommend opening the replication package using [RStudio](https://www.rstudio.com/) and opening the RStudio project file `uncovering-wealth-dynamics.Rproj`.
  - Package dependencies are managed using [renv](https://rstudio.github.io/renv/articles/renv.html).
  - Part of the code uses [Stan](https://mc-stan.org/) for nonlinear optimization, which is called via R using `cmdstanr`.

### Memory and Runtime Requirements

#### Summary

Approximate time needed to reproduce the analyses on a standard (CURRENT YEAR) desktop machine:

- [ ] <10 minutes
- [ ] 10-60 minutes
- [ ] 1-8 hours
- [ ] 8-24 hours
- [ ] 1-3 days
- [x] 3-14 days
- [ ] > 14 days
- [ ] Not feasible to run on a desktop machine, as described below.

#### Details

The code was last run on a **2,4 GHz 8-Core Intel Core i9 laptop with MacOS version 12.5 and 64 GB of RAM**. 


Description of programs/code
----------------------------

The codes are included in the folder `R`. They should run in the following order:

- Preliminaries:
  - `R/01-utils.R` defines several useful functions.
- Data imports:
  - `R/02-import-dina.R` imports the DINA micro and macro data.
  - `R/02-import-scf.R` imports the main, cross-sectional Survey of Consumer Finces data.
  - `R/02-import-scf-panel.R` imports the 2007-2009 Panel Survey of Consumer Finces.
  - `R/02-import-scf-plus.R` imports the SCF+ (the SCF extended historically by Kuh Schularick and Steins, 2020).
  - `R/02-plot-dina-scf.R` plots some basic results from the DINA and SCF data.
  - `R/02-import-sipp.R` imports the SIPP data.
  - `R/02-import-psid.R` imports the PSID data.
  - `R/02-import-census.R` imports the census microdata.
  - `R/02-import-population.R` imports the population data.
  - `R/02-import-mortality.R` imports mortality data (life tables).
  - `R/02-import-female-fertility.R` imports female fertility data.
  - `R/02-import-marital-status.R` imports data on marital status.
  - `R/02-import-estate-tax-schedule.R` imports data on the estate tax.
  - `R/02-import-forbes400.R` import the panel data from the Forbes 400.
- Preliminary data preparation:
  - `R/03-combine-calibrate-microdata.R` combines the DINA microdata with the SCF.
  - `R/03-estimate-marriage-rates.R` estimates age-specific marriage/divorce rates.
  - `R/03-estimate-male-fertility.R` estimates fertility rates for men.
  - `R/03-estimate-intergenerational-linkages.R` simulates intergenerational linkages in the microdata.
  - `R/03-estimate-inheritance-process.R` simulates the effect of inheritance in the microdata.
  - `R/03-estimate-marriage-process.R` simulates the effects of marriage and divorce.
- Creation of input data for the model:
  - `R/04-prepare-data.R` prepares the microdata for estimation.
  - `R/04-estimate-distribution-wealth.R` estimates the distribution of wealth.
  - `R/04-estimate-distribution-income.R` estimates the distribution of income (by wealth).
  - `R/04-estimate-distribution-birth.R` estimates a distribution for newly entering units.
  - `R/04-estimate-distribution-death.R` estimates the distribution of people at death.
  - `R/04-estimate-distribution-inheritance.R` estimates the distribution of the effect of inheritances.
  - `R/04-estimate-distribution-marriage.R` estimates the distribution of the effect of marriages and divorces.
- Model fit:
  - `R/05-prepare-data-model.R` combines all the relevant input data into a single object.
  - `R/05-fit-model.R` fits the benchmark model.
  - `R/05-fit-model-robustness-delta.R` performs robustness checks for the delta parameter.
  - `R/05-fit-model-robustness-bw.R` performs robustness checks for the bandwidth parameters.
  - `R/05-fit-model-robustness-bootstrap.R` calculates confidence intervals using bootstrap.
- Analysis of the results:
  - `R/06-decompose-effects.R` decomposes the drivers of the growth of the top 1%.
  - `R/06-analyze-panels.R` compares mobility estimates with the SCF and the SIPP.
- Perform simulations of the models:
  - `R/07-simulation-functions.R` defines functions useful for simulating the model.
  - `R/07-prepare-simulation.R` preprocesses data for the simulations.
  - `R/07-simulate-model.R` simulates the benchmark model until 2019.
  - `R/07-simulate-model-future.R` simulates the benchmark model until 2070.
  - `R/07-simulate-model-capital.R` simulates a counterfactual with pre-1980 rates of return.
  - `R/07-simulate-model-demography.R` simulates a counterfactual for demography.
  - `R/07-simulate-model-estate-tax-high.R` simulates a counterfactual with a confiscatory estate tax.
  - `R/07-simulate-model-estate-tax.R` simulates a counterfactual with no change in the estate tax.
  - `R/07-simulate-model-gains-zero.R` simulates a counterfactual with no capital gains.
  - `R/07-simulate-model-gains.R` simulates a counterfactual with pre-1980 capital gains.
  - `R/07-simulate-model-growth.R` simulates a counterfactual with pre-1980 growth.
  - `R/07-simulate-model-income-taxes.R` simulates a counterfactual with pre-1980 taxes.
  - `R/07-simulate-model-labor-income.R` simulates a counterfactual with pre-1980 labor income inequality.
  - `R/07-simulate-model-savings.R` simulates a counterfactual with pre-1980 savings.
  - `R/07-simulate-model-sanders.R` simulates a counterfactual with the Sanders wealth tax.
  - `R/07-simulate-model-warren-i.R` simulates a counterfactual with the Warren wealth tax (first version).
  - `R/07-simulate-model-warren-ii.R` simulates a counterfactual with the Warren wealth tax (second version).
- Analysis of wealth taxes:
  - `R/08-wealth-tax-simulations.R` applies the paper's wealth tax formulas.
 
### License for Code

![Modified BSD License](https://img.shields.io/badge/License-BSD-lightgrey.svg)

The code is licensed under the [Modified BSD License](https://opensource.org/licenses/BSD-3-Clause). See [LICENSE.txt](LICENSE.txt) for details.

Instructions to Replicators
---------------------------

- Open the file `uncovering-wealth-dynamics.Rproj` in RStudio.
- RStudio should recognize the `renv` which indicates all the necessary dependencies. Make sure to install all the packages.
- Run the codes in the folder `R`, in the order indicated above. Preferably restart R before running each code.
- Codes save intermediary objects as RDS files in the `work` directory. Some are already included in this package, so the codes need not be run again. Others could not for size/licensing issues.
- The codes output all the graphs and tables under `graphs`.

List of tables and programs
---------------------------

The provided code reproduces:

- [x] All numbers provided in text in the paper
- [x] All tables and figures in the paper
- [ ] Selected tables and figures in the paper, as explained and justified below.

Tables and figure are included in respective folders within `graphs`.

| Figure/Table # | Program | Output file |
|---|---|---|
| Figure 1 | R/02-plot-dina-scf.R | wealth-shares-top1.pdf |
| Figure 2 | n.a. (no data) |  |
| Figure 3 | n.a. (no data) |  |
| Figure 4a | R/04-estimate-distribution-wealth.R | density-wealth-1978-2019.pdf |
| Figure 4b | R/04-estimate-distribution-wealth.R | deriv-cdf.pdf |
| Figure 5a | R/05-fit-model.R | phase-portrait.pdf |
| Figure 5b | R/05-fit-model-bootstrap.R | diffu-drift-ci-top.pdf |
| Figure 6 | n.a. (no data) |  |
| Figure 7a | R/07-simulate-model-future.R | actual-simul-top1-future.pdf |
| Figure 7b | R/07-simulate-model.R | actual-simul-gic-2.pdf |
| Table 1 | R/06-analyze-panels.R | consumption.tex |
| Figure 8a | R/06-analyze-panels.R | mobility-rank-scf.pdf |
| Figure 8b | R/06-analyze-panels.R | mobility-rank-psid.pdf |
| Table 2 | R/06-decompose-effects.R | decomposition.tex |
| Figure 9a | R/07-simulate-model-labor-income.R | actual-simul-top1-labor-income.pdf |
| Figure 9b | R/07-simulate-model-capital.R | actual-simul-top1-capital.pdf |
| Figure 9c | R/07-simulate-model-income-taxes.R | actual-simul-top1-income-taxes.pdf |
| Figure 9d | R/07-simulate-model-savings.R | actual-simul-top1-savings.pdf |
| Figure 9e | R/07-simulate-model-growth.R | actual-simul-top1-growth.pdf |
| Figure 9f | R/07-simulate-model-estate-tax.R | actual-simul-top1-estate-tax.pdf |
| Figure 10a | R/08-wealth-tax-simulations.R | laffer-wealth-tax.pdf |
| Figure 10b | R/08-wealth-tax-simulations.R | abacus-wealth-tax.pdf |
| Figure 11 | R/07-simulate-model-estate-tax-high.R | simple-model-inheritance.pdf |

## References

Center for Disease Control (2022). *National Vital Statistics System*. <https://www.cdc.gov/nchs/nvss/index.htm>

Gapminder (2019). *Children per women since 1800*. <https://www.gapminder.org/news/children-per-women-since-1800-in-gapminder-world/>

Haines, Michael R. (1998). *“Estimated Life Tables for the United States, 1850–1910”*. Historical Methods: A Journal of Quantitative and Interdisciplinary History 31.4, pp. 149–169. <http://www.tandfonline.com/doi/abs/10.1080/01615449809601197>

Human Fertility Collection (2019). Max Planck Institute for Demographic Research in Rostock, Germany and Vienna Institute of Demography in Vienna, Austria. <https://www.fertilitydata.org/>

Human Fertility Database (2019). Max Planck Institute for Demographic Research in Rostock, Germany and Vienna Institute of Demography in Vienna, Austria. <https://www.humanfertility.org/>

Human Life Table Database (2019). Max Planck Institute for Demographic Research in Rostock,Germany, Department of Demography at University of California at Berkeley, USA and Institut d’études démographiques (INED) in Paris, France. <https://www.lifetable.de/>

Human Mortality Database (2019). Max Planck Institute for Demographic Research in Rostock, Germany, Department of Demography at University of California at Berkeley, USA. <https://www.mortality.org>

Piketty, Thomas, Emmanuel Saez, and Gabriel Zucman (2018). *“Distributional National Accounts: Methods and Estimates for the United States”*. The Quarterly Journal of Economics 133.2, pp. 553–609. <https://doi.org/10.1093/qje/qjx043>

Ruggles, Steven et al. (2022). IPUMS USA: Version 12.0. Version Number: 12.0 Type: dataset. <https://usa.ipums.org>

United Nations (2019). *World Population Prospects, 2019 Revision*. OCLC: 1142478963. <https://population.un.org/wpp/Publications/>

United States Census Bureau (2021). *National Intercensal Tables: 1900-1990*. <https://www.census.gov/content/census/en/data/tables/time-series/demo/popest/pre-1980-national.html>
