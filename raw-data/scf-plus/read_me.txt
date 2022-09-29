The folder "data" contains all necessary input data:

  * The main dataset is called "SCF_plus.dta".
  * The folder "aggregate_data" contains data from the Financial Accounts and National Income and Product Accounts.
  * The folder "census_data" contains data from the CPS and Decennial Census.
  * The folder "PSID_data" contains the PSID data (J252489.txt) used in Appendix C of the paper, as well two do-files processing the data, which are called by the main do-file "replication_codes.do".
  * The folder "replicate_weights" contains the SCF+ replicate weights.
  * The folder "tax_data" contains data from Saez and Zucman (2016) and Piketty and Saez (2003).
  * The file "assetprices.dta" contains real house and stock prices from the Macrohistory Database.
  * The file "cash_vic.dta" contains data on stolen cash from the National Crime Victimization Survey (NCVS).
  * The file "CPI_PCE.dta" contains CPI and PCE indices.
  * The file "SFCC_1962.dta" contains data from the 1962 Survey of Financial Characteristics of Consumers (SFCC).

The codes to produce all figures and tables from the paper are collected in "replication_codes.do".

Required Stata packages: sgini

All monetary variables are in 2016 dollars, deflated using the CPI of the same year (note that income variables refer to the previous year).

There are 5 implicates per observation due to multiple imputation. The implicates are indicated by the variable "impnum".

Overview over SCF+ variables:

adults - number of adults
ageh - age of head
agehgroup - age group
blackh - whether head is black
bnd - bonds
ccdebt - credit card debt
cerde - certificates of deposit
children - number of children
collegeh - whether head has attained at least some college
CPI - consumer price index
ffaass - total assets
ffabus - business wealth
ffaequ - equity and other managed assets
ffafin - financial assets (ffaequ, liqcer, bnd, mfun, ofin, life, pen)
ffanfin - non-financial assets (ffabus, house, oest, vehi, onfin)
ffanw - net wealth (ffafin + ffanfin - tdebt)
ffanwgroups - wealth groups
hdebt - housing debt on owner-occupied real estate
hhequiv - OECD equivalence scale
highsample  - indicator for high-income sample in 1983
house - asset value of house
housing_rent_yd - housing rental yield from Macrohistory Database
id - household id
impnum - imputation implicate number
inccap - capital income
inctrans - transfer income
incws - income from wages, salaries and self-employment
incwsse - income from wages and salaries
life - life insurance assets
liq - liquid assets
liqcer - liquid assets and certificates of deposit
mfun - mutual funds
moneymarketacc - money market accounts
oest - other real estate (net position)
oestdebt - other real estate debt
ofin - other financial assets
onfin - other non-financial assets
othdebt - other debt
PCE - personal consumption expenditures index
pdebt - personal debt
pen - pensions
prepaid - prepaid cards
raceh - race of head
savbnd - savings bonds
tdebt - total household debt (excluding other real estate debt, i.e. hdebt + pdebt)
tinc - total household income, excluding capital gains
tincgroups - income groups
vehi - vehicles
wgt - unadjusted weight
wgtI95W95 - survey weight
year - year
yearmerge - 3-year window
