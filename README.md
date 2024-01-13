# Introduction
The code in this repository was used to create the tables and figures in the main text and online appendix of Seth Chizeck, Kelley Fong, Ariel White, and Rebecca Goldstein (2023). "Political Underrepresentation Among Public Benefits Recipients: Evidence from Linked Administrative Data". *Urban Affairs Review, 60*(1): 420-434. The publication is available at https://journals.sagepub.com/doi/abs/10.1177/10780874231191703. 

# Requirements 
R version 4.2.0 or higher. 

# Published data
The data used in this paper comes from a combination of publicly-available Pennsylvania voter files, and confidential microdata from the Allegheny County (PA) Department of Human Services. In this repository we make available a deidentified version of the data that is sufficient to replicate the analysis in the publication. See the data dictionary below for descriptions of these public datasets. 

# Data dictionary

| Table name | Description |
|:------------|:-------------|
| allegheny_cvap_demographics | Demographic characteristics of the Allegheny County citizen voting-age population, from ACS Table S2901 2019 1-year estimates  |
| allegheny_cvap_earnings | Wage and salary income in the past year among the Allegheny County citizen voting-age population, from ACS PUMS 2019 1-year estimates |
| allegheny_registered_voters_2020ge | The political party of each registered voter in Allegheny County, according to the Pennsylvania full voter export snapshot data from January 11, 2021 |
| allegheny_turnout_rates | The voter turnout rates among Allegheny County registered voters in recent elections, based on data from https://www.alleghenycounty.us/Government/Elections/Election-Results |
| allegheny_versus_usa_benefit_pops | The demographic composition of public benefits recipients in Allegheny County and in the U.S. as a whole. The Allegheny County numbers are derived from confidential microdata from the Allegheny County Department of Human Services. The U.S.-wide numbers are derived from a variety of public sources (see table footnotes for more details) |
| benefit_counts_2020 | The number of Allegheny County residents that received various public benefits, by month. This table compares the numbers from two sources: 1) Allegheny County Department of Human Services confidential microdata, and 2) Public aggregate data from the Pennsylvania Department of Human Services | 
| dhs_voter_data | Deidentified individual-level data on Allegheny County public benefits recipients and their voting activity in recent elections |
| earnings_2019 | Deidentified individual-level data on the 2019 earnings of voting-age Allegheny County residents who received public benefits in year 2020. The earnings data comes from Pennsylvania unemployment insurance wage records. In order to maintain individual anonymity, this table is standalone and cannot be joined to the dhs_voter_data table. |

## Variable definitions 

### allegheny_cvap_demographics

| Variable name | Description |
|:------------|:-------------|
| group | A demographic grouping |
| allegheny_cvap | The percentage of the Allegheny County citizen voting-age population that falls in the given demograhic grouping |

### allegheny_cvap_earnings

| Variable name | Description |
|:------------|:-------------|
| puma | A Census Public Use Microdata Area that falls completely within Allegheny County |
| earnings | The mean wage and salary income in the past year among the citizen voting-age population that lives in the given PUMA |
| count | The number of citizen voting-age individuals who live in the given PUMA |

### allegheny_registered_voters_2020ge 

| Variable name | Description |
|:------------|:-------------|
| voter_id | An individual's Pennsylvania voter ID |
| party_2020ge | The political party that the voter was registered under, as of January 11, 2021 |

### allegheny_turnout_rates

| Variable name | Description |
|:------------|:-------------|
| election | The year of the general election |
| program | A placeholder variable | 
| turnout_rate | The percentage of registered Allegheny County voters that voted in the given election |

### allegheny_versus_usa_benefit_pops

| Variable name | Description |
|:------------|:-------------|
| program | A public benefits program |
| geography | Allegheny County or the entire U.S. | 
| variable | A demographic characteristic | 
| grouping | The value of the given demographic characteristic | 
| program_recipient_count | The number of recipients of the given public benefit in the given demographic grouping |
| population_count | The total number of people in the given demographic grouping |

### benefit_counts_2020

| Variable name | Description |
|:------------|:-------------|
| month | A calendar year and month (yyyymm)|
| benefit | A public benefit program |
| count_acdhs | The number of individuals that received the given benefit in the given month, based on confidential microdata from the Allegheny County Department of Human Services |
| count_official | The number of individuals that received the given benefit in the given month, based on public aggregate data from the Pennsylvania Department of Human Services | 

### dhs_voter_data 

| Variable name | Description |
|:------------|:-------------|
| pseudo_id | A unique individual identifier |
| age_2020ge | The person's age as of the date of the 2020 general election |
| age_2018ge | The person's age as of the date of the 2018 general election |
| age_2016ge | The person's age as of the date of the 2016 general election |
| public_hous_2020pe | A dummy variable that equals 1 if the person resided in public housing in Allegheny County at some point in 2020 prior to the November election |
| section8_2020pe | A dummy variable that equals 1 if the person received Section 8 housing subsidies in Allegheny County at some point in 2020 prior to the November election |
| snap_2020pe | A dummy variable that equals 1 if the person received SNAP benefits in Allegheny County at some point in 2020 prior to the November election |
| snap_2018pe | A dummy variable that equals 1 if the person received SNAP benefits in Allegheny County at some point in 2018 prior to the November election |
| snap_2016pe | A dummy variable that equals 1 if the person received SNAP benefits in Allegheny County at some point in 2016 prior to the November election |
| ssi_2020pe | A dummy variable that equals 1 if the person received SSI benefits in Allegheny County at some point in 2020 prior to the November election |
| ssi_2018pe | A dummy variable that equals 1 if the person received SSI benefits in Allegheny County at some point in 2018 prior to the November election |
| ssi_2016pe | A dummy variable that equals 1 if the person received SSI benefits in Allegheny County at some point in 2016 prior to the November election |
| tanf_2020pe | A dummy variable that equals 1 if the person received TANF benefits in Allegheny County at some point in 2020 prior to the November election |
| tanf_2018pe | A dummy variable that equals 1 if the person received TANF benefits in Allegheny County at some point in 2018 prior to the November election |
| tanf_2016pe | A dummy variable that equals 1 if the person received TANF benefits in Allegheny County at some point in 2016 prior to the November election |
| medicaid_2020pe | A dummy variable that equals 1 if the person received Medicaid benefits in Allegheny County at some point in 2020 prior to the November election |
| medicaid_2018pe | A dummy variable that equals 1 if the person received Medicaid benefits in Allegheny County at some point in 2018 prior to the November election |
| medicaid_2016pe | A dummy variable that equals 1 if the person received Medicaid benefits in Allegheny County at some point in 2016 prior to the November election |
| ccdf_2020pe | A dummy variable that equals 1 if the person received child care subsidies in Allegheny County at some point in 2020 prior to the November election |
| registered_2020ge | A dummy variable that equals 1 if the person was registered to vote in Allegheny County as of January 11, 2021 |
| voted_2020ge | A dummy variable that equals 1 if the person voted in the 2020 general election |
| party_2020ge | The party that the person was registered under as of January 11, 2021 |
| registered_2018ge | A dummy variable that equals 1 if the person was registered to vote in Allegheny County as of December 24, 2018 |
| voted_2018ge | A dummy variable that equals 1 if the person voted in the 2018 general election |
| registered_2016ge | A dummy variable that equals 1 if the person was registered to vote in Allegheny County as of December 26, 2016 |
| voted_2016ge | A dummy variable that equals 1 if the person voted in the 2016 general election |
| race | The person's race |
| hispanic | A dummy variable that equals 1 if the person is Hispanic |
| gender | The person's gender |
| deceased_2020ge | A dummy variable that equals 1 if the person was deceased as of the date of the 2020 general election |
| deceased_2018ge | A dummy variable that equals 1 if the person was deceased as of the date of the 2018 general election |
| deceased_2016ge | A dummy variable that equals 1 if the person was deceased as of the date of the 2016 general election |

### earnings_2019

| Variable name | Description |
|:------------|:-------------|
| earnings_2019 | The total 2019 earnings of an individual Allegheny County public benefits recipient |
