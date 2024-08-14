# Associations between the Minority Health Social Vulnerability Index Scores, Rurality, and Histoplasmosis Incidence

**General disclaimer** This repository was created for use by CDC programs to collaborate on public health related projects in support of the [CDC mission](https://www.cdc.gov/about/organization/mission.htm).  GitHub is not hosted by the CDC, but is a third party website used by CDC and its partners to share information and collaborate on software. CDC use of GitHub does not imply an endorsement of any one particular service, product, or enterprise. 

## Related documents

* [Open Practices](open_practices.md)
* [Rules of Behavior](rules_of_behavior.md)
* [Thanks and Acknowledgements](thanks.md)
* [Disclaimer](DISCLAIMER.md)
* [Contribution Notice](CONTRIBUTING.md)
* [Code of Conduct](code-of-conduct.md)

## Overview

This repository houses code for analyses for a paper exploring associations between rurality, 
the Minority Health Social Vulnerability Index, and histoplasmosis incidence in eight states in the US.

All analyses were done in R 4.2.2, using `renv` for managing dependencies. 
The package dependencies are versioned in [renv.lock](renv.lock), and after 
cloning this repository and installing `renv` can be set up by running the 
following code:

```
renv::activate()
renv::restore()
```

### Data

- All data are stored in the [data/raw](data/raw) folder, but not included in this repository.
  - `data/raw/NCHS Urban_Rural Codes.xlsx`: from the [NCHS urban-rural classification scheme for 2013](https://www.cdc.gov/nchs/data_access/urban_rural.htm#2013_Urban-Rural_Classification_Scheme_for_Counties)
  for the subset of states included in our dataset. Pulled from [this dataset](https://www.cdc.gov/nchs/data/data_acces_files/NCHSURCodes2013.xlsx).
  - `data/raw/mh_svi_20230329.csv`: the [Minority Health SVI](https://www.minorityhealth.hhs.gov/minority-health-svi/) measures as well as the component 
  metrics that go into the each of the indices/themes, available at [this link](https://www.minorityhealth.hhs.gov/minority-health-SVI/assets/downloads/mh_svi_county_20230329.csv).
  - `HistoSVI.xlsx`: the histoplasmosis case counts by county and year. This only includes
  counties and years with any cases of histoplasmosis. This data was from an enhanced surveillance project (2011 - 2014), 
  shared with CDC MDB by state partners, or from NNDSS (2020), available with permissions from each state partner. 

### Analyses
- The analyses are run using the scripts in the [analysis](analysis) folder in the 
following order:
  - [01_get_regional_mhsvi.R](analysis/01_get_regional_mhsvi.R): pulls in raw data sources for the MH SVI 
  and the NCHS urban-rural classification scheme to calculate regional
  specific, and rural classification specific SVIs based on code from  (Sarah Rockhill)[https://github.com/s5rockhill/mh-svi].
  - [02_clean_histo_data.R](analysis/02_clean_histo_data.R): takes the regional and stratified svi's and joins them 
  with county level histoplasmosis case count and population data across the six
  years included in the study (2011 - 2014, 2019, 2020). Population data were accessed from 
  US census data using the [tidycensus](https://walker-data.com/tidycensus/index.html) package.
  - [03_run_models.R](analysis/03_run_models.R): using the cleaned histo data, aggregates to the sum of population
  and histo cases across the six years. Runs a truncated poisson hurdle models 
  on this aggregated cases counts using the R package 
  [glmmTMB](https://glmmtmb.github.io/glmmTMB/index.html), both for regional and a stratified analysis by rurality. Uses the R package
  [DHARMa](https://cran.r-project.org/web/packages/DHARMa/index.html) to run diagnostics on simulated model residuals.
  - [04_make_figures.R](analysis/04_make_figures.R): takes output from previous steps and create main and 
  supplementary figures, as well as summary statistics for Table 1. 
  
### Outputs
- Outputs from the first two steps of the analysis are saved to the `data` folder, 
but are not included in this repository due to restrictions on sharing. 
- The outputs from the other steps of the analyses are included in this repository in the [output/](output)
folder, except for the full hurdle model output (which includes the original data set). 
- Figures are included in the [figs/](figs) folder. 
- [methods_results_supp.qmd](methods_results_supp.qmd) includes some results, methods, and supplementary text that is pulled in to the initial draft of the main manuscript (although wording may have been revised.) 

## Public Domain Standard Notice
This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

## License Standard Notice
The repository utilizes code licensed under the terms of the Apache Software
License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

## Privacy Standard Notice
This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](DISCLAIMER.md)
and [Code of Conduct](code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice
Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

## Records Management Standard Notice
This repository is not a source of government records, but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

## Additional Standard Notices
Please refer to [CDC's Template Repository](https://github.com/CDCgov/template) for more information about [contributing to this repository](https://github.com/CDCgov/template/blob/main/CONTRIBUTING.md), [public domain notices and disclaimers](https://github.com/CDCgov/template/blob/main/DISCLAIMER.md), and [code of conduct](https://github.com/CDCgov/template/blob/main/code-of-conduct.md).
