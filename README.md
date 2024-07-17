---
output:
  pdf_document: default
  html_document: default
---
# Replication codes for "Event Studies with a Continuous Treatment"
Replication codes for Callaway, Goodman-Bacon, and Sant'Anna (2024) "Event Studies with a Continuous Treatment", forthcoming in the American Economic Association Papers and Proceedings.

## Instructions to replicate the results in the paper

The replication files are structure in a self-explanatory way. 

The main file is `codes/Run_all.R`, which runs all the files. The main file is organized as follows:

1. `codes/00_ggplot_theme.R`: Set up the theme for the plots.

2. `codes/1_data_preparation.R`: Prepare the data to run all analysis. This file uses data from the `data/processed` folder. Such processed data builds on the [Bartik, Currie, Greenstone, Knittel (2019) replication packages](https://www.aeaweb.org/journals/dataset?id=10.1257/app.20170487). To construct the data we use in this paper (`data/processed/bcgk_replication.dta`), please follow the instructions in the `codes/stata/process_data_bcgk.do` file.

3. `codes/2_Fig1_event_study.R`: It generate the Figure 1 of the paper and Figure B1 of the Online Appendix. 

4. `codes/3_dose_response.R`: it generates the dose-response analysis in the paper.

5. `codes/4_Fig2_event_study.R`: It generates the Figure 2 of the paper and Figure B3 of the Online Appendix.

## Questions and comments

Any questions related to these replication files can be directed to Pedro H. C. Sant'Anna at [pedro.santanna@emory.edu](mailto:pedro.santanna@emory.edu)
# es-cont
