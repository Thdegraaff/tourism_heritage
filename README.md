# tourism_heritage

## Contents

This repository provides supplementary material for the paper **European cultural heritage and tourism flows: the magnetic role of superstar World Heritage Sites**. It consists of the figures used in the paper (in the folder *fig*), the data used (in *data/src*) and data as output (in *data/derived*) and finally the R-code (in the folder *code*). 

There are 3 R-files:

- `analysis_europe.R`: which is the main code
- `analysis_europe_rev.R`: gives the code for an analysis with length of stay as dependent variable
- `analysis_europe_rev_add_control.R`: Gives the code with additional control as robustness check

## Requisites

To run the R-files, the program STAN needs to be installed (to be found here [https://mc-stan.org/](https://mc-stan.org/)). Also note that it will take about 1-2 hours to run one file. 