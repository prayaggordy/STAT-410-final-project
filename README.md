# STAT-410-final-project
### Prayag Gordy

#### Directory structure
The scripts for data collection and analysis are in the `R/` folder. Each section of the paper has a dedicated `.Rmd` script, each of which can be found in the `Rmd/` folder.

To avoid storing data in GitHub, all `.csv` files are included in the `.gitignore`. The raw data from the CDC is stored in this repository, as I had to download these data manually (i.e., not programatically through an API). The first time you knit the paper, the data will download and save; the following times, the report should pull from the saved processed data.

#### Required packages
I would recommend installing all packages from the `tidyverse`, along with `ggcorrplot` and `tidycensus`. Additionally, you'll need a Census API key to download the Census data.

#### Knitting the paper
In RStudio, open the `paper.Rmd` script and click `Knit`. This may take a minute to run the first time, as the script has to download data from the Census.
