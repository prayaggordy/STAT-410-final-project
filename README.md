# STAT 410 final project
### Prayag Gordy

#### Directory structure
The scripts for data collection and analysis are in the `R/` folder. `dm_outcome.R` is the data mangement script for the CDC infant mortality data; `dm_census.R` is the data management script for the Census data; and `helper_functions.R` contains a function that prints an `lm` object as an equation in LaTeX.

Each section of the paper has a dedicated `.Rmd` script, each of which can be found in the `Rmd/` folder. Because these are knit as children to the main `paper.Rmd`, the environment (i.e., all the objects and loaded libraries) from `paper.Rmd` are loaded into each section's markdown file.

To avoid storing data in GitHub, all `.csv` files are included in the `.gitignore`. The raw data from the CDC is stored in this repository, as I had to download these data manually (i.e., not programatically through an API). The first time you knit the paper, the data will download and save, which will take a bit of time; the following times, the report should pull from the saved, processed data.

#### Knitting the paper
After cloning the repository, open your `R` console and install the `renv` package: `install.packages("renv")`. `renv` creates a virtual environment to keep all package dependencies nice and organized. If this is your first installation of `renv`, quit and reopen RStudio before proceeding. To install all the packages necessary for knitting this paper, in the console run: `renv::restore()`. I've found that `renv` can be a bit finicky—especially on Macs—so you may have to run `renv::restore()`, close RStudio, open the project again, and then run `renv::restore()` a second time.

Finally, knit `paper.Rmd`, which will in turn knit and stitch together all of the sections of the paper.

**Note:** You will need a Census API key to download Census data. If you don't have a key, get one [here](https://api.census.gov/data/key_signup.html) and use `tidycensus::census_api_key(your_key)` to install it.
