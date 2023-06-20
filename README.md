# data4whw

Data collection and analysis for the Women's Health & Wellbeing project.

***

## About

Exemplars in Global Health (<https://www.exemplars.health>) research framework aims to identify countries that are positive outliers in their performance within a specific topic, beyond what can be expected due to economic development alone. The Women's Health and Wellbeing (WHW) study focuses on using available data on the selected indicators to quantify the performance of LMICs across nine dimensions and five life course stages of a woman’s life and wellbeing.More information about this ongoing project is available here (<https://www.exemplars.health/stories/expanding-our-focus-on-womens-health-and-well-being>).

This page will be updated as the relevant publications are released.

This repository is set to create the WHW Global Database with an extensive list of indicators relevant to the broad area of WHW for 135 LMICs. The selected indicators are retrieved from publicly and globally available data collections such as Sustainable Development Goals (United Nations Statistics Division), the Global Health Observatory (World Health Organization), World Bank, UNICEF Data Warehouse, the Institute for Health Metrics and Evaluation (IHME), the Demographic Health Surveys and other relevant databases for the period between 2000 and 2019. This repository, therefore, hosts all the `R` scripts and data files related to data collection and analysis for the project. 

This page will be updated as the relevant publications are released.

## The Structure

The project's structure mostly follows a standard proposed in Marwick, Boettiger, and Mullen (2018).
Moreover, it is set up as an `R` package which facilitates the use of the functions defined herein.
The folders in this repository are used as follows:

- `analysis/` contains `R Markdown` files that perform parts of the data analysis. These are used as development notebooks that showcase how the code is used to complete specific analytical tasks. 
- `data/` has no files *per se* but comprises three different subfolders
  - `data/raw/` contains input data files, e.g., a list of indicators to be collected from a certain data source. One should not write any files to or modify any files in this folder. It should be **treated as read-only**.
  - `data/tmp`, a folder to write temporary files to. Preferably, any temporary files written
  to this folder should be deleted by the scripts once they are no longer needed. Note that this folder is included in `.gitignore`. Hence, files included in this folder will **neither be tracked by git not pushed to GitHub**.
  - `data/out`, a folder to write output files to. This folder is included in `.gitignore`. Hence, files included in this folder will **neither be tracked by git not pushed to GitHub**.
- `man/` includes documentation automatically generated from `roxygen2` comments. One should not modify this folder as it will be automatically generated and updated by the repository maintainer.
- `R/` contains `.R` scripts that define the main functionality of the package. Each script comprises only **a set of related functions**, e.g., a set of functions for obtaining the data from UNSD SDG API. To make code maintenance easier, scripts **are prefixed** by the function word that makes clear the primary goal of the script. For example, all scripts that interact with an external API start with `api_`, data cleaning scripts should start with `cleaning_` etc. 


## Project Environment

This project uses `renv` package for consistency and reproducibility. This ensures that all the packages required to run all the code are specified in the repo. The `renv` has already been set up. The rules for working with it are simple.

- When you clone the repository for the first time and open the project, you should see a message `Project '<path>' loaded. [renv 0.15.5]`. If you do not see this, install `renv` with `install.packages("renv")` and restart your session.
- In your `R` console, run `renv::restore()`. This will install all the packages needed for the project in your environment (note that this **will not** change anything in your global `R` package env). 
- If you need a package that is not available in the environment, you can install it normally with `install.packages`. Again, it will **only be installed to this project's env**.
- If you have installed any additional packages, you should run `renv::snapshot()` to update env specification file so that other people would know they need to update the env too.

## Summary Points

- Always start working by opening `data4whw.Rproj`. This makes sure that the `renv` and `git` are launched properly.
- When defining functions in `R/` folder **do not** load any packages, i.e. **avoid** using `library(<name>)`. This is a bad practice as it may incidentally overwrite functions with the same name. See [this short explanation](https://stackoverflow.com/questions/64737686/why-library-or-require-should-not-be-used-in-a-r-package) on StackOverflow.
- You should always explicitly use `packagename::functionname`, e.g., `dplyr::filter`, `httr::GET`.
- To make function defined in `R/` available in your markdown file in `analysis/`, add the following command at the beginning of your script:

```r
devtools::load_all(
  path = ".",
  reset = TRUE,
  export_all = TRUE,
  helpers = TRUE
)
``` 

- Do not forget to document your functions. When functions are documented, you can run `help(function_name)` to see their documentation.
- Commit changes regularly. Push the branch only when it is ready.

## Citations

Marwick, B., Boettiger, C., & Mullen, L. (2018). Packaging Data Analytical Work Reproducibly Using R (and Friends). *The American Statistician*, 72(1), 80--88. <https://doi.org/10.1080/00031305.2017.1375986>

Wickham, H. (2023). R packages (Second edition). O’Reilly Media. <https://r-pkgs.org>
