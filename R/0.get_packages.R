# setup

options(repos = c(CRAN = "https://cloud.r-project.org"))


required_packages <- c("ggplot2", 
                       "haven", 
                       "dplyr", 
                       "lme4",
                       "lmerTest", 
                       "car", 
                       "foreign", 
                       "readxl",
                       "knitr",
                       "apaTables",  
                       "reshape2",
                       "lavaan",
                       "sjPlot", # to use "tab_model"
                       "effectsize", # to  get the effect sizes
                       "emmeans", # estimated margina.l means
                       "ggeffects",# to use the "predict" formula
                       "parameters", # for standardized coefficients, 
                       "interactions", # for the interaction plots
                       "psych"# for correlation test
                       )

# Check and install missing packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg)
  } else {
    message(paste(pkg, "is already installed."))
  }
}

# Optionally load all packages
lapply(required_packages, library, character.only = TRUE)