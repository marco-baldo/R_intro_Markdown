# Install this packages for R Markdown tutorial

tinytex::install_tinytex() # In case MS word is not working with R Markdown.

# This is required for the installation of ggradar.
install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)


# Install Packages
install.packages("fmsb")             # Package that incorporate radar chart plot
install.packages("ggplot2")          # Data visualization: https://cran.r-project.org/web/packages/ggplot2/index.html
install.packages("tidyverse")        # Data manipulation: https://dplyr.tidyverse.org/
install.packages("scales")           # Scaling color palettes: https://cran.r-project.org/web/packages/scales/index.html
install.packages("purrr")            # Functional programming tool - graphics https://cran.r-project.org/web/packages/purrr/index.html
install.packages("fields")           # Visualization: https://cran.r-project.org/web/packages/fields/index.html
install.packages("GGally)")          # Visualization: https://cran.r-project.org/web/packages/GGally/index.html