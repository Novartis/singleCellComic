# ==============================================================================
#  R Package Installation Script for singleCellComic Game
# ==============================================================================
#
# This script checks if all the necessary packages for the shiny app are 
# installed and, if not, installs them from CRAN or GitHub.
#
# Run this script once before launching the app for the first time.
#
# ==============================================================================

# --- 1. List of packages from CRAN ---
# These packages are available on the standard R package repository.
cran_packages <- c(
  "shiny",
  "shinycssloaders",
  "ggplot2",
  "dplyr",
  "patchwork",
  "Seurat",
  "purrr",
  "tibble",
  "scales",
  "stringr",
  "ggforce",
  "circlize",
  "remotes" # Needed to install packages from GitHub
)

# --- 2. Check and install CRAN packages ---
message("Checking for required CRAN packages...")
for (pkg in cran_packages) {
  # requireNamespace returns TRUE if the package is available, FALSE otherwise
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Package '", pkg, "' not found. Installing...", sep = ""))
    install.packages(pkg)
  } else {
    message(paste("Package '", pkg, "' is already installed.", sep = ""))
  }
}


# --- 3. List of packages from GitHub ---
# The 'harmony' package for data integration is hosted on GitHub.
github_packages <- c(
  "immunogenomics/harmony"
)


# --- 4. Check and install GitHub packages ---
message("\nChecking for required GitHub packages...")
for (pkg_path in github_packages) {
  # Extract the package name from the "user/repo" path
  pkg_name <- basename(pkg_path)
  
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    message(paste("Package '", pkg_name, "' not found. Installing from GitHub...", sep = ""))
    # Use the remotes package to install from GitHub
    remotes::install_github(pkg_path)
  } else {
    message(paste("Package '", pkg_name, "' is already installed.", sep = ""))
  }
}

# --- 5. Final confirmation ---
message("\nAll required packages are installed and ready to go!")
message("You can now run the shiny app by executing 'shiny::runApp(\"app.R\")' in your console.")
