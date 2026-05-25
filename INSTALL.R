# ============================================================
# INSTALL.R
# Instalar todos os pacotes necessários para a app DSS
# ============================================================
# Corre este ficheiro UMA VEZ antes de abrir o app.R

pkgs <- c(
  "shiny",
  "ggplot2",
  "dplyr",
  "tidyr",
  "DT",
  "forecast",
  "randomForest",
  "zoo",
  "mco"       # necessário para O3 (NSGA-II)
)

# Define um mirror CRAN explícito para o `Rscript`
options(repos = c(CRAN = "https://cloud.r-project.org"))

to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(to_install) > 0) {
  cat("A instalar:", paste(to_install, collapse = ", "), "\n")
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  cat("Todos os pacotes já estão instalados.\n")
}

cat("\nVerificação:\n")
for (p in pkgs) {
  status <- if (requireNamespace(p, quietly = TRUE)) "✔" else "✖ FALHOU"
  cat(sprintf("  %s  %s\n", status, p))
}
