library(targets)
library(tarchetypes)
library(here)

# Set common dependencies used across both pipelines
tar_option_set(
  packages = c(
    "DBI", "RPostgres", "dplyr"
  )
)

# Load Supervised Classification functions
source(here("R", "supervised_classification_pipeline", "load_data.R"))
source(here("R", "supervised_classification_pipeline", "prepare_data.R"))
source(here("R", "supervised_classification_pipeline", "modeling.R"))

# Load Regression pipeline functions
source(here("R", "regression_pipeline", "load_fire_data.R"))
source(here("R", "regression_pipeline", "prepare_fire_data.R"))
source(here("R", "regression_pipeline", "model_fire_glm.R"))
source(here("R", "regression_pipeline", "model_fire_gam.R"))

# Load Unsupervised Pipeline functions
source(here("R", "unsupervised_learning_pipeline", "load_data.R"))
source(here("R", "unsupervised_learning_pipeline", "eda.R"))
source(here("R", "unsupervised_learning_pipeline", "pca.R"))
source(here("R", "unsupervised_learning_pipeline", "clustering.R"))
list(
  ### ── Supervised Classification ────────────────────────────────
  tar_target(raw_data,     load_data()),
  tar_target(prepared_data, prepare_data(raw_data)),
  tar_target(model_fit,     train_models(prepared_data)),
  tar_target(
    evaluation,
    evaluate_models(model_fit),
    deployment = "main"
  ),
  
  ### ── Fire‑brigade Regression (multinomial GLM / GAM) ──────────
  tar_target(fire_raw,   load_fire_data()),
  tar_target(fire_clean, prepare_fire_data(fire_raw)),
  tar_target(glm_model1, fire_glm1(fire_clean)),
  tar_target(glm_model2, fire_glm2(fire_clean)),
  tar_target(gam_model1, fire_gam1(fire_clean)),
  
  ### ── Olive‑oil Unsupervised Clustering ────────────────────────
  tar_target(oil_df,          load_oil_data()),
  tar_target(oil_pca,         unsup_pca(oil_df)),          # returns list
  tar_target(oil_clustering,  unsup_clustering(oil_pca$pca_df)),
  
  ### ── Reports (all use the same Rmd template) ──────────────────
  tar_render(
    classification_report,
    "vignettes/Report.Rmd",
    params      = list(
      report_type = "classification",
      evaluation  = evaluation
    ),
    output_file = "Report_classification.html",
    output_format = "html_document"
  ),
  
  tar_render(
    regression_report,
    "vignettes/Report.Rmd",
    params      = list(
      report_type  = "regression",
      glm1_summary = summary(glm_model1),
      glm2_summary = summary(glm_model2),
      gam1_summary = summary(gam_model1)
    ),
    output_file = "Report_regression.html",
    output_format = "html_document"
  ),
  
  tar_render(
    clustering_report,
    "vignettes/Report.Rmd",
    params      = list(
      report_type      = "clustering",
      pca              = oil_pca,
      clustering_stats = oil_clustering
    ),
    output_file = "Report_clustering.html",
    output_format = "html_document"
  )
)