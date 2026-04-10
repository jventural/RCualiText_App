# R/reliability.R
# Intercoder reliability calculations (Cohen's Kappa, Brennan-Prediger, percent agreement)

#' @title Compute intercoder reliability (Cohen's Kappa)
#' @description Calculates Cohen's Kappa, Brennan-Prediger Kappa, and percent agreement between two coders' classifications of the same fragments.
#' @param coder1_codigos Character vector of codes assigned by coder 1.
#' @param coder2_codigos Character vector of codes assigned by coder 2 (same length and order as coder1).
#' @return A list with kappa_cohen, kappa_bp, percent_agreement, n_total, n_agreed, confusion_matrix.
calcular_reliability <- function(coder1_codigos, coder2_codigos) {
  if (length(coder1_codigos) != length(coder2_codigos)) {
    stop("Both coders must have the same number of observations")
  }
  if (length(coder1_codigos) < 2) {
    stop("Need at least 2 observations")
  }

  n <- length(coder1_codigos)
  n_agreed <- sum(coder1_codigos == coder2_codigos, na.rm = TRUE)
  percent_agreement <- n_agreed / n

  # Get all unique categories
  all_codes <- unique(c(coder1_codigos, coder2_codigos))
  k <- length(all_codes)

  # Build confusion matrix
  conf_mat <- table(
    factor(coder1_codigos, levels = all_codes),
    factor(coder2_codigos, levels = all_codes)
  )

  # Cohen's Kappa
  p_o <- sum(diag(conf_mat)) / n
  marginal_rows <- rowSums(conf_mat) / n
  marginal_cols <- colSums(conf_mat) / n
  p_e <- sum(marginal_rows * marginal_cols)
  kappa_cohen <- if (p_e < 1) (p_o - p_e) / (1 - p_e) else NA

  # Brennan-Prediger Kappa (assumes equal marginal distributions)
  kappa_bp <- (p_o - 1/k) / (1 - 1/k)

  list(
    kappa_cohen = round(kappa_cohen, 3),
    kappa_bp = round(kappa_bp, 3),
    percent_agreement = round(percent_agreement, 3),
    n_total = n,
    n_agreed = n_agreed,
    n_codes = k,
    confusion_matrix = as.data.frame.matrix(conf_mat)
  )
}

#' @title Interpret a Kappa coefficient
#' @description Returns Landis & Koch (1977) interpretation of a kappa value.
#' @param kappa Numeric. Kappa coefficient (-1 to 1).
#' @param lang Character. "en" or "es".
#' @return Character. Interpretation string.
interpretar_kappa <- function(kappa, lang = "en") {
  if (is.na(kappa)) return(if (lang == "es") "No calculable" else "Not computable")
  levels_en <- c(
    "Poor agreement (< 0)",
    "Slight agreement (0-0.20)",
    "Fair agreement (0.21-0.40)",
    "Moderate agreement (0.41-0.60)",
    "Substantial agreement (0.61-0.80)",
    "Almost perfect agreement (> 0.80)"
  )
  levels_es <- c(
    "Acuerdo pobre (< 0)",
    "Acuerdo ligero (0-0.20)",
    "Acuerdo justo (0.21-0.40)",
    "Acuerdo moderado (0.41-0.60)",
    "Acuerdo sustancial (0.61-0.80)",
    "Acuerdo casi perfecto (> 0.80)"
  )
  levels_txt <- if (lang == "es") levels_es else levels_en
  idx <- if (kappa < 0) 1 else if (kappa <= 0.20) 2 else if (kappa <= 0.40) 3 else if (kappa <= 0.60) 4 else if (kappa <= 0.80) 5 else 6
  levels_txt[idx]
}
