# Effect Size Functions ----

# Functions used in effect size calculations across multiple datasets

#' Effect Size - Edge Case F
#' 
#' Calculates the effect size using the F-vlaue in weird edge cases where df1 > 1.
#'
#' @param Fvalue F-value.
#' @param df1 Numerator degrees of freedom.
#' @param df2 Denominator degrees of freedom.
#'
#' @returns A tibble with the effect size and its confidence intervals.
convert_cf <- function(Fvalue, df1, df2) {
  
  effectsize::F_to_f(Fvalue, df1, df2, alternative = "two.sided") |> 
    tibble::as_tibble() |> 
    dplyr::mutate(dplyr::across(-c(CI), ~ .x*2)) |> # f to d
    dplyr::mutate(dplyr::across(-c(CI), d_to_r)) |> # d to r
    dplyr::rename(r = Cohens_f_partial)
  
}

#' Effect Size - Chi-Square
#' 
#' Calculates the effect size using chi-square.
#'
#' @param chi_sq Chi-square
#' @param n Total sample size
#'
#' @returns A tibble with the effect size and its confidence intervals.
convert_chi <- function(chi_sq, n) {
  
  r <- effectsize::chisq_to_cramers_v(
    chi_sq, 
    n,
    nrow = 2,
    ncol = 2,
    adjust = F,
    alternative = "two.sided"
  ) |>
    dplyr::rename(r = Cramers_v)

}

#' Effect size - Log odds ratio
#' 
#' Calculates effect size from log odds ratios
#'
#' @param lor Should be orig_coef_value/repli_coef_value.
#' @param se Should be orig_coef_se/repli_coef_se
#'
#' @returns A tibble with the effect size and its confidence intervals.
lor_to_r <- function(lor, se) {
  
  lor_interval <- c(lor, lor - 1.96*se, lor + 1.96*se)
  
  lor_interval |>
    effectsize::logoddsratio_to_r() |>
    rlang::set_names(c("r", "CI_low", "CI_high")) |>
    tibble::enframe() |>
    tidyr::pivot_wider(names_from = name, values_from = value) |>
    dplyr::mutate(CI = "0.95", .before = CI_low)
}

#' Effect size conversions
#' 
#' Runs effect size conversions
#'
#' @param data A data frame or tibble.
#' @param key_id The primary key ID for the data.
#'
#' @returns A tibble.
convert_to_cosr <- function(data, key_id) {
  
  # All will be changed to correspond to outcomes variables
  stat_type <- names(dplyr::select(data, dplyr::ends_with("stat_type")))
  stat_value <- names(dplyr::select(data, dplyr::contains("stat_value")))
  coef_value <- names(dplyr::select(data, dplyr::contains("coef_value")))
  coef_se <- names(dplyr::select(data, dplyr::contains("coef_se")))
  sample_size <- names(
    dplyr::select(data, dplyr::contains("sample_size_value_effective"))
  )
  df1 <- names(dplyr::select(data, dplyr::contains("df_1")))
  df2 <- names(dplyr::select(data, dplyr::contains("dof_2")))
  
  t_table <- data |>
    dplyr::filter(!!as.name(stat_type) == "t") |>
    dplyr::filter(is.na(lor_conversion)) |>
    dplyr::mutate(
      convert_r = effectsize::t_to_r(get({{ stat_value }}), get({{ df1 }}))
    )
  
  z_table <- data |> 
    dplyr::filter(!!as.name(stat_type) == "z") |>
    dplyr::filter(is.na(lor_conversion)) |>
    dplyr::mutate(
      convert_r = effectsize::z_to_r(
        get({{ stat_value }}), get({{ sample_size }})
      )
    )
  
  chi_table <- data |>
    dplyr::filter(
      !!as.name(stat_type) == "chi_squared" | 
        !!as.name(stat_type) == "delta_g_squared"
    ) |>
    dplyr::filter(is.na(lor_conversion)) |>
    dplyr::mutate(
      convert_r = convert_chi(
        get({{ stat_value }}), get({{ sample_size }})
      )
    )
  
  F_table <- data |>
    dplyr::filter(!!as.name(stat_type) == "F" & !!as.name(df1) == 1) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      convert_r = effectsize::F_to_r(
        get({{ stat_value }}), get({{ df1 }}), get({{ df2 }})
      )
    )
  
  cf_table <- data |>
    dplyr::filter(!!as.name(stat_type) == "F" & !!as.name(df1) > 1) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      convert_r = convert_cf(
        get({{ stat_value }}), get({{ df1 }}), get({{ df2 }})
      )
    )
  
  # This should be empty for replications, but keeping it for transparency 
  lor_table <- data |>
    dplyr::filter(!is.na(lor_conversion))
  
  if (nrow(lor_table) != 0) {
    lor_table <- lor_table |>
      dplyr::rowwise() |>
      dplyr::mutate(
        convert_r = lor_to_r(get({{ coef_value }}), get({{ coef_se }}))
      )
  }

  rbind(t_table, 
        F_table, 
        cf_table, 
        z_table, 
        chi_table,
        lor_table) |>
    tidyr::unnest(convert_r) |>
    dplyr::rename(cos_r = r,
                  cos_r_lb = CI_low,
                  cos_r_ub = CI_high) |>
    dplyr::select({{ key_id }},
                  cos_r,
                  cos_r_lb,
                  cos_r_ub)
  
}