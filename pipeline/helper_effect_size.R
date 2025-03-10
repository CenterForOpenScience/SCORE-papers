# Effect Size Functions
# F, weird Edge Case, df1 > 1
convert_cf <- function(Fvalue, df1, df2) {
  
  F_to_f(Fvalue, df1, df2, alternative = "two.sided") %>% 
    as_tibble() %>% 
    mutate(across(-c(CI), ~ .x*2)) %>% # f to d
    mutate(across(-c(CI), d_to_r)) %>% # d to r
    rename(r = Cohens_f_partial)
  
}

# Chi
convert_chi <- function(chi_sq, n) {
  
  r <- chisq_to_cramers_v(chi_sq, 
                          n,
                          nrow = 2,
                          ncol = 2,
                          adjust = F,
                          alternative = "two.sided") %>%
    rename(r = Cramers_v)
  
  
}

# The lor argument should be orig_coef_value/repli_coef_value and the 
# se argument should be orig_coef_se/repli_coef_se
lor_to_r <- function(lor, se) {
  lor_interval <- c(lor, lor - 1.96*se, lor + 1.96*se)
  lor_interval %>%
    logoddsratio_to_r() %>%
    set_names(c("r", "CI_low", "CI_high")) %>%
    enframe() %>%
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(CI = "0.95", .before = CI_low)
}

# Run conversions
convert_to_cosr <- function(data, key_id) {
  
  # All will be changed to correspond to outcomes variables
  stat_type <- names(select(data, ends_with("stat_type")))
  stat_value <- names(select(data, contains("stat_value")))
  coef_value <- names(select(data, contains("coef_value")))
  coef_se <- names(select(data, contains("coef_se")))
  sample_size <- names(select(data, contains("sample_size_value_effective")))
  df1 <- names(select(data, contains("df_1")))
  df2 <- names(select(data, contains("dof_2")))
  
  t_table <- data %>%
    filter(!!as.name(stat_type) == "t") %>%
    filter(is.na(lor_conversion)) %>%
    mutate(convert_r = t_to_r(get({{ stat_value }}),
                              get({{ df1 }})))
  
  z_table <- data %>% 
    filter(!!as.name(stat_type) == "z") %>%
    filter(is.na(lor_conversion)) %>%
    mutate(convert_r = z_to_r(get({{ stat_value }}),
                              get({{ sample_size }})))
  
  chi_table <- data %>%
    filter(!!as.name(stat_type) == "chi_squared" |
             !!as.name(stat_type) == "delta_g_squared") %>%
    filter(is.na(lor_conversion)) %>%
    mutate(convert_r = convert_chi(get({{ stat_value }}),
                                   get({{ sample_size }})))
  
  
  F_table <- data %>%
    filter(!!as.name(stat_type) == "F" & !!as.name(df1) == 1) %>%
    rowwise() %>%
    mutate(convert_r = F_to_r(get({{ stat_value }}),
                              get({{ df1 }}),
                              get({{ df2 }})))
  
  cf_table <- data %>%
    filter(!!as.name(stat_type) == "F" & !!as.name(df1) > 1) %>%
    rowwise() %>%
    mutate(convert_r = convert_cf(get({{ stat_value }}),
                                  get({{ df1 }}),
                                  get({{ df2 }})))
  
  lor_table <- data %>%
    filter(!is.na(lor_conversion)) %>%
    rowwise() %>%
    mutate(convert_r = lor_to_r(get({{ coef_value }}),
                                get({{ coef_se }})))
  
  rbind(t_table, 
        F_table, 
        cf_table, 
        z_table, 
        chi_table,
        lor_table) %>%
    unnest(convert_r) %>%
    rename(cos_r = r,
           cos_r_lb = CI_low,
           cos_r_ub = CI_high) %>%
    select({{key_id}},
           cos_r,
           cos_r_lb,
           cos_r_ub)
  
}