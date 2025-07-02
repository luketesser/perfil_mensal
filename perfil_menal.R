data_reta <- readxl::read_excel(path = "data/data_perfil_mensal.xlsx", sheet = "reta_aloc") |>
  dplyr::mutate(date = zoo::as.Date(date)) |>
  dplyr::mutate(r_cota = cota/dplyr::lag(cota)) |>
  dplyr::mutate(r_cota2 = cota/dplyr::lag(cota) - 1) |>
  dplyr::mutate(var = zoo::rollapply(r_cota2, 21, stats::quantile, probs = .05, na.rm = T, fill = NA, align = "right")) |>
  dplyr::mutate(cvar = mean(r_cota2[r_cota2 < quantile(r_cota2, 0.01, na.rm = T)], na.rm = T)) |>
  dplyr::mutate(r_cambio = cambio/dplyr::lag(cambio)) |>
  dplyr::mutate(r_ibov = ibov/dplyr::lag(ibov)) |>
  dplyr::mutate(r_imab5 = ima_b_5/dplyr::lag(ima_b_5)) |>
  dplyr::mutate(beta_selic = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[2]]) |>
  dplyr::mutate(beta_selic_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[17]]) |>
  dplyr::mutate(beta_cambio = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[3]]) |>
  dplyr::mutate(beta_cambio_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[18]]) |>
  dplyr::mutate(beta_ibov = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[4]]) |>
  dplyr::mutate(beta_ibov_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[19]]) |>
  dplyr::mutate(beta_imab5 = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[5]]) |>
  dplyr::mutate(beta_imab5_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[20]])

data_long <- readxl::read_excel(path = "data/data_perfil_mensal.xlsx", sheet = "long_term") |>
  dplyr::mutate(date = zoo::as.Date(date)) |>
  dplyr::mutate(r_cota = cota/dplyr::lag(cota)) |>
  dplyr::mutate(r_cota2 = cota/dplyr::lag(cota) - 1) |>
  dplyr::mutate(var = zoo::rollapply(r_cota2, 21, stats::quantile, probs = .05, na.rm = T, fill = NA, align = "right")) |>
  dplyr::mutate(cvar = mean(r_cota2[r_cota2 < quantile(r_cota2, 0.01, na.rm = T)], na.rm = T)) |>
  dplyr::mutate(r_cambio = cambio/dplyr::lag(cambio)) |>
  dplyr::mutate(r_ibov = ibov/dplyr::lag(ibov)) |>
  dplyr::mutate(r_imab5 = ima_b_5/dplyr::lag(ima_b_5)) |>
  dplyr::mutate(beta_selic = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[2]]) |>
  dplyr::mutate(beta_selic_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[17]]) |>
  dplyr::mutate(beta_cambio = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[3]]) |>
  dplyr::mutate(beta_cambio_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[18]]) |>
  dplyr::mutate(beta_ibov = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[4]]) |>
  dplyr::mutate(beta_ibov_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[19]]) |>
  dplyr::mutate(beta_imab5 = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[5]]) |>
  dplyr::mutate(beta_imab5_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[20]])

data_linear_s <- readxl::read_excel(path = "data/data_perfil_mensal.xlsx", sheet = "linear_select") |>
  dplyr::mutate(date = zoo::as.Date(date)) |>
  dplyr::mutate(r_cota = cota/dplyr::lag(cota)) |>
  dplyr::mutate(r_cota2 = cota/dplyr::lag(cota) - 1) |>
  dplyr::mutate(var = zoo::rollapply(r_cota2, 21, stats::quantile, probs = .05, na.rm = T, fill = NA, align = "right")) |>
  dplyr::mutate(cvar = mean(r_cota2[r_cota2 < quantile(r_cota2, 0.01, na.rm = T)], na.rm = T)) |>
  dplyr::mutate(r_idex = idex/dplyr::lag(idex)) |>
  dplyr::mutate(beta_idex = summary(stats::lm(log(r_cota) ~ log(r_idex)))$coefficients[[2]]) |>
  dplyr::mutate(beta_idex_pvalue = summary(stats::lm(log(r_cota) ~ log(r_idex)))$coefficients[[8]])
