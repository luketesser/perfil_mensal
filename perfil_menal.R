
# Data Analysis -----------------------------------------------------------


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
  dplyr::mutate(beta_imab5_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[20]]) |>
  dplyr::mutate(log_returns = log(cota / dplyr::lag(cota))) |>
  dplyr::mutate(vol = 100*zoo::rollapply(log_returns, 21, stats::sd, fill = NA, align = "right")*252^(1/2)) |>
  dplyr::mutate(cvar_roll = 100*zoo::rollapply(log_returns, 21, function(x) mean(x[x < quantile(x, 0.01, na.rm = T)]), fill = NA, align = "right")*21^(1/2))


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
  dplyr::mutate(beta_imab5_pvalue = summary(stats::lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5)))$coefficients[[20]]) |>
  dplyr::mutate(log_returns = log(cota / dplyr::lag(cota))) |>
  dplyr::mutate(vol = 100*zoo::rollapply(log_returns, 21, stats::sd, fill = NA, align = "right")*252^(1/2)) |>
  dplyr::mutate(log_ret_ibov = log(ibov/dplyr::lag(ibov))) |>
  dplyr::mutate(vol_ibov = 100*zoo::rollapply(log_ret_ibov, 21, stats::sd, fill = NA, align = "right")*252^(1/2)) |>
  dplyr::mutate(cvar_roll = 100*zoo::rollapply(log_returns, 21, function(x) mean(x[x < quantile(x, 0.01, na.rm = T)]), fill = NA, align = "right")*21^(1/2))

data_linear_s <- readxl::read_excel(path = "data/data_perfil_mensal.xlsx", sheet = "linear_select") |>
  dplyr::mutate(date = zoo::as.Date(date)) |>
  dplyr::mutate(r_cota = cota/dplyr::lag(cota)) |>
  dplyr::mutate(r_cota2 = cota/dplyr::lag(cota) - 1) |>
  dplyr::mutate(var = zoo::rollapply(r_cota2, 21, stats::quantile, probs = .05, na.rm = T, fill = NA, align = "right")) |>
  dplyr::mutate(cvar = mean(r_cota2[r_cota2 < quantile(r_cota2, 0.01, na.rm = T)], na.rm = T)) |>
  dplyr::mutate(r_idex = idex/dplyr::lag(idex)) |>
  dplyr::mutate(beta_idex = summary(stats::lm(log(r_cota) ~ log(r_idex)))$coefficients[[2]]) |>
  dplyr::mutate(beta_idex_pvalue = summary(stats::lm(log(r_cota) ~ log(r_idex)))$coefficients[[8]])


# Graphs ------------------------------------------------------------------

readxl::read_excel("data/data_perfil_mensal.xlsx", sheet = "reta_aloc") |>
  dplyr::mutate(Data = zoo::as.Date(date)) |>
  dplyr::mutate(log_returns = log(cota / dplyr::lag(cota))) |>
  tidyr::drop_na() |>
  dplyr::mutate(vol = 100*zoo::rollapply(log_returns, 21, stats::sd, fill = NA, align = "right")*252^(1/2)) |>
  dplyr::mutate(var = 100*zoo::rollapply(log_returns, 21, stats::quantile, probs = .05, fill = NA, align = "right")*21^(1/2)) |>
  dplyr::mutate(cvar = 100*zoo::rollapply(log_returns, 21, function(x) mean(x[x < quantile(x, 0.01)]), fill = NA, align = "right")*21^(1/2)) |>
  ggplot2::ggplot(ggplot2::aes(x = Data)) +
  ggplot2::geom_line(ggplot2::aes(y = vol, color = "Volatilidade"), size = 1) +
  ggplot2::geom_line(ggplot2::aes(y = var, color = "VaR (95%)"), size = 1) +
  ggplot2::geom_line(ggplot2::aes(y = cvar, color = "CVaR (99%)"), size = 1) +
  ggplot2::scale_y_continuous(name = "Volatilidade (%)", labels = scales::percent_format(scale = 1),
                              sec.axis = ggplot2::sec_axis(~., name = "VaR & CVaR Mensal (%)", labels = scales::percent_format(scale = 1))) +
  ggplot2::scale_color_manual(values = c("Volatilidade" = "blue", "VaR (95%)" = "red", "CVaR (99%)" = "purple")) +
  ggplot2::labs(x = NULL, y = NULL, title = "Reta Alocação", color = "Legenda") +
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = "bottom")

readxl::read_excel("data/data_perfil_mensal.xlsx", sheet = "reta_aloc") |>
  dplyr::mutate(Data = zoo::as.Date(date)) |>
  dplyr::mutate(log_returns = log(cota / dplyr::lag(cota))) |>
  dplyr::mutate(r_cota = cota/dplyr::lag(cota)) |>
  dplyr::mutate(r_cambio = cambio/dplyr::lag(cambio)) |>
  dplyr::mutate(r_ibov = ibov/dplyr::lag(ibov)) |>
  dplyr::mutate(r_imab5 = ima_b_5/dplyr::lag(ima_b_5)) |>
  dplyr::mutate(log_ret_imab = log(ima_b_5/dplyr::lag(ima_b_5))) |>
  tidyr::drop_na() |>
  dplyr::mutate(vol = 100*zoo::rollapply(log_returns, 21, stats::sd, fill = NA, align = "right")*252^(1/2)) |>
  dplyr::mutate(vol_ima = 100*zoo::rollapply(log_ret_imab, 21, stats::sd, fill = NA, align = "right")*252^(1/2)) |>
  dplyr::mutate(beta_ibov = 100*slider::slide_dbl(.x = dplyr::cur_data_all(), .f = ~ {fit <- lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5), data = .x)
  stats::coef(fit)[["log(r_ibov)"]]}, .before = 20, .complete = T)) |>
  dplyr::mutate(beta_imab = 100*slider::slide_dbl(.x = dplyr::cur_data_all(), .f = ~ {fit <- lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5), data = .x)
  stats::coef(fit)[["log(r_imab5)"]]}, .before = 20, .complete = T)) |>

  tidyr::pivot_longer(cols = c(vol, vol_ima, beta_ibov, beta_imab), names_to = "serie", values_to = "valor") |>
  dplyr::mutate(tipo = ifelse(grepl("^vol", serie), "Volatilidade (%)", "Sensibilidade Índices (%)"),
                serie = dplyr::recode(serie, vol = "Vol Carteira", vol_ima = "Vol ImaB 5+", beta_ibov = "Beta Ibov", beta_imab = "Beta ImaB 5+")) |>

  ggplot2::ggplot(ggplot2::aes(x = Data, y = valor, color = serie)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::facet_wrap(~tipo, ncol = 1, scales = "free_y") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::labs(x = NULL, y = NULL, title = "Reta Alocação", color = "Legenda") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")




readxl::read_excel("data/data_perfil_mensal.xlsx", sheet = "long_term") |>
  dplyr::mutate(Data = zoo::as.Date(date)) |>
  dplyr::mutate(log_returns = log(cota / dplyr::lag(cota))) |>
  dplyr::mutate(r_cota = cota/dplyr::lag(cota)) |>
  dplyr::mutate(r_cambio = cambio/dplyr::lag(cambio)) |>
  dplyr::mutate(r_ibov = ibov/dplyr::lag(ibov)) |>
  dplyr::mutate(r_imab5 = ima_b_5/dplyr::lag(ima_b_5)) |>
  dplyr::mutate(log_ret_ibov = log(ibov/dplyr::lag(ibov))) |>
  tidyr::drop_na() |>
  dplyr::mutate(vol = 100*zoo::rollapply(log_returns, 21, stats::sd, fill = NA, align = "right")*252^(1/2)) |>
  dplyr::mutate(vol_ibov = 100*zoo::rollapply(log_ret_ibov, 21, stats::sd, fill = NA, align = "right")*252^(1/2)) |>
  dplyr::mutate(beta_ibov = 100*slider::slide_dbl(.x = dplyr::cur_data_all(), .f = ~ {fit <- lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5), data = .x)
  stats::coef(fit)[["log(r_ibov)"]]}, .before = 20, .complete = T)) |>
  dplyr::mutate(beta_imab = 100*slider::slide_dbl(.x = dplyr::cur_data_all(), .f = ~ {fit <- lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5), data = .x)
  stats::coef(fit)[["log(r_imab5)"]]}, .before = 20, .complete = T)) |>
  dplyr::mutate(beta_cambio = 100*slider::slide_dbl(.x = dplyr::cur_data_all(), .f = ~ {fit <- lm(log(r_cota) ~ log(selic) + log(r_cambio) + log(r_ibov) + log(r_imab5), data = .x)
  stats::coef(fit)[["log(r_cambio)"]]}, .before = 20, .complete = T)) |>

  tidyr::pivot_longer(cols = c(vol, vol_ibov, beta_ibov, beta_imab, beta_cambio), names_to = "serie", values_to = "valor") |>
  dplyr::mutate(tipo = ifelse(grepl("^vol", serie), "Volatilidade (%)", "Sensibilidade Índices (%)"),
                serie = dplyr::recode(serie, vol = "Vol Carteira", vol_ibov = "Vol Ibov", beta_ibov = "Beta Ibov", beta_imab = "Beta ImaB 5+",
                                      beta_cambio = "Beta Câmbio")) |>

  ggplot2::ggplot(ggplot2::aes(x = Data, y = valor, color = serie)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::facet_wrap(~tipo, ncol = 1, scales = "free_y") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::labs(x = NULL, y = NULL, title = "Long Term LB", color = "Legenda") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")


# Save data ---------------------------------------------------------------


write.table(data_reta, file = "202508_reta.csv", sep = ";", dec = ",", row.names = F)

write.table(data_long, file = "202508_long.csv", sep = ";", dec = ",", row.names = F)

write.table(data_linear_s, file = "202508_linear_s.csv", sep = ";", dec = ",", row.names = F)


# Tables ------------------------------------------------------------------

reta_table <- data_reta |>
  dplyr::filter(date == dplyr::last(date)) |>
  dplyr::select(var, cvar, cvar_roll, c(14:21), 23)

long_table <- data_long |>
  dplyr::filter(date == dplyr::last(date)) |>
  dplyr::select(var, cvar, cvar_roll, c(14:21), 23)

linear_s_table <- data_linear_s |>
  dplyr::filter(date == dplyr::last(date)) |>
  dplyr::select(c(6, 7, 9, 10))

table_data <- dplyr::full_join(reta_table, long_table) |>
  dplyr::full_join(linear_s_table) |>
  tibble::add_column(Fundo = c("Reta Alocação", "Long Term LB", "Linear Select"), .before = "var")

table_data |> # 1000 x 320
  dplyr::mutate(dplyr::across(.cols = where(is.numeric), .fns = ~ round(.x, 4))) |>
  gt::gt() |>
  gt::tab_header(title = "Métricas de Risco", subtitle = "Medidas de Risco Selecionadas e Parâmetros de Sensibilidade") |>
  gt::fmt_number(columns = where(is.numeric), decimals = 4) |>
  gt::cols_label(
    Fundo = "Fundo",
    var = "VaR",
    cvar = "CVaR",
    cvar_roll = "Rolling CVaR a.a.",
    beta_selic = "Beta (Selic)",
    beta_selic_pvalue = "Beta (Selic) p-valor",
    beta_cambio = "Beta (FX)",
    beta_cambio_pvalue = "Beta (FX) p-valor",
    beta_ibov = "Beta (Ibov)",
    beta_ibov_pvalue = "Beta (Ibov) p-valor",
    beta_imab5 = "Beta (IMA-B 5+)",
    beta_imab5_pvalue = "Beta (IMA-B 5+) p-valor",
    beta_idex = "Beta Idex CDI",
    beta_idex = "Beta Idex p-valor",
    vol  = "Vol"
  ) |>
  gt::tab_options(
    table.font.size = "small",
    table.border.top.width = gt::px(2),
    table.border.bottom.width = gt::px(2),
    heading.align = "center"
  )



