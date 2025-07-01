data_reta <- readxl::read_excel(path = "data/data_perfil_mensal.xlsx", sheet = "reta_aloc") |>
  dplyr::mutate(date = zoo::as.Date(date)) |>
  dplyr::mutate(r_cota = cota/dplyr::lag(cota) - 1) |>
  dplyr::mutate(var = zoo::rollapply(r_cota, 21, stats::quantile, probs = .05, na.rm = T, fill = NA, align = "right")) |>
  dplyr::mutate(cvar = mean(r_cota[r_cota < quantile(r_cota, 0.01, na.rm = T)], na.rm = T))
