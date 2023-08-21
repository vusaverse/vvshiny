test_that("sankey_app returns a Shiny app without error", {
  df <- dplyr::tribble(
    ~OPL_Onderdeel_CROHO_examen, ~OPL_Onderdeel_CROHO_instroom,
    ~OPL_CBS_Label_rubriek_examen, ~OPL_CBS_Label_rubriek_instroom,
    "GEDRAG EN MAATSCHAPPIJ", "GEZONDHEIDSZORG", "sociale geografie", "(huis)arts, specialist, geneeskunde",
    "GEDRAG EN MAATSCHAPPIJ", "RECHT EN MAATSCHAPPIJ", "sociale geografie", "sociale geografie",
    "GEDRAG EN MAATSCHAPPIJ", "RECHT", "sociale geografie", "notariaat",
    "RECHT", "GEZONDHEIDSZORG", "notariaat", "notariaat",
    "TAAL EN CULTUUR", "RECHT", "niet westerse talen en culturen", "notariaat"
  )

  df_config <- dplyr::tribble(
    ~Categorie, ~Veldnaam, ~Veldnaam_gebruiker, ~target, ~left_var, ~right_var, ~title_start, ~title_end, ~position_label_y,
    "Doorstroom vanuit B", "OPL_Onderdeel_CROHO_examen", "B Croho sector", "OPL_Onderdeel_CROHO_examen", 1, NA, "Waar stromen ", "Bachelor gediplomeerden naar toe?", "right",
    "Doorstroom vanuit B", "OPL_CBS_Label_rubriek_examen", "B ISCED-F Rubriek", "OPL_CBS_Label_rubriek_examen", 1, NA, "Waar stromen ", "Bachelor gediplomeerden naar toe?", "right",
    "Doorstroom vanuit B", "OPL_CBS_Label_richting_examen", "B ISCED-F Richting", "OPL_CBS_Label_richting_examen", 1, NA, "Waar stromen ", "Bachelor gediplomeerden naar toe?", "right",
    "Instroom bij M", "OPL_Onderdeel_CROHO_instroom", "M Croho sector", "OPL_Onderdeel_CROHO_instroom", NA, 1, "Waarvandaan stromen ", "Master studenten in?", "left",
    "Instroom bij M", "OPL_CBS_Label_rubriek_instroom", "M ISCED-F Rubriek", "OPL_CBS_Label_rubriek_instroom", NA, 1, "Waarvandaan stromen ", "Master studenten in?", "left",
    "Instroom bij M", "OPL_CBS_Label_richting_instroom", "M ISCED-F Richting", "OPL_CBS_Label_richting_instroom", NA, 1, "Waarvandaan stromen ", "Master studenten in?", "left"
  )

  app <- sankey_app(df, df_config)
  expect_silent(app)
  expect_s3_class(app, "shiny.appobj")

})
