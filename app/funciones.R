filtrar_datos <- function(datos, alcaldia_seleccionada, fecha_inicio_seleccionada, fecha_fin_seleccionada){
  datos %>%
    filter(alcaldia_hecho == alcaldia_seleccionada,
           fecha_inicio >= as.Date(fecha_inicio_seleccionada),
           fecha_inicio <= as.Date(fecha_fin_seleccionada))
}
