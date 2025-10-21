library(pacman)
pacman::p_load(
  munsell,
  shiny,
  here,
  dplyr,
  leaflet,
  yaml,
  sf,
  stringi,
  RColorBrewer,
  ggplot2
)

# app.R
library(shiny)
library(here)
library(dplyr)
library(leaflet)
library(yaml)
library(sf)
library(stringi)
library(RColorBrewer)
library(munsell)
library(ggplot2)

#here::i_am("procesamiento/app.R")
#source(here("procesamiento/funciones.R")) 

# ----------------------------
# Funciones
# ----------------------------
normalizar <- function(x){
  x <- tolower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- trimws(x)
  x
}

toCamel <- function(x){
  x <- tolower(x)
  x <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
  x
}

# ----------------------------
# Cargar datos
# ----------------------------
datos <- readRDS("datos.rds") %>%
  mutate(
    latitud  = as.numeric(latitud),
    longitud = as.numeric(longitud),
    alcaldia_hecho = ifelse(is.na(alcaldia_hecho), "SIN ALCALDIA", alcaldia_hecho),
    categoria_delito_norm = normalizar(categoria_delito),
    alcaldia_hecho_norm = normalizar(alcaldia_hecho)
  ) %>%
  filter(!is.na(latitud) & !is.na(longitud))

# ----------------------------
# Cargar usuarios
# ----------------------------
usuarios <- yaml::read_yaml("usuarios.yml")$usuarios
usuarios <- do.call(rbind.data.frame, lapply(usuarios, as.data.frame))
usuarios[] <- lapply(usuarios, as.character)

# ----------------------------
# Cargar capas y normalizar
# ----------------------------
alcaldias  <- st_read("Alcaldias.shp") %>%
  st_transform(4326) %>%
  mutate(nomgeo_norm = normalizar(nomgeo))

cuadrantes <- st_read("SSC_Cuadrantes_2025_region.shp") %>%
  st_zm() %>% st_transform(4326) %>%
  mutate(Alcaldia_norm = normalizar(Alcaldia))

sectores   <- st_read("SSC_Sectores_2025_region.shp") %>%
  st_zm() %>% st_transform(4326) %>%
  mutate(Alcaldia_norm = normalizar(Alcaldia))

# ----------------------------
# Paleta de colores mejorada
# ----------------------------
delitos_todos <- sort(unique(datos$categoria_delito))
n_delitos <- length(delitos_todos)

# Usando RColorBrewer Set3 para colores más distinguibles
pal_colores <- brewer.pal(min(n_delitos, 12), "Set3")
if(n_delitos > length(pal_colores)){
  pal_colores <- colorRampPalette(pal_colores)(n_delitos)
}

pal <- colorFactor(pal_colores, domain = delitos_todos)

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {background-image: url('cdmx.png'); background-size: cover; background-position: center; background-repeat: no-repeat; font-family: 'Poppins', sans-serif; }
      .sidebar-panel { background-color: #FFFFFF; border-radius: 15px; padding: 20px; box-shadow: 0px 4px 25px rgba(0,0,0,0.1); }
      .main-panel { background-color: #FFFFFF; border-radius: 15px; padding: 25px; box-shadow: 0px 4px 25px rgba(0,0,0,0.05); }
      .login-box { background-color: rgba(255,255,255,0.95); padding: 35px; border-radius: 20px; box-shadow: 0px 6px 25px rgba(0,0,0,0.2); width: 380px; color: #2C3E50; }
      h1.login-title, h3.login-subtitle, h4 { color: #2C3E50; }
      .welcome-left { height: 100vh; width: 100%;  background-color: #9F2242; display: flex; justify-content: center; align-items: center; flex-direction: column; color: white; text-align: center; }
      .welcome-left h1, .welcome-left h3 { color: white; }
      hr { border: 2px solid white; width: 60px; margin-bottom: 15px; }
      .leaflet-container { border-radius: 15px; }
      .checkbox-inline { margin-right: 15px; }
    "))
  ),
  uiOutput("app_ui")
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session){
  
  user <- reactiveVal(NULL)
  
  # ----------------------------
  # Cerrar sesión
  # ----------------------------
  observeEvent(input$cerrar_sesion, { user(NULL) })
  
  # ----------------------------
  # Login
  # ----------------------------
  observeEvent(input$entrar, {
    fila <- usuarios %>% filter(tolower(usuario) == tolower(input$usuario), password == input$password)
    if(nrow(fila) == 1){
      user(fila$alcaldia)
    } else {
      showModal(modalDialog(title = "Error", "Usuario o contraseña incorrectos", easyClose = TRUE))
    }
  })
  
  # ----------------------------
  # Indicador de login
  # ----------------------------
  output$usuario_logueado <- reactive({ !is.null(user()) })
  outputOptions(output, "usuario_logueado", suspendWhenHidden = FALSE)
  
  # ----------------------------
  # UI condicional
  # ----------------------------
  output$app_ui <- renderUI({
    if(is.null(user())){
      # Login
      fluidRow(
        column(6,
               div(class="welcome-left",
                   tags$img(
                     src = "lienzo_cdmx.svg",
                     alt = "Ciudad de México",
                     style = "width: 800px; height: auto;"
                   ),
                   tags$img(
                     src = "logo_adip.svg",
                     alt = "ADIP",
                     style = "width: 400px; height: auto;"
                   ),
               )
        ),
        column(6,
               div(
                 style="height:100vh; display:flex; justify-content:center; align-items:center;",
                 div(class="login-box",
                     h3("Iniciar sesión", style="text-align:center; margin-bottom:25px;"),
                     textInput("usuario", "Usuario:"),
                     passwordInput("password", "Contraseña:"),
                     actionButton("entrar", "Entrar", 
                                  style="width:100%; background-color:#235B4E; color:white; border:none; padding:12px; font-weight:bold;")
                 )
               )
        )
      )
    } else {
      # Contenido principal
      sidebarLayout(
        sidebarPanel(
          class = "sidebar-panel",
          style = "border-radius:15px; box-shadow:0px 4px 25px rgba(0,0,0,0.1); padding:20px;",
          
          actionButton("cerrar_sesion", "Cerrar sesión", 
                       style = "width:100%; background-color:#9F2241; color:white; border:none; padding:10px; font-weight:bold; margin-bottom:25px;"),
          
          h3("Filtros de visualización", style = "color:#235B4E; font-weight:bold; margin-bottom:20px;"),
          
          div(style="margin-bottom:25px;",
              tags$label("Selecciona rango de fechas:", style="font-weight:bold; color:#34495E;"),
              dateRangeInput("fechas", NULL,
                             start = min(datos$fecha_inicio),
                             end = max(datos$fecha_inicio),
                             width = "100%")
          ),
          
          div(style="margin-bottom:25px;",
              tags$label("Selecciona delitos:", style="font-weight:bold; color:#34495E;"),
              checkboxGroupInput("delitos_seleccionados", NULL,
                                 choices = delitos_todos,
                                 selected = delitos_todos[1])
          ),
          
          div(style="margin-bottom:15px;",
              tags$label("Capas de información:", style="font-weight:bold; color:#34495E;"),
              checkboxInput("mostrar_alcaldias", "Mostrar Alcaldías", value = TRUE),
              checkboxInput("mostrar_cuadrantes", "Mostrar Cuadrantes", value = TRUE),
              checkboxInput("mostrar_sectores", "Mostrar Sectores", value = TRUE)
          )
        ),
        
        mainPanel(
          class = "main-panel",
          div(
            style = "display:flex; flex-direction:column; align-items:center; margin-bottom:20px;",
            
            h2(style = "font-size:32px; font-weight:bold; color:#235B4E; margin-bottom:5px;",
               paste0("Alcaldía: ", toCamel(user()))),
            
            h5(style = "color:#7F8C8D; margin-bottom:15px;",
               paste0("Datos actualizados al: ", format(max(datos$fecha_inicio), "%d-%m-%Y"))),
            
            p(style = "color:#34495E; text-align:center; max-width:800px;",
              "Visualiza la incidencia delictiva por categoría y ubicación. Filtra por fechas, delitos y capas de información para un análisis más detallado."),
            
            div(style = "width:100%; border-radius:15px; box-shadow: 0px 4px 20px rgba(0,0,0,0.1);",
                leafletOutput("mapa", height = "650px")
            )
          )
        )
      )
    }
  })
  
  # ----------------------------
  # Datos filtrados
  # ----------------------------
  datos_filtrados <- reactive({
    req(user(), input$fechas, input$delitos_seleccionados)
    
    df <- datos %>%
      filter(
        fecha_inicio >= input$fechas[1],
        fecha_inicio <= input$fechas[2],
        categoria_delito_norm %in% normalizar(input$delitos_seleccionados)
      )
    
    if(user() != "TODAS"){
      df <- df %>% filter(alcaldia_hecho_norm == normalizar(user()))
    }
    
    df
  })
  
  # ----------------------------
  # Mapa
  # ----------------------------
  output$mapa <- renderLeaflet({
    df <- datos_filtrados()
    req(df)
    
    user_norm <- normalizar(user())
    
    if(user() != "TODAS"){
      alcaldias_user <- alcaldias %>% filter(nomgeo_norm == user_norm)
      cuadrantes_user <- cuadrantes %>% filter(Alcaldia_norm == user_norm)
      sectores_user <- sectores %>% filter(Alcaldia_norm == user_norm)
    } else {
      alcaldias_user <- alcaldias
      cuadrantes_user <- cuadrantes
      sectores_user <- sectores
    }
    
    mapa <- leaflet(df) %>%
      addProviderTiles("CartoDB.Positron", group = "Positron")
    
    if(input$mostrar_alcaldias && nrow(alcaldias_user) > 0){
      mapa <- mapa %>% addPolygons(data = alcaldias_user,
                                   color = "black", weight = 6, fill = FALSE,
                                   label = ~nomgeo,
                                   group = "Alcaldías")
    }
    if(input$mostrar_cuadrantes && nrow(cuadrantes_user) > 0){
      mapa <- mapa %>% addPolygons(data = cuadrantes_user,
                                   color = "#125C96", weight = 3, fill = FALSE,
                                   label = ~Nomenclatu,
                                   group = "Cuadrantes")
    }
    if(input$mostrar_sectores && nrow(sectores_user) > 0){
      mapa <- mapa %>% addPolygons(data = sectores_user,
                                   color = "#9F2241", weight = 5, fill = FALSE,
                                   label = ~Nombre_Sec,
                                   group = "Sectores")
    }
    
    overlay_groups <- input$delitos_seleccionados
    if(input$mostrar_alcaldias && nrow(alcaldias_user) > 0) overlay_groups <- c(overlay_groups, "Alcaldías")
    if(input$mostrar_cuadrantes && nrow(cuadrantes_user) > 0) overlay_groups <- c(overlay_groups, "Cuadrantes")
    if(input$mostrar_sectores && nrow(sectores_user) > 0) overlay_groups <- c(overlay_groups, "Sectores")
    
    mapa <- mapa %>%
      addCircleMarkers(
        data = df,
        lng = ~longitud, lat = ~latitud,
        color = "black",                 
        fillColor = ~pal(categoria_delito), 
        fillOpacity = 1,                 
        weight = 1.5,                    
        radius = 8,
        group = ~categoria_delito,
        label = ~lapply(
          paste0(
            "<div style='line-height:1.2;'>",
            "<span style='color:#9F2241; font-weight:bold;'>", categoria_delito, "</span><br>",
            "<b>Alcaldía:</b> ", toCamel(alcaldia_hecho), "<br>",
            "<b>Colonia:</b> ", toCamel(colonia_hecho), "<br>",
            "<b>Fecha:</b> ", fecha_inicio, "<br>",
            "<b>Sector:</b> ", toCamel(nombre_sec),
            "</div>"
          ),
          HTML
        )
      ) %>%
      addLayersControl(
        baseGroups = c("Positron"),
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = FALSE)
      )
    
    mapa
  })
}

# ----------------------------
# Lanzar app
# ----------------------------
shinyApp(ui = ui, server = server)
