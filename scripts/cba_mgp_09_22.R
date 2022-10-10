
# Librerías ---------------------------------------------------------------

require(tidyverse)
require(tidyr)
require(readxl)
require(formattable)
require(kableExtra)

# Bases de datos ----------------------------------------------------------

cba_pam <-  read_excel("data/cba_pampeana.xlsx")
mdp_cba <-  read_excel("data/mdp_cba_09_22.xlsx")

# Análisis ----------------------------------------------------------------

mdp_cba_resumen <- mdp_cba |> 
  group_by(producto_cba) |> 
  summarise(
    mediana = median(precioxunidad),
    media_arit = mean(precioxunidad),
    media_ar_t = mean(precioxunidad, trim = 0.1),
    media_geom = exp(mean(log(precioxunidad)))
    )

mdp_cba_resumen <- mdp_cba_resumen |> full_join(cba_pam)

mdp_cba_resumen <- mdp_cba_resumen |> mutate(mediana_mes = mediana*pampeana)

mdp_cba_resumen <- mdp_cba_resumen |> mutate(media_arit_mes = media_arit*pampeana)

mdp_cba_resumen <- mdp_cba_resumen |> mutate(media_ar_t_mes = media_ar_t*pampeana)

mdp_cba_resumen <- mdp_cba_resumen |> mutate(media_geom_mes = media_geom*pampeana)


(estadisticas <- mdp_cba_resumen |> summarise(
  mediana     = sum(mediana_mes),
  `media aritmética`  = sum(media_arit_mes),
  `media aritmética truncada` = sum(media_ar_t_mes),
  `media geométrica`  = sum(media_geom_mes)
) |> pivot_longer(cols = mediana:`media geométrica`) |> 
  rename(Estadísticas = name, Resultados = value))


estadisticas$Resultados <- color_bar("lightgreen")(estadisticas$Resultados)

kbl(estadisticas, escape = F) |> 
  kable_styling(font_size = 22) |> 
  kable_paper("hover", full_width = F) |>
  column_spec(2, width = "5cm") |>
  add_header_above(c("Índices de Precios | Septiembre 22" = 2))
 

tibble(
  hogar = c('3 integrantes', '4 integrantes', '5 integrantes'),
  cba   = c(2.46*sum(mdp_cba_resumen$media_geom_mes),
            3.09*sum(mdp_cba_resumen$media_geom_mes),
            3.25*sum(mdp_cba_resumen$media_geom_mes))
)
