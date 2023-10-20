library(tidyverse)
library(magrittr)
library(openxlsx)

setwd("~/Documents/r/psu_2022/")

psu <- read_delim(
  "psu_2022_colegios.csv",
  delim = "|",
  name_repair = "universal"
)

simce <- read.xlsx(
  "simce2m2022_rbd_final.xlsx",
  sheet = 1
)  %>%
  select(rbd, cod_depe1, cod_grupo, cod_rural_rbd) %>%
  mutate(
    cod_depe1 = recode_factor(
      cod_depe1,
      `1` = "Municipal Corporación",
      `2` = "Municipal DAEM",
      `3` = "Particular subvencionado",
      `4` = "Particular pagado",
      `5` = "Corporación de administración delegada",
      `6` = "Servicio Local de Educación"
    ),
    cod_grupo = recode_factor(
      cod_grupo,
      `1` = "Bajo",
      `2` = "Medio Bajo",
      `3` = "Medio",
      `4` = "Medio Alto",
      `5` = "Alto"
    ),
    cod_rural_rbd = recode_factor(
      cod_rural_rbd,
      `1` = "Urbano",
      `2` = "Rural"
    )
  ) %>%
  rename(
    tipo.dependencia = cod_depe1,
    nivel.socioeconómico = cod_grupo,
    ruralidad = cod_rural_rbd
  )

datos_completos <- left_join(
  psu,
  simce,
  by = join_by(rbd == rbd)
) %>%
  arrange(-promedio.matemática, -promedio.lectura)

datos <- datos_completos  %>%
  select(
    rbd,
    nombre,
    promedio.matemática,
    promedio.lectura,
    promedio.ciencias,
    promedio.historia,
    tipo.dependencia,
    nivel.socioeconómico
  ) %>%
  drop_na()

todas_dep <- datos  %>%
  select(tipo.dependencia)  %>%
  unique()  %>%
  pull()

dep_resumidas <- c(
  "Pagado",
  "Publico",
  "Publico",
  "Subvencionado",
  "Publico",
  "Publico"
)

datos %<>%
  mutate(dependencia = plyr::mapvalues(
    tipo.dependencia,
    from = todas_dep,
    to = dep_resumidas
  ))

ggplot(data = datos) +
  geom_boxplot(mapping = aes(x = promedio.matemática, y = dependencia))

# ggplot(data = datos) +
#   geom_bar(
#     mapping = aes(x = nivel.socioeconómico, fill = dependencia),
#     position = position_dodge2(preserve = "single")
#   )

ggplot(data = datos) +
  geom_bar(mapping = aes(y = fct_rev(nivel.socioeconómico))) +
  facet_grid(rows = vars(dependencia))

datos  %>%
  group_by(dependencia, nivel.socioeconómico) %>%
  summarize(n())

# # A tibble: 11 × 3
# # Groups:   dependencia [3]
#    dependencia   nivel.socioeconómico `n()`
#    <fct>         <fct>                <int>
#  1 Publico       Bajo                   904
#  2 Publico       Medio Bajo             567
#  3 Publico       Medio                  144
#  4 Publico       Medio Alto              17
#  5 Subvencionado Bajo                   347
#  6 Subvencionado Medio Bajo             710
#  7 Subvencionado Medio                  605
#  8 Subvencionado Medio Alto             302
#  9 Subvencionado Alto                    15
# 10 Pagado        Medio Alto              32
# 11 Pagado        Alto                   402

ggplot(data = datos) +
  geom_boxplot(mapping = aes(x = promedio.matemática, y = nivel.socioeconómico)) +
  facet_grid(rows = vars(dependencia))

datos %>%
  group_by(dependencia, nivel.socioeconómico) %>%
  summarise(
    q1 = quantile(
      promedio.matemática,
      probs = 0.25,
      na.rm = TRUE,
      names = FALSE,
      type = 1
    ),
    q3 = quantile(
      promedio.matemática,
      probs = 0.75,
      na.rm = TRUE,
      names = FALSE,
      type = 1
    ),
    diq = q3 - q1,
    low = q1 - diq * 1.5,
    high = q3 + diq * 1.5
  )

# # A tibble: 11 × 7
# # Groups:   dependencia [3]
#    dependencia   nivel.socioeconómico    q1    q3   diq   low  high
#    <fct>         <fct>                <dbl> <dbl> <dbl> <dbl> <dbl>
#  1 Publico       Bajo                  414.  456.  42.3  350.  519.
#  2 Publico       Medio Bajo            425.  468.  42.1  362.  531.
#  3 Publico       Medio                 450.  518.  67.3  350.  619.
#  4 Publico       Medio Alto            480.  559.  78.9  362.  678.
#  5 Subvencionado Bajo                  421.  467.  45.9  352.  536.
#  6 Subvencionado Medio Bajo            440.  483.  42.6  376.  547.
#  7 Subvencionado Medio                 469.  511.  41.9  406.  574.
#  8 Subvencionado Medio Alto            502.  549.  47.2  431.  620.
#  9 Subvencionado Alto                  547.  596.  49.6  472.  671.
# 10 Pagado        Medio Alto            471   540.  68.9  368.  643.
# 11 Pagado        Alto                  545   640.  94.7  403.  782.

datos_agrupados <- datos %>%
  group_by(dependencia, nivel.socioeconómico) %>%
  mutate(
    q1 = quantile(
      promedio.matemática,
      probs = 0.25,
      na.rm = TRUE,
      names = FALSE,
      type = 1
    ),
    q3 = quantile(
      promedio.matemática,
      probs = 0.75,
      na.rm = TRUE,
      names = FALSE,
      type = 1
    ),
    diq = q3 - q1,
    low = q1 - diq * 1.5,
    high = q3 + diq * 1.5
  )

datos_agrupados %>%
  filter(promedio.matemática > high) %>%
  select(dependencia, nivel.socioeconómico, rbd, nombre, promedio.matemática) %>%
  arrange(dependencia, nivel.socioeconómico) %>%
  print(n = Inf)

# ## CUADRO DE HONOR
# # A tibble: 46 × 5
# # Groups:   dependencia, nivel.socioeconómico [7]
#    dependencia   nivel.socioeconómico   rbd nombre                                                               promedio.matemática
#    <fct>         <fct>                <dbl> <chr>                                                                              <dbl>
#  1 Publico       Bajo                  1422 LICEO PEDRO DE VALDIVIA                                                             616 
#  2 Publico       Bajo                 12006 LICEO AGRICOLA SAN CARLOS                                                           564 
#  3 Publico       Bajo                  3909 LICEO BICENTENARIO DE EXCELENCIA POLIVALENTE JUVENAL HERNÁNDEZ JAQUE                548.
#  4 Publico       Bajo                  8174 LICEO BICENTENARIO INSULAR                                                          544.
#  5 Publico       Bajo                 16676 LICEO COMPLEJO EDUCACIONAL JAVIERA CARRERA                                          526 
#  6 Publico       Bajo                  9117 COLEGIO BICENTENARIO MATILDE HUICI NAVAS                                            521.
#  7 Publico       Bajo                 12759 LICEO CAMIÑA                                                                        520.
#  8 Publico       Bajo                 40024 LICEO TECNICO MUNICIPAL JUAN HOPPE GANTZ                                            520.
#  9 Publico       Medio Bajo             578 LICEO BICENTENARIO DE EXCELENCIA IGNACIO CARRERA PINTO                              627.
# 10 Publico       Medio Bajo           12332 LICEO POLITECNICO DE CON CON                                                        593 
# 11 Publico       Medio Bajo           31037 LICEO INSTITUTO CUMBRE DE CONDORES                                                  544.
# 12 Publico       Medio Bajo            8409 LICEO PIONEROS DEL SUR                                                              535 
# 13 Publico       Medio                20266 LICEO BICENTENARIO DE TEMUCO                                                        648.
# 14 Publico       Medio                31078 LICEO BICENTENARIO SAN PEDRO DE PUENTE ALTO                                         630.
# 15 Subvencionado Bajo                  4270 LICEO PARTICULAR CARLOS CONDELL                                                     627 
# 16 Subvencionado Bajo                 12719 LICEO PABLO NERUDA                                                                  546 
# 17 Subvencionado Bajo                 20219 COLEGIO BICENTENARIO DE EXCELENCIA ADENAUER                                         540.
# 18 Subvencionado Bajo                  9779 CENTRO EDUCACIONAL SANTA ROSA                                                       537.
# 19 Subvencionado Bajo                  9339 ESCUELA Y COLEGIO POLITECNICO LOS HEROES                                            536.
# 20 Subvencionado Medio Bajo           10773 COMPLEJO EDUC.PART. LUIS PASTEUR                                                    632 
# 21 Subvencionado Medio Bajo           16441 LICEO BICENTENARIO DE EXCELENCIA SANTA MARTA                                        590.
# 22 Subvencionado Medio Bajo           11706 LICEO PARTICULAR LOS ANDES                                                          564 
# 23 Subvencionado Medio Bajo           25070 COLEGIO NAVARRA                                                                     564.
# 24 Subvencionado Medio Bajo           13542 COLEGIO IRMA SALAS SILVA                                                            562.
# 25 Subvencionado Medio Bajo           22560 COLEGIO COLONOS DE ALERCE                                                           562.
# 26 Subvencionado Medio Bajo           16448 INSTITUTO SANTA MARTA                                                               556.
# 27 Subvencionado Medio Bajo            9646 COLEGIO POLIVALENTE FRANCISCO RAMIREZ                                               554.
# 28 Subvencionado Medio Bajo            3304 COLEGIO SAN MIGUEL ARCANGEL                                                         554.
# 29 Subvencionado Medio Bajo           22192 COLEGIO CATOLICO BEATO FEDERICO OZANAM                                              553.
# 30 Subvencionado Medio Bajo           24956 COLEGIO TECNICO PROFESIONAL NOCEDAL                                                 553 
# 31 Subvencionado Medio Bajo           15722 LICEO PARTICULAR CARDENAL RAUL SILVA HENRIQUE                                       552 
# 32 Subvencionado Medio Bajo           17635 COL. TECNICO PROFESIONAL LOS ACACIOS                                                549.
# 33 Subvencionado Medio Bajo            3796 LICEO BICENTENARIO POLIVALENTE NUESTRA SEÑORA DE LA MERCED                          548.
# 34 Subvencionado Medio                15690 COLEGIO LOS CIPRESES                                                                618.
# 35 Subvencionado Medio                16477 COLEGIO AQUELARRE                                                                   600.
# 36 Subvencionado Medio                24498 COLEGIO SAN JOAQUIN                                                                 592.
# 37 Subvencionado Medio                24486 COLEGIO POLIVALENTE SAN RAFAEL                                                      592.
# 38 Subvencionado Medio                 4822 COLEGIO EL REFUGIO                                                                  591.
# 39 Subvencionado Medio                25282 COLEGIO POLIV. CARDENAL RAUL SILVA HENRIQUEZ                                        590.
# 40 Subvencionado Medio                25781 COLEGIO BICENTENARIO ARZOBISPO CRESCENTE ERRAZURIZ                                  586.
# 41 Subvencionado Medio                 9947 COLEGIO INDUSTRIAL PART. DON ORIONE                                                 583 
# 42 Subvencionado Medio                12085 COLEGIO CRISTIANO EMMANUEL                                                          577.
# 43 Subvencionado Medio Alto           25961 COLEGIO PALMARES                                                                    648 
# 44 Subvencionado Medio Alto            9905 LICEO SAN PEDRO POVEDA                                                              640.
# 45 Subvencionado Medio Alto           41899 COLEGIO SAN JORGE                                                                   626.
# 46 Subvencionado Medio Alto           22536 CENTRO EDUCACIONAL SAN SEBASTIAN DE ANCUD                                           621.

datos_agrupados %>%
  filter(promedio.matemática < low) %>%
  select(dependencia, nivel.socioeconómico, rbd, nombre, promedio.matemática) %>%
  arrange(dependencia, nivel.socioeconómico) %>%
  print(n = Inf)

# ## DESTACADOS POR SU BAJO RENDIMIENTO
# # A tibble: 48 × 5
# # Groups:   dependencia, nivel.socioeconómico [8]
#    dependencia   nivel.socioeconómico   rbd nombre                                         promedio.matemática
#    <fct>         <fct>                <dbl> <chr>                                                        <dbl>
#  1 Publico       Bajo                  9695 LICEO POLIVALENTE EUGENIO PEREIRA SALAS                       348.
#  2 Publico       Bajo                 10780 LICEO POLIV. HNOS. SOTOMAYOR BAEZA                            338 
#  3 Publico       Bajo                 10255 COMPLEJO EDUCACIONAL JUANITA FERNANDEZ SOLAR                  338.
#  4 Publico       Bajo                  9601 CENTRO EDUCACIONAL MUNICIPAL SAN RAMON                        332.
#  5 Publico       Bajo                  1167 LICEO Y ESCUELA MUNICIPAL                                     306 
#  6 Publico       Bajo                 22022 LICEO IGNACIO CARRERA PINTO                                   305 
#  7 Publico       Bajo                 11287 LICEO BERTA ZAMORANO LIZANA                                   302.
#  8 Publico       Bajo                  5216 LICEO COMERCIAL ARMANDO BRAVO BRAVO                           289 
#  9 Publico       Bajo                  4616 ESCUELA BASICA MICHAIHUE                                      276.
# 10 Publico       Bajo                  6757 LICEO POLITECNICO BENJAMIN VICUNA MACKENNA                    270.
# 11 Publico       Medio Bajo            4982 LICEO CORONEL ANTONIO SALAMANCA                               360.
# 12 Publico       Medio Bajo             379 LICEO FEDERICO VARELA                                         360.
# 13 Publico       Medio Bajo             106 INSTITUTO DEL MAR ALMIRANTE CARLOS CONDEL                     360.
# 14 Publico       Medio Bajo            1335 LICEO BICENTENARIO LICEO POLITECNICO LLAY LLAY                356.
# 15 Publico       Medio Bajo            9075 LICEO BICENTENARIO JOAQUIN EDWARDS BELLO                      352 
# 16 Publico       Medio Bajo            1148 LICEO JOSE MANUEL BORGONO NUNEZ                               347.
# 17 Publico       Medio Bajo            2939 LICEO BICENTENARIO DIEGO PORTALES                             345 
# 18 Publico       Medio Bajo            1851 LICEO POLITECNICO QUINTERO                                    337.
# 19 Publico       Medio Bajo             218 LICEO FRANCISCO DE AGUIRRE                                    334 
# 20 Publico       Medio Bajo             371 ESCUELA CARACOLES                                             322.
# 21 Publico       Medio Bajo            1048 LICEO NICOLAS FEDERICO LOHSE VARGAS                           314.
# 22 Publico       Medio                 9060 LICEO POLITECNICO PEDRO DE VALDIVIA                           348.
# 23 Subvencionado Bajo                 17848 YIRE LICEO POLITECNICO DE COIHUECO                            348.
# 24 Subvencionado Bajo                 20052 COMPLEJO EDUCACIONAL VICTORIA                                 344.
# 25 Subvencionado Bajo                 12602 ESC. BASICA Y PARV. KRONOS SCHOOL                             332.
# 26 Subvencionado Bajo                 12749 COLEGIO MARISTA HERMANO FERNANDO                              306 
# 27 Subvencionado Bajo                 22374 INSTITUTO TECNOLOGICO DEL SUR                                 294 
# 28 Subvencionado Bajo                  5923 LICEO AGRICOLA CRUZ DEL SUR                                   223 
# 29 Subvencionado Medio Bajo           40208 COLEGIO TECNICO PROFESIONAL ALTUE                             376 
# 30 Subvencionado Medio Bajo            7907 COLEGIO RAMON ANGEL JARA                                      360.
# 31 Subvencionado Medio Bajo           40457 LICEO TECNICO PROFESIONAL PABLO NERUDA                        337.
# 32 Subvencionado Medio Bajo            9328 COLEGIO POLIV. SAN SEBASTIAN DE LA FLORIDA                    336.
# 33 Subvencionado Medio Bajo           13383 COLEGIO CAMBRIDGE SCHOOL                                      310.
# 34 Subvencionado Medio Bajo            6070 COMPLEJO EDUCACIONAL PABLO SEXTO                              298.
# 35 Subvencionado Medio Bajo           25448 CENTRO EDUC. NINO DIOS DE MALLOCO                             274 
# 36 Subvencionado Medio                12741 COLEGIO LATIONOAMERICANO LAS PARINAS                          403.
# 37 Subvencionado Medio                13330 COLEGIO PARTICULAR PIERROT                                    403.
# 38 Subvencionado Medio                14418 COLEGIO PANAL                                                 398 
# 39 Subvencionado Medio                25839 ESC. BAS. Y ESP. CELEI                                        394.
# 40 Subvencionado Medio                13156 COLEGIO HÉROES DE ATACAMA                                     386.
# 41 Subvencionado Medio                17850 COLEGIO BICENTENARIO PADRE MANUEL D`ALZON                     384.
# 42 Subvencionado Medio                15643 INSTITUTO CRISTIANO EDUMASTER COLLEGE                         370.
# 43 Subvencionado Medio Alto           14775 COLEGIO MONTESOL 2                                            431.
# 44 Subvencionado Medio Alto           25718 COLEGIO PART. LOS ANGELES SANTIAGO DE SAN MIG                 427.
# 45 Subvencionado Medio Alto           20085 COLEGIO GOLDEN SCHOOL                                         413.
# 46 Subvencionado Medio Alto           18114 COLEGIO A-LAFKEN                                              409 
# 47 Pagado        Alto                  8871 COLEGIO LOS ANDES DE VITACURA                                 393.
# 48 Pagado        Alto                 10513 COLEGIO PARTICULAR NIDAL                                      352.


write_csv(datos_completos, "psu_2022_datos_completos.csv")
openxlsx::write.xlsx(datos_completos, "psu_2022_datos_completos.xlsx")

library(tableHTML)

datos_agrupados %>%
  filter(promedio.matemática > high) %>%
  select(dependencia, nivel.socioeconómico, rbd, nombre, promedio.matemática) %>%
  arrange(dependencia, nivel.socioeconómico) %>%
  tableHTML(
    rownames = FALSE,
    headers = c(
      "Tipo de dependencia",
      "Nivel socioeconómico",
      "RBD",
      "Nombre del establecimiento",
      "Promedio en Matemática"
    )
  ) %>%
  write_tableHTML(file = "cuadro_honor.html")

datos_agrupados %>%
  filter(promedio.matemática < low) %>%
  select(dependencia, nivel.socioeconómico, rbd, nombre, promedio.matemática) %>%
  arrange(dependencia, nivel.socioeconómico) %>%
  tableHTML(
    rownames = FALSE,
    headers = c(
      "Tipo de dependencia",
      "Nivel socioeconómico",
      "RBD",
      "Nombre del establecimiento",
      "Promedio en Matemática"
    )
  ) %>%
  write_tableHTML(file = "puntajes_bajos.html")

library(jsonlite)

datos_simples <- datos_completos %>%
  select(
    rbd,
    nombre,
    promedio.matemática,
    promedio.lectura,
    promedio.ciencias,
    promedio.historia,
    promedio.nem,
    promedio.ranking,
    región,
    comuna,
    tipo.dependencia,
    nivel.socioeconómico
  ) %>%
  arrange(
    -promedio.matemática,
    -promedio.lectura,
    -promedio.ciencias,
    -promedio.historia,
  ) %>%
  rowid_to_column("posición")

datos_simples %>%
  head(100) %>%
  toJSON() %>%
  write("datos_simples_100.json")

datos_simples %>%
  toJSON() %>%
  write("datos_simples.json")
