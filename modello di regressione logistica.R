rm(list = ls())

library(hoopR)
library(dplyr)
library(stringr)
library(tidyr)
install.packages("httr")
library(httr)
library(dplyr)
library(stringr)
library(ggplot2)
install.packages("patchwork")
library(patchwork)
install.packages("devtools")
install.packages("sportyR")
library(sportyR)
library(ggbasketball)
install.packages("nnet")
library(nnet)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("ys-xue/ggbasketball")
library(ggbasketball)
ggcourt(orientation = "wide")


GET(
  url = "https://github.com/sportsdataverse/sportsdataverse-data/releases/download/espn_nba_pbp/play_by_play_2024.rds",
  write_disk("play_by_play_2024.rds", overwrite = TRUE),
  timeout(300)  # timeout di 5 minuti
)


# NBA season (2023-)2024
schedule <- load_nba_schedule(seasons = 2024)

# WCF DEN-MIN
den_min <- schedule |> filter(str_detect(notes_headline, "West Semifinals"),
                              home_abbreviation == "MIN" | home_abbreviation == "DEN") 

# Games ID
id <- den_min |>  select(id) |> pull() |> sort() 

# Play-by-play NBA 2024
pbp <- readRDS("play_by_play_2024.rds")

# Game 1 to 7
for(g in 1:7) assign(paste0("game",g), pbp[which(pbp$game_id == id[g]),])

#GAME1
# Substitutions
subs_g1 <- game1 |> 
  filter(str_detect(type_text, "Substitution")) |> 
  filter(str_detect(text, "Rudy Gobert")) |> 
  select(qtr, time, end_game_seconds_remaining, text)

subs_g1

# when enters 
enter_g1 <- subs_g1 |>  
  filter(str_detect(text, "Rudy Gobert enters"))

enter_g1

# when exits
exit_g1 <- subs_g1 |> 
  filter(str_detect(text, "for Rudy Gobert")) 

exit_g1

enter_g1 <- c(2880, enter_g1$end_game_seconds_remaining) |> sort(decreasing = TRUE)
exit_g1  <- c(exit_g1$end_game_seconds_remaining, 0)     |> sort(decreasing = TRUE)

gobert_intervalli_g1 <- tibble(
  enter = enter_g1,
  exit = exit_g1)

gobert_intervalli_g1

# Funzione: restituisce TRUE se il tempo del tiro è dentro un intervallo in cui Gobert era in campo
gobert_in_campo_g1 <- function(shot_time) {
  any(gobert_intervalli_g1$enter >= shot_time & gobert_intervalli_g1$exit <= shot_time)
}

#Filtra tiri di Denver
shots_g1 <- game1 |> 
  filter(team_id == 7,
         shooting_play, !str_detect(text, "free throw")) |>  
  select(qtr,
         time,
         end_game_seconds_remaining,
         text,
         type_text,
         #athlete_id_1,
         contains("coordinate"),
         scoring_play,
         away_score,
         home_score) |> 
  mutate(
    dunk = str_detect(type_text, "Dunk|Layup|Tip"), 
    three_point = str_detect(text, "three point"),
    shooter = str_extract(text, "^(.*?) (?=misses|makes)") %>%
      coalesce(str_extract(text, "(?<=blocks )(.+?)(?= 's)")),
    .after = text
  )|>   select(-text, -type_text) 

shots_g1 |> View()

shots_g1 <- shots_g1 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )

print(shots_g1)

shots_g1 <- shots_g1 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))

# Inverti coordinate_x perché MIN era in trasferta in Gara 1
shots_g1 <- shots_g1 |> 
  mutate(coordinate_x = -coordinate_x) |> 
  mutate(gobert_on_court = sapply(end_game_seconds_remaining, gobert_in_campo_g1))


#GAME2 
#GOBERT OUT PER GARA 2
# Substitutions
subs_g2 <- game2 |> 
  filter(str_detect(type_text, "Substitution")) |> 
  filter(str_detect(text, "Rudy Gobert")) |> 
  select(qtr, time, end_game_seconds_remaining, text)

subs_g2

# when enters 
enter_g2 <- game2 |>  
  filter(str_detect(text, "Rudy Gobert enters"))

enter_g2

# when exits
exit_g2 <- subs_g2 |> 
  filter(str_detect(text, "for Rudy Gobert")) 

exit_g2

enter_g2 <- c(0, enter_g2$end_game_seconds_remaining) |> sort(decreasing = TRUE)
exit_g2  <- c(exit_g2$end_game_seconds_remaining, 0)     |> sort(decreasing = TRUE)

gobert_intervalli_g2 <- tibble(
  enter = enter_g2,
  exit = exit_g2)

gobert_intervalli_g2

# Funzione: restituisce TRUE se il tempo del tiro è dentro un intervallo in cui Gobert era in campo
gobert_in_campo_g2 <- function(shot_time) {
  any(gobert_intervalli_g2$enter >= shot_time & gobert_intervalli_g2$exit <= shot_time)
}

#Filtra tiri di Denver
shots_g2 <- game2 |> 
  filter(team_id == 7,
         shooting_play, !str_detect(text, "free throw")) |>  
  select(qtr,
         time,
         end_game_seconds_remaining,
         text,
         type_text,
         #athlete_id_1,
         contains("coordinate"),
         scoring_play,
         away_score,
         home_score) |> 
  mutate(
    dunk = str_detect(type_text, "Dunk|Layup|Tip"), 
    three_point = str_detect(text, "three point"),
    shooter = str_extract(text, "^(.*?) (?=misses|makes)") %>%
      coalesce(str_extract(text, "(?<=blocks )(.+?)(?= 's)")),
    .after = text
  )|> select(-text, -type_text) 

shots_g2 |> View()

shots_g2 <- shots_g2 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )

shots_g2 <- shots_g2 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))

# Inverti coordinate_x perché MIN era in trasferta in Gara 2
shots_g2 <- shots_g2 |> 
  mutate(coordinate_x = -coordinate_x) |> 
  mutate(gobert_on_court = sapply(end_game_seconds_remaining, gobert_in_campo_g2))


#GAME3
# Substitutions
subs_g3 <- game3 |> 
  filter(str_detect(type_text, "Substitution")) |> 
  filter(str_detect(text, "Rudy Gobert")) |> 
  select(qtr, time, end_game_seconds_remaining, text)

subs_g3

# when enters 
enter_g3 <- subs_g3 |>  
  filter(str_detect(text, "Rudy Gobert enters"))

enter_g3

# when exits
exit_g3 <- subs_g3 |> 
  filter(str_detect(text, "for Rudy Gobert")) 

exit_g3

enter_g3 <- c(2880, enter_g3$end_game_seconds_remaining) |> sort(decreasing = TRUE)
exit_g3  <- exit_g3$end_game_seconds_remaining    |> sort(decreasing = TRUE)

gobert_intervalli_g3 <- tibble(
  enter = enter_g3,
  exit = exit_g3)

gobert_intervalli_g3

# Funzione: restituisce TRUE se il tempo del tiro è dentro un intervallo in cui Gobert era in campo
gobert_in_campo_g3 <- function(shot_time) {
  any(gobert_intervalli_g3$enter >= shot_time & gobert_intervalli_g3$exit <= shot_time)
}
#Filtra tiri di Denver
shots_g3 <- game3 |> 
  filter(team_id == 7,
         shooting_play, !str_detect(text, "free throw")) |>  
  select(qtr,
         time,
         end_game_seconds_remaining,
         text,
         type_text,
         #athlete_id_1,
         contains("coordinate"),
         scoring_play,
         away_score,
         home_score) |> 
  mutate(
    dunk = str_detect(type_text, "Dunk|Layup|Tip"), 
    three_point = str_detect(text, "three point"),
    shooter = str_extract(text, "^(.*?) (?=misses|makes)") %>%
      coalesce(str_extract(text, "(?<=blocks )(.+?)(?= 's)")),
    .after = text
  )|> select(-text, -type_text) 

shots_g3 |> View()

shots_g3 <- shots_g3 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_g3 <- shots_g3 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))

shots_g3 <- shots_g3 |> 
  mutate(gobert_on_court = sapply(end_game_seconds_remaining, gobert_in_campo_g3))


#GAME4
# Substitutions
subs_g4 <- game4 |> 
  filter(str_detect(type_text, "Substitution")) |> 
  filter(str_detect(text, "Rudy Gobert")) |> 
  select(qtr, time, end_game_seconds_remaining, text)

subs_g4

# when enters 
enter_g4 <- subs_g4 |>  
  filter(str_detect(text, "Rudy Gobert enters"))

enter_g4

# when exits
exit_g4 <- subs_g4 |> 
  filter(str_detect(text, "for Rudy Gobert")) 

exit_g4

enter_g4 <- c(2880, enter_g4$end_game_seconds_remaining) |> sort(decreasing = TRUE)
exit_g4  <- c(exit_g4$end_game_seconds_remaining, 0)     |> sort(decreasing = TRUE)

gobert_intervalli_g4 <- tibble(
  enter = enter_g4,
  exit = exit_g4)

gobert_intervalli_g4

# Funzione: restituisce TRUE se il tempo del tiro è dentro un intervallo in cui Gobert era in campo
gobert_in_campo_g4 <- function(shot_time) {
  any(gobert_intervalli_g4$enter >= shot_time & gobert_intervalli_g4$exit <= shot_time)
}

#Filtra tiri di Denver
shots_g4 <- game4 |> 
  filter(team_id == 7,
         shooting_play, !str_detect(text, "free throw")) |>  
  select(qtr,
         time,
         end_game_seconds_remaining,
         text,
         type_text,
         #athlete_id_1,
         contains("coordinate"),
         scoring_play,
         away_score,
         home_score) |> 
  mutate(
    dunk = str_detect(type_text, "Dunk|Layup|Tip"), 
    three_point = str_detect(text, "three point"),
    shooter = str_extract(text, "^(.*?) (?=misses|makes)") %>%
      coalesce(str_extract(text, "(?<=blocks )(.+?)(?= 's)")),
    .after = text
  )|>  select(-text, -type_text)

shots_g4 |> View()

shots_g4 <- shots_g4 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_g4 <- shots_g4 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))


shots_g4 <- shots_g4 |> 
  mutate(gobert_on_court = sapply(end_game_seconds_remaining, gobert_in_campo_g4))


#GAME5
# Substitutions
subs_g5 <- game5 |> 
  filter(str_detect(type_text, "Substitution")) |> 
  filter(str_detect(text, "Rudy Gobert")) |> 
  select(qtr, time, end_game_seconds_remaining, text)

subs_g5

# when enters 
enter_g5 <- subs_g5 |>  
  filter(str_detect(text, "Rudy Gobert enters"))

enter_g5

# when exits
exit_g5 <- subs_g5 |> 
  filter(str_detect(text, "for Rudy Gobert")) 

exit_g5

enter_g5 <- c(2880, enter_g5$end_game_seconds_remaining) |> sort(decreasing = TRUE)
exit_g5  <- c(exit_g5$end_game_seconds_remaining, 0)     |> sort(decreasing = TRUE)

gobert_intervalli_g5 <- tibble(
  enter = enter_g5,
  exit = exit_g5)

gobert_intervalli_g5

# Funzione: restituisce TRUE se il tempo del tiro è dentro un intervallo in cui Gobert era in campo
gobert_in_campo_g5 <- function(shot_time) {
  any(gobert_intervalli_g5$enter >= shot_time & gobert_intervalli_g5$exit <= shot_time)
}

#Filtra tiri di Denver
shots_g5 <- game5 |> 
  filter(team_id == 7,
         shooting_play, !str_detect(text, "free throw")) |>  
  select(qtr,
         time,
         end_game_seconds_remaining,
         text,
         type_text,
         #athlete_id_1,
         contains("coordinate"),
         scoring_play,
         away_score,
         home_score) |> 
  mutate(
    dunk = str_detect(type_text, "Dunk|Layup|Tip"), 
    three_point = str_detect(text, "three point"),
    shooter = str_extract(text, "^(.*?) (?=misses|makes)") %>%
      coalesce(str_extract(text, "(?<=blocks )(.+?)(?= 's)")),
    .after = text
  )|>  select(-text, -type_text) 

shots_g5 |> View()

shots_g5 <- shots_g5 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_g5 <- shots_g5 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))


# Inverti coordinate_x perché MIN era in trasferta in Gara 5
shots_g5 <- shots_g5 |> 
  mutate(coordinate_x = -coordinate_x) |> 
  mutate(gobert_on_court = sapply(end_game_seconds_remaining, gobert_in_campo_g5))


#GAME6
# Substitutions
subs_g6 <- game6 |> 
  filter(str_detect(type_text, "Substitution")) |> 
  filter(str_detect(text, "Rudy Gobert")) |> 
  select(qtr, time, end_game_seconds_remaining, text)

subs_g6

# when enters 
enter_g6 <- subs_g6 |>  
  filter(str_detect(text, "Rudy Gobert enters"))

enter_g6

# when exits
exit_g6 <- subs_g6 |> 
  filter(str_detect(text, "for Rudy Gobert")) 

exit_g6

enter_g6 <- c(2880, enter_g6$end_game_seconds_remaining) |> sort(decreasing = TRUE)
exit_g6  <- exit_g6$end_game_seconds_remaining     |> sort(decreasing = TRUE)

gobert_intervalli_g6 <- tibble(
  enter = enter_g6,
  exit = exit_g6)

gobert_intervalli_g6

# Funzione: restituisce TRUE se il tempo del tiro è dentro un intervallo in cui Gobert era in campo
gobert_in_campo_g6 <- function(shot_time) {
  any(gobert_intervalli_g6$enter >= shot_time & gobert_intervalli_g6$exit <= shot_time)
}

#Filtra tiri di Denver
shots_g6 <- game6 |> 
  filter(team_id == 7,
         shooting_play, !str_detect(text, "free throw")) |>  
  select(qtr,
         time,
         end_game_seconds_remaining,
         text,
         type_text,
         #athlete_id_1,
         contains("coordinate"),
         scoring_play,
         away_score,
         home_score) |> 
  mutate(
    dunk = str_detect(type_text, "Dunk|Layup|Tip"), 
    three_point = str_detect(text, "three point"),
    shooter = str_extract(text, "^(.*?) (?=misses|makes)") %>%
      coalesce(str_extract(text, "(?<=blocks )(.+?)(?= 's)")),
    .after = text
  ) |> select(-text, -type_text) 

shots_g6 |> View()

shots_g6 <- shots_g6 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_g6 <- shots_g6 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))

shots_g6 <- shots_g6 |> 
  mutate(gobert_on_court = sapply(end_game_seconds_remaining, gobert_in_campo_g6))


#GAME7
# Substitutions
subs_g7 <- game7 |> 
  filter(str_detect(type_text, "Substitution")) |> 
  filter(str_detect(text, "Rudy Gobert")) |> 
  select(qtr, time, end_game_seconds_remaining, text)

subs_g7

# when enters 
enter_g7 <- subs_g7 |>  
  filter(str_detect(text, "Rudy Gobert enters"))

enter_g7

# when exits
exit_g7 <- subs_g7 |> 
  filter(str_detect(text, "for Rudy Gobert")) 

exit_g7

enter_g7 <- c(2880, enter_g7$end_game_seconds_remaining) |> sort(decreasing = TRUE)
exit_g7  <- exit_g7$end_game_seconds_remaining     |> sort(decreasing = TRUE)

gobert_intervalli_g7 <- tibble(
  enter = enter_g7,
  exit = exit_g7)

gobert_intervalli_g7

# Funzione: restituisce TRUE se il tempo del tiro è dentro un intervallo in cui Gobert era in campo
gobert_in_campo_g7 <- function(shot_time) {
  any(gobert_intervalli_g7$enter >= shot_time & gobert_intervalli_g7$exit <= shot_time)
}

#Filtra tiri di Denver
shots_g7 <- game7 |> 
  filter(team_id == 7,
         shooting_play, !str_detect(text, "free throw")) |>  
  select(qtr,
         time,
         end_game_seconds_remaining,
         text,
         type_text,
         #athlete_id_1,
         contains("coordinate"),
         scoring_play,
         away_score,
         home_score) |> 
  mutate(
    dunk = str_detect(type_text, "Dunk|Layup|Tip"), 
    three_point = str_detect(text, "three point"),
    shooter = str_extract(text, "^(.*?) (?=misses|makes)") %>%
      coalesce(str_extract(text, "(?<=blocks )(.+?)(?= 's)")),
    .after = text
  ) |>  select(-text, -type_text) 

shots_g7 |> View()

shots_g7 <- shots_g7 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_g7 <- shots_g7 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))

# Inverti coordinate_x perché MIN era in trasferta in Gara 7
shots_g7 <- shots_g7 |> 
  mutate(coordinate_x = -coordinate_x) |> 
  mutate(gobert_on_court = sapply(end_game_seconds_remaining, gobert_in_campo_g7))


#unione 7 gare -> contiene tutti i tiri contro minnesota con gobert in campo e fuori
shots_serie_totale <- bind_rows(
  gara1 = shots_g1,
  gara2 = shots_g2,
  gara3 = shots_g3,
  gara4 = shots_g4,
  gara5 = shots_g5,
  gara6 = shots_g6,
  gara7 = shots_g7,
  .id = "origine"
)

#con ggbasketball
shots_serie_totale <- shots_serie_totale |> mutate(coordinate_x = coordinate_x + 47)


#mappa di tiro
#girato di 90 gradi
shots_serie_totale_half <- shots_serie_totale %>%
  filter(coordinate_x <= 47)  # solo metà campo offensiva

plot_gobert <- shots_serie_totale_half %>%
  ggshotchart(
    x = "coordinate_x",
    y = "coordinate_y",
    result = "scoring_play"
  ) +
  scale_color_manual(
    name = "Esito",
    values = c("FALSE" = "#D32F2F", "TRUE" = "#388E3C"),
    labels = c("FALSE" = "Tiro sbagliato", "TRUE" = "Tiro segnato")
  ) +
  scale_shape_manual(
    name = "Esito",
    values = c("FALSE" = 16, "TRUE" = 17),
    labels = c("FALSE" = "Tiro sbagliato", "TRUE" = "Tiro segnato")
  ) +
  ggtitle("Tiri tentati da Denver nella serie contro Minnesota") +
  facet_wrap(~gobert_status) +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()  # ruota di 90 gradi

plot_gobert



#distanza euclidea
shots_serie_totale$distanza_canestro <- sqrt((shots_serie_totale$coordinate_x-4)^2 + shots_serie_totale$coordinate_y^2)

#istogrammi sul file appunti

shots_filtrati <- shots_serie_totale %>%
  filter(!is.na(distanza_canestro),
        distanza_canestro <= 47)
shots_filtrati <- shots_filtrati %>%
  filter(!(origine == "gara2" & end_game_seconds_remaining <= 209)) |>
  filter(!(origine == "gara3" & end_game_seconds_remaining <= 303)) |>
  filter(!(origine == "gara6" & end_game_seconds_remaining <= 592))    
shots_filtrati_on <- shots_filtrati %>%
  filter(gobert_on_court == TRUE)
shots_filtrati_off <- shots_filtrati %>%
  filter(gobert_on_court == FALSE)

sort(shots_filtrati$distanza_canestro)

#istogramma con i 2 grafici sovrapposti
hist_gobert <- ggplot(shots_filtrati, aes(x = distanza_canestro, fill = gobert_on_court)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, position = "identity", color = "white") +
  labs(title = "Distribuzione delle distanze di tiro con e senza Gobert in campo",
       x = "Distanza dal canestro",
       y = "Frequenza",
       fill = "") +
  scale_fill_manual(values = c("TRUE" = "#1976D2", "FALSE" = "#D32F2F"),
                    labels = c("TRUE" = "Gobert in campo", "FALSE" = "Gobert in panchina")) +
  theme_minimal() +
  theme(legend.position = "bottom")
hist_gobert


hist_gobert +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # griglie principali
    panel.grid.minor = element_blank()   # griglie secondarie
  )

ggsave("hist_gobert.png", hist_gobert, width = 8, height = 6, dpi = 300)
getwd()

#creazione variabile midrange

shots_serie_totale$tripla <- ifelse(
  shots_serie_totale$coordinate_x <= 8.75 & abs(shots_serie_totale$coordinate_y) >= 22,
  TRUE,
  ifelse(shots_serie_totale$distanza_canestro >= 23.75, TRUE, FALSE)
)

shots_serie_totale <- shots_serie_totale %>%
 mutate(midrange = !tripla & !dunk)


#modello multivariato
#creata variabile home_game -> assegna >TRUE se squadra in casa, FALSE altrimenti
shots_serie_totale <- shots_serie_totale %>%
  mutate(home_game = if_else(origine %in% c("gara1", "gara2", "gara5", "gara7"), TRUE, FALSE))

#variabile scarto tra squadre
shots_serie_totale <- shots_serie_totale %>%
  mutate(scarto = case_when(
    origine %in% c("gara1", "gara2", "gara5", "gara7") ~ home_score - away_score,
    origine %in% c("gara3", "gara4", "gara6") ~ away_score - home_score  ))



#variabile categorica tiratore
#rimozione spazi iniziali/finali
shots_serie_totale <- shots_serie_totale %>%
  mutate(shooter = str_trim(shooter))

shots_serie_totale %>%
  count(shooter, sort = TRUE) %>%
  print(n = Inf)

shots_serie_totale <- shots_serie_totale %>%
  mutate(shooter = factor(shooter))

shots_serie_totale <- shots_serie_totale %>%
  filter(!is.na(shooter))

levels(shots_serie_totale$shooter)

#vvariabile binomiale -> true se tiratori sono jokic o murray
shots_serie_totale$shooter_bin <- shots_serie_totale$shooter %in% c("Nikola Jokic", "Jamal Murray")

#variabile shooter a 3 livelli
unique(shots_filtrati$shooter)

#filtro i dati poi uso dataset shots_filtrati per modelli logistici (non shots_serie_totale)
shots_filtrati <- shots_serie_totale %>%
  filter(!is.na(distanza_canestro),
         distanza_canestro <= 47)
shots_filtrati <- shots_filtrati %>%
  filter(!(origine == "gara2" & end_game_seconds_remaining <= 209)) |>
  filter(!(origine == "gara3" & end_game_seconds_remaining <= 303)) |>
  filter(!(origine == "gara6" & end_game_seconds_remaining <= 592))    
shots_filtrati_on <- shots_filtrati %>%
  filter(gobert_on_court == TRUE)
shots_filtrati_off <- shots_filtrati %>%
  filter(gobert_on_court == FALSE)

# shooter clean per rimuovere spazi dopo i nomi
shots_filtrati <- shots_filtrati %>%
  mutate(
    shooter_clean = str_squish(shooter),  
    shooter_cat = case_when(
      grepl("Jokic", shooter_clean, ignore.case = TRUE) ~ "Jokic",
      grepl("Murray", shooter_clean, ignore.case = TRUE) ~ "Murray",
      TRUE ~ "Altri"
    )
  )
shots_filtrati <- shots_filtrati %>%
  mutate(shooter_cat = factor(shooter_cat, levels = c("Jokic", "Murray", "Altri")))

shots_filtrati <- shots_filtrati %>%
  mutate(tripla_angolo = ifelse(tripla == TRUE & coordinate_x <= 8.75, TRUE, FALSE))

shots_filtrati$tripla_non_angolo <- shots_filtrati$tripla & !shots_filtrati$tripla_angolo

#variabile tipo di tiro categorica (dunk, mid, dall'angolo, non dall'angolo)
shots_filtrati$tipo_tiro <- NA

shots_filtrati$tipo_tiro[shots_filtrati$dunk == TRUE] <- "dunk"
shots_filtrati$tipo_tiro[shots_filtrati$midrange == TRUE] <- "midrange"
shots_filtrati$tipo_tiro[shots_filtrati$tripla_angolo == TRUE] <- "tripla_angolo"
shots_filtrati$tipo_tiro[shots_filtrati$tripla_non_angolo == TRUE] <- "tripla_non_angolo"

# Converto in fattore con ordine specifico
shots_filtrati$tipo_tiro <- factor(shots_filtrati$tipo_tiro,
                                   levels = c("dunk", "midrange", "tripla_angolo", "tripla_non_angolo"))


shots_filtrati <- shots_filtrati |> 
  mutate(seconds = 2880 - end_game_seconds_remaining)


library("lme4")
#random effects -> mod di partenza
mod_re <- glmer(midrange ~ gobert_on_court +poly(seconds, 5)+ home_game
                + scarto + gobert_on_court:home_game + (1|origine), 
                family = binomial, 
                data = shots_filtrati)
summary(mod_re)



#mod completo
mod8b <- glm(midrange ~ gobert_on_court +poly(end_game_seconds_remaining, 2)+ home_game + scarto, 
             family = binomial, 
             data = shots_filtrati)
summary(mod8b)

#mod finale
mod_finale <- glm(midrange ~ gobert_on_court +poly(seconds, 2) + home_game, 
                  family = binomial, 
                  data = shots_filtrati)
summary(mod_finale)

mod_finale$coefficients
attach(shots_filtrati)
#plot(sort(end_game_seconds_remaining), sum(mod8$coefficients[1:2])+ 
 #      (mod8$coefficients[3] +  mod8$coefficients[5]) * sort(end_game_seconds_remaining) +
  #     (mod8$coefficients[4] +  mod8$coefficients[6]) * sort(end_game_seconds_remaining^2), type="l" )

#gobert off, home_game false
logit1 = (mod_finale$coefficients[1])+ 
  (mod_finale$coefficients[3]) * poly(seconds, 2)[order(seconds),1] +
  (mod_finale$coefficients[4]) * poly(seconds, 2)[order(seconds),2]
##
plot(sort(seconds),exp(logit1)/(1+exp(logit1)), type="l" )

#gobert on, home_game false
logit2 = sum(mod_finale$coefficients[1:2])+ 
  (mod_finale$coefficients[3]) * poly(seconds, 2)[order(seconds),1] +
  (mod_finale$coefficients[4]) * poly(seconds, 2)[order(seconds),2]

plot(sort(seconds),exp(logit2)/(1+exp(logit2)), type="l" )
#gobert off, home_game true
logit3 = sum(mod_finale$coefficients[c(1,5)])+ 
  (mod_finale$coefficients[3]) * poly(seconds, 2)[order(seconds),1] +
  (mod_finale$coefficients[4]) * poly(seconds, 2)[order(seconds),2]

plot(sort(seconds),exp(logit3)/(1+exp(logit3)), type="l" )
#gobert on, home_game true
logit4 = sum(mod_finale$coefficients[c(1, 2, 5)]) + 
  mod_finale$coefficients[3] * poly(seconds, 2)[order(seconds),1] +
  mod_finale$coefficients[4] * poly(seconds, 2)[order(seconds),2]

plot(sort(seconds),exp(logit4)/(1+exp(logit4)), type="l" )

prob1 <- exp(logit1) / (1 + exp(logit1))
prob2 <- exp(logit2) / (1 + exp(logit2))
prob3 <- exp(logit3) / (1 + exp(logit3))
prob4 <- exp(logit4) / (1 + exp(logit4))


library(ggplot2)

grafico_prob <- data.frame(
  seconds = rep(sort(seconds), 4),
  prob = c(prob1, prob2, prob3, prob4),
  group = factor(rep(c("Gobert off, @MIN",
                       "Gobert on, @MIN",
                       "Gobert off, @DEN",
                       "Gobert on, @DEN"), each=length(seconds)))
)

ggplot(grafico_prob, aes(x=seconds/60, y=prob, color=group)) +
  geom_line(size=1.2) +
  scale_color_manual(values = c("#FFA500", "#4e79a7", "#D32F2F", "#388E3C"))+
  theme_minimal() +
  theme(legend.position="right", legend.title=element_blank()) +
  guides(color=guide_legend(ncol=1)) +  # legenda verticale
  labs(x="Minuti dall'inizio della partita", y="Probabilità (%)", title="Probabilità di tiro dal midrange",
       subtitle="Analisi in base a presenza in campo di Gobert e sede della partita")

vcov(mod_finale)

#valutazione coefficienti
#intercetta
exp(-0.7684)
#0.4637
exp(-0.7684)/(1+exp(-0.7684))
#probabilità -> 31,67%
#gob on court true
1.41*0.4637
#0.6538
0.6538/(1+0.6538)
#0.3953
exp(-0.3162)
0.7289*0.4637
#0.338
0.338/(1+0.338)
#0,2526
step(mod_finale, scope = list(lower = mod_min, upper= mod8b), direction = "forward") |> summary()

1- (658.93/670.95)




