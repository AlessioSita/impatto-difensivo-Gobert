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
shots_ga1 <- game1 |> 
  filter(team_id == 7,
         shooting_play) |>  
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
  )  

shots_ga1 |> View()


shots_ga1 <- shots_ga1 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )

print(shots_ga1)

shots_ga1 <- shots_ga1 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))


shots_ga1 <- shots_ga1 %>%
  mutate(
    # Estrai il nome del tiratore
    shooter = str_extract(text, "^(.*?) (?=makes|misses)"),
    
    # Classifica esito
    result = case_when(
      str_detect(text, "makes") ~ "made",
      str_detect(text, "misses") ~ "missed",
      TRUE ~ NA_character_
    ),
    
    # Tiro libero o da campo
    is_ft = str_detect(type_text, "Free Throw"),
    is_fg = !is_ft,
    
    # Chiave temporale
    time_key = paste(qtr, end_game_seconds_remaining),
    
    # Flag esiti
    fg_made = if_else(is_fg & result == "made", 1, 0),
    fg_missed = if_else(is_fg & result == "missed", 1, 0),
    ft_made = if_else(is_ft & result == "made", 1, 0),
    ft_missed = if_else(is_ft & result == "missed", 1, 0)
  ) %>%
  group_by(shooter, qtr, end_game_seconds_remaining, time_key) %>%
  summarise(
    num_events   = n(),
    # Riepilogo tiri da campo
    fgs_made     = sum(fg_made, na.rm = TRUE),
    fgs_missed   = sum(fg_missed, na.rm = TRUE),
    fts_made     = sum(ft_made, na.rm = TRUE),
    fts_missed   = sum(ft_missed, na.rm = TRUE),
    dunk         = any(dunk, na.rm = TRUE),
    three_point  = any(three_point, na.rm = TRUE),
    coordinate_x = first(coordinate_x),
    coordinate_y = first(coordinate_y),
    away_score   = first(away_score),
    home_score   = first(home_score),
    scoring_play = any(scoring_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    action_summary = case_when(
      fgs_made > 0 & fts_made > 0 ~ paste0(shooter, ": FG made + FT made"),
      fgs_made > 0 & fts_made == 0 ~ paste0(shooter, ": FG made only"),
      fgs_made == 0 & fts_made > 0 ~ paste0(shooter, ": FT sequence"),
      fgs_made == 0 & fts_made == 0 & fgs_missed > 0 ~ paste0(shooter, ": FG missed"),
      fgs_made == 0 & fts_made == 0 & fts_missed > 0 ~ paste0(shooter, ": FT missed"),
      TRUE ~ paste0(shooter, ": no made shot")
    )
  )


# Inverti coordinate_x perché MIN era in trasferta in Gara 1
shots_ga1 <- shots_ga1 |> 
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
shots_ga2 <- game2 |> 
  filter(team_id == 7,
         shooting_play) |>  
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
  )

shots_ga2 |> View()

shots_ga2 <- shots_ga2 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_ga2 <- shots_ga2 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))


shots_ga2 <- shots_ga2 %>%
  mutate(
    # Estrai il nome del tiratore
    shooter = str_extract(text, "^(.*?) (?=makes|misses)"),
    
    # Classifica esito
    result = case_when(
      str_detect(text, "makes") ~ "made",
      str_detect(text, "misses") ~ "missed",
      TRUE ~ NA_character_
    ),
    
    # Tiro libero o da campo
    is_ft = str_detect(type_text, "Free Throw"),
    is_fg = !is_ft,
    
    # Chiave temporale
    time_key = paste(qtr, end_game_seconds_remaining),
    
    # Flag esiti
    fg_made = if_else(is_fg & result == "made", 1, 0),
    fg_missed = if_else(is_fg & result == "missed", 1, 0),
    ft_made = if_else(is_ft & result == "made", 1, 0),
    ft_missed = if_else(is_ft & result == "missed", 1, 0)
  ) %>%
  group_by(shooter, qtr, end_game_seconds_remaining, time_key) %>%
  summarise(
    num_events   = n(),
    # Riepilogo tiri da campo
    fgs_made     = sum(fg_made, na.rm = TRUE),
    fgs_missed   = sum(fg_missed, na.rm = TRUE),
    fts_made     = sum(ft_made, na.rm = TRUE),
    fts_missed   = sum(ft_missed, na.rm = TRUE),
    dunk         = any(dunk, na.rm = TRUE),
    three_point  = any(three_point, na.rm = TRUE),
    coordinate_x = first(coordinate_x),
    coordinate_y = first(coordinate_y),
    away_score   = first(away_score),
    home_score   = first(home_score),
    scoring_play = any(scoring_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    action_summary = case_when(
      fgs_made > 0 & fts_made > 0 ~ paste0(shooter, ": FG made + FT made"),
      fgs_made > 0 & fts_made == 0 ~ paste0(shooter, ": FG made only"),
      fgs_made == 0 & fts_made > 0 ~ paste0(shooter, ": FT sequence"),
      fgs_made == 0 & fts_made == 0 & fgs_missed > 0 ~ paste0(shooter, ": FG missed"),
      fgs_made == 0 & fts_made == 0 & fts_missed > 0 ~ paste0(shooter, ": FT missed"),
      TRUE ~ paste0(shooter, ": no made shot")
    )
  )


# Inverti coordinate_x perché MIN era in trasferta in Gara 2
shots_ga2 <- shots_ga2 |> 
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
shots_ga3 <- game3 |> 
  filter(team_id == 7,
         shooting_play) |>  
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
  ) 

shots_ga3 |> View()

shots_ga3 <- shots_ga3 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_ga3 <- shots_ga3 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))


shots_ga3 <- shots_ga3 %>%
  mutate(
    # Estrai il nome del tiratore
    shooter = str_extract(text, "^(.*?) (?=makes|misses)"),
    
    # Classifica esito
    result = case_when(
      str_detect(text, "makes") ~ "made",
      str_detect(text, "misses") ~ "missed",
      TRUE ~ NA_character_
    ),
    
    # Tiro libero o da campo
    is_ft = str_detect(type_text, "Free Throw"),
    is_fg = !is_ft,
    
    # Chiave temporale
    time_key = paste(qtr, end_game_seconds_remaining),
    
    # Flag esiti
    fg_made = if_else(is_fg & result == "made", 1, 0),
    fg_missed = if_else(is_fg & result == "missed", 1, 0),
    ft_made = if_else(is_ft & result == "made", 1, 0),
    ft_missed = if_else(is_ft & result == "missed", 1, 0)
  ) %>%
  group_by(shooter, qtr, end_game_seconds_remaining, time_key) %>%
  summarise(
    num_events   = n(),
    # Riepilogo tiri da campo
    fgs_made     = sum(fg_made, na.rm = TRUE),
    fgs_missed   = sum(fg_missed, na.rm = TRUE),
    fts_made     = sum(ft_made, na.rm = TRUE),
    fts_missed   = sum(ft_missed, na.rm = TRUE),
    dunk         = any(dunk, na.rm = TRUE),
    three_point  = any(three_point, na.rm = TRUE),
    coordinate_x = first(coordinate_x),
    coordinate_y = first(coordinate_y),
    away_score   = first(away_score),
    home_score   = first(home_score),
    scoring_play = any(scoring_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    action_summary = case_when(
      fgs_made > 0 & fts_made > 0 ~ paste0(shooter, ": FG made + FT made"),
      fgs_made > 0 & fts_made == 0 ~ paste0(shooter, ": FG made only"),
      fgs_made == 0 & fts_made > 0 ~ paste0(shooter, ": FT sequence"),
      fgs_made == 0 & fts_made == 0 & fgs_missed > 0 ~ paste0(shooter, ": FG missed"),
      fgs_made == 0 & fts_made == 0 & fts_missed > 0 ~ paste0(shooter, ": FT missed"),
      TRUE ~ paste0(shooter, ": no made shot")
    )
  )

shots_ga3 <- shots_ga3 |> 
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
shots_ga4 <- game4 |> 
  filter(team_id == 7,
         shooting_play) |>  
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
  ) 

shots_ga4 |> View()


shots_ga4 <- shots_ga4 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_ga4 <- shots_ga4 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))

shots_ga4 <- shots_ga4 %>%
  mutate(
    # Estrai il nome del tiratore
    shooter = str_extract(text, "^(.*?) (?=makes|misses)"),
    
    # Classifica esito
    result = case_when(
      str_detect(text, "makes") ~ "made",
      str_detect(text, "misses") ~ "missed",
      TRUE ~ NA_character_
    ),
    
    # Tiro libero o da campo
    is_ft = str_detect(type_text, "Free Throw"),
    is_fg = !is_ft,
    
    # Chiave temporale
    time_key = paste(qtr, end_game_seconds_remaining),
    
    # Flag esiti
    fg_made = if_else(is_fg & result == "made", 1, 0),
    fg_missed = if_else(is_fg & result == "missed", 1, 0),
    ft_made = if_else(is_ft & result == "made", 1, 0),
    ft_missed = if_else(is_ft & result == "missed", 1, 0)
  ) %>%
  group_by(shooter, qtr, end_game_seconds_remaining, time_key) %>%
  summarise(
    num_events   = n(),
    # Riepilogo tiri da campo
    fgs_made     = sum(fg_made, na.rm = TRUE),
    fgs_missed   = sum(fg_missed, na.rm = TRUE),
    fts_made     = sum(ft_made, na.rm = TRUE),
    fts_missed   = sum(ft_missed, na.rm = TRUE),
    dunk         = any(dunk, na.rm = TRUE),
    three_point  = any(three_point, na.rm = TRUE),
    coordinate_x = first(coordinate_x),
    coordinate_y = first(coordinate_y),
    away_score   = first(away_score),
    home_score   = first(home_score),
    scoring_play = any(scoring_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    action_summary = case_when(
      fgs_made > 0 & fts_made > 0 ~ paste0(shooter, ": FG made + FT made"),
      fgs_made > 0 & fts_made == 0 ~ paste0(shooter, ": FG made only"),
      fgs_made == 0 & fts_made > 0 ~ paste0(shooter, ": FT sequence"),
      fgs_made == 0 & fts_made == 0 & fgs_missed > 0 ~ paste0(shooter, ": FG missed"),
      fgs_made == 0 & fts_made == 0 & fts_missed > 0 ~ paste0(shooter, ": FT missed"),
      TRUE ~ paste0(shooter, ": no made shot")
    )
  )
shots_ga4 <- shots_ga4 |> 
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
shots_ga5 <- game5 |> 
  filter(team_id == 7,
         shooting_play) |>  
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
  )

shots_ga5 |> View()

shots_ga5 <- shots_ga5 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_ga5 <- shots_ga5 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))

shots_ga5 <- shots_ga5 %>%
  mutate(
    # Estrai il nome del tiratore
    shooter = str_extract(text, "^(.*?) (?=makes|misses)"),
    
    # Classifica esito
    result = case_when(
      str_detect(text, "makes") ~ "made",
      str_detect(text, "misses") ~ "missed",
      TRUE ~ NA_character_
    ),
    
    # Tiro libero o da campo
    is_ft = str_detect(type_text, "Free Throw"),
    is_fg = !is_ft,
    
    # Chiave temporale
    time_key = paste(qtr, end_game_seconds_remaining),
    
    # Flag esiti
    fg_made = if_else(is_fg & result == "made", 1, 0),
    fg_missed = if_else(is_fg & result == "missed", 1, 0),
    ft_made = if_else(is_ft & result == "made", 1, 0),
    ft_missed = if_else(is_ft & result == "missed", 1, 0)
  ) %>%
  group_by(shooter, qtr, end_game_seconds_remaining, time_key) %>%
  summarise(
    num_events   = n(),
    # Riepilogo tiri da campo
    fgs_made     = sum(fg_made, na.rm = TRUE),
    fgs_missed   = sum(fg_missed, na.rm = TRUE),
    fts_made     = sum(ft_made, na.rm = TRUE),
    fts_missed   = sum(ft_missed, na.rm = TRUE),
    dunk         = any(dunk, na.rm = TRUE),
    three_point  = any(three_point, na.rm = TRUE),
    coordinate_x = first(coordinate_x),
    coordinate_y = first(coordinate_y),
    away_score   = first(away_score),
    home_score   = first(home_score),
    scoring_play = any(scoring_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    action_summary = case_when(
      fgs_made > 0 & fts_made > 0 ~ paste0(shooter, ": FG made + FT made"),
      fgs_made > 0 & fts_made == 0 ~ paste0(shooter, ": FG made only"),
      fgs_made == 0 & fts_made > 0 ~ paste0(shooter, ": FT sequence"),
      fgs_made == 0 & fts_made == 0 & fgs_missed > 0 ~ paste0(shooter, ": FG missed"),
      fgs_made == 0 & fts_made == 0 & fts_missed > 0 ~ paste0(shooter, ": FT missed"),
      TRUE ~ paste0(shooter, ": no made shot")
    )
  )


# Inverti coordinate_x perché MIN era in trasferta in Gara 5
shots_ga5 <- shots_ga5 |> 
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
shots_ga6 <- game6 |> 
  filter(team_id == 7,
         shooting_play) |>  
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
  ) 

shots_ga6 |> View()

shots_ga6 <- shots_ga6 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )
shots_ga6 <- shots_ga6 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))

shots_ga6 <- shots_ga6 %>%
  mutate(
    # Estrai il nome del tiratore
    shooter = str_extract(text, "^(.*?) (?=makes|misses)"),
    
    # Classifica esito
    result = case_when(
      str_detect(text, "makes") ~ "made",
      str_detect(text, "misses") ~ "missed",
      TRUE ~ NA_character_
    ),
    
    # Tiro libero o da campo
    is_ft = str_detect(type_text, "Free Throw"),
    is_fg = !is_ft,
    
    # Chiave temporale
    time_key = paste(qtr, end_game_seconds_remaining),
    
    # Flag esiti
    fg_made = if_else(is_fg & result == "made", 1, 0),
    fg_missed = if_else(is_fg & result == "missed", 1, 0),
    ft_made = if_else(is_ft & result == "made", 1, 0),
    ft_missed = if_else(is_ft & result == "missed", 1, 0)
  ) %>%
  group_by(shooter, qtr, end_game_seconds_remaining, time_key) %>%
  summarise(
    num_events   = n(),
    # Riepilogo tiri da campo
    fgs_made     = sum(fg_made, na.rm = TRUE),
    fgs_missed   = sum(fg_missed, na.rm = TRUE),
    fts_made     = sum(ft_made, na.rm = TRUE),
    fts_missed   = sum(ft_missed, na.rm = TRUE),
    dunk         = any(dunk, na.rm = TRUE),
    three_point  = any(three_point, na.rm = TRUE),
    coordinate_x = first(coordinate_x),
    coordinate_y = first(coordinate_y),
    away_score   = first(away_score),
    home_score   = first(home_score),
    scoring_play = any(scoring_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    action_summary = case_when(
      fgs_made > 0 & fts_made > 0 ~ paste0(shooter, ": FG made + FT made"),
      fgs_made > 0 & fts_made == 0 ~ paste0(shooter, ": FG made only"),
      fgs_made == 0 & fts_made > 0 ~ paste0(shooter, ": FT sequence"),
      fgs_made == 0 & fts_made == 0 & fgs_missed > 0 ~ paste0(shooter, ": FG missed"),
      fgs_made == 0 & fts_made == 0 & fts_missed > 0 ~ paste0(shooter, ": FT missed"),
      TRUE ~ paste0(shooter, ": no made shot")
    )
  )

shots_ga6 <- shots_ga6 |> 
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
shots_ga7 <- game7 |> 
  filter(team_id == 7,
         shooting_play) |>  
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
  )  

shots_ga7 |> View()

shots_ga7 <- shots_ga7 %>%
  separate_wider_delim(
    cols = time,
    delim = ":",          # il delimitatore è ":"
    names = c("minuti", "secondi")
  )


shots_ga7 <- shots_ga7 %>%
  mutate(end_game_seconds_remaining = (4 - qtr) * 12 * 60 + as.numeric(minuti) * 60 + as.numeric(secondi))

shots_ga7 <- shots_ga7 %>%
  mutate(
    # Estrai il nome del tiratore
    shooter = str_extract(text, "^(.*?) (?=makes|misses)"),
    
    # Classifica esito
    result = case_when(
      str_detect(text, "makes") ~ "made",
      str_detect(text, "misses") ~ "missed",
      TRUE ~ NA_character_
    ),
    
    # Tiro libero o da campo
    is_ft = str_detect(type_text, "Free Throw"),
    is_fg = !is_ft,
    
    # Chiave temporale
    time_key = paste(qtr, end_game_seconds_remaining),
    
    # Flag esiti
    fg_made = if_else(is_fg & result == "made", 1, 0),
    fg_missed = if_else(is_fg & result == "missed", 1, 0),
    ft_made = if_else(is_ft & result == "made", 1, 0),
    ft_missed = if_else(is_ft & result == "missed", 1, 0)
  ) %>%
  group_by(shooter, qtr, end_game_seconds_remaining, time_key) %>%
  summarise(
    num_events   = n(),
    # Riepilogo tiri da campo
    fgs_made     = sum(fg_made, na.rm = TRUE),
    fgs_missed   = sum(fg_missed, na.rm = TRUE),
    fts_made     = sum(ft_made, na.rm = TRUE),
    fts_missed   = sum(ft_missed, na.rm = TRUE),
    dunk         = any(dunk, na.rm = TRUE),
    three_point  = any(three_point, na.rm = TRUE),
    coordinate_x = first(coordinate_x),
    coordinate_y = first(coordinate_y),
    away_score   = first(away_score),
    home_score   = first(home_score),
    scoring_play = any(scoring_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    action_summary = case_when(
      fgs_made > 0 & fts_made > 0 ~ paste0(shooter, ": FG made + FT made"),
      fgs_made > 0 & fts_made == 0 ~ paste0(shooter, ": FG made only"),
      fgs_made == 0 & fts_made > 0 ~ paste0(shooter, ": FT sequence"),
      fgs_made == 0 & fts_made == 0 & fgs_missed > 0 ~ paste0(shooter, ": FG missed"),
      fgs_made == 0 & fts_made == 0 & fts_missed > 0 ~ paste0(shooter, ": FT missed"),
      TRUE ~ paste0(shooter, ": no made shot")
    )
  )

# Inverti coordinate_x perché MIN era in trasferta in Gara 7
shots_ga7 <- shots_ga7 |> 
  mutate(coordinate_x = -coordinate_x) |> 
  mutate(gobert_on_court = sapply(end_game_seconds_remaining, gobert_in_campo_g7))


shots_serie_tot_poisson <- bind_rows(
  gara1 = shots_ga1,
  gara2 = shots_ga2,
  gara3 = shots_ga3,
  gara4 = shots_ga4,
  gara5 = shots_ga5,
  gara6 = shots_ga6,
  gara7 = shots_ga7,
  .id = "origine"
)


#con ggbasketball

shots_serie_tot_poisson <- shots_serie_tot_poisson |> mutate(coordinate_x = coordinate_x + 47)



#distanza euclidea
shots_serie_tot_poisson$distanza_canestro <- sqrt((shots_serie_tot_poisson$coordinate_x-4)^2 + shots_serie_tot_poisson$coordinate_y^2)


shots_filtrati_poisson <- shots_serie_tot_poisson %>%
  filter(!is.na(distanza_canestro),
         distanza_canestro <= 47)
shots_filtrati_poisson <- shots_filtrati_poisson %>%
  filter(!(origine == "gara2" & end_game_seconds_remaining <= 209)) |>
  filter(!(origine == "gara3" & end_game_seconds_remaining <= 303)) |>
  filter(!(origine == "gara6" & end_game_seconds_remaining <= 592))    
shots_filtrati_on_poisson <- shots_filtrati_poisson %>%
  filter(gobert_on_court == TRUE)
shots_filtrati_off_poisson <- shots_filtrati_poisson %>%
  filter(gobert_on_court == FALSE)

sort(shots_filtrati_poisson$distanza_canestro)



shots_serie_tot_poisson$tripla <- ifelse(
  shots_serie_tot_poisson$coordinate_x <= 8.75 & abs(shots_serie_tot_poisson$coordinate_y) >= 22,
  TRUE,
  ifelse(shots_serie_tot_poisson$distanza_canestro >= 23.75, TRUE, FALSE)
)

#tripla dall'angolo
shots_filtrati_poisson <- shots_filtrati_poisson %>%
  mutate(tripla_angolo = ifelse(tripla == TRUE & coordinate_x <= 8.75, TRUE, FALSE))

shots_serie_tot_poisson <- shots_serie_tot_poisson %>%
  mutate(midrange = !tripla & !dunk)

shots_filtrati_poisson <- shots_filtrati_poisson %>%
  mutate(midrange = !tripla & !dunk & !(coordinate_x == 19.00 & coordinate_y == 0))

shots_filtrati_poisson <- shots_filtrati_poisson %>%
  mutate(tiro_libero = coordinate_x == 19.00 & coordinate_y == 0)



#modello multivariato
#creata variabile home_game -> assegna >TRUE se squadra in casa, FALSE altrimenti
shots_serie_tot_poisson <- shots_serie_tot_poisson %>%
  mutate(home_game = if_else(origine %in% c("gara1", "gara2", "gara5", "gara7"), TRUE, FALSE))

#variabile scarto tra squadre
shots_serie_tot_poisson <- shots_serie_tot_poisson %>%
  mutate(scarto = case_when(
    origine %in% c("gara1", "gara2", "gara5", "gara7") ~ home_score - away_score,
    origine %in% c("gara3", "gara4", "gara6") ~ away_score - home_score  ))



#variabile categorica tiratore
#rimozione spazi iniziali/finali
shots_serie_tot_poisson <- shots_serie_tot_poisson %>%
  mutate(shooter = str_trim(shooter))

shots_serie_tot_poisson %>%
  count(shooter, sort = TRUE) %>%
  print(n = Inf)

shots_serie_tot_poisson <- shots_serie_tot_poisson %>%
  mutate(shooter = factor(shooter))

shots_serie_tot_poisson <- shots_serie_tot_poisson %>%
  filter(!is.na(shooter))




#vvariabile binomiale -> true se tiratori sono jokic o murray
shots_serie_tot_poisson$shooter_bin <- shots_serie_tot_poisson$shooter %in% c("Nikola Jokic", "Jamal Murray")

#variabile shooter a 3 livelli
unique(shots_filtrati_poisson$shooter)

#filtro i dati poi uso dataset shots_filtrati per modelli logistici (non shots_serie_tot_poisson)
shots_filtrati_poisson <- shots_serie_tot_poisson %>%
  filter(!is.na(distanza_canestro),
         distanza_canestro <= 47)
shots_filtrati_poisson <- shots_filtrati_poisson %>%
  filter(!(origine == "gara2" & end_game_seconds_remaining <= 209)) |>
  filter(!(origine == "gara3" & end_game_seconds_remaining <= 303)) |>
  filter(!(origine == "gara6" & end_game_seconds_remaining <= 592))    
shots_filtrati_on_poisson <- shots_filtrati_poisson %>%
  filter(gobert_on_court == TRUE)
shots_filtrati_off_poisson <- shots_filtrati_poisson %>%
  filter(gobert_on_court == FALSE)

# shooter clean per rimuovere spazi dopo i nomi
shots_filtrati_poisson <- shots_filtrati_poisson %>%
  mutate(
    shooter_clean = str_squish(shooter),  
    shooter_cat = case_when(
      grepl("Jokic", shooter_clean, ignore.case = TRUE) ~ "Jokic",
      grepl("Murray", shooter_clean, ignore.case = TRUE) ~ "Murray",
      TRUE ~ "Altri"
    )
  )
shots_filtrati_poisson <- shots_filtrati_poisson %>%
  mutate(shooter_cat = factor(shooter_cat, levels = c("Jokic", "Murray", "Altri")))

shots_filtrati_poisson <- shots_filtrati_poisson %>%
  mutate(tiro_libero = coordinate_x == 19.00 & coordinate_y == 0)

shots_filtrati_poisson <- shots_filtrati_poisson %>%
  mutate(tripla_angolo = ifelse(tripla == TRUE & coordinate_x <= 8.75, TRUE, FALSE))

shots_filtrati_poisson$tripla_non_angolo <- shots_filtrati_poisson$tripla & !shots_filtrati_poisson$tripla_angolo

#variabile tipo di tiro categorica (dunk, mid, dall'angolo, non dall'angolo)
shots_filtrati_poisson$tipo_tiro <- NA

shots_filtrati_poisson$tipo_tiro[shots_filtrati_poisson$dunk == TRUE] <- "dunk"
shots_filtrati_poisson$tipo_tiro[shots_filtrati_poisson$midrange == TRUE] <- "midrange"
shots_filtrati_poisson$tipo_tiro[shots_filtrati_poisson$tripla_angolo == TRUE] <- "tripla_angolo"
shots_filtrati_poisson$tipo_tiro[shots_filtrati_poisson$tripla_non_angolo == TRUE] <- "tripla_non_angolo"
shots_filtrati_poisson$tipo_tiro[shots_filtrati_poisson$tiro_libero == TRUE] <- "tiro_libero"

# Converto in fattore con ordine specifico
shots_filtrati_poisson$tipo_tiro <- factor(shots_filtrati_poisson$tipo_tiro,
                                   levels = c("dunk", "midrange", "tripla_angolo", "tripla_non_angolo", "tiro_libero"))

shots_filtrati_poisson <- shots_filtrati_poisson |>
  mutate(
    punti_per_tiro = case_when(
      !scoring_play ~ 0,
      tipo_tiro %in% c("tiro_libero") ~ fts_made,
      tipo_tiro %in% c("midrange", "dunk") ~ 2,
      tipo_tiro %in% c("tripla_angolo", "tripla_non_angolo") ~ 3,
      TRUE ~ NA_real_  # Per eventuali casi non coperti
    )
  )

save(shots_serie_totale,shots_serie_tot_poisson, shots_filtrati, shots_filtrati_poisson, file = "workspace_Gobert.RData")


shots_filtrati_poisson <- shots_filtrati_poisson |> 
  mutate(seconds = 2880 - end_game_seconds_remaining)

plot(shots_filtrati_poisson$seconds^2, shots_filtrati_poisson$end_game_seconds_remaining^2)
model <- glm(punti_per_tiro ~ gobert_on_court + poly(seconds, 2) + home_game, 
             family = poisson,
             data = shots_filtrati_poisson)
summary(model)

1-pchisq(810.73, 535)


library(glmmTMB)

model_nb_mixed <- glmmTMB(
  punti_per_tiro ~ gobert_on_court + home_game + poly(seconds, 5) + scarto +
    gobert_on_court:home_game + (1 | origine),
  family = nbinom2,
  data = shots_filtrati_poisson
)
summary(model_nb_mixed)

VarCorr(model_nb_mixed)

library(MASS)
#binomiale negativa
mod_nb_finale <- glm.nb(punti_per_tiro ~ gobert_on_court +home_game + poly(seconds, 2) + scarto, 
                        data = shots_filtrati_poisson)
summary(mod_nb_finale)
