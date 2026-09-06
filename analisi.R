library(hoopR)
library(dplyr)
library(stringr)
library(tidyr)
library(httr)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(sportyR)
library(ggbasketball)
library(nnet)
library(lme4)
library(MASS)
library(glmmTMB)

mod_random_effects <- glmer(midrange ~ gobert_on_court +poly(seconds, 5)+ home_game
                            + poly(scarto, 5) + gobert_on_court:home_game + (1|origine), 
                            family = binomial, 
                            data = shots_filtrati)
summary(mod_random_effects)



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

#gobert off, home_game false
logit1 = (mod_finale$coefficients[1])+ 
  (mod_finale$coefficients[3]) * poly(seconds, 2)[order(seconds),1] +
  (mod_finale$coefficients[4]) * poly(seconds, 2)[order(seconds),2]

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



# Aggiustiamo i dati per usare colori "home" e linetype "Gobert"
grafico_prob <- data.frame(
  seconds = rep(sort(seconds), 4),
  prob = c(prob1, prob2, prob3, prob4),
  team = factor(rep(c("MINNESOTA", "MINNESOTA", "DENVER", "DENVER"), each=length(seconds))),
  gobert = factor(rep(c("Gobert in panchina", "Gobert in campo", "Gobert in panchina", "Gobert in campo"), each=length(seconds)))
)

# Definiamo colori per squadra
color_team <- c("MINNESOTA" = "#4e79a7", "DENVER" = "#D32F2F")  # home team colors
# Definiamo linee per Gobert
linetype_gobert <- c("Gobert in panchina" = "dashed", "Gobert in campo" = "solid")


ggplot(grafico_prob, aes(x = seconds/60, y = prob, 
                         color = team, linetype = gobert)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = color_team) +
  scale_linetype_manual(
    values = c(
      "Gobert in panchina" = "22",  # doppio tratteggio
      "Gobert in campo" = "solid"
    ),
    guide = guide_legend(
      override.aes = list(size = 1.5),  # linee più spesse nella legenda
      keywidth = 3                      # aumenta lunghezza della linea nella legenda
    )
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(
    x = "Minuti dall'inizio della partita",
    y = "Probabilità (%)",
    title = "Probabilità di tiro dal midrange, serie playoff 2024",
    subtitle = "Analisi in base a presenza in campo di Gobert e sede della partita",
    color = "Squadra in casa", 
    linetype = "Presenza di Gobert"
  ) +
  geom_vline(xintercept = c(12, 24, 36, 48), linetype = "dotted", color = "gray50")


###POISSON###
#
shots_filtrati_poisson <- shots_filtrati_poisson |> 
  mutate(seconds = 2880 - end_game_seconds_remaining)


plot(shots_filtrati_poisson$seconds^2, shots_filtrati_poisson$end_game_seconds_remaining^2)
model <- glm(punti_per_tiro ~ gobert_on_court + poly(seconds, 5) + home_game 
             + poly(scarto,5) + gobert_on_court:home_game , 
             family = poisson,
             data = shots_filtrati_poisson)
summary(model)

1-pchisq(788.97, 526)


library(glmmTMB)

model_nb_mixed <- glmmTMB(
  punti_per_tiro ~ gobert_on_court + home_game + poly(seconds, 5) + 
    poly(scarto, 5) + gobert_on_court:home_game + (1 | origine),
  family = nbinom2,
  data = shots_filtrati_poisson)
summary(model_nb_mixed)

VarCorr(model_nb_mixed)

library(MASS)
#binomiale negativa
mod_nb_finale <- glm.nb(punti_per_tiro ~ gobert_on_court + poly(seconds, 2) + home_game, 
                        data = shots_filtrati_poisson)
summary(mod_nb_finale)
