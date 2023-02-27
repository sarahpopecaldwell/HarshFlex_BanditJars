#title: "Harsh Flex Project"
#author: "Sarah Pope-Caldwell"
#Last updated date: "27/02/2023"

### Load packages and clear environment.
library(readxl)
library(tidyverse)
library(data.table)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(rstan)
library(brms)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(matrixStats)

#Clear Environment
rm(list =ls())

#Pallete
conditioncolors <- c("#76C5AD","#E89F68")
jarcolors <-c("#ffef00","#ed3d26","#71afa4","#11244e")

#### General Analyses #### 
##load datafile
bandit_df <- read.csv("round2_datafile.csv")

bandit_df <- bandit_df %>%
  mutate(switch = as.numeric(switch),
         subject_id = as.factor (subject_id),
         sex = as.numeric (Sex),
         rewardfound = as.numeric(rewardfound),
         universe = as.factor (universe),
         harshness = as.factor (harshness),
         age = as.numeric (scale(age)),
         looplabel = as.factor(looplabel))

#Calculate the proportion of optimal choices for all participants in each condition for all trials
bandit_df_perf <- bandit_df %>%
  mutate(bestchoice = ifelse(rewardfound < one, 0,
                             ifelse(rewardfound < two, 0,
                                    ifelse(rewardfound < three, 0,
                                           ifelse(rewardfound < four,0,1))))) %>%
  group_by(universe,harshness,subject_id) %>%
  summarise(mean_bestchoice = mean(bestchoice), sd_bestchoice = sd(bestchoice)) %>%
  mutate(chance = 25)

bandit_df_perf %>%
  ggplot(aes(y = mean_bestchoice, x = subject_id,color = harshness)) +
  geom_point() +
  facet_grid(~universe) + 
  geom_hline(aes(yintercept = .25)) +
  scale_colour_manual(values = conditioncolors) +
  theme(text  = element_text(size = 12),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black',size=12,face = 'bold'),
        axis.title.y = element_text(color = 'black',size=12,face="bold"),
        axis.text.x = element_blank()) +
  xlab('Trial') + ylab('Proportion Optimal Choices')

bandit_df_perf_stnh <- bandit_df_perf %>%
  filter(universe == "st" & harshness == "easy")
t.test(bandit_df_perf_stnh$mean_bestchoice, mu=.25, alt="g")
sd(bandit_df_perf_stnh$mean_bestchoice)

bandit_df_perf_sth <- bandit_df_perf %>%
  filter(universe == "st" & harshness == "harsh")
t.test(bandit_df_perf_sth$mean_bestchoice, mu=.25, alt="g")
sd(bandit_df_perf_sth$mean_bestchoice)

bandit_df_perf_uvnh <- bandit_df_perf %>%
  filter(universe == "uv" & harshness == "easy")
t.test(bandit_df_perf_uvnh$mean_bestchoice, mu=.25, alt="g")
sd(bandit_df_perf_uvnh$mean_bestchoice)

bandit_df_perf_uvh <- bandit_df_perf %>%
  filter(universe == "uv" & harshness == "harsh")
t.test(bandit_df_perf_uvh$mean_bestchoice, mu=.25, alt="g")
sd(bandit_df_perf_uvh$mean_bestchoice)

#Correlation check between average cumulative score and trial
bandit_df_perftime <- bandit_df %>%
  mutate(avg_cumscore = totalrewardfound/trial)

bandit_df_perftime_stnh <- bandit_df_perftime %>%
  filter(universe == "st" & harshness == "easy")
cor.test(bandit_df_perftime_stnh$trial,bandit_df_perftime_stnh$avg_cumscore) 

bandit_df_perftime_sth <- bandit_df_perftime %>%
  filter(universe == "st" & harshness == "harsh")
cor.test(bandit_df_perftime_sth$trial,bandit_df_perftime_sth$avg_cumscore) 

bandit_df_perftime_uvnh <- bandit_df_perftime %>%
  filter(universe == "uv" & harshness == "easy")
cor.test(bandit_df_perftime_uvnh$trial,bandit_df_perftime_uvnh$avg_cumscore) 

bandit_df_perftime_uvh <- bandit_df_perftime %>%
  filter(universe == "uv" & harshness == "harsh")
cor.test(bandit_df_perftime_uvh$trial,bandit_df_perftime_uvh$avg_cumscore) 

bandit_df_perftime %>%
  ggplot(aes(y = avg_cumscore, x = trial,color = harshness)) +
  geom_smooth() +
  facet_grid(~universe) + 
  geom_hline(aes(yintercept = .25)) +
  scale_colour_manual(values = jarcolors[c(1,3)]) +
  theme(text  = element_text(size = 12),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black',size=12,face = 'bold'),
        axis.title.y = element_text(color = 'black',size=12,face="bold"),
        axis.text.x = element_blank()) +
  xlab('Trial') + ylab('Cumulative average score')

#### Main switching Analyses #### 

#To run the models from scratch, uncomment the following section. Otherwise load pre-run models below.

##load datafile
#bandit_df <- read.csv("round2_datafile.csv")
# 
# bandit_df <- bandit_df %>%
#   mutate(switch = as.numeric(switch),
#          subject_id = as.factor (subject_id),
#          sex = as.numeric (Sex),
#          rewardfound = as.numeric(rewardfound),
#          universe = as.factor (universe),
#          harshness = as.factor (harshness),
#          age = as.numeric (scale(age)),
#          looplabel = as.factor(looplabel))
# 
# # Model 1.0
# M1_0 <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + (1|subject_id),
#   prior = prior(normal(0,1.5), class=Intercept),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M1_0 <- add_criterion(M1_0, "waic")
# M1_0 %>% save(M1_0, file = "M1_0.rda")
# 
# # Model 1.1
# M1_1 <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + rewardfound + (1 + rewardfound | subject_id),
#   prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, cores = 4, chains = 4, seed = 111)
# # #Add WAIC
# M1_1 <- add_criterion(M1_1, "waic")
# M1_1 %>% save(M1_1, file = "M1_1.rda")
# 
# #Model M1.2
# M1_2 <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + harshness + universe +
#                rewardfound*harshness +  rewardfound*universe +
#           (1 + harshness + universe +
#                rewardfound*harshness +  rewardfound*universe | subject_id),
#   prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 1.5), class="b") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="universeuv:rewardfound") + 
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M1_2 <- add_criterion(M1_2, "waic")
# M1_2 %>% save(M1_2, file = "M1_2.rda")
# 
# #Model 1.3
# M1_3 <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + harshness*universe +
#                rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness +
#           (1 + harshness*universe +
#                rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness | subject_id),
#     prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 1.5), class="b") +
#           prior(normal(0, 0.375), class="b", coef="harshnessharsh:universeuv") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="universeuv:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:universeuv:rewardfound") +
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M1_3 <- add_criterion(M1_3, "waic")
# M1_3 %>% save(M1_3, file = "M1_3.rda")

#Load prerun models
load("models/M1_0.rda")
load("models/M1_1.rda")
load("models/M1_2.rda")
load("models/M1_3.rda")

# Plot the conditional effects
cond_eff = conditional_effects(M1_3)
plot(cond_eff, plot = F)[[3]] +
  theme_classic()
plot(cond_eff, plot = F)[[4]] +
  theme_classic()
plot(cond_eff, plot = F)[[5]] +
  theme_classic()
plot(cond_eff, plot = F)[[6]] +
  theme_classic()

#Plot the three way interaction
conditions <- make_conditions(M1_3, 'universe') %>%
  mutate(cond__ = ifelse(cond__ == "universe = st", "Stable","Variable"))

p <- conditional_effects(M1_3, 'rewardfound:harshness', conditions=conditions)

plot(p, plot = FALSE)[[1]] + 
  theme_minimal() +
  scale_color_manual(values =conditioncolors) +
  scale_fill_manual(values = conditioncolors) +
  theme(text  = element_text(size = 16),
        panel.grid= element_blank(),
        axis.text.x = element_text(color = 'black',size=12),
        axis.text.y = element_text(color = 'black',size=12),
        legend.title = element_blank()) +
  labs(x = NULL, y = "Likelihood of switching",title = NULL, size = 20)

#Compare WAICs
w <- loo_compare(M1_0, M1_1, M1_2, M1_3,criterion = "waic")
round(w,2) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
# comparison
cbind(waic_diff = w[, 1] * -2,
      se        = w[, 2] *  2) %>%
  round(digits = 2) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
# visualize WAIC
w %>%
  data.frame() %>%
  rownames_to_column(var = "model") %>%
  mutate(model = ifelse(model == "M1_0","Model 1.0",
                        ifelse(model == "M1_1","Model 1.1",
                               ifelse(model == "M1_2","Model 1.2", "Model 1.3")))) %>%
  ggplot() +
  geom_pointrange(aes(x = reorder(model, -waic), y = waic,
                      ymin = waic - se_waic,
                      ymax = waic + se_waic,
                      color = model),
                  shape = 16,size = .4) +
  theme_classic() +
  scale_color_manual(values = jarcolors) +
  coord_flip() +
  labs(x = NULL, y = "WAIC",title = NULL) +
  theme(text  = element_text(size = 12),
        panel.grid= element_blank(),
        axis.text.x = element_text(color = 'black',size=12, angle = 25, hjust = 1),
        axis.text.y = element_text(color = 'black',size=12, face="bold"),
        legend.position = "none")
ggsave(height=1.5,width=3,"Model1 WAIC.png")

#tab_model(M1_0,M1_1,M1_2,M1_3, transform = NULL) # For log odds, transform = T

#### Main Switching Predictions #### 
# Generate new data
newdata <- expand_grid(universe=c("uv","st"), #both stable and variable conditions
                       rewardfound=seq(0, 100, .5), #reward values between 0-100
                       harshness=c("harsh", "easy")) #easy and harsh conditions


predictions <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                        newdata=newdata))

newdata$prob <- colMeans(predictions)
newdata$lower_prediction <- colQuantiles(predictions, probs=0.050)
newdata$upper_prediction <- colQuantiles(predictions, probs=0.950)

#Plot threeway effect
ggplot(newdata) +
  geom_line(aes(x=rewardfound, y=prob, color=harshness))+
  geom_ribbon(aes(x=rewardfound, ymin=lower_prediction, ymax=upper_prediction,
                  fill=harshness), alpha=0.15) +
  facet_grid(~universe) +
  theme_classic()

# ------------ LOW VALUE REWARDS, STABLE, NOT HARSH vs LOW VALUE REWARDS, STABLE, HARSH
#Generate new data for low value rewards (0-45), stable, harsh condition only
newdata_low_stable_harsh <- filter(newdata, universe == "st", rewardfound >= 0, rewardfound <= 45, harshness == "harsh") 
predictions_low_stable_harsh  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                          newdata=newdata_low_stable_harsh))
#Generate new data for low value rewards (0-45), stable, not harsh condition only
newdata_low_stable_easy <- filter(newdata, universe == "st", rewardfound >= 0, rewardfound <= 45, harshness == "easy") 
predictions_low_stable_easy  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                         newdata=newdata_low_stable_easy))
#Contrast quantiles
Quantils_low_stable <- as.data.frame(quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs=c(0.050, 0.950)))

# ------------ LOW VALUE REWARDS, VARIABLE, NOT HARSH vs LOW VALUE REWARDS, VARIABLE, HARSH
#Generate new data for low value rewards (0-45), variable, harsh condition only
newdata_low_variable_harsh <- filter(newdata, universe == "uv", rewardfound >= 0, rewardfound <= 45, harshness == "harsh") 
predictions_low_variable_harsh  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                            newdata=newdata_low_variable_harsh))
#Generate new data for low value rewards (0-45), variable, not harsh condition only
newdata_low_variable_easy <- filter(newdata, universe == "uv", rewardfound >= 0, rewardfound <= 45, harshness == "easy") 
predictions_low_variable_easy  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                           newdata=newdata_low_variable_easy))
#Contrast quantiles
Quantils_low_variable <- as.data.frame(quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, STABLE, NOT HARSH vs HIGH VALUE REWARDS, STABLE, HARSH
#Generate new data for high value rewards (55-100), stable, harsh condition only
newdata_high_stable_harsh <- filter(newdata, universe == "st", rewardfound >= 55, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_stable_harsh  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                           newdata=newdata_high_stable_harsh))
#Generate new data for high value rewards (55-100), stable, not harsh condition only
newdata_high_stable_easy <- filter(newdata, universe == "st", rewardfound >= 55, rewardfound <=100, harshness == "easy") #And this is high rewardfound values
predictions_high_stable_easy  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                          newdata=newdata_high_stable_easy))
#Contrast quantiles
Quantils_high_stable <- as.data.frame(quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, VARIABLE, NOT HARSH vs HIGH VALUE REWARDS, VARIABLE, HARSH
#Generate new data for high value rewards (55-100), variable, harsh condition only
newdata_high_variable_harsh <- filter(newdata, universe == "uv", rewardfound >= 55, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_variable_harsh  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                             newdata=newdata_high_variable_harsh))
#Generate new data for high value rewards (55-100), variable, not harsh condition only
newdata_high_variable_easy <- filter(newdata, universe == "uv", rewardfound >= 55, rewardfound <= 100, harshness == "easy") #And this is high rewardfound values
predictions_high_variable_easy  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                            newdata=newdata_high_variable_easy))
#Contrast quantiles
Quantils_high_variable <- as.data.frame(quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs=c(0.050, 0.950)))


#Generate overview
Quantils_low_stable <- tibble(Quantils_low_stable) %>%
  rename(value = `quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Low Stable")

Quantils_low_variable <- tibble(Quantils_low_variable) %>%
  rename(value = `quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Low Variable")

Quantils_high_stable <- tibble(Quantils_high_stable) %>%
  rename(value = `quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "High Stable")

Quantils_high_variable <- tibble(Quantils_high_variable) %>%
  rename(value = `quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "High Variable")


#Overview
Quantils_low_stable %>%
  add_row(Quantils_high_stable) %>%
  add_row(Quantils_low_variable) %>%
  add_row(Quantils_high_variable) %>%
  spread(key = quantile,value = value) %>%
  mutate(`5%` = round(`5%`,2),`95%` = round(`95%`,2)) %>%
  rename(` ` = condition) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)

#### Main Switching Predictions with tighter responsive/elective ####
# Generate new data
newdata <- expand_grid(universe=c("uv","st"), #both stable and variable conditions
                       rewardfound=seq(0, 100, .5), #reward values between 0-100
                       harshness=c("harsh", "easy")) #easy and harsh conditions


predictions <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                        newdata=newdata))

newdata$prob <- colMeans(predictions)
newdata$lower_prediction <- colQuantiles(predictions, probs=0.050)
newdata$upper_prediction <- colQuantiles(predictions, probs=0.950)

#Plot threeway effect
ggplot(newdata) +
  geom_line(aes(x=rewardfound, y=prob, color=harshness))+
  geom_ribbon(aes(x=rewardfound, ymin=lower_prediction, ymax=upper_prediction,
                  fill=harshness), alpha=0.15) +
  facet_grid(~universe) +
  theme_classic()

# ------------ LOW VALUE REWARDS, STABLE, NOT HARSH vs LOW VALUE REWARDS, STABLE, HARSH
#Generate new data for low value rewards (0-45), stable, harsh condition only
newdata_low_stable_harsh <- filter(newdata, universe == "st", rewardfound >= 0, rewardfound <= 25, harshness == "harsh") 
predictions_low_stable_harsh  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                          newdata=newdata_low_stable_harsh))
#Generate new data for low value rewards (0-45), stable, not harsh condition only
newdata_low_stable_easy <- filter(newdata, universe == "st", rewardfound >= 0, rewardfound <= 25, harshness == "easy") 
predictions_low_stable_easy  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                         newdata=newdata_low_stable_easy))
#Contrast quantiles
Quantils_low_stable <- as.data.frame(quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs=c(0.050, 0.950)))

# ------------ LOW VALUE REWARDS, VARIABLE, NOT HARSH vs LOW VALUE REWARDS, VARIABLE, HARSH
#Generate new data for low value rewards (0-45), variable, harsh condition only
newdata_low_variable_harsh <- filter(newdata, universe == "uv", rewardfound >= 0, rewardfound <= 25, harshness == "harsh") 
predictions_low_variable_harsh  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                            newdata=newdata_low_variable_harsh))
#Generate new data for low value rewards (0-45), variable, not harsh condition only
newdata_low_variable_easy <- filter(newdata, universe == "uv", rewardfound >= 0, rewardfound <= 25, harshness == "easy") 
predictions_low_variable_easy  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                           newdata=newdata_low_variable_easy))
#Contrast quantiles
Quantils_low_variable <- as.data.frame(quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, STABLE, NOT HARSH vs HIGH VALUE REWARDS, STABLE, HARSH
#Generate new data for high value rewards (55-100), stable, harsh condition only
newdata_high_stable_harsh <- filter(newdata, universe == "st", rewardfound >= 75, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_stable_harsh  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                           newdata=newdata_high_stable_harsh))
#Generate new data for high value rewards (55-100), stable, not harsh condition only
newdata_high_stable_easy <- filter(newdata, universe == "st", rewardfound >= 75, rewardfound <=100, harshness == "easy") #And this is high rewardfound values
predictions_high_stable_easy  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                          newdata=newdata_high_stable_easy))
#Contrast quantiles
Quantils_high_stable <- as.data.frame(quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, VARIABLE, NOT HARSH vs HIGH VALUE REWARDS, VARIABLE, HARSH
#Generate new data for high value rewards (55-100), variable, harsh condition only
newdata_high_variable_harsh <- filter(newdata, universe == "uv", rewardfound >= 75, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_variable_harsh  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                             newdata=newdata_high_variable_harsh))
#Generate new data for high value rewards (55-100), variable, not harsh condition only
newdata_high_variable_easy <- filter(newdata, universe == "uv", rewardfound >= 75, rewardfound <= 100, harshness == "easy") #And this is high rewardfound values
predictions_high_variable_easy  <- plogis(posterior_linpred(M1_3, re_formula = NA,
                                                            newdata=newdata_high_variable_easy))
#Contrast quantiles
Quantils_high_variable <- as.data.frame(quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs=c(0.050, 0.950)))


#Generate overview
Quantils_low_stable <- tibble(Quantils_low_stable) %>%
  rename(value = `quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Lower Stable")

Quantils_low_variable <- tibble(Quantils_low_variable) %>%
  rename(value = `quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Lower Variable")

Quantils_high_stable <- tibble(Quantils_high_stable) %>%
  rename(value = `quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Higher Stable")

Quantils_high_variable <- tibble(Quantils_high_variable) %>%
  rename(value = `quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Higher Variable")


#Overview
Quantils_low_stable %>%
  add_row(Quantils_high_stable) %>%
  add_row(Quantils_low_variable) %>%
  add_row(Quantils_high_variable) %>%
  spread(key = quantile,value = value) %>%
  mutate(`5%` = round(`5%`,2),`95%` = round(`95%`,2)) %>%
  rename(` ` = condition) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
```

#### Sex/Age Switching Analyses #####

#To run the models from scratch, uncomment the following section. Otherwise load pre-run models below.
##load datafile
#bandit_df <- read.csv("round2_datafile.csv")
# 
# bandit_df <- bandit_df %>%
#   mutate(switch = as.numeric(switch),
#          subject_id = as.factor (subject_id),
#          sex = as.numeric (Sex),
#          rewardfound = as.numeric(rewardfound),
#          universe = as.factor (universe),
#          harshness = as.factor (harshness),
#          age = as.numeric (scale(age)),
#          looplabel = as.factor(looplabel))
# 
# # Model 1.0
# M2_0_agesex <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + (1|subject_id),
#   prior = prior(normal(0,1.5), class=Intercept),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M2_0_agesex <- add_criterion(M2_0_agesex, "waic")
# M2_0_agesex %>% save(M2_0_agesex, file = "M2_0_agesex.rda")
# 
# # Model 1.1
# M2_1_agesex <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + rewardfound + age + sex + looplabel + (1 + rewardfound | subject_id),
#   prior = prior(normal(0,1.5), class="Intercept") +
#                prior(normal(0, 1.5), class="b") +
#                prior(normal(0, 0.1), class="b", coef="rewardfound") +
#                prior(normal(0, 0.375), class="b", coef="age") +
#                prior(normal(0, .75), class="b", coef="sex") +
#                prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M2_1_agesex <- add_criterion(M2_1_agesex, "waic")
# M2_1_agesex %>% save(M2_1_agesex, file = "M2_1_agesex.rda")
# 
# #Model M1.2
# M2_2_agesex <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + harshness + universe + age + sex + looplabel +
#                rewardfound*harshness +  rewardfound*universe +
#           (1 + harshness + universe +
#                rewardfound*harshness +  rewardfound*universe | subject_id),
#   prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 1.5), class="b") +
#           prior(normal(0, 0.375), class="b", coef="age") +
#           prior(normal(0, .75), class="b", coef="sex") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="universeuv:rewardfound") + 
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M2_2_agesex <- add_criterion(M2_2_agesex, "waic")
# M2_2_agesex %>% save(M2_2_agesex, file = "M2_2_agesex.rda")
# 
# #Model 1.3
# M2_3_agesex <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + harshness*universe + age + sex + looplabel +
#                rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness +
#           (1 + harshness*universe +
#                rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness | subject_id),
#     prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 1.5), class="b") +
#           prior(normal(0, 0.375), class="b", coef="age") +
#           prior(normal(0, .75), class="b", coef="sex") +
#           prior(normal(0, 0.375), class="b", coef="harshnessharsh:universeuv") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="universeuv:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:universeuv:rewardfound") +
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M2_3_agesex <- add_criterion(M2_3_agesex, "waic")
# M2_3_agesex %>% save(M2_3_agesex, file = "M2_3_agesex.rda")

#Load prerun models
load("models/M2_0_agesex.rda")
load("models/M2_1_agesex.rda")
load("models/M2_2_agesex.rda")
load("models/M2_3_agesex.rda")

# Plot the conditional effects
cond_eff = conditional_effects(M2_3_agesex)
plot(cond_eff, plot = F)[[3]] +
  theme_classic()
plot(cond_eff, plot = F)[[4]] +
  theme_classic()
plot(cond_eff, plot = F)[[5]] +
  theme_classic()
plot(cond_eff, plot = F)[[6]] +
  theme_classic()

#Plot the three way interaction
conditions <- make_conditions(M2_3_agesex, 'universe') %>%
  mutate(cond__ = ifelse(cond__ == "universe = st", "Stable","Variable"))

p <- conditional_effects(M2_3_agesex, 'rewardfound:harshness', conditions=conditions)

plot(p, plot = FALSE)[[1]] + 
  theme_minimal() +
  scale_color_manual(values = conditioncolors) +
  scale_fill_manual(values = conditioncolors) +
  labs(x = NULL, y = "Likelihood of switching",title = NULL) +
  theme(text  = element_text(size = 12),
        panel.grid= element_blank(),
        axis.text.x = element_text(color = 'black',size=12),
        axis.text.y = element_text(color = 'black',size=12, face="bold"),
        legend.title = element_blank())

#Compare WAICs
w <- loo_compare(M2_0_agesex, M2_1_agesex, M2_2_agesex, M2_3_agesex,criterion = "waic")
round(w,2) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
# comparison
cbind(waic_diff = w[, 1] * -2,
      se        = w[, 2] *  2) %>%
  round(digits = 2) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
# visualize WAIC
w %>%
  data.frame() %>%
  rownames_to_column(var = "model") %>%
  mutate(model = ifelse(model == "M2_0_agesex","Model 2.0",
                        ifelse(model == "M2_1_agesex","Model 2.1",
                               ifelse(model == "M2_2_agesex","Model 2.2", "Model 2.3")))) %>%
  ggplot() +
  geom_pointrange(aes(x = reorder(model, -waic), y = waic,
                      ymin = waic - se_waic,
                      ymax = waic + se_waic,
                      color = model),
                  shape = 16,size = .4) +
  theme_classic() +
  scale_color_manual(values = jarcolors) +
  coord_flip() +
  labs(x = NULL, y = "WAIC",title = NULL) +
  theme(text  = element_text(size = 12),
        panel.grid= element_blank(),
        axis.text.x = element_text(color = 'black',size=12, angle = 45, hjust = 1),
        axis.text.y = element_text(color = 'black',size=12, face="bold"),
        legend.position = "none")
ggsave(height=1.5,width=3,"Model2 WAIC.png")

#tab_model(M2_0_agesex,M2_1_agesex,M2_2_agesex,M2_3_agesex, transform = NULL) # For log odds, transform = T

#### Sex/Age Switching Predictions ####
# Generate new data
newdata <- expand_grid(universe=c("uv","st"), #both stable and variable conditions
                       rewardfound=seq(0, 100, .5), #reward values between 0-100
                       harshness=c("harsh", "easy"), #easy and harsh conditions
                       age = 0, #Average age
                       looplabel = c("loop1","loop2","loop3","loop4","loop5"), #All reward schedules
                       sex = .5) #Average sex


predictions <- plogis(posterior_linpred(M2_3_agesex, re_formula = NA,
                                        newdata=newdata))

newdata$prob <- colMeans(predictions)
newdata$lower_prediction <- colQuantiles(predictions, probs=0.050)
newdata$upper_prediction <- colQuantiles(predictions, probs=0.950)

#Plot threeway effect
ggplot(newdata) +
  geom_line(aes(x=rewardfound, y=prob, color=harshness))+
  geom_ribbon(aes(x=rewardfound, ymin=lower_prediction, ymax=upper_prediction,
                  fill=harshness), alpha=0.15) +
  facet_grid(~universe) +
  theme_classic()

# ------------ LOW VALUE REWARDS, STABLE, NOT HARSH vs LOW VALUE REWARDS, STABLE, HARSH
#Generate new data for low value rewards (0-45), stable, harsh condition only
newdata_low_stable_harsh <- filter(newdata, universe == "st", rewardfound >= 0, rewardfound <= 45, harshness == "harsh") 
predictions_low_stable_harsh  <- plogis(posterior_linpred(M2_3_agesex, re_formula = NA,
                                                          newdata=newdata_low_stable_harsh))
#Generate new data for low value rewards (0-45), stable, not harsh condition only
newdata_low_stable_easy <- filter(newdata, universe == "st", rewardfound >= 0, rewardfound <= 45, harshness == "easy") 
predictions_low_stable_easy  <- plogis(posterior_linpred(M2_3_agesex, re_formula = NA,
                                                         newdata=newdata_low_stable_easy))
#Contrast quantiles
Quantils_low_stable <- as.data.frame(quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs=c(0.050, 0.950)))

# ------------ LOW VALUE REWARDS, VARIABLE, NOT HARSH vs LOW VALUE REWARDS, VARIABLE, HARSH
#Generate new data for low value rewards (0-45), variable, harsh condition only
newdata_low_variable_harsh <- filter(newdata, universe == "uv", rewardfound >= 0, rewardfound <= 45, harshness == "harsh") 
predictions_low_variable_harsh  <- plogis(posterior_linpred(M2_3_agesex, re_formula = NA,
                                                            newdata=newdata_low_variable_harsh))
#Generate new data for low value rewards (0-45), variable, not harsh condition only
newdata_low_variable_easy <- filter(newdata, universe == "uv", rewardfound >= 0, rewardfound <= 45, harshness == "easy") 
predictions_low_variable_easy  <- plogis(posterior_linpred(M2_3_agesex, re_formula = NA,
                                                           newdata=newdata_low_variable_easy))
#Contrast quantiles
Quantils_low_variable <- as.data.frame(quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, STABLE, NOT HARSH vs HIGH VALUE REWARDS, STABLE, HARSH
#Generate new data for high value rewards (55-100), stable, harsh condition only
newdata_high_stable_harsh <- filter(newdata, universe == "st", rewardfound >= 55, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_stable_harsh  <- plogis(posterior_linpred(M2_3_agesex, re_formula = NA,
                                                           newdata=newdata_high_stable_harsh))
#Generate new data for high value rewards (55-100), stable, not harsh condition only
newdata_high_stable_easy <- filter(newdata, universe == "st", rewardfound >= 55, rewardfound <=100, harshness == "easy") #And this is high rewardfound values
predictions_high_stable_easy  <- plogis(posterior_linpred(M2_3_agesex, re_formula = NA,
                                                          newdata=newdata_high_stable_easy))
#Contrast quantiles
Quantils_high_stable <- as.data.frame(quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, VARIABLE, NOT HARSH vs HIGH VALUE REWARDS, VARIABLE, HARSH
#Generate new data for high value rewards (55-100), variable, harsh condition only
newdata_high_variable_harsh <- filter(newdata, universe == "uv", rewardfound >= 55, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_variable_harsh  <- plogis(posterior_linpred(M2_3_agesex, re_formula = NA,
                                                             newdata=newdata_high_variable_harsh))
#Generate new data for high value rewards (55-100), variable, not harsh condition only
newdata_high_variable_easy <- filter(newdata, universe == "uv", rewardfound >= 55, rewardfound <= 100, harshness == "easy") #And this is high rewardfound values
predictions_high_variable_easy  <- plogis(posterior_linpred(M2_3_agesex, re_formula = NA,
                                                            newdata=newdata_high_variable_easy))
#Contrast quantiles
Quantils_high_variable <- as.data.frame(quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs=c(0.050, 0.950)))


#Generate overview
Quantils_low_stable <- tibble(Quantils_low_stable) %>%
  rename(value = `quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Low Stable")

Quantils_low_variable <- tibble(Quantils_low_variable) %>%
  rename(value = `quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Low Variable")

Quantils_high_stable <- tibble(Quantils_high_stable) %>%
  rename(value = `quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "High Stable")

Quantils_high_variable <- tibble(Quantils_high_variable) %>%
  rename(value = `quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "High Variable")


#Overview
Quantils_low_stable %>%
  add_row(Quantils_high_stable) %>%
  add_row(Quantils_low_variable) %>%
  add_row(Quantils_high_variable) %>%
  spread(key = quantile,value = value) %>%
  mutate(`5%` = round(`5%`,2),`95%` = round(`95%`,2)) %>%
  rename(` ` = condition) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)

#### Harshcheck Analyses ####

#To run the models from scratch, uncomment the following section. Otherwise load pre-run models below.

#bandit_df <- round2_datafile_onlyreportedharsh

# bandit_df <- bandit_df %>%
#   mutate(switch = as.numeric(switch),
#          subject_id = as.factor (subject_id),
#          sex = as.numeric (Sex),
#          rewardfound = as.numeric(rewardfound),
#          universe = as.factor (universe),
#          harshness = as.factor (harshness),
#          age = as.numeric (scale(age)),
#          looplabel = as.factor(looplabel))
# 
# # Model 1.0
# M3_0_hc <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + (1|subject_id),
#   prior = prior(normal(0,1.5), class=Intercept),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M3_0_hc <- add_criterion(M3_0_hc, "waic")
# M3_0_hc %>% save(M3_0_hc, file = "M3_0_hc.rda")
# 
# # Model 1.1
# M3_3_hc <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + rewardfound + (1 + rewardfound | subject_id),
#   prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, cores = 4, chains = 4, seed = 111)
# # #Add WAIC
# M3_1_hc <- add_criterion(M3_1_hc, "waic")
# M3_1_hc %>% save(M3_1_hc, file = "M3_1_hc.rda")
# 
# #Model M1.2
# M3_2_hc <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + harshness + universe +
#                rewardfound*harshness +  rewardfound*universe +
#           (1 + harshness + universe +
#                rewardfound*harshness +  rewardfound*universe | subject_id),
#   prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 1.5), class="b") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="universeuv:rewardfound") + 
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M3_2_hc <- add_criterion(M3_2_hc, "waic")
# M3_2_hc %>% save(M3_2_hc, file = "M3_2_hc.rda")
# 
# #Model 1.3
# M3_3_hc <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + harshness*universe +
#                rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness +
#           (1 + harshness*universe +
#                rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness | subject_id),
#     prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 1.5), class="b") +
#           prior(normal(0, 0.375), class="b", coef="harshnessharsh:universeuv") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="universeuv:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:universeuv:rewardfound") +
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M3_3_hc <- add_criterion(M3_3_hc, "waic")
# M3_3_hc %>% save(M3_3_hc, file = "M3_3_hc.rda")


#Load prerun models
load("models/M3_0_hc.rda")
load("models/M3_1_hc.rda")
load("models/M3_2_hc.rda")
load("models/M3_3_hc.rda")

# Plot the conditional effects
cond_eff = conditional_effects(M3_3_hc)
plot(cond_eff, plot = F)[[3]] +
  theme_classic()
plot(cond_eff, plot = F)[[4]] +
  theme_classic()
plot(cond_eff, plot = F)[[5]] +
  theme_classic()
plot(cond_eff, plot = F)[[6]] +
  theme_classic()

#Plot the three way interaction
conditions <- make_conditions(M3_3_hc, 'universe') %>%
  mutate(cond__ = ifelse(cond__ == "universe = st", "Stable","Variable"))

p <- conditional_effects(M3_3_hc, 'rewardfound:harshness', conditions=conditions)

plot(p, plot = FALSE)[[1]] + 
  theme_minimal() +
  scale_color_manual(values = conditioncolors) +
  scale_fill_manual(values = conditioncolors) +
  labs(x = NULL, y = "Likelihood of switching",title = NULL) +
  theme(text  = element_text(size = 12),
        panel.grid= element_blank(),
        axis.text.x = element_text(color = 'black',size=12),
        axis.text.y = element_text(color = 'black',size=12, face="bold"),
        legend.title = element_blank())

#Compare WAICs
w <- loo_compare(M3_0_hc, M3_1_hc, M3_2_hc, M3_3_hc,criterion = "waic")
round(w,2) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
# comparison
cbind(waic_diff = w[, 1] * -2,
      se        = w[, 2] *  2) %>%
  round(digits = 2) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
# visualize WAIC
w %>%
  data.frame() %>%
  rownames_to_column(var = "model") %>%
  mutate(model = ifelse(model == "M3_0_hc","Model 3.0",
                        ifelse(model == "M3_1_hc","Model 3.1",
                               ifelse(model == "M3_2_hc","Model 3.2", "Model 3.3")))) %>%
  ggplot() +
  geom_pointrange(aes(x = reorder(model, -waic), y = waic,
                      ymin = waic - se_waic,
                      ymax = waic + se_waic,
                      color = model),
                  shape = 16,size = .4) +
  theme_classic() +
  scale_color_manual(values = jarcolors) +
  coord_flip() +
  labs(x = NULL, y = "WAIC",title = NULL) +
  theme(text  = element_text(size = 12),
        panel.grid= element_blank(),
        axis.text.x = element_text(color = 'black',size=12, angle = 45, hjust = 1),
        axis.text.y = element_text(color = 'black',size=12, face="bold"),
        legend.position = "none")
ggsave(height=1.5,width=3,"Model3 WAIC.png")

#tab_model(M3_0_hc,M3_1_hc,M3_2_hc,M3_3_hc, transform = NULL) # For log odds, transform = T


#### Harshcheck Predictions #####
# Generate new data
newdata <- expand_grid(universe=c("uv","st"), #both stable and variable conditions
                       rewardfound=seq(0, 100, .5), #reward values between 0-100
                       harshness=c("harsh", "easy")) #easy and harsh conditions


predictions <- plogis(posterior_linpred(M3_3_hc, re_formula = NA,
                                        newdata=newdata))

newdata$prob <- colMeans(predictions)
newdata$lower_prediction <- colQuantiles(predictions, probs=0.050)
newdata$upper_prediction <- colQuantiles(predictions, probs=0.950)

#Plot threeway effect
ggplot(newdata) +
  geom_line(aes(x=rewardfound, y=prob, color=harshness))+
  geom_ribbon(aes(x=rewardfound, ymin=lower_prediction, ymax=upper_prediction,
                  fill=harshness), alpha=0.15) +
  facet_grid(~universe) +
  theme_classic()

# ------------ LOW VALUE REWARDS, STABLE, NOT HARSH vs LOW VALUE REWARDS, STABLE, HARSH
#Generate new data for low value rewards (0-45), stable, harsh condition only
newdata_low_stable_harsh <- filter(newdata, universe == "st", rewardfound >= 0, rewardfound <= 45, harshness == "harsh") 
predictions_low_stable_harsh  <- plogis(posterior_linpred(M3_3_hc, re_formula = NA,
                                                          newdata=newdata_low_stable_harsh))
#Generate new data for low value rewards (0-45), stable, not harsh condition only
newdata_low_stable_easy <- filter(newdata, universe == "st", rewardfound >= 0, rewardfound <= 45, harshness == "easy") 
predictions_low_stable_easy  <- plogis(posterior_linpred(M3_3_hc, re_formula = NA,
                                                         newdata=newdata_low_stable_easy))
#Contrast quantiles
Quantils_low_stable <- as.data.frame(quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs=c(0.050, 0.950)))

# ------------ LOW VALUE REWARDS, VARIABLE, NOT HARSH vs LOW VALUE REWARDS, VARIABLE, HARSH
#Generate new data for low value rewards (0-45), variable, harsh condition only
newdata_low_variable_harsh <- filter(newdata, universe == "uv", rewardfound >= 0, rewardfound <= 45, harshness == "harsh") 
predictions_low_variable_harsh  <- plogis(posterior_linpred(M3_3_hc, re_formula = NA,
                                                            newdata=newdata_low_variable_harsh))
#Generate new data for low value rewards (0-45), variable, not harsh condition only
newdata_low_variable_easy <- filter(newdata, universe == "uv", rewardfound >= 0, rewardfound <= 45, harshness == "easy") 
predictions_low_variable_easy  <- plogis(posterior_linpred(M3_3_hc, re_formula = NA,
                                                           newdata=newdata_low_variable_easy))
#Contrast quantiles
Quantils_low_variable <- as.data.frame(quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, STABLE, NOT HARSH vs HIGH VALUE REWARDS, STABLE, HARSH
#Generate new data for high value rewards (55-100), stable, harsh condition only
newdata_high_stable_harsh <- filter(newdata, universe == "st", rewardfound >= 55, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_stable_harsh  <- plogis(posterior_linpred(M3_3_hc, re_formula = NA,
                                                           newdata=newdata_high_stable_harsh))
#Generate new data for high value rewards (55-100), stable, not harsh condition only
newdata_high_stable_easy <- filter(newdata, universe == "st", rewardfound >= 55, rewardfound <=100, harshness == "easy") #And this is high rewardfound values
predictions_high_stable_easy  <- plogis(posterior_linpred(M3_3_hc, re_formula = NA,
                                                          newdata=newdata_high_stable_easy))
#Contrast quantiles
Quantils_high_stable <- as.data.frame(quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, VARIABLE, NOT HARSH vs HIGH VALUE REWARDS, VARIABLE, HARSH
#Generate new data for high value rewards (55-100), variable, harsh condition only
newdata_high_variable_harsh <- filter(newdata, universe == "uv", rewardfound >= 55, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_variable_harsh  <- plogis(posterior_linpred(M3_3_hc, re_formula = NA,
                                                             newdata=newdata_high_variable_harsh))
#Generate new data for high value rewards (55-100), variable, not harsh condition only
newdata_high_variable_easy <- filter(newdata, universe == "uv", rewardfound >= 55, rewardfound <= 100, harshness == "easy") #And this is high rewardfound values
predictions_high_variable_easy  <- plogis(posterior_linpred(M3_3_hc, re_formula = NA,
                                                            newdata=newdata_high_variable_easy))
#Contrast quantiles
Quantils_high_variable <- as.data.frame(quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs=c(0.050, 0.950)))


#Generate overview
Quantils_low_stable <- tibble(Quantils_low_stable) %>%
  rename(value = `quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Low Stable")

Quantils_low_variable <- tibble(Quantils_low_variable) %>%
  rename(value = `quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Low Variable")

Quantils_high_stable <- tibble(Quantils_high_stable) %>%
  rename(value = `quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "High Stable")

Quantils_high_variable <- tibble(Quantils_high_variable) %>%
  rename(value = `quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "High Variable")


#Overview
Quantils_low_stable %>%
  add_row(Quantils_high_stable) %>%
  add_row(Quantils_low_variable) %>%
  add_row(Quantils_high_variable) %>%
  spread(key = quantile,value = value) %>%
  mutate(`5%` = round(`5%`,2),`95%` = round(`95%`,2)) %>%
  rename(` ` = condition) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)


###### Round 1 Analyses #####
#To run the models from scratch, uncomment the following section. Otherwise load pre-run models below.

# bandit_df <- supplemental_round1_datafile
# 
# bandit_df <- bandit_df %>%
#   mutate(switch = as.numeric(switch),
#          subject_id = as.factor (subject_id),
#          rewardfound = as.numeric(rewardfound),
#          universe = as.factor (universe),
#          harshness = as.factor (harshness))
# 
# # Model 1.0
# M4_0_round1 <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + (1|subject_id),
#   prior = prior(normal(0,1.5), class=Intercept),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M4_0_round1 <- add_criterion(M4_0_round1, "waic")
# M4_0_round1 %>% save(M4_0_round1, file = "M4_0_round1.rda")
# 
# # Model 1.1
# M4_1_round1 <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + rewardfound + (1 + rewardfound | subject_id),
#   prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, cores = 4, chains = 4, seed = 111)
# # #Add WAIC
# M4_1_round1 <- add_criterion(M4_1_round1, "waic")
# M4_1_round1 %>% save(M4_1_round1, file = "M4_1_round1.rda")
# 
# #Model M1.2
# M4_2_round1 <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + harshness + universe +
#                rewardfound*harshness +  rewardfound*universe +
#           (1 + harshness + universe +
#                rewardfound*harshness +  rewardfound*universe | subject_id),
#   prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 1.5), class="b") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="universevariable:rewardfound") +
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# 
# # #Add WAIC
# M4_2_round1 <- add_criterion(M4_2_round1, "waic")
# M4_2_round1 %>% save(M4_2_round1, file = "M4_2_round1.rda")
# 
# #Model 1.3
# M4_3_round1 <- brm(data = bandit_df, family = bernoulli,
#   switch ~ 1 + harshness*universe +
#                rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness +
#           (1 + harshness*universe +
#                rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness | subject_id),
#     prior = prior(normal(0,1.5), class="Intercept") +
#           prior(normal(0, 1.5), class="b") +
#           prior(normal(0, 0.375), class="b", coef="harshnessharsh:universevariable") +
#           prior(normal(0, 0.1), class="b", coef="rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="universevariable:rewardfound") +
#           prior(normal(0, 0.05), class="b", coef="harshnessharsh:universevariable:rewardfound") +
#           prior(exponential(10), class="sd"),
#   iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
# # #Add WAIC
# M4_3_round1 <- add_criterion(M4_3_round1, "waic")
# M4_3_round1 %>% save(M4_3_round1, file = "M4_3_round1.rda")


#Load prerun models
load("models/M4_0_round1.rda")
load("models/M4_1_round1.rda")
load("models/M4_2_round1.rda")
load("models/M4_3_round1.rda")

# Plot the conditional effects
cond_eff = conditional_effects(M4_3_round1)
plot(cond_eff, plot = F)[[3]] +
  theme_classic()
plot(cond_eff, plot = F)[[4]] +
  theme_classic()
plot(cond_eff, plot = F)[[5]] +
  theme_classic()
plot(cond_eff, plot = F)[[6]] +
  theme_classic()

#Plot the three way interaction
conditions <- make_conditions(M4_3_round1, 'universe') %>%
  mutate(cond__ = ifelse(cond__ == "universe = stable", "Stable","Variable"))

p <- conditional_effects(M4_3_round1, 'rewardfound:harshness', conditions=conditions)

plot(p, plot = FALSE)[[1]] + 
  theme_minimal() +
  scale_color_manual(values = conditioncolors) +
  scale_fill_manual(values = conditioncolors) +
  labs(x = NULL, y = "Likelihood of switching",title = NULL) +
  theme(text  = element_text(size = 12),
        panel.grid= element_blank(),
        axis.text.x = element_text(color = 'black',size=12),
        axis.text.y = element_text(color = 'black',size=12, face="bold"),
        legend.title = element_blank())

#Compare WAICs
w <- loo_compare(M4_0_round1, M4_1_round1, M4_2_round1, M4_3_round1,criterion = "waic")
round(w,2) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
# comparison
cbind(waic_diff = w[, 1] * -2,
      se        = w[, 2] *  2) %>%
  round(digits = 2) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
# visualize WAIC
w %>%
  data.frame() %>%
  rownames_to_column(var = "model") %>%
  mutate(model = ifelse(model == "M4_0_round1","Model 4.0",
                        ifelse(model == "M4_1_round1","Model 4.1",
                               ifelse(model == "M4_2_round1","Model 4.2", "Model 4.3")))) %>%
  ggplot() +
  geom_pointrange(aes(x = reorder(model, -waic), y = waic,
                      ymin = waic - se_waic,
                      ymax = waic + se_waic,
                      color = model),
                  shape = 16,size = .4) +
  theme_classic() +
  scale_color_manual(values = jarcolors) +
  coord_flip() +
  labs(x = NULL, y = "WAIC",title = NULL) +
  theme(text  = element_text(size = 12),
        panel.grid= element_blank(),
        axis.text.x = element_text(color = 'black',size=12, angle = 45, hjust = 1),
        axis.text.y = element_text(color = 'black',size=12, face="bold"),
        legend.position = "none")
ggsave(height=1.5,width=3,"Model4 WAIC.png")

#tab_model(M4_0_round1,M4_1_round1,M4_2_round1,M4_3_round1, transform = NULL) # For log odds, transform = T

##### Round 1 Predictions #####
# Generate new data
newdata <- expand_grid(universe=c("variable","stable"), #both stable and variable conditions
                       rewardfound=seq(0, 100, .5), #reward values between 0-100
                       harshness=c("harsh", "easy")) #easy and harsh conditions


predictions <- plogis(posterior_linpred(M4_3_round1, re_formula = NA,
                                        newdata=newdata))

newdata$prob <- colMeans(predictions)
newdata$lower_prediction <- colQuantiles(predictions, probs=0.050)
newdata$upper_prediction <- colQuantiles(predictions, probs=0.950)

#Plot threeway effect
ggplot(newdata) +
  geom_line(aes(x=rewardfound, y=prob, color=harshness))+
  geom_ribbon(aes(x=rewardfound, ymin=lower_prediction, ymax=upper_prediction,
                  fill=harshness), alpha=0.15) +
  facet_grid(~universe) +
  theme_classic()

# ------------ LOW VALUE REWARDS, STABLE, NOT HARSH vs LOW VALUE REWARDS, STABLE, HARSH
#Generate new data for low value rewards (0-45), stable, harsh condition only
newdata_low_stable_harsh <- filter(newdata, universe == "stable", rewardfound >= 0, rewardfound <= 45, harshness == "harsh") 
predictions_low_stable_harsh  <- plogis(posterior_linpred(M4_3_round1, re_formula = NA,
                                                          newdata=newdata_low_stable_harsh))
#Generate new data for low value rewards (0-45), stable, not harsh condition only
newdata_low_stable_easy <- filter(newdata, universe == "stable", rewardfound >= 0, rewardfound <= 45, harshness == "easy") 
predictions_low_stable_easy  <- plogis(posterior_linpred(M4_3_round1, re_formula = NA,
                                                         newdata=newdata_low_stable_easy))
#Contrast quantiles
Quantils_low_stable <- as.data.frame(quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs=c(0.050, 0.950)))

# ------------ LOW VALUE REWARDS, VARIABLE, NOT HARSH vs LOW VALUE REWARDS, VARIABLE, HARSH
#Generate new data for low value rewards (0-45), variable, harsh condition only
newdata_low_variable_harsh <- filter(newdata, universe == "variable", rewardfound >= 0, rewardfound <= 45, harshness == "harsh") 
predictions_low_variable_harsh  <- plogis(posterior_linpred(M4_3_round1, re_formula = NA,
                                                            newdata=newdata_low_variable_harsh))
#Generate new data for low value rewards (0-45), variable, not harsh condition only
newdata_low_variable_easy <- filter(newdata, universe == "variable", rewardfound >= 0, rewardfound <= 45, harshness == "easy") 
predictions_low_variable_easy  <- plogis(posterior_linpred(M4_3_round1, re_formula = NA,
                                                           newdata=newdata_low_variable_easy))
#Contrast quantiles
Quantils_low_variable <- as.data.frame(quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, STABLE, NOT HARSH vs HIGH VALUE REWARDS, STABLE, HARSH
#Generate new data for high value rewards (55-100), stable, harsh condition only
newdata_high_stable_harsh <- filter(newdata, universe == "stable", rewardfound >= 55, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_stable_harsh  <- plogis(posterior_linpred(M4_3_round1, re_formula = NA,
                                                           newdata=newdata_high_stable_harsh))
#Generate new data for high value rewards (55-100), stable, not harsh condition only
newdata_high_stable_easy <- filter(newdata, universe == "stable", rewardfound >= 55, rewardfound <=100, harshness == "easy") #And this is high rewardfound values
predictions_high_stable_easy  <- plogis(posterior_linpred(M4_3_round1, re_formula = NA,
                                                          newdata=newdata_high_stable_easy))
#Contrast quantiles
Quantils_high_stable <- as.data.frame(quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs=c(0.050, 0.950)))

# ------------ HIGH VALUE REWARDS, VARIABLE, NOT HARSH vs HIGH VALUE REWARDS, VARIABLE, HARSH
#Generate new data for high value rewards (55-100), variable, harsh condition only
newdata_high_variable_harsh <- filter(newdata, universe == "variable", rewardfound >= 55, rewardfound <= 100, harshness == "harsh") #So this is high rewardfound values
predictions_high_variable_harsh  <- plogis(posterior_linpred(M4_3_round1, re_formula = NA,
                                                             newdata=newdata_high_variable_harsh))
#Generate new data for high value rewards (55-100), variable, not harsh condition only
newdata_high_variable_easy <- filter(newdata, universe == "variable", rewardfound >= 55, rewardfound <= 100, harshness == "easy") #And this is high rewardfound values
predictions_high_variable_easy  <- plogis(posterior_linpred(M4_3_round1, re_formula = NA,
                                                            newdata=newdata_high_variable_easy))
#Contrast quantiles
Quantils_high_variable <- as.data.frame(quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs=c(0.050, 0.950)))


#Generate overview
Quantils_low_stable <- tibble(Quantils_low_stable) %>%
  rename(value = `quantile(predictions_low_stable_easy - predictions_low_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Low Stable")

Quantils_low_variable <- tibble(Quantils_low_variable) %>%
  rename(value = `quantile(predictions_low_variable_easy - predictions_low_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "Low Variable")

Quantils_high_stable <- tibble(Quantils_high_stable) %>%
  rename(value = `quantile(predictions_high_stable_easy - predictions_high_stable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "High Stable")

Quantils_high_variable <- tibble(Quantils_high_variable) %>%
  rename(value = `quantile(predictions_high_variable_easy - predictions_high_variable_harsh, probs = c(0.05, 0.95))`) %>%
  mutate(quantile = c("5%","95%"),condition = "High Variable")


#Overview
Quantils_low_stable %>%
  add_row(Quantils_high_stable) %>%
  add_row(Quantils_low_variable) %>%
  add_row(Quantils_high_variable) %>%
  spread(key = quantile,value = value) %>%
  mutate(`5%` = round(`5%`,2),`95%` = round(`95%`,2)) %>%
  rename(` ` = condition) %>%
  kbl(booktabs = T) %>%
  kable_paper("striped", full_width = F)
