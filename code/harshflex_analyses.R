#title: "Harsh Flex Project"
#author: "Sarah Pope-Caldwell"
#Last updated date: "19/03/2024" - update 90% HPDI to 95%

#Wipe Environment
rm(list =ls())

library(tidyverse)
library(brms)
library(matrixStats)
library(BayesianFirstAid)
library(repmod)
library(sjPlot)

#Pallete
conditioncolors <- c("#BCDED0","#F1C9AD","#5EA881","#DD8546")

#setwd("~/HarshFlex_BanditJars") #Set the working directory


# Load all data files
data_files <- list.files("processed_data/", pattern=".csv",recursive = T)

#### Switching Model Setup ####
# Models to be run
subset_list <- c("all","harshcheckpass") # Run models for all participants, and for only participants who reported that the Harsh condition was more stressful than Not Harsh
models_list <- c("base", "agesex") #Run models with and without age/sex as predictors

# Recode the data
recode_for_model <- function(d) {
  d_model <- d %>%
    mutate(switch = as.numeric(switch),
           subject_id = as.factor (subject_id),
           sex=if_else(sex == "Male", -0.5, 0.5),
           incentive = as.factor(ifelse(incentive == "No added incentive","nomoney","money")),
           age = c(scale(age)),
           rewardfound = as.numeric(rewardfound),
           universe = as.factor (universe),
           harshness = as.factor (harshness),
           age = as.numeric (scale(age)),
           looplabel = as.factor(looplabel)
    ) %>%
    filter(!is.na(age),!is.na(sex))
  return(d_model)
}
# Model formulas for individual data collection rounds
st1_formulas = c(
  "switch ~ 1 + (1|subject_id)", 
  "switch ~ 1 + rewardfound + age + sex + looplabel + (1 + rewardfound | subject_id)",
  "switch ~ 1 + harshness + universe + age + sex + looplabel + rewardfound*harshness +  rewardfound*universe + (1 + harshness + universe + rewardfound*harshness + rewardfound*universe | subject_id)",
  "switch ~ 1 + harshness*universe + age + sex + looplabel + rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness + (1 + harshness*universe + rewardfound*harshness + rewardfound*universe + rewardfound:universe:harshness | subject_id)"
)

# Priors for Model formulas for individual data collection rounds
st1_bern_base_prior <- 
  prior(normal(0,1.5), class="Intercept") # (Intercept outside of quotations in first version?)
st1_bern_base_prior_1 <- bern_base_prior + 
  prior(normal(0, 1.5), class="b") +
  prior(normal(0, 0.1), class="b", coef="rewardfound") +
  prior(exponential(10), class="sd")
st1_bern_base_prior_2 <- bern_base_prior_1 + 
  prior(normal(0, 0.05), class="b", coef="harshnessharsh:rewardfound") +
  prior(normal(0, 0.05), class="b", coef="universeuv:rewardfound")
st1_bern_base_prior_3 <- bern_base_prior_2 + 
  prior(normal(0, 0.375), class="b", coef="harshnessharsh:universeuv") +
  prior(normal(0, 0.05), class="b", coef="harshnessharsh:universeuv:rewardfound")
#List of priors
st1_bern_priors <- list(st1_bern_base_prior, st1_bern_base_prior_1, st1_bern_base_prior_2, st1_bern_base_prior_3)

# Model formulas for Study 1 vs Study 2
st2_formulas = c(
  "switch ~ 1 + (1|subject_id)", 
  "switch ~ 1 + rewardfound + age + sex + looplabel + (1 + rewardfound | subject_id)",
  "switch ~ 1 + age + sex + looplabel + rewardfound*harshness +  rewardfound*universe + rewardfound*incentive + (1 + harshness + universe + +rewardfound*harshness + rewardfound*universe | subject_id)",
  "switch ~ 1 + age + sex + looplabel + rewardfound*universe*harshness + rewardfound*universe*incentive + rewardfound*harshness*incentive + harshness*universe*incentive + (1 + rewardfound*universe*harshness | subject_id)",
  "switch ~ 1 + age + sex + looplabel + rewardfound*universe*harshness*incentive + (1 + rewardfound*universe*harshness | subject_id)"
)

# Priors for Model formulas for Study 1 vs Study 2
st2_bern_base_prior <- 
  prior(normal(0,1.5), class="Intercept") # (Intercept outside of quotations in first version?)
st2_bern_base_prior_1 <- bern_base_prior + 
  prior(normal(0, 1.5), class="b") +
  prior(normal(0, 0.1), class="b", coef="rewardfound") +
  prior(exponential(10), class="sd")
st2_bern_base_prior_2 <- bern_base_prior_1 + 
  prior(normal(0, 0.05), class="b", coef="rewardfound:harshnessharsh") + 
  prior(normal(0, 0.05), class="b", coef="rewardfound:universeuv") + 
  prior(normal(0, 0.05), class="b", coef="rewardfound:incentivenomoney") 
st2_bern_base_prior_3 <- bern_base_prior_2 + 
  prior(normal(0, 0.375), class="b", coef="universeuv:harshnessharsh") + 
  prior(normal(0, 0.375), class="b", coef="harshnessharsh:incentivenomoney") + 
  prior(normal(0, 0.375), class="b", coef="universeuv:incentivenomoney") + 
  prior(normal(0, 0.05), class="b", coef="rewardfound:universeuv:harshnessharsh") +
  prior(normal(0, 0.05), class="b", coef="rewardfound:universeuv:incentivenomoney") +
  prior(normal(0, 0.05), class="b", coef="rewardfound:harshnessharsh:incentivenomoney") + 
  prior(normal(0, 0.05), class="b", coef="universeuv:harshnessharsh:incentivenomoney")
st2_bern_base_prior_4 <- bern_base_prior_3 + 
  prior(normal(0, 0.025), class="b", coef="rewardfound:universeuv:harshnessharsh:incentivenomoney") 
# Priors List
st2_bern_priors <- list(st2_bern_base_prior, st2_bern_base_prior_1, st2_bern_base_prior_2, st2_bern_base_prior_3, st2_bern_base_prior_4)


#### Fit Switching models ####
dir.create("fitted_models", showWarnings = FALSE)
for(filename in data_files){
  data_filename <- file.path("processed_data", filename)
  d <- read_csv(data_filename)
  
  for(subset in subset_list){
    #One set of models with all ppts, one with only harsh check pass ppts
    if(subset=="harshcheckpass") {
      d<-d %>% filter(harshcheck_report==2)
    }
    d_model <- recode_for_model(d) 
    for(modelstype in models_list){
      modelname <- paste(subset,"_",modelstype,sep="")
      for(i in 1:length(formulas)) {
        if (grepl("_clean", filename) == T) {
          formstr <- st1_formulas[i]
          prior = st1_bern_priors[[i]]
        } else {
          formstr <- st2_formulas[i]
          prior = st2_bern_priors[[i]]
        }
        if(modelstype == "base") {
          # Remove age sex and looplabel from formula for base model analyses
          formstr <- str_replace_all(formstr, " \\+ age \\+ sex \\+ looplabel", "")
        } else {
          d_model <- d_model %>% filter(!is.na(age),!is.na(sex))
          if(i > 1) {
            # Add age and sex priors
            prior <- prior + prior(normal(0, 0.375), class="b", coef="age") + prior(normal(0, .75), class="b", coef="sex")
          }
        }
        
        m <- brm(as.formula(formstr), prior=prior, family = bernoulli(),
                 data = d_model, cores=4, warmup = 2000, iter = 4000, control=list(adapt_delta=0.9))
        model_filename = file.path("fitted_models", str_replace(filename, "clean.csv", paste(modelname,"_model_", i, ".rds", sep="")))
        saveRDS(m, file = model_filename)
      }
    }
  }
}

#### Switching Model comparison functions ####
##Visualize WAIC
visualize_WAIC <- function(w){
  w %>%
    data.frame() %>% 
    rownames_to_column(var = "model") %>% 
    #mutate(model = ifelse(model == "M1_0","Model1.0","Model1.1")) %>%
    ggplot() +
    geom_pointrange(aes(x = reorder(model, -waic), y = waic,
                        ymin = waic - se_waic,
                        ymax = waic + se_waic,
                        color = model),
                    shape = 16) +
    theme_classic() +
    coord_flip() +
    labs(x = NULL, y = "WAIC",title = NULL) +
    theme(text  = element_text(size = 12),
          panel.grid= element_blank(),
          axis.text.x = element_text(color = 'black',size=12),
          axis.text.y = element_text(color = 'black',size=12, face="bold"),
          legend.position = "none")
}
#Visualize Conditional Effects 
conditional_effects_func <- function(model,model_name,i){
  if (i > 1){
    #Number of conditional effects
    n_condeff <- length(conditional_effects(model))
    cond_eff <-(conditional_effects(model))
    #Plot conditional effects
    for(i in 1:n_condeff){
      plot(cond_eff, plot = F)[[i]] +
        theme_classic()
      ggsave(paste("figures/conditional_effects/",model_name,"_condeff_",i,".png",sep=""))
    }
  }
}

#### Switching Model Comparisons ####
# Model lists
round1_all_base <- list.files("fitted_models/", pattern="round1_all_base")
round1_all_agesex <- list.files("fitted_models/", pattern="round1_all_agesex")
round1_harshcheckpass_base <- list.files("fitted_models/", pattern="round1_harshcheckpass_base")
round1_harshcheckpass_agesex<- list.files("fitted_models/", pattern="round1_harshcheckpass_agesex")

round2_all_base <- list.files("fitted_models/", pattern="round2_all_base")
round2_all_agesex <- list.files("fitted_models/", pattern="round2_all_agesex")
round2_harshcheckpass_base <- list.files("fitted_models/", pattern="round2_harshcheckpass_base")
round2_harshcheckpass_agesex<- list.files("fitted_models/", pattern="round2_harshcheckpass_agesex")

round3_all_base <- list.files("fitted_models/", pattern="round3_all_base")
round3_all_agesex <- list.files("fitted_models/", pattern="round3_all_agesex")
round3_harshcheckpass_base <- list.files("fitted_models/", pattern="round3_harshcheckpass_base")
round3_harshcheckpass_agesex<- list.files("fitted_models/", pattern="round3_harshcheckpass_agesex")

round2v3_all_base <- list.files("fitted_models/", pattern="round2v3_all_base")
round2v3_all_agesex <- list.files("fitted_models/", pattern="round2v3_all_agesex")
round2v3_harshcheckpass_base <- list.files("fitted_models/", pattern="round2v3_harshcheckpass_base")
round2v3_harshcheckpass_agesex<- list.files("fitted_models/", pattern="round2v3_harshcheckpass_agesex")


all_models_list <- list(#Study 1
                        round1_all_base,
                        round1_all_agesex,
                        round1_harshcheckpass_base,
                        round1_harshcheckpass_agesex,
                        round2_all_base,
                        round2_all_agesex,
                        round2_harshcheckpass_base,
                        round2_harshcheckpass_agesex,
                        round3_all_base,
                        round3_all_agesex,
                        round3_harshcheckpass_base,
                        round3_harshcheckpass_agesex,
                        #Study 2
                        round2v3_all_base,
                        round2v3_all_agesex,
                        round2v3_harshcheckpass_base
                        )

#Compute winning models and save to document
winning_models = data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("modelset", "winningmodel"))))
for (modelset in all_models_list){
  modelsetname <- sub("_model.*", "", modelset[1])
  for(i in 1:length(modelset)){
    m <- read_rds(paste("fitted_models/",modelset[i],sep=""))
    m <- add_criterion(m, "waic")
    if(i == 1 ){m1<-m}
    else if(i == 2){m2<-m}
    else if(i == 3){m3<-m}
    else if(i == 4){m4<-m}
    else if(i == 5){m5<-m}
    #conditional_effects_func(m,modelset[i],i) # To visualize conditional effects for all models
  }
  if (grepl("2v3", modelsetname) == T) {w <- loo_compare(m1,m2,m3,m4,m5, criterion = "waic") }
  else {w <- loo_compare(m1,m2,m3,m4, criterion = "waic")}
  round(w,2) 
  w_modelweights <- cbind(waic_diff = w[, 1] * -2,
                          se        = w[, 2] *  2) %>% 
    round(digits = 2)
  visualize_WAIC(w)
  ggsave(paste("figures/waic/",modelsetname,".png",sep=""))
  winning_modelname <- rownames(w)[1]
  winning_models <- rbind(winning_models, data.frame(modelset = modelsetname, winningmodel = winning_modelname))
}
write_csv(winning_models,"results/winningmodelslist.csv")  

#### Switching Model Predictions ####
# For each of the winning models, run the predictions:
winning_models <- read_csv("results/winningmodelslist.csv")  
# Generate new data
newdata <- expand_grid(universe=c("uv","st"), #both stable and variable conditions
                       incentive=c("money","nomoney"),
                       rewardfound=seq(0, 100, .5), #reward values between 0-100
                       harshness=c("harsh", "easy"),
                       age = 0, #Average age
                       looplabel = "loop3",#c("loop1","loop2","loop3","loop4","loop5"), #All reward schedules
                       sex = 0) #Average sex

contrasts_file = data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL, c("contrast", "twofive_perc","ninetysevenfive_perc","modelset","winningmodel","sig"))))

for (model_num in 1:length(winning_models$modelset)){
  winningmodel <- read_rds(paste("fitted_models/",winning_models$modelset[model_num],"_model_",parse_number(winning_models$winningmodel[model_num]),".rds",sep=""))
  predictions <- plogis(posterior_linpred(winningmodel, re_formula = NA, newdata=newdata))
  newdata$prob <- colMeans(predictions)
  newdata$lower_prediction <- colQuantiles(predictions, probs=0.025)
  newdata$upper_prediction <- colQuantiles(predictions, probs=0.975)
  
# # If the winning model involves a 4-way interaction, plot it.
#   if (parse_number(winning_models$winningmodel[model_num]) >= 4){
#   #Plot four-way effect
#     newdata %>%
#       mutate(
#         universe = ifelse(universe == "st","Stable","Variable"),
#         incentiveharsh = case_when(
#         harshness == "harsh" & incentive == "money" ~ "Monetary Incentive, Harsh",
#         harshness == "easy" & incentive == "money" ~ "Monetary Incentive, Not Harsh",
#         harshness == "harsh" & incentive == "nomoney" ~ "No Added Incentive, Harsh",
#         harshness == "easy" & incentive == "nomoney" ~ "No Added Incentive, Not Harsh"),
#         incentiveharsh = fct_relevel(incentiveharsh,"No Added Incentive, Not Harsh","No Added Incentive, Harsh","Monetary Incentive, Not Harsh")) %>%
#     
#     ggplot() +
#     geom_ribbon(aes(x=rewardfound, ymin=lower_prediction, ymax=upper_prediction,
#                     fill=incentiveharsh), alpha=.75) +
#     geom_line(aes(x=rewardfound, y=prob, color=incentiveharsh))+
#     facet_grid(~universe) +
#     scale_color_manual(values =conditioncolors) +
#     scale_fill_manual(values = conditioncolors) +
#     theme_minimal() +
#       theme(text  = element_text(size = 16),
#             panel.grid= element_blank(),
#             axis.text.x = element_text(color = 'black',size=12),
#             axis.text.y = element_text(color = 'black',size=12),
#             legend.title = element_blank(),
#             legend.position = "Null") +
#       labs(x = NULL, y = "Likelihood of switching",title = NULL, size = 20)
#   ggsave(paste("figures/fourway_interact/",winning_models$modelset[model_num],"_fourway.png",sep=""))
#   }
  #Comparisons
  #RewardRange list
  if (grepl("round2_all_base", winning_models$modelset[model_num]) == T) {reward_range_list <- c("low","high","lowtight")}
  else if (grepl("round3_all_base", winning_models$modelset[model_num]) == T) {reward_range_list <- c("low","high","lowtight")}
  else {reward_range_list <- c("low","high")}
  #Incentive List
  if (grepl("2v3", winning_models$modelset[model_num]) == T) {incentive_list <- c("money","nomoney")} 
  else if (grepl("round3", winning_models$modelset[model_num]) == T) {incentive_list <- "money"} 
  else {incentive_list <- "nomoney"}
  #Variability List
  variability_list <- c("st","uv")
  #Harshness List
  harshness_list <- c("harsh","easy")
  
  for (variability in variability_list){
    for (reward_range in reward_range_list){
      if (reward_range == "low"){
        rewardfoundmin = 0
        rewardfoundmax = 45
      } else if (reward_range == "lowtight"){
        rewardfoundmin = 0
        rewardfoundmax = 25
      } else if (reward_range == "high"){
        rewardfoundmin = 55
        rewardfoundmax = 100
      } else if (reward_range == "hightight"){
        rewardfoundmin = 75
        rewardfoundmax = 100
      }
      for (incentivetype in incentive_list){
            contrastname <- paste(variability,"_",incentivetype,"_harshvnotharsh_",reward_range,sep="")
            
            d1 <- newdata %>% filter(universe == variability, incentive == incentivetype, rewardfound >= rewardfoundmin, rewardfound <= rewardfoundmax, harshness == harshness_list[1]) 
            d1_predictions <- plogis(posterior_linpred(winningmodel, re_formula = NA, newdata=d1))
            
            d2 <- newdata %>% filter(universe == variability, incentive == incentivetype, rewardfound >= rewardfoundmin, rewardfound <= rewardfoundmax, harshness == harshness_list[2]) 
            d2_predictions <- plogis(posterior_linpred(winningmodel, re_formula = NA, newdata=d2))
          
           #Contrast quantiles
           Quantils <- quantile(d2_predictions - d1_predictions, probs=c(0.025, 0.975))
           Conf_over_0 <- ifelse(between(0,Quantils[1],Quantils[2])==T,"nonsignificant","SIGNIFICANT")
           contrasts_file <- rbind(contrasts_file, data.frame(contrast = contrastname, twofive_perc = Quantils[[1]],ninetysevenfive_perc = Quantils[[2]],modelset = winning_models$modelset[model_num],winningmodel = winning_models$winningmodel[model_num],sig=Conf_over_0))
         }
      if (grepl("2v3", winning_models$modelset[model_num]) == T) {
        for (harshnesstype in harshness_list){
          contrastname <- paste(variability,"_",harshnesstype,"_moneyvnomoney_",reward_range,sep="")
          
          d1 <- newdata %>% filter(universe == variability, harshness == harshnesstype, rewardfound >= rewardfoundmin, rewardfound <= rewardfoundmax, incentive == incentive_list[1]) 
          d1_predictions <- plogis(posterior_linpred(winningmodel, re_formula = NA, newdata=d1))
          
          d2 <- newdata %>% filter(universe == variability, harshness == harshnesstype, rewardfound >= rewardfoundmin, rewardfound <= rewardfoundmax, incentive == incentive_list[2]) 
          d2_predictions <- plogis(posterior_linpred(winningmodel, re_formula = NA, newdata=d2))
          
          #Contrast quantiles
          Quantils <- quantile(d2_predictions - d1_predictions, probs=c(0.025, 0.975))
          Conf_over_0 <- ifelse(between(0,Quantils[1],Quantils[2])==T,"nonsignificant","SIGNIFICANT")
          contrasts_file <- rbind(contrasts_file, data.frame(contrast = contrastname, twofive_perc = Quantils[[1]],ninetysevenfive_perc = Quantils[[2]],modelset = winning_models$modelset[model_num],winningmodel = winning_models$winningmodel[model_num],sig=Conf_over_0))
        }
      }
    }  
  }
  write_csv(contrasts_file,"results/allcontrasts-95HPDI.csv")
}

#### Performance above chance ####
performance_above_chance <- function(d,d_name) {
  #Calculate the proportion of optimal choices for all participants in each condition for all trials
  bandit_df_perf <- d %>%
    mutate(bestchoice = ifelse(rewardfound < one, 0,
                               ifelse(rewardfound < two, 0,
                                      ifelse(rewardfound < three, 0,
                                             ifelse(rewardfound < four,0,1))))) %>%
    group_by(universe,harshness,subject_id) %>%
    summarise(mean_bestchoice = mean(bestchoice), sd_bestchoice = sd(bestchoice)) %>%
    mutate(chance = 25)
  
  #Bayes t-test for differences between ppt's actual performance and chance
  bandit_df_perf_stnh <- bandit_df_perf %>%
    filter(universe == "st" & harshness == "easy")
  st_easy <- bayes.t.test(bandit_df_perf_stnh$mean_bestchoice, mu=.25, cred.mass=.95)
  
  bandit_df_perf_sth <- bandit_df_perf %>%
    filter(universe == "st" & harshness == "harsh")
  st_harsh <- bayes.t.test(bandit_df_perf_sth$mean_bestchoice, mu=.25, cred.mass=.95)
  
  bandit_df_perf_uvnh <- bandit_df_perf %>%
    filter(universe == "uv" & harshness == "easy")
  uv_easy <- bayes.t.test(bandit_df_perf_uvnh$mean_bestchoice, mu=.25, cred.mass=.95)
  
  bandit_df_perf_uvh <- bandit_df_perf %>%
    filter(universe == "uv" & harshness == "harsh")
  uv_harsh <- bayes.t.test(bandit_df_perf_uvh$mean_bestchoice, mu=.25, cred.mass=.95)
  
  bandit_df_perf %>%
    mutate(universe = ifelse(universe == "st", "Stable", "Variable"),
           harshness = ifelse(harshness == "easy", "Not Harsh", "Harsh")) %>%
    ggplot(aes(y = mean_bestchoice, x = harshness,color = harshness)) +
    geom_boxplot() +
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
          axis.text.x = element_blank(),
          legend.title = element_blank()) +
    xlab('Trial') + ylab('Proportion Optimal Choices')
  ggsave(paste("figures/perf_abov_chance/",d_name,"/perfabovechance.png",sep=""))
  
  ttresultslist <- list(st_easy,st_harsh,uv_easy,uv_harsh)
  ttresults = c()
  for (i in 1:4){
    result <- ttresultslist[[i]]
    ttresults[i] <- paste(result$data_name,paste("Mean = ",result$stats[1,1],"HDIlo =", result$stats[1,5],"HDIhi =",result$stats[1,6],sep =", "))
  }
  return(ttresults)
}

for(filename in data_files){
  data_filename <- file.path("processed_data", filename)
  d <- read_csv(data_filename)
 ttresults<-performance_above_chance(d,data_filename)
 ttresults_filename <- paste("results/ttresults_95_",filename,sep="")
 write.csv(ttresults,ttresults_filename)
  }

#### Score / time Analyses ####
model_score_time <- function(d,d_name) {
  ## Relationship between average cumulative score and trial
  d <- d %>%
    mutate(avg_cumscore = totalrewardfound/trial,
           trial = as.numeric(trial))
     M_score_time <- brm(data = d, family = gaussian,
                      avg_cumscore ~ 1 + harshness:universe:trial + (1 + harshness:universe:trial | subject_id),
                      prior =
                        prior(normal(0, 10), class = "Intercept") +
                        prior(normal(0, 5), class = "b", coef = "trial") +
                        prior(normal(0, 5), class = "b", coef = "harshnessharsh") +
                        prior(normal(0, 10), class = "b", coef = "universeuv") +
                        prior(normal(0, 5), class = "b", coef = "trial:universeuv") +
                        prior(normal(0, 5), class = "b", coef = "trial:harshnessharsh") +
                        prior(normal(0, 5), class = "b", coef = "universeuv:harshnessharsh")+
                        prior(normal(0, 5), class = "b", coef = "trial:universeuv:harshnessharsh"),
                      iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 111)
  # #Add WAIC
  M_score_time <- add_criterion(M_score_time, "waic")
  model_filename <- file.path(str_replace(data_filename, "clean.csv","M_score_time.rds"))
  write_rds(M_score_time, model_filename)
}

contrasts_score_time <- function(d,d_name,model) {
# Generate new data
newdata <- expand_grid(universe=c("uv","st"), #both stable and variable conditions
                       trial=seq(1, 75, 1), #trials between 1-75
                       harshness=c("harsh", "easy"))

predictions <- posterior_linpred(model, re_formula = NA, newdata=newdata)
newdata$prob <- colMeans(predictions)
newdata$lower_prediction <- colQuantiles(predictions, probs=0.025)
newdata$upper_prediction <- colQuantiles(predictions, probs=0.975)

#Comparisons
variability_list <- c("st","uv")
harshness_list <- c("harsh","easy")

  for (variability in variability_list){
    for (harshnesstype in harshness_list){
      contrastname <- paste(variability,"_",harshnesstype,"_",d_name,sep="")
      
      d1 <- newdata %>% filter(universe == variability, harshness == harshnesstype, trial <= 10) 
      d1_predictions <- posterior_linpred(model, re_formula = NA, newdata=d1)
      
      d2 <- newdata %>% filter(universe == variability, harshness == harshnesstype, trial >= 66) 
      d2_predictions <- posterior_linpred(model, re_formula = NA, newdata=d2)
      
      #Contrast quantiles
      Quantils <- quantile(d2_predictions - d1_predictions, probs=c(0.025, 0.975))
      Conf_over_0 <- ifelse(between(0,Quantils[1],Quantils[2])==T,"nonsignificant","SIGNIFICANT")
      contrasts_file_scoretime <- rbind(contrasts_file_scoretime, data.frame(contrast = contrastname, twofive_perc = Quantils[[1]],ninetysevenfive_perc = Quantils[[2]],modelset = d_name,sig=Conf_over_0))
    }  
  }
 return(contrasts_file_scoretime)
}
contrasts_file_scoretime = data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("contrast", "twofive_perc","ninetysevenfive_perc","modelset","sig"))))

# Load all data files
data_files <- list.files("processed_data/", pattern=".csv",recursive = T)

for(filename in data_files[1:3]){ #Excludes the Study 1 vs 2 datafile
 # data_filename <- file.path("processed_data", filename)
 # d <- read_csv(data_filename)
 # model_score_time(d,data_filename) # Only run if you want to generate models from scratch
  data_filename <- file.path("fitted_models", filename)
  m_score_time <- read_rds(paste(str_replace(data_filename, "clean.csv","M_score_time.rds"),sep=""))
  contrasts_file_scoretime <- contrasts_score_time(d,data_filename,m_score_time)
}
write_csv(contrasts_file_scoretime,"results/allcontrasts_scoretime_95HDPI.csv")


#### Harsh Check Analyses ####
harshcheckpasstest <- function(d,data_filename) {
  #Ordinal regression to predict harshness ratings between harsh and easy conditions
  harshcheckpass<- d %>%
    group_by(subject_id) %>%
    slice(1) %>% ungroup() %>%
    gather("harshcheck_question","harshcheck_answer",easy,harsh)
  
 harshcheckpass %>% group_by(harshcheck_question,harshcheck_answer) %>%
   summarise(n())
  
  M_harshcheck <- brm(
    formula = harshcheck_answer ~ 1 + (1|subject_id) + harshcheck_question,
    data = harshcheckpass, 
    family = cumulative("probit"),
    chains = 4,
    iter = 4000,
    prior(normal(0, 5), 
          class = Intercept),
    init = "0",
    save_pars = save_pars(all = TRUE))
    
    c_eff <- conditional_effects(harshchecktest, categorical = TRUE)
    #creating plot
    plot(c_eff, plot = FALSE)[[1]] + 
      theme_minimal() +
      theme(text  = element_text(size = 12),
            panel.grid   = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            strip.text = element_text(color = 'black',size=12,face = 'bold'),
            axis.title.y = element_blank(), 
            axis.text.x = element_text(color = 'black',size=12,angle = 0,hjust =.85),
            axis.text.y = element_text(color = 'black',size=12),
            legend.title = element_text("Reported Stress Level"),
            legend.background = element_rect(size=0.5, linetype="solid",
                                             colour ='black'))
    ggsave(paste("../Projects/2021-05-01_HarshFlex/05_analyses/figures/",str_replace(data_filename, "clean.csv","harshreport.png"),sep=""))
    #Write model    
    model_filename <- file.path(str_replace(data_filename, "clean.csv","M_harshresponse.rds"))
    write_rds(M_harshcheck, model_filename)
    M_harshcheck<- read_rds("fitted_models/round2v3_M_harshresponse.rds")
    #Output results
    results <- summary(M_harshcheck,prob=0.95)
    results[["fixed"]][5,]
}
for(filename in data_files[1:3]){
    data_filename <- file.path("processed_data", filename)
    d <- read_csv(data_filename)
    harshcheckresults<-harshcheckpasstest(d,data_filename)
    harshcheckresults_filename <- paste("results/harshcheckresults_",filename,sep="")
    write.csv(harshcheckresults,harshcheckresults_filename)
}


#### Generate SI Model summary tables ####
#Supplementary Table 1 - Study 1 BASE
Model1.0 <- read_rds("fitted_models/df_round2_all_base_model_1.rds") %>% add_criterion("waic")
Model1.1 <- read_rds("fitted_models/df_round2_all_base_model_2.rds") %>% add_criterion("waic")
Model1.2 <- read_rds("fitted_models/df_round2_all_base_model_3.rds") %>% add_criterion("waic")
Model1.3 <- read_rds("fitted_models/df_round2_all_base_model_4.rds") %>% add_criterion("waic")
#Summary table
tab_model(Model1.0,Model1.1,Model1.2,Model1.3, transform = NULL) #transform is Null, delete for Odds Ratios; #show.ci=0.90 if 90 CI wanted
#WAIC table and figure
w <- loo_compare(Model1.0,Model1.1,Model1.2,Model1.3, criterion = "waic")
round(w,2) 
w_modelweights <- cbind(waic_diff = w[, 1] * -2,
                        se        = w[, 2] *  2) %>% 
  round(digits = 2)
visualize_WAIC(w)

#Supplementary Table 2 - Study 1 Age, Sex, Reward Schedule included
Model2.0 <- read_rds("fitted_models/df_round2_all_agesex_model_1.rds") %>% add_criterion("waic")
Model2.1 <- read_rds("fitted_models/df_round2_all_agesex_model_2.rds") %>% add_criterion("waic")
Model2.2 <- read_rds("fitted_models/df_round2_all_agesex_model_3.rds") %>% add_criterion("waic")
Model2.3 <- read_rds("fitted_models/df_round2_all_agesex_model_4.rds") %>% add_criterion("waic")
#Summary table
tab_model(Model2.0,Model2.1,Model2.2,Model2.3, transform = NULL)
#WAIC table and figure
w <- loo_compare(Model2.0,Model2.1,Model2.2,Model2.3, criterion = "waic")
round(w,2) 
w_modelweights <- cbind(waic_diff = w[, 1] * -2,
                        se        = w[, 2] *  2) %>% 
  round(digits = 2)
visualize_WAIC(w)

#Supplementary Table 3 - Study 1 Only participants who thought Harsh condition was more stressful than Not Harsh
Model3.0 <- read_rds("fitted_models/df_round2_harshcheckpass_base_model_1.rds") %>% add_criterion("waic")
Model3.1 <- read_rds("fitted_models/df_round2_harshcheckpass_base_model_2.rds") %>% add_criterion("waic")
Model3.2 <- read_rds("fitted_models/df_round2_harshcheckpass_base_model_3.rds") %>% add_criterion("waic")
Model3.3 <- read_rds("fitted_models/df_round2_harshcheckpass_base_model_4.rds") %>% add_criterion("waic")
#Summary table
tab_model(Model3.0,Model3.1,Model3.2,Model3.3, transform = NULL) 
#WAIC table and figure
w <- loo_compare(Model3.0,Model3.1,Model3.2,Model3.3, criterion = "waic")
round(w,2) 
visualize_WAIC(w)

#Supplementary Table 4 - Dropped round of data collection, due to sampling bias (see Methods)
Model4.0 <- read_rds("fitted_models/df_round1_all_base_model_1.rds") %>% add_criterion("waic")
Model4.1 <- read_rds("fitted_models/df_round1_all_base_model_2.rds") %>% add_criterion("waic")
Model4.2 <- read_rds("fitted_models/df_round1_all_base_model_3.rds") %>% add_criterion("waic")
Model4.3 <- read_rds("fitted_models/df_round1_all_base_model_4.rds") %>% add_criterion("waic")
#Summary table
tab_model(Model4.0,Model4.1,Model4.2,Model4.3, transform = NULL)
#WAIC table and figure
w <- loo_compare(Model4.0,Model4.1,Model4.2,Model4.3, criterion = "waic")
round(w,2) 
visualize_WAIC(w)

#Supplementary Table 5 - Study 2, Harsh vs Not Harsh
Model5.0 <- read_rds("fitted_models/df_round3_all_base_model_1.rds") %>% add_criterion("waic")
Model5.1 <- read_rds("fitted_models/df_round3_all_base_model_2.rds") %>% add_criterion("waic")
Model5.2 <- read_rds("fitted_models/df_round3_all_base_model_3.rds") %>% add_criterion("waic")
Model5.3 <- read_rds("fitted_models/df_round3_all_base_model_4.rds") %>% add_criterion("waic")
#Summary table
tab_model(Model5.0,Model5.1,Model5.2,Model5.3, transform = NULL)
#WAIC table and figure
w <- loo_compare(Model5.0,Model5.1,Model5.2,Model5.3, criterion = "waic")
round(w,2) 
visualize_WAIC(w)


#Supplementary Table 6 - Study 1 vs Study 2, No monetary incentive vs monetary incentive
Model6.0 <- read_rds("fitted_models/df_round2v3_all_base_model_1.rds") %>% add_criterion("waic")
Model6.1 <- read_rds("fitted_models/df_round2v3_all_base_model_2.rds") %>% add_criterion("waic")
Model6.2 <- read_rds("fitted_models/df_round2v3_all_base_model_3.rds") %>% add_criterion("waic")
Model6.3 <- read_rds("fitted_models/df_round2v3_all_base_model_4.rds") %>% add_criterion("waic")
Model6.4 <- read_rds("fitted_models/df_round2v3_all_base_model_5.rds") %>% add_criterion("waic")
#Summary table
tab_model(Model6.0,Model6.1,Model6.2,Model6.3,Model6.4, transform = NULL)
#WAIC table and figure
w <- loo_compare(Model6.0,Model6.1,Model6.2,Model6.3,Model6.4, criterion = "waic")
round(w,2) 
visualize_WAIC(w)

#Supplementary Table 7 - Study 1 vs Study 2, Age, Sex, Reward Schedule included
Model7.0 <- read_rds("fitted_models/df_round2v3_all_agesex_model_1.rds") %>% add_criterion("waic")
Model7.1 <- read_rds("fitted_models/df_round2v3_all_agesex_model_2.rds") %>% add_criterion("waic")
Model7.2 <- read_rds("fitted_models/df_round2v3_all_agesex_model_3.rds") %>% add_criterion("waic")
Model7.3 <- read_rds("fitted_models/df_round2v3_all_agesex_model_4.rds") %>% add_criterion("waic")
Model7.4 <- read_rds("fitted_models/df_round2v3_all_agesex_model_5.rds") %>% add_criterion("waic")
#Summary table
tab_model(Model7.0,Model7.1,Model7.2,Model7.3,Model7.4, transform = NULL)
#WAIC table and figure
w <- loo_compare(Model7.0,Model7.1,Model7.2,Model7.3,Model7.4, criterion = "waic")
round(w,2) 
visualize_WAIC(w)

#Supplementary Table 8 - Study 1 vs Study 2, only participants who thought Harsh condition was more stressful than Not Harsh
Model8.0 <- read_rds("fitted_models/df_round2v3_harshcheckpass_base_model_1.rds") %>% add_criterion("waic")
Model8.1 <- read_rds("fitted_models/df_round2v3_harshcheckpass_base_model_2.rds") %>% add_criterion("waic")
Model8.2 <- read_rds("fitted_models/df_round2v3_harshcheckpass_base_model_3.rds") %>% add_criterion("waic")
Model8.3 <- read_rds("fitted_models/df_round2v3_harshcheckpass_base_model_4.rds") %>% add_criterion("waic")
Model8.4 <- read_rds("fitted_models/df_round2v3_harshcheckpass_base_model_5.rds") %>% add_criterion("waic")
#Summary table
tab_model(Model8.0,Model8.1,Model8.2,Model8.3,Model8.4, transform = NULL)
#WAIC table and figure
w <- loo_compare(Model8.0,Model8.1,Model8.2,Model8.3,Model8.4, criterion = "waic")
round(w,2) 
visualize_WAIC(w)


#Supplementary Table ____ - Frequentist stats
library(lme4)
library(emmeans)

oo <- options(repos = "https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)


#Get both Study 1 and 2 datafiles
data_filename<- file.path("processed_data", data_files[2])
d <- read_csv(data_filename) %>% mutate(incentive = "nomoney")
data_filename<- file.path("processed_data", data_files[3])
d2 <- read_csv(data_filename) %>% mutate(incentive = "money")

#Study 1 Frequentist Main Results
study1_elective <- d %>%
  filter(rewardfound > 55) %>%
  mutate(harshness = as.factor(harshness),
         universe = as.factor(universe),
         incentive = as.factor(incentive))
study1_responsive <- d %>%
  filter(rewardfound < 45) %>%
  mutate(harshness = as.factor(harshness),
         universe = as.factor(universe),
         incentive = as.factor(incentive))
#Elective frequentist model
freq_mod_elective <- glmer(switch ~ 1 + universe*harshness + (1|subject_id), family = "binomial", study1_elective)
#summary
summary(freq_mod_elective)
#contrasts
ems <- emmeans(freq_mod_elective, ~harshness, by = c("universe"))
summary(pairs(ems), point.est = mean)
#Responsive frequentist model
freq_mod_responsive <- glmer(switch ~ 1 + universe*harshness + (1|subject_id), family = "binomial", study1_responsive)
#summary
summary(freq_mod_responsive)
#contrasts
ems <- emmeans(freq_mod_responsive, ~harshness, by = c("universe"))
summary(pairs(ems), point.est = mean)


#Study 2 Frequentist Main Results

# Subset into elective
study2_elective <- d %>%
  full_join(d2) %>%
  filter(rewardfound > 55) %>%
  mutate(harshness = as.factor(harshness),
         universe = as.factor(universe),
         incentive = as.factor(incentive))
#Subset into responsive
study1_responsive <- d %>%
  full_join(d2) %>%
  filter(rewardfound < 45) %>%
  mutate(
         harshness = as.factor(harshness),
         universe = as.factor(universe),
         incentive = as.factor(incentive))

#Elective frequentist model
freq_mod_elective <- glmer(switch ~ 1 + universe*incentive*harshness + (1|subject_id), family = "binomial", study2_elective)
#summary
summary(freq_mod_elective)
#contrasts
ems <- emmeans(freq_mod_elective, ~incentive*harshness, by = c("universe"))
summary(pairs(ems), point.est = mean)

#Responsive frequentist model
freq_mod_responsive <- glmer(switch ~ 1 + universe*incentive*harshness + (1|subject_id), family = "binomial", study1_responsive)
#summary
summary(freq_mod_responsive)
#contrasts
ems <- emmeans(freq_mod_responsive, ~incentive*harshness, by = c("universe"))
summary(pairs(ems), point.est = mean)

