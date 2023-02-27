## *******************************************************************************
## --------------------------------PACKAGES------------------------------
## *******************************************************************************

library(ggplot2)
library(reshape2)
library(spatstat.utils)
library(dplyr)

## *******************************************************************************
## --------------------------------PARAMATERS------------------------------
## *******************************************************************************


# Total number of trials
t = 75
#Total number of arms
narms = 4
# Low spacing 
low = 500
# High spacing
high = 3000
#range
r = c(1,100)

paltopia1 <- rep(c("#cf4949","#e9b838", "#3b728d", "#b30d64",  "#721746"),20)

## *******************************************************************************
## --------------------------------FUNCTIONS------------------------------
## *******************************************************************************

#Plot with lines
plotit2lines = function(z){
  d <- melt(z, id.vars="trial")
  # Separate plots
  ggplot(d, aes(trial,value,color = variable)) +
    geom_path(size = 1.5) +
    theme_minimal() +
    ylim(c(0,100)) +
    scale_color_manual(values = paltopia1) +
    theme(text  = element_text(size = 12),
          panel.grid   = element_blank(),
          axis.title.x = element_blank(),
          strip.text = element_text(color = 'black',size=12,face = 'bold'),
          axis.title.y = element_text(color = 'black',size=12,face="bold"),
          axis.text.x = element_text(color = 'black',size=12,angle = 45,hjust =.85),
          axis.text.y = element_text(color = 'black',size=12),
          legend.text = element_text(color = 'black',size=8,face="bold"),
          legend.title = element_blank(),
          legend.background = element_rect(size=0.5, linetype="solid",
                                           colour ='black')) +
    ylab("Reward") +
    xlab("Selection")
}

# Generate arm list
generate_arm_list <- function(narms, start_prob, end_prob) {
  seq(1:narms)
}

# Generate STABLE rewards schedules
rsched_stable = function(x, t) {
  AR.st <- list(order=c(1,0,0), ar=.8, sd=.1)
  AR1.st <- arima.sim(n=t, model=AR.st)
  st = unclass(AR1.st)
  rewards = round(st*abs((25/(min(st)-max(st)))) + (x*25) - (25/2),0)
  return(rewards)
}

# Generate VARIABLE rewards schedules
rsched_unpred = function(x,t) {
  AR.uv <- list(order=c(1,0,0), ar=.8, sd=.1)
  AR1.uv <- arima.sim(n=t, model=AR.uv)
  uv = unclass(AR1.uv)
  rewards = round(uv*abs((80/(min(uv)-max(uv)))) + 25 + (x*10),0)
  return(rewards)
}

#Generate all arms - Stable
generate_st_walk <- function(t, arm_list) {
  spaced = F 
  variance = F 
  range = F 
  totalsum = F
  st_walk <- data.frame(trial = 1:t)
  while (spaced == F | variance ==F | range ==F | totalsum ==F) {
    for (i in 1:length(arm_list)) {
      st_walk[,paste0('arm', i)] = rsched_stable(arm_list[i], t)
    }
      cs1 = max(cumsum(st_walk$arm1[1:t]))
      cs2 = max(cumsum(st_walk$arm2[1:t]))
      cs3 = max(cumsum(st_walk$arm3[1:t]))
      cs4 = max(cumsum(st_walk$arm4[1:t]))
    spaced = 
      between(cs2-cs1,low,high) & between(cs3-cs2,low,high) & between(cs4-cs3,low,high)
      va1 = var(st_walk$arm1[1:t])
      va2 = var(st_walk$arm2[1:t])
      va3 = var(st_walk$arm3[1:t])
      va4 = var(st_walk$arm4[1:t])
    variance = 
      between(va1,20,40) & between(va2,20,40) & between(va3,20,40) & between(va4,20,40)
      ra1 = inside.range(st_walk$arm1[1:t],r)
      ra2 = inside.range(st_walk$arm2[1:t],r)
      ra3 = inside.range(st_walk$arm3[1:t],r)
      ra4 = inside.range(st_walk$arm4[1:t],r)
    range = !(F %in% ra1) & !(F %in% ra2) & !(F %in% ra3) & !(F %in% ra4)
    ts = cs1+cs2+cs3+cs4
    totalsum =  between(ts,14500,15000)
  }
    return(st_walk)
}

#Generate all arms - Variable
generate_uv_walk <- function(t, arm_list) {
  spaced = F
  variance = F
  range = F
  totalsum = F
  uv_walk <- data.frame(trial = 1:t)
  while (spaced == F | variance ==F | range == F | totalsum ==F) {
    for (i in 1:length(arm_list)) {
      uv_walk[,paste0('arm', i)] = rsched_unpred(arm_list[i], t)
    }
      cs1 = max(cumsum(uv_walk$arm1[1:t]))
      cs2 = max(cumsum(uv_walk$arm2[1:t]))
      cs3 = max(cumsum(uv_walk$arm3[1:t]))
      cs4 = max(cumsum(uv_walk$arm4[1:t]))
    spaced = 
      between(cs2-cs1,low,high) & between(cs3-cs2,low,high) & between(cs4-cs3,low,high)
      va1 = var(uv_walk$arm1[1:t])
      va2 = var(uv_walk$arm2[1:t])
      va3 = var(uv_walk$arm3[1:t])
      va4 = var(uv_walk$arm4[1:t])
    variance = 
    between(va1,200,400) & between(va2,200,400) & between(va3,200,400) & between(va4,200,400)
      ra1 = inside.range(uv_walk$arm1[1:t],r)
      ra2 = inside.range(uv_walk$arm2[1:t],r)
      ra3 = inside.range(uv_walk$arm3[1:t],r)
      ra4 = inside.range(uv_walk$arm4[1:t],r)
    range = !(F %in% ra1) & !(F %in% ra2) & !(F %in% ra3) & !(F %in% ra4)
    ts = cs1+cs2+cs3+cs4
    totalsum =  between(ts,14500,15000)
  }
  return(uv_walk)
}

## *******************************************************************************
## --------------------------------DATA FILES------------------------------
## *******************************************************************************
# Generate Arm List
arm_list <- generate_arm_list(narms = narms)

# Generate Data
st_walk <- generate_st_walk(t = t, arm_list = arm_list)
uv_walk <- generate_uv_walk(t = t, arm_list = arm_list)

#Plot Data
plotit2lines(st_walk[1:t, ])
plotit2lines(uv_walk[1:t, ])


# rename arms
uv = uv_walk %>%
  rename(uv_one = arm1, uv_two = arm2, uv_three= arm3, uv_four = arm4)
st = st_walk %>%
  rename(st_one = arm1, st_two = arm2, st_three = arm3, st_four = arm4)

#Join all walks
fullrewardschedule = uv %>%
  full_join(st)

