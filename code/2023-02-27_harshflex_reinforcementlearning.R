#title: "Harsh Flex Project"
#author: "Dominik Deffner"
#Last updated date: "28/02/2023"

#This script runs Bayesian multilevel reinforcement learning model and plots results

library(rstan)
library(scales)   
library(RColorBrewer)
library(rethinking)

#Clear Environment
rm(list =ls())

#Create a palette for plotting
conditioncolors <- c("#E89F68","#76C5AD")

#Set working directory
setwd("~/GitHub/HarshFlex_BanditJars")

#Load data
d_flex <- read.csv("data/round2_datafile.csv")

#Recode choices
for (i in 1:nrow(d_flex)) {
  if (d_flex$armpick[i] == "one") d_flex$armpick[i]    <- 1
  if (d_flex$armpick[i] == "two") d_flex$armpick[i]    <- 2
  if (d_flex$armpick[i] == "three") d_flex$armpick[i]  <- 3
  if (d_flex$armpick[i] == "four") d_flex$armpick[i]   <- 4
  
  if (d_flex$looplabel[i] == "loop1") d_flex$looplabel[i]    <- 1
  if (d_flex$looplabel[i] == "loop2") d_flex$looplabel[i]    <- 2
  if (d_flex$looplabel[i] == "loop3") d_flex$looplabel[i]    <- 3
  if (d_flex$looplabel[i] == "loop4") d_flex$looplabel[i]    <- 4
  if (d_flex$looplabel[i] == "loop5") d_flex$looplabel[i]    <- 5
  
}

d_flex$armpick <- as.integer(d_flex$armpick)
d_flex$looplabel <- as.integer(d_flex$looplabel)

#Create numeric ids
d_flex$id <- sapply(1:nrow(d_flex), function(x) which(unique(d_flex$subject_id) == d_flex$subject_id[x] ))

#Store relevant variables in data list as required by stan
dat <- list(N = nrow(d_flex), 
            N_id = length(unique(d_flex$subject_id)),
            id = d_flex$id,
            choice = d_flex$armpick,
            Reward = d_flex$rewardfound/max(d_flex$rewardfound), 
            schedule = d_flex$looplabel,
            Stability = ifelse(d_flex$universe == "stable", 1, 2),
            Harshness = ifelse(d_flex$harshness == "easy", 1, 2)
)


#Stan model code

reinforcement_model <- " 

//Data block: Define and name the size of each observed variable

data{    

   int N;                 //Number of observations, i.e. trials
   int N_id;             //Number of individuals
   int id[N];            //Unique individual identification 
   int choice[N];        //Chosen arm
   real Reward[N];       //Obtained reward
   int Stability[N];     //Stability (1 = stable)
   int Harshness[N];     //Harshness (1 = easy)
   
} 

//Parameter block: Define and name the size of each unobserved variable. 

parameters{ 
  
  //Learning parameters phi and lambda; [Stability (1 or 2), Harshness (1 or 2)]
   real logit_phi[2,2];   
   real log_L[2,2];   
   
  matrix[8, N_id] z_ID;           //Matrix for our latent individual samples (z scores)
  vector<lower = 0>[8] sigma_ID;  //Standard deviation of learning parameters among individuals 
  cholesky_factor_corr[8] Rho_ID; //Cholesky factor for covariance of learning parameters among individuals
  
} 
   
transformed parameters{
  matrix[N_id, 8] v_ID;    
  v_ID = (diag_pre_multiply(sigma_ID, Rho_ID) * z_ID)';   
}

model{

  matrix[N_id, 4] A; //Attraction matrix
  
  //Assign priors
  for (i in 1:2){
    for (j in 1:2){
        logit_phi[i,j] ~  normal(0, 2); 
        log_L[i,j] ~  normal(0, 2); 
     }
   }

  //Define prior distribution of varying individual effects
  to_vector(z_ID) ~ normal(0, 1);  
  sigma_ID ~ exponential(1);       
  Rho_ID ~ lkj_corr_cholesky(4);   

  
  //Initialize attraction scores
  for (i in 1:N_id) A[i, 1:4] = rep_vector(0, 4)'; 
  
  //Loop over Choices
  for (i in 1:N) {
  
  //Define and name local variables that update across choices
  vector[4] p;    
  vector[4] pay;     
  real L;           
  real phi;  
  int counter; 
  
  //Create counter varible to account for experimental condition
  if (Stability[i]==1 && Harshness[i]==1) counter = 1;
  if (Stability[i]==1 && Harshness[i]==2) counter = 2;
  if (Stability[i]==2 && Harshness[i]==1) counter = 3;
  if (Stability[i]==2 && Harshness[i]==2) counter = 4;
  
  //First, what is the log-probability of observed choice
  L = exp(log_L[Stability[i], Harshness[i]] + v_ID[id[i], counter]);
  p = softmax(L * A[id[i], 1:4]' );                                                                        

  target += categorical_lpmf(choice[i] | p);

  //Second, update attractions conditional on observed choice
  pay[1:4] = rep_vector(0, 4);   
  pay[ choice[i] ] = Reward[i];
  phi = inv_logit(logit_phi[Stability[i], Harshness[i]] + v_ID[id[i], 4 + counter]);

  // Update attraction for all choices
  A[ id[i] , 1:4 ] = ( (1-phi)*to_vector(A[ id[i] , 1:4 ]) + phi*pay)';
  }
}

generated quantities{ 
  
  //Here we just compute updating and exploration rate on proper scale
  real updating_rate[2,2]; 
  real exploration_rate[2,2];
  vector[N] log_lik;
  matrix[N_id, 4] A; //Attraction matrix
  vector[4] p;    
  vector[4] pay;     
  real L;           
  real phi;  
  int counter; 
  
  
    for (i in 1:2){
     for (j in 1:2){
          updating_rate[i,j] =  inv_logit(logit_phi[i,j]); 
          exploration_rate[i,j] =  exp(log_L[i,j]); 
     }
   }

 
  //This repeats the whole model block to compute lpps for model comparisons

    //Initialize attraction scores
  for (i in 1:N_id) A[i, 1:4] = rep_vector(0, 4)'; 
  
  //Loop over Choices
  for (i in 1:N) {
  
  //Create counter varible to account for experimental condition
  if (Stability[i]==1 && Harshness[i]==1) counter = 1;
  if (Stability[i]==1 && Harshness[i]==2) counter = 2;
  if (Stability[i]==2 && Harshness[i]==1) counter = 3;
  if (Stability[i]==2 && Harshness[i]==2) counter = 4;
  
  //First, what is the log-probability of observed choice
  L = exp(log_L[Stability[i], Harshness[i]] + v_ID[id[i], counter]);
  p = softmax(L * A[id[i], 1:4]' );
  
  log_lik[i] = categorical_lpmf(choice[i] | p);

  //Second, update attractions conditional on observed choice
  pay[1:4] = rep_vector(0, 4);   
  pay[ choice[i] ] = Reward[i];
  phi = inv_logit(logit_phi[Stability[i], Harshness[i]] + v_ID[id[i], 4 + counter]);

  // Update attraction for all choiceisors
  A[ id[i] , 1:4 ] = ( (1-phi)*to_vector(A[ id[i] , 1:4 ]) + phi*pay)';
  }
  
}

" 



#Run reinforcement learning model
m <- stan(model_code = reinforcement_model, data = dat, iter = 2000, cores = 4, chains =4,
          refresh = 1, control = list(adapt_delta = 0.9, max_treedepth = 14))



#Extract samples from posterior
s <- extract.samples(m)

#store samples in arrays
phi    <- array(NA, dim = c(2000, 2, 2))
lambda <- array(NA, dim = c(2000, 2, 2))

for (i in 1:2) {
  for (j in 1:2) {
    phi[,i,j] <- s$updating_rate[,i,j]
    lambda[,i,j] <- s$exploration_rate[,i,j]
  }
}


###
##
# PLOTTING CODE
##
###


#Uncomment to save plot
#graphics.off()
#pdf("FlexPlot.pdf", height = 5, width = 7)


par(mfrow = c(2,2), 
    mar= c(2.5,2.5,0,1), 
    oma =c(0,0,3,0))


###
##
# LEARNING RATE
##
###


dens <- density(phi[,1,2])
x1 <- min(which(dens$x >= quantile(phi[,1,2], 0)))  
x2 <- max(which(dens$x <  quantile(phi[,1,2], 1)))
plot(dens, xlim = c(0,1), ylim = c(0,10), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(conditioncolors[1],alpha = 0.6), border = NA))
legend("topright", c("Not Harsh", "Harsh"), col = c(alpha(conditioncolors[1],alpha = 0.6),alpha(conditioncolors[2],alpha = 0.6)), lwd = 6, bty="n", cex = 1.2)

dens <- density(phi[,1,1])
x1 <- min(which(dens$x >= quantile(phi[,1,1], 0)))  
x2 <- max(which(dens$x <  quantile(phi[,1,1], 1)))
par(new = TRUE)
plot(dens, xlim = c(0,1), ylim = c(0,10), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(conditioncolors[2],alpha = 0.6), border = NA))
legend("topleft","A",bty = "n",cex = 1.1)
mtext("Stable",  cex = 1.2, side = 3, line = 1.5)

mtext("Learning rate", side = 2, line = 1, cex = 1.2)


dens <- density(phi[,2,2])
x1 <- min(which(dens$x >= quantile(phi[,2,2], 0)))  
x2 <- max(which(dens$x <  quantile(phi[,2,2], 1)))
plot(dens, xlim = c(0,1), ylim = c(0,10), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(conditioncolors[1],alpha = 0.6), border = NA))

dens <- density(phi[,2,1])
x1 <- min(which(dens$x >= quantile(phi[,2,1], 0)))  
x2 <- max(which(dens$x <  quantile(phi[,2,1], 1)))
par(new = TRUE)
plot(dens, xlim = c(0,1), ylim = c(0,10), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(conditioncolors[2],alpha = 0.6), border = NA))
legend("topleft","B",bty = "n",cex = 1.1)
mtext("Variable",  cex = 1.2, side = 3, line = 1.5)



###
##
# Elective flexibility
##
###


dens <- density(lambda[,1,2])
x1 <- min(which(dens$x >= quantile(lambda[,1,2], 0)))  
x2 <- max(which(dens$x <  quantile(lambda[,1,2], 1)))
plot(dens, xlim = c(3,6), ylim = c(0,4), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(conditioncolors[1],alpha = 0.6), border = NA))

dens <- density(lambda[,1,1])
x1 <- min(which(dens$x >= quantile(lambda[,1,1], 0)))  
x2 <- max(which(dens$x <  quantile(lambda[,1,1], 1)))
par(new = TRUE)
plot(dens, xlim = c(3,6), ylim = c(0,4), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(conditioncolors[2],alpha = 0.6), border = NA))
legend("topleft","C",bty = "n",cex = 1.1)

mtext("Elective flexibility", side = 2, line = 1, cex = 1.2)


dens <- density(lambda[,2,2])
x1 <- min(which(dens$x >= quantile(lambda[,2,2], 0)))  
x2 <- max(which(dens$x <  quantile(lambda[,2,2], 1)))
plot(dens, xlim = c(3,6), ylim = c(0,4), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(conditioncolors[1],alpha = 0.6), border = NA))

dens <- density(lambda[,2,1])
x1 <- min(which(dens$x >= quantile(lambda[,2,1], 0)))  
x2 <- max(which(dens$x <  quantile(lambda[,2,1], 1)))
par(new = TRUE)
plot(dens, xlim = c(3,6), ylim = c(0,4), type="n", ann = FALSE, bty = "n", yaxt = "n")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(conditioncolors[2],alpha = 0.6), border = NA))
legend("topleft","D",bty = "n",cex = 1.1)

#Uncomment to save plot
#dev.off()

