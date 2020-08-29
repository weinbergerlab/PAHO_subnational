model_string7 <- "

model {

for (i in 1:subnat) {

   for (t in 1:time) {

      y[i,t] ~ dpois(lambda[i,t])
      log(lambda[i,t]) <- x[i,t,]%*%beta[i,] + 
                          vax[i,t]%*%theta[i] +
                          disp[i,t]

      log.rr.vax[i,t] <- vax[i,t]%*%theta[i]
   }
   
   for (j in 1:p_x) {
      beta[i,j] ~ dnorm(mu_beta[j], inv_var_beta[j])
   }
      
   theta[i] <-z[i,]%*%gamma  + eta[i]
   eta[i] ~ dnorm(0, inv_var_eta)
}

for (i in 1:subnat) {
    disp[i,1] ~ dnorm(0,tau.disp)
    for(t in 2:time) {
    disp[i,t] ~ dnorm(disp[i,t-1]*rho, tau.disp)
  }
}

rho ~ dunif(0,1)
tau.disp ~ dgamma(0.01,0.01)

# https://stackoverflow.com/questions/47135726/error-slicer-stuck-at-value-with-infinite-density-running-binomial-beta-model
# for(i in 1:subnat){
#    rho[i] ~ dbeta((a+0.01),(b+0.01))T(0.001,0.999)
#    tau.disp[i] ~ dgamma(c, d)
# }
# 
# a ~ dgamma(0.01,0.01)
# b ~ dgamma(0.01,0.01)
# c ~ dgamma(0.01,0.01)
# d ~ dgamma(0.01,0.01)

for (j in 1:p_x) {
   mu_beta[j] ~ dnorm(0, 0.0001)
   inv_var_beta[j] ~ dgamma(0.01,0.01)
}

for(j in 1:p_z){
   gamma[j] ~ dnorm(0, 0.0001)
}
  
ave.effect.pov[1] <- gamma[1] 
ave.effect.pov[2] <- gamma[1] + gamma[2]
ave.effect.pov[3] <- gamma[1] + gamma[3]

inv_var_eta ~ dgamma(0.01,0.01)
mu.eta ~ dnorm(0, 1e-4)
  
}
"