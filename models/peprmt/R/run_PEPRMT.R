source(here::here("models", "peprmt", "R", "PEPRMT_CH4_FINAL.R"))
source(here::here("models", "peprmt", "R", "PEPRMT_GPP_FINAL.R"))
source(here::here("models", "peprmt", "R", "PEPRMT_Reco_FINAL.R"))

run_PEPRMT <- function(target) {
  target <- data.frame(target)
  #First run GPP Module
  GPP_theta <- c(0.7479271, 1.0497113, 149.4681710, 94.4532674 )
  GPP_mod_target <- PEPRMT_GPP_final(theta = GPP_theta,
                                     data = target)
  
  #Create a new dataset that included model results
  target_results <- target %>%
    left_join(GPP_mod_target %>%
                rename(GPP_mod = GPP,
                       DOY = Time_2),
              by = c("DOY", "site"))
  
  #Second run Reco Module
  #Add modeled GPP into data before running Reco module (16th column)
  target[,16]<-target_results$GPP_mod
  
  Reco_theta <- c(18.41329, 1487.65701, 11.65972, 61.29611 )
  Reco_mod_target <- PEPRMT_Reco_FINAL(Reco_theta,
                                       data = target,
                                       wetland_type=2)
  
  #Create a new dataset that included model results
  target_results<-target_results %>%
    left_join(Reco_mod_target %>%
                rename(DOY = Time_2,
                       Reco_mod = Reco_full),
              by = c("DOY", "site"))
  
  #Last, run CH4 module
  #Add modeled S1, S2 into data before running CH4 module (17th & 18th columns)
  target$SOM_total <- target_results$S1
  target$SOM_labile <-target_results$S2
  
  CH4_theta<- c( 14.9025078 + 67.1, #Ea_CH4_SOC kJ mol-1
                 0.4644174 + 17, #kM_CH4_SOC
                 86.7, #Ea_CH4_labile kJ mol-1- used to be 16.7845002 + 71.1
                 0.4359649 + 23, #kM_Ch4labile
                 15.8857612 + 75.4, #Ea_CH4_oxi
                 0.5120464 + 23, #kM_CH4oxi
                 100, #486.4106939, #kI_SO4
                 0.2) #0.1020278 ) #kI_NO3
  CH4_mod_target <- PEPRMT_CH4_FINAL(theta = CH4_theta,
                                     data = target,
                                     wetland_type=2)
  
  #Create a new dataset that included model results
  target_results<-target_results %>%
    left_join(CH4_mod_target %>%
                rename(DOY = Time_2,
                       CH4_mod = pulse_emission_total),
              by = c("DOY", "site"))
  
  target %>%
    group_by(site) %>%
    mutate(EVI = (EVI - min(EVI)) * 40) %>%
    ggplot(aes(x = DOY)) +
    geom_line(aes(y = -V16, color = "GPP"))+
    geom_line(aes(y = EVI, color = "EVI"))+
    facet_wrap(~site)
  
  target_results %>%
    complete(DOY, site) %>%
    ggplot(aes(x = DOY)) +
    ylab("Flux")+
    geom_line(aes(y = Plant_flux_net, color = "Plant"))+
    geom_line(aes(y = Hydro_flux, color = "Hydro"))+
    facet_wrap(~site)
  
  target_results %>%
    ggplot(aes(x = DOY, y = Plant_flux_net)) +
    geom_line()+
    facet_wrap(~site)
  
  return(target_results)
}

