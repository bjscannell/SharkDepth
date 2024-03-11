library(R2jags)
library(abind)
library(boot)
library(MASS)
library(ggplot2)
library(MCMCvis)


tag_ids <- unique(df3m$tag_id)
tag_id_index <- match(df3m$tag_id, tag_ids)
species_index <- as.integer(factor(df3m$species, levels = unique(df3m$species)))


mod_string <- "
model {
  for (i in 1:Nspecies) {
    b_species[i] ~ dnorm(0, 0.001)
  }

  # Random effect for individuals
  for (i in 1:Nindividuals) {
    b_individual[i] ~ dnorm(0, 0.001)
  }

  # Likelihood
  for (i in 1:Nobs) {
    logit(p[i]) <- b_species[species[i]] + b_individual[individual_index[i]]
    above3[i] ~ dbern(p[i])
    y_pred[i] ~ dbern(p[i])
  }
}

"
# Data preparation
Nobs <- nrow(df3m)
Nspecies <- length(unique(df3m$species))
Nindividuals <- length(unique(df3m$tag_id)) # Adding this line for number of individuals
Dat <- list(
  above3 = df3m$above3,
  species = df3m$species,
  individual_index = tag_id_index,  # Numeric indices for individuals
  y_pred = rep(0, Nobs),  # Placeholder for predicted values
  Nspecies = Nspecies,
  Nobs = Nobs,
  Nindividuals = Nindividuals)



# Initial values
InitStage <- list(
  list(b_species = rep(1, Nspecies), b_individual = rnorm(Nindividuals)),  # Adding initial values for b_individual
  list(b_species = rep(2, Nspecies), b_individual = rnorm(Nindividuals)),  # Adding initial values for b_individual
  list(b_species = rep(3, Nspecies), b_individual = rnorm(Nindividuals))   # Adding initial values for b_individual
)

# Parameters to monitor
ParsStage <- c("b_species", "b_individual", "y_pred")  # Adding b_individual to parameters to monitor

# MCMC settings
ni <- 100000
nt <- 20
nb <- 50000
nc <- 3


# Running the JAGS model
mod_3 <- jags(inits = InitStage,
              n.chains = nc,
              model.file = textConnection(mod_string),
              working.directory = getwd(),
              data = Dat,
              parameters.to.save = ParsStage,
              n.thin = nt,
              n.iter = ni,
              n.burnin = nb,
              DIC = TRUE)


MCMCtrace(mod_2, params = "b_species")

mod_2_mcmc <- as.mcmc(mod_2) # turn chains into an mcmc object then df 
mod_2_mcmc_chains <- as.data.frame(rbind(mod_2_mcmc[[1]], mod_2_mcmc[[2]],mod_2_mcmc[[3]]))


b_species_columns_wide <- mod_2_mcmc_chains[grep("^b_species", names(mod_2_mcmc_chains))]  
b_species_columns_long <- b_species_columns_wide %>% rowid_to_column() %>% 
  pivot_longer(!rowid, names_to = "species", values_to = "n" )

ggplot() +
  stat_halfeye(data = b_species_columns_long, aes(x = n, y = species)) 


b_individual_columns_wide <- mod_2_mcmc_chains[grep("^b_individual", names(mod_2_mcmc_chains))]    


species_summary <- data.frame(mod_2$BUGSoutput$summary)
species_summary <- summary[grepl("b_species*", rownames(summary)), ] %>% rownames_to_column()

# Extracting posterior samples from the model
model_output <- mod_2$BUGSoutput$sims.list

# Assuming you have the species information in your data
# Extracting species information
species_data <- Dat$species

# Extract species names
species_names <- unique(species_data)

# Create an empty list to store predicted probabilities for each species
predicted_probabilities_by_species <- list()

# Iterate over each species
for (i in 1:9) {
  # Extract posterior samples for the current species
  b_samples <- model_output$b_species[,i]
  b_individual_samples <- model_output$b_individual[,i]
  
  # Calculate log odds for each observation
  log_odds <- b_samples + b_individual_samples
  
  # Calculate predicted probabilities using the logistic function
  predicted_probs <- plogis(log_odds)
  
  # Combine with species information
  predicted_probs_df <- data.frame(Species = paste("b_species[", i, "]", sep = ""),
                                   Predicted_Probability = predicted_probs)
  
  # Store predicted probabilities for the current species
  predicted_probabilities_by_species[[paste0("b_species[", i, "]")]] <- predicted_probs_df
}

# Combine predicted probabilities for all species into a single data frame
all_predicted_probs <- do.call(rbind, predicted_probabilities_by_species)

# Plot predicted probabilities for each species
ggplot(all_predicted_probs, aes(x = Predicted_Probability, y = Species)) +
  stat_halfeye() +
  labs(title = "Predicted Probabilities by Species", x = "Species", y = "Predicted Probability")


