mod <- "model{
# -------- Priors --------
for (s in 1:n_seasons){ # Iterate over the season
  for (x in 1:n_x_vals){ # Iterate over the constraint categories
    S4[s,x] ~ dbeta(1,1) # Non-informative Beta Prior for the Survival Probability in PWS 
    S8[s,x] ~ dbeta(1,1) # Non-informative Beta Prior for the Survival/Permenent Immigration Probability in the GOA
    psi2[1:2,s,x] ~ ddirch(c(2,2)) # Non-Informative Dirichlet Prior for the Movement Probabilities at the Spawning Arrays 
    psi3[1:2,s,x] ~ ddirch(c(2,2)) # Non-Informative Dirichlet Prior for the Movement Probabilities at the Other PWS Arrays
    psi4[1:6,s,x] ~ ddirch(c(2,2,2,2,2,2)) # Non-Informative Dirichlet Prior for the Movement Probabilities in PWS
    psi5[1:3,s,x] ~ ddirch(h_data[,s,x]) # Informative Dirichlet Prior at Hinchinbrook
    psi6[1:3,s,x] ~ ddirch(m_data[,s,x]) # Informative Dirichlet Prior at Montague
    psi7[1:3,s,x] ~ ddirch(s_data[,s,x]) # Informative Dirichlet Prior for the Southwest Passage Arrays
    psi8[1:4,s,x] ~ ddirch(c(2,2,2,2))  # Non-Informative Dirichlet Prior for the Movement Probabilities in the GOA
  }
}

# -------- Transition Matrix --------
for (s in 1:n_seasons){ # Iterate over the season
  for (x in 1:n_x_vals){ # Iterate over the constraint categories
    tr[1,1,s,x] <- 1 # dead to dead
    tr[2,1,s,x] <- 0 # dead to spawning
    tr[3,1,s,x] <- 0 # dead to pws array
    tr[4,1,s,x] <- 0 # dead to pws
    tr[5,1,s,x] <- 0 # dead to hinchinbrook
    tr[6,1,s,x] <- 0 # dead to montague
    tr[7,1,s,x] <- 0 # dead to southwest
    tr[8,1,s,x] <- 0 # dead to goa
    
    tr[1,2,s,x] <- 0 # spawning to dead
    tr[2,2,s,x] <- psi2[1,s,x] # spawning to spawning
    tr[3,2,s,x] <- 0 # spawning to pws array
    tr[4,2,s,x] <- psi2[2,s,x] # spawning to pws
    tr[5,2,s,x] <- 0 # spawning to hinchinbrook
    tr[6,2,s,x] <- 0 # spawning to montague
    tr[7,2,s,x] <- 0 # spawning to southwest
    tr[8,2,s,x] <- 0 # spawning to goa
    
    tr[1,3,s,x] <- 0 # pws array to dead
    tr[2,3,s,x] <- 0 # pws array to spawing
    tr[3,3,s,x] <- psi3[1,s,x] # pws array to pws array
    tr[4,3,s,x] <- psi3[2,s,x] # pws array to pws
    tr[5,3,s,x] <- 0 # pws array to hinchinbrook
    tr[6,3,s,x] <- 0 # pws array to montague
    tr[7,3,s,x] <- 0 # pws array to southwest
    tr[8,3,s,x] <- 0 # pws array to goa

    tr[1,4,s,x] <- 1-S4[s,x] # pws to dead
    tr[2,4,s,x] <- S4[s,x]*psi4[1,s,x] # pws to spawning
    tr[3,4,s,x] <- S4[s,x]*psi4[2,s,x] # pws to pws array
    tr[4,4,s,x] <- S4[s,x]*psi4[3,s,x] # pws to pws
    tr[5,4,s,x] <- S4[s,x]*psi4[4,s,x] # pws to hinchinbrook
    tr[6,4,s,x] <- S4[s,x]*psi4[5,s,x] # pws to montague
    tr[7,4,s,x] <- S4[s,x]*psi4[6,s,x] # pws to southwest
    tr[8,4,s,x] <- 0 # pws to goa

    tr[1,5,s,x] <- 0 # hinchinbrook to dead
    tr[2,5,s,x] <- 0 # hinchinbrook to spawning
    tr[3,5,s,x] <- 0 # hinchinbrook pws array
    tr[4,5,s,x] <- psi5[1,s,x] # hinchinbrook to pws
    tr[5,5,s,x] <- psi5[2,s,x] # hinchinbrook to hinchinbrook
    tr[6,5,s,x] <- 0 # hinchinbrook to montague
    tr[7,5,s,x] <- 0 # hinchinbrook to southwest
    tr[8,5,s,x] <- psi5[3,s,x] # hinchinbrook to goa

    tr[1,6,s,x] <- 0 # montague to dead
    tr[2,6,s,x] <- 0 # montague to spawning
    tr[3,6,s,x] <- 0 # montague to pws array
    tr[4,6,s,x] <- psi6[1,s,x] # montague to pws
    tr[5,6,s,x] <- 0 # montague to hinchinbrook
    tr[6,6,s,x] <- psi6[2,s,x] # montague to montague
    tr[7,6,s,x] <- 0 # montague to southwest
    tr[8,6,s,x] <- psi6[3,s,x] # montague to goa

    tr[1,7,s,x] <- 0 # southwest to dead
    tr[2,7,s,x] <- 0 # southwest to spawning
    tr[3,7,s,x] <- 0 # southwest to pws array
    tr[4,7,s,x] <- psi7[1,s,x] # southwest to pws
    tr[5,7,s,x] <- 0 # southwest to hinchinbrook
    tr[6,7,s,x] <- 0 # southwest to montague
    tr[7,7,s,x] <- psi7[2,s,x] # southwest to southwest
    tr[8,7,s,x] <- psi7[3,s,x] # southwest to goa

    tr[1,8,s,x] <- 1-S8[s,x] # goa to dead
    tr[2,8,s,x] <- 0 # goa to spawning
    tr[3,8,s,x] <- 0 # goa to pws array
    tr[4,8,s,x] <- 0 # goa to pws
    tr[5,8,s,x] <- S8[s,x]*psi8[1,s,x] # goa to hinchinbrook
    tr[6,8,s,x] <- S8[s,x]*psi8[2,s,x] # goa to montague
    tr[7,8,s,x] <- S8[s,x]*psi8[3,s,x] # goa to southwest
    tr[8,8,s,x] <- S8[s,x]*psi8[4,s,x] # goa to goa
  }
}

# -------- Emission Matrix --------
em[1,1] <- 1 # dead and no detect
em[2,1] <- 0 # dead and spawning detect
em[3,1] <- 0 # dead and pws array detect
em[4,1] <- 0 # dead and hinchinbrook detect
em[5,1] <- 0 # dead and montague detect
em[6,1] <- 0 # dead and southwest detect

em[1,2] <- 0 # spawning and no detect
em[2,2] <- 1 # spawning and spawning detect
em[3,2] <- 0 # spawning and pws array detect
em[4,2] <- 0 # spawning and hinchinbrook detect
em[5,2] <- 0 # spawning and montague detect
em[6,2] <- 0 # spawning and southwest detect

em[1,3] <- 0 # pws array and no detect
em[2,3] <- 0 # pws array and spawning detect
em[3,3] <- 1 # pws array and pws array detect
em[4,3] <- 0 # pws array and hinchinbrook detect
em[5,3] <- 0 # pws array and montague detect
em[6,3] <- 0 # pws array and southwest detect

em[1,4] <- 1 # pws and no detect
em[2,4] <- 0 # pws and spawning detect
em[3,4] <- 0 # pws and pws array detect
em[4,4] <- 0 # pws and hinchinbrook detect
em[5,4] <- 0 # pws and montague detect
em[6,4] <- 0 # pws and southwest detect

em[1,5] <- 0 # hinchinbrook and no detect
em[2,5] <- 0 # hinchinbrook and spawning detect
em[3,5] <- 0 # hinchinbrook and pws array detect
em[4,5] <- 1 # hinchinbrook and hinchinbrook detect
em[5,5] <- 0 # hinchinbrook and montague detect
em[6,5] <- 0 # hinchinbrook and southwest detect

em[1,6] <- 0 # montague and no detect
em[2,6] <- 0 # montague and spawning detect
em[3,6] <- 0 # montague and pws array detect
em[4,6] <- 0 # montague and hinchinbrook detect
em[5,6] <- 1 # montague and montague detect
em[6,6] <- 0 # montague and southwest detect

em[1,7] <- 0 # southwest and no detect 
em[2,7] <- 0 # southwest and spawning detect
em[3,7] <- 0 # southwest and pws array detect
em[4,7] <- 0 # southwest and hinchinbrook detect
em[5,7] <- 0 # southwest and montague detect
em[6,7] <- 1 # southwest and southwest detect

em[1,8] <- 1 # goa and no detect
em[2,8] <- 0 # goa and spawning detect
em[3,8] <- 0 # goa and pws array detect
em[4,8] <- 0 # goa and hinchinbrook detect
em[5,8] <- 0 # goa and montague detect
em[6,8] <- 0 # goa and southwest detect

# -------- Likelihood --------
for (i in 1:M){  # Iterate through fish
  z[i,t_0[i]] ~ dcat(c(0,.25,0,.75,0,0,0,0))  # XX To Modify this Piece XX
  for (t in t_0[i]:min((N-1),(t_0[i]+tl[i]-2))){  # Iterate over times when tag is active
    z[i,t+1] ~ dcat(tr[1:8, z[i,t], season[t+1], x_data[i]]) # Latent process   
  }
}
for(i in 1:M){ # Iterate through fish
  for (t in (t_0[i]+1):min((N-1),(t_0[i]+tl[i]-1))){ # Iterate over the times when the tag is active
     y_data[i,t] ~ dcat(em[1:6,z[i,t]] ) # Conditional likelihood
  }
}

# -------- Re-parametrize to Interpret Constraint Effects --------
for (s in 1:n_seasons){ # Iterate over the season
  for (x in 1:n_x_vals){ # Iterate over the constraint categories
    # Backtransform the survival and movement probabilities
    mus4[s,x] <- logit(S4[s,x]) # Survival PWS
    mus8[s,x] <- logit(S8[s,x]) # Survival GOA
    mup2[1:2,s,x] <- logit(psi2[1:2,s,x]) # Movement Spawning
    mup3[1:2,s,x] <- logit(psi3[1:2,s,x]) # Movement Other PWS
    mup4[1:6,s,x] <- logit(psi4[1:6,s,x]) # Movement PWS
    mup5[1:3,s,x] <- logit(psi5[1:3,s,x]) # Movement Hinchinbrook
    mup6[1:3,s,x] <- logit(psi6[1:3,s,x]) # Movement Montague
    mup7[1:3,s,x] <- logit(psi7[1:3,s,x]) # Movement Southwest Passages
    mup8[1:4,s,x] <- logit(psi8[1:4,s,x]) # Movement GOA
  }
}
for (s in 1:n_seasons){ # Iterate over the season
  for (x in 1:n_x_vals){ # Iterate over the constraint categories
    # Calculate the beta coefficients for the linear constraints
    betas4[s,x] <- mus4[s,x]-mus4[s,1] # Survival PWS
    betas8[s,x] <- mus8[s,x]-mus8[s,1] # Survival GOA
    betap2[1:2,s,x] <- mup2[1:2,s,x]-mup2[1:2,s,1] # Movement Spawning
    betap3[1:2,s,x] <- mup3[1:2,s,x]-mup3[1:2,s,1] # Movement Other PWS
    betap4[1:6,s,x] <- mup4[1:6,s,x]-mup4[1:6,s,1] # Movement PWS
    betap5[1:3,s,x] <- mup5[1:3,s,x]-mup5[1:3,s,1] # Movement Hinchinbrook
    betap6[1:3,s,x] <- mup6[1:3,s,x]-mup6[1:3,s,1] # Movement Montague
    betap7[1:3,s,x] <- mup7[1:3,s,x]-mup7[1:3,s,1] # Movement Southwest Passages
    betap8[1:4,s,x] <- mup8[1:4,s,x]-mup8[1:4,s,1] # Movement GOA
  }
}
}"

file_name = "C:/Users/19708/Desktop/Herring/Modeling/R Multistate CJS/Jags/09-27-jags.bugs"
writeLines(mod, con=file_name)
