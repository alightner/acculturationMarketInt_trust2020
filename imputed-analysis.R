source('dictionary.R')
source('cleaning.R')
load('data/mids_object')

# scaling dataframe for pca plots here ------------------------------------

d <- read_csv('data/pca_df.csv')
d$condition <- ifelse(d$condition=='prestige',1,0)
mc <- which(colnames(d)=='MI')

## pca on randomly selected dataset to collect ~pc1 for a plot
pca1 <- prcomp(d[-c(1:5,mc)], scale.=TRUE)
pca1$x[,1] <- pca1$x[,1]*-1
pca1$rotation[,1] <- pca1$rotation[,1]*-1
d$pc1 <- pca1$x[,1]

d[-c(1:5)] <- data.frame(scale(d[-c(1:5)]))

write.table(d, file='data/pca_df_scaled.csv', sep=',', row.names=FALSE)

# imputed analysis here ---------------------------------------------------

analysis <- 
  tibble(imputation=1:5) %>%
  dplyr::mutate(
    data=map(1:5, ~complete(dfl, action = .)),
    pca=map(data, ~prcomp(., scale.=TRUE))
  ) %>%
  rowwise() %>%
  dplyr::mutate(
    data=list(bind_cols(data, 
                        MI=data$purchases_market +
                          data$sell_handcrafts +
                          data$sell_milk_meat +
                          data$sell_crops,
                        wealth=data$wives_cowives +
                          data$roof +
                          data$solar,
                        TLU=0.7*data$cattle +
                          0.5*data$donkeys +
                          0.1*data$goats +
                          0.1*data$sheep +
                          0.01*data$chickens)),
    data=list(bind_cols(data,
                        ETB=data$depend +
                          data$female_circumcision +
                          data$cattle_raid +
                          data$maasai_all_cattle +
                          data$polygyny +
                          data$chickens + 
                          data$cattle_over_cash +
                          data$sheep +
                          data$goats +
                          data$donkeys +
                          data$TLU +
                          data$cattle,
                        EMI=data$purchases_market +
                          data$MI +
                          data$sell_crops +
                          data$prayer_frequency +
                          data$sell_handcrafts +
                          data$solar +
                          data$roof +
                          data$need)),
    data=list(bind_cols(data, pc1=pca$x[,1])),
    data=list(data.frame(scale(data))),
    data=list(bind_cols(data, 
                        trust=df$trust, 
                        check=df$check,
                        condition=df$condition))
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    
    ## pc1 model only
    pc1_trust_m=map(data, ~glm(trust ~ pc1, family=binomial(), data=.)),
    pc1_trust_s = map(pc1_trust_m, summary),
    pc1_trust = map(pc1_trust_s, 'coefficients'),
    pc1_check_m = map(data, ~glm(check ~ pc1, family=binomial(), data=.)),
    pc1_check_s = map(pc1_check_m, summary),
    pc1_check = map(pc1_check_s, 'coefficients'),
    
    ## pc1+condition
    pc1pbm_trust_m=map(data, ~glm(trust ~ pc1+condition, family=binomial(), data=.)),
    pc1pbm_trust_s = map(pc1pbm_trust_m, summary),
    pc1pbm_trust = map(pc1pbm_trust_s, 'coefficients'),
    pc1pbm_check_m = map(data, ~glm(check ~ pc1+condition, family=binomial(), data=.)),
    pc1pbm_check_s = map(pc1pbm_check_m, summary),
    pc1pbm_check = map(pc1pbm_check_s, 'coefficients'),
    
    # MI
    mi_trust_m=map(data, ~glm(trust ~ MI, family=binomial(), data=.)),
    mi_trust_s = map(mi_trust_m, summary),
    mi_trust = map(mi_trust_s, 'coefficients'),
    mi_check_m = map(data, ~glm(check ~ MI, family=binomial(), data=.)),
    mi_check_s = map(mi_check_m, summary),
    mi_check = map(mi_check_s, 'coefficients'),
    
    # depend
    dep_trust_m=map(data, ~glm(trust ~ depend, family=binomial(), data=.)),
    dep_trust_s = map(dep_trust_m, summary),
    dep_trust = map(dep_trust_s, 'coefficients'),
    dep_check_m = map(data, ~glm(check ~ depend, family=binomial(), data=.)),
    dep_check_s = map(dep_check_m, summary),
    dep_check = map(dep_check_s, 'coefficients'),
    
    ## confirmatory analyses with imputation
    
    # PBM
    pbm_trust_m=map(data, ~glm(trust ~ condition, family=binomial(), data=.)),
    pbm_trust_s = map(pbm_trust_m, summary),
    pbm_trust = map(pbm_trust_s, 'coefficients'),
    pbm_check_m = map(data, ~glm(check ~ condition, family=binomial(), data=.)),
    pbm_check_s = map(pbm_check_m, summary),
    pbm_check = map(pbm_check_s, 'coefficients'),
    
    # RIM
    rim_trust_m=map(data, ~glm(trust ~ insecure+need+depend+wealth, family=binomial(), data=.)),
    rim_trust_s = map(rim_trust_m, summary),
    rim_trust = map(rim_trust_s, 'coefficients'),
    rim_check_m = map(data, ~glm(check ~ insecure+need+depend+wealth, family=binomial(), data=.)),
    rim_check_s = map(rim_check_m, summary),
    rim_check = map(rim_check_s, 'coefficients'),
    
    # PBM+RIM
    pbrim_trust_m=map(data, ~glm(trust ~ condition+insecure+need+depend+wealth, family=binomial(), data=.)),
    pbrim_trust_s = map(pbrim_trust_m, summary),
    pbrim_trust = map(pbrim_trust_s, 'coefficients'),
    pbrim_check_m = map(data, ~glm(check ~ condition+insecure+need+depend+wealth, family=binomial(), data=.)),
    pbrim_check_s = map(pbrim_check_m, summary),
    pbrim_check = map(pbrim_check_s, 'coefficients'),
    
    ## hclust analyses after imputation
    hclust_trust_m=map(data, ~glm(trust ~ EMI+ETB, family=binomial(), data=.)),
    hclust_trust_s = map(hclust_trust_m, summary),
    hclust_trust = map(hclust_trust_s, 'coefficients'),
    hclust_check_m=map(data, ~glm(check ~ EMI+ETB, family=binomial(), data=.)),
    hclust_check_s = map(hclust_check_m, summary),
    hclust_check = map(hclust_check_s, 'coefficients'),
    
    EMI_trust_m=map(data, ~glm(trust ~ EMI, family=binomial(), data=.)),
    EMI_trust_s = map(EMI_trust_m, summary),
    EMI_trust = map(EMI_trust_s, 'coefficients'),
    EMI_check_m=map(data, ~glm(check ~ EMI, family=binomial(), data=.)),
    EMI_check_s = map(EMI_check_m, summary),
    EMI_check = map(EMI_check_s, 'coefficients'),
    
    ETB_trust_m=map(data, ~glm(trust ~ ETB, family=binomial(), data=.)),
    ETB_trust_s = map(ETB_trust_m, summary),
    ETB_trust = map(ETB_trust_s, 'coefficients'),
    ETB_check_m=map(data, ~glm(check ~ ETB, family=binomial(), data=.)),
    ETB_check_s = map(ETB_check_m, summary),
    ETB_check = map(ETB_check_s, 'coefficients')
    
  ) %>% 
  rowwise() %>%
  mutate(
    # compute AIC tables
    aictab_trust = list(aictab(list(
      pc1=pc1_trust_m,
      MI=mi_trust_m,
      pc1_pbm=pc1pbm_trust_m,
      dep=dep_trust_m,
      RIM=rim_trust_m,
      PBM=pbm_trust_m,
      PBM_RIM=pbrim_trust_m,
      hclust=hclust_trust_m,
      EMI=EMI_trust_m,
      ETB=ETB_trust_m
    ))), 
    aictab_check = list(aictab(list(
      pc1=pc1_check_m,
      MI=mi_check_m,
      pc1_pbm=pc1pbm_check_m,
      dep=dep_check_m,
      RIM=rim_check_m,
      PBM=pbm_check_m,
      PBM_RIM=pbrim_check_m,
      hclust=hclust_check_m,
      EMI=EMI_check_m,
      ETB=ETB_check_m
    )))
  ) %>% 
  ungroup()

dfl_mods <- c(
  'pc1_trust', 'pc1_check',
  'pc1pbm_trust', 'pc1pbm_check',
  'mi_trust', 'mi_check',
  'dep_trust', 'dep_check',
  'pbm_trust', 'pbm_check',
  'rim_trust', 'rim_check',
  'pbrim_trust', 'pbrim_check'
)

asm <- 
  analysis %>%
  summarise(
    across(ends_with('_m'), ~list(pool(as.mira(.x))))
  ) 

pooled <- data.frame()

for(i in 1:length(colnames(asm))){
  model <- unlist(strsplit(colnames(asm)[i], split='_'))
  
  model_cat <- model[1]
  outcome <- model[2]
  model <- paste(model[-(length(model))], collapse='_')
  
  x <- asm[i][[1]][[1]]$pooled$estimate
  est <- x[2:length(x)]
  
  x <- asm[i][[1]][[1]]$pooled$term
  predictor <- x[2:length(x)]
  predictor <- as.character(predictor)
  
  x <- asm[i][[1]][[1]]$pooled$ubar
  wvar <- x[2:length(x)]
  
  x <- asm[i][[1]][[1]]$pooled$b
  bvar <- x[2:length(x)]
  
  x <- asm[i][[1]][[1]]$pooled$t
  totalvar <- x[2:length(x)]
  
  pooled <- rbind(pooled, data.frame(model,model_cat,predictor,outcome,
                                     est,wvar,bvar,totalvar, sd_t=sd(totalvar)))
}

#pooled <- subset(pooled, model_cat!='dep')
pooled$mod_pred <- paste(as.character(pooled$model_cat), as.character(pooled$predictor), sep=':')
pooled$sd_t <- sqrt(pooled$totalvar)

# saved objects -----------------------------------------------------------

save(analysis, file = 'data/imputed-analyses')
save(asm, file='data/pooled-imputed-effects')
write.table(pooled, file='data/pooled-df.csv', sep=',', row.names=FALSE)

