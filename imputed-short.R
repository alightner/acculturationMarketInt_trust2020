source('cleaning.R')
source('functions.R')
source('dictionary.R')
# analyses with a randomly selected imputation ----------------------------

## exploratory model for trust and fact-checking as functions of pc1
## also running exploratory models to select a list of aic vals for fig 6

d <- read_csv('data/pca_df_scaled.csv')
d <- d[-c(1:5)]
d <- cbind(df[c(1:4,which(colnames(df)=='region'))], d)
d$condition <- ifelse(d$condition=='prestige',1,0)

mpc1_trust <- glm(trust ~ pc1, data=d, family=binomial())
mpc1pbm_trust <- glm(trust ~ pc1+condition, data=d, family=binomial())

mpc1_check <- glm(check ~ pc1, data=d, family=binomial())
mpc1pbm_check <- glm(check ~ pc1+condition, data=d, family=binomial())

mmi_trust <- glm(trust ~ MI, data=d, family=binomial())
mdep_trust <- glm(trust ~ depend, data=d, family=binomial())

mmi_check <- glm(check ~ MI, data=d, family=binomial())
mdep_check <- glm(check ~ depend, data=d, family=binomial())

pbm_trust <- glm(trust ~ condition, data=d, family=binomial())
rim_trust <- glm(trust ~ insecure + need + wealth + depend, data=d, family=binomial())
pbrim_trust <- glm(trust ~ condition + insecure + need + wealth + depend, data=d, family=binomial())

pbm_check <- glm(check ~ condition, data=d, family=binomial())
rim_check <- glm(check ~ insecure + need + wealth + depend, data=d, family=binomial())
pbrim_check <- glm(check ~ condition + insecure + need + wealth + depend, data=d, family=binomial())

trust_mods <- list(
  pbm_trust,
  rim_trust,
  pbrim_trust,
  mmi_trust,
  mdep_trust,
  mpc1_trust,
  mpc1pbm_trust
)

check_mods <- list(
  pbm_check,
  rim_check,
  pbrim_check,
  mmi_check,
  mdep_check,
  mpc1_check,
  mpc1pbm_check
)

expl_trust_aic <- aictab(trust_mods,
                         modnames=c(
                           'PBM', 'RIM', 'PBM+RIM',
                           'MI', 'DEP', 'PC1', 'PC1+PBM'
                         ))

expl_check_aic <- aictab(check_mods,
                         modnames=c(
                           'PBM', 'RIM', 'PBM+RIM',
                           'MI', 'DEP', 'PC1', 'PC1+PBM'
                         ))

# key figure: trust and check as functions of pc1
pc1_trust <- pointReg(trust_mods[[6]], transpar=0.1)
pc1_check <- pointReg(check_mods[[6]], transpar=0.1)

# hclust with randomly selected imputation --------------------------------

d0 <- d %>% 
  dplyr::select(
    -pc1,
    -(id:region)
  )

d2 <- read_csv('data/cluster_df.csv')
load('data/hclust_imputation')
load('data/cuts')

mclust_trust1 <- glm(trust ~ cluster2+cluster5, data=d2, family=binomial())
mclust_check1 <- glm(check ~ cluster2+cluster5, data=d2, family=binomial())

m2 <- as_tbl_graph(as.dendrogram(m$hclust))
auvals <- rep(NA, length(m$hclust$height))
for(i in 1:length(m$hclust$height)){
  x <- V(m2)$height[V(m2)$height!=0][i]
  if(x %in% m$hclust$height){
    auvals[i] <- m$edges$au[which(x==m$hclust$height)]
  }
}

clust_key <- c(
  'education/urban',
  'market integration',
  'farming/religious',
  'elder/household size',
  'traditional/large herds'
)

m2 <- m2 %>% 
  activate(nodes) %>% 
  tidygraph::mutate(
    au=ifelse(height>0, auvals, NA),
    #au_90=ifelse(au>=0.9,1,0)
    au_cat = case_when(
      au >= 0.9 ~ paste("\u2265",0.9), #"≥0.9",
      au >= 0.8 ~ paste("\u2265",0.8), #"≥0.8",
      TRUE ~ "< 0.8"
    )
  ) %>%
  tidygraph::mutate(
    cluster=cuts[as.character(V(m2)$label)],
    cluster=clust_key[cluster],
    label2 = var_dict2[as.character(V(m2)$label)]
  )

# figure 5 ----------------------------------------------------------------

circ_dendro_plot <- 
  ggraph(m2, 'dendrogram', circular=TRUE) +
  geom_edge_elbow(alpha=0.3) +
  geom_node_point(aes(colour=au_cat, shape=au_cat), size=2.5, alpha=0.4) +
  scale_colour_aaas(na.translate=FALSE) +
  labs(colour='au prob.', shape='au prob.') + 
  new_scale_color() +
  geom_node_text(aes(label=label2, colour=factor(cluster)), repel=TRUE, size=3) +
  scale_colour_manual(values=c(
    'red',
    'blue',
    'darkgreen',
    'purple',
    'black',
    'brown',
    'orange'
  ), na.translate=FALSE) +
  theme_void(base_size=10) +
  coord_fixed(clip='off') +
  labs(colour='cluster')

# adding cluster models (EMI, ETB) to model list --------------------------------

load('data/imputed-analyses')
set.seed(1989)
dkl <- analysis$data[[sample(1:5,1)]]
d0 <- d %>% 
  left_join(
    data.frame(id=d$id, EMI=dkl$EMI, ETB=dkl$ETB),
    by='id'
  )

trust_mods2 <- trust_mods
check_mods2 <- check_mods
add_trust <- list(
  glm(trust ~ EMI+ETB, data=d0, family=binomial()),
  glm(trust ~ EMI, data=d0, family=binomial()),
  glm(trust ~ ETB, data=d0, family=binomial())
)
add_check <- list(
  glm(check ~ EMI+ETB, data=d0, family=binomial()),
  glm(check ~ EMI, data=d0, family=binomial()),
  glm(check ~ ETB, data=d0, family=binomial())
)

len <- length(trust_mods)
for(i in 1:3){
  trust_mods2[[len+i]] <- add_trust[[i]]
  check_mods2[[len+i]] <- add_check[[i]]
}

expl_trust_aic2 <- aictab(trust_mods2,
                         modnames=c(
                           'PBM', 'RIM', 'PBM+RIM',
                           'MI', 'DEP', 'PC1', 'PC1+PBM',
                           'EMI+TB', 'EMI', 'TB'
                         ))

expl_check_aic2 <- aictab(check_mods2,
                         modnames=c(
                           'PBM', 'RIM', 'PBM+RIM',
                           'MI', 'DEP', 'PC1', 'PC1+PBM',
                           'EMI+TB', 'EMI', 'TB'
                         ))

# figure 6 ----------------------------------------------------------------

pooled <- read_csv('data/pooled-df.csv')
pooled$outcome <- factor(pooled$outcome, levels=c('trust','check'))
pool_dict2 <- c(
  'PC1',
  'PC1+PBM',
  'MI',
  'DEP',
  'PBM',
  'RIM',
  'PBM+RIM',
  'EMI+TB',
  'EMI',
  'TB'
)
names(pool_dict2) <- unique(as.character(pooled$model_cat))
pooled$model_cat <- pool_dict2[pooled$model_cat]

expl_trust_aic2$outcome <- 'trust'
expl_check_aic2$outcome <- 'check'
aic_df <- rbind(expl_trust_aic2, expl_check_aic2)
pooled2 <- left_join(pooled, aic_df, 
                     by=c('model_cat'='Modnames', 'outcome'='outcome'))

## ordering model categories as factor levels, based on AICc ordering
pooled2$model_cat <- factor(pooled2$model_cat, levels=c(
  'PC1',
  'PC1+PBM',
  'MI',
  'EMI',
  'EMI+TB',
  'DEP',
  'RIM',
  'PBM+RIM',
  'TB',
  'PBM'
))

pool_dict2 <- c(
  'PC1',
  'prestige',
  'MI',
  'depend',
  'insecure',
  'need',
  'wealth',
  'EMI',
  'TB'
)
names(pool_dict2) <- unique(as.character(pooled2$predictor))
pooled2$predictor <- pool_dict2[pooled2$predictor]

pooled2$outcome <- factor(pooled2$outcome, levels=c('trust', 'check'))
pooled2$Delta_AICc[pooled2$outcome=='trust'] <- txtredx(pooled2$Delta_AICc[pooled2$outcome=='trust'])
pooled2$Delta_AICc[pooled2$outcome=='check'] <- txtredx(pooled2$Delta_AICc[pooled2$outcome=='check'])

exploratory_coef_plot2 <- pooled2 %>% 
  ggplot(aes(x=est, y=predictor, 
             colour=predictor
             )) + 
  geom_point(alpha=1) + 
  geom_errorbarh(aes(xmin=est-(2*sd_t), xmax=est+(2*sd_t)), height=0, lwd=1, alpha=0.4) +
  geom_text(aes(x=1.75, y=Inf, 
                label=ifelse(is.na(Delta_AICc), '', 
                             paste0("\u0394AIC=",round(Delta_AICc,2)))),
            size=2.5, hjust=1, vjust=1.5, colour='black') +
  facet_grid(model_cat ~ outcome, scales='free_y') +
  theme_bw(base_size=11) + 
  labs(y='predictor', x='estimate') +
  geom_vline(xintercept=0) +
  theme(strip.text.y=element_text(angle=0), legend.position='none') + 
  scale_colour_aaas()

# reported stats here -------------------------------------------------
## some reported stats in the main text

df <- monduliDataset::dataset
df$christian <- df$christian-1

perc_christian <- signif(mean(df$christian, na.rm=TRUE),2)*100
MI_test <- t.test(MI ~ region, data=d)
mi1 <- signif(as.numeric(MI_test$estimate[1]),2)
mi2 <- signif(as.numeric(MI_test$estimate[2]),2)
mi_est <- signif(as.numeric(MI_test$statistic),3)
mi_pv <- signif(as.numeric(MI_test$p.value),2)

set.seed(1989)
dkl <- analysis$data[[sample(1:5,1)]]
MI_corr <- signif(as.numeric(cor.test(~ pc1 + MI, data=d)$est),2)
EMI_corr <- signif(as.numeric(cor.test(~ pc1 + EMI, data=dkl)$est),2)
ETB_corr <- signif(as.numeric(cor.test(~ pc1 + ETB, data=dkl)$est),2)

d <- d %>% 
  left_join(
    data.frame(id=df$id, christian=df$christian),
    by='id'
  ) %>% 
  left_join(
    data.frame(id=d$id, EMI=dkl$EMI, ETB=dkl$ETB),
    by='id'
  ) %>% 
  dplyr::select(
    christian,
    MI,
    EMI,
    ETB
  )

christ_cor <- cor(d[complete.cases(d),])
christ_mi <- signif(as.numeric(christ_cor[,1])[2],2)
christ_emi <- signif(as.numeric(christ_cor[,1])[3],2)
christ_etb <- signif(as.numeric(christ_cor[,1])[4],1)

perc_trust <- sum(df$trust==1, na.rm=TRUE)/nrow(df)*100

