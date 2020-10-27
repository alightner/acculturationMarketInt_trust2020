source('cleaning.R')
source('functions.R')
scale_cols <- c('insecure','need','wealth','depend')
df[scale_cols] <- data.frame(scale(df[scale_cols]))

# prestige model only
m1 <- glm(trust ~ condition, data=df, family=binomial())

# incentives model only
m2 <- glm(trust ~ insecure + need + wealth + depend, data=df, family=binomial())

# both models (prestige + incentives)
m3 <- glm(trust ~ condition + insecure + need + wealth + depend, data=df, family=binomial())

# fact checking as outcome variable ---------------------------------------

# prestige model only
m4 <- glm(check ~ condition, data=df, family=binomial())

# incentives model only
m5 <- glm(check ~ insecure + need + wealth + depend, data=df, family=binomial())

# both models (prestige + incentives)
m6 <- glm(check ~ condition + insecure + need + wealth + depend, data=df, family=binomial())

# effects plots ------------------------------------------------------------
# vr1 <- visreg(m1, scale='response', gg=TRUE)
# pvr1 <- vr1 + theme_bw() + ylim(c(0,1))      ## cut for the figure limit

pvr2 <- pointReg(m2, multList=TRUE)

# vr3 <- visreg(m4, scale='response', gg=TRUE)
# pvr3 <- vr3 + theme_bw() + ylim(c(0,1))      ## cut for the figure limit

# vr4 <- visreg(m5, scale='response', gg=TRUE)
# pvr4.1 <- vr4[[1]] + theme_bw() + ylim(c(0,1))
# pvr4.2 <- vr4[[2]] + theme_bw() + ylim(c(0,1))
# pvr4.3 <- vr4[[3]] + theme_bw() + ylim(c(0,1))
# pvr4.4 <- vr4[[4]] + theme_bw() + ylim(c(0,1))      ## cut for the figure limit

# descriptive stats -------------------------------------------------------

desc_table <- df %>% group_by(condition) %>% 
  summarise(
    trust=mean(trust, na.rm=TRUE),
    check=mean(check, na.rm=TRUE)
  )
desc_table <- desc_table[-which(is.na(desc_table$condition)),]
tbl_trust <- data.frame(table(df$trust, df$condition))

mosaic_tab <- df %>% 
  group_by(region) %>% 
  summarise(
    trust1=sum(trust==1, na.rm=TRUE)/length(!is.na(trust)),
    trust05=sum(trust==0.5, na.rm=TRUE)/length(!is.na(trust)),
    trust0=sum(trust==0, na.rm=TRUE)/length(!is.na(trust)),
    check=mean(check, na.rm=TRUE)
  )
mosaic_tab[-1] <- signif(mosaic_tab[-1]*100,2)

# rmarkdown output ---------------------------------------------------------

trust_k <- signif(desc_table$trust[desc_table$condition=='experience'],2)
trust_p <- signif(desc_table$trust[desc_table$condition=='prestige'],2)
check_k <- signif(desc_table$check[desc_table$condition=='experience'],2)
check_p <- signif(desc_table$check[desc_table$condition=='prestige'],2)
ttbl <- signif(tbl_trust$Freq/sum(tbl_trust$Freq),2)

## trust model
pbm1_coef <- signif(summary(m1)$coefficients[2,1],2)
pbm1_p <- signif(summary(m1)$coefficients[2,4],2)

rim1_coef1 <- signif(summary(m2)$coefficients[2,1],2)
rim1_p1 <- signif(summary(m2)$coefficients[2,4],2)
rim1_coef2 <- signif(summary(m2)$coefficients[3,1],2)
rim1_p2 <- signif(summary(m2)$coefficients[3,4],2)
rim1_coef3 <- signif(summary(m2)$coefficients[4,1],2)
rim1_p3 <- signif(summary(m2)$coefficients[4,4],2)
rim1_coef4 <- signif(summary(m2)$coefficients[5,1],2)
rim1_p4 <- signif(summary(m2)$coefficients[5,4],2)

## fact-check model
pbm2_coef <- signif(summary(m4)$coefficients[2,1],2)
pbm2_p <- signif(summary(m4)$coefficients[2,4],2)

rim2_coef1 <- signif(summary(m5)$coefficients[2,1],2)
rim2_p1 <- signif(summary(m5)$coefficients[2,4],2)
rim2_coef2 <- signif(summary(m5)$coefficients[3,1],2)
rim2_p2 <- signif(summary(m5)$coefficients[3,4],2)
rim2_coef3 <- signif(summary(m5)$coefficients[4,1],2)
rim2_p3 <- signif(summary(m5)$coefficients[4,4],2)
rim2_coef4 <- signif(summary(m5)$coefficients[5,1],2)
rim2_p4 <- signif(summary(m5)$coefficients[5,4],2)


