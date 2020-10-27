source('cleaning.R')
set.seed(2020)

# preparing quantitative df w/ minimal NAs --------------------------------

df$int_id <- ifelse(df$region=='north',1,0)
df$condition <- ifelse(df$condition=='experience',0,1)

df2 <- df %>% 
  select_if(is.numeric) %>% 
  dplyr::select(-livestock_manage
  ) %>% 
  dplyr::select(-trust,
                -check,
                -wealth,
                -TLU,
                -condition,
                -id,
                -int_id
  ) %>% 
  select_if(~sum(is.na(.))<=10)

# imputation --------------------------------------------------------------

dfl <- mice(df2, m=5)

# creating and writing df and objects -------------------------------------
d <- complete(dfl, action=sample(1:5,1))

d <- cbind(id=df$id, 
      trust=df$trust, 
      check=df$check,
      condition=df$condition,
      region=df$int_id,
      d)

d$wealth <- d$wives_cowives+d$solar+d$roof
d$TLU <- d$cattle*0.7 + d$sheep*0.1 + d$goats*0.1 + d$chickens*0.01 + d$donkeys*0.5

d <- cbind(d, MI=scale(
  d$purchases_market+d$sell_crops+d$sell_milk_meat+d$sell_handcrafts
))

write.table(d, file='data/pca_df.csv', sep=',', row.names = FALSE)
save(dfl, file='data/mids_object')

