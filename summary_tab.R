source('cleaning.R')
source('dictionary.R')
set.seed(2020)
load('data/mids_object')

sum_tab <- 
  dfl$data %>% 
  mutate(
    MI=purchases_market+sell_crops+sell_milk_meat+sell_handcrafts
  ) %>% 
  cbind(
    data.frame(
      trust=df$trust,
      check=df$check
    )
  ) %>% 
  dplyr::select(
    age:MI,
    insecure:purchases_market,
    trust,
    check
  ) %>% 
  skim() %>% 
  dplyr::select(
    name=skim_variable,
    complete=complete_rate,
    mean=numeric.mean,
    sd=numeric.sd,
    range=numeric.p0,
    min=numeric.p0, 
    max=numeric.p100,
   # median=numeric.p50,
    histogram=numeric.hist
  ) %>% 
  mutate(
    name = var_dict2[name],
    mean=round(mean,1),
    sd=round(sd,1),
    range = paste0(min," - ",max)
    # min = as.integer(min),
    # max = as.integer(max)
    # median = as.integer(median)
  ) %>% 
  dplyr::select(
    -min,
    -max
  )

sum_tab_split <- split(sum_tab, rep(1:2, each=nrow(sum_tab)/2))

prayer_test <- 
  t.test(df$prayer_frequency[df$christian==1], 
                      df$prayer_frequency[df$christian==0])
