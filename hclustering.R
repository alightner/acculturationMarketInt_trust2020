source('cleaning.R')

# hierarchical clustering analysis ----------------------------------------

d <- read_csv('data/pca_df_scaled.csv')
d <- d %>% 
  dplyr::select(
    -(id:region),
    -pc1
  )
# To re-run hierarchical clustering: uncomment, run, and save
# **NOTE**: takes time to run -- Time difference of 50.847 mins
# m <- pvclust(d, method.hclust = 'ward.D', nboot=1e4)
# save(m, file='hclust_imputation')


# create cuts for dendrogram and assign clusters --------------------------

load('data/hclust_imputation')
# cuts <- cutree(m$hclust, k=5)
# save(cuts, file='data/cuts')
load('data/cuts')
d <- read_csv('data/pca_df_scaled.csv')
for(i in 1:max(cuts)){
  d[length(d)+1] <- scale(rowSums(d[names(cuts[cuts==i])]))
}
colnames(d)[(length(d)-max(cuts)+1):length(d)] <- paste0('cluster',1:max(cuts))
# write.table(d, file='data/cluster_df.csv', sep=',', row.names=FALSE)
