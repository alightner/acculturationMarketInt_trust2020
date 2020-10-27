# cleaning script for loading df and cleaning env -------------------------
source('load-packages.R')
df <- monduliDataset::dataset
df$christian <- df$christian-1
# The removal of two outliers is commented out because truncating certain outliers
# for anonymity resolved the issue we saw in our PCA, prior to anonymizing/truncating
# those outliers, which was that two older men with many children were driving
# most of the variation. After anonymizing/truncating, they can be included/excluded
# without affecting the exploratory results much.
# df <- df[-c(110,145),]
# cleaning up environment -------------------------------------------------
## archive objects for analysis; remove clutter
arch <- c(
  'df'   # dataset
)
xcl <- ls(all=TRUE)
xcl <- xcl[-which(xcl %in% arch)]
rm(list=xcl); rm('xcl')
