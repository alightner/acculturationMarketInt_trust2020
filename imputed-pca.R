# creates the pca plots ---------------------------------------------------
source('dictionary.R')

# pooled imputation -------------------------------------------------------

load('data/imputed-analyses')
d <- read_csv('data/pca_df_scaled.csv')
set.seed(2020)
pca1 <- analysis$pca[[sample(1:5,1)]]
# pca1$x[,1] <- pca1$x[,1]*-1
# pca1$rotation[,1] <- pca1$rotation[,1]*-1

pca_plot <- autoplot(pca1, label=FALSE, colour='region', data=d) +
  theme_bw(base_size=10) + theme(legend.position = 'none') +
  scale_colour_gradientn(colours=viridis::magma(11)[c(4,8)]) +
  annotate(geom='rect', xmin=0.075, xmax=0.17, ymin=0.29, ymax=0.38, fill='white', colour='black') +
  annotate(geom='point', x=0.09, y=0.35, size=3, colour='#F76F5CFF') +
  annotate(geom='text', hjust=0, x=0.1, y=0.35, label='Northern region', size=3) +
  annotate(geom='point', x=0.09, y=0.32, size=3, colour='#641A80FF') +
  annotate(geom='text', hjust=0, x=0.1, y=0.32, label='Southern region', size=3)

row.names(pca1$rotation) <- var_dict2[row.names(pca1$rotation)]

loadings_plot <- pca_loadings_plot(pca1, 1:2) + 
  theme_bw(base_size=8) + labs(y='') + theme(legend.position = 'none') +
  scale_colour_gradient2(low='#641A80FF', mid='white', high='#F76F5CFF')

# note that figure 2 in the paper is a saved pdf because we manually corrected the axis label placement
fig2 <- (loadings_plot / pca_plot) + plot_annotation(tag_levels = 'A')

