#---------------------------Q1-------------------------
#We want to explore the tissue_gene_expression predictors by plotting them.

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#We want to get an idea of which observations are close to each other, but, as you can see from the dimensions, 
#the predictors are 500-dimensional, making plotting difficult. Plot the first two principal components with color 
#representing tissue type.

head(tissue_gene_expression)

pc <- prcomp(tissue_gene_expression$x)
pc$rotation

as.data.frame(pc$rotation) %>% select(PC1,PC2) %>% mutate(predictor = predictors) %>%
  filter(abs(PC1)==abs(PC2))

as.data.frame(pc$rotation) %>% select(PC1,PC2) %>% mutate(predictor = predictors) %>%
  ggplot(aes(PC1,PC2),color= predictors) +
    geom_point()

predictors <- row.names(as.data.frame(pc$rotation))
