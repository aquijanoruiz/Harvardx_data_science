#-------------------Q1---------------------
#We want to explore the tissue_gene_expression predictors by plotting them.

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#We want to get an idea of which observations are close to each other, but, as you can see from the 
#dimensions, the predictors are 500-dimensional, making plotting difficult. Plot the first two principal 
#components with color representing tissue type.

#Which tissue is in a cluster by itself?

x<- tissue_gene_expression$x
pc <- prcomp(x)

dat <- as.data.frame(pca$x) %>% select(PC1,PC2)

dat %>% mutate(tissue = tissue_gene_expression$y) %>% ggplot(aes(x=PC1, y=PC2,color=tissue)) + geom_point()

#-------------------Q2---------------------
#The predictors for each observation are measured using the same device and experimental procedure.
#This introduces biases that can affect all the predictors from one observation. For each observation, 
#compute the average across all predictors, and then plot this against the first PC with color 
#representing tissue. Report the correlation.

#What is the correlation?

avgs <- rowMeans(tissue_gene_expression$x)

data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()

cor(avgs, pc$x[,1])

#-------------------Q3---------------------
#We see an association with the first PC and the observation averages. Redo the PCA but only after 
#removing the center. Part of the code is provided for you.

means <- rowMeans(x)

x <- with(tissue_gene_expression, sweep(x, 1, means)) #remember that sweep has FUN="-" as default

pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

##This is an example to understand the use of sweep
mat <- matrix(seq(from=2,to=18,by=2),3,3)
sweep(mat, 1, rowMeans(mat))
sweep(mat, 1, mean(mat))

#-------------------Q4---------------------
#For the 7th PC, which two tissues have the second greatest median difference?
data.frame(pc_7=pc$x[,7], tissue=tissue_gene_expression$y) %>% 
  ggplot(aes(tissue,pc_7)) + geom_boxplot()

for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

#-------------------Q5---------------------
#Plot the percent variance explained by PC number. Hint: use the summary function.

#How many PCs are required to reach a cumulative percent variance explained greater than 50%?

#A learner's answer
importance_df <- data.frame(summary(pc)$importance)
importance_df <- importance_df[2,] %>% 
  gather(key = pc, value = importance)
importance_df <- importance_df %>% mutate(pc_index = as.integer(str_remove(importance_df$pc, "PC")))
importance_df$pc <- factor(importance_df$pc, levels = importance_df$pc[order(importance_df$pc_index)])
importance_df <- importance_df %>% mutate(cum_sum = cumsum(importance))

importance_df %>% 
  filter(pc_index < 20) %>% 
  arrange(pc_index, cum_sum) %>% 
  ggplot(aes(x = pc, y = cum_sum, fill=pc)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  theme_grey()

#The staff's answer
plot(summary(pc)$importance[3,])
