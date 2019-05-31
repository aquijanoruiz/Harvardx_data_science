#------------------------Q1--------------------------------
library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x) #the data set includes a matrix with dim 189 x 500

#This matrix has the gene expression levels of 500 genes from 189 biological samples representing 
#seven different tissues. The tissue type is stored in y:
table(tissue_gene_expression$y) 
sum(table(tissue_gene_expression$y)) #the biological samples sum up to 189

#Compute the Euclidean distance between each observation and stores it in the object d.
d <- dist(tissue_gene_expression$x)

#------------------------Q2--------------------------------
#Compare the distances between observations 1 and 2 (both cerebellum), observations 39 and 40 (both colon), 
#and observations 73 and 74 (both endometrium).

as.matrix(d)[35:42,35:42] #cerebellum
as.matrix(d)[39:40,39:40] #colon
as.matrix(d)[73:74,73:74] #endometrium

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind] #this way we can compare them against each other.

#------------------------Q3--------------------------------
#Make a plot of all the distances using the image function to see if the pattern you observed in Q2 is general.

image(as.matrix(d))
