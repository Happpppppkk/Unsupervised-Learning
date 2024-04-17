#assignment1
#===Data Preparation====#
library(foreign)
pewdata = read.spss("Mar19public.sav", use.value.labels = TRUE,
                    to.data.frame = TRUE)
#print(str(pewdata)) 

# check on column indices for political opinion variables
#print(names(pewdata)[26]) # this is q1
#print(names(pewdata)[98])
pewopinion = pewdata[,26:98]
ok = complete.cases(pewopinion)
cat("\nNumber of original cases with complete data:", sum(ok))
cat("\nNumber of original cases with incomplete data:", sum(!ok))
item_complete = c() # initialize list of names of items
for (item in seq(along = names(pewopinion))) {
  if (sum(complete.cases(pewopinion[,item])) == nrow(pewopinion))
    item_complete = c(item_complete, names(pewopinion)[item])
}
cat("\n\nNumber of items with complete data:",length(item_complete)) 
cat("\nNames of items with complete data:\n", item_complete)
pewwork = pewopinion[,item_complete]
#print(str(pewwork))
design_string = paste("~ q1 + q2 + q19 + q20 + q25 + q47 + q50a + q50b +", 
                      "q50c + q50d + q50e + q58 + q60 + q61a + q61b +", 
                      "q61c + q64 + q65a + q65b + q65c + q65d + q65e +", 
                      "q66 + q68a + q68b + q68d + q69 + q70 + q71 + q75")
pewmat = model.matrix(formula(design_string), data = pewwork)
#print(str(pewmat))
pewdf = as.data.frame(pewmat)
unique_values <- unique(pewwork$q68d)
print(unique_values)

print(str(pewdf))
rows_with_na <- which(rowSums(is.na(pewdf)) > 0, arr.ind = TRUE)
print(rows_with_na)
#====PCA======#
library(psych) 
library(lessR)
library(ggplot2)
library(reshape2)
mydata1 <- pewdf[,-1]
mydata1$`q69(VOL) Other/Depends` <- NULL
#visualzied 
body.cor <- cor(mydata1)
# Create the heatmap
heatmap(body.cor, 
        symm = TRUE,         
        main = "Correlation Matrix Heatmap",  
        Colv = NA,            
        Rowv = NA,            
        scale = "none",       
        margins = c(5, 10))  
#component retain visualization and eigen
fa.parallel(mydata1, fa="pc", n.iter=100, 
            show.legend=FALSE, main="Scree plot with parallel analysis")
scree(body.cor)
Z <- eigen(body.cor)
Z$val
Z$vec
#pca
pc1 <- principal(body.cor, nfactors=2, rotate="none", scores=TRUE)
print(pc1)
head(pc1$scores)
pc2 <- principal(mydata1, nfactors=2, rotate="none", scores=TRUE)
print(pc2)
#head(pc2$scores)
#print(pc1$loadings)
#pctb <-as.data.frame(pc1$loadings)
#write.csv(pctb, "loadings.csv", row.names = FALSE)
biplot(pc2)
flibrary(GGally)
pca_scores <- as.data.frame(pc2$scores)  
ggpairs(pca_scores, title = "Pair Plot of Principal Component Scores")


