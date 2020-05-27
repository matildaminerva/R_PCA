library(dplyr)
library(ggfortify)


data("iris")
head(iris)

#directory <- "/***/***/*"
#load(directory)


iris_without_species <- iris %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

pca_res <- prcomp(iris_without_species, scale. = TRUE)



plot1 <- autoplot(pca_res, data = iris, colour = 'Species',
                  loadings = TRUE, loadings.colour = 'blue',
                  loadings.label = TRUE, loadings.label.size = 5, frame = TRUE, frame.type = 'norm') + theme_bw() + labs(title = "PCA for the iris data set")+ scale_color_manual(values = c("turquoise2","purple2", "orange"))




removed_all_rows_petallength_under_two <- iris[!(iris$Petal.Length > 3.7),]

removed_without_species <- removed_all_rows_petallength_under_two %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)


pca_res2 <- prcomp(removed_without_species, scale. = TRUE)



plot2 <- autoplot(pca_res2, data = removed_all_rows_petallength_under_two, colour = 'Species',
                  loadings = TRUE, loadings.colour = 'blue',
                  loadings.label = TRUE, loadings.label.size = 5, frame = TRUE, frame.type = 'norm') + theme_bw() + labs(title = "PCA for the iris ds without rows where petal length < 3.7 ")+ scale_color_manual(values = c( "turquoise2", "purple2", "orange"))

 
 

pdfname <- "pca_iris"

pdf(paste(pdfname, ".pdf"),onefile = TRUE)

for(i in 3){
  
  
  gridi <- grid.arrange(plot1, plot2, nrow = 2, ncol = 1,  bottom = textGrob(
    "Graphs",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  ))
  
  
  print(gridi)
  
  
  
}
dev.off()


