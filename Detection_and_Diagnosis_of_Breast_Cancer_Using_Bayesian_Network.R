#install.packages("ggplot2") #ggplot2_3.2.1
#install.packages("corrplot") #corrplot_0.84
#install.packages("Hmisc") #Hmisc_4.2-0
#BiocManager::install("DEGreport") #DEGreport_1.22.0
#install.packages("FSelector") #FSelector_0.31
#install.packages("arules") #arules_1.6-4
#install.packages("arulesViz") #arulesViz_1.3-3
#install.packages("bnlearn") #bnlearn_4.5 
#install.packages("BiocManager") #BiocManager_1.30.9
#BiocManager::install("Rgraphviz") #Rgraphviz_2.30.0 
#install.packages("qgraph") #qgraph_1.6.4 
#BiocManager::install("RBGL") #RBGL_1.62.1
#install.packages("pcalg") #pcalg_2.6-7  
#install.packages("gRain") #gRain_1.3-0
#install.packages("caret") #caret_6.0-84 

#sessionInfo()
#R version 3.6.1 (2019-07-05)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows >= 8 x64 (build 9200)

options(max.print=999999)


##Read data into R.
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", header = FALSE, sep = ",")

##Create names vector.
col_names <- c("id", "diagnosis", "mean_radius", "mean_texture", "mean_perimeter", "mean_area", "mean_smoothness", "mean_compactness", "mean_concavity", "mean_concave_points", "mean_symmetry", "mean_fractal_dimension", "radius_se", "texture_se", "perimeter_se", "area_se", "smoothness_se", "compactness_se", "concavity_se", "concave_points_se", "symmetry_se", "fractal_dimension_se", "worst_radius", "worst_texture", "worst_perimeter", "worst_area", "worst_smoothness", "worst_compactness", "worst_concavity", "worst_concave_points", "worst_symmetry", "worst_fractal_dimension")

##Assign attribute names, col_names to names.
names(data) <- col_names

##Structure of the data.
str(data)
dim(data)

##Change the attribute id from numeric to categorical variable using as.factor.
data$id<- as.factor(data$id)
str(data)

##Determine any missing values.
sum(is.na(data))
#There are no missing values.

##Look for duplicate cases.
sum(duplicated(data))
#There are no duplicate cases.

##Find minimum, 1st quantile, median, mean, 3rd quantile and maximum for numeric attributes.
summary(data)

##Find standard deviation for numeric attributes.
sd(data$mean_radius)
sd(data$mean_texture)
sd(data$mean_perimeter)
sd(data$mean_area)
sd(data$mean_smoothness)
sd(data$mean_compactness)
sd(data$mean_concavity)
sd(data$mean_concave_points)
sd(data$mean_symmetry)
sd(data$mean_fractal_dimension)
sd(data$radius_se)
sd(data$texture_se)
sd(data$perimeter_se)
sd(data$area_se)
sd(data$smoothness_se)
sd(data$compactness_se)
sd(data$concavity_se)
sd(data$concave_points_se)
sd(data$symmetry_se)
sd(data$fractal_dimension_se)
sd(data$worst_radius)
sd(data$worst_texture)
sd(data$worst_perimeter)
sd(data$worst_area)
sd(data$worst_smoothness)
sd(data$worst_compactness)
sd(data$worst_concavity)
sd(data$worst_concave_points)
sd(data$worst_symmetry)
sd(data$worst_fractal_dimension)

##Check to see if there is a class imbalance for class attribute, diagnosis.
prop.table(table(data$diagnosis))
#Yes, there is a class imbalance, 62.74% (B) and 37.26% (M).

##Bar chart to show class imbalance of class attribute, diagnosis.
plot(data$diagnosis, main="Bar Chart of Class Attribute, Diagnois", xlab="Diagnosis", ylab="Count")

#########################################################################################
#                  Determining Outlier Values for Numeric Attributes                    #
#########################################################################################

library(ggplot2)

##Boxplots of numeric attributes to check for presence of outliers (1.5IQR).
par(mfrow=c(1,1))

boxplot(data$mean_radius, main="Boxplot for Mean Radius", ylab="Radius Measurement (micrometres)")
boxplot(data$mean_texture, main="Boxplot for Mean Texture", ylab="Texture")
boxplot(data$mean_perimeter, main="Boxplot for Mean Perimeter", ylab="Perimeter Measurement (micrometres)")
boxplot(data$mean_area, main="Boxplot for Mean Area", ylab="Area Measurement (square micrometres)")
boxplot(data$mean_smoothness, main="Boxplot for Mean Smoothness", ylab="Smoothness")
boxplot(data$mean_compactness, main="Boxplot for Mean Compactness", ylab="Compactness")
boxplot(data$mean_concavity, main="Boxplot for Mean Concavity", ylab="Concavity")
boxplot(data$mean_concave_points, main="Boxplot for Mean Concave Points", ylab="Concave Points")
boxplot(data$mean_symmetry, main="Boxplot for Mean Symmetry", ylab="Symmetry")
boxplot(data$mean_fractal_dimension, main="Boxplot for Mean Fractal Dimension", ylab="Fractal Dimension")
boxplot(data$radius_se, main="Boxplot for Radius Standard Error", ylab="Radius Measurement (micrometres)")
boxplot(data$texture_se, main="Boxplot for Texture Standard Error", ylab="Texture")
boxplot(data$perimeter_se, main="Boxplot for Perimeter Standard Error", ylab="Perimeter Measurement (micrometres)")
boxplot(data$area_se, main="Boxplot for Area Standard Error", ylab="Area Measurement (square micrometres)")
boxplot(data$smoothness_se, main="Boxplot for Smoothness Standard Error", ylab="Smoothness")
boxplot(data$compactness_se, main="Boxplot for Compactness Standard Error", ylab="Compactness")
boxplot(data$concavity_se, main="Boxplot for Concavity Standard Error", ylab="Concavity")
boxplot(data$concave_points_se, main="Boxplot for Concave Points Standard Error", ylab="Concave Points")
boxplot(data$symmetry_se, main="Boxplot for Symmetry Standard Error", ylab="Symmetry")
boxplot(data$fractal_dimension_se, main="Boxplot for Fractal Dimension Standard Error", ylab="Fractal Dimension")
boxplot(data$worst_radius, main="Boxplot for Worst Radius", ylab="Radius Measurement (micrometres)")
boxplot(data$worst_texture, main="Boxplot for Worst Texture", ylab="Texture")
boxplot(data$worst_perimeter, main="Boxplot for Worst Perimeter", ylab="Perimeter Measurement (micrometres)")
boxplot(data$worst_area, main="Boxplot for Worst Area", ylab="Area Measurement (square micrometres)")
boxplot(data$worst_smoothness, main="Boxplot for Worst Smoothness", ylab="Smoothness")
boxplot(data$worst_compactness, main="Boxplot for Worst Compactness", ylab="Compactness")
boxplot(data$worst_concavity, main="Boxplot for Worst Concavity", ylab="Concavity")
boxplot(data$worst_concave_points, main="Boxplot for Worst Concave Points", ylab="Concave Points")
boxplot(data$worst_symmetry, main="Boxplot for Worst Symmetry", ylab="Symmetry")
boxplot(data$worst_fractal_dimension, main="Boxplot for Worst Fractal Dimension", ylab="Fractal Dimension")

##Boxplots of numeric attributes to check for presence of outliers (1.5IQR) by class.
par(mfrow=c(1,1))

ggplot(data=data, aes(x=data$diagnosis, y=data$mean_radius)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Radius") + labs(x="Diagnosis", y="Mean Radius (micrometres)") + geom_point(position="jitter", color="blue", alpha=.3)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$mean_texture)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Texture") + labs(x="Diagnosis", y="Texture") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$mean_perimeter)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Perimeter") + labs(x="Diagnosis", y="Mean Perimeter (micrometres)") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$mean_area)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Area") + labs(x="Diagnosis", y="Mean Area (square micrometres)") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$mean_smoothness)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Smoothness") + labs(x="Diagnosis", y="Smoothness") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$mean_compactness)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Compactness") + labs(x="Diagnosis", y="Compactness") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$mean_concavity)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Concavity") + labs(x="Diagnosis", y="Concavity") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$mean_concave_points)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Concave Points") + labs(x="Diagnosis", y="Concave Points") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$mean_symmetry)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Symmetry") + labs(x="Diagnosis", y="Symmetry") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$mean_fractal_dimension)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Mean Fractal Dimension") + labs(x="Diagnosis", y="Fractal Dimension") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$radius_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Radius Standard Error") + labs(x="Diagnosis", y="Radius (micrometres)") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$texture_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Texture Standard Error") + labs(x="Diagnosis", y="Texture") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$perimeter_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Perimeter Standard Error") + labs(x="Diagnosis", y="Perimeter (micrometres)") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$area_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Area Standard Error") + labs(x="Diagnosis", y="Area (square micrometres)") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$smoothness_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Smoothness Standard Error") + labs(x="Diagnosis", y="Smoothness") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$compactness_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Compactness Standard Error") + labs(x="Diagnosis", y="Compactness") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$concavity_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Concavity Standard Error") + labs(x="Diagnosis", y="Concavity") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$concave_points_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Concave Points Standard Error") + labs(x="Diagnosis", y="Concave Points") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$symmetry_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Symmetry Standard Error") + labs(x="Diagnosis", y="Symmetry") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$fractal_dimension_se)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Fractal Dimension Standard Error") + labs(x="Diagnosis", y="Fractal Dimension") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_radius)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Radius") + labs(x="Diagnosis", y="Radius (micrometres)") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_texture)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Texture") + labs(x="Diagnosis", y="Texture") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_perimeter)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Perimeter") + labs(x="Diagnosis", y="Perimeter (micrometres)") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_area)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Area") + labs(x="Diagnosis", y="Area (sqaure micrometres)") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_smoothness)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Smoothness") + labs(x="Diagnosis", y="Smoothness") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_compactness)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Compactness") + labs(x="Diagnosis", y="Compactness") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_concavity)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Concavity") + labs(x="Diagnosis", y="Concavity") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_concave_points)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Concave Points") + labs(x="Diagnosis", y="Concave Points") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_symmetry)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Symmetry") + labs(x="Diagnosis", y="Symmetry") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")
ggplot(data=data, aes(x=data$diagnosis, y=data$worst_fractal_dimension)) + geom_boxplot(fill="cornflowerblue", color="black", outlier.colour="black", outlier.shape=16,outlier.size=2, notch=TRUE) + labs(title="Boxplot for Worst Fractal Dimension") + labs(x="Diagnosis", y="Worst Fractal Dimension") + geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")

##Determine outlier values for numeric attributes and by class (1.5IQR).
boxplot.stats(data$mean_radius) 
data[boxplot.stats(data$mean_radius)$out,c(2,3)]
table(data[boxplot.stats(data$mean_radius)$out, 2])
#Mean radius has 14 outliers (2B, 12M).

boxplot.stats(data$mean_radius[data$diagnosis=="B"])
boxplot.stats(data$mean_radius[data$diagnosis=="M"])
#By class for mean radius, there are 3 outliers for B and 3 outliers for M.

boxplot.stats(data$mean_texture)
data[boxplot.stats(data$mean_texture)$out,c(2,4)]
table(data[boxplot.stats(data$mean_texture)$out, 2])
#Mean texture has 7 outliers (0B, 7M).

boxplot.stats(data$mean_texture[data$diagnosis=="B"])
boxplot.stats(data$mean_texture[data$diagnosis=="M"])
#By class for mean texture, there are 18 outliers for B and 7 outliers for M.

boxplot.stats(data$mean_perimeter) 
data[boxplot.stats(data$mean_perimeter)$out,c(2,5)]
table(data[boxplot.stats(data$mean_perimeter)$out, 2]) 
#Mean perimeter has 13 outliers (11B, 2M).

boxplot.stats(data$mean_perimeter[data$diagnosis=="B"])
boxplot.stats(data$mean_perimeter[data$diagnosis=="M"])
#By class for mean perimeter, there are 4 outliers for B and 3 outliers for M.

boxplot.stats(data$mean_area) 
data[data$mean_area < 143.5 | data$mean_area > 1326.0, c(2,6)]
table(data[data$mean_area < 143.5 | data$mean_area > 1326.0, 2]) 
#Mean area has 25 outliers (0B, 25M).

boxplot.stats(data$mean_area[data$diagnosis=="B"])
boxplot.stats(data$mean_area[data$diagnosis=="M"])
#By class for mean area, there are 4 outliers for B and 4 outliers for M.

boxplot.stats(data$mean_smoothness) 
data[data$mean_smoothness < 0.06251 | data$mean_smoothness > 0.13350, c(2,7)]
table(data[data$mean_smoothness < 0.06251 | data$mean_smoothness > 0.13350, 2]) 
#Mean smoothness has 6 outliers (3B, 3M).

boxplot.stats(data$mean_smoothness[data$diagnosis=="B"])
boxplot.stats(data$mean_smoothness[data$diagnosis=="M"])
#By class for mean smoothness, there are 4 outliers for B and 3 outliers for M.

boxplot.stats(data$mean_compactness)
data[data$mean_compactness < 0.01938 | data$mean_compactness > 0.22840, c(2,8)]
table(data[data$mean_compactness < 0.01938 | data$mean_compactness > 0.22840, 2])
#Mean compactness has 16 outliers (0B, 16M).

boxplot.stats(data$mean_compactness[data$diagnosis=="B"])
boxplot.stats(data$mean_compactness[data$diagnosis=="M"])
#By class for mean compactness, there are 9 outliers for B and 8 outliers for M.

boxplot.stats(data$mean_concavity)
data[data$mean_concavity < 0.00000 | data$mean_concavity > 0.28100, c(2,9)]
table(data[data$mean_concavity < 0.00000 | data$mean_concavity > 0.28100, 2])
#Mean concavity has 18 outliers (3B, 15M).

boxplot.stats(data$mean_concavity[data$diagnosis=="B"])
boxplot.stats(data$mean_concavity[data$diagnosis=="M"])
#By class for mean concavity, there are 12 outliers for B and 6 outliers for M.

boxplot.stats(data$mean_concave_points)
data[data$mean_concave_points < 0.00000 | data$mean_concave_points > 0.15200, c(2,10)]
table(data[data$mean_concave_points < 0.00000 | data$mean_concave_points > 0.15200, 2])
#Mean concave points has 10 outliers (0B, 10M).

boxplot.stats(data$mean_concave_points[data$diagnosis=="B"])
boxplot.stats(data$mean_concave_points[data$diagnosis=="M"])
#By class for mean concave points, there are 17 outliers for B and 7 outliers for M.

boxplot.stats(data$mean_symmetry)
data[data$mean_symmetry < 0.1167 | data$mean_symmetry > 0.2459, c(2,11)]
table(data[data$mean_symmetry < 0.1167 | data$mean_symmetry > 0.2459, 2])
#Mean symmetry has 15 outliers (6B, 9M).

boxplot.stats(data$mean_symmetry[data$diagnosis=="B"])
boxplot.stats(data$mean_symmetry[data$diagnosis=="M"])
#By class for mean symmetry, there are 10 outliers for B and 4 outliers for M.

boxplot.stats(data$mean_fractal_dimension)
data[data$mean_fractal_dimension < 0.04996 | data$mean_fractal_dimension > 0.07871, c(2,12)]
table(data[data$mean_fractal_dimension < 0.04996 | data$mean_fractal_dimension > 0.07871, 2])
#Mean fractal dimension has 15 outliers (10B, 5M).

boxplot.stats(data$mean_fractal_dimension[data$diagnosis=="B"])
boxplot.stats(data$mean_fractal_dimension[data$diagnosis=="M"])
#By class for mean fractal dimension, there are 17 outliers for B and 1 outlier for M.

boxplot.stats(data$radius_se)
data[data$radius_se < 0.1115 | data$radius_se > 0.8426, c(2,13)]
table(data[data$radius_se < 0.1115 | data$radius_se > 0.8426, 2])
#Radius SE has 38 outliers (1B, 37M).

boxplot.stats(data$radius_se[data$diagnosis=="B"])
boxplot.stats(data$radius_se[data$diagnosis=="M"])
#By class for radius SE, there are 9 outliers for B and 4 outliers for M.

boxplot.stats(data$texture_se)
data[boxplot.stats(data$texture_se)$out,c(2,14)]
table(data[boxplot.stats(data$texture_se)$out, 2])
#Texture SE has 20 outliers (0B, 20M).

boxplot.stats(data$texture_se[data$diagnosis=="B"])
boxplot.stats(data$texture_se[data$diagnosis=="M"])
#By class for texture SE, there are 12 outliers for B and 7 outliers for M.

boxplot.stats(data$perimeter_se)
data[boxplot.stats(data$perimeter_se)$out,c(2,15)]
table(data[boxplot.stats(data$perimeter_se)$out, 2])
#Perimeter SE has 38 outliers (1B, 37M).

boxplot.stats(data$perimeter_se[data$diagnosis=="B"])
boxplot.stats(data$perimeter_se[data$diagnosis=="M"])
#By class for perimeter SE, there are 12 outliers for B and 8 outliers for M.

boxplot.stats(data$area_se)
data[boxplot.stats(data$area_se)$out,c(2,16)]
table(data[boxplot.stats(data$area_se)$out, 2])
#Area SE has 65 outliers (41B, 24M).

boxplot.stats(data$area_se[data$diagnosis=="B"])
boxplot.stats(data$area_se[data$diagnosis=="M"])
#By class for area SE, there are 15 outliers for B and 5 outliers for M.

boxplot.stats(data$smoothness_se)
data[data$smoothness_se < 0.001713 | data$smoothness_se > 0.012430, c(2,17)]
table(data[data$smoothness_se < 0.001713 | data$smoothness_se > 0.012430, 2])
#Smoothness SE has 30 outliers (26B, 4M).

boxplot.stats(data$smoothness_se[data$diagnosis=="B"])
boxplot.stats(data$smoothness_se[data$diagnosis=="M"])
#By class for smoothness SE, there are 15 outliers for B and 5 outliers for M.

boxplot.stats(data$compactness_se)
data[data$compactness_se < 0.002252 | data$compactness_se > 0.060630, c(2,18)]
table(data[data$compactness_se < 0.002252 | data$compactness_se > 0.060630, 2])
#Compactness SE has 28 outliers (14B, 14M).

boxplot.stats(data$compactness_se[data$diagnosis=="B"])
boxplot.stats(data$compactness_se[data$diagnosis=="M"])
#By class for compactness SE, there are 23 outliers for B and 11 outliers for M.

boxplot.stats(data$concavity_se)
data[data$concavity_se < 0.00000 | data$concavity_se > 0.08232, c(2,19)]
table(data[data$concavity_se < 0.00000 | data$concavity_se > 0.08232, 2])
#Concavity SE has 22 outliers (12B, 10M).

boxplot.stats(data$concavity_se[data$diagnosis=="B"])
boxplot.stats(data$concavity_se[data$diagnosis=="M"])
#By class for concavity SE, there are 22 outliers for B and 10 outliers for M.

boxplot.stats(data$concave_points_se)
data[data$concave_points_se < 0.000000 | data$concave_points_se > 0.025270, c(2,20)]
table(data[data$concave_points_se < 0.000000 | data$concave_points_se > 0.025270, 2])
#Concave points SE has 19 outliers (7B, 12M).

boxplot.stats(data$concave_points_se[data$diagnosis=="B"])
boxplot.stats(data$concave_points_se[data$diagnosis=="M"])
#By class for concave points SE, there are 15 outliers for B and 7 outliers for M.

boxplot.stats(data$symmetry_se)
data[data$symmetry_se < 0.007882 | data$symmetry_se > 0.035460, c(2,21)]
table(data[data$symmetry_se < 0.007882 | data$symmetry_se > 0.035460, 2])
#Symmetry SE has 27 outliers (11B, 16M).

boxplot.stats(data$symmetry_se[data$diagnosis=="B"])
boxplot.stats(data$symmetry_se[data$diagnosis=="M"])
#By class for symmetry SE, there are 15 outliers for B and 16 outliers for M.

boxplot.stats(data$fractal_dimension_se)
data[data$fractal_dimension_se < 0.0008948 | data$fractal_dimension_se > 0.0080150, c(2,22)]
table(data[data$fractal_dimension_se < 0.0008948 | data$fractal_dimension_se > 0.0080150, 2])
#Fractal dimension SE has 28 outliers (18B, 10M).

boxplot.stats(data$fractal_dimension_se[data$diagnosis=="B"])
boxplot.stats(data$fractal_dimension_se[data$diagnosis=="M"])
#By class for fractal dimension SE, there are 26 outliers for B and 9 outliers for M.

boxplot.stats(data$worst_radius)
data[boxplot.stats(data$worst_radius)$out,c(2,23)]
table(data[boxplot.stats(data$worst_radius)$out, 2])
#Worst radius has 17 outliers (0B, 17M).

boxplot.stats(data$worst_radius[data$diagnosis=="B"])
boxplot.stats(data$worst_radius[data$diagnosis=="M"])
#By class for worst radius, there are 2 outliers for B and 3 outliers for M.

boxplot.stats(data$worst_texture)
data[boxplot.stats(data$worst_texture)$out,c(2,24)]
table(data[boxplot.stats(data$worst_texture)$out, 2])
#Worst texture has 5 outliers (2B, 3M).

boxplot.stats(data$worst_texture[data$diagnosis=="B"])
boxplot.stats(data$worst_texture[data$diagnosis=="M"])
#By class for worst texture, there are 10 outliers for B and 4 outliers for M.

boxplot.stats(data$worst_perimeter)
data[boxplot.stats(data$worst_perimeter)$out,c(2,25)]
table(data[boxplot.stats(data$worst_perimeter)$out, 2])
#Worst perimeter has 15 outliers (4B, 11M).

boxplot.stats(data$worst_perimeter[data$diagnosis=="B"])
boxplot.stats(data$worst_perimeter[data$diagnosis=="M"])
#By class for worst perimeter, there are 2 outliers for B and 3 outliers for M.

boxplot.stats(data$worst_area)
data[data$worst_area < 185.2 | data$worst_area > 1933.0, c(2,26)]
table(data[data$worst_area < 185.2 | data$worst_area > 1933.0, 2])
#Worst area has 35 outliers (0B, 35M).

boxplot.stats(data$worst_area[data$diagnosis=="B"])
boxplot.stats(data$worst_area[data$diagnosis=="M"])
#By class for worst area, there are 3 outliers for B and 7 outliers for M.

boxplot.stats(data$worst_smoothness)
data[data$worst_smoothness < 0.08125 | data$worst_smoothness > 0.18830, c(2,27)]
table(data[data$worst_smoothness < 0.08125 | data$worst_smoothness > 0.18830, 2])
#Worst smoothness has 7 outliers (3B, 4M).

boxplot.stats(data$worst_smoothness[data$diagnosis=="B"])
boxplot.stats(data$worst_smoothness[data$diagnosis=="M"])
#By class for worst smoothness, there are 3 outliers for B and 4 outliers for M.

boxplot.stats(data$worst_compactness)
data[data$worst_compactness < 0.02729 | data$worst_compactness > 0.62470, c(2,28)]
table(data[data$worst_compactness < 0.02729 | data$worst_compactness > 0.62470, 2])
#Worst compactness has 16 outliers (0B, 16M).

boxplot.stats(data$worst_compactness[data$diagnosis=="B"])
boxplot.stats(data$worst_compactness[data$diagnosis=="M"])
#By class for worst compactness, there are 10 outliers for B and 8 outliers for M.

boxplot.stats(data$worst_concavity)
data[data$worst_concavity < 0.0000 | data$worst_concavity > 0.7727, c(2,29)]
table(data[data$worst_concavity < 0.0000 | data$worst_concavity > 0.7727, 2])
#Worst concavity has 12 outliers (2B, 10M).

boxplot.stats(data$worst_concavity[data$diagnosis=="B"])
boxplot.stats(data$worst_concavity[data$diagnosis=="M"])
#By class for worst concavity, there are 13 outliers for B and 4 outliers for M.

boxplot.stats(data$worst_concave_points)
data[data$worst_concave_points < 0.00000 | data$worst_concave_points > 0.29100, c(2,30)]
table(data[data$worst_concave_points < 0.00000 | data$worst_concave_points > 0.29100, 2])
#Worst concave points has 0 outlier (0B, 0M).

boxplot.stats(data$worst_concave_points[data$diagnosis=="B"])
boxplot.stats(data$worst_concave_points[data$diagnosis=="M"])
#By class for worst concave points, there are 2 outliers for B and 1 outlier for M.

boxplot.stats(data$worst_symmetry)
data[data$worst_symmetry < 0.1565 | data$worst_symmetry > 0.4154, c(2,31)]
table(data[data$worst_symmetry < 0.1565 | data$worst_symmetry > 0.4154, 2])
#Worst symmetry has 23 outliers (1B, 22M).

boxplot.stats(data$worst_symmetry[data$diagnosis=="B"])
boxplot.stats(data$worst_symmetry[data$diagnosis=="M"])
#By class for worst symmetry, there are 3 outliers for B and 7 outliers for M.

boxplot.stats(data$worst_fractal_dimension)
data[data$worst_fractal_dimension < 0.05504 | data$worst_fractal_dimension > 0.12240, c(2,32)]
table(data[data$worst_fractal_dimension < 0.05504 | data$worst_fractal_dimension > 0.12240, 2])
#Worst fractal dimension has 24 outliers (6B, 18M).

boxplot.stats(data$worst_fractal_dimension[data$diagnosis=="B"])
boxplot.stats(data$worst_fractal_dimension[data$diagnosis=="M"])
#By class for worst fractal dimension, there are 13 outliers for B and 4 outliers for M.

#All outliers are included in our analysis and not removed as such values are expected in this medical context.

#########################################################################################
#           Analyzing the Distribution of Numeric Attributes (Normal or Not Normal)     #
#########################################################################################

library(ggplot2)

##Check for normality using histograms.
par(mfrow=c(1,1))

qplot(x=data$mean_radius, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Radius", x="Mean Radius (micrometres)", y="Count")
qplot(x=data$mean_texture, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Texture", x="Mean Texture", y="Count")
qplot(x=data$mean_perimeter, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Perimeter", x="Mean Perimeter (micrometres)", y="Count")
qplot(x=data$mean_area, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Area", x="Mean Area (square micrometres)", y="Count")
qplot(x=data$mean_smoothness, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Smoothness", x="Mean Smoothness", y="Count")
qplot(x=data$mean_compactness, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Compactness", x="Mean Compactness", y="Count")
qplot(x=data$mean_concavity, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Concavity", x="Mean Concavity", y="Count")
qplot(x=data$mean_concave_points, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Concave Points", x="Mean Concave Points", y="Count")
qplot(x=data$mean_symmetry, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Symmetry", x="Mean Symmetry", y="Count")
qplot(x=data$mean_fractal_dimension, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Mean Fractal Dimension", x="Mean Fractal Dimension", y="Count")
qplot(x=data$radius_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Radius Standard Error", x="Radius Standard Error (micrometres)", y="Count")
qplot(x=data$texture_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Texture Standard Error", x="Texture Standard Error", y="Count")
qplot(x=data$perimeter_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Perimeter Standard Error", x="Perimeter Standard Error (micrometres)", y="Count")
qplot(x=data$area_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Area Standard Error", x="Area Standard Error (square micrometres)", y="Count")
qplot(x=data$smoothness_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Smoothness Standard Error", x="Smoothness Standard Error", y="Count")
qplot(x=data$compactness_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Compactness Standard Error", x="Compactness Standard Error", y="Count")
qplot(x=data$concavity_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Concavity Standard Error", x="Concavity Standard Error", y="Count")
qplot(x=data$concave_points_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Concave Points Standard Error", x="Concave Points Standard Error", y="Count")
qplot(x=data$symmetry_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Symmetry Standard Error", x="Symmetry Standard Error", y="Count")
qplot(x=data$fractal_dimension_se, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Fractal Dimension Standard Error", x="Fractal Dimension Standard Error", y="Count")
qplot(x=data$worst_radius, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Radius", x="Worst Radius (micrometres)", y="Count")
qplot(x=data$worst_texture, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Texture", x="Worst Texture", y="Count")
qplot(x=data$worst_perimeter, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Perimeter", x="Worst Perimeter (micrometres)", y="Count")
qplot(x=data$worst_area, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Area", x="Worst Area (square micrometres)", y="Count")
qplot(x=data$worst_smoothness, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Smoothness", x="Worst Smoothness", y="Count")
qplot(x=data$worst_compactness, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Compactness", x="Worst Compactness", y="Count")
qplot(x=data$worst_concavity, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Concavity", x="Worst Concavity", y="Count")
qplot(x=data$worst_concave_points, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Concave Points", x="Worst Concave Points", y="Count")
qplot(x=data$worst_symmetry, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Symmetry", x="Worst Symmetry", y="Count")
qplot(x=data$worst_fractal_dimension, fill=..count.., geom="histogram") + scale_fill_gradient(low="blue", high="red") + labs(title="Histogram for Worst Fractal Dimension", x="Worst Fractal Dimension", y="Count")

##Check for normality using histograms by class, B and M.
par(mfrow=c(1,1))

ggplot(data=data, aes(data$mean_radius, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Radius", x="Mean Radius (micrometres)", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_texture, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Texture", x="Mean Texture", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_perimeter, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Perimeter", x="Mean Perimeter (micrometres)", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_area, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Area", x="Mean Area (square micrometres)", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_smoothness, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Smoothness", x="Mean Smoothness", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_compactness, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Compactness", x="Mean Compactness", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_concavity, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Concavity", x="Mean Concavity", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_concave_points, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Concave Points", x="Mean Concave Points", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_symmetry, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Symmetry", x="Mean Symmetry", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_fractal_dimension, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Mean Fractal Dimension", x="Mean Fractal Dimension", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$radius_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Radius Standard Error", x="Radius Standard Error (micrometres)", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$texture_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Texture Standard Error", x="Texture Standard Error", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$perimeter_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Perimeter Standard Error", x="Perimeter Standard Error (micrometres)", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$area_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Area Standard Error", x="Area Standard Error (square micrometres)", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$smoothness_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Smoothness Standard Error", x="Smoothness Standard Error", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$compactness_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Compactness Standard Error", x="Compactness Standard Error", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$concavity_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Concavity Standard Error", x="Concavity Standard Error", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$concave_points_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Concave Points Standard Error", x="Concave Points Standard Error", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$symmetry_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Symmetry Standard Error", x="Symmetry Standard Error", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$fractal_dimension_se, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Fractal Dimension Standard Error", x="Fractal Dimension Standard Error", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_radius, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Radius", x="Worst Radius (micrometres)", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_texture, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Texture", x="Worst Texture", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_perimeter, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Perimeter", x="Worst Perimeter (micrometres)", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_area, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Area", x="Worst Area (square micrometres)", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_smoothness, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Smoothness", x="Worst Smoothness", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_compactness, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Compactness", x="Worst Compactness", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_concavity, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Concavity", x="Worst Concavity", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_concave_points, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Concave Points", x="Worst Concave Points", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_symmetry, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Symmetry", x="Worst Symmetry", y="Count", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_fractal_dimension, fill=data$diagnosis, color=data$diagnosis)) + geom_histogram(alpha=0.5, position="identity") + labs(title="Histogram for Worst Fractal Dimension", x="Worst Fractal Dimension", y="Count", fill="Diagnosis", color="Diagnosis")

##Check for normality using density plots by class, B and M.
par(mfrow=c(1,1))

ggplot(data=data, aes(data$mean_radius, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Radius", x="Mean Radius (micrometres)", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$mean_texture, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Texture", x="Mean Texture", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$mean_perimeter, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Perimeter", x="Mean Perimeter (micrometres)", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$mean_area, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Area", x="Mean Area (square micrometres)", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$mean_smoothness, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Smoothness", x="Mean Smoothness", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$mean_compactness, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Compactness", x="Mean Compactness", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$mean_concavity, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Concavity", x="Mean Concavity", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$mean_concave_points, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Concave Points", x="Mean Concave Points", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$mean_symmetry, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Symmetry", x="Mean Symmetry", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$mean_fractal_dimension, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Mean Fractal Dimension", x="Mean Fractal Dimension", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$radius_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Radius Standard Error", x="Radius Standard Error (micrometres)", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$texture_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Texture Standard Error", x="Texture Standard Error", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$perimeter_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Perimeter Standard Error", x="Perimeter Standard Error (micrometres)", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$area_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Area Standard Error", x="Area Standard Error (square micrometres)", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$smoothness_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Smoothness Standard Error", x="Smoothness Standard Error", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$compactness_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Compactness Standard Error", x="Compactness Standard Error", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$concavity_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Concavity Standard Error", x="Concavity Standard Error", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$concave_points_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Concave Points Standard Error", x="Concave Points Standard Error", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$symmetry_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Symmetry Standard Error", x="Symmetry Standard Error", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$fractal_dimension_se, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Fractal Dimension Standard Error", x="Fractal Dimension Standard Error", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_radius, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Radius", x="Worst Radius (micrometres)", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_texture, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Texture", x="Worst Texture", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_perimeter, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Perimeter", x="Worst Perimeter (micrometres)", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_area, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Area", x="Worst Area (square micrometres)", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_smoothness, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Smoothness", x="Worst Smoothness", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_compactness, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Compactness", x="Worst Compactness", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_concavity, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Concavity", x="Worst Concavity", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_concave_points, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Concave Points", x="Worst Concave Points", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_symmetry, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Symmetry", x="Worst Symmetry", y="Density", fill="Diagnosis")
ggplot(data=data, aes(data$worst_fractal_dimension, fill=data$diagnosis)) + geom_density(alpha=0.4) + labs(title="Density Plot for Worst Fractal Dimension", x="Worst Fractal Dimension", y="Density", fill="Diagnosis")

##Check for normality using histograms and density plots by class, B and M.
par(mfrow=c(1,1))

ggplot(data=data, aes(data$mean_radius, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Radius", x="Mean Radius (micrometres)", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_texture, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Texture", x="Mean Texture", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_perimeter, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Perimeter", x="Mean Perimeter (micrometres)", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_area, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Area", x="Mean Area (square micrometres)", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_smoothness, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Smoothness", x="Mean Smoothness", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_compactness, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Compactness", x="Mean Compactness", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_concavity, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Concavity", x="Mean Concavity", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_concave_points, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Concave Points", x="Mean Concave Points", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_symmetry, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Symmetry", x="Mean Symmetry", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$mean_fractal_dimension, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Mean Fractal Dimension", x="Mean Fractal Dimension", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$radius_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Radius Standard Error", x="Radius Standard Error (micrometres)", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$texture_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Texture Standard Error", x="Texture Standard Error", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$perimeter_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Perimeter Standard Error", x="Perimeter Standard Error (micrometres)", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$area_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Area Standard Error", x="Area Standard Error (square micrometres)", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$smoothness_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Smoothness Standard Error", x="Smoothness Standard Error", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$compactness_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Compactness Standard Error", x="Compactness Standard Error", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$concavity_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Concavity Standard Error", x="Concavity Standard Error", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$concave_points_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Concave Points Standard Error", x="Concave Points Standard Error", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$symmetry_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Symmetry Standard Error", x="Symmetry Standard Error", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$fractal_dimension_se, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Fractal Dimension Standard Error", x="Fractal Dimension Standard Error", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_radius, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Radius", x="Worst Radius (micrometres)", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_texture, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Texture", x="Worst Texture", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_perimeter, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Perimeter", x="Worst Perimeter (micrometres)", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_area, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Area", x="Worst Area (square micrometres)", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_smoothness, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Smoothness", x="Worst Smoothness", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_compactness, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Compactness", x="Worst Compactness", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_concavity, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Concavity", x="Worst Concavity", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_concave_points, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Concave Points", x="Worst Concave Points", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_symmetry, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Symmetry", x="Worst Symmetry", y="Density", fill="Diagnosis", color="Diagnosis")
ggplot(data=data, aes(data$worst_fractal_dimension, color=data$diagnosis, fill=data$diagnosis)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2) + labs(title="Histogram and Density Plot for Worst Fractal Dimension", x="Worst Fractal Dimension", y="Density", fill="Diagnosis", color="Diagnosis")

##Check for normality using Shapiro test.
#If the p-value is above 0.05, then the data is normal (normally distributed).
#Null hypothesis is "sample distribution is normal". Alternative hypothesis is "sample distribution is not normal" and test is significant.
shapiro.test(data$mean_radius) 
#p-value = 3.106e-14
shapiro.test(data$mean_texture) 
#p-value = 7.284e-08
shapiro.test(data$mean_perimeter) 
#p-value = 7.011e-15
shapiro.test(data$mean_area) 
#p-value < 2.2e-16
shapiro.test(data$mean_smoothness) 
#p-value = 8.601e-05
shapiro.test(data$mean_compactness) 
#p-value < 2.2e-16
shapiro.test(data$mean_concavity) 
#p-value < 2.2e-16
shapiro.test(data$mean_concave_points) 
#p-value < 2.2e-16
shapiro.test(data$mean_symmetry) 
#p-value = 7.885e-09
shapiro.test(data$mean_fractal_dimension) 
#p-value < 2.2e-16
shapiro.test(data$radius_se) 
#p-value < 2.2e-16
shapiro.test(data$texture_se) 
#p-value < 2.2e-16
shapiro.test(data$perimeter_se) 
#p-value < 2.2e-16
shapiro.test(data$area_se) 
#p-value < 2.2e-16
shapiro.test(data$smoothness_se) 
#p-value < 2.2e-16
shapiro.test(data$compactness_se) 
#p-value < 2.2e-16
shapiro.test(data$concavity_se) 
#p-value < 2.2e-16
shapiro.test(data$concave_points_se) 
#p-value < 2.2e-16
shapiro.test(data$symmetry_se) 
#p-value < 2.2e-16
shapiro.test(data$fractal_dimension_se) 
#p-value < 2.2e-16
shapiro.test(data$worst_radius) 
#p-value < 2.2e-16
shapiro.test(data$worst_texture) 
#p-value = 2.564e-06
shapiro.test(data$worst_perimeter) 
#p-value < 2.2e-16
shapiro.test(data$worst_area) 
#p-value < 2.2e-16
shapiro.test(data$worst_smoothness) 
#p-value = 0.0002097
shapiro.test(data$worst_compactness) 
#p-value < 2.2e-16
shapiro.test(data$worst_concavity) 
#p-value < 2.2e-16
shapiro.test(data$worst_concave_points)
#p-value = 1.985e-10
shapiro.test(data$worst_symmetry) 
#p-value < 2.2e-16
shapiro.test(data$worst_fractal_dimension) 
#p-value < 2.2e-16

#Since all p-values are less than 0.05, we reject the null hypothesis that the data is normally distributed.
#Data are not normally distributed therefore non-parametric tests are applied.

#########################################################################################
#                  Determining Correlations Between Numeric Attributes                  #
#                   Spearman correlations (not normally distributed)                    #
#########################################################################################

##Subset for numeric attributes.
numeric_data <- Filter(is.numeric, data)

##Spearman correlations.
spearman_cor <- cor(numeric_data, method = "spearman")
spearman_cor

##Plot for Spearman correlations. 
library(corrplot)
corrplot(spearman_cor, method = "circle", tl.srt=45, sig.level = 0.05, title = "Spearman Correlation Matrix")

##Plot for Spearman correlations with correlation coefficients.
corrplot(spearman_cor, method = "number", tl.srt=45, sig.level = 0.05, title = "Spearman Correlation Matrix")

##Spearman correlation coefficients and their p-values.
library(Hmisc)
spearman_cor_matrix <- rcorr(as.matrix(numeric_data), type = "spearman")

##Correlation coefficients.
spearman_cor_matrix$r 

##Correlation p-values.
spearman_cor_matrix$P 

##Matrix with both correlation coefficients and p-values.
##Flatten function.
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

##Matrix with both correlation coefficients and p-values.
correlation_matrix<-rcorr(as.matrix(numeric_data[,1:7]), type = "spearman")
flattenCorrMatrix(spearman_cor_matrix$r, spearman_cor_matrix$P) 
correlation_table <- flattenCorrMatrix(spearman_cor_matrix$r, spearman_cor_matrix$P)
correlation_df <- as.data.frame(correlation_table)

##Correlation coefficients >= 0.800000000 or <= -0.800000000 and p-values < 0.050000 (strong positive/negative linear correlations).
sig <- correlation_df[which(correlation_df$cor >= 0.800000000 | correlation_df$cor <= -0.800000000 & correlation_df$p < 0.050000), ]

##Correlation coefficients and p-values for Benign.
##Subset for B.
b_data <- data[data$diagnosis=="B", ]
b_data
numeric_data_b <- Filter(is.numeric, b_data)

##Spearman correlation coefficients and p-values for B.
spearman_cor_matrix_b <- rcorr(as.matrix(numeric_data_b), type = "spearman")

##Correlation coefficients for B.
spearman_cor_matrix_b$r

##Correlation p-values for B.
spearman_cor_matrix_b$P

##Correlation coefficients and p-values for Malignant.
##Subset for M.
m_data <- data[data$diagnosis=="M", ]
m_data
numeric_data_m <- Filter(is.numeric, m_data)

##Spearman correlation coefficients and p-values for M.
spearman_cor_matrix_m <- rcorr(as.matrix(numeric_data_m), type = "spearman")

##Correlation coefficients for M.
spearman_cor_matrix_m$r

##Correlation p-values for M.
spearman_cor_matrix_m$P

##Scatterplots to visualize correlations between numeric attributes by class, B and M.
library(ggplot2)
library(DEGreport)

par(mfrow=c(1,1))

ggplot(data, aes(x=data$mean_radius, y=data$mean_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Mean Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$mean_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Mean Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$mean_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Mean Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$mean_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Mean Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$mean_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Mean Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$mean_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Mean Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$mean_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Mean Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$mean_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Mean Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$mean_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Mean Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Symmtery", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_radius, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Radius", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearly correlated mean_radius and mean_perimeter, mean_radius and mean_area, mean_radius and worst_radius, mean_radius and worst_perimeter, and mean_radius and worst_area (pval=0)

ggplot(data, aes(x=data$mean_texture, y=data$mean_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Mean Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$mean_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Mean Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$mean_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Mean Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$mean_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Mean Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$mean_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Mean Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$mean_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Mean Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$mean_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Mean Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$mean_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Mean Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_texture, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Texture", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearly correlated mean_texture and worst_texture (pval=0)

ggplot(data, aes(x=data$mean_perimeter, y=data$mean_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Mean Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$mean_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Mean Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$mean_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Mean Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$mean_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Mean Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$mean_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Mean Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$mean_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Mean Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$mean_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Mean Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_perimeter, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Perimeter", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearly correlated mean_perimeter and mean_area, mean_perimeter and worst_radius, mean_perimeter and worst_perimeter, and mean_perimeter and worst_area (pval=0)

ggplot(data, aes(x=data$mean_area, y=data$mean_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Mean Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$mean_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Mean Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$mean_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Mean Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$mean_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Mean Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$mean_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Mean Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$mean_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Mean Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_area, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Area", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearly correlated mean_area and worst_radius, mean_area and worst_perimeter, and mean_area and worst_area (pval=0)

ggplot(data, aes(x=data$mean_smoothness, y=data$mean_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Mean Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$mean_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Mean Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$mean_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Mean Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$mean_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Mean Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$mean_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Mean Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_smoothness, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Smoothness", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearly correlated mean_smoothness and worst_smoothness (only B, pval=0)

ggplot(data, aes(x=data$mean_compactness, y=data$mean_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Mean Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$mean_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Mean Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$mean_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Mean Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$mean_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Mean Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_compactness, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Compactness", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearly correlated mean_compactness and mean_concavity, and mean_compactness and worst_compactness (pval=0)

ggplot(data, aes(x=data$mean_concavity, y=data$mean_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Mean Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$mean_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Mean Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$mean_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Mean Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concavity, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concavity", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearly correlated mean_concavity and mean_concave_points (pval=0), mean_concavity and worst_compactness (only B, pval=0), and mean_concavity and worst_concavity (only B, pval=0)

ggplot(data, aes(x=data$mean_concave_points, y=data$mean_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Mean Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$mean_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Mean Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_concave_points, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Concave Points", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated mean_concave_points and worst_concave_points (only B, pval=0)

ggplot(data, aes(x=data$mean_symmetry, y=data$mean_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Mean Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_symmetry, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Symmetry", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$mean_fractal_dimension, y=data$radius_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Radius SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$mean_fractal_dimension, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Mean Fractal Dimension", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated mean_fractal_dimension and worst_fractal_dimension (only M, pval=0)

ggplot(data, aes(x=data$radius_se, y=data$texture_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Texture SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$radius_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Radius SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated radius_se and perimeter_se, and radius_se and area_se (pval=0)

ggplot(data, aes(x=data$texture_se, y=data$perimeter_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Perimeter SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$texture_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Texture SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$perimeter_se, y=data$area_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Area SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$perimeter_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Perimeter SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated perimeter_se and area_se (pval=0)

ggplot(data, aes(x=data$area_se, y=data$smoothness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Smoothness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$area_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Area SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated area_se and worst_radius, area_se and worst_perimeter, and area_se and worst_area (all only M, pval=0)

ggplot(data, aes(x=data$smoothness_se, y=data$compactness_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Compactness SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$smoothness_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Smoothness SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$compactness_se, y=data$concavity_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Concavity SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$compactness_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Compactness SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated compactness_se and concavity_se (pval=0), compactness_se and fractal_dimension_se (only M, pval=0), and compactness_se and worst_compactness (only B, pval=0)

ggplot(data, aes(x=data$concavity_se, y=data$concave_points_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Concave Points SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concavity_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concavity SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated concavity_se and worst_concavity (only B, pval=0)

ggplot(data, aes(x=data$concave_points_se, y=data$symmetry_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Symmetry SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$concave_points_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Concave Points SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$symmetry_se, y=data$fractal_dimension_se, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Fractal Dimension SE", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$symmetry_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Symmetry SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_radius, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Radius", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$fractal_dimension_se, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Fractal Dimension SE", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$worst_radius, y=data$worst_texture, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Radius", y="Worst Texture", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_radius, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Radius", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_radius, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Radius", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_radius, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Radius", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_radius, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Radius", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_radius, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Radius", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_radius, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Radius", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_radius, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Radius", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_radius, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Radius", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated worst_radius and worst_perimeter, and worst_radius and worst_area (pval=0)

ggplot(data, aes(x=data$worst_texture, y=data$worst_perimeter, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Texture", y="Worst Perimeter", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_texture, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Texture", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_texture, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Texture", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_texture, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Texture", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_texture, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Texture", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_texture, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Texture", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_texture, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Texture", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_texture, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Texture", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$worst_perimeter, y=data$worst_area, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Perimeter", y="Worst Area", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_perimeter, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Perimeter", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_perimeter, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Perimeter", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_perimeter, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Perimeter", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_perimeter, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Perimeter", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_perimeter, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Perimeter", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_perimeter, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Perimeter", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated worst_perimeter and worst_area (pval=0)

ggplot(data, aes(x=data$worst_area, y=data$worst_smoothness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Area", y="Worst Smoothness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_area, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Area", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_area, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Area", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_area, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Area", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_area, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Area", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_area, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Area", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$worst_smoothness, y=data$worst_compactness, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Smoothness", y="Worst Compactness", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_smoothness, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Smoothness", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_smoothness, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Smoothness", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_smoothness, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Smoothness", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_smoothness, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Smoothness", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$worst_compactness, y=data$worst_concavity, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Compactness", y="Worst Concavity", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_compactness, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Compactness", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_compactness, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Compactness", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_compactness, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Compactness", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearily correlated worst_compactness and worst_concavity (pval=0), and worst_compactness and worst_fractal_dimension (only M, pval=0)

ggplot(data, aes(x=data$worst_concavity, y=data$worst_concave_points, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Concavity", y="Worst Concave Points", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_concavity, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Compactness", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_concavity, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Compactness", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")
#Linearly correlated worst_concavity and worst_concave_points (only B, pval=0)

ggplot(data, aes(x=data$worst_concave_points, y=data$worst_symmetry, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Concave points", y="Worst Symmetry", color="Diagnosis") + geom_cor(method="spearman")
ggplot(data, aes(x=data$worst_concave_points, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Concave Points", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")

ggplot(data, aes(x=data$worst_symmetry, y=data$worst_fractal_dimension, color=data$diagnosis)) + geom_point(alpha=0.5) + labs(title="", x="Worst Symmetry", y="Worst Fractal Dimension", color="Diagnosis") + geom_cor(method="spearman")


#########################################################################################################
#                                        Transforming Attributes                                        #
#                       (i.e. discretizing numeric attributes to categorical attributes)                #
#########################################################################################################

##Discretize continous attributes using quantile cuts.
quantile_df <- as.data.frame(sapply(data[,3:32], function (x) {
  qx <- quantile(x)
  cut(x, qx, include.lowest = TRUE,
      labels = 1:4)
}))

new_data <- as.data.frame(cbind(data[,1:2], quantile_df))
new_data 
#New data frame with discretized attributes for our analysis.

##Count frequency of diagnosis variable (B and M) across the 4 intervals for numeric attributes (1 - 1st quartile, 2 - 2nd quartile, 3 - 3rd quartile, 4 - 4th quartile)
table(new_data$mean_radius, new_data$diagnosis)
table(new_data$mean_texture, new_data$diagnosis)
table(new_data$mean_perimeter, new_data$diagnosis)
table(new_data$mean_area, new_data$diagnosis)
table(new_data$mean_smoothness, new_data$diagnosis)
table(new_data$mean_compactness, new_data$diagnosis)
table(new_data$mean_concavity, new_data$diagnosis)
table(new_data$mean_concave_points, new_data$diagnosis)
table(new_data$mean_symmetry, new_data$diagnosis)
table(new_data$mean_fractal_dimension, new_data$diagnosis)
table(new_data$radius_se, new_data$diagnosis)
table(new_data$texture_se, new_data$diagnosis)
table(new_data$perimeter_se, new_data$diagnosis)
table(new_data$area_se, new_data$diagnosis)
table(new_data$smoothness_se, new_data$diagnosis)
table(new_data$compactness_se, new_data$diagnosis)
table(new_data$concavity_se, new_data$diagnosis)
table(new_data$concave_points_se, new_data$diagnosis)
table(new_data$symmetry_se, new_data$diagnosis)
table(new_data$fractal_dimension_se, new_data$diagnosis)
table(new_data$worst_radius, new_data$diagnosis)
table(new_data$worst_texture, new_data$diagnosis)
table(new_data$worst_perimeter, new_data$diagnosis)
table(new_data$worst_area, new_data$diagnosis)
table(new_data$worst_smoothness, new_data$diagnosis)
table(new_data$worst_compactness, new_data$diagnosis)
table(new_data$worst_concavity, new_data$diagnosis)
table(new_data$worst_concave_points, new_data$diagnosis)
table(new_data$worst_symmetry, new_data$diagnosis)
table(new_data$worst_fractal_dimension, new_data$diagnosis)

#########################################################################################################
#                                             Information Gain                                          #
#                                            (Feature Selection)                                        #
#########################################################################################################

library(FSelector)

##Set seed for reproducibility
set.seed(100)

infogain_weights <-information.gain(diagnosis~., new_data)
#names(infogain_weights)
#str(infogain_weights)

infogain_df <- data.frame(cbind(rownames(infogain_weights)), infogain_weights$attr_importance)
infogain_df

names(infogain_df) <- c("attribute", "attr_importance")
str(infogain_df)

infogain_df2 <- infogain_df[order(infogain_df$attr_importance, decreasing = TRUE),]
infogain_df2

subset_25 <- cutoff.k(infogain_weights, 25)
subset_25

#########################################################################################################
#                                            Apriori algorithm                                          #
#                                        (Association Rule Mining)                                      #
#########################################################################################################

library(arules)
library(arulesViz)

##New data frame without id attribute for association rules.
new_data_without_id <- new_data[,2:32]
new_data_without_id

##Association rules for Benign.
b_rules <- apriori(data=new_data_without_id, parameter=list (supp=0.20,conf = 0.80, maxlen = 10, minlen = 1), appearance = list (rhs="diagnosis=B"))
b_rules
summary(b_rules)
inspect(b_rules[1:25])
inspect(b_rules) 
#There are a total of 108 rules.

##Order by decreasing confidence values for B.
b_rules2 <- b_rules[order(b_rules@ quality$confidence, decreasing = TRUE),]
b_rules2
inspect(b_rules2)

##Association rules for Malignant.
m_rules <- apriori(data=new_data_without_id, parameter=list (supp=0.20,conf = 0.80, maxlen = 10, minlen = 1), appearance = list (rhs="diagnosis=M"))
m_rules
summary(m_rules)
inspect(m_rules[1:25])
inspect(m_rules)
#There are a total of 104 rules.

##Order by decreasing confidence values for M.
m_rules2 <- m_rules[order(m_rules@ quality$confidence, decreasing = TRUE),]
m_rules2
inspect(m_rules2)


#########################################################################################################
#                                        BN Structure Learning                                          #
#                       (i.e. score-based learning, hill-climbing algorithm)                            #
#########################################################################################################

library(bnlearn)
str(new_data)

##Hill-Climbing score-based method for BN
bn.hc <- hc(new_data, score = "aic")
bn.hc #dag

##Plot the BN.
library(Rgraphviz)
graphviz.plot(bn.hc) #graph of bn

library(RBGL)
library(qgraph)
qgraph(bn.hc) #graph of bn using qgraph
class(bn.hc) #bn

modelstring(bn.hc) #model string of bn

str(bn.hc) #structure of the learned bn with nodes (attributes) and their mb, nbr, parents and children
root.nodes(bn.hc) #having no parents (2 nodes)
#These are id and mean_smoothness.
leaf.nodes(bn.hc) #having no children (9 nodes)
#These are id, mean_area, mean_fractal_dimension, texture_se, perimeter_se, concave_points_se, worst_perimeter, worst_smoothness, worst_symmetry.
bn.hc$arcs #There are 54 directed arcs.

##Looking into the diagnois node (Benign and Malignant)
bn.hc$nodes$diagnosis$mb
# Diagnosis node Markov blanket consists of 7 nodes mean_texture, mean_smoothness, compactness_se, worst_radius
#worst_smoothness, worst_concave_points and worst fractal dimension.
bn.hc$nodes$diagnosis$parents #largest radius from centre of nucleus to the nuclear envelope, highest number of concave points on nuclear envelope
#Diagnosis node has parents worst_radius and worst_concave_points (conditionally dependent on)
bn.hc$nodes$diagnosis$children
#Diagnosis node has children mean_texture and worst_smoothness.
bn.hc$nodes$diagnosis$nbr #consists of parents and children
#Diagnosis node nrb consists of mean_texture, worst_radius, worst_smoothness, and worst_concave_points.

bn.hc$arcs
#There are 54 directed arcs.

arc.strength(bn.hc, new_data) #measure of confidence/strength of arc 
#14            worst_radius               diagnosis  -59.2022991
#38    worst_concave_points               diagnosis  -45.3212595


#########################################################################################################
#                                           Parameter Learning                                          #
#                            (i.e. Bayesian Posterior Estimates for Discrete Data)                      #
#########################################################################################################

##Fit the parameters.
fitted = bn.fit(hc(new_data), new_data, method = "bayes", iss = 20)
bn.net(fitted)
coef(fitted)
str(fitted)
class(fitted) #bn.fit
fitted$diagnosis

##Plot the conditional probabilities for diagnosis.
fitted$diagnosis #conditional probability table (worst_concave_points low, medium, high, and very high, worst_radius, low, medium, high, and very high)
bn.fit.barchart(fitted$diagnosis) #green worst_concave_points, orange worst_radius
bn.fit.dotplot(fitted$diagnosis) #green worst_concave_points, orange worst_radius

##Subgraph of Markov Blanket.
MarkovBlanket.Graph <- subgraph(bn.hc, nodes = c("diagnosis", bn.hc$nodes$diagnosis$mb))
plot(MarkovBlanket.Graph)
graphviz.plot(MarkovBlanket.Graph)
qgraph(MarkovBlanket.Graph)

##Susbet data with only mb.
new_data
new_data_2 <- new_data[,c(2,4,7,18,23,27,30,32)]

####Fit the parameters for Markov Blanket subgraph.
fitted2 = bn.fit(MarkovBlanket.Graph, new_data_2, method = "bayes", iss = 20)
coef(fitted2)
fitted2$worst_fractal_dimension

##Plot the conditional probabilities for Markov Blanket subgraph.
bn.fit.barchart(fitted2$diagnosis)
barchart(fitted2$diagnosis, new_data_2)


#########################################################################################################
#                                             Checking for FPs                                          #
#                                             (diagnosis$mb)                                            #
#########################################################################################################

bn.hc$nodes$diagnosis$mb #mb consists of parents, children, and spouses
#7 nodes

##Check for FPs
bn.hc$nodes$mean_texture$mb
#{diagnosis, compactness_se, and worst_texture}
#3 nodes
bn.hc$nodes$mean_smoothness$mb
#{diagnosis, mean_compactness, mean_concavity, mean_concave_points, worst_smoothness, and worst_fractal_dimension}
#6 nodes
bn.hc$nodes$compactness_se$mb
#{diagnosis, mean_texture, mean_compactness, radius_se, perimeter_se, smoothness_se, concavity_se, symmetry_se, and fractal_dimension_se}
#9 nodes
bn.hc$nodes$worst_radius$mb
#{diagnosis, mean_perimeter, area_se, worst_perimeter, worst_area, and worst_concave_points}
#6 nodes
bn.hc$nodes$worst_smoothness$mb
#{diagnosis, mean_smoothness, and worst_fractal_dimension}
#3 nodes
bn.hc$nodes$worst_concave_points$mb
#{diagnosis, mean_concave_points, worst_radius, worst_perimeter, and worst_concavity}
#5 nodes
bn.hc$nodes$worst_fractal_dimension$mb
#{diagnosis, mean_smoothness, mean_fractal_dimension, fractal_dimension_se, worst_area, worst_smoothness, and worst_compactness}
#7 nodes
#No false positives present (diagnosis in all 7 mb nodes).


#########################################################################################################
#                                        BN Structure Learning Renamed                                  #
#                       (i.e. score-based learning, hill-climbing algorithm)                            #
#########################################################################################################

#Renaming nodes to numbers for easy reading of plot.
new_data_1 <- new_data
col_names_bn <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32")
col_names_bn
names(new_data_1) <- col_names_bn
str(new_data_1)

library(bnlearn)
str(new_data_1)

##Hill-Climbing score-based method for BN.
bn.hc_1 <- hc(new_data_1, score = "aic")
bn.hc_1

##Plot the BN.
library(Rgraphviz)
graphviz.plot(bn.hc_1)

library(qgraph)
qgraph(bn.hc_1) #graph of bn using qgraph

modelstring(bn.hc_1)
str(bn.hc_1)

class(bn.hc_1)
names(bn.hc_1)

##Subgraph of Markov Blanket.
MarkovBlanket.Graph_1 <- subgraph(bn.hc_1, nodes = c("2", bn.hc_1$nodes$`2`$mb))
plot(MarkovBlanket.Graph_1)
graphviz.plot(MarkovBlanket.Graph_1)
qgraph(MarkovBlanket.Graph_1)

##Fit the parameters (Bayesian Posterior estimates for discrete data).
fitted_renamed = bn.fit(hc(new_data_1), new_data_1, method = "bayes", iss = 20)
bn.net(fitted_renamed)
coef(fitted_renamed)
str(fitted_renamed)

graphviz.plot(fitted_renamed)

#########################################################################################################
#                                       Estimating Bayesian Network                                     #
#                                            (i.e. PC algorithm)                                        #
#########################################################################################################

##PC algorithm from pcalg package

library(pcalg)

##Change data frame to a matrix for PC algorithm.
class(new_data)
new_data_matrix <- as.matrix(new_data)
class(new_data_matrix)

##Renaming id and diagnosis values to include 0 to etc and and rest of column levels to 0, 1, 2 and 3 (still 4 levels).
new_data_matrix_2 <- new_data_matrix
rf <- as.data.frame(new_data_matrix_2)
rf$id <- as.numeric(rf$id) -1
rf$diagnosis <- as.numeric(rf$diagnosis) -1

for (i in 3:32) {
  rf[,i] <- as.numeric(rf[,i]) -1
}

rf_matrix <- as.matrix(rf)
class(rf_matrix)


##For alpha = 0.05:

##Skeleton
suffStat_rf_matrix <- list(dm = rf_matrix, nlev = c(569, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), adaptDF = FALSE)
pc.fit_rf_matrix_0.05 <- skeleton(suffStat_rf_matrix, 
                                  ## independence test: G^2 statistic
                                  indepTest = disCItest, p = 32, alpha = 0.05)
plot(pc.fit_rf_matrix_0.05)
str(pc.fit_rf_matrix_0.05)
class(pc.fit_rf_matrix_0.05) #pcAlgo

##PC algorithm
pc.fit_rf_matrix_pc_0.05 <- pc(suffStat_rf_matrix, indepTest = disCItest, p = 32, alpha = 0.05)
plot(pc.fit_rf_matrix_pc_0.05) #undirected and bidirected edges have same meanings (pg. 5 pcalg Swiss article, this is CPDAG)
#2**3(pg.17, Swiss article), therefore 8 DAGs, response variable is b/m
summary(pc.fit_rf_matrix_pc_0.05)
pc.fit_rf_matrix_pc_0.05@ pMax #p-values

str(pc.fit_rf_matrix_pc_0.05)
names(pc.fit_rf_matrix_pc_0.05)
class(pc.fit_rf_matrix_pc_0.05) #pcAlgo


#########################################################################################################
#                                      Inference in Static Bayesian Networks                            #
#                                                                                                       #
#########################################################################################################

##Inference in Static Bayesian Network, Bayesian parameter estimates
##Perform parameter learning 
val.str = paste("[id][mean_smoothness][mean_compactness|mean_smoothness][mean_concavity|mean_compactness][mean_concave_points|mean_smoothness:mean_concavity][worst_concavity|mean_concavity][concavity_se|mean_concavity:mean_concave_points][worst_compactness|mean_compactness:worst_concavity][worst_concave_points|mean_concave_points:worst_concavity][compactness_se|mean_compactness:concavity_se][concave_points_se|mean_concave_points:concavity_se][fractal_dimension_se|compactness_se][smoothness_se|fractal_dimension_se:worst_compactness][worst_fractal_dimension|fractal_dimension_se:worst_compactness][symmetry_se|smoothness_se:compactness_se][mean_symmetry|mean_compactness:symmetry_se][radius_se|mean_concave_points:symmetry_se][perimeter_se|radius_se:compactness_se][area_se|mean_concave_points:radius_se][worst_symmetry|mean_symmetry:worst_compactness][mean_radius|area_se:smoothness_se][mean_perimeter|mean_radius:worst_compactness][mean_area|mean_radius][worst_radius|mean_perimeter:area_se][diagnosis|worst_radius:worst_concave_points][worst_perimeter|worst_radius:worst_concave_points][worst_area|worst_radius][mean_texture|diagnosis:compactness_se][mean_fractal_dimension|worst_area:worst_fractal_dimension][worst_smoothness|diagnosis:mean_smoothness:worst_fractal_dimension][worst_texture|mean_texture][texture_se|symmetry_se:worst_texture]")
val = model2network(val.str)

for (i in names(new_data[,3:32]))
  levels(new_data[, i]) = c("Low", "Medium", "High", "Very High")
fitted = bn.fit(val, new_data, method = "bayes")
fitted
#str(fitted)

#########################################################################################################
#                                               Inference                                               #
#                                     (Conditional Probabiliy Queries)                                  #
#########################################################################################################

##Conditional Probabiliy Queries 

library(gRain)
library(Rgraphviz)

fitted.grain = as.grain(fitted)
#Loading required namespace: gRain
fitted.grain
fitted.grain$universe$nodes
fitted.grain$cptlist$diagnosis
str(fitted.grain)
class(fitted.grain)
class(fitted)

#Perform conditional probability queries.
#cpquery(fitted, event, evidence, cluster = NULL, method = "ls", ...,debug = FALSE)

#Examples
cpquery(fitted2, (diagnosis=="B"), (worst_radius==1))
cpquery(fitted2, (diagnosis=="M"), (mean_smoothness==4)) #class imbalance
cpquery(fitted2, (worst_radius==1 & worst_concave_points==1), (diagnosis=="B"))

cpquery(fitted2, diagnosis == "B", evidence = TRUE) #prior
#61.4%
cpquery(fitted2, diagnosis == "B", evidence = list(worst_radius = "3",worst_concave_points = "2"), method = "lw", n = 100)
#90%
cpquery(fitted2, diagnosis == "M", evidence = TRUE) #prior
#38.2%
cpquery(fitted2, diagnosis == "M", evidence = list(worst_radius = "3",worst_concave_points = "1"), method = "lw", n = 100)
#10%
cpquery(fitted2, diagnosis == "B", evidence = list(mean_smoothness = "3"), method = "lw", n = 100)
#62%
cpquery(fitted2, diagnosis == "M", evidence = list(worst_radius = "3",worst_concave_points = "4"), method = "lw", n = 100)
#92%

cpquery(fitted2, diagnosis == "M", evidence = list(worst_radius = "4",worst_concave_points = "1"), method = "lw", n = 100)
#44%

cpquery(fitted2, diagnosis == "B", evidence = list(worst_radius = "4",worst_concave_points = "1"), method = "lw", n = 100)
#56%

cpquery(fitted, diagnosis == "B", evidence = list(worst_perimeter = "1"), method = "lw", n = 100)
#84%

cpquery(fitted, diagnosis == "B", evidence = list(mean_concavity = "1", mean_concave_points = "1"), method = "lw", n = 100)
#85%

cpquery(fitted, diagnosis == "B", evidence = list(worst_symmetry = "1"), method = "lw", n = 100)
#69%

#########################################################################################################
#                                       Bayesian Classifier                                             #
#                                               bn.cv                                                   #
#########################################################################################################

set.seed(50)
data_rows1 <- sample(nrow(new_data))
shuffled_data <- new_data[data_rows1, ]
set.seed(50)

library(caret)
sum(complete.cases(shuffled_data))

inTrain1 <- createDataPartition(y = shuffled_data$diagnosis, p = 0.70, list = FALSE)
training1 <- shuffled_data[inTrain1,]
test1 <- shuffled_data[-inTrain1,]
dim(training1)

set.seed(50)

model <- bn.cv(data = shuffled_data, bn = bn.hc, method = "k-fold", fit = "bayes", k = 10, loss = "pred-lw", loss.args = list(target = "diagnosis"))
print(model)
plot(model)
#Classification error is 0.03690685
1-0.03690685
#Accuracy is 1 - 0.03690685 = 96.31 %

OBS = unlist(lapply(model, `[[`, "observed"))
PRED = unlist(lapply(model, `[[`, "predicted"))
str(OBS)
str(PRED)
table(OBS, PRED)

