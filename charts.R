#---------------------------------------CIRCULAR BAR PLOT--------------------------------------
library(tidyverse)

data <- data.frame(
  id = 1:60, individual=paste( "Hello ", 1:60, sep=""),
  value = sample(10:100, 60, replace=T)) 
#creating a data frame with 60 rows and 3 columns

label <- data
#A copy of original data frame to avoid modifying the original.

# calculate the ANGLE of the labels
n_bar <- nrow(label)
angle <-  90 - 360 * (label$id-0.5) /n_bar
#substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label$h<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label$angle<-ifelse(angle < -90, angle+180, angle)

# Start the plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       
  # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # The -ve value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(), #remove the axis text
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      
    # rep() function in R replicates the given values a specified number of times
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label, aes(x=id, y=value+10, label=individual, hjust=h),
            color="black", fontface="bold",alpha=0.6, size=2.5, angle= label$angle, inherit.aes = FALSE )
#Transforms the Cartesian plot into a circular (polar) plot.
#Add the labels, using the label_data dataframe that we have created before

p
#-------------------------------------------RADAR CHART----------------------------------------------------------
install.packages("fmsb") 
library(fmsb)

data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math", "english", "biology", "music", "R", "data-viz",
                    "french", "physic", "statistic", "sport" )

#I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot
data <- rbind(rep(20,10) , rep(0,10) , data) 

#radar chart 
radarchart(data)


#-----------------------------------------LOLLI POP CHART-----------------------------------------------
library(tidyverse)

data <- data.frame(
  x=LETTERS[1:26],
  y=abs(rnorm(26))) #rnorm : random numbers from a normal distribution 

ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)
#alpha : adjusting transparency
#stroke : sets the border thickness

#-------------------------------------------CUSTOMIZED RADAR CHART--------------------------------------------------------
install.packages("scales")
library(scales)
library(fmsb)

# Set graphic colors
library(RColorBrewer)

data <- as.data.frame(matrix( sample( 1:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "chemistry" , "biology" , "physics" , "Computer" )
rownames(data) <- letters[1:3]

# To use the fmsb package, I have to add 2 lines to the dataframe- max & min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

c <- brewer.pal(3, "Set1") #Brewer color palettes
colors_border <- c

colors_in <- alpha(c,0.5)

# If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
radarchart( data[-c(1,2),]  , axistype=0 , maxmin=F,
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.5, vlcex=0.9)
#cglcol Sets the color of the grid lines.
#cglty: Specifies the grid line type (solid).
#axislabcol: Sets the color of axis labels.
#cglwd: Specifies the thickness of the grid lines
#vlcex: Specifies the size of the variable labels.

# Add a legend
legend(x=1.5, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , 
       text.col = "black", cex=1.1, pt.cex=2)
#bty: Specifies the type of box around the legend. n: no box.


