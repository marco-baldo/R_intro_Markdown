
# Create Data 
df <- data.frame(Mon=c(100, 0, 34),
                 Tue=c(100, 0, 48),
                 Wed=c(100, 0, 58),
                 Thu=c(100, 0, 67),
                 Fri=c(100, 0, 55),
                 Sat=c(100, 0, 29),
                 Sun=c(100, 0, 18))

#view data
df

# Once the data is in this format, we can use the radarchart() function from the 
# fmsb library to create a basic radar chart:


library(fmsb)

radarchart(df)


# Customizing Radar Charts in R

# We can customize the radar chart by using the following arguments:
  
# pcol: Line color
# pfcol: Fill color
# plwd: Line width
# cglcol: Net color
# cglty: Net line type
# axislabcol: Axis label color
# caxislabels: Vector of axis labels to display
# cglwd: Net width
# vlcex: Group labels size

radarchart(df,
           axistype=1, 
           pcol='pink',
           pfcol=rgb(0.9,0.2,0.5,0.3),
           plwd=3, 
           cglcol='grey',
           cglty=1,
           axislabcol='grey',
           cglwd=0.6,
           vlcex=1.1,
           title='Customers per Day'
)

# Doing the same but with an avarage or threshold line - can be useful also for other analysis
# https://stackoverflow.com/questions/50353923/generate-radar-charts-with-ggplot2

library(readr)

install.packages("ggplot2")
library(ggplot2)

data <- read.csv("C:/Users/baldo/Desktop/Baldo_pen_3 (264)/R/R_lab/Radar_chart/data.csv")
str(data)

# data <- data[,2:5]
# str(data)


ggplot(data=data,  aes(x=X2, y=Count, group=X3, colour=X3)) + 
  geom_point(size=5) + 
  geom_line() + 
  xlab("Decils") + 
  ylab("% difference in nº Pk") + 
  ylim(-50,25) + ggtitle("CL")  + 
  geom_hline(aes(yintercept=0), lwd=1, lty=2) + 
  scale_x_discrete(limits=c("DC1","DC2","DC3","DC4","DC5","DC6","DC7","DC8","DC9","DC10"))
                      
# I would like to transform this chart in a radar chart. I tried to use ggradar or ggRadar, but unsuccessfully. Something like this would be amazing:


ggplot(data=data,  aes(x=X2, y=Count, group=X3, colour=X3)) + 
  geom_point(size=5) + 
  geom_line() + 
  xlab("Decils") + 
  ylab("% difference in nº Pk") + 
  ylim(-50,25) + ggtitle("CL")  + 
  geom_hline(aes(yintercept=0), lwd=1, lty=2) + 
  scale_x_discrete(limits=c("DC1","DC2","DC3","DC4","DC5","DC6","DC7","DC8","DC9","DC10")) +
  coord_polar()


#-------------------------------------------------------------------------------------------------------------------
# Third Part - The most important 10+

# Followed by this [website](https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/)
# Beautiful Radar Chart in R using FMSB and GGPlot Packages

# A radar chart, also known as a spider plot is used to visualize the values or scores assigned to an individual over multiple quantitative variables, where each variable corresponds to a specific axis.

# This article describes how to create a radar chart in R using two different packages: the fmsb or the ggradar R packages.

# Note that, the fmsb radar chart is an R base plot. The ggradar package builds a ggplot spider plot.

# You will learn:
  
#  * how to create a beautiful fmsb radar chart
# * how to create ggplot radar chart
# * alternatives to radar charts

# **Demo data**
  
#  We’ll use a demo data containing exam scores for 3 students on 9 topics (Biology, Physics, etc). The scores range from 0 to 20. Columns are quantitative variables and rows are individuals:
  
# Demo data
exam_scores <- data.frame(
  row.names = c("Student.1", "Student.2", "Student.3"),
  Biology = c(7.9, 3.9, 9.4),
  Physics = c(10, 20, 0),
  Maths = c(3.7, 11.5, 2.5),
  Sport = c(8.7, 20, 4),
  English = c(7.9, 7.2, 12.4),
  Geography = c(6.4, 10.5, 6.5),
  Art = c(2.4, 0.2, 9.8),
  Programming = c(0, 0, 20),
  Music = c(20, 20, 20)
)
exam_scores

# Data preparation

# The data should be organized as follow:
  
#  * **The row 1 must contain the maximum values for each variable**
#  * **The row 2 must contain the minimum values for each variable**
#  * **Data for cases or individuals should be given starting from row 3**
#  * **The number of columns or variables must be more than 2.**

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  Biology = c(20, 0), Physics = c(20, 0), Maths = c(20, 0),
  Sport = c(20, 0), English = c(20, 0), Geography = c(20, 0),
  Art = c(20, 0), Programming = c(20, 0), Music = c(20, 0)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, exam_scores)
df

# Basic radar plot

# Plot the data for student 1
library(fmsb)
student1_data <- df[c("Max", "Min", "Student.1"), ]
radarchart(student1_data)

# Customize the radar charts

--------------------------------------------------------------------------------
Key arguments to customize the different components of the fmsb radar chart:

Variable options
vlabels: variable labels
vlcex: controls the font size of variable labels
Polygon options:
  pcol: line color
pfcol: fill color
plwd: line width
plty: line types. Can be a numeric vector 1:6 or a character vector c(“solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”). To remove the line, use plty = 0 or plty = “blank”.
Grid options:
  cglcol: line color
cglty: line type
cglwd: line width
Axis options:
  axislabcol: color of axis label and numbers. Default is “blue”.
caxislabels: Character vector to be used as labels on the center axis.
--------------------------------------------------------------------------------

# Helper function to produce a beautiful radar chart:

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# In the code above, we used the function alpha() [in scales package] to change the polygon fill color transparency.

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(student1_data, caxislabels = c(0, 5, 10, 15, 20))
par(op)

# Create radar charts for multiple individuals

# Create the radar chart of the three students on the same plot:

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "topright", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)



-------------------------------------------------------------------------------
In the legend() function of ggplot2, you can specify the position of the legend using the x and y arguments. Here are some of the possible positions for the legend:
  
"top": Places the legend at the top of the plot.
"bottom": Places the legend at the bottom of the plot.
"left": Positions the legend on the left side of the plot.
"right": Positions the legend on the right side of the plot.
"topleft": Positions the legend in the top left corner of the plot.
"topright": Positions the legend in the top right corner of the plot.
"bottomleft": Positions the legend in the bottom left corner of the plot.
"bottomright": Positions the legend in the bottom right corner of the plot.
c(x_coordinate, y_coordinate): Allows you to specify custom coordinates for the legend. You can provide the x and y coordinates as numeric values.

For example, if you want to place the legend on the top right corner of the plot, you can use legend(x = "topright"). Similarly, if you want to specify custom coordinates, you can use legend(x = 0.8, y = 0.2) to position the legend at the coordinates (0.8, 0.2) within the plot.

These different positions give you flexibility in placing the legend in various parts of the plot depending on your visualization requirements.
--------------------------------------------------------------------------------

# Create separated spider charts for each individual. This is recommended when you have more than 3 series.

# Define colors and titles

colors <- c("#00AFBB", "#E7B800", "#FC4E07")
titles <- c("Student.1", "Student.2", "Student.3")

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,3))

# Create the radar chart
for(i in 1:3){
  create_beautiful_radarchart(
    data = df[c(1, 2, i+2), ], caxislabels = c(0, 5, 10, 15, 20),
    color = colors[i], title = titles[i]
  )
}
par(op)

#-------------------------------------------------------------------------------
# Compare every profile to an average profile

# Radar charts are most useful if the profile of every individual is compared to an average profile.

# Create a demo data containing exam scores for 10 students:

set.seed(123)
df <- as.data.frame(
  matrix(sample(2:20 , 90 , replace = TRUE),
         ncol=9, byrow = TRUE)
)

# In this example the matrix we build up has a vote (variable) between 2 and 20,
# x90 times (total votes=values 10 student x9 subjects), replace the value in 
# the empty matrix. ncol = at the number of subject (9), order the frame by 
# number of rows of the data frame = 10!
df 

colnames(df) <- c(
  "Biology", "Physics", "Maths", "Sport", "English", 
  "Geography", "Art", "Programming", "Music"
)
rownames(df) <- paste0("Student.", 1:nrow(df))
head(df)

# Give the name based on a root = Student and adding the row number from 1 to 
# end (10), Same is giving the name of the subject per every column


# Rescale each variable to range between 0 and 1:

install.packages("scales")
library(scales)

df_scaled <- round(apply(df, 2, scales::rescale), 2)
df_scaled <- as.data.frame(df_scaled)
head(df_scaled)

#------------------------------------------------------------------------------
# Prepare the data for creating the radar plot using the fmsb package:

# Variables summary
# Get the minimum and the max of every column  
col_max <- apply(df_scaled, 2, max)
col_min <- apply(df_scaled, 2, min)
# Calculate the average profile 
col_mean <- apply(df_scaled, 2, mean)
# Put together the summary of columns
col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))

# Bind variables summary to the data
df_scaled2 <- as.data.frame(rbind(col_summary, df_scaled))
head(df_scaled2)


# Here we created a data frame in which are present also the minimum scaled value, the maximum scaled value and the mean scaled value, for every subject in our matrix. This is added to the original data frame above the others rows.

# **Produce radar plots showing both the average profile and the individual profile:**

opar <- par() 
# Define settings for plotting in a 3x4 grid, with appropriate margins:
par(mar = rep(0.8,4))
par(mfrow = c(3,4))
# Produce a radar-chart for each student
for (i in 4:nrow(df_scaled2)) {
  radarchart(
    df_scaled2[c(1:3, i), ],
    pfcol = c("#99999980",NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    title = row.names(df_scaled2)[i]
  )
}
# Restore the standard par()

settings
par <- par(opar) 

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Forth Part

install.packages("ggradar")
library(ggradar)

# Key function and arguments

ggradar(
  plot.data, values.radar = c("0%", "50%", "100%"),
  grid.min = 0, grid.mid = 0.5, grid.max = 1, 
)

--------------------------------------------------------------------------------
plot.data: data containing one row per individual or group
values.radar: values to show at minimum, average and maximum grid lines
grid.min: value at which minimum grid line is plotted
grid.mid: value at which average grid line is plotted
grid.max: value at which maximum grid line is plotted
--------------------------------------------------------------------------------
  
  
# Data preparation 
  
# All variables in the data should be at the same scale. If this is not the case, you need to rescale the data. For example, you can rescale the variables to have a minimum of 0 and a maximum of 1 using the function rescale() [scales package]. We’ll describe this method in the next sections.

# Put row names into  a column named group
library(tidyverse)

df <- exam_scores %>% rownames_to_column("group")
df  
  
# Basic radar plot

# Plotting student 1
ggradar(
  df[1, ], 
  values.radar = c("0", "10", "20"),
  grid.min = 0, grid.mid = 10, grid.max = 20
)

# Customize radar charts  
  
# Key arguments to customize the different components of the ggplot radar chart. For more options see the documentation.

ggradar(
  df[1, ], 
  values.radar = c("0", "10", "20"),
  grid.min = 0, grid.mid = 10, grid.max = 20,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#00AFBB",
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey"
)

# Radar chart with multiple individuals or groups

# Create the radar chart of the three students on the same plot:

ggradar(
  df, 
  values.radar = c("0", "10", "20"),
  grid.min = 0, grid.mid = 10, grid.max = 20,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800", "#FC4E07"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom"
)


# Alternatives to radar charts

# A circular plot is difficult to read. An alternative to a radar chart is an ordered lolliplot or dotchart. This section describes how to create dotcharts. The ggpubr R package will be used in this section to create a dotchart.

# Load required packages:

install.packages("ggpubr")

library(tidyverse)
library(ggpubr)

# Case when all quantitative variables have the same scale**
  
# Displaying one individual

# Data preparation:

df2 <- t(exam_scores) %>%
  as.data.frame() %>%
  rownames_to_column("Field")
df2

# Plot creation:

ggdotchart(
  df2, x = "Field", y = "Student.1",
  add = "segments", sorting = "descending",
  ylab = "Exam Score", title = "Student 1"
)

# Displaying two individuals

# Data preparation:

df3 <- df2 %>%
  select(Field, Student.1, Student.2) %>%
  pivot_longer(
    cols = c(Student.1, Student.2),
    names_to = "student",
    values_to = "value"
  )
head(df3)

# Plot creation

ggdotchart(
  df3, x = "Field", y = "value", 
  group = "student", color = "student", palette = "jco",
  add = "segment", position = position_dodge(0.3),
  sorting = "descending"
)

#-------------------------------------------------------------------------------
# Displaying multiple individuals

# Data preparation:
  
  
df4 <- df2 %>%
  select(Field, Student.1, Student.2, Student.3) %>%
  pivot_longer(
    cols = c(Student.1, Student.2, Student.3),
    names_to = "student",
    values_to = "value"
  )
head(df4)


# Plot creation

ggdotchart(
  df4, x = "Field", y = "value", 
  group = "student", color = "student", palette = "jco",
  add = "segment", position = position_dodge(0.3),
  sorting = "descending", facet.by = "student",
  rotate = TRUE, legend = "none"
)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Case when you have a lot of individuals to plot or if your variables have different scales

# A solution is to create a parallel coordinates plot.**

library(GGally)

ggparcoord(
  iris,
  columns = 1:4, groupColumn = 5, order = "anyClass",
  showPoints = TRUE, 
  title = "Parallel Coordinate Plot for the Iris Data",
  alphaLines = 0.3
) + 
  theme_bw() +
  theme(legend.position = "top")

# Note that, the default of the function ggparcoord() is to rescale each variable by subtracting the mean and dividing by the standard deviation.  
  
  
  