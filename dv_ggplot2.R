# install packages
pkgs.needs <- c("ggplot2", "ggsci", "dplyr")
pkgs.installed <- installed.packages()[,"Package"] 
new.pkgs <- pkgs.needs[!(pkgs.needs %in% pkgs.installed)]
if(length(new.packages)) install.packages(new.pkgs)                         
library(ggplot2)  # plot 
library(dplyr)    # data manipulation & pipe line
library(ggsci)    # scientific journals themed color palettes for ggplot2 

# input data
df <- iris

# define function 
## Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# explore data
df %>% summary
# add flag
df$Petal.Length.flag <-  case_when(
  df$Sepal.Length <= 5 ~ "len_below_5",
  df$Sepal.Length > 5 & df$Sepal.Length <= 6 ~ "len_between_5_6",
  df$Sepal.Length > 6 ~ "len_over_6"
)
# barChart
## simple barChart
p1 <-
ggplot(data=df, aes(x=Species)) +
  geom_bar(stat="count", color="steelblue", fill="white") ## blue frame, fill white
p1
## fill bars based on species
p2 <-
ggplot(data=df, aes(x=Species, fill=Species)) +
  geom_bar(stat="count")
p2
## stacked bar
p3 <- 
  ggplot(data=df, aes(x=Species, fill=Petal.Length.flag)) +
  geom_bar(stat="count", position="fill")
p3
## horizontal 
p4 <- 
  ggplot(data=df, aes(x=Species, fill=Petal.Length.flag)) +
  geom_bar(stat="count", position="fill") +
  coord_flip()
p4
## add sci themed color
p5 <-
  ggplot(data=df, aes(x=Petal.Length.flag, fill=Petal.Length.flag)) +
  geom_bar(stat="count") + 
  scale_fill_aaas()
p5
## remove all text and background
p6 <-
  ggplot(data=df, aes(x=Petal.Length.flag, fill=Petal.Length.flag)) +
  geom_bar(stat="count") + 
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.position="none")
p6
## multiple plot
multiplot(p1, p2, p3, p4, p5, p6, cols=3)
