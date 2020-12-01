library(imager)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(sp)
library(scales)
library(cowplot)
library(ggplot2)
#install.packages("devtools")
#library(devtools)
#devtools::install_github("sharlagelfand/dmc")
library(dmc)

# FUNCTION 1
process_image <- function(image_file_name, k_list){
  ## The function will return a tibble of information that includes:
  ## - the output of the clustering: k, totss, tot.withinss,betweenss, iter
  ## - the clusters centres associated with RGB values and the matched 
  ##    thread information   
  ##
  ## Input:
  ## - image_file_name: a PNG or JPEG image
  ## - k_list: the number of centres in the clustering
  ##
  ## Output:
  ## - cluster_info: A list or tibble of information derived from the k_means that will 
  ##   be sufficient to be the input to any other function I write.
  ##
  ## Example:
  ##   library(imager)
  ##   library(tidyverse)
  ##   library(tidymodels)
  ##   library(dplyr)
  ##   library(dmc)
  ##   k_list<-c(2:8)
  ##   process_image("file_name.jpg",k_list)
  ##   
  
  im<-imager::load.image(image_file_name)

  tidy_dat <- as.data.frame(im,wide="c") %>% rename(R=c.1, G=c.2, B=c.3)
  
  #delete column x,y
  dat <-select(tidy_dat,c(-x,-y))
  
  kclusts <- 
    tibble(k=c(k_list)) %>% 
    mutate(
      kclust = map(k,~kmeans(x=dat, centers = .x, nstart = 4)),
      glanced = map(kclust,glance),
    )
  
  #expand glanced
  clusterings <-
    kclusts %>% 
    unnest(cols=c(glanced))
  
  # add center info to each K 
  clusterings <- clusterings %>% 
    mutate(centres = map(kclust,tidy),
           tidy_dat = map(kclust,~augment(.x,tidy_dat)%>% rename(cluster = .cluster)))

  for (i in 1:length(k_list)) {
    ct<-clusterings[i,]$centres[[1]] %>%
      mutate(col=rgb(R,G,B)) %>% 
      mutate(dmc=map(col,~dmc(.x)))
    clusterings[i,]$centres[[1]] <- ct
  }
  
  return(clusterings)
}


# FUNCTION 2
scree_plot <- function(cluster_info){
  ## The function will Produce and plot a scree plot.
  ##
  ## Input:
  ## - cluster_info: The ouput from process_image function
  ##
  ## Output:
  ## - A image contains two plots: 
  ##   -number of cluster(k) vs. tot.withinss plot (total within-cluster sum of squares)
  ##    this represents the variance within the clusters
  ##   -number of cluster(k) vs. ratio of tot.withinss
  ##    where ratio = tot.withinss(k)/tot.withinss(k-1)
  ##
  ## Example:
  ##   library(tidyverse)
  ##   library(ggplot2)
  ##   scree_plot(cluster_info)
  ##   
  
  # k vs. tot.withinss
  pic_1 <- ggplot(cluster_info, aes(k, tot.withinss)) +
          geom_line() +
          geom_point()+ggtitle("k vs. tot.withinss")

  # ratio version
  nclust = length(cluster_info$k)
  ratio = rep(NA, nclust-1)
  for (kk in 2:nclust) {
    ratio[kk-1] = cluster_info$tot.withinss[kk]/cluster_info$tot.withinss[kk-1]
  }
  plot_data <- data.frame(k = cluster_info$k[2:nclust],ratio) 
  
  pic_2<-ggplot(plot_data, aes(x=k, y = ratio)) + geom_line()+ggtitle("ratio version")
  
  plot_grid(pic_1,pic_2)
}
 

# FUNCTION 3
colour_strips <- function(cluster_info){
  ## This function produces colour strips with the DMC colour
  ##          closest to the cluster centre colour
  ##            
  ## Input:
  ## - cluster_info: The ouput from process_image
  ##
  ## Output:
  ## - A list of colour strips for each number of cluster from cluster_info
  ##
  ## Example:
  ##   library(tidyverse)
  ##   library(ggplot2)
  ##   library(scales)
  ##   colour_strips(cluster_info)
  ## 
  
  square <- function(x, label_size) { 
    ggplot()  + 
      coord_fixed(xlim=c(0,1), ylim = c(0,1)) + theme_void() + 
      theme(plot.background = element_rect(fill = x)) + 
      geom_text(aes(0.5,0.5),label = x , size = label_size)
  }
  
  plot1<-list()
  for (i in 1:nrow(cluster_info)) {
    k_val<-cluster_info$k[i]
    colours<-NULL
    for (x in 1:k_val) {
      colours[x]<-cluster_info$centres[[i]]$dmc[[x]][[3]] # select the 3rd 
    }
  
    t <- tibble(colours = colours,
                squares = purrr::map(colours, ~ square(.x, 24/length(colours))))
    
    
    n_col = length(t$colours)
    rect_dat <- tibble(x1 = c(0:(n_col-1)), x2 = c(1:n_col), y1 = rep(0,n_col),
                       y2 =rep(1,n_col), colour = t$colours)
    plot<-rect_dat %>% ggplot() + coord_fixed() + 
      geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=colour), color="black") +
      geom_text(aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=colour), size=12/n_col) + 
      scale_fill_manual(values = rect_dat$colour)+ theme_void() + theme(legend.position = "none") 
    
    plot1[[i]]<-plot
  }
  
  plot_grid(plotlist=plot1,labels=paste0("k=",c(cluster_info$k)),ncol = 1)
}


# Given Code
change_resolution <- function(image_df, x_size)
{
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  ##   im_dat<- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  ##            select(x,y,R,G,B)
  ##   agg_image <- change_resolution(im_dat, 50)
  ##
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}


# FUNCTION 4
make_pattern <-function(cluster_info, k, x_size, black_white =FALSE,
                        background_colour=NULL){
  ## Plot the pattern of image
  ##
  ## Input:
  ## - cluster_info: The ouput from process_image
  ## - k: The chosen cluster size
  ## - x_size: The (approximate) total number of possible stitches in the horizontal direction
  ## - black_white: (logical) Print the pattern in back and white (TRUE) ot colour (FALSE)
  ## - background_colour: The colour of the background, which should not be stitched in the 
  ##                      pattern. (Defalut is to not have a colour)
  ##
  ## Output:
  ## - Produce a cross stitch pattern that can be followed, complete with a legend that has thread 
  ##   colour, and a guide grid.
  ##
  ## Example:
  ##   library(tidyverse)
  ##   library(ggplot2)  
  ##   library(dplyr)
  ##   library(scales)
  ##   library(cowplot)
  ##   make_pattern(cluster_info,k=5,x_size=120)
  ## 
  
  tidy_dat_k<-cluster_info[cluster_info$k==k,]$tidy_dat[[1]]
  
  #change resolution & we only need coordinate and cluster
  agg_image<-change_resolution(tidy_dat_k, x_size = x_size) %>% select(x,y,cluster)
  
  centre_k<-cluster_info[cluster_info$k==k,]$centres[[1]] %>% unnest(cols=c(dmc))%>%
    select(cluster,col,dmc,name,hex)
  
  agg_image<- inner_join(agg_image,centre_k,by="cluster")
  
  if(!black_white){
    plot2<-agg_image %>% ggplot(aes(x, y)) + 
      geom_point(aes(col = factor(dmc), 
                     shape = factor(dmc))) +
      scale_colour_manual(values = agg_image %>% select(dmc, hex) %>% deframe,
                          label =  agg_image %>% select(dmc, name) %>% deframe)+
      scale_y_reverse()+theme_void()
  }
  else{
    plot2<-agg_image %>% ggplot(aes(x, y)) + 
      geom_point(aes(shape = factor(dmc))) +
      scale_colour_manual(values = agg_image %>% select(dmc, hex) %>% deframe,
                          label =  agg_image %>% select(dmc, name) %>% deframe)+
      scale_y_reverse()+theme_void()
  }
  plot2
}

