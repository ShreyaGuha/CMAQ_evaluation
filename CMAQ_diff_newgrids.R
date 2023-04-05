#where is grid data
setwd <- "D:/spatial/"
grid <- read.delim("D:/spatial/GRIDDESC.txt")
#do not think this is required

##reading .npy files
install.packages("reticulate")
library(reticulate)
np <- import("numpy")

#extract the lat lon data
grid.lat <- c(np$load("D:/spatial/lat.npy"))
grid.lon <- c(np$load("D:/spatial/lon.npy"))
grid_latlon <- data.table( grid_no <- seq(1, 42224),
                           lat = grid.lat,
                           lon = grid.lon)

#save grid info in another directory
setwd("D:/CMAQ/")
fwrite(grid_latlon, "china_grid.csv")

#open CMAQ data
CMAQ_data <- fread("CMAQ_all_grids.csv")

#find the total difference'
CMAQ_data$diff_emis <- CMAQ_data$PM_emis - CMAQ_data$PM_base
CMAQ_data$diff_met <- CMAQ_data$PM_met - CMAQ_data$PM_base

#convert to wide data form
CMAQ_data.wide <- reshape(CMAQ_data, 
                          idvar = "grid.no", timevar = "year",
                          direction = "wide")

#subset for differences
CMAQ_data.sub <- subset(CMAQ_data, select = c('year','grid.no',
                                              'diff_emis', 'diff_met'))
#convert this to wide data
CMAQ_data.sub.wide <- reshape(CMAQ_data.sub, 
                              idvar = "grid.no", timevar = "year",
                              direction = "wide")

#merge spatial info with CMAQ data
CMAQ_grid_data_diff <- mutate( CMAQ_data.sub.wide, grid_latlon)

#save this data for plotting in future
fwrite(CMAQ_grid_data_diff, file = "CMAQ_new_grids.csv")

#convert into long data
CMAQ_diff_long <- reshape(CMAQ_grid_data_diff, 
                          idvar = "grid.no", timevar = "year",
                          direction = "long")

#rename columns
colnames(CMAQ_diff_long)[6] <- "diff_emiss" 
colnames(CMAQ_diff_long)[7] <- "diff_met" 

#convert to data frame
CMAQ_diff_long_df <- as.data.frame(CMAQ_diff_long)

#plotting with grids
#subset data for plotting
dataset_plot <- CMAQ_diff_long_df[, c( 'lat', 'lon', 'year', 
                                       'diff_emiss', 'diff_met')] 
fwrite(dataset_plot, "newgrid.csv")

#convert to spatial object
dataset_plot.sf <- st_as_sf( dataset_plot, coords = c( 'lat', 'lon'))

#plot for emissions
plot1 <-  ggplot(data = dataset_plot.sf) +
  geom_point(data = dataset_plot, 
             aes(x= lon, y= lat, color = diff_emiss)) +
  coord_sf(xlim = c(66.94, 153.05), ylim = c(8.92, 55.48)) +
  theme( axis.text = element_text( size = 4)) +
  facet_wrap( . ~ year)  


#plot for meteorology
plot2 <-  ggplot(data = dataset_plot.sf) +
  geom_point(data = dataset_plot, 
             aes(x= lon, y= lat, color = diff_met)) +
  coord_sf(xlim = c(66.94, 153.05), ylim = c(8.92, 55.48)) +
  theme( axis.text = element_text( size = 4)) +
  facet_wrap( . ~ year) 


#arrange the plots together
library(ggpubr)
ggarrange(plot1, plot2,
          ncol = 2, nrow = 1)









