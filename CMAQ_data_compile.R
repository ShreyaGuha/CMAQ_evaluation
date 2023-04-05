##CMAQ evaluation##

##reading .npy files
install.packages("reticulate")
library(reticulate)
np <- import("numpy")

##base
## where is CMAQ data
CMAQ.dir <- "D:/CMAQ/CMAQ_simulation_yearly/Base/"
CMAQ.files <- list.files( CMAQ.dir, full.names = TRUE,
                          recursive = TRUE,
                          pattern = '.npy')

# establish empty data.table to fill with for loop
cmaq_data_base <- data.table()

for (i in 2009:2019) {
  print( i)
  
  # define pattern to serch for that matches the year
  pattern <- paste0( 'PM25_', i, '.npy')
  
  # select correct files
  file_name <- grep( pattern, CMAQ.files, value = TRUE)
  
  # read file
  cmaq.i <-  np$load(file_name)
  
  #expand into long data form
  cmaq.i.col <- c(cmaq.i)
  
  #create a sequence for naming grids
  grid.i <- seq(1, 42224)
 
    # create a data table of the results
  cmaq_i_base.dt <- 
    data.table( year = i,
                PM_base = cmaq.i.col,
                grid.no = grid.i)
  
  # rbind with existing data table
  cmaq_data_base <- 
    rbind( cmaq_data_base,
           cmaq_i_base.dt)
  
}

#save data
fwrite(cmaq_data_base, file = "CMAQ_base.csv")


##HYPO_EMIS
## where is CMAQ data
CMAQ.dir <- "D:/CMAQ/CMAQ_simulation_yearly/HYPO_EMIS/"
CMAQ.files <- list.files( CMAQ.dir, full.names = TRUE,
                          recursive = TRUE,
                          pattern = '.npy')

# establishish empty data.table to fill with for loop
cmaq_data_emis <- data.table()

for (i in 2009:2019) {
  print( i)
  
  # define pattern to serch for that matches the year
  pattern <- paste0( 'PM25_', i, '.npy')
  
  # select correct files
  file_name <- grep( pattern, CMAQ.files, value = TRUE)
  
  # read file
  cmaq.i <-  np$load(file_name)
  
  #expand into long data form
  cmaq.i.col <- c(cmaq.i)
  
  #create a sequence for naming grids
  grid.i <- seq(1, 42224)
  
  # create a data table of the results
  cmaq_i_base.dt <- 
    data.table( year = i,
                PM_emis = cmaq.i.col,
                grid.no = grid.i)
  
  # rbind with existing data table
  cmaq_data_emis <- 
    rbind( cmaq_data_emis,
           cmaq_i_base.dt)
  
}

#save data
fwrite(cmaq_data_emis, file = "CMAQ_HYPOEMIS.csv")



##HYPO_MET
CMAQ.dir.met <- "D:/CMAQ/CMAQ_simulation_yearly/HYPO_MET/"
CMAQ.files.met <- list.files( CMAQ.dir.met, full.names = TRUE,
                          recursive = TRUE,
                          pattern = '.npy')

# establishish empty data.table to fill with for loop
cmaq_data_HYPOMET <- data.table()

for (i in 2009:2019) {
  print( i)
  
 # define pattern to serch for that matches the year
  pattern <- paste0( 'PM25_', i, '.npy')
  
  # select correct files
  file_name.met <- grep( pattern, CMAQ.files.met, value = TRUE)
  
  # read file
  cmaq.i <-  np$load(file_name)
  
  #expand into long data form
  cmaq.i.col <- c(cmaq.i)
  
  #create a sequence for naming grids
  grid.i <- seq(1, 42224)
  
  # create a data table of the results
  cmaq_i_base.dt <- 
    data.table( year = i,
                PM_met = cmaq.i.col,
                grid.no = grid.i)
  
  # rbind with existing data table
  cmaq_data_HYPOMET <- 
    rbind( cmaq_data_HYPOMET,
           cmaq_i_base.dt)
  
}

#save data
fwrite(cmaq_data_HYPOMET, file = "CMAQ_HYPOMET.csv")

#Combine data
CMAQ_data <- mutate(cmaq_data_base, cmaq_data_emis, cmaq_data_HYPOMET)

#save data
fwrite(CMAQ_data, file = "CMAQ_all_grids.csv")


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


##open grid data
setwd("D:/CMAQ/")
grid <- nc_open("map_cn27.nc")
print(grid) #get info about variables
lat <- ncvar_get(grid,"XLAT")
lon <- ncvar_get(grid,"XLON")

#create a data table with the coordinates
grid_dt <- data.table()
grid_dt$lat <- lat
grid_dt$lon <- lon

#create a long data for grid
grid.col <- c(grid_dt)

#name the grid id's
grid.col$grid.no <- seq(1, 42224)

#convert into data table
grid.col.dt <- as.data.table(grid.col)

#merge spatial info with CMAQ data
CMAQ_grid_data <- mutate( CMAQ_data.wide, grid.col.dt)

#for just differences, merge grid info with subset of differences
CMAQ_grid_data_diff <- mutate( CMAQ_data.sub.wide, grid.col.dt)

#save this data for plotting in future
fwrite(CMAQ_grid_data_diff, file = "CMAQ_all_grids_lat_lon.csv")

#convert into long data
CMAQ_diff_long <- reshape(CMAQ_grid_data_diff, 
                              idvar = "grid.no", timevar = "year",
                              direction = "long")

#rename columns
colnames(CMAQ_diff_long)[5] <- "diff_emiss" 
colnames(CMAQ_diff_long)[6] <- "diff_met" 

#save this data
fwrite(CMAQ_diff_long, file = "CMAQ_diff_long.csv")

#convert to data frame
CMAQ_diff_long_df <- as.data.frame(CMAQ_diff_long)

#plotting with grids
#subset data for plotting
dataset_plot <- CMAQ_diff_long_df[, c( 'lat', 'lon', 'year', 
                                       'diff_emiss', 'diff_met')] 
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

