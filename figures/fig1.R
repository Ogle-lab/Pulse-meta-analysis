# Figure 1. Map of sites

library(tidyverse)
library(raster)
library(terra)
library(cowplot)
library(patchwork)


path_out = "./figures/" # set save path


# Read in data
df_sites<- read_csv("./data_clean/Clean_record_info.csv")

r <- getData("worldclim",var="bio",res=10)

r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")

lats <- df_sites$Latitude
lons <- df_sites$Longitude

coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)


# mapping aridity
aridity_index_df <- read.csv("./data_raw/aridity_index_df.csv")
aridity_spat <- rast("./data_raw/aridity_index.tif")

# Make infinite values the max aridity so humid areas won't show up as blanks
# aridity_index_df2 <- aridity_index_df %>%
#   mutate(layer = case_when(layer>2 ~ 2,
#          layer<=2 ~ layer)) %>%
#   select(-category)
# aridity_spat2 <- rasterFromXYZ(aridity_index_df2) # convert to rasterbrick
# aridity_spat2 <- rast(aridity_spat2) # convert to spatraster for plotting

# Make custom color palette
library(unikn)
mypal2 = rev(cetcolor::cet_pal(20, name = "r2", alpha = 0.5)) 
unikn::seecol(mypal2)
mypal3 = rev(cetcolor::cet_pal(20, name = "l18", alpha = 0.5))
unikn::seecol(mypal3)


# plot using ggplot and tidyterra

library(tidyterra)
# soil moisture
# p <- ggplot() +
#   geom_spatraster(data = sm) +
#   geom_point(data = df_sites, aes(x=Longitude, y=Latitude), color = "black", size=1) +
#   scale_fill_gradientn(colors=mypal2,                               # Use user-defined colormap
#                        name = "Soil Moisture",                                 # Colorbar name
#                        na.value = "transparent",                    # transparent NA cells
#                        labels=(c("0", "0.2", "0.4", "0.6", "0.8")), # Colorbar labels
#                        breaks=seq(0,0.8,by=0.2),                    # Set breaks of colorbar
#                        limits=c(0,0.8))+                            # Z-axis limits
#   theme_void()                                 # Try different themes: theme_bw(), theme_gray(), theme_minimal
# 
# p

#aridity
colours <- c('#e31a1c', '#fd8d3c', '#fecc5c', '#ffffb2', '#666666')
p <- ggplot() +
  geom_point(data = df_sites, aes(x=Longitude, y=Latitude), color = "black", size=1) +
  geom_raster(data = aridity_index_df,aes(y = y,
                                             x = x,
                                             fill = category)) +
  scale_fill_manual(values = colours,
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(-60, 90),
                     expand = c(0, 0),
                     breaks = c(-40, -20, 0, 20, 40, 60, 80),
                     labels = c(expression('40'*degree*'S'),
                                expression('20'*degree*'S'),
                                expression('0'*degree),
                                expression('20'*degree*'N'),
                                expression('40'*degree*'N'),
                                expression('60'*degree*'N'),
                                expression('80'*degree*'N'))) +
  scale_x_continuous(limits = c(-180, 180),
                     expand = c(0, 0),
                     breaks = c(-180, -120, -60, 0, 60, 120, 180),
                     labels = c(expression('180'*degree*'W'),
                                expression('120'*degree*'W'),
                                expression('60'*degree*'W'),
                                expression('0'*degree),
                                expression('60'*degree*'E'),
                                expression('120'*degree*'E'),
                                expression('180'*degree*'E'))) +
  theme_void()                                 # Try different themes: theme_bw(), theme_gray(), theme_minimal

p

#~~~Projection 2: Robinson projection

#NE_places - SpatialPointsDataFrame with city and town points

WorldSHP=terra::vect(spData::world)

#--- convert to a SpatRaster ---#
points2 <- vect(points)

# RobinsonPlot <- ggplot() +
#   geom_spatraster(data = sm)+                   # Plot SpatRaster layer               
#   geom_spatvector(data = WorldSHP, 
#                   fill = "transparent") +       # Add world political map
#   geom_spatvector(data = points2, # add points
#                   fill   = "transparent",
#                   colour = "black",
#                   #stroke = 1,
#                   alpha  = 0.5) +
#   ggtitle("Site Locations") +              # Add title
#   scale_fill_gradientn(colors=mypal2,           # Use user-defined colormap
#                        name = "Soil Moisture",  # Name of the colorbar
#                        na.value = "transparent",# Set color for NA values
#                        lim=c(0,0.8))+           # Z axis limit
#   theme_minimal()+                              # Select theme. Try 'theme_void'
#   theme(plot.title = element_text(hjust =0.5),  # Place title in the middle of the plot
#         text = element_text(size = 12))+        # Adjust plot text size for visibility
#   coord_sf(crs = "ESRI:54030",                  # Reproject to World Robinson
#            xlim = c(-152,152)*100000,    
#            ylim = c(-55,90)*100000)
# 
# print(RobinsonPlot)
#ggsave2("testmap.png", plot = RobinsonPlot, path = path_out)

# Try with aridity
# RobinsonPlot <- ggplot() +
#   geom_spatraster(data = aridity_spat)+                   # Plot SpatRaster layer               
#   geom_spatvector(data = WorldSHP, 
#                   fill = "transparent") +       # Add world political map
#   geom_spatvector(data = points2, # add points
#                   #fill   = "transparent",
#                   #colour = "black",
#                   shape  = 21,
#                   colour = "#666666", 
#                   fill   = "black",
#                   size   = 1,
#                   #stroke = 1,
#                   alpha  = 0.3) +
#   ggtitle("Site Locations") +              # Add title
#   scale_fill_gradientn(colors=mypal2,           # Use user-defined colormap
#                        name = NULL,  # Name of the colorbar
#                        na.value = "transparent",# Set color for NA values
#                        labels=(c("Hyper-arid", "Arid","Semi-arid", "Dry sub-humid", "Humid")), # Colorbar labels
#                        breaks=c(0.05, 0.2, 0.5, 0.65, 0.8),
#                        lim=c(0,0.8))+           # Z axis limit
#   theme_minimal()+                              # Select theme. Try 'theme_void'
#   theme(plot.title = element_text(hjust =0.5),  # Place title in the middle of the plot
#         text = element_text(size = 12))+        # Adjust plot text size for visibility
#   coord_sf(crs = "ESRI:54030",                  # Reproject to World Robinson
#            xlim = c(-152,152)*100000,    
#            ylim = c(-55,90)*100000)
# 
# print(RobinsonPlot)


#### plot whittiker biome plot
#devtools::install_github("valentinitnelav/plotbiomes")
library(plotbiomes)

 df_sites$MAP.cm.wc <- df_sites$MAP.mm.wc/10 # convert to cm

p <- whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = df_sites,
             aes(x = MAT.C.wc,
                 y = MAP.cm.wc),
             size   = .7,
             shape  = 21,
             colour = "#666666",
             fill   = "black",
             stroke = 1,
             alpha  = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
p
 
my_legend <- get_legend(p)
 
# p_noleg <- whittaker_base_plot() +
#   # add the temperature - precipitation data points
#   geom_point(data = df_sites, 
#              aes(x = MAT.C.wc, 
#                  y = MAP.cm.wc), 
#              size   = .2,
#              shape  = 21,
#              colour = "#666666", 
#              fill   = "black",
#              stroke = 1,
#              alpha  = 0.3) +
#   theme_bw() +
#   theme(legend.position = "none")
# p_noleg
# 
# #ggsave2("whittaker.png", plot = p, path = path_out)
# 

# fig1 <- RobinsonPlot + patchwork::inset_element(p_noleg,.03,.03,.27,.45)
# fig1
# design <- "AAAAAA
#            AAAAAA
#            ##BBB#"
# fig_together <- fig1 + as_ggplot(my_legend) + plot_layout(design = design)
# 
# ggsave2("fig1.png", plot = fig_together, path = path_out, width = 9, height = 6)

##### v2
RobinsonPlot <- ggplot() +
  geom_spatraster(data = aridity_spat)+                   # Plot SpatRaster layer               
  geom_spatvector(data = WorldSHP, 
                  fill = "transparent") +       # Add world political map
  geom_spatvector(data = points2, # add points
                  #fill   = "transparent",
                  #colour = "black",
                  shape  = 21,
                  colour = "white", # outline in white for visibility
                  fill   = "black",
                  size   = 2,
                  #stroke = 1,
                  alpha  = 0.3) +
  ggtitle("Site Locations") +              # Add title
  scale_fill_gradientn(colors=mypal2,           # Use user-defined colormap
                       name = NULL,  # Name of the colorbar
                       na.value = "transparent",# Set color for NA values
                       labels=(c("Hyper-arid", "Arid","Semi-arid", "Dry sub-humid", "Humid")), # Colorbar labels
                       breaks=c(0.05, 0.2, 0.5, 0.65, 0.8),
                       lim=c(0,0.8))+           # Z axis limit
  theme_minimal()+                              # Select theme. Try 'theme_void'
  theme(plot.title = element_text(hjust =0.5),  # Place title in the middle of the plot
        text = element_text(size = 12),
        legend.position = c(0.14,.3))+        # Adjust plot text size for visibility
  coord_sf(crs = "ESRI:54030",                  # Reproject to World Robinson
           xlim = c(-152,152)*100000,    
           ylim = c(-55,90)*100000)

print(RobinsonPlot)

p <- whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = df_sites, 
             aes(x = MAT.C.wc, 
                 y = MAP.cm.wc), 
             size   = 1,
             shape  = 21,
             colour = "white", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.3) +
  theme_bw()
design <- "AAAAAA
           AAAAAA
           #BB###"
fig_together <- RobinsonPlot + p + plot_layout(design = design) + plot_annotation()
ggsave2("fig1.png", plot = fig_together, path = path_out, width = 9, height = 7.5)
ggsave2("fig1.tiff", plot = fig_together, path = path_out, width = 9, height = 7.5)

#fig1 <- gridExtra::grid.arrange(RobinsonPlot,p,nrow=2)

