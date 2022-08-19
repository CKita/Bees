################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info: https://github.com/CKita/Bees#readme
################################################################################


################################################################################
######### GEOGRAPHIC DISTRIBUTION OF STUDY SITES AND EFFECT SIZES ##############
################################################################################


#Let's get ready for running the code. 

#Set the working directory to the source of this script file.  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Delete all previous objects.
rm(list= ls())

#Load or install the required packages.
if (!require(dplyr)){
   install.packages('dplyr')
   library(dplyr)
}

if (!require(ggplot2)){
   install.packages('ggplot2')
   library(ggplot2)        
} 
        
if (!require(grDevices)){
   install.packages('grDevices') 
   library(grDevices)        
} 

if (!require(sf)){
   install.packages('sf')
   library(sf)
} 

if (!require(rnaturalearth)){
   install.packages('rnaturalearth') 
   library(rnaturalearth)     
} 

if (!require(rnaturalearthdata)){
   install.packages('rnaturalearthdata')  
   library(rnaturalearthdata)        
} 

if (!require(ggspatial)){
   install.packages('ggspatial') 
   library(ggspatial)        
} 

if (!require(rgeos)){
   install.packages('rgeos') 
   library(rgeos)        
} 

if (!require(cowplot)){
        install.packages('cowplot') 
        library(cowplot)        
} 


##### A. Cartogram: geographic distribution of effect sizes by study type ######


# First, let's import and check our data set.
sites <- read.csv("../Data/sites.csv", h= T, sep = ",")
class(sites)
str(sites)
head(sites)
tail(sites)


#Now, select the columns with the coordinates and study type.
sites_short <- sites %>% 
        dplyr::select(Latitude, Longitude, StudyType)

#Check the data.
head(sites_short)

#Load the world map from the mapdata package
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

#Plot the base map.
g1 <- ggplot(data = world) +
        geom_sf(colour = "white", fill = "lightgray") +
        coord_sf(xlim = c(-180, 180), ylim = c(-58,90), expand = FALSE) +
        theme_bw() + 
        
        #Plot the sites
        geom_point(data = sites_short, aes(x = Longitude, y = Latitude, 
                                           colour = StudyType), 
                   alpha = 0.6, size = 2) +
       
        #Customize the colors and labels
        scale_color_manual(values = c("#E64B35B2","#00A087B2")) + 
        labs(colour = "Study type", x = "Longitude", y = "Latitude") +
        theme(panel.grid = element_blank(),
              legend.text = element_text(size = 10),
              legend.title = element_text(face = "bold", size = 10),
              axis.text = element_text(size = 11, colour = "black"),
              axis.title.x = element_text(size = 12, colour= "black", vjust= -4,
                                          face = "bold"),
              axis.title.y = element_text(size = 12, colour= "black", vjust = 3,
                                          face = "bold"),
              legend.position = c(0.15,0.3),
              legend.background = element_rect(fill = "NA"),
              legend.key = element_rect(fill = "NA"),
              plot.margin = unit(rep(0.5,4), "lines")) +
        
        #Add a scale bar
        ggspatial::annotation_scale(
             location = "bl", width_hint = 0.3,
             bar_cols = c("grey30", "white")) +
       
         # Add a north arrow
        ggspatial::annotation_north_arrow(
             location = "tr", which_north = "true",
             height = unit(0.8, "cm"), 
                width = unit(0.8, "cm"),
                style = ggspatial::north_arrow_fancy_orienteering(
                                                  fill = c("white","grey30")))

#Check the map.
g1


############ B. Barplot: Number of effect sizes per country ####################


#Pick the countries.
countries <- sites$Country

#Check the number of effect size values per country.
table(countries)

#Make a barplot.
g2 <- ggplot(data.frame(countries), aes(x=countries)) +
      labs( y = "Number of effect sizes", x = "Country") +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),          
              panel.grid = element_blank(),
              axis.text = element_text(size = 8.5, colour = "black"),
              axis.title.x = element_text(size = 12, colour = "black", vjust =-3,
                                          face = "bold"),
              axis.title.y = element_text(size = 12, colour = "black", vjust = 3,
                                          face = "bold"),
              plot.margin = unit(c(1,1,1,1), "lines")) +
      geom_bar(fill ="lightgray") 

#Check the barplot.
g2

#Export both plots together as a single PNG image.
png("../Figure/sites.png", res = 300,
    width = 4300, height = 1800, unit = "px")

cowplot::plot_grid(g1, g2,  axis = "tb", align = "hv",rel_heights = c(1.5,0.5), 
                   rel_widths = c(2,1.5),
                  labels = c("A", "B"))

dev.off()

############################ END ###############################################