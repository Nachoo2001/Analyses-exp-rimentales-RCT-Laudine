#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
#---------------------     Investigating how administrative burden and search costs        -----------------------------------#               
#--------------------                       affect social inequalities                     -----------------------------------#  
# --------------------      in early childcare access, a randomised controlled trial       -----------------------------------#
#                                               -------------
#--------------------                    Load and install libraries                        -----------------------------------# 
#--------------------              Authors: Laudine Carbuccia & Arthur Heim                -----------------------------------#    
#--------------------                            Version Final                             -----------------------------------#  
#--------------------                            July 2025                                 -----------------------------------#     
#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
##### load or install libraries %%%%%%%%%%%##
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

#------- cran_packages ---------
library(pacman)
rm(list=ls())


# Packages for importing, storing and manipulating data

p_load(here, devtools,                 # Packages from git an others
       tidyverse,                # Tidyverse for data wrangling, ggplot2 etc.
       broom,                    # Broom for tidy methods
       lubridate,                # For Date-time formats
       readxl,                   # Read write excel files
       officer                   # Manipulate office documents from Rmarkdown
       )

# Packages used for models and estimations

p_load(fixest,                    # Fast fixed-effect models
       estimatr,                  # Wrappers of models for treatment effects
       hdm,                       # High dimensional models : lasso and post-lasso from Chernozukov et al.
       grf,                       # Generalised random forest from Athey et al.
       fwildclusterboot,          # Fast wild cluster bootstrap from McKinnon et al (2023)
       glmnet,                    # Generalised linear model with regularization (lasso, ridge and elasticnets)
       nnet,                      # Neural network models and multinomial logit
       multcomp,                  # Multiple comparisons
       twang,                     # Functions for propensity score estimations and weightings + non-response analysis and weighting
       caret,
       randomizr
      )


# Package used for generating, formatting and exporting plots and tables

p_load(modelsummary,              # data and model summary tables and plots
       gtsummary,                 # other package for summary tables and regressions (balance test easy !)
       flextable,                 # Rmarkdown tables pretty good with word exports (and other formats)
       #kableExtra,               # Good package for LaTeX export (and html) but not word documents 
       viridis,                   # Viridis palettes
       RColorBrewer,              # Predifined palettes 
       gglgbtq,                   # color palettes with LGBTQIA flags
       ggh4x,                     # Hack for some ggplot functions (in particular, useful for adapting facets)
       cowplot, patchwork,        # Two packages to build figures from ggplot objects
       gridExtra,                 # additional functions for grids of graphics
       hrbrthemes,                # Additional thems for ggplot
       legendry,                   # Nested axis guide
       extrafont, extrafontdb,    # additional fonts
       patchwork, 
       grid,
       ggpubr,                    # Nice graphical possibilities for publication ready plots
       sf,                        # Simple features for spatial data
      spData)                     # Spatial data



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

select <- dplyr::select
summarize <- dplyr::summarize


## Colorblind-friendly palettes
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


custom.col <- c( "#C4961A","#FFDB6D",
                 "#D16103", "#F4EDCA",  "#52854C",  "#C3D7A4", "#293352","#4E84C4")


# Main colors using LGBTQ colors #Lobby ;-) 

ColorD2 <- palette_lgbtq("bisexual")

ColorZ <- cbPalette[c(8,6)]
ColorD <- palette_lgbtq("transgender")[c(1,2)]

# Flextable defaults font, fontsize...
set_flextable_defaults(
  font.family = "Arial",         # Required by NHB
  font.size = 8,                 # Main table font
  font.size.header = 8,          # Header font
  font.size.footer = 7,          # Notes font
  padding = 2,                   # Tight layout, single spacing
  line_spacing = 1,              # NHB requires single spacing
  align = "center",              # Default for headers
  border.color = "black",        # Clean black borders
  digits = 3,                    # Useful for estimates
  decimal.mark = ".",            # Ensure English format
  na_str = ""                  # How missing values appear
)

# set fixest to adjust Dof for pointwise p-value in every clustered estimations
setFixest_ssc(ssc(adj = TRUE))

#GGplot default theme
theme_main <-   theme_bw()+  
  theme(text = element_text(family = "Arial",size=7),
        legend.position = "bottom",
        plot.title=element_text(hjust = 0.5,margin=unit(c(0, 0, .6, 0), "cm"),size=7, 
                                face="bold",family = "Arial"),
        plot.subtitle=element_text(hjust = 0.2,margin=unit(c(0, 0, .3, 0), "cm"),size=6),
        plot.caption = element_text(hjust = 0, size=5,family = "Arial"),
        #axis.text.x = element_text(angle = 45),
        axis.text.x = element_text(angle = 0, size=5,family = "Arial"),
        axis.title.y = element_text(angle=90, size=5,family = "Arial"),
        strip.background = element_rect(fill = "lightgrey"),
        plot.margin = unit(c(0, 0, 1, 0), "cm")
  )

theme_nhb <- theme_minimal(base_family = "Arial") +
  theme(
    legend.position = "bottom",
    text = element_text(size = 5),      # main body text
    strip.text = element_text(size = 7),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 5),
    plot.tag = element_text(size = 7, face = "bold"),
    strip.background = element_rect(fill = "lightgrey"),
    plot.tag.position = c(0, 1) ,        # top-left panel tag
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )


theme_set(theme_nhb)

SourcesStacked <- "stacked database of pairwise comparisons."
SourcesMain <- "endline database."

# save_nhb <- function(plot, filename, cols = 1, height_cm = 8.9) {
#   width_cm <- switch(cols,
#                      `1` = 8.9,
#                      `1.5` = 12,
#                      `2` = 18.3,
#                      stop("cols must be 1, 1.5, or 2"))
#   ggsave(filename, plot = plot,
#          width = width_cm, height = height_cm*cols, units = "cm",
#          device = cairo_pdf)
# }

save_nhb <- function(plot, filename, cols = 1, height_cm = 8.9, format = "pdf") {
  # Calculate width based on columns
  width_cm <- switch(cols,
                     `1` = 8.9,
                     `1.5` = 12,
                     `2` = 18.3,
                     stop("cols must be 1, 1.5, or 2"))
  
  # Determine device and file extension based on format
  device_info <- switch(tolower(format),
                        "pdf" = list(device = cairo_pdf, ext = ".pdf"),
                        "eps" = list(device = cairo_ps, ext = ".eps"),
                        "jpg" = list(device = "jpeg", ext = ".jpg"),
                        "jpeg" = list(device = "jpeg", ext = ".jpg"),
                        stop("format must be 'pdf', 'eps', 'jpg', or 'jpeg'"))
  
  # Add file extension if not present
  if (!grepl("\\.[a-zA-Z0-9]+$", filename)) {
    filename <- paste0(filename, device_info$ext)
  }
  
  # Save the plot with conditional DPI for raster formats
  if (tolower(format) %in% c("jpg", "jpeg")) {
    ggsave(filename, plot = plot,
           width = width_cm, height = height_cm * cols, units = "cm",
           device = device_info$device, dpi = 300)
  } else {
    ggsave(filename, plot = plot,
           width = width_cm, height = height_cm * cols, units = "cm",
           device = device_info$device)
  }
}

p_load(magick)
# Save flextable objects as jpg.
flextable_to_jpg <- function(ft, filename, width = 8.9, height = 8.9, res = 300) {
  # Save as PNG first
  temp_png <- tempfile(fileext = ".png")
  save_as_image(ft, path = temp_png, width = width, height = height, res = res)
  
  # Convert PNG to JPG using magick
  
  img <- image_read(temp_png)
  # Add white background (JPG doesn't support transparency)
  img <- image_background(img, "white")
  image_write(img, path = filename, format = "jpeg", quality = 95)
  
  # Clean up temp file
  unlink(temp_png)
}


