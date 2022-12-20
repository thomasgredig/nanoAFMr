# make LOGO
library(nanoAFMr)
library(ggplot2)
da = AFM.import(AFM.getSampleImages(type='ibw'))
plot(da, graphType=2, trimPeaks = 0.01)
ggsave('man/figures/logo2.png', width=3,height=3)

library(hexSticker) # Create Hexagon Sticker in R
library(showtext)   # Using Fonts More Easily in R Graphs

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Inconsolata", "incon")

sticker(
  # Subplot (image)
  subplot = "man/figures/logo2.png",       # Image name
  s_y = 1.3,                          # Position of the sub plot (y)
  s_x = 1,                       # Position of the sub plot (x)
  s_width = 0.4,                   # Width of the sub plot
  s_height=0.4,                    # Height of the sub plot
  # Font
  package = "nanoAFMr",            # Package name (will be printed on the sticker)
  p_size = 64,                       # Font size of the text
  p_y = 0.6,                        # Position of the font (y)
  p_x=0.85,                         # Position of the font (x)
  p_family = "incon",               # Defines font
  # Spotlight
  spotlight = TRUE,                 # Enables spotlight
  l_y=0.8,                          # Position of spotlight (y)
  l_x=0.7,                          # Position of spotlight (x)
  # Sticker colors
  h_fill = "#5d8aa6",               # Color for background
  h_color = "#2A5773",              # Color for border
  # Resolution
  dpi=1200,                         # Sets DPI
  # Save
  filename="man/figures/logo.png"               # Sets file name and location where to store the sticker
)
