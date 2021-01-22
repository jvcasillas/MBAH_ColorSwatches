# Author: Martina Zaharieva
# Aim: Generate CIELUV color samples with equal hue spacing, chroma and lightness values for Project Many Babies At Home. 

# Install and load packages in library. 
# install.packages("colorspace")
# install.packages("rstudioapi")
library(colorspace) # Allows to simulate color vision deficiency using the simulate_cvd function.
library(tidyverse)
library(rstudioapi)

# GENERATE N EQUILUMINANT COLORS ----

# Compute N amount of colors from a different hue, while keeping luminance and chroma fixed.
# HCL values represent polar coordinates in CIELUV space of existing RGB values with a reference
# to a grey value: #b0aca9 at 70% luminance. 
stim_colors <- hclplot(qualitative_hcl(n = 8, c = 50, l = 70, fixup = TRUE))
plot(stim_colors)

# Save the resulting matrix into a dataframe. Compute the difference in hue between adjacent values.
stim_colors_hex <- as.data.frame(stim_colors) %>% 
  as.data.frame(hex(polarLUV(L = stim_colors_hex$L, C = stim_colors_hex$C, H = stim_colors_hex$H), fixup = FALSE)) %>%
  tibble::rownames_to_column(., var = "hex_code") %>%
  # Append grey value (same luminance) used for background stimulus display color:#b0aca9  
  dplyr::bind_rows(tibble(hex_code = "#b0aca9")) 

# Export as .csv.  
write.csv2(stim_colors_hex, file = 'MBAH_colors.csv', sep = ",", dec = ".", col.names = TRUE, row.names = FALSE)