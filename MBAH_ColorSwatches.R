# Author: Martina Zaharieva
# Aim: Generate CIELUV color samples with equal hue spacing, chroma and lightness values for Project Many Babies At Home. 

### BACKGROUND ###

# CIE terminology Commission Internationale de L'Eclairage (CIE) (2011). International Lighting Vocabulary. 
# CIE S 017/E:2011. Searchable online at http://eilv.cie.co.at/.]
# Hue - numerical representation of the color categories (yellow, green, etc.) ranging from 0 to 359. 
# Chroma - attribute of a visual perception according to which the perceived colour of an area appears
# to be more or less chromatic; the degree of vividness of a color.
# Saturation - the colorfulness of an area in proportion to its brightness. 
# Colorfulness - colourfulness of an area judged as a proportion of the brightness of a similarly 
# illuminated area that appears white or highly transmitting.
# Chroma and lightness must fit in the sRGB hue range, else the colors will not be displayed as such.

# LAB color space (L - luminance (0-100); A - green (-128) to red (+127); B - blue (-128) to yellow (+127)).
# black = 0,0,0; white = 100,0,0
# LAB is designed to model human visual perception of lightness and since we can distinguish more color 
# in darker than lighter ranges, there are more values reserved for darker areas. 
# A & B - the further away from 0, the more saturated the color. 

# Install and load packages in library. 
install.packages("colorspace")
install.packages("rstudioapi")
library(colorspace) # Allows to simulate color vision deficiency using the simulate_cvd function.
library(tidyverse)
library(rstudioapi)

# Compute an n amount of colors from a different hue, while keeping luminance and chroma fixed.
# HCL values represent polar coordinates in CIELUV space of existing RGB values with a reference greay value: #b0aca9 at luminance = 71
stim_colors <- hclplot(qualitative_hcl(n = 4, c = 70, l = 71, fixup = TRUE))
plot(stim_colors)

# Save the resulting matrix into a dataframe. Compute the difference in hue between adjacent values.
stim_colors_df <- as.data.frame(stim_colors)
stim_colors_df$hue_diff <- stim_colors_df$H - dplyr::lag(stim_colors_df$H, 1)

# Compute descriptives of the hue difference.  
mean(stim_colors_df$hue_diff, na.rm = TRUE)
sd(stim_colors_df$hue_diff, na.rm = TRUE)

# Generate pairs of complementary colors. 
# Compute true complementary hues and find the best matching value within the existing color set. 
closest_complement <- function(hue_vec, target_hue){
  # This function takes two arguments, a vector of hues and the target hue to find the closest match for.  
  difference_scores <- pmin(
    abs((hue_vec + 180) %% 360 - target_hue),
    abs((target_hue + 180) %% 360 - hue_vec))
  
  return(hue_vec[which(difference_scores == min(difference_scores))])
  
}

for (i in 1:nrow(stim_colors_df)){
  stim_colors_df$H_c[i] <- closest_complement(stim_colors_df$H, stim_colors_df$H[i])
}

stim_colors_df <- stim_colors_df %>% 
  select(H_c = H, C_c = C, L_c = L) %>%
  left_join(stim_colors_df, ., by = "H_c")

# Convert the dataframe back into a matrix.
stim_colors_matrix <- polarLUV(L = stim_colors_df$L, C = stim_colors_df$C, H = stim_colors_df$H)
plot(stim_colors_matrix)
stim_colors_matrix_c <- polarLUV(H = stim_colors_df$H_c, C = stim_colors_df$C_c, L = stim_colors_df$L_c)

# Map the polarLUV values to hex  (for Photoshop) via sRGB (gamma = 2.4).
# From Photoshop, the stimuli will be saved to RGB without any further color corrections. 
# Background on color space mappings: http://colorspace.r-forge.r-project.org/articles/color_spaces.html
stim_colors_matrix_hex <- hex(stim_colors_matrix, fixup = FALSE)
barplot(rep(1,length(stim_colors_matrix_hex)), col = stim_colors_matrix_hex)
stim_colors_matrix_hex_c <- hex(stim_colors_matrix_c, fixup = FALSE)
barplot(rep(1,length(stim_colors_matrix_hex)), col = stim_colors_matrix_hex_c)

# Covert the two hex matrices into dataframes and join them.
stim_colors_hex_df <- tibble(hex = stim_colors_matrix_hex, hex_c = stim_colors_matrix_hex_c)

# Export as .csv.
write.csv2(stim_colors_hex_df, file = 'MBAH_polarLUV-hex_L_C.csv', sep = ",", dec = ".", col.names = TRUE, row.names = TRUE)