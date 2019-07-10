#' Function creates the colour palette for plotting the temperature bars
#'
#' @param colors a vector of colours for a colour scale ranging from low to high
#' (Default is colors <- c("darkturquoise", "deepskyblue", "deepskyblue1",
#' "deepskyblue1", "deepskyblue2", "deepskyblue2", "deepskyblue3", "maroon1",
#' "firebrick1") and was chosen to be consistent with the original reference
#' artwork)
#' @param pal_len The length of the temperature palette returned.
#' (Default is 100)
#'
#' @return A colour paletter in vector form
#'
create_colour_palette <- function(colors, pal_len = 100){
  if(missing(colors))
    colors <- c("darkturquoise", "deepskyblue", "deepskyblue1","deepskyblue1", "deepskyblue2", "deepskyblue2", "deepskyblue3", "maroon1", "firebrick1")
  temp_ramp <- colorRampPalette(colors)
  temp_palette <- temp_ramp(pal_len)
  return(temp_palette)
}
