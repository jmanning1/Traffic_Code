library(tmap)
library(tidyverse)
library(sf)
uk = rnaturalearth::ne_countries(scale = 50) %>% 
  st_as_sf() %>% 
  filter(grepl(pattern = "United Kingdom", x = name_long))
plot(uk)
st_bbox(uk)

library(raster)
r = raster(as(uk, "Spatial"), ncols = 100, nrows = 100)
values(r) = runif(10000)
plot(r)

x = runif(n = 100, min = -8, max = 2)
y = runif(n = 100, min = 50, max = 60)
df = data.frame(
  n = 1:100,
  x = x,
  y = y
)
dfsf = st_as_sf(df, coords = c("x", "y"))

tm_shape(r) +
  tm_raster() +
tm_shape(uk) +
  tm_polygons(alpha = 0.1) +
  tm_shape(dfsf) +
  tm_dots(alpha = 0.9, size = 5) # add transparency

st_crs(dfsf) = 4326
uk = st_transform(uk, 27700)
dfsf = st_transform(dfsf, 27700)

dfsf$year = rep(1:10, 10) 

# illustrate facet
tm_shape(uk, bbox = st_bbox(uk)) +
  tm_polygons() +
  tm_shape(dfsf) +
  tm_dots() +
  # tm_shape(roadnet) +
  # tm_lines()
  tm_facets(by = "year", ncol = 10)

citation()

#' @Manual{rcore,
#'   title = {R: A Language and Environment for Statistical Computing},
#'   author = {{R Core Team}},
#'   organization = {R Foundation for Statistical Computing},
#'   address = {Vienna, Austria},
#'   year = {2018},
#'   url = {https://www.R-project.org/},
#' }

citation(package = "spatstat")
# \footnote{See www.ldkjfd.com} - for blog posts, random code, links.

# histogram
# ?geom_histogram
names(diamonds)
ggplot(diamonds, aes(carat, fill = color)) +
  geom_histogram(bins = 200) +
  facet_wrap(facets = aes(color)) +
  scale_fill_manual(values = rep(c("red", "yellow", "green"), length.out = 7))
