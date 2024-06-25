Internship Notes: Christine Kwon

<span style="color:blue">06/24/24:</span>

- Cloned git repo locally, RStudio set up, relevant packages installed
- Went through R markdown file exploring and playing with sample data
- Some useful functions:
  - `ggplot(map) + geom_sf(color = "white", aes(fill = some_data)) + theme(legend.position = "none")`
  - `ggplot() + geom_sf(data = BS1, aes(fill = some_data))`
- Prior to code review, read Shiny App documentation/structure/basics [Shiny Basics Lesson 1](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html), [Addtl Resource 2](https://www.paulamoraga.com/book-geospatial/sec-shiny.html)
- `app.R` components: `UI` object, `server` function, and call to `shinyApp` function
  - `ui <- fluidPage()`
  - `server <- function(input, output)` contains instructions for the app
  - `shinyApp` function creates objects from explicit `UI`/`server` pair
  - `runApp('appdir_path')`
- Ran Shiny with built-in sample Shiny library data
- Glanced at prev. code in HarborSealAbundance_Functions.R
  - dplyr rev:
    - `group_by_at()`: group by specified variables
    - `arrange()`: orders data frame rows
    - `mutate()`: add new or change variables
    - `ungroup()`,`filter()` recall scrnaseq soup cleanup, `select()`, `summarise()`, `inner_join()`
  - tidyr


<span style="color:blue">06/25/24:</span>

- Focused on working w/ spatial data -> [Leaflet](https://rstudio.github.io/leaflet/), [Prac](https://rpubs.com/velshnia/geospatial) and Projections
- Basic leaflet map: `leaflet()` init map object, `addTiles()` adds map layers
`leaflet(df) %>%` 
  `addTiles() %>%` *default tiles v. polygons*
  `addMarkers(lng = val, lat = val2, popup = "marker name")`
  `addPolygons()` for specified regions/areas/boundaries etc
  - or, given some custom crs, `leaflet(options = leafletOptions(crs = custom_crs)) %>%` ... with `custom_crs <- leafletCRS(crsClass, code, proj4def, resolutions)`
- Projections
  - `shapes <- st_read(shapefile)` vec data, check w `st_crs(shapes)`
  - `data <- raster(rasterfile)`, check w `crs(data)` (vector data v. raster data)
  - `projectRaster(exenvironmentdata, crs = CRS("+init=epsg:4326"))`
  - WGS84 (4326), Albers Equal Areas projection (3338)
    - WGS84 is gcs w lat and long, used in GPS/global datasets
    - Albers (3338) is projected coord system good for high lat (ex: Alaska) 
    - the actual computation - helped by sf package `result <- st_transform(vectordata, crs = 4326)`
  - What projection to use, how to choose
- [Projections in leaflet](https://rstudio.github.io/leaflet/articles/projections.html)
