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


<span style="color:blue">06/26/24:</span>

- Manipulating sample data 
- Code review
  - `reactiveVal()` and `reactiveValues(a= defaultval, b...)` -> `observeEvent(input$update_a, {values$a <- values$a + 1})`
    - output
    

<span style="color:blue">06/27/24:</span>

- More code review
- Meeting to go over plan
- Leaflet vs. Mapbox GL
  - Mapbox has more visualization options, much more complex than simple Leaflet options
    - supports heatmaps, 3D extrusions, etc (Leaflet does not)
  - Leaflet is open-source with good documentation (R lib available), but no longer developed (no updates in future)
  - Vector maps support: Mapbox Y, Leaflet N (as in new maps it seems?)
  - Leaflet ultimately still more popular as of now and **easier to integrate with Shiny** 
  - Overall it seems that if the ultimate goal is to make interactive maps without also creating the base map, Leaflet is sufficient. If more complex/more large datasets -> mapgl may be the move
    - use with `install.packages('mapdeck')`
    - maybe try using both? -- offer both options? interesting possibility but may be difficult, not sure

  
<span style="color:blue">06/28/24:</span>

- working on UI
- basic graphics, theme, text, etc. 
- Added the mapdeck consideration stuff as possibility in sidebar
  - sidebar pretty empty for legend etc later
- [sf cheatsheet](https://github.com/rstudio/cheatsheets/blob/main/sf.pdf)


<span style="color:blue">07/01/24:</span>

- making UI of shiny app (getting the actual shiny app up and running) with 
- Dropdowns (UI & server), map stuff in main panel (not accurate yet)
- Currently trying to integrate the sample data into the map and work with the select species dropdown
  - bit of trouble with wrapping, fitting data - also need legend to add for colors
  - others are mainly placeholders
  
  
<span style="color:blue">07/02/24:</span>

- Dateline issue unresolved but left it for now - ` BS_grid_sf <- st_wrap_dateline(BS_grid_sf, options = c("WRAPDATELINE=YES"), quiet = TRUE)` / `BS_grid_sf$geom <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)` 
- new map ea time vs. base map - efficiency
- Shiny intern meet up - exchanged code info and possible ideas (tabs vs. dropdown/etc)


<span style="color:blue">07/03/24:</span>

- Rewrote some code accessing the data due to inefficiency
- Dropdown button works for selecting species-specific map
  - Some data will still likely need to be fixed afterwards (relative v. absolute abundance, etc)
- Trying to add a collapsible sidebar; requires different UI style.
  - `fluidPage()` is the basic layout, `dashboardPage` will provide a more accessible dashboard and more complex applications. *requires extra library - `shinydashboard`
  - [Tool](https://rstudio.github.io/shinydashboard/structure.html)
  - Finished elementary implementation


<span style="color:blue">07/05/24:</span>

- Changed mapview (Zoom vs. set view) -> can be changed later to be by data not by set size
- Added buttons on maps of select regions (polygon/circle/etc), similar to demo; part of leaflet extras library
- looked at new files uploaded on Git
- Revisiting dateline issue
  - https://github.com/rstudio/leaflet/issues/553, 
  - Finally resolved; `st_wrap_dateline` ineffective
- Next: Revisit longlat marker detail and the new data


<span style="color:blue">07/08/24:</span>

- Added legend, but need to create unified color palette for all graphs/have unique palette for specific graphs
- Working on getting the polygons to output something (currently not download data, just simple output)
- Loading data [tools package](https://stackoverflow.com/questions/29113973/get-filename-without-extension-in-r)


<span style="color:blue">07/09/24:</span>

- Wrote a function to more efficiently load all the data and put into new helper functions file
- New data set to distinct appropriate var names - all matrices (11424) relative abundances
- Bind each row in sf file (hexagons to rows) to one column (v1)
  - Typically take means/standarddev, etc
  - Just disregard other cols for now
  - Use `cbind` (dplyr)
  - Hexagonal vs. grid based data - all new data seems to be hexagonal layers
  - `cu_mat_col <- CU_MCMC[,1]`
  - `cu_df <- as.data.frame(cu_mat_col)`
  - `combined_cu <- cbind(POP_hex_sf, cu_df)`
  - `print(combined_cu)` check 
  - `ggplot()+geom_sf(data=Sample_data[["POP_hexagons_sf"]],aes(fill=CU))`

  
<span style="color:blue">07/10/24:</span>

- Source for generating shapelist, intaking shape csv [link](https://stackoverflow.com/questions/65347690/how-do-i-save-adddrawtoolbar-shapes-drawn-in-an-r-leaflet-shiny-map-so-i-can-re)
- Adding marker now provides lat and long coordinates
- Polygons now can output/print something, no data actually outputted yet
- select vs selectize as number of species grows
- Keep POPhexagons_sf as one file. Any modifications will be done to MCMC likely so do not create separate dataframes for each species in POPhex
- `column()` and `wellPanel()`


<span style="color:blue">07/11/24:</span>

- filtering by species where it is not NA and then filling based on the MCMC data
- Graphed sample of few new species
- [HTML Tools](https://unleash-shiny.rinterface.com/htmltools-overview)
- Code review today 
-   Consider log scale for the legend - assign define brightness - 
-   Move the select species on the right 
-   Do all survey areas, not just not NA
-   reactive legend - playing with breaks in data
-   zipped shapefile upload - .kml/.kmz 
-   commenting file 


*no updates for 07/12/24 (minor changes reflected in 07/15)*


<span style="color:blue">07/15/24:</span>

- Adding informational text in sidebar using project proposal 
  - [Color palette](https://www.color-hex.com/color-palette/1294)
  - [More UI](https://algoritmaonline.com/advancing-your-shinyapp/#:~:text=Here's%20the%20detailed%20documentation%20on,Below%20is%20an%20example:)
- More attempts on fixing download/upload shapefiles
- Legend fixed, yet colorshading remains possibly something to be fixed
  - Depends on how we want to modify data/new fillColor/etc
- Code review tomorrow


<span style="color:blue">07/16-17/24:</span>

- Addressing changes from code review
  - Changed from local download to access via Git. 
    - Issue - takes a considerable time to load - when timed, around 1.5min
    - But once it's open and loaded, switching between maps is not a problem
      - *Current solution*: do not load MCMC unless complex analysis is selected
          - prevents lag/excessive load times for simple features (view, toggle, etc)
  - Improved documentation on code scripts
  - Alphabetize selection on UI 
  - Layer toggle option on map functional
  

<span style="color:blue">07/18/24:</span>

- Added default selection button
- Changed data from MCMC to POPhexagons
    - modified legend to align with this - currently showing absolute abundance
- Check-in today


<span style="color:blue">07/19/24:</span>

- Changes to be made post-meeting
  - reverting some old changes
  - Change back to MCMC data. This involves taking posterior mean (`rowMeans()`) and `cbind()`
    - means of all the V columns, into new dataframe `POPhex_MCMC`
  - store relative abundance separately so only one dataframe rather than many 
- Adding the new dataframe into maps, modify legend and etc accordingly 


<span style="color:blue">07/22/24:</span>

- Making legend based off of quartiles to obtain a better color gradient
- .csv file for `POPhex_MCMC` added to Git, trouble with reading on app
- Working on map quality - zoom and NA cells 


<span style="color:blue">07/23/24:</span>

- fillOpacity and Opacity, along with fillColor and color, using same palette
  - change from previous static border opacity and color
  - much improved view with changes
- Opacity of 0.7 or so improves resolution; 0.4 or below makes it difficult
- Previous bug found --> legend and data are disconnected;
  - linked using `species_data` value succesfully
  - [REFRESHER](https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent)
    - reactive allows variables to be accessed OUTSIDE the "reactive"/isolated environment
      - "lazy" v. observe - not "lazy", executes right away when dependencies are changed, even 
        if it directly is not called whereas reactive waits until called
    - in contrast, observeEvent does NOT; thus if you have an object defined
      within the observed event, it will not show up
- `fillOpacity = get_opacity(as.vector(survey_polygons$abund_est), abund_bins)` from Harbor Seal Ap
- solution to resolution?? -> smoothfactor


<span style="color:blue">07/25/24:</span>

- Finally uploads shapefile
  - Takes zipped file and shows onto map 
- Changed color theme to be more natural/toned down - previously inferno
- Offering various breaks in non-linear legend to choose from
- Meeting check-in today


<span style="color:blue">07/26-29/24:</span>

- Plan for now -> adding the abundance estimate option
  - Successfully accomplished
- Working on download button. 


<span style="color:blue">07/30/24:</span>

- Downloading shapefile works - `st_write` does heavylifting 
- Updating certain aesthetic components in UI
  - Adding selection option for palettes to explore variety of palettes in 
    showing the map data accurately
  - Changing name of certain tabs
  - Adding description of how to use the tool in the How to Use tab
  - Adding extra button for standard error (does not do anything yet)
- Note timing of each
  - Switch species: Ranges between 7- 11 seconds
  - Switch legend: Ranges between 6-9 seconds
  - Switch total abundance value: Depends on size of value
    - 10,000 - 10 sec
    - 50,000 - 11 sec
  

<span style="color:blue">07/31/24:</span>

- Adding informational text to the tabs to make the map tool accessible


<span style="color:blue">08/02/24:</span>

- Changing the organization of the website
  - Additional options no longer all listed and visible on sidebar (clutter)
  - `library(shinyBS)` for collapsible panels 
  - Now moved to collapsible panels with one larger panels (additional options)
  - Modified palette options and provided reverse option (color-blind friendly)
- Planning out abundance calculations work
  - New script (custom_area_analysis.R) added for calculations
- Uninstalled and reinstalled R--seemed to fix load time issue
  
  
<span style="color:blue">08/5-7/24:</span>

- `POPdata_with_MCMC` for data with MCMC (avoiding rewriting data)
- 