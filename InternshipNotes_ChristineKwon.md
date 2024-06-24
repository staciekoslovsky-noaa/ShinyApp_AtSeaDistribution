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
