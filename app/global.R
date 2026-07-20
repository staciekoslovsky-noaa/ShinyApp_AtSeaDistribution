# basic shiny packages needed
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)

# packages for spatial visualization on map
library(leaflet)
library(leaflet.extras)
library(mapview)

# tools for data processing
library(sf)
library(tidyverse)

#misc. tools
library(htmltools)
library(tools)
library(zip)

library(RColorBrewer)
library(viridis)


# Initialize POPhex_MCMC (used for later custom area analysis)
species_codes <- read.csv("data/Species_codes.csv")
loaded_shapefiles <- read.csv("shapefiles/shapefiles.csv")

# ========================= helper_functions =========================

# =============== about tab ===================
about_title <- div("Integrating Diverse Datasets to Understand the Seasonal Distributions and Densities of Marine Mammals", style = "color: #03396c")

about_info1 <- div("Understanding seasonal distributions and densities of marine
                   mammals remains a high priority for NMFS science centers and 
                   regional offices. In addition to answering basic ecological
                   questions, such information is frequently requested of
                   science center staff to help in calculation of “takes” under
                   the Marine Mammal Protection Act (MMPA) and the
                   Endangered Species Act (ESA)
                   (e.g., due to offshore energy development,
                   fisheries interactions,naval exercises, aquaculture
                   permitting, pier construction, or hazard remediation).",
                   style = "color: #005b96")

about_info2 <- div("Calculation of at-sea densities remains challenging,
                   however, as many Arctic marine mammal surveys are timed
                   to occur during spring or summer months when animals are
                   the most accessible (and often on land in the case of
                   many pinniped species). In practice,there is a large amount
                   of data that may be leveraged to provide estimates of
                   seasonal densities, including scientific surveys,
                   satellite telemetry, acoustic detections,
                   Alaska Native subsistence harvests, and
                   platform-of-opportunity (POP) observations. Seasonal species
                   distribution and density maps inevitably require fitting a
                   statistical model to one or more of these data sources,
                   and to use the fitted model to make spatially referenced
                   predictions. Our goal with this tool is to share such
                   predictive maps with interested stakeholders.
                   We anticipate more maps being added over time as increasingly
                   sophisticated models are applied to individual species.",
                   style = "color: #005b96")

about_info3 <- div("Marine spatial planning requires knowledge of the timing and
                   location of marine mammal distribution, migrations,
                   density in local areas, and movements to mitigate
                   anthropogenic impacts on protected species. The ability to
                   combine data from surveys, POP, telemetry, and passive
                   acoustic recorders will shrink spatial-temporal data gaps
                   and provide managers with a product that represents
                   the best scientific information available. This more complete
                   information can then be used to assist in NEPA analyses,
                   ESA Section 7 consultations, MMPA Incidental Harassment
                   Authorizations, and other permit and management needs.
                   Further, because the gaps present in the data proposed for
                   development of this toolbox are not unique to the 
                   Alaska Region, the framework developed here will have broad
                   applicability beyond Arctic ecosystems and for all 
                   NMFS science centers and regional offices.",
                   style = "color: #005b96")

about_info4 <- div("The potential benefits of these analytical approaches are
                   expected to reach well beyond just NMFS.
                   Other federal and state agencies, tribal governments and
                   Alaska Native organizations, non-governmental organizations,
                   and a variety of industry and community stakeholders seek the
                   type of information products that this project is targeting.
                   Information about seasonal patterns of marine mammal density
                   distributions and habitat use will help these groups to
                   better assess potential impacts, implement mitigation actions
                   where appropriate, and develop plans that are fully informed
                   by science. Examples of specific federal agencies that have
                   expressed a desire for access to better information on the
                   seasonal locations and densities of marine mammals include
                   BOEM and the U.S. Navy.",
                   style = "color: #005b96")


# ============== how to use tab ====================
tool_info1 <- div("This tool was developed using Shiny, a package that facilitates web app development directly from coding languages, such as R.")
tool_info2 <- div("To access species density maps, use the Select Species sidebar panel. Use the Select Marine Mammal dropdown menu to select a species.")

tool_descript1 <- div(
                      p("The toolbar on the left of the map contains various
                        tools to work with select data shown on the map
                        (for instance to perform small area calculations,
                        as when the user is interested in the number of animals
                        or fraction of a population in an area of interest.", 
                        tags$ul(
                          tags$li("polygon, rectangle, and circle options on the toolbar can be used to draw shapes around areas of interest."),
                          tags$li("trash bin can be used to delete any shape that is no longer desired.")
                        ),
                        br(),
                        "The Custom Area Analysis sidebar panel allows users to download a shapefile
                        of the drawn polygons including summary statistics and upload a user's own shapefile
                        for analysis."))

  tool_descript2 <- div(
                      p("The legend can be customized using the 'Select Legend'
                      option in the sidebar. This can be useful in visualizing
                      changes in abundance across a map, particularly when
                      species are clustered in small areas. The options are:",
                      tags$ul(
                              tags$li("'Quintiles' divides them into the following percentiles: 0, 0.2, 0.4, 0.6, 0.8, 1"),
                              tags$li("'Low and High Density Emphasis 1' divides them into the following percentiles: 0, 0.01, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.99, 1"),
                              tags$li("'Low and High Density Emphasis 2' divides them into the following percentiles: 0, 0.05, 0.1, 0.5, 0.9, 0.95, 1"),
                              tags$li("'Low Density Emphasis' divides them into the following percentiles: 0, 0.01, 0.05, 0.6, 0.8, 1"),
                              tags$li("'High Density Emphasis' divides them into the following percentiles: 0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1"))))

relative_description <- div(h4("Relative"),
                            p("Some data sets in this application are based on relative abundance values.
                              With these, the cells show the estimated relative abundance per the cell area, listed
                              below the map.", br(),br(),
                              "If you have an abundance estimate, you can enter this in the Abundance Estimate panel, which updates the map
                              to show estimates per cell."
                            ))

absolute_description <- div(h4("Absolute"),
                            p("Other data are absolute datasets, meaning that all cells represent the abundance estimates
                            for the specified area.", br(), br(),br(),
                            "Because the dataset is an abundance estimate, the Abundance Estimate panel is disabled when a species
                            with this data type is selected."))

spatial_description <- div(h4("Spatial"),
                          p("The spatial data in these datasets represent either a calculated proportion of a
                          species (relative), or an observation of the number of animals (absolute) in a certain physical location (cell)")
                          )

temporal_description <- div(h4("Temporal/Spatiotemporal"),
                            "The temporal datasets show the changes in location over a specific amount of time (varies by species)."
                            )

tool_descript3 <- div(
                      p("Once a custom shape is drawn (using the draw toolbar), selected, or uploaded (in the Custom Area Analysis panel),
                        the generate button at the top of the tab can be pressed, to calculate species specific distribution estimates for the given area.
                        The results from these calculations can be seen in the Analysis Results tab."),
                      
                      p("The results will show either a relative abundance estimate (for relative datasets), or an abundance estimate (for absolute datasets)."),
                      
                      p("For a relative abundance data set, a user can specify:"),
                      tags$ul(
                        tags$li("A population-wide abundance estimate"),
                        tags$li("A coefficient of variation value (CV)")
                      ),
                      
                      p("to generate a histogram and more detailed results. If no CV value is input, the default value is 0.2.")
                    )

# ============== methods tab ==================
methods_title <- div("Methods", style = "#011f4b")

methods_info1 <- div("The statistical approaches for different data integration
                     sub-projects differ depending on species and the types of
                     data available. For select cetacean stocks, POP
                     constitutes the only data available and existing models
                     developed with previous NMFS Protected Resources Toolbox
                     funding can be applied directly to estimate species 
                     distributions (Ver Hoef et al., 2021). For stocks with
                     aerial survey data only (e.g., eastern Bering Sea beluga whales), 
                     spatially explicit maps are available for years and times surveys were 
                     conducted (typically summer).  For other species with more data sources 
                     (e.g., bearded seals,
                     Cook Inlet beluga whales), partially or fully integrated
                     species distribution models can be used to estimate seasonal
                     distributions, as described in
                     Conn et al. (In prep) and presented at the 2023 PSAW
                     conference. These models work similarly to fisheries
                     stock-assessment models with inference conducted using a
                     product log-likelihood, assuming that each data source is
                     attempting to “sample” the underlying species distribution.
                     At present, POP-derived relative abundance surfaces are available
                     for most species, with the exception of bearded seals and EBS belugas.
                     We anticipate more maps are expected to be added in the future.
                     Note that the POP estimates are produced using observations
                     obtained between May and September, so only represents
                     spatial distributions for the “ice free” period.",
                     style = "color: #005b96")

methods_info2 <- div(
                     p("The estimates are generated using platform-of-opportunity (POP) analyses from Jay Ver Hoef."),
                     p("Posterior means are taken from the resulting Bayesian
                       analysis for each hexagon. The sum of these cells within
                       the selected area is taken as  a relative abundance 
                       estimate, ranging between 0 and 1. Uncertainty in this
                       quantity is propagated by taking quantiles or calculating
                       coefficient of variation (CV) from posterior (MCMC) samples."),
                     p("When an abundance estimate and CV value is inputted by a
                       user, a more in-depth estimate can be generated.
                       The relative abundance estimate (a proportion) is 
                       multiplied by the absolute abundance estimate to generate
                       an absolute abundance estimate for the selected area.
                       Additionally, the user-input CV, which indicates the
                       uncertainty in the user's inputted abundance estimate
                       of the entire area, can be propagated into the small
                       area estimate using Goodman's exact formula:"),
                     h5("$$Var(XY) = \\mu_x^2 Var(Y) + \\mu_y^2 Var(X) + Var(X) Var(Y)$$"),
                     p("where"),
                     tags$ul(
                             tags$li("\\(\\mu_x\\): inputted user abundance;"),
                             tags$li("Var(X): calculated by multiplying the user inputted abundance and the coefficient of variation and then squaring;"),
                             tags$li("\\(\\mu_y\\): summed relative abundance over the area of interest (posterior mean);"),
                             tags$li("Var(Y): variance of the sum of MCMC chains over the filtered area.")),
                     p("This will then provide the new variance that takes into
                       account both the uncertainty in the user inputted data
                       and the POP analyses. It is converted to a coefficient 
                       of variation value for interpretability. Note that if
                       a value is NOT provided, the CV will default to 0.2."),
                     p("A histogram is also displayed, conveying uncertainty in the small area abundance estimate. In particular, we assume a log normal sampling distribution."),
                     style = "color: #005b96")

methods_info3 <- div(
                     p("The estimates are generated using partially integrated models from Conn et al. (unpublished)."),
                     p("Seasonal models (summer: June-November; winter: December-May) were
                       fitted to data from aerial surveys, satellite tagged seals, and acoustic
                       detections to produce absolute abundance predictions for 2004-2021. Estimates
                       were generated via mimization of a joint likelihood function, with standard
                       errors generated using the associated Hessian matrix. Note that Conn et al.
                       recommend caution regarding uncertainty as estimates are almost assuredly too precise."),
                     style = "color: #005b96")

methods_info4 <- div(
                     withMathJax(
                       p("The estimates are generated using spatially-explicit density surface models,
                         as described in Ferguson et al. (2025) and Conn et al. (2026)."),
                       p("Estimates represent equal-weight ensembles from multiple models fitted
                          to data that have different spatial autocorrelation structures. Each individual
                          model is fitted using maximum marginal likelihood, with standard errors generated
                          using the associated Hessian matrix.  Standard errors for the ensemble estimate
                          in a specific grid cell", em("s"), "is generated
                          using the standard formula $$\\hat{SE}(\\hat{N}_s) = \\sum_m \\sqrt{\\textrm{Var}(\\hat{N}_{ms}|M_m)+(\\hat{N}_{ms}-\\hat{N}_{es})^2},$$
                          where", em("m"), "indexes model and \\(\\hat{N}_{es}\\) is the model-averaged (i.e. ensemble) estimate.")),
                       style = "color: #005b96")

methods_info5 <- div(h3("Custom-area calculations"),
                     withMathJax(
                       p("We use different methods for custom-area calculations depending on 
                          whether spatial predictions are made using Bayesian or likelihood-based
                          inference.  For Bayesian predictions, MCMC chains are stored internally in
                          the within our Shiny app, and we simply calculate posterior moments
                          (e.g., mean, mode) and variance for the collection
                          of cells that encompass the user-identified polygon."),
                       p("Computation for likelihood-based estimates is somewhat more complicated.
                          The total abundance estimate for a given polygon \\(\\mathcal{R}\\) is simply the sum of 
                          cell-specific estimates, but variance and confidence intervals require
                          summing over both variances and covariances:
                          $$\\widehat{\\textrm{Var}}(\\hat{N}_\\mathcal{R}) = 
                           \\sum_{s_i \\in \\mathcal{R}} \\widehat{\\textrm{Var}}(\\hat{N}_{s_i}) + 
                           2 \\sum_{s_j \\in \\mathcal{R}, s_k \\in \\mathcal{R}, s_j<s_k} 
                           \\widehat{\\textrm{Cov}}(\\hat{N}_{s_j},\\hat{N}_{s_k}).$$
                           The issue is that for large surfaces it may be prohibitively difficult to 
                           calculate \\(\\widehat{\\textrm{Cov}}(\\hat{N}_{s_j},\\hat{N}_{s_k})\\) for 
                           all potential pairs \\(s_j\\) and \\(s_k\\), and even if possible, storing
                           this matrix can potentially require multiple gigabytes of memory. Instead, 
                           we rely on the following approximation, which takes advantage of a spatial 
                           auto-correlation function \\(\\rho()\\): 
                          $$\\widehat{\\textrm{Var}}(\\hat{N}_\\mathcal{R}) = 
                           \\sum_{s_i \\in \\mathcal{R}} \\widehat{\\textrm{Var}}(\\hat{N}_{s_i}) + 
                           2 \\sum_{s_j \\in \\mathcal{R}, s_k \\in \\mathcal{R}, s_j<s_k} 
                           \\hat{\\sigma}_{s_j}\\hat{\\sigma}_{s_k}\\rho(d_{jk})).$$  In particular,
                           we assume that an exponential correlation function is used, which expresses
                           realized correlation as a function of \\(d_{jk}\\), the distance between the centroids
                           of grid cell ",em("j")," and grid cell ",em("k")," as well as spatial range parameter.
                           Each dataset has spatial range parameters uploaded (based on variogram analysis),
                           and the distances between grid cells are computed by the Shiny app.  This computation 
                           should be fast for small custom-area polygons but may take some time for large ones. 
                           We then assume a lognormal sampling distribution to produce confidence intervals.")
                     ),
                     style = "color: #005b96")

# Reference tab
licenses <- div(p("NOAA data is available under the CC-BY-4.0 license, which
                  allows for unrestricted use. User must read and fully
                  comprehend the metadata prior to use. Applications or
                  inferences derived from the data should be carefully
                  considered for accuracy. While every effort has been made
                  to ensure that these data are accurate and reliable within
                  the limits of the current state of the art, NOAA cannot
                  assume liability for any damages caused by any errors or
                  omissions in the data, nor as a result of the failure of
                  the data to function on a particular system. NOAA makes no
                  warranty, expressed or implied, nor does the fact of
                  distribution constitute such a warranty. Acknowledge
                  NOAA/NMFS/AFSC or the specified citation as the source
                  from which these data were obtained in any publications
                  and/or other representations of these data.
                  Communication and collaboration with dataset authors is strongly encouraged."),
                style = "color: #005b96")