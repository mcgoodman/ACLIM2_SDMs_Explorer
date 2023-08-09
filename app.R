
require("shiny")
require("here")
require("dplyr")
require("ggplot2")
require("shinythemes")

## Read in overlap data, extract species that estimates are available for
overlap <- read.csv("output/overlap_summary.csv")
overlap$bin1[is.na(overlap$bin1)] <- "all"
overlap$bin2[is.na(overlap$bin2)] <- "all"
species1 <- unique(overlap$species1)
species2 <- unique(overlap$species2)

## Options for climate models / scenarios
models <- c("CESM", "GFDL", "MIROC")
scenario_abr <- c("SSP126", "RCP45", "SSP585")
scenario_nm <- c("SSP1-2.6 (CMIP6)", "RCP 4.5 (CMIP5)", "SSP5-8.5 (CMIP6)")

overlap <- overlap |> mutate(
  scenario = ifelse(sim == "hindcast", sim, vapply(sim, \(x) strsplit(x, " ")[[1]][1], character(1))), 
  scenario = factor(scenario, c("hindcast", scenario_abr))
)

## Colors for plots
sim_names <- c(
  "hindcast", "SSP126 CESM", "SSP126 GFDL", "SSP126 MIROC", "RCP45 CESM", 
  "RCP45 GFDL", "RCP45 MIROC", "SSP585 CESM", "SSP585 GFDL", "SSP585 MIROC"
)
sim_cols <- c("black", PNWColors::pnw_palette("Bay", length(sim_names[sim_names != "hindcast"])))

## Colors for ensembled means
ens_cols <- sim_cols[c(1, 2, 6, 10)]

## Global plot theme settings
theme_set(
  theme_bw() + 
    theme(
      axis.text = element_text(size = 12, color = "black"), 
      plot.title = element_text(size = 14, color = "black", face = "bold"),
      axis.title = element_text(size = 14, color = "black"), 
      legend.text = element_text(size = 12)
    )
  
)

linebreaks <- function(n){HTML(strrep(br(), n))}

# Descriptive text used on multiple pages
description <- list(
  models = paste(
    "The moodels used here included two components - a binomial component to fit presence/absence data and",
    "with which to project probability of occurrence / habitat suitability, and a lognormal component",
    "fit to biomass values greater than zero. Covariates were selected separately for each component.",
    "Projections and overlap values can be based on either estimated probability of occurrence (in which case",
    "the estimates from the lognormal model are ignored), or total estimated biomass, i.e., the product of",
    "estimated probability of occurrence and estimated positive biomass. The best-fit model according to time-series",
    "cross validation is described below. The AUC and R^2 correspond to a model fitted to all available data",
    "while the RMSE pertains to the training data and was used to select the best-fit model."
  ),
  centroids = paste(
    "These plots display the latitudinal and longitudinal components of the range centroid (respectively,",
    "the mean location in eastings (UTM zone 2) and northings weighted by either probability of occurrence",
    "or expected biomass (these plots will change depending on which option you choose to compute overlap",
    "using). Empirical means computed directly from the survey data are shown in light purple, along with means",
    "computed from the model estimates only for the survey locations sampled in each respective year in", 
    "dark purple. If this species is expected to occupy a substantial portion of the NBS survey area",
    "during the surveyed years, the model estimates computed only for the surveyed years may differ",
    "substantially from the model estimates for the entire EBS + NBS survey region. Shown are the means",
    "and 95% confidence bands."
  ),
  area_occupied = paste(
    "Area occupied is defined here as the proportion of the survey area (EBS + NBS) for which the predicted",
    "probability of occurrence is greater than 0.5 (this plot will not differ bewteen 'probability of occurrence'",
    "and 'estimated biomass' options). Shown are the means and 95% confidence band."
  ), 
  mahalanobis = paste(
    "Plots of Mahalanobis distance, which is a multivariate metric of environmental novelty",
    "comparing each year's environmental conditions with the multivariate mean and covariance",
    "of the biophyiscal state of the bering sea during years in the observed data, based on the",
    "hindcast. The variables used to guage environmental novelty are those in the best-fit models",
    "and are therefore different for the binomial and lognormal components:"
  ),
  fitted_map = paste(
    "These are maps of fitted vs. observed biomass for selected years. The final model includes annual random",
    "intercepts and spatially-correlated errors. Annual intercepts are reflected in the plots below, but not",
    "spatial errors - i.e., annual variation in average 'brightness' can result from either the fixed effects",
    "listed above or from the annual intercepts, but all spatial variation displayed here is attributable",
    "to the fixed effects."
  ),
  proj_map = paste(
    "These maps display the projected distribution under each scenario and climate model, averaaged across the",
    "years 2070 - 2079 and compared to the 1995 - 2015 average (they are pre-rendered and will not change depending",
    "on the climate models and scenarios selected in the side panel). Because the overlap metrics are invariant under",
    "linear transformations of species abundance / biomass, what is relevant for these overlap metrics is the relative",
    "distribution of biomass in space - so, for biomass, I've plotted the log of the normalized biomass in each panel",
    "(biomass divided by the total). Click 'probability of occurrence' in the side panel to display the projected",
    "probability of occurrence maps. Diamonds display the range centroid on each panel (these can be quite different",
    "between biomass and occurrence maps)."
  )
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("ACLIM2 SDMs: Species range and overlap forecasts"),
  
  # Left align LaTeX math
  tags$head(
    tags$style(HTML("
                    div.MathJax_Display{
                    text-align: left !important;
                    }
  "))
  ),
  
  # Theme adjustments
  tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #EAEAEA;  color:#3C4C58}
    .tabbable > .nav > li[class=active] > a {background-color: white; color:#3C4C58}
  ")),
  tags$style(HTML("body {background-color: #F5F5F5;}")),
  tags$style(".well {background-color: #EAEAEA;}"),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sp1", 
        label = "Species 1", 
        choices = species1, 
        selected = "arrowtooth flounder"
      ), 
      selectInput(
        inputId = "sp2", 
        label = "Species 2",
        choices = species1,
        selected = "walleye pollock"
      ),
      uiOutput("bin1"), 
      uiOutput("bin2"),
      shinyWidgets::radioGroupButtons(
        inputId = "index", 
        label = "Compute overlap indices using", 
        choices = c("Estimated biomass", "Probability of occurrence"), 
        justified = TRUE
      ), #new
      shinyWidgets::checkboxGroupButtons(
        inputId = "scenario", 
        label = "Climate scenarios", 
        choices = scenario_nm,
        selected = scenario_nm, 
        justified = TRUE
      ), #new
      h5(HTML("<b>Climate models<b>")),
      helpText("Note: Ensembling combines estimates from selected models only"),
      fluidRow(
        column(4,
               shinyWidgets::checkboxGroupButtons(
                 inputId = "ensemble", 
                 label = NULL, 
                 choices = "Ensemble",
                 selected = c(), 
                 justified = TRUE
               ) #new
        ), 
        column(8,
               shinyWidgets::checkboxGroupButtons(
                 inputId = "model", 
                 label = NULL, 
                 choices = models,
                 selected = "GFDL", 
                 justified = TRUE
               )
        )
      ), 
      h5(HTML("<b>Download output<b>")),
      fluidRow(
        column(4, downloadButton('downloadOverlap', 'Overlap', style = "width:100%;")), 
        column(4, downloadButton('downloadSp1', 'Species 1', style = "width:100%;")), 
        column(4, downloadButton('downloadSp2', 'Species 2', style = "width:100%;"))
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Overlap", style = "background-color: #ffffff;", fluidPage(
          h2("Background"),
          p(paste(
            "This interface displays fitted means and confidence intervals from SDMs built for a suite", 
            "of eastern Bering Sea groundfish and crab species as part of the ACLIM2 project.",
            "Generalized additive models (GAMs) were built for each species with environmental covariates",
            "selected using time-series cross validation (i.e., by optimizing several-year-ahead predictive",
            "performance). Terms considered in the models include temperature, depth, oxygen, pH, and",
            "spatially-varying effects of the cold pool. Models were fit separately for juveniles and adults",
            "for most species, but not for snow crabs, and red king crab - when plotting these species, choose",
            "'all' for the size bin. Models were fit as delta-GAMs, with a binomial component for estimating",
            "probability of occurrence, and lognormal component for estimating positive (non-zero) biomass.",
            "Overlap projections are plotted below, with model summaries and range projections for individual",
            "species available in tabs to the right."
          )),
          p(paste(
            "Overlap metrics can be computed from either probability of occurrence, in which case only the binomial",
            "component of each delta model is used, or expected biomass (i.e., the product of the expected",
            "values from the binomial and Lognormal model component). Fit is generally much better for the binomial",
            "component (i.e., the models tend to do a better job describing the distribution of encounters / non-encounters",
            "for each species than they do for species biomass distributions), so overlap estimates based on",
            "probability of occurrence only may be more reliable, possibly at the cost of ecological interpetability."
          )),
          h2("Spatial overlap"),
          p(textOutput("overlap_background")),
          h4(textOutput("overlap_title1")),
          uiOutput("overlap_math1"),
          p(textOutput("overlap_description1")),
          fluidPage(
            fluidRow(
              div(
                shinyWidgets::materialSwitch("y_axis", "Y-axis: 0-1", value = TRUE, right = TRUE, inline = TRUE), 
                style = "position: absolute; right: 0; padding: 1% 2%; z-index: 1;"
              )), 
            plotOutput("overlap_plot1")
          ),
          h4("Bhattacharyya's coefficient"),
          withMathJax("$$\\sum \\sqrt{p_x p_y}$$"),
          p(paste(
            "Bhattacharyya's coefficient is a metric of similarity in the fine-scale spatial distributions",
            "of two species:"
          )),
          plotOutput("overlap_plot2"),
          h4("Global index of collocation"),
          withMathJax("$$\\frac{1 - \\Delta CG_{x, y}^2}{\\Delta CG_{x,y}^2 + I_x + I_y}$$"), 
          p(paste(
            "The global index of collocation is a measure of broad-scale geographic similarity in two species",
            "distributions, as a function of each species center of gravity (CG) and dispersion (I):"
          )),
          plotOutput("overlap_plot3"),
          p("Overlap formulas and definitions following Carroll et al. (2019).")
        )), 
        tabPanel("Species 1", style = "background-color: #ffffff;", fluidPage(
          h2("Selected models"),
          p(description$models),
          tableOutput("sp1_mod"),
          h2("Plots"),
          h3("Range centroids"),
          p(description$centroids),
          plotOutput("sp1_northings"), 
          plotOutput("sp1_eastings"),
          h3("Area occupied"),
          p(description$area_occupied),
          plotOutput("sp1_area"),
          h3("Environmental novelty"),
          p(description$mahalanobis),
          plotOutput("sp1_novelty_binom"),
          plotOutput("sp1_novelty_pos"),  
          h3("Map of fitted and observed values"), 
          p(description$fitted_map),
          imageOutput("sp1_fit_pred", height = "100%"), 
          h3("Projected distribution, 2070-2079"),
          p(description$proj_map),
          imageOutput("sp1_proj_map", height = "100%")
        )), 
        tabPanel("Species 2", style = "background-color: #ffffff;", fluidPage(
          h2("Selected models"),
          p(description$models),
          tableOutput("sp2_mod"),
          h2("Plots"),
          h3("Range centroids"),
          p(description$centroids),
          plotOutput("sp2_northings"), 
          plotOutput("sp2_eastings"),
          h3("Area occupied"),
          p(description$area_occupied),
          plotOutput("sp2_area"),
          h3("Environmental novelty"),
          p(description$mahalanobis),
          plotOutput("sp2_novelty_binom"),
          plotOutput("sp2_novelty_pos"), 
          h3("Map of fitted and observed values"), 
          p(description$fitted_map),
          imageOutput("sp2_fit_pred"), 
          h3("Projected distribution, 2070-2079"),
          p(description$proj_map),
          imageOutput("sp2_proj_map", height = "100%")
        ))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## Function to plot spatial overlap
  plot_overlap <- function(data, overlap_index, ylab, plot_sims) { 
    
    data <- data |> filter(index == overlap_index)
    
    if(any(input$ensemble == "Ensemble")) {
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
    } else {
      plot_cols <- sim_cols[which(sim_names %in% plot_sims)]
    }
    
    data |> 
      ggplot(aes(year, mean)) + 
      geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill = sim), alpha = 0.5) + 
      geom_line(aes(color = sim)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      annotate("segment", x = 2020, y = ifelse(input$y_axis, 0, min(data[["X2.5."]])), 
               xend = 2035, yend = ifelse(input$y_axis, 0, min(data[["X2.5."]])),
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("segment", x = 2018, y = ifelse(input$y_axis, 0, min(data[["X2.5."]])), 
               xend = 2003, yend = ifelse(input$y_axis, 0, min(data[["X2.5."]])),
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("text", x = 2018, y = ifelse(input$y_axis, 0, min(data[["X2.5."]])) + 0.025 * ifelse(input$y_axis, 1, (max(data[["X97.5."]]) - min(data[["X2.5."]]))), 
               label = "hindcast", hjust = 1, vjust = 0, size = 5) +
      annotate("text", x = 2020, y = ifelse(input$y_axis, 0, min(data[["X2.5."]])) + 0.025 * ifelse(input$y_axis, 1, (max(data[["X97.5."]]) - min(data[["X2.5."]]))),
               label = "forecast", hjust = 0, vjust = 0, size = 5) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + {
        if (input$y_axis) scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))
      } +
      labs(y = ylab)
    
  }
   
  #output$sp1_bins <- reactive(input$sp1)
  sp1_bins <- reactive({
    unique(c(
      unique(overlap$bin1[overlap$species1 == input$sp1]), 
      unique(overlap$bin2[overlap$species2 == input$sp1])
    ))
  })
  
  output$bin1 <- renderUI({
    
    if (length(sp1_bins()) == 0) {
      fluidPage()
    } else {
      selectInput(
        inputId = "bin1", 
        "Species 1 size bin", 
        sp1_bins(), 
        selected = sp1_bins()[1]
      )
    }
    
  })
  
  sp2_bins <- reactive({
    unique(c(
      unique(overlap$bin1[overlap$species1 == input$sp2]), 
      unique(overlap$bin2[overlap$species2 == input$sp2])
    ))
  })
  
  output$bin2 <- renderUI(
    
    if (length(sp2_bins()) == 0) {
      fluidPage()
    } else {
      selectInput(
        inputId = "bin2", 
        "Species 2 size bin", 
        sp2_bins(), 
        selected = ifelse(length(sp2_bins()) > 1, sp2_bins()[2], sp2_bins()[1])
      )
    }
    
  )
  
  sims <- reactive({
    scenarios <- scenario_abr[match(input$scenario, scenario_nm)]
    as.character(c(apply(expand.grid(scenarios, input$model), MARGIN = 1, paste, collapse = " ")))
  })
  
  plot_data <- reactive({
    
    if (any(overlap$species1 == input$sp1 & overlap$bin1 == input$bin1 & 
            overlap$species2 == input$sp2 & overlap$bin2 == input$bin2)) {
      plot_data <- overlap |> 
        filter(
          species1 == input$sp1 &
            species2 == input$sp2 &
            bin1 == input$bin1 & 
            bin2 == input$bin2 &
            sim %in% c("hindcast", sims()) 
        )
    } else {
      plot_data <- overlap |> 
        filter(
          species1 == input$sp2 &
            species2 == input$sp1 &
            bin1 == input$bin2 & 
            bin2 == input$bin1 &
            sim %in% c("hindcast", sims())
        ) 
    }
    
    if (any(input$ensemble == "Ensemble")) {
      plot_data <- plot_data |> mutate(sim = scenario) |> group_by(sim, year, index) |> 
        summarize(mean = mean(mean), X2.5. = min(X2.5.), X97.5. = max(X97.5.))
      
    } else {
      plot_data <- plot_data |> mutate(sim = factor(sim, levels = sim_names))
    }
    
    return(plot_data)
    
  })
  
  output$overlap_background <- reactive(
    if(input$index == "Probability of occurrence") {
      paste(
        "Each of the below overlap metrics varies between 0 and 1, with 0", 
        "indicating no overlap and 1 indicating complete overlap.",
        "The years containing groundfish survey data (1982-2019) are shaded."
      )
    } else {
      paste(
        "Each of the below overlap metrics varies between 0 and 1, with 0", 
        "indicating no overlap and 1 indicating complete overlap, and each is",
        "invariant under linear transformation, i.e., impacted only by the relative biomass",
        "distributions of each species, not by the aggregate sum biomass in a given year.",
        "The years containing groundfish survey data (1982-2019) are shaded."
      )
    }
  )
  
  output$overlap_title1 <- reactive(
    if(input$index == "Probability of occurrence") {
      "Area overlap"
    } else {
      "Local index of collocation"
    }
  )
  
  output$overlap_description1 <- reactive(
    if(input$index == "Probability of occurrence") {
      paste(
        "Area overlap is a simple measure of the proportion of the Eastern Bering Sea (EBS + NBS)",
        "survey region that we expect both species to occur together, i.e., for which the estimated",
        "probability of occurrence is greater than 0.5."
      )
    } else {
      paste(
        "The local index of collocation is (loosely) a relative metric of interspecies encounter, which",
        "measures co-occurrence as a non-centered correlation among estimated species biomass:"
      )
    }
  )
  
  output$overlap_math1 <- renderUI(
    if(input$index == "Probability of occurrence") {
      withMathJax("$$A_{x, y} / A_{\\text{total}}$$")
    } else {
      withMathJax("$$\\frac{\\sum p_xp_y}{\\sum p_x^2 \\sum p_y^2}$$")
    }
  )
  
  output$overlap_plot1 <- renderPlot(
    
    if (input$index == "Probability of occurrence") {
      plot_overlap(plot_data(), "area_overlap", "area overlap", c("hindcast", sims()))
    } else {
      plot_overlap(plot_data(), "loc_colloc", "local index of collocation", c("hindcast", sims()))
    }
    
  )
  
  output$overlap_plot2 <- renderPlot(
    
    if (input$index == "Probability of occurrence") {
      plot_overlap(plot_data(), "bhattacharyya_encounter", "Bhattacharyya's coefficient", c("hindcast", sims()))
    } else {
      plot_overlap(plot_data(), "bhattacharyya", "Bhattacharyya's coefficient", c("hindcast", sims()))
    }
    
  )
  
  output$overlap_plot3 <- renderPlot(
    
    if (input$index == "Probability of occurrence") {
      plot_overlap(plot_data(), "gbl_colloc_encounter", "global index of collocation", c("hindcast", sims()))
    } else {
      plot_overlap(plot_data(), "gbl_colloc", "global index of collocation", c("hindcast", sims()))
    }
    
  )
  
  # Directories containing output for each species
  sp1_dir <- reactive(paste0("output/", gsub(" ", "_", input$sp1), ifelse(input$bin1 == "all", "", paste0("-", input$bin1))))
  sp2_dir <-  reactive(paste0("output/", paste0(gsub(" ", "_", input$sp2), ifelse(input$bin2 == "all", "", paste0("-", input$bin2)))))
  
  # Range summary files #new
  sp1_summary <- reactive({
    x <- read.csv(paste0(sp1_dir(), "/range_summary.csv")) |> mutate(
      scenario = ifelse(sim == "hindcast", sim, vapply(sim, \(x) strsplit(x, " ")[[1]][1], character(1))), 
      scenario = factor(scenario, c("hindcast", scenario_abr))
    ) 
    if (input$index == "Probability of occurrence") {
      cols <- names(x)[endsWith(names(x), "_occ")]
      x[,gsub("_occ", "", cols)] <- x[,cols]
    }
    x
  })
  sp2_summary <- reactive({
    x <- read.csv(paste0(sp2_dir(), "/range_summary.csv")) |> mutate(
      scenario = ifelse(sim == "hindcast", sim, vapply(sim, \(x) strsplit(x, " ")[[1]][1], character(1))), 
      scenario = factor(scenario, c("hindcast", scenario_abr))
    ) 
    if (input$index == "Probability of occurrence") {
      cols <- names(x)[endsWith(names(x), "_occ")]
      x[,gsub("_occ", "", cols)] <- x[,cols]
    }
    x
  })
  
  # Empirical range summaries for years in observed data
  sp1_empirical <- reactive({
    x <- read.csv(paste0(sp1_dir(), "/range_empirical.csv"))
    if (input$index == "Probability of occurrence") {
      x$centroid_northings <- x$centroid_northings_occ
      x$centroid_eastings <- x$centroid_eastings_occ
    }
    x
  })
  sp2_empirical <- reactive({
    x <- read.csv(paste0(sp2_dir(), "/range_empirical.csv"))
    if (input$index == "Probability of occurrence") {
      x$centroid_northings <- x$centroid_northings_occ
      x$centroid_eastings <- x$centroid_eastings_occ
    }
    x
  })
  
  # Model summaries
  output$sp1_mod <- renderTable({
    mod_tab <- read.csv(paste0(sp1_dir(), "/covariate_selection.csv"))
    mod_tab |> group_by(component) |> filter(RMSE == min(RMSE)) |> dplyr::select(-species, -AIC)
  })
  output$sp2_mod <- renderTable({
    mod_tab <- read.csv(paste0(sp2_dir(), "/covariate_selection.csv"))
    mod_tab |> group_by(component) |> filter(RMSE == min(RMSE)) |> dplyr::select(-species, -AIC)
  })
  
  ## Function to format data for species range summary plots, given mean and sd
  species_plotdata_sd <- function(data, var, sd, var_srvy) { #new
    
    var <- enquo(var)
    sd <- enquo(sd)
    var_srvy <- enquo(var_srvy)
    
    data <- data |> 
      filter(sim %in% c("hindcast", sims())) |> 
      dplyr::select(year, sim, scenario, y = !!var, sd = !!sd, y_srvy = !!var_srvy) |> 
      mutate(lower = qnorm(0.025, y, sd), upper = qnorm(0.975, y, sd))
    
    if (any(input$ensemble == "Ensemble")) {
      
      data <- data |> 
        mutate(sim = scenario) |> 
        group_by(sim, year) |> 
        summarize(y = mean(y), lower = min(lower), upper = max(upper), y_srvy = unique(y_srvy))
      
    }
    
    data
    
  }
  
  ## Function to format data for species range summary plots, given mean and lower / upper
  species_plotdata_ci <- function(data, var, lower, upper, var_srvy) { #new
    
    var <- enquo(var)
    lower <- enquo(lower)
    upper <- enquo(upper)
    var_srvy <- enquo(var_srvy)
    
    data <- data |> 
      filter(sim %in% c("hindcast", sims())) |> 
      dplyr::select(year, sim, scenario, y = !!var, y_srvy = !!var_srvy, lower = !!lower, upper = !!upper)
    
    if (any(input$ensemble == "Ensemble")) {
      
      data <- data |> 
        mutate(sim = scenario) |> 
        group_by(sim, year) |> 
        summarize(y = mean(y), lower = min(lower), upper = max(upper), y_srvy = unique(y_srvy))
      
    }
    
    data
    
  }
  
  
  ## Plots for species 1
  output$sp1_northings <- renderPlot({
    
    if(any(input$ensemble == "Ensemble")) {
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
    } else {
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
    }
    
    sp1_summary() |> 
      species_plotdata_sd(northings_mean, northings_sd, northings_survey_mean) |> 
      ggplot(aes(year, y)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = sim), alpha = 0.5) +
      geom_line(aes(color = sim)) +
      geom_line(aes(year, centroid_northings), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = y_srvy), color = "darkorchid4") +
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) +
      labs(y = "northings (km)")
    
  })
  
  output$sp1_eastings <- renderPlot({
    
    if(any(input$ensemble == "Ensemble")) {
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
    } else {
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
    }
    
    sp1_summary() |> 
      species_plotdata_sd(eastings_mean, eastings_sd, eastings_survey_mean) |> 
      ggplot(aes(year, y)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = sim), alpha = 0.5) +
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, centroid_eastings), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = y_srvy), color = "darkorchid4") +
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(y = "eastings (km)")
    
  })
  
  output$sp1_area <- renderPlot({
    
    if(any(input$ensemble == "Ensemble")) {
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
    } else {
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
    }
    
    sp1_summary() |> 
      species_plotdata_ci(area_occupied_mean, area_occupied_2.5, area_occupied_97.5, area_occupied_survey_mean) |> 
      ggplot(aes(year, y)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = sim), alpha = 0.5) +
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, area_occupied), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = y_srvy), color = "darkorchid4") +
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(y = "area occupied")
    
  })
  
  output$sp1_novelty_binom <- renderPlot({
    
    data <- sp1_summary() |> 
      filter(sim %in% c("hindcast", sims())) |> 
      dplyr::select(year, sim, scenario, y = mahalanobis_binom_mean)
    
    if(any(input$ensemble == "Ensemble")) {
      
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
      
      data <- data |> mutate(sim = scenario) |> group_by(sim, year) |> summarize(y = mean(y))
      
    } else {
      
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
      
    }
    
    data |> 
      ggplot(aes(year, y)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      labs(y = "Mahalanobis distance\n(bionomial component)")
    
  })
  
  output$sp1_novelty_pos <- renderPlot({
    
    data <- sp1_summary() |> 
      filter(sim %in% c("hindcast", sims())) |> 
      dplyr::select(year, sim, scenario, y = mahalanobis_pos_mean)
    
    if(any(input$ensemble == "Ensemble")) {
      
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
      
      data <- data |> mutate(sim = scenario) |> group_by(sim, year) |> summarize(y = mean(y))
      
    } else {
      
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
      
    }
    
    data |> 
      ggplot(aes(year, y)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      labs(y = "Mahalanobis distance\n(lognormal component)")
    
  })
  
  output$sp1_fit_pred <- renderImage(list(src = paste0(sp1_dir(), "/fitted_observed_map.png"), width = "100%"), deleteFile = FALSE)
  
  output$sp1_proj_map <- renderImage(list(src = paste0(sp1_dir(), ifelse(input$index == "Estimated biomass", "/projection_2070-2079.png", "/projection_2070-2079_occurrence.png")), width = "100%"), deleteFile = FALSE)
  
  ## Plots for species 2
  output$sp2_northings <- renderPlot({
    
    if(any(input$ensemble == "Ensemble")) {
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
    } else {
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
    }
    
    sp2_summary() |> 
      species_plotdata_sd(northings_mean, northings_sd, northings_survey_mean) |> 
      ggplot(aes(year, y)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = sim), alpha = 0.5) +
      geom_line(aes(color = sim)) +
      geom_line(aes(year, centroid_northings), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = y_srvy), color = "darkorchid4") +
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(y = "northings (km)")
    
  })
  
  output$sp2_eastings <- renderPlot({
    
    if(any(input$ensemble == "Ensemble")) {
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
    } else {
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
    }
    
    sp2_summary() |> 
      species_plotdata_sd(eastings_mean, eastings_sd, eastings_survey_mean) |> 
      ggplot(aes(year, y)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = sim), alpha = 0.5) +
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, centroid_eastings), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = y_srvy), color = "darkorchid4") +
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(y = "eastings (km)")
    
  })
  
  output$sp2_area <- renderPlot({
    
    if(any(input$ensemble == "Ensemble")) {
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
    } else {
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
    }
    
    sp2_summary() |> 
      species_plotdata_ci(area_occupied_mean, area_occupied_2.5, area_occupied_97.5, area_occupied_survey_mean) |> 
      ggplot(aes(year, y)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = sim), alpha = 0.5) +
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, area_occupied), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = y_srvy), color = "darkorchid4") +
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(y = "area occupied")
    
  })
  
  output$sp2_novelty_binom <- renderPlot({
    
    data <- sp2_summary() |> 
      filter(sim %in% c("hindcast", sims())) |> 
      dplyr::select(year, sim, scenario, y = mahalanobis_binom_mean)
    
    if(any(input$ensemble == "Ensemble")) {
      
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
      
      data <- data |> mutate(sim = scenario) |> group_by(sim, year) |> summarize(y = mean(y))
      
    } else {
      
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
      
    }
    
    data |> 
      ggplot(aes(year, y)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      labs(y = "Mahalanobis distance\n(binomial component)")
    
  })
  
  output$sp2_novelty_pos <- renderPlot({
    
    data <- sp2_summary() |> 
      filter(sim %in% c("hindcast", sims())) |> 
      dplyr::select(year, sim, scenario, y = mahalanobis_pos_mean)
    
    if(any(input$ensemble == "Ensemble")) {
      
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
      
      data <- data |> mutate(sim = scenario) |> group_by(sim, year) |> summarize(y = mean(y))
      
    } else {
      
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
      
    }
    
    data |> 
      ggplot(aes(year, y)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      labs(y = "Mahalanobis distance\n(lognormal component)")
    
  })
  
  output$sp2_fit_pred <- renderImage(list(src = paste0(sp2_dir(), "/fitted_observed_map.png"), width = "100%"), deleteFile = FALSE)
  
  output$sp2_proj_map <- renderImage(list(src = paste0(sp2_dir(), ifelse(input$index == "Estimated biomass", "/projection_2070-2079.png", "/projection_2070-2079_occurrence.png")), width = "100%"), deleteFile = FALSE)
  
  output$downloadOverlap <- downloadHandler(
    filename = function() {
      paste0(paste('overlap', gsub(" ", "_", input$sp1), input$bin1, gsub(" ", "_", input$sp2), input$bin2, sep = "-"), ".csv")
    },
    content = function(con) {
      write.csv(plot_data(), con, row.names = FALSE)
    }
  )
  
  output$downloadSp1 <- downloadHandler(
    filename = function() {
      paste0(paste(gsub(" ", "_", input$sp1), input$bin1, sep = "-"), ".csv")
    },
    content = function(con) {
      write.csv(sp1_summary(), con, row.names = FALSE)
    }
  )
  
  output$downloadSp2 <- downloadHandler(
    filename = function() {
      paste0(paste(gsub(" ", "_", input$sp2), input$bin2, sep = "-"), ".csv")
    },
    content = function(con) {
      write.csv(sp2_summary(), con, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
