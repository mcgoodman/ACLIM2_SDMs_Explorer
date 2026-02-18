
require("dplyr")
require("ggplot2")
require("shinythemes")
require("here")

## Read in overlap data, extract species that estimates are available for
overlap <- read.csv(here("output", "overlap_summary.csv"))
overlap$pred_bin[is.na(overlap$pred_bin)] <- "all"
overlap$prey_bin[is.na(overlap$prey_bin)] <- "all"
pred <- unique(overlap$pred)
prey <- unique(overlap$prey)

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
    "We considered multiple candidate models for each species, and weighted their predictions using 'stacking,'",
    "an approach aimed at minimizing the predictive error of the ensemble. Here are the partial effects (i.e., the effects",
    "of each covariate when all other covariates are held at their average) of each covariate estimated in the candidate", 
    "models, with darker shades indicating that an effect comes from a model with more weight:"
  ),
  svcs = paste(
    "And here is the average partial effect of the cold pool spatially-varying coefficient, plotted at extents of the 2°C cold",
    "pool of 0%, 35%, and 70% of the EBS + NBS shelf. These can be a little difficult to interpret since they represent the", 
    "partial effect of the cold pool holding all other covariates at their average, but the extent of the cold pool will also",
    "of course mean extents in the average temperature and the spatial distribution of temperature. Generally, however,", 
    "locations with very low log-odds of occurrence or expected log biomass CPUE for e.g. a cold pool extent of 70% will", 
    "indicate that no matter how suitable the environmental conditions in that area, that species is unlikely to be observed",
    "in that location while the cold pool is extensive. If the expected log CPUE or log-odds of occurrence is less than -10, that",
    "it has been truncated to -10 here:"
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
    "and standard errors, computed from the pooled posterior samples of the candidate models."
  ),
  area_occupied = paste(
    "Area occupied is defined here as the proportion of the survey area (EBS + NBS) for which the predicted",
    "probability of occurrence is greater than 0.5. The estimates in this plot are derived from the probability", 
    "of ocurrence models, regardless of which model type is selected in the side bar. Shown are the means and 95%", 
    "credible bands, computed from the pooled posterior samples of the candidate models."
  ), 
  mahalanobis = paste(
    "Plots of Mahalanobis distance, which is a multivariate metric of environmental novelty",
    "comparing each year's environmental conditions with the multivariate mean and covariance",
    "of the biophyiscal state of the bering sea during years in the observed data, based on the",
    "hindcast. The variables used to guage environmental novelty are those in the candidate models,",
    "with the plots below showing the weighted average Mahalanobis distance across models."
  ),
  fitted_map = paste(
    "These are maps of fitted vs. observed biomass for selected years:"
  ),
  proj_map = paste(
    "These maps display the projected distribution under each scenario and climate model, averaged across the",
    "years 2040-2059, 2060-2079, and 2080-2099 and compared to the 1995 - 2015 average (they are pre-rendered and", "
    will not change depending on the climate models and scenarios selected in the side panel, but will be different", 
    "between probability of ocurrence and biomass projections)."
  )
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  # ACE Header
  includeHTML("shinyheader.html"),
  
  # Application title
  #titlePanel("ACLIM2 SDMs: Species range and overlap forecasts"),
  
  # Left align LaTeX math
  tags$head(
    
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny-custom.css"),
    
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
        inputId = "pred", 
        label = "Predator", 
        choices = paste(pred, "(adult)"), 
        selected = "arrowtooth flounder (adult)"
      ), 
      selectInput(
        inputId = "prey", 
        label = "Prey",
        choices = paste(prey, ifelse(grepl("crab", prey), "(all)", "(juv.)")),
        selected = "walleye pollock (juv.)"
      ),
      shinyWidgets::radioGroupButtons(
        inputId = "index", 
        label = "Compute overlap indices using", 
        choices = c("Probability of occurrence", "Estimated biomass"), 
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
      shinyWidgets::radioGroupButtons(
        inputId = "plot_type", 
        label = "Overlap plot type", 
        choices = c("Annual", "Decadal mean", "Smoothed trend"), 
        justified = TRUE
      ), 
      h5(HTML("<b>Download output<b>")),
      fluidRow( 
        column(4, downloadButton('downloadSp1', 'Predator', style = "width:100%;")), 
        column(4, downloadButton('downloadSp2', 'Prey', style = "width:100%;")), 
        column(4, downloadButton('downloadOverlap', 'Overlap', style = "width:100%;"))
      ), 
      div(style = "height: 20px;"),
      helpText(
        p("Prepared by Maurice Goodman (maurice.goodman@noaa.gov)"),
        p("Based on outputs from", a("Goodman et al. (2025)", href = "https://doi.org/10.1111/faf.12875")),
        p("Contact authors for additional outputs")
        )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Predator distribution", style = "background-color: #ffffff;", fluidPage(
          h2("Background"),
          p(
              "This interface displays fitted means and confidence intervals from SDMs built for a suite", 
              "of eastern Bering Sea groundfish and crab species as part of the ACLIM2 project.",
              "Generalized additive models (GAMs) were built for each species with different combinations of", 
              "environmental covariates, and the resulting set of models were combined into a weighted ensemble, with",
              "models weighted to minimize the predictive error of the ensemble.",
              "Terms considered in the models include temperature, oxygen, pH, depth, sediment grain size, and",
              "spatially-varying effects of the cold pool. Models for groundfish were fit separately for juveniles and adults",
              "We fit models for both probability of occurrence (using a binomial likelihood) and biomass (using a Tweedie likelihood).",
              "See"
            , 
            a("Goodman et al. (2025)", href = "https://doi.org/10.1111/faf.12875"), 
            "for details on model implementation."
          ),
          p(paste(
            "Select two species using the 'predator' and 'prey' drop downs on the sidebar. Model estimates and",
            "projections are displayed for the chosen predator below, with prey distribution and predator-prey overlap",
            "estimates available in the tabs to the right."
          )),
          h2("Covariate effects"),
          p(description$models),
          plotOutput("sp1_smooths"),
          p(description$svcs),
          imageOutput("sp1_svc", width = "100%"),
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
          plotOutput("sp1_novelty"),
          h3("Map of fitted and observed values"), 
          p(description$fitted_map),
          imageOutput("sp1_fit_pred", height = "100%"), 
          h3("Projected distribution"),
          p(description$proj_map),
          imageOutput("sp1_proj_map", height = "100%")
        )), 
        tabPanel("Prey distribution", style = "background-color: #ffffff;", fluidPage(
          h2("Covariate effects"),
          p(description$models),
          plotOutput("sp2_smooths"),
          p(description$svcs),
          imageOutput("sp2_svc", width = "100%"),
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
          plotOutput("sp2_novelty"),
          h3("Map of fitted and observed values"), 
          p(description$fitted_map),
          imageOutput("sp2_fit_pred", height = "100%"), 
          h3("Projected distribution"),
          p(description$proj_map),
          imageOutput("sp2_proj_map", height = "100%")
        )),
        tabPanel("Predator-prey overlap", style = "background-color: #ffffff;", fluidPage(
          h2("Spatial overlap"),
          p(paste(
            "Overlap metrics can be computed from either probability of occurrence (in which case predictions from the binomial",
            "models are used) or expected biomass (from the Tweedie models). The overlap indices computed from these products", 
            "differ: see below for descriptions."
          )),
          p(paste(
            "Note: All aggregate statistics (ensembles, decadal means, smoothers) are computed in an ad-hoc way", 
            "for visualization purposes only."   
          )),
          p(textOutput("overlap_background")),
          h4(textOutput("overlap_title1")),
          uiOutput("overlap_math1"),
          p(textOutput("overlap_description1")),
          fluidPage(
            fluidRow(
              div(
                shinyWidgets::materialSwitch("y_axis", "Y-axis: 0-1", value = FALSE, right = TRUE, inline = TRUE), 
                style = "position: absolute; right: 0; padding: 1% 2%; z-index: 1;"
              )), 
            plotOutput("overlap_plot1")
          ),
          h4(textOutput("overlap_title2")), 
          uiOutput("overlap_math2"),
          p(textOutput("overlap_description2")), 
          plotOutput("overlap_plot2"),
          h4("Bhattacharyya's coefficient"),
          withMathJax("$$\\sum \\sqrt{p_{prey} p_{pred}}$$"),
          p(paste(
            "Bhattacharyya's coefficient is a metric of similarity in the fine-scale spatial distributions",
            "of two species:"
          )),
          plotOutput("overlap_plot3"),
          h4("Global index of collocation"),
          withMathJax("$$\\frac{1 - \\Delta CG_{prey, pred}^2}{\\Delta CG_{prey, pred}^2 + I_{prey} + I_{pred}}$$"), 
          p(paste(
            "The global index of collocation is a measure of broad-scale geographic similarity in two species",
            "distributions, as a function of each species center of gravity (CG) and dispersion (I):"
          )),
          plotOutput("overlap_plot4"),
          p("Overlap formulas and definitions following Carroll et al. (2019).")
        ))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## Function to plot spatial overlap
  plot_overlap <- function(data, overlap_index, ylab, plot_sims, responsive_y = TRUE) { 
    
    data <- data |> filter(index == overlap_index)
    
    if(any(input$ensemble == "Ensemble")) {
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
    } else {
      plot_cols <- sim_cols[which(sim_names %in% plot_sims)]
    }
    
    if (input$plot_type == "Decadal mean") {
      
      data <- data |> 
        mutate(year = floor(year/10) * 10 + 5) |> 
        group_by(year, sim) |>
        summarize(mean = mean(mean), X2.5. = mean(X2.5.), X97.5. = mean(X97.5.)) |> 
        ungroup()
      
      data$mean[data$sim != "hindcast" & data$year == 2025] <- data$mean[data$sim == "hindcast" & data$year == 2025]
      data$X2.5.[data$sim != "hindcast" & data$year == 2025] <- data$X2.5.[data$sim == "hindcast" & data$year == 2025]
      data$X97.5.[data$sim != "hindcast" & data$year == 2025] <- data$X97.5.[data$sim == "hindcast" & data$year == 2025]
      
      data |> 
        ggplot(aes(year, mean)) + 
        geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill = sim), alpha = 0.5) + 
        geom_line(aes(color = sim)) + 
        geom_point(aes(color = sim)) + 
        annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
        annotate("segment", x = 2023, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])), 
                 xend = 2038, yend = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])),
                 arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
        annotate("segment", x = 2021, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])), 
                 xend = 2006, yend = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])),
                 arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
        annotate("text", x = 2021, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])) + 0.025 * ifelse(input$y_axis & responsive_y, 1, (max(data[["X97.5."]]) - min(data[["X2.5."]]))), 
                 label = "hindcast", hjust = 1, vjust = 0, size = 5) +
        annotate("text", x = 2023, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])) + 0.025 * ifelse(input$y_axis & responsive_y, 1, (max(data[["X97.5."]]) - min(data[["X2.5."]]))),
                 label = "forecast", hjust = 0, vjust = 0, size = 5) +
        scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
        scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
        scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + {
          if (input$y_axis & responsive_y) scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))
        } +
        labs(y = ylab)
      
    } else if (input$plot_type == "Smoothed trend") {
      
      range <- (max(data$year) - min(data$year))
      
      data |> 
        ggplot(aes(year, mean)) + 
        stat_smooth(geom = "ribbon", aes(ymin = X2.5., ymax = X97.5., fill = sim, group = sim), alpha = 0.5, method = "loess", span = 0.5) + 
        stat_smooth(geom = "line", aes(color = sim, group = sim), method = "loess", span = 0.5) + 
        annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
        annotate("segment", x = 2023, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])), 
                 xend = 2038, yend = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])),
                 arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
        annotate("segment", x = 2021, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])), 
                 xend = 2006, yend = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])),
                 arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
        annotate("text", x = 2021, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])) + 0.025 * ifelse(input$y_axis & responsive_y, 1, (max(data[["X97.5."]]) - min(data[["X2.5."]]))), 
                 label = "hindcast", hjust = 1, vjust = 0, size = 5) +
        annotate("text", x = 2023, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])) + 0.025 * ifelse(input$y_axis & responsive_y, 1, (max(data[["X97.5."]]) - min(data[["X2.5."]]))),
                 label = "forecast", hjust = 0, vjust = 0, size = 5) +
        scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
        scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
        scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + {
          if (input$y_axis & responsive_y) scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))
        } +
        labs(y = ylab)
      
    } else {
      
      data |> 
        ggplot(aes(year, mean)) + 
        geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill = sim), alpha = 0.5) + 
        geom_line(aes(color = sim)) + 
        annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
        annotate("segment", x = 2023, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])), 
                 xend = 2038, yend = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])),
                 arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
        annotate("segment", x = 2021, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])), 
                 xend = 2006, yend = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])),
                 arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
        annotate("text", x = 2021, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])) + 0.025 * ifelse(input$y_axis & responsive_y, 1, (max(data[["X97.5."]]) - min(data[["X2.5."]]))), 
                 label = "hindcast", hjust = 1, vjust = 0, size = 5) +
        annotate("text", x = 2023, y = ifelse(input$y_axis & responsive_y, 0, min(data[["X2.5."]])) + 0.025 * ifelse(input$y_axis & responsive_y, 1, (max(data[["X97.5."]]) - min(data[["X2.5."]]))),
                 label = "forecast", hjust = 0, vjust = 0, size = 5) +
        scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
        scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
        scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + {
          if (input$y_axis & responsive_y) scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))
        } +
        labs(y = ylab)
      
    }
    
  }
  
  sims <- reactive({
    scenarios <- scenario_abr[match(input$scenario, scenario_nm)]
    as.character(c(apply(expand.grid(scenarios, input$model), MARGIN = 1, paste, collapse = " ")))
  })
  
  plot_data <- reactive({
    
    plot_data <- overlap |> 
      filter(
        pred == gsub(" (adult)", "", input$pred, fixed = TRUE) &
          prey == gsub(" (juv.)", "", gsub(" (all)", "", input$prey, fixed = TRUE), fixed = TRUE) & 
          sim %in% c("hindcast", sims()) 
      )
    
    if (any(input$ensemble == "Ensemble")) {
      plot_data <- plot_data |> mutate(sim = scenario) |> group_by(sim, year, index) |> 
        summarize(mean = mean(mean), X2.5. = mean(X2.5.), X97.5. = mean(X97.5.))
      
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
        "The years containing groundfish survey data (1982-2022) are shaded."
      )
    } else {
      paste(
        "Each of the below overlap metrics varies between 0 and 1, with 0", 
        "indicating no overlap and 1 indicating complete overlap, and each is",
        "invariant under linear transformation, i.e., impacted only by the relative biomass",
        "distributions of each species, not by the aggregate sum biomass in a given year.",
        "The years containing groundfish survey data (1982-2022) are shaded."
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
  
  output$overlap_title2 <- reactive(
    if(input$index == "Probability of occurrence") {
      "Range Overlap"
    } else {
      "AB Ratio"
    }
  )
  
  
  output$overlap_description1 <- reactive(
    if(input$index == "Probability of occurrence") {
      paste(
        "Area overlap is a simple measure of the proportion of the Eastern Bering Sea (EBS + NBS)",
        "survey region that we expect both species to occur together."
      )
    } else {
      paste(
        "The local index of collocation is (loosely) a relative metric of interspecies encounter, which",
        "measures co-occurrence as a non-centered correlation among estimated species biomass:"
      )
    }
  )
  
  output$overlap_description2 <- reactive(
    if(input$index == "Probability of occurrence") {
      paste(
        "Range overlap measured the proportion of a prey's range in which a predator is expected to",
        "co-occur with it, i.e., the proportion of a prey's range where it is vulnerable to predation",
        "by a given predator."
      )
    } else {
      paste(
        "The AB ratio measures predator production that can be attributed to spatial overlap with prey,",
        "assuming a Type-I functional response. This metric is not 0-1 bounded."
      )
    }
  )
  
  output$overlap_math1 <- renderUI(
    if(input$index == "Probability of occurrence") {
      withMathJax("$$A_{pred, prey} / A_{total}$$")
    } else {
      withMathJax("$$\\frac{\\sum p_{pred}p_{prey}}{\\sum p_{pred}^2 \\sum p_{prey}^2}$$")
    }
  )
  
  output$overlap_math2 <- renderUI(
    if(input$index == "Probability of occurrence") {
      withMathJax("$$A_{pred, prey} / A_{prey}$$")
    } else {
      withMathJax("$$\\frac{1}{n} \\sum \\left( \\frac{(pred - \\bar{pred})(prey - \\bar{prey})}{\\bar{pred} \times \\bar{prey}} \\right)$$")
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
      plot_overlap(plot_data(), "range_overlap", "range overlap", c("hindcast", sims()))
    } else {
      plot_overlap(plot_data(), "AB_overlap", "AB ratio", c("hindcast", sims()), responsive_y = FALSE)
    }
    
  )
  
  output$overlap_plot3 <- renderPlot(
    
    if (input$index == "Probability of occurrence") {
      plot_overlap(plot_data(), "bhattacharyya_encounter", "Bhattacharyya's coefficient", c("hindcast", sims()))
    } else {
      plot_overlap(plot_data(), "bhattacharyya", "Bhattacharyya's coefficient", c("hindcast", sims()))
    }
    
  )
  
  output$overlap_plot4 <- renderPlot(
    
    if (input$index == "Probability of occurrence") {
      plot_overlap(plot_data(), "gbl_colloc_encounter", "global index of collocation", c("hindcast", sims()))
    } else {
      plot_overlap(plot_data(), "gbl_colloc", "global index of collocation", c("hindcast", sims()))
    }
    
  )
  
  # Directories containing output for each species
  sp1_dir <- reactive(here("output", paste0(gsub(" ", "_", gsub(" (adult)", "", input$pred, fixed = TRUE)), "-adult")))
  sp2_dir <- reactive(here("output", paste0(gsub(" ", "_", gsub(" (juv.)", "", gsub(" (all)", "", input$prey, fixed = TRUE), fixed = TRUE)), ifelse(grepl("crab", input$prey), "", "-juvenile"))))
  
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
  
  # Covariate effects
  sp1_cov_tab <- reactive({
    x <- read.csv(paste0(sp1_dir(), "/smoother_fits.csv"))
    if (input$index == "Probability of occurrence") {
      x <- filter(x, component == "binomial")
    } else {
      x <- filter(x, component == "tweedie")
    }
    x
  }) 
  sp2_cov_tab <- reactive({
    x <- read.csv(paste0(sp2_dir(), "/smoother_fits.csv"))
    if (input$index == "Probability of occurrence") {
      x <- filter(x, component == "binomial")
    } else {
      x <- filter(x, component == "tweedie")
    }
    x
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
  
  # Function to plot parital covariate effects
  plot_smoothers <- function(data, y_lab) {
    
    data |>
      filter(!(predictor == "pH_bottom5m" & (x < 7.5 | x > 8.2))) |> 
      filter(!(predictor == "oxygen_bottom5m" & (x < 100 | x > 440))) |> 
      mutate(predictor = factor(
        predictor, 
        levels = c("temp_bottom5m", "oxygen_bottom5m", "pH_bottom5m", "depth_m", "phi"),
        labels = c("'temperature (°C)'", "'oxygen (mmol m'^-3*')'", "pH", "'depth (m)'", "'sediment size ('*phi*')'")
      )) |>
      ggplot(aes(x, fit, group = model)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = weight), alpha = 0.5) +
      geom_line(aes(color = weight)) +
      facet_wrap( ~ predictor, scales = "free_x", strip.position = "bottom", nrow = 1, labeller = label_parsed) +
      scale_color_gradientn(colors = RColorBrewer::brewer.pal(9, "Blues")[-1]) +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Blues")[-1]) +
      guides(
        fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = unit(15, "lines")), 
        color = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = unit(15, "lines"))
      ) +
      theme(
        strip.placement = "outside",
        strip.background = element_blank(), 
        strip.text = element_text(size = 14, color = "black"),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.title.x = element_blank()
      )  + 
      labs(y = y_lab, color = "model weight", fill = "model weight")
    
  }
 
  # Covariate effect plots 
  output$sp1_smooths <- renderPlot({
    
    if (input$index == "Probability of occurrence") {
      y_lab <- "s(log-odds occurrence)"
    } else {
      y_lab <- "s(log CPUE)"
    }
    
    plot_smoothers(sp1_cov_tab(), y_lab)
    
  })
  output$sp2_smooths <- renderPlot({
    
    if (input$index == "Probability of occurrence") {
      y_lab <- "s(log-odds occurrence)"
    } else {
      y_lab <- "s(log CPUE)"
    }
    
    plot_smoothers(sp2_cov_tab(), y_lab)
    
  })
  
  # Spatially varying effects (pre-rendered)
  output$sp1_svc <- renderImage(list(src = paste0(sp1_dir(), ifelse(input$index == "Estimated biomass", "/tweedie_SVC.png", "/binomial_SVC.png")), width = "100%"), deleteFile = FALSE)
  output$sp2_svc <- renderImage(list(src = paste0(sp2_dir(), ifelse(input$index == "Estimated biomass", "/tweedie_SVC.png", "/binomial_SVC.png")), width = "100%"), deleteFile = FALSE)
  
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
      annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
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
      annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
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
      annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = sim), alpha = 0.5) +
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, area_occupied), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = y_srvy), color = "darkorchid4") +
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(y = "area occupied")
    
  })
  
  output$sp1_novelty <- renderPlot({
    
    if (input$index == "Estimated biomass") {
      data <- sp1_summary() |> 
        filter(sim %in% c("hindcast", sims())) |> 
        dplyr::select(year, sim, scenario, y = mahalanobis_binom)
    } else {
      data <- sp1_summary() |> 
        filter(sim %in% c("hindcast", sims())) |> 
        dplyr::select(year, sim, scenario, y = mahalanobis_tweedie)
    }
    
    if(any(input$ensemble == "Ensemble")) {
      
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
      
      data <- data |> mutate(sim = scenario) |> group_by(sim, year) |> summarize(y = mean(y))
      
    } else {
      
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
      
    }
    
    data |> 
      ggplot(aes(year, y)) +
      annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      labs(y = "Mahalanobis distance")
    
  })
  
  output$sp1_fit_pred <- renderImage(list(src = paste0(sp1_dir(), "/fitted_observed_map.png"), width = "100%"), deleteFile = FALSE)
  
  output$sp1_proj_map <- renderImage(list(src = paste0(sp1_dir(), ifelse(input$index == "Estimated biomass", "/projection_biomass.png", "/projection_occurrence.png")), width = "100%"), deleteFile = FALSE)
  
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
      annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
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
      annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
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
      annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = sim), alpha = 0.5) +
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, area_occupied), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = y_srvy), color = "darkorchid4") +
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      scale_fill_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(y = "area occupied")
    
  })
  
  output$sp2_novelty <- renderPlot({
    
    if (input$index == "Estimated biomass") {
      data <- sp2_summary() |> 
        filter(sim %in% c("hindcast", sims())) |> 
        dplyr::select(year, sim, scenario, y = mahalanobis_binom)
    } else {
      data <- sp2_summary() |> 
        filter(sim %in% c("hindcast", sims())) |> 
        dplyr::select(year, sim, scenario, y = mahalanobis_tweedie)
    }
    
    
    if(any(input$ensemble == "Ensemble")) {
      
      plot_cols <- ens_cols[c(1, which(scenario_abr %in% scenario_abr[match(input$scenario, scenario_nm)]) + 1)]
      
      data <- data |> mutate(sim = scenario) |> group_by(sim, year) |> summarize(y = mean(y))
      
    } else {
      
      plot_cols <- sim_cols[which(sim_names %in% c("hindcast", sims()))]
      
    }
    
    data |> 
      ggplot(aes(year, y)) +
      annotate("rect", xmin = 1982, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = plot_cols, labels = function(x) stringr::str_pad(x, 12, "right")) + 
      labs(y = "Mahalanobis distance")
    
  })
  
  output$sp2_fit_pred <- renderImage(list(src = paste0(sp2_dir(), "/fitted_observed_map.png"), width = "100%"), deleteFile = FALSE)
  
  output$sp2_proj_map <- renderImage(list(src = paste0(sp2_dir(), ifelse(input$index == "Estimated biomass", "/projection_biomass.png", "/projection_occurrence.png")), width = "100%"), deleteFile = FALSE)
  
  output$downloadOverlap <- downloadHandler(
    filename = function() {
      paste0(paste('overlap', gsub(" ", "_", input$pred), input$pred_bin, gsub(" ", "_", input$prey), input$prey_bin, sep = "-"), ".csv")
    },
    content = function(con) {
      write.csv(plot_data(), con, row.names = FALSE)
    }
  )
  
  output$downloadSp1 <- downloadHandler(
    filename = function() {
      paste0(paste(gsub(" ", "_", input$pred), input$pred_bin, sep = "-"), ".csv")
    },
    content = function(con) {
      write.csv(sp1_summary(), con, row.names = FALSE)
    }
  )
  
  output$downloadSp2 <- downloadHandler(
    filename = function() {
      paste0(paste(gsub(" ", "_", input$prey), input$prey_bin, sep = "-"), ".csv")
    },
    content = function(con) {
      write.csv(sp2_summary(), con, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
