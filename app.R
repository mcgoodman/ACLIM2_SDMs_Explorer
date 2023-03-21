
require("shiny")
require("here")
require("dplyr")
require("ggplot2")

## Read in overlap data, extract species that estimates are available for
overlap <- read.csv("output/overlap_summary.csv")
overlap$bin1[is.na(overlap$bin1)] <- "all"
overlap$bin2[is.na(overlap$bin2)] <- "all"
species1 <- unique(overlap$species1)
species2 <- unique(overlap$species2)
sims <- unique(overlap$sim)

## Overlap indices
indices <- c("Local index of collocation", "Bhattacharyya's coefficient", "Global index of collocation")

theme_set(
  theme_bw() + 
    theme(
      axis.text = element_text(size = 12, color = "black"), 
      plot.title = element_text(size = 14, color = "black", face = "bold"),
      axis.title = element_text(size = 14, color = "black"), 
      legend.text = element_text(size = 12)
    )
  
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ACLIM2 SDMs: Species range and overlap forecasts"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sp1", 
        label = "Species 1", 
        choices = species1, 
        selected = species1[1]
      ), 
      selectInput(
        inputId = "sp2", 
        label = "Species 2",
        choices = species1,
        selected = species1[1]
      ),
      uiOutput("bin1"), 
      uiOutput("bin2"),
      selectInput(
        inputId = "index", 
        label = "Overlap Index", 
        choices = indices
      ), 
      checkboxGroupInput(
        inputId = "sim", 
        label = "Plot series", 
        choices = sims,
        selected = sims
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Overlap", fluidPage(
          h2("Background"),
          p(paste(
            "This interface displays fitted means and confidence intervals from SDMs built for a suite", 
            "of eastern Bering Sea groundfish and crab species as part of the ACLIM2 project.",
            "Generalized additive models (GAMs) were built for each species with environmental covariates",
            "selected using time-series cross validation (i.e., by optimizing 10-year ahead predictive",
            "performance. Terms considered in the models include temperature, depth, oxygen, pH, and principal",
            "components axes for a collection of other variables in the ROMS-NPZ hindcast, including",
            "spatiall-varying effects of the cold pool. Models were fit separately for juveniles and adults",
            "for most species, but not for snow crabs, and red king crab - when plotting these species, choose",
            "'all' for the size bin. Overlap projections are plotted below, with model summaries and range",
            "projections for individual species available in tabs to the right."
          )),
          plotOutput("overlap_plot")), 
          h3("Overlap metrics:"),
          p(paste(
            "The available overlap metrics are as follows. Each varies between 0 and 1, with 0", 
            "indicating no overlap and 1 indicating complete overlap, and each is",
            "invariant under linear transformation, i.e., impacted only by the relative biomass",
            "distributions of each species, not by the aggregate sum biomass in a given year."
          )),
          h4("Local Index of Collocation"),
          p(paste(
            "The local index of collocation is (loosely) a relative metric of interspecies encounter, which",
            "measures co-occurrence as a non-centered correlation among estimated species biomass:"
          )),
          withMathJax("$$\\frac{\\sum p_xp_y}{\\sum p_x^2 \\sum p_y^2}$$"),
          h4("Bhattacharyya's coefficient"),
          p(paste(
            "Bhattacharyya's coefficient is a metric of similarity in the fine-scale spatial distributions",
            "of two species:"
          )),
          withMathJax("$$\\sum \\sqrt{p_x p_y}$$"),
          h4("Global index of collocation"),
          p(paste(
            "The global index of collocation is a measure of broad-scale geographic similarity in two species",
            "distributions, as a function of each species center of gravity (CG) and dispersion (I):"
          )),
          withMathJax("$$\\frac{1 - \\Delta CG_{x, y}^2}{\\Delta CG_{x,y}^2 + I_x + I_y}$$"), 
          p("(Overlap formulas and definitions following Carroll et al. (2019).")
        ), 
        tabPanel("Species 1", fluidPage(
          h2("Selected models"),
          p(paste(
            "Models included two components - a binomial component to fit presence/absence data and",
            "with which to project probability of occurrence / habitat suitability, and a gamma component",
            "fit to biomass values greater than zero. Covariates were selected separately for each component.",
            "Projections are based on total estimated biomass, i.e., the product of estimated probability of",
            "occurrence and estimated positive biomass. The best-fit model according to time-series cross validation",
            "is described below (The AIC and RMSE values are useful only in comparison to other candidate models)."
          )),
          tableOutput("sp1_mod"),
          h2("Plots"),
          h3("Range characteristics"),
          p(paste(
            "Plots of the latitudinal and longitudinal components of the range centroid, as well as the area of",
            "the eastern & northern Bering Sea shelf occupied, with 95% credible intervals:"
          )),
          plotOutput("sp1_northings"), 
          plotOutput("sp1_eastings"),
          plotOutput("sp1_area"),
          h3("Environmental novelty"),
          p(paste(
            "Plots of Mahalanobis distance, which is a multivariate metric of environmental novelty",
            "comparing each year's environmental conditions with the multivariate mean and covariance",
            "of the biophyiscal state of the bering sea during years in the observed data, based on the",
            "hindcast. The variables used to guage environmental novelty are those in the best-fit models",
            "and are therefore different for the binomial and gamma components:"
          )),
          plotOutput("sp1_novelty_binom"),
          plotOutput("sp1_novelty_gamma")
        )), 
        tabPanel("Species 2", fluidPage(
          h2("Selected models"),
          p(paste(
            "Models included two components - a binomial component to fit presence/absence data and",
            "with which to project probability of occurrence / habitat suitability, and a gamma component",
            "fit to biomass values greater than zero. Covariates were selected separately for each component.",
            "Projections are based on total estimated biomass, i.e., the product of estimated probability of",
            "occurrence and estimated positive biomass. The best-fit model according to time-series cross validation",
            "is described below (The AIC and RMSE values are useful only in comparison to other candidate models)."
          )),
          tableOutput("sp2_mod"),
          h2("Plots"),
          h3("Range characteristics"),
          p(paste(
            "Plots of the latitudinal and longitudinal components of the range centroid, as well as the area of",
            "the eastern & northern Bering Sea shelf occupied, with 95% credible intervals:"
          )),
          plotOutput("sp2_northings"), 
          plotOutput("sp2_eastings"),
          plotOutput("sp2_area"),
          h3("Environmental novelty"),
          p(paste(
            "Plots of Mahalanobis distance, which is a multivariate metric of environmental novelty",
            "comparing each year's environmental conditions with the multivariate mean and covariance",
            "of the biophyiscal state of the bering sea during years in the observed data, based on the",
            "hindcast. The variables used to guage environmental novelty are those in the best-fit models",
            "and are therefore different for the binomial and gamma components:"
          )),
          plotOutput("sp2_novelty_binom"),
          plotOutput("sp2_novelty_gamma")
        ))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #output$sp1_bins <- reactive(input$sp1)
  sp1_bins <- reactive(unique(overlap$bin1[overlap$species1 == input$sp1]))
  
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
  
  sp2_bins <- reactive(unique(overlap$bin1[overlap$species2 == input$sp1]))
  
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
  
  get_index <- reactive(
    case_when(
      input$index == "Local index of collocation" ~ "loc_colloc", 
      input$index == "Bhattacharyya's coefficient" ~ "bhattacharyya", 
      input$index == "Global index of collocation" ~ "gbl_colloc"
    )
  )
  
  plot_data <- reactive(
    overlap |> 
      filter(
        species1 == input$sp1 &
          species2 == input$sp2 &
          bin1 == input$bin1 & 
          bin2 == input$bin2 & 
          index == get_index() & 
          sim %in% input$sim
      )
  )
  
  output$overlap_plot <- renderPlot({
    
    plot_data() |> 
      ggplot(aes(year, mean)) + 
      geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill = sim), alpha = 0.5) + 
      geom_line(aes(color = sim)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      annotate("segment", x = 2020, y = min(plot_data()[["X2.5."]]), 
               xend = 2035, yend = min(plot_data()[["X2.5."]]),
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("segment", x = 2018, y = min(plot_data()[["X2.5."]]), 
               xend = 2003, yend = min(plot_data()[["X2.5."]]),
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("text", x = 2018, y = min(plot_data()[["X2.5."]]) + 0.025 * (max(plot_data()[["X2.5."]]) - min(plot_data()[["X2.5."]])), 
               label = "hindcast", hjust = 1, vjust = 0, size = 5) +
      annotate("text", x = 2020, y = min(plot_data()[["X2.5."]]) + 0.025 * (max(plot_data()[["X2.5."]]) - min(plot_data()[["X2.5."]])),
               label = "forecast", hjust = 0, vjust = 0, size = 5) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = input$index, 
        title = "Spatial overlap",
        subtitle = "Shaded area corresponds to years with observed groundfish and crab survey data"
      )
    
  })
  
  # Directories containing output for each species
  sp1_dir <- reactive(paste0("output/", gsub(" ", "_", input$sp1), ifelse(input$bin1 == "all", "", paste0("-", input$bin1))))
  sp2_dir <-  reactive(paste0("output/", paste0(gsub(" ", "_", input$sp2), ifelse(input$bin2 == "all", "", paste0("-", input$bin2)))))
  
  # Range summary files
  sp1_summary <- reactive(read.csv(paste0(sp1_dir(), "/range_summary.csv")))
  sp2_summary <- reactive(read.csv(paste0(sp2_dir(), "/range_summary.csv")))
  
  # Empirical range summaries for years in observed data
  sp1_empirical <- reactive(read.csv(paste0(sp1_dir(), "/range_empirical.csv")))
  sp2_empirical <- reactive(read.csv(paste0(sp2_dir(), "/range_empirical.csv")))
  
  # Model summaries
  output$sp1_mod <- renderTable({
    read.csv(paste0(sp1_dir(), "/covariate_selection.csv")) |> 
      group_by(component) |> 
      filter(RMSE == min(RMSE))
  })
  output$sp2_mod <- renderTable({
    read.csv(paste0(sp2_dir(), "/covariate_selection.csv")) |> 
      group_by(component) |> 
      filter(RMSE == min(RMSE))
  })
  
  ## Plots for species 1
  output$sp1_northings <- renderPlot({
    
    sp1_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, northings_mean)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = northings_mean - northings_sd, ymax = northings_mean + northings_sd, fill = sim), 
                  alpha = 0.5) + 
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, centroid_northings), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5) +
      geom_point(aes(year, centroid_northings), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "northings (km)",
        title = "Latitudinal component of range centroid (mean and SD)", 
        subtitle = "(Weighted mean of observed survey data in black)"
      )
    
  })
  
  output$sp1_eastings <- renderPlot({
    
    sp1_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, eastings_mean)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = eastings_mean - eastings_sd, ymax = eastings_mean + eastings_sd, fill = sim), 
                  alpha = 0.5) + 
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, centroid_eastings), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5) +
      geom_point(aes(year, centroid_eastings), data = sp1_empirical(), inherit.aes = FALSE) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "eastings (km)",
        title = "Longitudinal component of range centroid (mean and SD)",
        subtitle = "(Weighted mean of observed survey data in black)"
      )
    
  })
  
  output$sp1_area <- renderPlot({
    
    sp1_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, area_occupied_mean)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = area_occupied_2.5, ymax = area_occupied_97.5, fill = sim), 
                  alpha = 0.5) + 
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, area_occupied), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5) +
      geom_point(aes(year, area_occupied), data = sp1_empirical(), inherit.aes = FALSE) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "area occupied (%)",
        title = "Area occupied (mean and 95% CI)", 
        subtitle = "(Area occupied computed from observed survey data in black)"
      )
    
  })
  
  output$sp1_novelty_binom <- renderPlot({
    
    sp1_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, mahalanobis_binom_mean)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "Mahalanobis distance",
        title = "Environmental novelty, bionomial component (mean and SD)"
      )
    
  })
  
  output$sp1_novelty_gamma <- renderPlot({
    
    sp1_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, mahalanobis_gamma_mean)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "Mahalanobis distance",
        title = "Environmental novelty, gamma component (mean and SD)"
      )
    
  })
  
  ## Plots for species 1
  output$sp2_northings <- renderPlot({
    
    sp2_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, northings_mean)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = northings_mean - northings_sd, ymax = northings_mean + northings_sd, fill = sim), 
                  alpha = 0.5) + 
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, centroid_northings), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5) +
      geom_point(aes(year, centroid_northings), data = sp2_empirical(), inherit.aes = FALSE) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "northings (km)",
        title = "Latitudinal component of range centroid (mean and SD)", 
        subtitle = "(Weighted mean of observed survey data in black)"
      )
    
  })
  
  output$sp2_eastings <- renderPlot({
    
    sp2_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, eastings_mean)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = eastings_mean - eastings_sd, ymax = eastings_mean + eastings_sd, fill = sim), 
                  alpha = 0.5) + 
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, centroid_eastings), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5) +
      geom_point(aes(year, centroid_eastings), data = sp2_empirical(), inherit.aes = FALSE) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "eastings (km)",
        title = "Longitudinal component of range centroid (mean and SD)", 
        subtitle = "(Weighted mean of observed survey data in black)"
      )
    
  })
  
  output$sp2_area <- renderPlot({
    
    sp2_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, area_occupied_mean)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = area_occupied_2.5, ymax = area_occupied_97.5, fill = sim), 
                  alpha = 0.5) + 
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, area_occupied), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5) +
      geom_point(aes(year, area_occupied), data = sp2_empirical(), inherit.aes = FALSE) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "area occupied (%)",
        title = "Area occupied (mean and 95% CI)", 
        subtitle = "(Area occupied computed from observed survey data in black)"
      )
    
  })
  
  output$sp2_novelty_binom <- renderPlot({
    
    sp2_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, mahalanobis_binom_mean)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "Mahalanobis distance",
        title = "Environmental novelty, bionomial component (mean and SD)"
      )
    
  })
  
  output$sp2_novelty_gamma <- renderPlot({
    
    sp2_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, mahalanobis_gamma_mean)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "Mahalanobis distance",
        title = "Environmental novelty, gamma component (mean and SD)"
      )
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
