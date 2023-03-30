

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

## Colors for plots
sim_cols <- c("black", PNWColors::pnw_palette("Bay", length(sims[sims != "hindcast"])))

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

## Function to plot spatial overlap
plot_overlap <- function(data, overlap_index, ylab, plot_sims) {
  
  data <- data |> filter(index == overlap_index)
  
  data |> 
    ggplot(aes(year, mean)) + 
    geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill = sim), alpha = 0.5) + 
    geom_line(aes(color = sim)) + 
    annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
    annotate("segment", x = 2020, y = min(data[["X2.5."]]), 
             xend = 2035, yend = min(data[["X2.5."]]),
             arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
    annotate("segment", x = 2018, y = min(data[["X2.5."]]), 
             xend = 2003, yend = min(data[["X2.5."]]),
             arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
    annotate("text", x = 2018, y = min(data[["X2.5."]]) + 0.025 * (max(data[["X2.5."]]) - min(data[["X2.5."]])), 
             label = "hindcast", hjust = 1, vjust = 0, size = 5) +
    annotate("text", x = 2020, y = min(data[["X2.5."]]) + 0.025 * (max(data[["X2.5."]]) - min(data[["X2.5."]])),
             label = "forecast", hjust = 0, vjust = 0, size = 5) +
    scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
    scale_color_manual(values = sim_cols[which(sims %in% plot_sims)]) + 
    scale_fill_manual(values = sim_cols[which(sims %in% plot_sims)]) +
    labs(y = ylab)
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
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
      radioButtons(
        inputId = "index", 
        label = "Overlap Indices from:", 
        choices = c("Estimated biomass", "Probability of occurrence")
      ), 
      checkboxGroupInput(
        inputId = "sim", 
        label = "Climate models", 
        choices = sims,
        selected = sims
      ), 
      downloadButton('downloadData', 'Download')
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
          p(paste(
            "Overlap metrics can be computed from either probability of occurrence, in which case only the Binomial",
            "component of each delta model is used, or expected biomass (i.e., the product of the expected",
            "values from the Binomial and Lognormal model component). Fit is generally much better for the Binomial",
            "component (i.e., the models tend to do a better job describing the distribution of encounters / non-encounters",
            "for each species than they do for species biomass distributions), so overlap estimates based on",
            "probability of occurrence only may be more reliable, possibly at the cost of ecological interpetability."
          )),
          h2("Spatial overlap"),
          p(textOutput("overlap_background")),
          h4(textOutput("overlap_title1")),
          uiOutput("overlap_math1"),
          p(textOutput("overlap_description1")),
          plotOutput("overlap_plot1")),
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
            "with which to project probability of occurrence / habitat suitability, and a lognormal component",
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
            "the eastern & northern Bering Sea shelf occupied, with standard error bands or 95% credible intervals.",
            "Empirical means computed directly from the survey data are shown in light purple, along with means",
            "computed from the model estimates only for the survey locations sampled in each respective year in", 
            "dark purple."
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
            "and are therefore different for the binomial and lognormal components:"
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
  
  plot_data <- reactive(
    overlap |> 
      filter(
        species1 == input$sp1 &
          species2 == input$sp2 &
          bin1 == input$bin1 & 
          bin2 == input$bin2 &
          sim %in% input$sim
      )
  )
  
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
      plot_overlap(plot_data(), "area_overlap", "area overlap", input$sim)
    } else {
      plot_overlap(plot_data(), "loc_colloc", "local index of collocation", input$sim)
    }
    
  )
  
  output$overlap_plot2 <- renderPlot(
    
    if (input$index == "Probability of occurrence") {
      plot_overlap(plot_data(), "bhattacharyya_encounter", "Bhattacharyya's coefficient", input$sim)
    } else {
      plot_overlap(plot_data(), "bhattacharyya", "Bhattacharyya's coefficient", input$sim)
    }
    
  )
  
  output$overlap_plot3 <- renderPlot(
    
    if (input$index == "Probability of occurrence") {
      plot_overlap(plot_data(), "gbl_colloc_encounter", "global index of collocation", input$sim)
    } else {
      plot_overlap(plot_data(), "gbl_colloc", "global index of collocation", input$sim)
    }
    
  )
  
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
    mod_tab <- read.csv(paste0(sp1_dir(), "/covariate_selection.csv"))
    if(!all(is.na(mod_tab$RMSE))) {
      mod_tab |> group_by(component) |> filter(RMSE == min(RMSE))
    } else {
      mod_tab |> group_by(component) |> filter(AIC == min(AIC))
    }
  })
  output$sp2_mod <- renderTable({
    mod_tab <- read.csv(paste0(sp2_dir(), "/covariate_selection.csv"))
    if(!all(is.na(mod_tab$RMSE))) {
      mod_tab |> group_by(component) |> filter(RMSE == min(RMSE))
    } else {
      mod_tab |> group_by(component) |> filter(AIC == min(AIC))
    }
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
      geom_line(aes(year, centroid_northings), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = northings_survey_mean), color = "darkorchid4") +
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      scale_fill_manual(values = sim_cols[which(sims %in% input$sim)]) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "northings (km)",
        title = "Latitudinal component of range centroid (mean and SD)"
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
      geom_line(aes(year, centroid_eastings), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = eastings_survey_mean), color = "darkorchid4") +
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      scale_fill_manual(values = sim_cols[which(sims %in% input$sim)]) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "eastings (km)",
        title = "Longitudinal component of range centroid (mean and SD)"
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
      geom_line(aes(year, area_occupied), data = sp1_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = area_occupied_survey_mean), color = "darkorchid4") +
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      scale_fill_manual(values = sim_cols[which(sims %in% input$sim)]) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "area occupied (%)",
        title = "Area occupied (mean and 95% CI)"
      )
    
  })
  
  output$sp1_novelty_binom <- renderPlot({
    
    sp1_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, mahalanobis_binom_mean)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      labs(
        y = "Mahalanobis distance",
        title = "Environmental novelty, bionomial component (mean and SD)"
      )
    
  })
  
  output$sp1_novelty_gamma <- renderPlot({
    
    sp1_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, mahalanobis_pos_mean)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      labs(
        y = "Mahalanobis distance",
        title = "Environmental novelty, lognormal component (mean and SD)"
      )
    
  })
  
  ## Plots for species 2
  output$sp2_northings <- renderPlot({
    
    sp2_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, northings_mean)) + 
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_ribbon(aes(ymin = northings_mean - northings_sd, ymax = northings_mean + northings_sd, fill = sim), 
                  alpha = 0.5) + 
      geom_line(aes(color = sim)) + 
      geom_line(aes(year, centroid_northings), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = northings_survey_mean), color = "darkorchid4") +
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      scale_fill_manual(values = sim_cols[which(sims %in% input$sim)]) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "northings (km)",
        title = "Latitudinal component of range centroid (mean and SD)"
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
      geom_line(aes(year, centroid_eastings), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = eastings_survey_mean), color = "darkorchid4") +
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      scale_fill_manual(values = sim_cols[which(sims %in% input$sim)]) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "eastings (km)",
        title = "Longitudinal component of range centroid (mean and SD)"
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
      geom_line(aes(year, area_occupied), data = sp2_empirical(), inherit.aes = FALSE, alpha = 0.5, color = "darkorchid1") +
      geom_line(aes(y = area_occupied_survey_mean), color = "darkorchid4") +
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      scale_fill_manual(values = sim_cols[which(sims %in% input$sim)]) +
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      labs(
        y = "area occupied (%)",
        title = "Area occupied (mean and 95% CI)"
      )
    
  })
  
  output$sp2_novelty_binom <- renderPlot({
    
    sp2_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, mahalanobis_binom_mean)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      labs(
        y = "Mahalanobis distance",
        title = "Environmental novelty, bionomial component (mean and SD)"
      )
    
  })
  
  output$sp2_novelty_gamma <- renderPlot({
    
    sp2_summary() |> 
      filter(sim %in% input$sim) |> 
      ggplot(aes(year, mahalanobis_pos_mean)) +
      annotate("rect", xmin = 1982, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = .2) +
      geom_line(aes(color = sim)) + 
      scale_x_continuous(breaks = seq(1970, 2100, 10)) + 
      scale_color_manual(values = sim_cols[which(sims %in% input$sim)]) + 
      labs(
        y = "Mahalanobis distance",
        title = "Environmental novelty, lognormal component (mean and SD)"
      )
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(paste('overlap', gsub(" ", "_", input$sp1), input$bin1, gsub(" ", "_", input$sp2), input$bin2, sep = "-"), ".csv")
    },
    content = function(con) {
      write.csv(plot_data(), con)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)