library(lme4)
library(merTools)
library(brms)
library(viridis)
library(MASS)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidybayes)
library(see)
library(shiny)


ui <- fluidPage(
  titlePanel("Mehrebenenmodelle"),
  tags$head(
    tags$style(HTML("
      .sidebar {
        max-height: 90vh; /* Limit height to 90% of viewport height */
        overflow-y: auto; /* Enable scrolling if content overflows */
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",  # Apply CSS styling
      h3("Parameter des Populationsmodells"),
      sliderInput("b0_mu", "Mittleres Intercept über Therapiearten hinweg", min = 0, max = 10, step = 1, value = 5),
      sliderInput("b0_sigma", "Standardabweichung des Intercepts über Therapiearten hinweg", min = .01, max = 3, step=0.1, value = .001),
      
      sliderInput("b1_mu", "Mittleres Slope über Therapiearten hinweg", min = -.5, max = .5, step=.1, value = 0),
      sliderInput("b1_sigma", "Standardabweichung des Slopes über Therapiearten hinweg", min = 0, max = 0.2, step=.01, value = 0.0001),
      sliderInput("rho", "Korrelation der Intercepts and Slopes über Therapiearten hinweg", min = -1, max = 1, step=.1, value = 0),
      sliderInput("sigma", "Residualvarianz", min = .1, max = 5, step=.1, value = .4),
      sliderInput("nind", "Anzahl Messwerte pro Therapieart", min = 5, max = 100, step = 5, value = 50),
      
      downloadButton("downloadData", "Daten herunterladen")
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        id = "plot_tabs", 
        tabPanel("Stichprobenebene", plotOutput("sample_plot", height = 900)),
        tabPanel("Populationsebene", plotOutput("population_plot", height = 900))
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    
    # Setup
    set.seed(1) # 40
    ntherapy <- 6
    nind <- input$nind
    
    means <- c(input$b0_mu, input$b1_mu)
    sigmas <- c(input$b0_sigma, input$b1_sigma)
    Rho <- matrix(c(1, input$rho, input$rho, 1), nrow = 2)
    Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
    
    # Sample country-level intercepts and slopes
    pars <- mvrnorm(ntherapy, mu = means, Sigma = Sigma) %>%
      as.data.frame() %>%
      rename(b0 = V1, b1 = V2) %>%
      mutate(therapy = 1:ntherapy)
    
    # Sample more to plot population
    pop = mvrnorm(50, mu = means, Sigma = Sigma) %>%
      as.data.frame() %>%
      rename(b0 = V1, b1 = V2) %>%
      mutate(Stichprobe = "hypothetisch") %>%
      bind_rows(pars %>% mutate(Stichprobe = "tatsächlich"))
    
    # Sample data
    d <- lapply(1:nrow(pars), function(x) {
      symptom <- rnorm(nind, mean = 6, sd = 2)
      data.frame(
        commitment = rnorm(nind, mean = pars$b0[x] + pars$b1[x] * symptom, sd = input$sigma),
        symptom = symptom, therapy = pars$therapy[x],
        b0 = pars$b0[[x]], b1 = pars$b1[[x]]
        
      )
    }) %>% bind_rows()
    
    
    results = list(
      d = d, pop=pop
    )
    
    
    
    
  })
  
  output$population_plot <- renderPlot({
    p1 = data()$pop %>% ggplot(aes(x=b0, y=b1, col=Stichprobe)) + 
      geom_point(size=6, alpha=.5) +
      scale_color_manual(values = c("black", "red"))+
      labs(x="Intercepts", y="Slopes", title = "Populationsmodell auf Ebene 2") +
      #geom_point(data$pop %>% filter(country == "actual"), col="red") +
      stat_ellipse(level = 0.95, color = "blue") +  # Confidence ellipse
      stat_ellipse(level = 0.75, color = "red") +
      stat_ellipse(level = 0.5, color = "green") +
      stat_ellipse(level = 0.25, color = "purple") +
      stat_ellipse(level = 0.05, color = "orange") +
      xlim(0, 10) + ylim(-.4, .4) +
      theme_linedraw(base_size = 10) +
      theme(text = element_text(size=rel(5)),
            plot.title = element_text(size=rel(7), hjust = .5),
            legend.text = element_text(size=rel(5)),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            strip.text.x = element_text(size=rel(5)),
            strip.text.y = element_text(size=rel(5))) 
    
    
    p2 = data()$d %>% ggplot(aes(x=symptom, y=commitment)) +
      geom_abline(data = data()$pop, aes(intercept=mean(b0), slope=mean(b1), color="Populationsmittel"), lwd=2, lty=2) + 
      geom_abline(data = data()$pop %>% filter(Stichprobe == "hypothetisch"), aes(intercept=b0, slope=b1, col=Stichprobe), lwd=.5, alpha=.5) +
      geom_abline(data = data()$pop %>% filter(Stichprobe == "tatsächlich"), aes(intercept=b0, slope=b1, col=Stichprobe), lwd=2, alpha=1) +
      scale_color_manual(name = "Stichprobe", # Add a name for the legend title
                         values = c("Populationsmittel" = "black", # Assign color to "Mean Line"
                                    "hypothetisch" = "black",    # Assuming 'black' is a value in 'Land'
                                    "tatsächlich" = "red")) +     # Assuming 'red' is a value in 'Land'
      ylim(0, 10) + xlim(0, 10) +
      labs(x = "Symptomschwere", y = "Commitment", title = "Populationsmodell auf Ebene 1") +
      theme_linedraw(base_size = 10) +
      theme(text = element_text(size=rel(5)),
            plot.title = element_text(size=rel(7), hjust = .5),
            legend.text = element_text(size=rel(5)),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            strip.text.x = element_text(size=rel(5)),
            strip.text.y = element_text(size=rel(5))) 
    
    p = ggarrange(p1, p2, ncol = 1)
    p
  })
  
  output$sample_plot <- renderPlot({
    
    
    p2 = data()$d %>% ggplot(aes(x=symptom,y=commitment)) +
      geom_abline(aes(intercept=b0, slope=b1), lwd=2) +
      geom_point(size=6, shape=21, fill="black", alpha=.5) +
      ylim(0, 10) +
      labs(x="Symptomschwere", y ="Commitment", title = "Stichprobenebene") +
      scale_x_continuous(breaks = seq(0, 10, 1)) +
      theme_linedraw(base_size = 10) +
      theme(text = element_text(size=rel(5)),
            plot.title = element_text(size=rel(7), hjust = .5),
            legend.text = element_text(size=rel(5)),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            strip.text.x = element_text(size=rel(5)),
            strip.text.y = element_text(size=rel(5)))  +
      facet_wrap(~ therapy, labeller = as_labeller(function(x) paste("Therapieart", x)))
    p2
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = "S13_01_randomcoefficients.csv",
    content = function(file) {
      write.csv(data()$d %>% select(-c(b0, b1)), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)

