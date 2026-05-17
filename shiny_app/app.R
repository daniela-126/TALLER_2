library(shiny)
library(plotly)

# =========================================================
# INTERFAZ
# =========================================================

ui <- fluidPage(
  
  titlePanel("Simulación Interactiva de Distribuciones y TCL"),
  
  navbarPage(
    title = "",
    
    # =====================================================
    # DISTRIBUCIÓN NORMAL
    # =====================================================
    
    tabPanel(
      "Normal",
      
      sidebarLayout(
        
        sidebarPanel(
          
          sliderInput(
            "mu",
            "Media (μ):",
            min = -5,
            max = 5,
            value = 0,
            step = 0.1
          ),
          
          sliderInput(
            "sigma",
            "Desviación estándar (σ):",
            min = 0.5,
            max = 5,
            value = 1,
            step = 0.1
          )
        ),
        
        mainPanel(
          plotlyOutput("normalPlot", height = "600px")
        )
      )
    ),
    
    # =====================================================
    # JI-CUADRADO
    # =====================================================
    
    tabPanel(
      "Ji-cuadrado",
      
      sidebarLayout(
        
        sidebarPanel(
          
          sliderInput(
            "chi_df",
            "Grados de libertad:",
            min = 1,
            max = 30,
            value = 5
          )
        ),
        
        mainPanel(
          plotlyOutput("chiPlot", height = "600px")
        )
      )
    ),
    
    # =====================================================
    # T STUDENT
    # =====================================================
    
    tabPanel(
      "t Student",
      
      sidebarLayout(
        
        sidebarPanel(
          
          sliderInput(
            "t_df",
            "Grados de libertad:",
            min = 1,
            max = 30,
            value = 10
          )
        ),
        
        mainPanel(
          plotlyOutput("tPlot", height = "600px")
        )
      )
    ),
    
    # =====================================================
    # F FISHER
    # =====================================================
    
    tabPanel(
      "F Fisher",
      
      sidebarLayout(
        
        sidebarPanel(
          
          sliderInput(
            "f_df1",
            "GL Numerador:",
            min = 1,
            max = 30,
            value = 5
          ),
          
          sliderInput(
            "f_df2",
            "GL Denominador:",
            min = 1,
            max = 30,
            value = 10
          )
        ),
        
        mainPanel(
          plotlyOutput("fPlot", height = "600px")
        )
      )
    ),
    
    # =====================================================
    # TCL
    # =====================================================
    
    tabPanel(
      "Teorema del Límite Central",
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "dist",
            "Distribución:",
            choices = c(
              "Binomial",
              "Poisson",
              "Hipergeométrica"
            )
          ),
          
          conditionalPanel(
            condition = "input.dist == 'Binomial'",
            
            numericInput(
              "size",
              "Ensayos:",
              10,
              min = 1
            ),
            
            sliderInput(
              "p",
              "Probabilidad:",
              0.3,
              0,
              1,
              step = 0.01
            )
          ),
          
          conditionalPanel(
            condition = "input.dist == 'Poisson'",
            
            numericInput(
              "lambda",
              "Lambda:",
              4,
              min = 0.1
            )
          ),
          
          conditionalPanel(
            condition = "input.dist == 'Hipergeométrica'",
            
            numericInput(
              "Npop",
              "Tamaño población:",
              1000,
              min = 100
            ),
            
            numericInput(
              "K",
              "Número de éxitos:",
              300,
              min = 1
            ),
            
            numericInput(
              "m",
              "Tamaño muestra:",
              20,
              min = 1
            )
          ),
          
          hr(),
          
          sliderInput(
            "n",
            "Tamaño de muestra TCL:",
            min = 5,
            max = 100,
            value = 30
          ),
          
          sliderInput(
            "Nsim",
            "Número de simulaciones:",
            min = 1000,
            max = 20000,
            value = 5000,
            step = 1000
          )
        ),
        
        mainPanel(
          plotlyOutput("tclPlot", height = "650px")
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output) {
  
  # =====================================================
  # NORMAL
  # =====================================================
  
  output$normalPlot <- renderPlotly({
    
    x <- seq(-10, 10, length.out = 1000)
    
    y <- dnorm(
      x,
      mean = input$mu,
      sd = input$sigma
    )
    
    plot_ly() %>%
      
      add_lines(
        x = x,
        y = y,
        
        line = list(
          color = "deepskyblue",
          width = 4
        ),
        
        name = "Normal"
      ) %>%
      
      layout(
        title = "Distribución Normal",
        
        template = "plotly_dark",
        
        xaxis = list(title = "x"),
        
        yaxis = list(title = "Densidad")
      )
  })
  
  # =====================================================
  # CHI CUADRADO
  # =====================================================
  
  output$chiPlot <- renderPlotly({
    
    x <- seq(0, 20, length.out = 1000)
    
    y <- dchisq(x, df = input$chi_df)
    
    plot_ly() %>%
      
      add_lines(
        x = x,
        y = y,
        
        line = list(
          color = "orange",
          width = 4
        )
      ) %>%
      
      layout(
        title = "Distribución Ji-cuadrado",
        
        template = "plotly_dark"
      )
  })
  
  # =====================================================
  # T STUDENT
  # =====================================================
  
  output$tPlot <- renderPlotly({
    
    x <- seq(-5, 5, length.out = 1000)
    
    y <- dt(x, df = input$t_df)
    
    plot_ly() %>%
      
      add_lines(
        x = x,
        y = y,
        
        line = list(
          color = "mediumseagreen",
          width = 4
        )
      ) %>%
      
      layout(
        title = "Distribución t Student",
        
        template = "plotly_dark"
      )
  })
  
  # =====================================================
  # F FISHER
  # =====================================================
  
  output$fPlot <- renderPlotly({
    
    x <- seq(0, 5, length.out = 1000)
    
    y <- df(
      x,
      df1 = input$f_df1,
      df2 = input$f_df2
    )
    
    plot_ly() %>%
      
      add_lines(
        x = x,
        y = y,
        
        line = list(
          color = "violet",
          width = 4
        )
      ) %>%
      
      layout(
        title = "Distribución F Fisher",
        
        template = "plotly_dark"
      )
  })
  
  # =====================================================
  # TCL
  # =====================================================
  
  output$tclPlot <- renderPlotly({
    
    set.seed(123)
    
    # -------------------------------------------------
    # BINOMIAL
    # -------------------------------------------------
    
    if(input$dist == "Binomial"){
      
      medias <- replicate(
        input$Nsim,
        mean(
          rbinom(
            input$n,
            size = input$size,
            prob = input$p
          )
        )
      )
    }
    
    # -------------------------------------------------
    # POISSON
    # -------------------------------------------------
    
    else if(input$dist == "Poisson"){
      
      medias <- replicate(
        input$Nsim,
        mean(
          rpois(
            input$n,
            lambda = input$lambda
          )
        )
      )
    }
    
    # -------------------------------------------------
    # HIPERGEOMÉTRICA
    # -------------------------------------------------
    
    else{
      
      medias <- replicate(
        input$Nsim,
        mean(
          rhyper(
            input$n,
            m = input$K,
            n = input$Npop - input$K,
            k = input$m
          )
        )
      )
    }
    
    # CURVA NORMAL
    
    x <- seq(
      min(medias),
      max(medias),
      length.out = 1000
    )
    
    y <- dnorm(
      x,
      mean = mean(medias),
      sd = sd(medias)
    )
    
    # GRÁFICA
    
    plot_ly() %>%
      
      add_histogram(
        x = medias,
        
        histnorm = "probability density",
        
        nbinsx = 25,
        
        marker = list(
          color = "yellowgreen",
          
          line = list(
            color = "white",
            width = 1
          )
        ),
        
        opacity = 0.85,
        
        name = "Medias muestrales"
      ) %>%
      
      add_lines(
        x = x,
        y = y,
        
        line = list(
          color = "darkblue",
          width = 4,
          dash = "dash"
        ),
        
        name = "Curva Normal"
      ) %>%
      
      layout(
        
        title = paste(
          "Teorema del Límite Central -",
          input$dist
        ),
        
        template = "plotly_white",
        
        bargap = 0.05,
        
        xaxis = list(
          title = "Media muestral"
        ),
        
        yaxis = list(
          title = "Densidad"
        ),
        
        legend = list(
          orientation = "h",
          x = 0.25,
          y = -0.2
        )
      )
  })
}

# =========================================================
# EJECUTAR APP
# =========================================================

shinyApp(ui = ui, server = server)