# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(shiny, DT, shinyWidgets, shinycssloaders)

library(shiny)
library(shinyWidgets)
library(DT)
library(shinycssloaders)



ui <- fluidPage(

    # Application title
    titlePanel("Simulation: lancer de pièces de monnaie"),
    withMathJax(),
    tags$div(HTML("<script type='text/x-mathjax-config' >
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script >
                ")),
    sidebarLayout(
        sidebarPanel(
            numericInput("ns",
                         "Nombre d'essais:",
                         min = 1000,
                         max = 100000,
                         value = 10000,
                         step = 1000,
                         width = 100),
            sliderInput("n",
                        "Nombre de pièces:",
                        min = 2,
                        max = 25,
                        value = 10,
                        step = 1),
            sliderInput("nx",
                        "Facteur:",
                        min = 1,
                        max = 20,
                        value = 1,
                        step = 1),
 
            splitLayout(
              cellWidths = c("70%", "30%"), 
                  sliderInput("p",
                        "Probabilité d'une FACE:",
                        min = 0,
                        max = 1,
                        value = .5,
                        step = .01,
                        width = "75%"),

                   actionButton("go",
                                "Exécuter")
                   ),

            wellPanel(style="background: lightblue",
                      titlePanel(h3("Mode d'emploi")),
                      strong(h4("Onglet Empirique:")),
                      helpText(h4("La distribution de probabilités est obtenue par simulation. ",
                      "Par défaut, on lance n pièces de monnaie 10,000 fois, comptabilisant",
                      "le nombre de FACEs observées à chaque lancer. Ce nombre peut être changé et prendre",
                      "toute valeur comprise entre 1000 et 100,000 en utilisant les flèches de contrôle, ou ",
                      "toute autre valeur inscrite manuellement dans la case.",
                      br(),
                      br(),
                      "Le nombre de pièces utilisées est contrôlé par les curseurs libellés 'Nombre de pièces'",
                      "et 'Facteur'. Par défaut, n = 10, et le facteur est fixé à 1. Le produit de ces deux",
                      "nombres donne le nombre de pièces. Ceci permet d'examiner la distribution du lancer",
                      "de 5 à 25x20 = 500 pièces.",
                      br(),
                      br(),
                      "Finalement, la probabilité d'obtenir FACE est déterminée par le curseur libellé",
                      "'Probabilité d'une FACE', qui peut prendre une valeur comprise entre 0 et 1, par étape",
                      "de 0.1.",
                      br(),
                      br(),
                      "Les résultats sont présentés sous forme d'un diagramme en barres, et sous forme d'un",
                      "tableau reproduisant les probabilités de chacun des résultats possibles." 
                       )
                      ),
                      strong(h4("Onglet Théorique:")),
                      helpText(h4("La présentation et les fonctionnalités demeurent inchangées, mais la distribution ",
                                  "est obtenue à partir de la Loi binomiale plutôt que par simulation."))
                    ),
            
            wellPanel(style = "background: lightblue",
                      fluidRow(
                        shiny::column(4,
                               a(h4("Par Daniel Coulombe, Ph.D.")),
                               p("2022")
                        ),
                        shiny::column(4,
                               tags$a(
                                 href="https://isteah.org", 
                                 tags$img(src="ISTEAH_LOGO.png", 
                                          title="ISTEAH", 
                                          width="160",
                                          height="140")
                               )
                        )
                      )
            )
            ),
        

        mainPanel(
          tabsetPanel(
            tabPanel(
            title = "Empirique",
            uiOutput("descript"),
            uiOutput("startinst"),
            br(),
            plotOutput("distPlot")  |> 
              shinycssloaders::withSpinner(
                type = 1, 
                color.background = "white"
              ), 
            br(),
            br(),
           
            fluidRow(
              column(6, 
                   uiOutput("probbtw")),  
              
              column(3, 
                   textOutput("calc3"),
                   tags$head(tags$style("#calc3{color: red;
                                 font-size: 30px;
                                 font-style: bold;
                                 }"
                                     )
                             )
                   )
              ),
    

           dataTableOutput(outputId = "freq",
                           width = "60%"),

           ),
          tabPanel(
            title = "Théorique",
            uiOutput("descript2"),
            plotOutput("distplot2")|> 
              shinycssloaders::withSpinner(
                type = 1, 
                color.background = "white"
              ), 

            fluidRow(
              column(6, 
                     uiOutput("probbtw2")),
              column(3, 
                     textOutput("calc32"),
                     tags$head(tags$style("#calc32{color: red;
                                 font-size: 30px;
                                 font-style: bold;
                                 }"
                     )
                     )
              )
            ),

            br(),
            br(),
            dataTableOutput(outputId = "freq2",
                            width = "60%")
          ),
          tabPanel(
            title = "Exercices",
            uiOutput("exercices")
          )
          )
          
        )
    )
)

server <- function(input, output) {
  
    N <- eventReactive(input$go, {input$n * input$nx})
    data <- eventReactive(input$go, {replicate(input$ns, sum(sample(c(1, 0), N(), replace=TRUE, prob=c(input$p, 1-input$p)) == 1))})
    Freq <- eventReactive(input$go, {as.matrix(table(data()))})
    Prop <- eventReactive(input$go, {Freq()[, 1]/input$ns})
    x <- eventReactive(input$go, {sort(unique(data()))})
    pth <- eventReactive(input$go, {dbinom(0:N(), N(), input$p)})

    output$distPlot <- renderPlot({
      
      datcol <- x()[(x() >= input$ll[1]) & (x() <= input$ll[2])]
      cols <- ifelse(x() %in% datcol, "red","darkgray")
      espm <- sum(x() * Prop())
      varx <- sum(Prop() * (x() - espm)^2)
      barplot(Prop(),  
              main="Distribution de Probabilités",  
              xlab="Nombre de FACEs",  
              ylab="Probabilité",  
              col = cols, 
              border = 'blue',
              cex.main=2,
              cex.lab=1.5)
      legend("topright", 
             legend = c(paste0("E(x)   = ", round(espm, 3)),
                        paste0("Var(x) = ", round(varx, 3))),
             col = c("black", "black"),
             cex=1.2,
             title = "Paramètres",
             text.font = 2,
             bg = "lightblue",
             box.lty = 1,
             box.lwd = 2)
    })
    
    output$freq <- renderDataTable({
      dat <- data.frame(x(), Freq(), Prop(), cumsum(Prop()))
      datatable(dat,
                rownames = FALSE,
                colnames=c("X", "Fréquences", "Probabilités", "Probabilités Cumulatives"),
                options = list(dom = "liptr",
                               pageLength = 15,
                               lengthMenu = c(5, 10, 15, 25, 50, 100)))
    })
    
    output$descript <- renderUI({
      helpText(h3("Expérience Aléatoire: Lancer de ", strong(input$n * input$nx), "pièces de monnaie"),
                  h4(" \u2022  Un de deux événements possibles ['FACE/PILE'] survient à chaque essai",
                  br(),
                  paste(" \u2022  ", "P[FACE] = ", input$p, " est invariante d'un essai à l'autre"),
                  br(),
                  " \u2022  Les essais sont indépendants les uns des autres"))
    })
      
     output$startinst <- renderUI({
       helpText(h3("Cliquez sur ", strong("Exécuter"), " pour débuter ou ré-initialiser..."))
     }) 
    
    output$probbtw <- renderUI({

      sliderInput("ll", 
                  label = h3("P( x \u2264 X \u2264 y )"),
                  value = c(0, N()),
                  min = 0,
                  max = N(),
                  step = 1)
    })

    output$calc3 <- renderText({
      if(is.null(input$ll)){
        return()
      }
      paste(" = ", round(mean((data() >= input$ll[1]) & (data() <= input$ll[2])), 4))
    })
    
    output$descript2 <- renderUI({
      withMathJax(
      helpText(h3("Expérience Aléatoire: Lancer de ", strong(input$n * input$nx), "pièces de monnaie"),
               h4(" \u2022  Un de deux événements possibles ('FACE/PILE') survient à chaque essai",
                  br(),
                  " \u2022  ", strong("P(FACE) = ", input$p), "est invariante d'un essai à l'autre",
                  br(),
                  " \u2022  Les essais sont indépendants les uns des autres")),
      helpText(h4("Les probabilités ont été obtenues en appliquant la fonction $$p(x)=C_x^n p^x (1-p)^{n-x}$$",
                  "Les probabilités obtenues empiriquement sont ajoutées au tableau pour permettre les comparaisons."))
       )
      })
    
    output$distplot2 <- renderPlot({

      xx <- 0:N()

      datcol2 <- x()[(x() >= input$ll2[1]) & (x() <= input$ll2[2])]
      cols <- ifelse(xx %in% datcol2, "red","darkgray")
      espm2 <- sum(xx * pth())
      varx2 <- sum(pth() * (xx - espm2)^2)
      barplot(pth(),  
              main="Distribution de Probabilités",  
              xlab="Nombre de FACEs",  
              ylab="Probabilité", 
              names.arg = 0:N(),
              col = cols, 
              border = 'blue',
              cex.main=2,
              cex.lab=1.5)
      legend("topright", 
             legend = c(paste0("E(x)   = ", round(espm2, 3)),
                        paste0("Var(x) = ", round(varx2, 3))),
             col = c("black", "black"),
             cex=1.2,
             title = "Paramètres",
             text.font = 2,
             bg = "lightblue",
             box.lty = 1,
             box.lwd = 2)
    })
    
    output$calc12 <- renderText({
      if(is.null(input$greater2)){
        return()
      }
      paste(" = ", round(1 - pbinom(input$greater2 - 1, input$n * input$nx, input$p), 4))
    })
    
    output$probbtw2 <- renderUI({
      
      sliderInput("ll2", 
                  label = h3("P( x \u2264 X \u2264 y )"),
                  value = c(0, N()),
                  min = 0,
                  max = N(),
                  step = 1)
    })
    
    output$calc32 <- renderText({
      if(is.null(input$ll2)){
        return()
      }
      paste(" = ", round(pbinom(input$ll2[2], input$n * input$nx, input$p) - pbinom(input$ll2[1] - 1, input$n * input$nx, input$p), 4))
    })
    
    output$freq2 <- renderDataTable({
      prnul <- rep(NA, N() + 1)
      prnul[which(0:N() %in% sort(unique(x())) == TRUE)] <- Prop()
      cpth <- cumsum(pth())
      dat <- data.frame(0:N(), pth(), cpth, prnul)
      datatable(round(dat, 6),
                rownames = FALSE,
                colnames=c("X", "Prob. Théorique", "Prob. Cumulative", "Prob. Empirique"),
                options = list(dom = "liptr",
                               pageLength = 15,
                               lengthMenu = c(5, 10, 15, 25, 50, 100)
                ))
      
    }) 
    
    output$exercices <- renderUI({
      withMathJax(
      helpText(
        strong(h3("Exercice 1:")), p(),
        h4(
          "Supposez une collection de 20 pièces de monnaie trafiquées de manière à ce que la probabilité d'obtenir une FACE ",
          "est $p=0.75$. En lançant ces pièces:", p(),
          "a. Quelle est la probabilité d'obtenir moins de 12 FACEs?", p(),
          "b. Quelle est la probabilité d'obtenir au plus 14 FACEs?", p(),
          "c. Quelle est la probabilité d'obtenir 15 FACEs ou plus?", p(),
          "d. Quelle est la probabilité d'obtenir exactement 17 FACEs?", p(),
          "e. Quelle est la probabilité d'obtenir plus de 12 FACEs, mais moins de 17?", p(),
        ), p(),
        strong(h3("Exercice 2:")), p(),
        h4( 
          "Un service de messagerie indique que 95% des colis expédiés sont livrés en 2 jours ou moins. Pour un échantillon de ",
          "6 colis expédiés:", p(),
          "a. quelle est la probabilité que les 6 colis soient livrés en 2 jours ou moins?", p(),
          "b. quelle est la probabilité que 5 colis soient livrés en 2 jours ou moins?", p()
        ),
        
        strong(h3("Exercice 3:")), p(),
        h4(
        "Une compagnie aérienne a mis en place pour une de ses lignes un système de sur-réservation afin d’abaisser les coûts. ",
        "Sur cette ligne, la compagnie affrète un appareil de 190 places et a vendu 200 réservations. On suppose que le nombre ",
        "de clients se présentant à l’embarquement peut être modélisé par une variable aléatoire  qui suit la loi binomiale dont ",
        "les paramètres sont $n=200$ et $p=0.90$.", p(),
        "a. Calculer la probabilité que la compagnie se trouve en situation de sur-réservation $($c’est-à-dire avec plus de ",
        "   clients qui se présentent à l’embarquement que de places diponibles$)$.", p(),
        "b. En supposant qu'un minimum de 175 passagers sont requis pour éviter un vol déficitaire, quelle est la probabilité que ",
        "   la compagnie aérienne subisse des pertes pour un vol.", p(),
        
      ), p()
      

        )
      )
    })
}

shinyApp(ui = ui, server = server)
