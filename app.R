                
rsconnect::setAccountInfo(name='xochitlcardenasmtz', token='9AAB8532CA48CEF571BD5DFAB337D7C2', secret='CatoYk3lluPChfHQEUm3faU9aNu1G7XhwUQB4Sfc')


#Parametros fijos:
proba <- 0:1

#Funciones:

modelokyt <- function(gamma,proba) {return((proba^gamma)/(((proba^gamma)+(1-proba)^gamma)^1/gamma))}


modelogold <- function(delta, gamma, proba) {return(proba^gamma / delta*proba^gamma + (1-proba)^gamma)}


modelowu <- function(delta,gamma,proba){return(proba^gamma/(proba^gamma+(1-proba)^gamma)^delta)}


modelo1p <- function(gamma,proba){return(exp(-(-log(proba))^gamma))}

#Inicio de código para shinyapp

ui <- fluidPage(
  shinyUI(
    navbarPage("Teoría del Prospecto",    #navbarPage para abrir pestañas superiores
        tabPanel("Teoría del Prospecto",          #Iniciar código de primer pestaña 
            tags$h1("Teoría del Prospecto"),
            tags$h4("Xochitl Cárdenas - Lab 25"),
            tags$p("La Teoría del Prospecto, propuesta en 1979 por Kahneman y Tversky, surge como una alternativa descriptiva a la teoría de Utilidad para elección bajo riesgo.",
                "Esta teoría mantiene un principio de maximización asumiendo que las personas transforman los valores reales de probabilidad y recompensa en valores subjetivos. Dicha transformación se lleva a cabo en dos fases,",
                "la primera es una de edición que consiste en interpretar un prospecto con respecto a un punto de referencia a partir del cual se computa el valor de las consecuencias en términos de ganancias o perdidas. La segunda fase",
                "es una de evaluación, en la cual se evaluan y editan los prospectos a partir de dos funciones. Estas funciones son la función de valor y la función de ponderación, las cuales permiten transformar los valores objetivos en subjetivos.",
                br(),
                "El valor de un prospecto simple es definido como:",
                br(em("V(x,p)= w(p)(v(x))")),
                "donde v calcula el valor subjetivo de la consecuencia x, mientras que w lo hace con el impacto de la probabilidad p.",
                br(),
                br(),
                br(em("Puedes encontrar el código de está app en el siguiente link: ",
                ))),
            tags$a (href="//github.com/xochitlcardenas/Teoria-del-Prospecto-Shinyapp.git", "Github repository")
        ),
        
        # tabPanel(title = "Función de Valor",                 #segunda pestaña
        #   tags$h1("Función de valor"),
        #   tags$p("La función de utilidad es remplazada por la función de valor en ganancias o pérdidas relativo a un punto de referencia.",
        #           "Esta es una función psicofisica ya que refleja el impacto marginal de un cambio de valor disminuye con la distancia entre el",
        #           "punto de referencia, lo cuál genera una forma concava a la representación grafica para ganancias y convexa para pérdidas."), 
        # tabsetPanel(
        #   tabPanel(
        #     title = "",
        #     tags$h3(),
        #     tags$p(),
        #     sidebarLayout(
        #       sidebarPanel(),
        #       mainPanel()
        #     ) #Cierra Layout
        #   ) #cierra tabpanel
        # ) #cierra tabsetPanel
        #   ), #cierra tabPanel Funcion de valor 
        
        tabPanel(title = "Función de ponderación",          #tercer pestaña 
            tags$h1("Función de ponderación"),
            tags$p("Siendo la función de ponderación la segunda escala para evaluar un prospecto, esta función representa el impacto de la probabilidad de obtener un resultado sobre la valoración del prospecto. Esto se obtiene al ponderar la recompensa por un peso de decisión.",
                "La representación gráfica de esta función adopta una forma de S invertida que muestra la sobreponderacion de probabilidades bajas y subponderaciones para probabilidades grandes. "),
            tags$h3("Parametrización"),
            tags$p("Para la aplicación de la Teoría del Prospecto se requiere evaluar y modelar de manera individual cada una de las funciones." ,
                br(em("Te invito a que juegues con los valores y observes cómo se modifica la gráfica"))),
            tabsetPanel(                              #abre subpestañas en pestaña superior 
                
                tabPanel(                               #primer pestaña
                    title = "K y T",
                    tags$h3("Modelo de Parametrización de Kahneman y Tversky"),
                    tags$p("Tversky y Kahneman (1992) proponen una función de ponderación con un único parámetro (γ).",
                        br(em("     w(p)= p^γ / (p^γ + (1 - p)^γ)^γ"))),
                    sidebarLayout(                       #divide pagina en dos partes
                        sidebarPanel(                      #primer parte, aparece barra de parametro variable
                            sliderInput("gamma1", "Elige un valor de gamma", min = 0, max = 1, value = 0.5)
                        ),
                        mainPanel(                         #segunda parte, aparece gráfica 
                            plotOutput("modelokytplot")      #variable que permite leer código de gráfica (se encuentra en server) 
                        )#cierra mainPanel
                    ) #cierra sidebarLayout
                ), #cierra tabPanel de tabsetPanel "k Y t"
                
                tabPanel(
                    title = "Modelo de Goldstein",
                    tags$h3("Modelo de Parametrización de Goldstein - Einhorn"),
                    tags$p("En este modelo se incluye un segundo parámetro (δ) que controla la elevación de la función de ponderación, y el parámetro (γ) que controla la curvatura.", 
                        br(em("           w(p) = p^γ / δ*p^γ + (1-p)^γ"))),
                    sidebarLayout(
                        sidebarPanel(
                            sliderInput("delta2", "Elige un valor de delta", min = 0, max = 1, value = 0.5),
                            sliderInput("gamma2", "Elige un valor de gamma", min = 0, max = 1, value = 0.5)
                        ),
                        mainPanel(
                            plotOutput("modelogoldplot")
                        )
                    ) #cierra layout
                ), #cierra tabPanel
                
                tabPanel(
                    title = "Modelo de Wu",
                    tags$h3("Modelo de Parametrización de Wu y Gonzalez"),
                    tags$p("Este modelo de parametrización incluye dos parámetros (γ y δ).",
                        br(em("         w(p) = p^γ / (p^γ + (1-p) ^γ)^ δ "))),
                    sidebarLayout(
                        sidebarPanel(
                            sliderInput("gamma3", "Elige un valor de gamma", min = 0, max = 1, value = 0.5),
                            sliderInput("delta3", "Elige un valor de delta", min = 0, max = 1, value = 0.5)
                        ),
                        mainPanel(
                            plotOutput("modelowuplot")
                        )
                    )
                ), #cierra tabPanel modelo de wu 
                
                tabPanel(
                    title = "Single Parameter",
                    tags$h3("Modelo de Parametrización de Un Parametro"),
                    tags$p("Prelec (1998) deriva una función de la función de ponderación que ajusta 3 principios:",
                        br("-Sobreponderación y subponderación de probabilidades"),
                        br("-Subproporcionalidad de ponderaciones de decisión"),
                        br("-Subaditividad de ponderaciones de decisión"),
                        br(em("                w(p) = exp[-(-lnp)^γ]"))),
                    sidebarLayout(
                        sidebarPanel(
                            sliderInput("gamma4", "Elige un valor de gamma", min = 0, max = 1, value = 0.5)
                        ),
                        mainPanel(
                            plotOutput("modelo1pplot")
                        )
                    )
                ) #cierra tabpanel de "sigle parameters" 
                
            ) #cierra el tabsetpanel
        ) #cierra el tabPanel
        
    ) #cierra el navbarPage
    
  ) #cierra shinyUI
  
) #cierra fluidPage
    
    
    
    server <- function(input, output){             #segunda parte de template para diseño de shinyapp 
        
        output$modelokytplot <- renderPlot ({        #render permite generar codigo de gráfica 
            curve(modelokyt(input$gamma1, x), #parametros del modelo
                from = 0, 
                to = 100,
                n = 1001,
                xlab = "probabilidad",
                ylab = "modelo",
                xlim = c(0,1),
                ylim = c(0,1),
                col ="darkslategrey",
                lwd = 2
            )#cierra funcion curve 
            abline(a = 0, b= 1,  lty = 2, col="gray", lwd = 1)
            
        }) #cierra renderPlot de modelokytplot
        
        output$modelogoldplot <- renderPlot ({
            curve(modelogold(input$delta2, input$gamma2, x), #parametros del modelo
                from = 0, 
                to = 100,
                n = 1001,
                xlab = "probabilidad",
                ylab = "modelo",
                xlim = c(0,1),
                ylim = c(0.5,3),
                col ="palevioletred4",
                lwd = 2
            )#cierra funcion curve 
            abline(a = 1, b= 1,  lty = 2, col="gray", lwd = 1)
            
        }) #cierra renderPlot de modelogoldplot
        
        output$modelowuplot <- renderPlot ({
            curve(modelowu(input$delta3, input$gamma3, x), #parametros del modelo
                from = 0, 
                to = 100,
                n = 1001,
                xlab = "probabilidad",
                ylab = "modelo",
                xlim = c(0,1),
                ylim = c(0,1),
                col ="pink4",
                lwd = 2
            )#cierra funcion curve 
            abline(a = 0, b= 1,  lty = 2, col="gray", lwd = 1)
            
        }) #cierra renderPlot de modelowuplot
        
        output$modelo1pplot <- renderPlot ({
            curve(modelo1p(input$gamma4, x), #parametros del modelo
                from = 0, 
                to = 100,
                n = 1001,
                xlab = "probabilidad",
                ylab = "modelo",
                xlim = c(0,1),
                ylim = c(0,1),
                col ="rosybrown4",
                lwd = 2
            )#cierra funcion curve 
            abline(a = 0, b= 1,  lty = 2, col="gray", lwd = 1)
            
        }) #cierra render 1pplot
        
    } #cierra server
    
    shinyApp(ui=ui, server=server)              #manda todo el código a la página de la app
