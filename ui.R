######################################################################
## SHINY_UI ##
######################################################################

library(shiny)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)
library(dplyr)
library(kableExtra)
library(shinydashboard)
library(dashboardthemes)

dataset <- ds

shinyUI(fluidPage(
  
  dashboardPage(
  dashboardHeader(title = "Menu",
                  dropdownMenu(
                  )
  ),
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText",
                      buttonId = "searchButton",
                      label = "Search..."),
    sidebarMenu(
      menuItem("Inicio",tabName = "inicio"),
      menuItem("Dataset Overview",tabName = "dataset"),
      menuItem("Analisis Exploratorio",tabName = "analisis"),
      menuItem("¿Qué es más probable?", tabName = "poblaciones"),
      menuItem("Inferencia", tabName = "inferencia"),
      menuItem("Modelos", tabName = "modelos"),
      menuItem("Confianza", tabName = "confianza"),
      menuItem("Show Code", tabName = "code")
    )
  ),
  
  dashboardBody(
    customTheme <- shinyDashboardThemeDIY(
      
      ### general
      appFontFamily = "Arial"
      ,appFontColor = "#61C5D7"
      ,primaryFontColor = "black"
      ,infoFontColor = "rgb(0,0,0)"
      ,successFontColor = "rgb(0,0,0)"
      ,warningFontColor = "rgb(0,0,0)"
      ,dangerFontColor = "rgb(0,0,0)"
      ,bodyBackColor = "#fff"
      
      ### header
      ,logoBackColor = "rgb(55,72,80)"
      
      ,headerButtonBackColor = "rgb(55,72,80)"
      ,headerButtonIconColor = "#F0F0F0"
      ,headerButtonBackColorHover = "rgb(55,72,80)"
      ,headerButtonIconColorHover = "#F0F0F0"
      
      ,headerBackColor = "rgb(55,72,80)"
      ,headerBoxShadowColor = "#fff"
      ,headerBoxShadowSize = "2px 2px 2px"
      
      ### sidebar
      ,sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "rgb(55,72,80)"
        ,colorMiddle = "rgb(55,72,80)"
        ,colorEnd = "rgb(55,72,80)"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
      )
      ,sidebarPadding = 0
      
      ,sidebarMenuBackColor = "transparent"
      ,sidebarMenuPadding = 0
      ,sidebarMenuBorderRadius = 0
      
      ,sidebarShadowRadius = "1px 5px 5px"
      ,sidebarShadowColor = "#fff"
      
      ,sidebarUserTextColor = "rgb(255,255,255)"
      
      ,sidebarSearchBackColor = "rgb(55,72,80)"
      ,sidebarSearchIconColor = "rgb(153,153,153)"
      ,sidebarSearchBorderColor = "rgb(55,72,80)"
      
      ,sidebarTabTextColor = "rgb(255,255,255)"
      ,sidebarTabTextSize = 13
      ,sidebarTabBorderStyle = "none none solid none"
      ,sidebarTabBorderColor = "rgb(55,72,80)"
      ,sidebarTabBorderWidth = 1
      
      ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "#17B3BF"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
      )
      ,sidebarTabTextColorSelected = "rgb(0,0,0)"
      ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
      
      ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
      )
      ,sidebarTabTextColorHover = "rgb(50,50,50)"
      ,sidebarTabBorderStyleHover = "none none solid none"
      ,sidebarTabBorderColorHover = "rgb(75,126,151)"
      ,sidebarTabBorderWidthHover = 1
      ,sidebarTabRadiusHover = "0px 20px 20px 0px"
      
      ### boxes
      ,boxBackColor = "rgb(55,72,80)"
      ,boxBorderRadius = 5
      ,boxShadowSize = "0px 0px 0px"
      ,boxShadowColor = "#fff"
      ,boxTitleSize = 16
      ,boxDefaultColor = "#fff"
      ,boxPrimaryColor = "#fff"
      ,boxInfoColor = "#fff"
      ,boxSuccessColor = "rgb(55,72,80)"
      ,boxWarningColor = "rgb(55,72,80)"
      ,boxDangerColor = "rgb(55,72,80)"
      
      ,tabBoxTabColor = "rgb(55,72,80)"
      ,tabBoxTabTextSize = 14
      ,tabBoxTabTextColor = "rgb(55,72,80)"
      ,tabBoxTabTextColorSelected = "rgb(55,72,80)"
      ,tabBoxBackColor = "rgb(55,72,80)"
      ,tabBoxHighlightColor = "rgb(55,72,80)"
      ,tabBoxBorderRadius = 5
      
      ### inputs
      ,buttonBackColor = "rgb(55,72,80)"
      ,buttonTextColor = "rgb(55,72,80)"
      ,buttonBorderColor = "rgb(55,72,80)"
      ,buttonBorderRadius = 5
      
      ,buttonBackColorHover = "rgb(235,235,235)"
      ,buttonTextColorHover = "rgb(100,100,100)"
      ,buttonBorderColorHover = "rgb(200,200,200)"
      
      ,textboxBackColor = "#fff"
      ,textboxBorderColor = "#fff"
      ,textboxBorderRadius = 5
      ,textboxBackColorSelect = "rgb(55,72,80)"
      ,textboxBorderColorSelect = "rgb(55,72,80)"
      
      ### tables
      ,tableBackColor = "#fff"
      ,tableBorderColor = "#fff"
      ,tableBorderTopSize = 1
      ,tableBorderRowSize = 1
      
    ),

    customTheme,
    
    tabItems(
      
      
      ############################################################### 
      ## Inicio ##
      ############################################################### 
             tabItem(tabName = "inicio", 
                     fluidRow( 
                       h1(id="titulo","Práctica Final Fundamentos Matematicos"),
                       tags$style(HTML("#titulo{color: #61C5D7; font-size: 30px; font-style: bold; text-align: center"))
                     ),
                     fluidRow( 
                       h2(id="nombre","Esther Vazquez Rodriguez, 2º MIT + MBD"),
                       tags$style(HTML("#nombre{color: rgb(55,72,80); font-size: 18px; font-style: bold; text-align: center"))
                     ),
                     fluidRow( 
                       box(title ="Objetivos y Necesidades", width = 12, 
                       h2(id="obj_text", "La demanda de TI en el campo de la medicina y la salud aumenta día a día, resultando un gran desafío
                         mejorar la calidad de la atención y el servicio brindado, haciendo un correcto diagnóstico y proporcionando los tratamientos
                         y medicamentos adecuados a los pacientes, pues un diagnóstico deficiente puede tener consecuencias graves en la salud que son inaceptables."),
                       tags$style(HTML("#obj_text{color: #fff; font-size: 13px; text-align: justify; marginLeft:10px")),
                       h2(id="obj_text", "Diagnosticar y predecir rápidamente las enfermedades de cualquier paciente, así como detectar sus complicaciones
                         supone explotar la gran variedad y abundancia de datos de manera correcta. La tecnología de la información en el ámbito de la salud
                         persigue mantenerse al día con los cambios imperativos en toda la industria que afecta a las ganancias, evitar tiempos de espera excesivos
                         que dañen relaciones médico-paciente, reconocer y equilibrar las necesidades de los pacientes y la del personal médico,
                         optimizar la atención al paciente y los flujos de ingresos, aprovechar los datos en tiempo real para situaciones de la vida real,
                         minimizar costes, etc. Según los Centros para el Control y la Prevención de Enfermedades (CDC), la enfermedad cardíaca es la principal causa de
                         muerte para hombres, mujeres y personas de la mayoría de los grupos raciales y étnicos del mundo. En esta práctica final se va a trabajar con un 
                         dataset generado por: "),a(href="https://www.kaggle.com/johnsmith88/heart-disease-dataset","Cleveland Clinic Heart Disease"),
                       h2(id="obj_text","Este dataset recoge 14 variables relacionadas con el diagnóstico del paciente y una variable resultado que indica
                         la presencia o ausencia de la enfermedad cardíaca. Cuenta con más de mil datos de pacientes, lo que supone un amplio set de datos del que
                         se puede extraer mucho conocimiento."),
                       a(href="https://www.youtube.com/watch?v=g5KBAnFbxFg","Acceso al vídeo explicativo de esta práctica final")
                     )),
              ),
      
      ############################################################### 
      ## Apartado 1: Dataset Overview ##
      ############################################################### 
      
          tabItem( tabName = "dataset",
               fluidRow(
                 h1(id="titulo","Dataset Overview"),
                        box(title = "El dataset contiene los siguientes campos", width = 6,
                            strong(id="obj_text","1. age:"),
                            p(id="obj_text","Edad del paciente
                            Rango de 29 a 76 años" ),
                            
                            strong(id="obj_text","2. sex:"),
                            p(id="obj_text","Sexo del paciente, siendo:
                              0: Mujer,
                              1: Hombre"),
                            
                            strong(id="obj_text","3. cp:"),
                            p(id="obj_text","Tipo de dolor en el pecho experimentado por el individuo, siendo:
                              0: Angina típica,
                              1: Angina atípica,
                              2: Dolor sin angina,
                              3: Angina asintomática"),
                            
                            strong(id="obj_text","4. trestbps:"),
                            p(id="obj_text"," Presión arterial en reposo en mm Hg, 
                              Rango de 94 a 200"),
                            
                            strong(id="obj_text","5. chol:"),
                            p(id="obj_text","Colesterol sérico en mg / dl,
                              Rango de 126 a 564"),
                            
                            strong(id="obj_text","6. fbs:"),
                            p(id="obj_text","Nivel de azúcar en sandre en ayunas > 120 mg/dl, siendo:
                              1: True,
                              0: False "),
                            
                            strong(id="obj_text","7. restceg"),
                            p(id="obj_text","Resultados electrocardiográficos en reposo, siendo:
                              0: Normal,
                              1: Anormalidad a la onda ST-T,
                              2: Hipertrofilia en el ventrículo izquierdo"),
                            
                            strong(id="obj_text","8. thalach"),
                            p(id="obj_text","Frecuencia cardíaca máxima del sujeto,
                              Rango de 71 a 202"),
                            
                            strong(id="obj_text","9. exang"),
                            p(id="obj_text","Angina inducida por ejercicio siendo:
                              0: No,
                              1: Si,"),
                            
                            strong(id="obj_text","10. oldpeak"),
                            p(id="obj_text","Segmento pico del segmento ST, 
                              Rango de 0 a 6.2"),
                            
                            strong(id="obj_text","11. slope"),
                            p(id="obj_text","La pendiente del segmento ST de ejercicio pico: 1 = pendiente ascendente, siendo
                              0: Cuesta abajo,
                              1: Plano, 
                              2: Cuesta arriba"),
                            
                            strong(id="obj_text","12. ca"),
                            p(id="obj_text","Número de vasos visibles debajo del fluoro"),
                            
                            strong( id="obj_text","13. thal"),
                            p(id="obj_text","Forma de thalasemia, siendo:
                              1: Normal,
                              2: Defecto fijo,
                              3: Defecto reversible"),
                            
                            strong( id="obj_text","14. num"),
                            p(id="obj_text","Indica si el paciente sufre enfermedad cardíaca o no, siendo:
                              0: No presente,
                              1: Presente")
                        ),
                        box(width = 6,
                                selectInput("analisis","¿Qué más quieres saber?",c("--Elige una opcion--","Número de observaciones","Número de variables","¿Hay datos ausentes?","Overview","Tipo de variables","¿Cuántas personas son positivas en enfermedad cardíaca?")),
                                textOutput("result"),
                                verbatimTextOutput("summaryDset"),
                                tableOutput("table"),
                        ),
               ),
                 ),

      ############################################################### 
      ## Apartado 2: Análisis Exploratorio ##
      ############################################################### 
      
      tabItem(tabName = "analisis",
              fluidRow(
                h1(id="titulo", "Análisis Exploratorio")
                ),
              fluidRow(
              box(width = 6,
                h2(id="obj_text","El objetivo de esta sección es realizar un análisis exploratorio básico del conjunto de datos con el fin de extraer
                          un conocimiento inicial del mismo. Esto se realiza con herramientas gráficas y estadísticas con las que podamos conocer las posibles
                          relaciones que existen entre las variables."),
                h2(id="obj_text","En el apartado anterior hemos visto que hay un total de 27 pacientes más con la enfermedad cardíaca. Ahora observamos
                   la relación que tiene el diagnóstico de enfermedad positiva (num = 1) con el resto de variables:"),

                column(16, offset = 1, selectInput('x', 'Enfermedad Cardíaca Presete + ', names(dataset)),
                verbatimTextOutput("defVar")),
                tags$style(HTML("#defVar{color: black; font-size: 12px; text-align: justify")),
                column(8, offset = 1,
                       selectInput('medida', 'Medida', c("--Elige una opcion--",
                                                         "Media", 
                                                         "Mediana",
                                                         "Desviación Típica",
                                                         "Varianza Muestral",
                                                         "Valores Atípicos",
                                                         "Posición Valores Atípicos",
                                                         "IQR","Cuantiles")),
                       verbatimTextOutput("sumMedida"),
                       h2(id="obj_text","A continuación con la función summary() podemos ver el rango de la variable (valor mínimo y valor máximo),
                          así como el valor del primer cuartil, del segundo (que corresponde con la mediana), de la media, y del tercer cuartil"),
                       selectInput('observaciones', 'Observaciones', c("--Elige una opcion--","Summary")),
                       verbatimTextOutput("sumObs"),
                       )),
                box(width = 6,
                  selectInput('grafico', 'Tipo de gráfico', c("--Elige una opcion--","Gráfico de densidad","Histrograma","Boxplot")),
                  textOutput("plotsD"),
                  plotOutput(outputId = "plots")
                ),
              ),
              fluidRow(
                box(width = 12,
                h2(id="obj_text","Las siguientes gráficas (divididas por un lado en histogramas para variables categóricas
                   y boxplots para variables numéricas), sugieren que las siguientes condiciones están asociadas con una mayor
                   prevalencia de la enfermedad cardíaca (esto no significa que la relación sea causal): "),
                br(),
                  strong(id="obj_text","- Dolor de Angina en pecho Atípica y sin Angina"),
                  p(""),
                  strong(id="obj_text","- Nivel del azúcar en sangre en ayunas > 120"),
                  p(""),
                  strong(id="obj_text","- Pendiente del segmento ST de ejercicio pico cuesta arriba"),
                  p(""),
                  strong(id="obj_text","- La enfermedad cardíaca es más predominante en hombres que en mujeres"),
                  p(""),
                  strong(id="obj_text","- Mayor puntuaje de talasemia con Defecto fijo"),
                  p(""),
                  strong(id="obj_text","- Edad entre los 45 y los 60 años"),
                  p(""),
                  strong(id="obj_text","- Mayor presión arterial en reposo"),
                  p(""),
                  strong(id="obj_text","- Colesterol alto"),

              )),
              box(width = 14,
                h2(id="obj_text","No todos podemos ser cardiólogos, pero estos datos parecen no pasar por alto,
                   particularmente la edad, presión arterial, colesterol y sexo apuntan en la dirección correcta según
                   lo que generalmente sabemos sobre el mundo que nos rodea."),
                plotOutput(outputId = "fact_plots"),
                plotOutput(outputId = "num_plots"),
                )
              ),
      
      ############################################################### 
      ## Apartado 3: ¿Qué es más probable? ##
      ############################################################### 
      
             tabItem(tabName = "poblaciones",
                     fluidRow(
                       h1(id="titulo","¿Qué es más probable?")),
                     fluidRow(
                     box(width = 5,
                       h2(id ="obj_text","Aunque ya lo hemos visto en la parte de Análisis Exploratorio, resulta interesante analizar
                          la posible relación entre el factor género (variable sex) y el factor presencia de enfermedad cardíaca (num)"),
                       h2(id="obj_text","El siguiente gráfico muestra lo que ya hemos deducido en el apartado anterior, es decir, que
                          la enfermedad cardíaca se presenta más en hombres que en mujeres."),
                       column(6,offset =1,
                          plotOutput(outputId = "rel_plot")
                     )),
                     box(width = 7,
                     column(9,offset=1,
                              selectInput('probabilidad', '¿?', c("--Elige una opcion--",
                                                                  "Tabla de probabilidad",
                                                                  "Probabilidades marginales",
                                                                  "¿Cuál es la probabilidad de que una persona elegida al azar corresponda a un caso positivo de enfermedad?",
                                                                  "¿Cuál es la probabilidad de que un positivo sea mujer?",
                                                                  "¿Y de que un hombre sea positivo?",
                                                                  "¿Y de que sea a la vez hombre y no posea la enfermedad?",
                                                                  "¿Son independientes las variables hombre y poseer la enfermedad?",
                                                                  "Si elegimos de forma independiente (con remplazamiento) 10 personas de esta muestra, ¿cuál es la probabilidad de que 4 de ellas sean positivas?",
                                                                  "¿Y cuál es la probabilidad de que lo sean 4 o menos?")),
                      textOutput("textOutProb"),
                      verbatimTextOutput("outProb"),
             )),
             box(width = 7,
                 h3("Prueba Chi"),
               h2(id="obj_text","Dado que 35 % y 40 % difieren bastante, vamos a hacer la prueba Chi, que es un contraste de hipótesis sobre la independencia de dos factores. 
                  La hipótesis nula de este contraste es que los factores son independientes."),
               verbatimTextOutput("outChi"),
               h2(id="obj_text", "Como el p-valor que ha salido es muy pequeño, rechazamos la hipótesis nula. Es decir, 
                  no podemos aceptar que los factores sean independientes, no permite sostener la hipótesis nula.")
             )),

             fluidRow(
               box(width = 5,
                   h4("Tabla cruzada"),
                   radioButtons(inputId="choice", label="¿Qué quieres comparar?", 
                                choices=c("Nivel medio de colesterol","Edad media","Nivel medio de presión arterial", "Nivel medio de frecuencia cardíaca máxima")),
                   h2(id="obj_text","Por ejemplo... Para las personas que padecen la enfermedad y que tienen edades superiores a 45 años ¿cuál es su nivel medio de colesterol?"),
                   verbatimTextOutput("pregunta1"),
                   ),
               box(width = 7, 
                   selectInput('var1',"Elige una variable con la que cruzarlo: ",
                                      c("sex",
                                        "cp",
                                        "restecg",
                                        "exang",
                                        "slope",
                                        "thal",
                                        "num")),
                   selectInput('var2',"Elige otra variable con la que cruzarlo: ",
                               c("cp",
                                 "restecg",
                                 "exang",
                                 "slope",
                                 "thal",
                                 "sex",
                                 "num")),
                   uiOutput("tablaCruzada")
                   )
             ),
             ),
      
      ############################################################### 
      ## Apartado 4: Inferencia ##
      ############################################################### 
      
             tabItem(tabName = "inferencia",
                     fluidRow(
                       h1(id="titulo","Inferencia"),),
                     
                     fluidRow(
                       box(width=7,
                           h2(id="obj_text","Vamos a checkear la normalidad de las variables numericas"),
                         selectInput('normalidad','Elige una opcion', c("age",
                                                        "trestbps",
                                                        "chol",
                                                        "thalach",
                                                        "oldpeak")),  
                         plotOutput("plot_norm"),
                         verbatimTextOutput("norm")
                       ),
                       box(width = 5,
                           h3("Prueba de Shapiro-Wilk"),
                           h2(id="obj_text","Dado que a veces no queda muy clara la normalidad, existen una serie de contrastes de hipotesis que 
                           uno puede hacer para ver si estos datos proceden de una normal. 
                              Uno de los más populares es el contraste de Shapiro-Wilk:"),
                           selectInput('shapiro','Elige una opcion', c("age",
                                                          "trestbps",
                                                          "chol",
                                                          "thalach",
                                                          "oldpeak")),  
                           h2(id="obj_text","La hipótesis nula sostiene que la variable se trata de una normal. "),
                           verbatimTextOutput("shap"),
                           h2(id="obj_text","Dado que el p-valor es muy pequeño, rechazamos la hipótesis nula de normalidad, no podemos aceptar que esta variable es normal."),
                      )
                     ),
                     fluidRow(
                       box(width = 6,
                           h2(id="obj_text","Vamos a analizar la posible relación entre la edad y el colesterol:"),
                           h2(id="obj_text","Divido la edad en franjas de 10 años: "),
                           verbatimTextOutput("franjaEdad"),
                           h2(id="obj_text","Divido el colesterol en franjas de 100 unidades: "),
                           verbatimTextOutput("franjaChol"),
                           h2(id="obj_text","Estudiamos ahora la posible relación mediante la prueba Chi:"),
                           verbatimTextOutput("relChi"),
                           h2(id="obj_text","Tabla de contingencia:"),
                           verbatimTextOutput("tablaContin"),
                       ),
                       box(width = 6,
                           h2(id="obj_text","Estos niveles tan bajos en la muestra nos dicen que realmente no tenemos suficientes datos para poder justificar hacerlo con Chi cuadrado."),
                           verbatimTextOutput("expected"),
                           h2(id="obj_text","Dado que hay muchos valores inferiores a 5,  la idea de hacer un contraste chi cuadrado es regular. Si lo observamos gráficamente:"),
                           plotOutput("ploteando"),
                           h2(id="obj_text","Se puede ver que cuanta más edad, el nivel de colesterol es mayor")
                           )
                     )
                     ),
      
      ############################################################### 
      ## Apartado 5: Modelos ##
      ############################################################### 
      
      tabItem(tabName = "modelos",
              fluidRow(
                h1(id="titulo","Modelos")),
              fluidRow(
                box(width = 6,
                    h2(id="obj_text","En primer lugar, analizamos ahora la posible relación entre las variables que queramos. 
                       Ploteamos un diagrama de dispersión y un modelo (de regresión lineal o logística) usando la variable 1 como variable respuesta y la variable 2 como variable explicativa. 
                       Añadimos, además, la recta de regresión lineal al diagrama de dispersión."),
                    selectInput('modVar1','Variable Respuesta', c("age",
                                                                   "trestbps",
                                                                   "chol",
                                                                   "thalach",
                                                                   "oldpeak")), 
                    
                    h2(id="obj_text", "No elijas las mismas, o te dará error"),
                    
                    selectInput('modVar2','Variable Explicativa', c("trestbps",
                                                                  "chol",
                                                                  "age",
                                                                  "thalach",
                                                                  "oldpeak")), 
                    
                    h2(id="obj_text", "Vemos primero si estan relacionadas"),
                    plotOutput("relacionModelos"),
                ),
                box( width = 6,
                    h2(id="obj_text","Para confirmar esa relación hacemos un modelo: "),
                    selectInput("tipoRegresion","--Elige una opcion--",c("Regresión Lineal","Regresión Logística")),
                    verbatimTextOutput("modLM"),
                    h2(id="obj_text","Por último, mostramos la recta de dispersión: "),
                    plotOutput("rectaModelos"),
                )),
                
                fluidRow(
                  box(width = 12,
                      h4("Bondad del ajuste"),
                      h2(id="obj_text","¿Es este modelo de regresión adecuado para describir la relación entre esas variables?"),
                      h2(id="obj_text","Hemos visto que hay una serie de gráficos diagnósticos para evaluar el modelo:"),
                      box(width = 4, h4("Resudios:"),
                      plotOutput("modelo1")),
                      box(width = 4, h4("Normal Q-Q:"),
                      plotOutput("modelo2")),
                      box(width = 4, h4("Scale Location:"),
                      plotOutput("modelo3"),
                      h2(id="obj_text","A la vista de los resultados, este modelo no es muy adecuado para describir la relación entre las variables"),
                      ),
                ))
              ),
      
      ############################################################### 
      ## Apartado 7: Show Code ##
      ############################################################### 
      
      tabItem(tabName = "code",
              fluidRow(
                h1(id="titulo","Show code"),
                box(width = 12,
                uiOutput("source")))),
      
      ############################################################### 
      ## Apartado 6: Confianza ##
      ############################################################### 
      
             tabItem(tabName = "confianza",
                     fluidRow(
                       h1(id="titulo","Confianza")),
                     fluidRow(
                       box(width = 6,
                         h2(id="obj_text","Vamos a ver los intervalos de confianza para el modelo de regresión lineal"),
                         h2(id="obj_text", "Elegimos de nuevo las variables de nuestro modelo"),
                         selectInput('confVar1','Variable Respuesta', c("age",
                                                                       "trestbps",
                                                                       "chol",
                                                                       "thalach",
                                                                       "oldpeak")), 
                         
                         h2(id="obj_text", "No elijas las mismas, o te dará error"),
                         
                         selectInput('confVar2','Variable Explicativa', c("trestbps",
                                                                         "chol",
                                                                         "age",
                                                                         "thalach",
                                                                         "oldpeak")), 
                       ),
                       box(width = 6,
                         h2(id="obj_text","Veamos primero información acerca de nuestro modelo: "),
                         verbatimTextOutput("sumModelo"),
                         selectInput('optModelo','¿Qué quieres saber del modelo?',c("Estimación de la varianza resudial",
                                                                                    "Intervalos de confianza de los coeficientes del modelo",
                                                                                    "Medidas de influencia")),
                         verbatimTextOutput("salidaSumModelo"),          
                     )
             ),
             )
    
    )
  )
)
)
)






