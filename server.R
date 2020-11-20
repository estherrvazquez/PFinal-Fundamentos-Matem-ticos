######################################################################
## SHINY_SERVER ##
######################################################################

library(dplyr)
library(tidyverse)
library(kableExtra)

library(ggplot2)
library(rsample)
library(recipes)
library(parsnip)
library(yardstick)
library(viridisLite)
library(GGally)

shinyServer(function(input, output) {
  
  ############################################################### 
  ## Apartado 1: Dataset Overview ##
  ############################################################### 
  
  output$result <- renderText({
    if(input$analisis == "Número de observaciones"){
      paste("Este dataset contiene 1025 observaciones, lo que supone un conjunto de datos suficientemente grande para hacer un
            estudio interesante.")
    }else if(input$analisis == "Número de variables"){
      paste("Por otro lado, contiene 14 variables. Si quieres ver de qué tipo, elige la opción 'Tipo de variables'. ")
    }else if(input$analisis == "¿Hay datos ausentes?"){
      paste("Dado que el resultado es TRUE, no hay datos ausentes")
    }else if(input$analisis == "Overview"){
      paste("Este conjunto de datos recogido de Cleveland Clinic Heart Disease contiene 14 variables
            relacionadas con el diagnóstico del paciente y una variable resultado que indica la presencia o 
            ausencia de la enfermedad cardíaca. La función summary() devuelve resultados muy interesantes de cada variable (cuartiles y media), interesante sobre todo para variables numéricas,
            mientras que la función glimpse() nos da una idea de qué tipo de variables son y al final muestra todas las columnas y algunos ejemplos del dataset:")
    }else if(input$analisis == "Tipo de variables"){
      paste("Aunque en overview también se podía ver de qué tipo son las variables, mediante la función str(), observamos
            mejor cómo son estas variables. Se puede ver que hay variables de dos tipos: categóricas (factores) y numéricas.")
    }
    else if(input$analisis == "¿Cuántas personas son positivas en enfermedad cardíaca?"){
      paste("Resulta interesante analizar primeramente el recuento toal de resultados positivos de enfermedad cardíaca. 
            El recuento de resultados positivos de enfermedad cardíaca es mayor que el número de resultados negativos.
            Hay un total de 27 pacientes más con la enfermedad:")
    }
  })
  output$summaryDset <- renderPrint({
    if(input$analisis == "Número de observaciones"){
      x <- as.numeric(dim(ds)[1]) 
      print(x)
    }else if(input$analisis == "Número de variables"){
      x <- dim(ds)[2]
      print(x)
    }else if(input$analisis == "¿Hay datos ausentes?"){
      x <- all(complete.cases(ds))
      if(x == "TRUE"){
        print("No hay datos ausentes")
      }
     
    }else if(input$analisis == "Overview"){
      print("summary(ds)")
      print(summary(df))

      print("glimpse(ds)")
      print(glimpse(df))
    }else if(input$analisis == "Tipo de variables"){
      print(str(df))
    }
  })
    output$table <- function() {
      df$num = factor(df$num, labels = c("No Presente", "Presente"))
      req(input$analisis == "¿Cuántas personas son positivas en enfermedad cardíaca?")
      df %>%
        group_by(num) %>% 
        count() %>%
        ungroup() %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
    }
    
    output$visual <- renderPlot({
      ggplotly(chart, tooltip = c("x", "y"))
    })
    

  ############################################################### 
  ## Apartado 2: Análisis Exploratorio ##
  ############################################################### 

    output$defVar <- renderPrint({
      
    xlab <- input$x
      
    if(xlab == "age"){ paste("Variable numérica -> Rango de 29 a 76 años")
    }else if(xlab == "sex"){ paste("Variable Categórica -> 0: Mujer, 1: Hombre")
    }else if(xlab == "cp"){ paste("Variable Categórica -> 0: Angina típica, 1: Angina atípica, 2: Dolor sin angina, 3: Angina asintomática")
    }else if(xlab == "trestbps"){ paste("Variable numérica -> Rango de 94 a 180 mm Hg")
    }else if(xlab == "chol"){ paste("Variable numérica -> Rango de 126 a 564 mg / dl")
    }else if(xlab == "fbs"){ paste("Variable Categórica -> Nivel de azúcar en sangre en ayunas > 120 mg/dl, siendo: 1: True, 0: False")
    }else if(xlab == "restecg"){ paste("Variable Categórica -> 0: Normal, 1: Anormalidad a la onda ST-T, 2: Hipertrofilia en el ventrículo izquierdo")
    }else if(xlab == "thalach"){ paste("Variable numérica ->  Rango de 71 a 202")
    }else if(xlab == "exang"){ paste("Variable Categórica -> 0: No, 1: Si")
    }else if(xlab == "oldpeak"){ paste("Variable numérica -> Rango de 0 a 4.2")
    }else if(xlab == "slope"){ paste("Variable Categórica -> 1 = pendiente ascendente, siendo 0: Cuesta arriba, 1: Plano, 2: Cuesta abajo")
    }else if(xlab == "ca"){ paste("Variable numérica -> Número de vasos visibles debajo del fluoro")
    }else if(xlab == "thal"){ paste("Variable Categórica ->  1: Normal, 2: Defecto fijo, 3: Defecto reversible")
    }else if(xlab == "num"){ paste("Variable Categórica -> 0: No presente, 1: Presente")
    }
      
    })
    
    output$sumMedida <- renderPrint({
  
      ds_h <- ds %>% 
        filter(num == 1)
      
      xlab <- input$x
      
      if(xlab == "age"){x = ds_h$age
      }else if(xlab == "sex"){x = ds_h$sex
      }else if(xlab == "cp"){x = ds_h$cp
      }else if(xlab == "trestbps"){x = ds_h$trestbps
      }else if(xlab == "chol"){x = ds_h$chol
      }else if(xlab == "fbs"){x = ds_h$fbs
      }else if(xlab == "restecg"){x = ds_h$restecg
      }else if(xlab == "thalach"){x = ds_h$thalach
      }else if(xlab == "exang"){x = ds_h$exang
      }else if(xlab == "oldpeak"){x = ds_h$oldpeak
      }else if(xlab == "slope"){x = ds_h$slope
      }else if(xlab == "ca"){x = ds_h$ca
      }else if(xlab == "thal"){x = ds_h$thal
      }else if(xlab == "num"){x = ds_h$num
      }
      

      if(input$medida == "Media"){
        med <- mean(x)
        print(med)
      
      }else if(input$medida == "Mediana"){
        med <- median(x)
        print(med)
        
      }else if(input$medida == "Desviación Típica"){
        med <- sd(x)
        print(med)
        
      }else if(input$medida == "Varianza Muestral"){
        print(var(x))
        
      }else if(input$medida == "Valores Atípicos"){
        bxp_x = boxplot(x)
        atipicos = bxp_x$out
        if(is_empty(atipicos)){
          print("No hay valores atípicos")
        }else{
          print(atipicos)
        }
        
        
      }else if(input$medida == "Posición Valores Atípicos"){
        bxp_x = boxplot(x)
        atipicos = bxp_x$out
        if(is_empty(atipicos)){
          print("No hay valores atípicos")
        }else{
          which(x %in% atipicos)
        }
        
      }else if(input$medida == "IQR"){
        print(IQR(x))
      }else if(input$medida == "Cuantiles"){
        print(quantile(x))
      }
    })
    
    
    output$sumObs <- renderPrint({
      ds_h <- ds %>% 
        filter(num == 1)
      
      xlab <- input$x
      
      if(xlab == "age"){x = ds_h$age
      }else if(xlab == "sex"){x = ds_h$sex
      }else if(xlab == "cp"){x = ds_h$cp
      }else if(xlab == "trestbps"){x = ds_h$trestbps
      }else if(xlab == "chol"){x = ds_h$chol
      }else if(xlab == "fbs"){x = ds_h$fbs
      }else if(xlab == "restecg"){x = ds_h$restecg
      }else if(xlab == "thalach"){x = ds_h$thalach
      }else if(xlab == "exang"){x = ds_h$exang
      }else if(xlab == "oldpeak"){x = ds_h$oldpeak
      }else if(xlab == "slope"){x = ds_h$slope
      }else if(xlab == "ca"){x = ds_h$ca
      }else if(xlab == "thal"){x = ds_h$thal
      }else if(xlab == "num"){x = ds_h$num
      }
      
    if(input$observaciones == "Summary"){
      print(summary(x))
    }
  })  
      
    output$plotsD <- renderText({
      ds_h <- ds %>% 
        filter(num == 1)
      
      xlab <- input$x
      
      if(xlab == "age"){
        x = ds_h$age
        if(input$grafico == "Gráfico de densidad"){
          print("En este gráfico de densidad se puede ver que la distribución es bimodal. Además, su forma no se asemeja para nada a una normal, por lo que podríamos rechazar de primeras su semejanza a una distribución normal")
        }else if(input$grafico == "Histrograma"){
          print("El gráfico del histograma es parecido al gráfico de densidad, se ven claramente los picos de la bimodal (al rededor de los 42 años y los 52), así como el rango (de 29 a 76 años)")
        }else if(input$grafico == "Boxplot"){
          print("La media y la mediana de edad están en los 52 años. No se observan valores atípicos")
        }
      }else if(xlab == "sex"){
        x = ds_h$sex
        if(input$grafico == "Gráfico de densidad"){
          print("Dado que es una variable categórica, los gráficos de densidad no tienen mucho sentido. Se puede observar que hay más hombres (sex=1) que de mujeres con presencia de enfermedad cardíaca. ")
        }else if(input$grafico == "Histrograma"){
          print("Lo mismo podríamos decir del histograma. Se observan más valores de hombres que de mujeres.")
        }else if(input$grafico == "Boxplot"){
          print("No tiene sentido un boxplot para variables categóricas. Podemos observar que la mediana está en el 1 (hombres).")
        }
      }else if(xlab == "cp"){
        x = ds_h$cp
        if(input$grafico == "Gráfico de densidad"){
          print("Dado que es una variable categórica, los gráficos de densidad no tienen mucho sentido. La conclusión que se puede sacar es que hay más personas que sufren la enfermedad cardíaca sin angina de pecho.")
        }else if(input$grafico == "Histrograma"){
          print("Sacamos la misma conclusión con el histograma.")
        }else if(input$grafico == "Boxplot"){
          print("Como era de esperar, la mediana del boxplot es 2, que significa que es más normal pasar la enfermedad cardíaca sin angina de pecho.")
        }
      }else if(xlab == "trestbps"){
        x = ds_h$trestbps
        if(input$grafico == "Gráfico de densidad"){
          print("Se trata de una variable continua, por lo que el gráfico de densidad nos ofrece mucha información. En este caso tampoco podríamos aproximarla a una normal ya que se distinguen varios picos y se pueden observar los valores atípicos en la cola de la derecha")
        }else if(input$grafico == "Histrograma"){
          print("El histograma nos ofrece la misma información que el gráfico de densidad, y se observan mucho mejor los valores atípicos en la cola de la derecha.")
        }else if(input$grafico == "Boxplot"){
          print("Se observa que la mediana está en 130, así como sus valores atípicos (172, 178 y 180)")
        }
      }else if(xlab == "chol"){
        x = ds_h$chol
        if(input$grafico == "Gráfico de densidad"){
          print("No podemos decir que la variable se asemeje a una normal. Además, vemos que tiene muchos valores atípicos en la cola de la derecha")
        }else if(input$grafico == "Histrograma"){
          print("Podemos observar lo mismo que en el gráfico de densidad. Pero llama mucho más la atención el valor atípico de 564, ya que se encuentra muy alejado del anterior (417)")
        }else if(input$grafico == "Boxplot"){
          print("El boxplot nos da la misma información, se ven muy bien los atípicos y la mediana en 234")
        }
      }else if(xlab == "fbs"){
        x = ds_h$fbs
        if(input$grafico == "Gráfico de densidad"){
          print("Dado que es una variable categórica el gráfico de densidad nos informa de que es más probable sufrir de enfermedad cardíaca si el nivel de azúcar en sangre en ayunas es > 120")
        }else if(input$grafico == "Histrograma"){
          print("El histograma nos ofrece la misma información que el gráfico de densidad")
        }else if(input$grafico == "Boxplot"){
          print("No tiene sentido analizar esta variable con un boxplot")
        }
      }else if(xlab == "restecg"){
        x = ds_h$restecg
        if(input$grafico == "Gráfico de densidad"){
          print("Se trata de una variable categórica por lo que la información que sacamos es que es más normal sufrir enfermedad cardíaca cuando los resultados electrocardiográficos en reposo son anormales a la onda ST")
        }else if(input$grafico == "Histrograma"){
          print("El histograma nos ofrece la misma información que el gráfico de densidad")
        }else if(input$grafico == "Boxplot"){
          print("No tiene sentido analizar esta variable con un boxplot")
        }
      }else if(xlab == "thalach"){
        x = ds_h$thalach
        if(input$grafico == "Gráfico de densidad"){
          print("Para variables numéricas si que es interesante analizar el gráfico de densidad. En este caso vemos que no se asemeja a una normal y observamos los valores atípicos en la cola de la izquierda.")
        }else if(input$grafico == "Histrograma"){
          print("El histograma nos ofrece la misma información que el gráfico de densidad")
        }else if(input$grafico == "Boxplot"){
          print("Observamos la mediana en 161.5, así como todos los valores de los cuartiles y los valores atípicos en la cola de la izquierda")
        }
      }else if(xlab == "exang"){
        x = ds_h$exang
        if(input$grafico == "Gráfico de densidad"){
          print("Se trata de una variable categórica por lo que la información que concluimos es que la presencia de enfermedad cardíaca no se ve afectada por la angina inducida por el ejercicio")
        }else if(input$grafico == "Histrograma"){
          print("El histograma nos ofrece la misma información que el gráfico de densidad")
        }else if(input$grafico == "Boxplot"){
          print("No tiene sentido analizar esta variable con un boxplot")
        }
      }else if(xlab == "oldpeak"){
        x = ds_h$oldpeak
        if(input$grafico == "Gráfico de densidad"){
          print("Este gráfico de densidad tiene una forma muy rara, debido a la presencia de tantos valores atípicos. No se asemeja para nada a una distribución normal. Se observa la asimetría a la derecha")
        }else if(input$grafico == "Histrograma"){
          print("El histograma nos ofrece la misma información que el gráfico de densidad")
        }else if(input$grafico == "Boxplot"){
          print("Se observa claramente una asimetría a la derecha, y la mediana en 0.2 (la asimetría también la podemos saber porque la media es mayor que la mediana)")
        }
      }else if(xlab == "slope"){
        x = ds_h$slope
        if(input$grafico == "Gráfico de densidad"){
          print("Dado que se trata de una variable categórica la información que concluimos es que la presencia de enfermedad cardíaca se ve afectada por la pendiente del segmento ST cuesta arriba")
        }else if(input$grafico == "Histrograma"){
          print("El histograma nos ofrece la misma información que el gráfico de densidad")
        }else if(input$grafico == "Boxplot"){
          print("No tiene sentido analizar esta variable con un boxplot")
        }
      }else if(xlab == "ca"){
        x = ds_h$ca
        if(input$grafico == "Gráfico de densidad"){
          print("")
        }else if(input$grafico == "Histrograma"){
          print("")
        }else if(input$grafico == "Boxplot"){
          print("")
        }
      }else if(xlab == "thal"){
        x = ds_h$thal
        if(input$grafico == "Gráfico de densidad"){
          print("Dado que es una variable categórica la información que obtenemos de este gráfico de densidad es que la presencia de enfermedad cardíaca se ve afectada por la forma de thalasemia por defecto fijo")
        }else if(input$grafico == "Histrograma"){
          print("El histograma nos ofrece la misma información que el gráfico de densidad")
        }else if(input$grafico == "Boxplot"){
          print("No tiene sentido analizar esta variable con un boxplot")
        }
      }else if(xlab == "num"){
        x = ds_h$num
        if(input$grafico == "Gráfico de densidad"){
          print("")
        }else if(input$grafico == "Histrograma"){
          print("")
        }else if(input$grafico == "Boxplot"){
          print("")
        }
      }
      
      
      
    })
      
    output$plots <- renderPlot({
      ds_h <- ds %>% 
        filter(num == 1)
      
      xlab <- input$x
      
      if(xlab == "age"){x = ds_h$age
      }else if(xlab == "sex"){x = ds_h$sex
      }else if(xlab == "cp"){x = ds_h$cp
      }else if(xlab == "trestbps"){x = ds_h$trestbps
      }else if(xlab == "chol"){x = ds_h$chol
      }else if(xlab == "fbs"){x = ds_h$fbs
      }else if(xlab == "restecg"){x = ds_h$restecg
      }else if(xlab == "thalach"){x = ds_h$thalach
      }else if(xlab == "exang"){x = ds_h$exang
      }else if(xlab == "oldpeak"){x = ds_h$oldpeak
      }else if(xlab == "slope"){x = ds_h$slope
      }else if(xlab == "ca"){x = ds_h$ca
      }else if(xlab == "thal"){x = ds_h$thal
      }else if(xlab == "num"){x = ds_h$num
      }
      
      
      if(input$grafico == "Gráfico de densidad"){
        ggplot(ds_h) +
          geom_density(mapping = aes(x), color="#17B3BF", fill="#FAFA77", size=1.5) +
          theme_light() +
          xlab(xlab)
        
        
      }else if(input$grafico == "Histrograma"){
        ggplot(ds_h) +
          geom_histogram(mapping = aes(x), color="#17B3BF", fill="#FAFA77", bins = 20) +
          theme_light() +
          xlab(xlab)
        
      }else if(input$grafico == "Boxplot"){
        
        ggplot(ds_h,aes(y = x)) +
          geom_boxplot(color="#17B3BF", fill="#FAFA77") + 
          theme_light() +
          xlab(xlab)
        
      }
      
    })
  
    ############### CATEGORICAS ###############  
    output$fact_plots <- renderPlot({ 
      df <- df %>% 
        mutate_at(c("restecg", 
                    "fbs", 
                    "sex", 
                    "num", 
                    "exang",
                    "slope", 
                    "cp"), as_factor) 
      
      df_fact_tbl <- df  %>%
        select(sex,
               cp,
               fbs,
               restecg,
               exang,
               slope,
               thal,
               num) %>%
        mutate(sex = recode_factor(sex, '0' = "Mujer",'1' = "Hombre" ),
               cp = recode_factor(cp, '0' = "Típica",'1' = "Atípica",'2' = "Sin Angina",'3' = "Angina-Asintom"),
               fbs = recode_factor(fbs, '0' = "> 120 mg/dl", '1' = "<= 120 mg/dl"),
               restecg = recode_factor(restecg, '0' = "Normal",'1' = "ST-T anormal",'2' = "LV hipertrofia"),
               exang = recode_factor(exang, '0' = "No",'1' = "Si"),
               slope = recode_factor(slope, '0' = "Cuesta abajo",'1' = "Plano",'2' = "Cuesta arriba"),
               thal = recode_factor(thal, '1' = "Normal",'2' = "Defecto fijo",'3' = "Defecto reversible")) %>%
        gather(key = "key", value = "value", -num)
      
      df_fact_tbl %>% 
        ggplot(aes(value)) +
        geom_bar(aes(x = value, fill = num), 
                 alpha    = .6, 
                 position = "dodge", 
                 color    = "black",
                 width    = .8
        ) +
        labs(x = "",
             y = "",
             title = "Efecto de escala variables categóricas") +
        theme(
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank()) +
        facet_wrap(~ key, scales = "free", nrow = 4) +
        scale_fill_manual(
          values = c("#5A7C9C", "#17B3BF"),
          name   = "Presencia\nEnfermedad\nCardíaca",
          labels = c("No", "Si")) 
       # + stat_count(geom = "text", colour = "black", size = 5,
       #             aes(label = ..count..),position=position_stack(vjust=0.5))
      
      })
    
    ############### NUMERICAS ###############  
    output$num_plots <- renderPlot({ 

      df_cont_tbl <- df  %>%
        select(age,
               trestbps,
               chol,
               thalach,
               oldpeak,
               num) %>%
        gather(key   = "key",
               value = "value",
               -num)

      df_cont_tbl %>%
        ggplot(aes(y = value)) +
        geom_boxplot(aes(fill = num),
                     alpha  = .6,
                     fatten = .7) +
        labs(x = "",
             y = "",
             title = "Boxplots de Variables Numéricas") +
        scale_fill_manual(
          values = c("#5A7C9C", "#17B3BF"),
          name   = "Presencia\nEnfermedad\nCardíaca",
          labels = c("No", "Si")) +
        theme(
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank()) +
        facet_wrap(~ key,
                   scales = "free",
                   ncol   = 2)

    })
    
    
    ############################################################### 
    ## Apartado 3: ¿Qué es más probable? ##
    ############################################################### 
    
    output$rel_plot <- renderPlot({
      df$num = factor(df$num, labels = c("No Presente", "Presente"))
      df$sex = factor(df$sex, labels = c("Mujer","Hombre"))
      
      mosaicplot(num ~ sex, data = df, col = "#FAFA77")
  
    })

    output$textOutProb <- renderText({
      if(input$probabilidad == "¿Cuál es la probabilidad de que una persona elegida al azar corresponda a un caso positivo de enfermedad?"){
        paste("Hay una probabilidad del 51.31 %")
      }else if(input$probabilidad =="¿Cuál es la probabilidad de que un positivo sea mujer?"){
        paste("Hay una probabilidad del 42.96 %")
      }else if(input$probabilidad == "¿Y de que un hombre sea positivo?"){
        paste("La probabilidad es de un 42.08 %")
      }else if(input$probabilidad == "¿Y de que sea a la vez hombre y no posea la enfermedad?"){
        paste("Hay una probabilidad del 40.29 %")
      }else if(input$probabilidad == "¿Son independientes las variables hombre y poseer la enfermedad?"){
        paste("Para que sean independientes, el producto tiene que dar 42.96 %. Dado que no son iguales, no podemos afirmar que sean independientes. A continuación realizamos la prueba Chi para comprobarlo con mayor precisión.")
      }else if(input$probabilidad == "Si elegimos de forma independiente (con remplazamiento) 10 personas de esta muestra, ¿cuál es la probabilidad de que 4 de ellas sean positivas?"){
        paste("Para responder a esta pregunta hay que calcular primero la probabilidad de poseer la enfermedad, probabilidad_enfermo = prop.table(table(df$num))[2]. Y calcularlo mediante la función dbinom(4, size = 10, prob = probabilidad_enfermo) de la distribución Binomial. Hay una probabilidad del 19.39 %")
      }else if(input$probabilidad == "¿Y cuál es la probabilidad de que lo sean 4 o menos?"){
        paste("Similar al cálculo previo, pero ahora con la función pbinom(4, size = 10, prob = probabilidad_enfermo). La probabilidad es del 34.49 %")
      }      
    })
    
    output$outProb <- renderPrint({
      df$num = factor(df$num, labels = c("No Presente", "Presente"))
      df$sex = factor(df$sex, labels = c("Mujer","Hombre"))

      if(input$probabilidad == "Tabla de probabilidad"){
        prop.table(table(df$num,df$sex))
      }else if(input$probabilidad == "Probabilidades marginales"){
        addmargins(prop.table(table(df$num,df$sex)))
      }else if(input$probabilidad == "¿Cuál es la probabilidad de que una persona elegida al azar corresponda a un caso positivo de enfermedad?"){
        prop.table(table(df$num))
      }else if(input$probabilidad =="¿Cuál es la probabilidad de que un positivo sea mujer?"){
        positivos = df %>% 
          select(num, sex) %>% 
          filter(num == "Presente")
        
        prop.table(table(positivos$sex))
        
      }else if(input$probabilidad == "¿Y de que un hombre sea positivo?"){
        hombres = df %>% 
          select(num, sex) %>% 
          filter(sex == "Hombre")
        
        prop.table(table(hombres$num))
      }else if(input$probabilidad == "¿Y de que sea a la vez hombre y no posea la enfermedad?"){
        table = table(df$num, df$sex)
        addmargins(prop.table(table))
      }else if(input$probabilidad == "¿Son independientes las variables hombre y poseer la enfermedad?"){
        (probabilidad_enfermo = prop.table(table(df$num))[2])
        (probabilidad_hombre = prop.table(table(df$sex))[2])
        (probabilidad_enfermo*probabilidad_hombre)
      }else if(input$probabilidad == "Si elegimos de forma independiente (con remplazamiento) 10 personas de esta muestra, ¿cuál es la probabilidad de que 4 de ellas sean positivas?"){
        (probabilidad_enfermo = prop.table(table(df$num))[2])
        dbinom(4, size = 10, prob = probabilidad_enfermo)
      }else if(input$probabilidad == "¿Y cuál es la probabilidad de que lo sean 4 o menos?"){
        (probabilidad_enfermo = prop.table(table(df$num))[2])
        pbinom(4, size = 10, prob = probabilidad_enfermo)
      }
    })
    
    output$outChi <- renderPrint({
      df$num = factor(df$num, labels = c("No Presente", "Presente"))
      df$sex = factor(df$sex, labels = c("Mujer","Hombre"))
      chisq.test(df$sex, df$num)
    })
    output$pregunta1 <- renderPrint({
      enfermos = df %>% 
        filter(num == 1)
      
      enfermos %>% 
        filter (age > 45) %>% 
        summarise(mediaCol = mean(chol))
      
    })
    
    output$tablaCruzada <- renderTable({
      
      df$num = factor(df$num, labels = c("No Presente", "Presente"))
      df$sex = factor(df$sex, labels = c("Mujer","Hombre"))
      df$cp = factor(df$cp, labels = c("Angina típica","Angina atípica","Dolor sin angina","Angina asintomática"))
      df$restecg = factor(df$restecg, labels = c("Normal","Anormalidad a la onda ST-T","Hipertrofilia en el ventrículo izquierdo"))
      df$exang = factor(df$exang, labels = c("No","Si"))
      df$slope = factor(df$slope, labels = c("Cuesta arriba","Plano","Cuesta abajo"))
      df$thal = factor(df$thal, labels = c("Normal","Defecto fijo","Defecto reversible"))

      if(input$choice == "Nivel medio de colesterol"){
        x <- df$chol
      }else if(input$choice == "Edad media"){
        x <- df$ age
      }else if(input$choice == "Nivel medio de presión arterial"){
        x <- df$trestbps
      }else if(input$choice == "Nivel medio de frecuencia cardíaca máxima"){
        x <- df$thalach
      }
      
      xlab <- input$var1
      
      if(xlab == "sex"){var1 = df$sex
      }else if(xlab == "cp"){var1 = df$cp
      }else if(xlab == "restecg"){var1 = df$restecg
      }else if(xlab == "exang"){var1 = df$exang
      }else if(xlab == "slope"){var1 = df$slope
      }else if(xlab == "thal"){var1 = df$thal
      }else if(xlab == "num"){var1 = df$num
      }
      
      ylab <- input$var2
      
      if(ylab == "sex"){var2 = df$sex
      }else if(ylab == "cp"){var2 = df$cp
      }else if(ylab == "restecg"){var2 = df$restecg
      }else if(ylab == "exang"){var2 = df$exang
      }else if(ylab == "slope"){var2 = df$slope
      }else if(ylab == "thal"){var2 = df$thal
      }else if(ylab == "num"){var2 = df$num
      }
      
      tabla = aggregate(x ~ var1 + var2, FUN = mean)
      
      matrix(tabla$x, nrow = length(levels(var1)), ncol = length(levels(var2)),
             dimnames = list(levels(var1), levels(var2)))
    })
    

    ############################################################### 
    ## Apartado 4: Inferencia ##
    ############################################################### 
    
    output$plot_norm <- renderPlot({
      xlab <- input$normalidad
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }

      qqnorm(x, col = "#5A7C9C")
      qqline(x, col = 5)
      
    })
    
    output$shap <- renderPrint({
      xlab <- input$shapiro
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }
      
      shapiro.test(x)
      
    })
  
    
    franjaEdad = cut(df$age, breaks = seq(from = 20, to = 80, by = 10))
    nivelColesterol = cut(df$chol, breaks = seq(from = 120, to = 570, by = 100))
    
    output$franjaEdad <- renderPrint({
      head(franjaEdad)
    })
    
    output$franjaChol <- renderPrint({
      head(nivelColesterol)
    })
  
    output$relChi <- renderPrint({
      chisq.test(franjaEdad, nivelColesterol)
    })
    output$tablaContin <- renderPrint({
      prop.table(table(franjaEdad, nivelColesterol))
    })
    
    output$expected <- renderPrint({
      chitest = chisq.test(franjaEdad, nivelColesterol)
      chitest$expected
    })
    
    output$ploteando <- renderPlot({
      ggplot(data.frame(franjaEdad, nivelColesterol)) +
        geom_bar(aes(x=franjaEdad, fill = nivelColesterol), position = "fill")
    })
    
    
    ############################################################### 
    ## Apartado 5: Modelos ##
    ############################################################### 
    
    output$relacionModelos <- renderPlot({
      xlab <- input$modVar1
      ylab <- input$modVar2
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }
      
      if(ylab == "age"){y = ds$age
      }else if(ylab == "trestbps"){y = ds$trestbps
      }else if(ylab == "chol"){y = ds$chol
      }else if(ylab == "thalach"){y = ds$thalach
      }else if(ylab == "oldpeak"){y = ds$oldpeak
      }
      
      plot(x = x, y = y)
      
    })
    
    output$modLM <- renderPrint({
      xlab <- input$modVar1
      ylab <- input$modVar2
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }
      
      if(ylab == "age"){y = ds$age
      }else if(ylab == "trestbps"){y = ds$trestbps
      }else if(ylab == "chol"){y = ds$chol
      }else if(ylab == "thalach"){y = ds$thalach
      }else if(ylab == "oldpeak"){y = ds$oldpeak
      }
      
      if(input$tipoRegresion == "Regresión Lineal"){
        (modelo = lm(x ~ y))
      }else if(input$tipoRegresion == "Regresión Logística"){
        (modelo = glm(x ~ y))
      }
      
      
    })
    
    output$rectaModelos <- renderPlot({
      xlab <- input$modVar1
      ylab <- input$modVar2
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }
      
      if(ylab == "age"){y = ds$age
      }else if(ylab == "trestbps"){y = ds$trestbps
      }else if(ylab == "chol"){y = ds$chol
      }else if(ylab == "thalach"){y = ds$thalach
      }else if(ylab == "oldpeak"){y = ds$oldpeak
      }
      if(input$tipoRegresion == "Regresión Lineal"){
        modelo = lm(x ~ y)
        dispersion = ggplot(df) +
          geom_point(aes(x = x, y = y))
        
        dispersion +
          geom_smooth(method = "lm", aes(x = x, y = y), fill="red")
        
      }else if(input$tipoRegresion == "Regresión Logística"){
        modelo = glm(x ~ y)
        dispersion = ggplot(df) +
          geom_point(aes(x = x, y = y))
        
        dispersion +
          geom_smooth(method = "glm", aes(x = x, y = y), fill="red")
      }

    })
    
    output$modelo1 <- renderPlot({
      xlab <- input$modVar1
      ylab <- input$modVar2
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }
      
      if(ylab == "age"){y = ds$age
      }else if(ylab == "trestbps"){y = ds$trestbps
      }else if(ylab == "chol"){y = ds$chol
      }else if(ylab == "thalach"){y = ds$thalach
      }else if(ylab == "oldpeak"){y = ds$oldpeak
      }
      
      if(input$tipoRegresion == "Regresión Lineal"){
        modelo = lm(x ~ y)
        plot(modelo, which = 1)
      }else if(input$tipoRegresion == "Regresión Logística"){
        modelo = glm(x ~ y)
        plot(modelo, which = 1)
      }
      
    })
    
    output$modelo2 <- renderPlot({
      xlab <- input$modVar1
      ylab <- input$modVar2
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }
      
      if(ylab == "age"){y = ds$age
      }else if(ylab == "trestbps"){y = ds$trestbps
      }else if(ylab == "chol"){y = ds$chol
      }else if(ylab == "thalach"){y = ds$thalach
      }else if(ylab == "oldpeak"){y = ds$oldpeak
      }
      if(input$tipoRegresion == "Regresión Lineal"){
        modelo = lm(x ~ y)
        plot(modelo, which = 2)
      }else if(input$tipoRegresion == "Regresión Logística"){
        modelo = glm(x ~ y)
        plot(modelo, which = 2)
      }
      
    })
    
    output$modelo3 <- renderPlot({
      xlab <- input$modVar1
      ylab <- input$modVar2
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }
      
      if(ylab == "age"){y = ds$age
      }else if(ylab == "trestbps"){y = ds$trestbps
      }else if(ylab == "chol"){y = ds$chol
      }else if(ylab == "thalach"){y = ds$thalach
      }else if(ylab == "oldpeak"){y = ds$oldpeak
      }
      
      if(input$tipoRegresion == "Regresión Lineal"){
        modelo = lm(x ~ y)
        plot(modelo, which = 3)
      }else if(input$tipoRegresion == "Regresión Logística"){
        modelo = glm(x ~ y)
        plot(modelo, which = 3)
      }
      
    })
    
    ############################################################### 
    ## Apartado 6: Confianza ##
    ############################################################### 
    
    output$sumModelo <- renderPrint({
      xlab <- input$confVar1
      ylab <- input$confVar2
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }
      
      if(ylab == "age"){y = ds$age
      }else if(ylab == "trestbps"){y = ds$trestbps
      }else if(ylab == "chol"){y = ds$chol
      }else if(ylab == "thalach"){y = ds$thalach
      }else if(ylab == "oldpeak"){y = ds$oldpeak
      }
      modelo = lm(x ~ y)
      (sumModelo = summary(modelo))
    })
    
    output$salidaSumModelo <- renderPrint({
      xlab <- input$confVar1
      ylab <- input$confVar2
      
      if(xlab == "age"){x = ds$age
      }else if(xlab == "trestbps"){x = ds$trestbps
      }else if(xlab == "chol"){x = ds$chol
      }else if(xlab == "thalach"){x = ds$thalach
      }else if(xlab == "oldpeak"){x = ds$oldpeak
      }
      
      if(ylab == "age"){y = ds$age
      }else if(ylab == "trestbps"){y = ds$trestbps
      }else if(ylab == "chol"){y = ds$chol
      }else if(ylab == "thalach"){y = ds$thalach
      }else if(ylab == "oldpeak"){y = ds$oldpeak
      }
      modelo = lm(x ~ y)
      sumModelo = summary(modelo)
      if(input$optModelo == "Estimación de la varianza resudial"){
        sumModelo$sigma
      }else if(input$optModelo == "Intervalos de confianza de los coeficientes del modelo"){
        confint(modelo)
      }else if(input$optModelo == "Medidas de influencia"){
        head(hatvalues(modelo))
      }
    })
    
    ############################################################### 
    ## Apartado 6: Show Code ##
    ############################################################### 
    
    output$source <- renderUI({
      showfiles
    })
    
})

