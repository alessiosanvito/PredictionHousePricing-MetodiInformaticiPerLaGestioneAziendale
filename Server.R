server <- function(input, output) {
  
  output$istogramma <- renderPlot({
    if(input$hist_selected == "price"){
      hist(case.df$price, main = "Istogramma dei prezzi", xlab= "Prezzo",
           ylab = "Numero di case con quel prezzo", breaks = 50, ylim = c(0, 200))
      price_q <- quantile(case.df$price)
      abline(v=price_q[1], col="red", lwd=2) # 0% (min)
      abline(v=price_q[2], col="blue", lwd=2) # 1st quartile 25%
      abline(v=price_q[3], col="green", lwd=2.5) # Median value distribution 50%
      abline(v=price_q[4], col="blue", lwd=2) # 3rd quartile 75%
      abline(v=price_q[5], col="red", lwd=2) # 100% (max)
      price.mean <- mean(case.df$price)
      abline(v = price.mean, col = "orange", lwd = 2)
    }
    if(input$hist_selected == "bedrooms"){
      hist(case.df$bedrooms, main = "Istogramma delle camere da letto",
           xlab= "Numero di camere", ylab = "Frequenza", breaks = 7)
      axis(side=1, at=seq(0,8, by=1))
      bed_q <- quantile(case.df$bedrooms)
      abline(v=bed_q[1], col="red", lwd=2) # 0% (min)
      abline(v=bed_q[2], col="blue", lwd=2) # 1st quartile 25%, coincide con 2nd 
      abline(v=bed_q[3], col="green", lwd=2.5) # Median value distribution 50%
      abline(v=bed_q[4], col="blue", lwd=2) # 3rd quartile 75%
      abline(v=bed_q[5], col="red", lwd=2) # 100% (max)
      bed.mean <- mean(case.df$bedrooms)
      abline(v = bed.mean, col = "orange", lwd = 2)
    }
    if(input$hist_selected=="bathrooms"){
      hist(case.df$bathrooms, main = "Istogramma dei bagni", xlab= "Numero di bagni",
           ylab = "Frequenza", breaks = 7)
      bath_q <- quantile(case.df$bathrooms)
      abline(v=bath_q[1], col="red", lwd=2) # 0% (min)
      abline(v=bath_q[2], col="blue", lwd=2) # 1st quartile 25%
      abline(v=bath_q[3], col="green", lwd=2.5) # Median value distribution 50%
      abline(v=bath_q[4], col="blue", lwd=2) # 3rd quartile 75%
      abline(v=bath_q[5], col="red", lwd=2) # 100% (max)
      bath.mean <- mean(case.df$bathrooms)
      abline(v = bath.mean, col = "orange", lwd = 2)
    }
    if(input$hist_selected=="sqft_living"){
      hist(case.df$sqft_living, main = "Istogramma superficie abitabile", 
           xlab= "Numero di piedi quadrati", ylab = "Frequenza", breaks = 50)
      liv_q <- quantile(case.df$sqft_living)
      abline(v=liv_q[1], col="red", lwd=2) # 0% (min)
      abline(v=liv_q[2], col="blue", lwd=2) # 1st quartile 25%
      abline(v=liv_q[3], col="green", lwd=2.5) # Median value distribution 50%
      abline(v=liv_q[4], col="blue", lwd=2) # 3rd quartile 75%
      abline(v=liv_q[5], col="red", lwd=2) # 100% (max)
      liv.mean <- mean(case.df$sqft_living)
      abline(v = liv.mean, col = "orange", lwd = 2)
    }
    if(input$hist_selected=="sqft_lot"){
      hist(case.df$sqft_lot, main = "Istogramma superficie lotto", 
           xlab= "Numero di piedi quadrati", ylab = "Frequenza", breaks = 500)
      hist(case.df$sqft_lot, main = "Istogramma superficie lotto", 
           xlab= "Numero di piedi quadrati", ylab = "Frequenza", breaks = 1000, 
           xlim = c(0,25000))
      lot_q <- quantile(case.df$sqft_lot)
      abline(v=lot_q[1], col="red", lwd=2) # 0% (min)
      abline(v=lot_q[2], col="blue", lwd=2) # 1st quartile 25%
      abline(v=lot_q[3], col="green", lwd=2.5) # Median value distribution 50%
      abline(v=lot_q[4], col="blue", lwd=2) # 3rd quartile 75%
      abline(v=lot_q[5], col="red", lwd=2) # 100% (max)
      lot.mean <- mean(case.df$sqft_lot)
      abline(v = lot.mean, col = "orange", lwd = 2)
    }
    if(input$hist_selected=="waterfront"){
      hist(case.df$waterfront, main = "Istogramma case posizionate vicino al mare", 
           xlab= "Vicinanza al mare", ylab = "Frequenza")
    }
    if(input$hist_selected=="condition"){
      hist(case.df$condition, main = "Istogramma condizione delle case", 
           xlab= "Condizione (da 1 a 5)", ylab = "Frequenza", ylim = c(0, 3000))
      cond_q <- quantile(case.df$condition)
      abline(v=cond_q[1], col="red", lwd=2) # 0% (min)
      abline(v=cond_q[2], col="blue", lwd=2) # 1st quartile 25%, coincide con 2nd 
      abline(v=cond_q[3], col="green", lwd=2.5) # Median value distribution 50%
      abline(v=cond_q[4], col="blue", lwd=2) # 3rd quartile 75%
      abline(v=cond_q[5], col="red", lwd=2) # 100% (max)
      cond.mean <- mean(case.df$condition)
      abline(v = cond.mean, col = "orange", lwd = 2)
    }
    if(input$hist_selected=="sqft_basement"){
      hist(case.df$sqft_basement, main = "Istogramma dimensione piano interrato", 
           xlab= "Dimensione piano interrato", ylab = "Frequenza", breaks = 50, 
           ylim = c(0,2900))
    }
    if(input$hist_selected=="yr_built"){
      hist(case.df$yr_built, main = "Istogramma anno di costruzione", 
           xlab= "Anno di costruzione", ylab = "Frequenza", breaks = 50)
    }
    if(input$hist_selected=="yr_renovated"){
      hist(case.df$yr_renovated, main = "Istogramma anno di ristrutturazione", 
           xlab= "Anno di ristrutturazione", ylab = "Frequenza", breaks = 50, 
           ylim = c(0,2900))
    }
  })
  
  output$box_plot <- renderPlot({
    if(input$box_selected == "price"){
      boxplot(case.df$price, xlab= "Prezzo", horizontal = T, 
              main ="Distribuzione dei prezzi")
    }
    if(input$box_selected == "bedrooms"){
      boxplot(case.df$bedrooms, xlab= "Numero di camere da letto", horizontal = T,
              main ="Distribuzione del numero di camere da letto")
    }
    if(input$box_selected=="bathrooms"){
      boxplot(case.df$bathrooms, xlab= "Numero di bagni", horizontal = T,
              main ="Distribuzione del numero di bagni")
    }
    if(input$box_selected=="sqft_living"){
      boxplot(case.df$sqft_living, xlab= "Superficie abitabile", horizontal = T,
              main ="Distribuzione della superficie abitabile")
    }
    if(input$box_selected=="sqft_lot"){
      par(mfrow = c(2,1))
      boxplot(case.df$sqft_lot, xlab= "Superficie lotto", horizontal = T,
              main ="Distribuzione della superficie del lotto", outline = TRUE)
      boxplot(case.df$sqft_lot, xlab= "Superficie lotto", horizontal = T,
              main ="Distribuzione della superficie del lotto", outline = FALSE)
      }
    if(input$box_selected=="condition"){
      boxplot(case.df$condition, xlab= "Condizione della casa", horizontal = T,
              main ="Distribuzione delle condizioni")
    }
  })
  
  output$qq_plot <- renderPlot({
    if(input$qq_selected == "price"){
      qqnorm(case.df$price, main = "qqplot dei prezzi")
      qqline(case.df$price)
    }
    if(input$qq_selected == "bedrooms"){
      qqnorm(case.df$bedrooms, main = "qqplot numero di camere da letto")
      qqline(case.df$bedrooms)
    }
    if(input$qq_selected=="bathrooms"){
      qqnorm(case.df$bathrooms, main = "qqplot numero di bagni")
      qqline(case.df$bathrooms)
    }
    if(input$qq_selected=="sqft_living"){
      qqnorm(case.df$sqft_living, main = "qqplot superficie abitabile")
      qqline(case.df$sqft_living)
    }
    if(input$qq_selected=="sqft_lot"){
      par(mfrow = c(2,1))
      qqnorm(case.df$sqft_lot, main = "qqplot dimensione superficie lotto")
      qqline(case.df$sqft_lot)
      qqnorm(case.df$sqft_lot, main = "qqplot dimensione superficie lotto", ylim = c(0,25000))
      qqline(case.df$sqft_lot)
    }
    if(input$qq_selected=="condition"){
      qqnorm(case.df$condition, main = "qqplot condizione della casa")
      qqline(case.df$condition)
    }
  })
  
  observeEvent(input$corr_selected,{
  output$correlazioni <- renderPlot({
    if(input$corr_selected == "corr_mixed"){
    corrplot.mixed(corr=cor(case.df[,c(1:10)],
                            use="complete.obs"),upper="circle", tl.pos="lt",
                   lower.col = colorpanel(50, "red", "grey", "blue"),
                   upper.col = colorpanel(50, "red", "grey", "blue"),
                   tl.col = "black")
    }
    if(input$corr_selected == "corr_superficie"){
      corrplot(cor(case.df[,c(4,5,8)]), method="pie", type = "lower", tl.col="black",
               tl.srt=45, addCoef.col = "black", diag = T, number.cex = 1.2)
    }
    if(input$corr_selected == "heatmap"){
      correlation_matrix <- cor(case.df)
      heatmap(correlation_matrix,  main = "Heatmap della matrice di correlazione", 
              symm = F)
    }
    if(input$corr_selected == "corr_citta"){
      corrplot(cor(case.df[,c(1,11,12)]), method="color", type = "lower", tl.col="black",
               tl.srt=45, addCoef.col = "black", diag = T, number.cex = 1.2)
    }
    if(input$corr_selected == "corr_PCA"){
      PCA.case<-PCA(case.df[,c(1:12)],quanti.sup = 1,graph = FALSE)
      plot(PCA.case,choix='varcor')
    }
  })
})
  
  output$predict_price <- renderText({
      variabili <- data.frame(
        bedrooms = input$bedrooms,
        bathrooms = input$bathrooms,
        sqft_living = input$sqft_living,
        sqft_lot = input$sqft_lot,
        waterfront = input$waterfront,
        condition  = input$condition,
        sqft_basement = input$sqft_basement,
        yr_built = input$yr_built,
        yr_renovated = input$yr_renovated,
        city = input$city,
        statezip = input$statezip,
        row.names = NULL
      )
      paste('Prezzo predetto:', floor(predict(fit_m5p, variabili)), '$')
    })
}
