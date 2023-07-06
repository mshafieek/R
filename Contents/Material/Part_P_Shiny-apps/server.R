library(shiny)
library(tidyverse)
library(MASS)
library(ISLR)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Plotting Example
    output$diaPlot <- renderPlot({

        filtered <- 
            diamonds %>%
            filter(price >= input$priceInput[1],
                   price <= input$priceInput[2],
                   carat >= input$caratInput[1],
                   carat <= input$caratInput[2],
                   cut == input$cutInput
            )
        model.lin <- lm(carat ~ price, data = filtered)
        model.sq <- lm(carat ~ price + I(price^2), data = filtered)
        model.cub <- lm(carat ~ price + I(price^2) + I(price^3), data = filtered)
        
        x_pred <- seq(min(filtered$price), max(filtered$price), length.out = 500)
        y_pred.lin <- predict(model.lin, newdata = tibble(price = x_pred))
        y_pred.sq <- predict(model.sq, newdata = tibble(price = x_pred))
        y_pred.cub <- predict(model.cub, newdata = tibble(price = x_pred))
        
        ggplot(data = filtered, 
               mapping = aes(x = price, y = carat, colour = cut)) + 
            geom_point() +
            geom_line(data = tibble(price = x_pred, carat = y_pred.lin), size = input$regInput, col = "blue") +
            geom_line(data = tibble(price = x_pred, carat = y_pred.sq), size = input$sqInput, col = "red") +
            geom_line(data = tibble(price = x_pred, carat = y_pred.cub), size = input$cubInput, col = "green") +
            scale_color_brewer(palette = input$colInput) + 
            theme_minimal()
        
        
        
    })
    
    # Model Table output
    output$modelres <- renderTable({
        filtered <- 
            diamonds %>%
            filter(price >= input$priceInput[1],
                   price <= input$priceInput[2],
                   carat >= input$caratInput[1],
                   carat <= input$caratInput[2],
                   cut == input$cutInput
            )
        model.lin <- lm(carat ~ price, data = filtered)
        model.sq <- lm(carat ~ price + I(price^2), data = filtered)
        model.cub <- lm(carat ~ price + I(price^2) + I(price^3), data = filtered)
        
        model.lin.sum <- summary(model.lin)
        model.sq.sum <- summary(model.sq)
        model.cub.sum <- summary(model.cub)
        
        tablemodelres <- matrix(c("Linear regression", "Square regression", "Cubic regression",
                                  round(model.lin.sum$r.squared, 3), round(model.sq.sum$r.squared, 3), 
                                  round(model.cub.sum$r.squared, 3), round(model.lin.sum$adj.r.squared, 3), 
                                  round(model.sq.sum$adj.r.squared, 3), round(model.cub.sum$adj.r.squared, 3),
                                  model.lin.sum$df[2], model.sq.sum$df[2], model.cub.sum$df[2]), ncol = 4)
        colnames(tablemodelres) <- c(" ", "R-squared", "Adj R-squared", "df")
        
        
        tablemodelres
        })
    
    # Model Statistical Output (Text)
    output$statout <- renderText({
        
        filtered <- 
            diamonds %>%
            filter(price >= input$priceInput[1],
                   price <= input$priceInput[2],
                   carat >= input$caratInput[1],
                   carat <= input$caratInput[2],
                   cut == input$cutInput
            )
        model.lin <- lm(carat ~ price, data = filtered)
        model.sq <- lm(carat ~ price + I(price^2), data = filtered)
        model.cub <- lm(carat ~ price + I(price^2) + I(price^3), data = filtered)
        
        model.lin.sum <- summary(model.lin)
        model.sq.sum <- summary(model.sq)
        model.cub.sum <- summary(model.cub)
        
        model.lin.sum[["name"]] <- "Linear regression"
        model.sq.sum[["name"]] <- "Square regression"
        model.cub.sum[["name"]] <- "Cubic regression"
        
        # Best fitting model, with R-squared
        if((model.lin.sum$adj.r.squared > model.sq.sum$adj.r.squared) & (model.lin.sum$adj.r.squared > model.cub.sum$adj.r.squared)){
                model.text.out <- model.lin.sum
        } else if((model.sq.sum$adj.r.squared > model.lin.sum$adj.r.squared) & (model.sq.sum$adj.r.squared > model.cub.sum$adj.r.squared)){
                model.text.out <- model.sq.sum
        } else if ((model.cub.sum$adj.r.squared > model.lin.sum$adj.r.squared ) & (model.cub.sum$adj.r.squared > model.sq.sum$adj.r.squared)){
                model.text.out <- model.cub.sum
        }
        
        paste0("The regression analysis which most accounts for the relationship is: ", model.text.out$name, ". ", 
               "With an Adjusted R-squared of: ", round(model.text.out$adj.r.squared, 3), ", F(",
               model.text.out$fstatistic[2], ", ", model.text.out$fstatistic[3], ") = ", round(model.text.out$fstatistic[1],3), ".")
    })

        

})
