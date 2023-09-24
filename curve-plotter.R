library(readxl)

data <- read_excel("22-09-23.xlsx")

x_ref <- c(0,10,20,30,40,50)
y_ref <- c(0)

y590ph3 <- append(y_ref, data$'590ph3')
y525ph3 <- append(y_ref, data$'525ph3')
y590ph15 <- append(y_ref, data$'590ph15')
y525ph15 <- append(y_ref, data$'525ph15')
ysubph3 <- y590ph3 - y525ph3
ysubph15 <- y590ph15 - y525ph15

# Create a list of data frames
data_list <- list(data.frame(x = x_ref, y = y590ph3),
                  data.frame(x = x_ref, y = y590ph15),
                  data.frame(x = x_ref, y = y525ph3),
                  data.frame(x = x_ref, y = y525ph15),
                  data.frame(x = x_ref, y = ysubph3),
                  data.frame(x = x_ref, y = ysubph15))


plot_titles <- c("Curva padrão 590nm pH3,0", "Curva padrão 525nm pH3,0", "Curva padrão 590nm pH1,5", "Curva padrão 525nm pH1,5", "Curva padrão 590-525nm pH3,0", "Curva padrão 590-525nm pH1,5")

par(mfrow = c(3, 2))

for (i in 1:6) {
  plot(data_list[[i]]$x, data_list[[i]]$y, pch = 16, col = "black", # Change point color to black
       xlab = "Condroitina (μg/mL)", ylab = "Absorbância (OD)", main = plot_titles[i])
  
  lm_model <- lm(data_list[[i]]$y ~ data_list[[i]]$x)
  coef_values <- coef(lm_model)
  rsquared <- summary(lm_model)$r.squared
  
  if (i %in% c(1, 2, 5, 6)) {
  text(x = max(data_list[[i]]$x)- 0.25*max(data_list[[i]]$x), y = max(data_list[[i]]$y), 
       labels = paste("R-squared =", round(rsquared, 2), "\nSlope =", round(coef_values[2], 4)),
       adj = c(0, 1), col = "black") # Change text color to black
  } else {
  text(x = min(data_list[[i]]$x), y = max(data_list[[i]]$y), 
         labels = paste("R-squared =", round(rsquared, 2), "\nSlope =", round(coef_values[2], 4)),
         adj = c(0, 1), col = "black") # Change text color to black
  }
  
  abline(lm_model, col = "red", lwd = 2)
}