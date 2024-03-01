# Function to convert fahrenheit to celsius
fahr_to_celsius <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  return(celsius)
}

# Function to convert celsius to fahrenheit
celsius_to_fahr <- function(celsius) {
  fahr <- celsius * 9/5 + 32
  return(fahr)
}

airtemps <- c(212, 30.3, 78, 32)

# Convert fahrenheit to celsius
celsius_temps <- numeric(length(airtemps))
for (i in 1:length(airtemps)) {
  celsius_temps[i] <- fahr_to_celsius(airtemps[i])
}
print(celsius_temps)

# Convert celsius back to Fahrenheit
fahr_temps <- numeric(length(celsius_temps))
for (j in 1:length(celsius_temps)) {
  fahr_temps[j] <- celsius_to_fahr(celsius_temps[j])
}
print(fahr_temps)

# Check conversions went smoothly
print(airtemps == fahr_temps)

convert_temps <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  kelvin <- celsius + 273.15
  return(list(fahr = fahr, celsius = celsius, kelvin = kelvin))
}

temps_df <- data.frame(convert_temps(seq(-100,100,10)))

custom_theme <- function(base_size = 9) {
  ggplot2::theme(
    text             = ggplot2::element_text(family = 'Helvetica', 
                                             color = 'black', 
                                             size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), 
                                             hjust = 0.5, 
                                             face = 'bold'),
    panel.background = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', 
                                             linewidth = 1),
    legend.position  = 'right',
    legend.key       = ggplot2::element_rect(colour = NA, 
                                             fill = NA),
    axis.ticks       = ggplot2::element_blank(),
    axis.line        = ggplot2::element_blank()
  )
}

library(ggplot2)

ggplot(temps_df, mapping = aes(x = fahr, y = celsius, color = kelvin)) +
  geom_point() +
  custom_theme(base_size = 14)


scatterplot <- function(df, point_size = 2, font_size = 9) {
  ggplot(df, mapping = aes(x = fahr, y = celsius, color = kelvin)) +
    geom_point(size = point_size) +
    custom_theme(font_size) +
    labs(x = "fahrenheit")  # Label for x-axis
}
scatterplot(temps_df, point_size = 3, font_size = 16)
print(scatterplot(temps_df, point_size = 3, font_size = 16))