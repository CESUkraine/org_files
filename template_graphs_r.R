install.packages("extrafont")
library(extrafont)
font_import()
loadfonts()
library(dplyr)
library(ggplot2)
#Кольори
#Переклад кодів в слова для зручності
ces_colors <- c(
  `green`      = "#73B932",
  `blue`       = "#00509b",
  `yellow`     = "#FAC31E",
  `dark green`= "#2DA069",
  `orange`     = "#E64B00",
  `light blue` = "#417DC8",
  `light orange`  = "#F0783C",
  `pink`= "#AF2D5A",
  `grey`="#B2B2B2",
  `light green`= "#96D25A",
  `another light blue`="#6496CD",
  `light yellow`="#F0CD41"
  
)


ces_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ces_colors)
  
  ces_colors[cols]
}

ces_palettes <- list(
  `main`  = ces_cols("green", "blue"),
  
  `three_colours`  = ces_cols( "green", "blue", "yellow"),
  
  `four_colours` = ces_cols("green", "blue", "yellow", "dark green"),
  
  `five_colours`  = ces_cols( "green","blue", "yellow", "dark green" , "orange"),
  `six_colours`= ces_cols("green","blue", "yellow", "dark green" , "orange", "light blue"),
  `seven_colours`= ces_cols("green","blue", "yellow", "dark green" , "orange", "light blue", "light orange"),
  `eight_colours`= ces_cols("green","blue", "yellow", "dark green" , "orange", "light blue", "light orange", "pink")  
)

ces_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ces_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}
#Scale_color_ces i scale_fill_ces використовувати тільки у випадках, коли у нас декілька категорій, ми кольорами позначаємо різні категорії.
#Color коли є лінія чи крапочки, fill коли колонки (тобто коли щось треба закрашувати всередині кольором)
#Якщо у нас одна категорія -  замість Scale_color_ces i scale_fill_ces пишемо всередині команди fill=ces_cols("blue") або color=ces_cols("blue") (буде уже наш синій, можна писати інші кольори, див. функцію ces_cols)
scale_color_ces <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ces_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ces_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_ces <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ces_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("ces_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#Для вертикальних графіків
theme_ces <- function (base_size = 12, base_family = "Calibri") 
{
  half_line <- base_size/2
theme(line = element_line(colour = "#184E82", size = 0.5, 
                          linetype = 1),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.border = element_blank(), 
  panel.grid.minor.y  =  element_line(colour = "grey88"), 
  panel.grid.major.y = element_line(colour = "grey88"),
  plot.background = element_rect(colour ="white" ) ,
  axis.text.x =element_text(family= base_family, size = base_size, colour = "#6d6d6d"), 
  axis.title.x =element_text(family=base_family, size = base_size, colour = "#6d6d6d"),
  axis.text.y  =element_text(family=base_family, size = base_size, colour = "#6D6D6D"), 
  axis.title.y  =element_text(family=base_family, size = base_size, colour = "#6D6D6D"),
  axis.ticks = NULL,
  axis.ticks.length=unit(0, "pt"),
  text = element_text(family = base_family,
                      colour = ces_cols("grey"), size = base_size),
  legend.text = element_text(size = base_size,family=base_family, colour = "#6D6D6D" ),
  legend.position = "bottom",
  legend.direction ="vertical",
  legend.box = "vertical",
  legend.key=element_rect(fill = "white"),
  legend.title = element_text(colour="#6D6D6D")
                      
)
}

#Для горизонтальних графіків
theme_ces2 <- function (base_size = 11, base_family = "Calibri") 
{
  half_line <- base_size/2
  theme(line = element_line(colour = "#184E82", size = 0.5, 
                            linetype = 1),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_blank(), 
        panel.grid.minor.x  =  element_line(colour = "grey88"), 
        panel.grid.major.x = element_line(colour = "grey88"),
        plot.background = element_rect(colour ="white" ) ,
        axis.text.x =element_text(family= base_family, size = base_size, colour = "#6D6D6D"), 
        axis.title.x =element_text(family=base_family, size = base_size, colour = "#6D6D6D"),
        axis.text.y  =element_text(family=base_family, size = base_size, colour = "#6D6D6D"), 
        axis.title.y  =element_text(family=base_family, size = base_size, colour = "#6D6D6D"),
        axis.ticks = NULL,
        axis.ticks.length=unit(0, "pt"),
        text = element_text(family = base_family,
                            colour = ces_cols("grey"), size = base_size),
        legend.text = element_text(size = base_size,family=base_family, colour = "#6D6D6D" ),
        legend.position = "bottom",
        legend.direction ="vertical",
        legend.box = "vertical",
        legend.key=element_rect(fill = "white"),
        legend.title = element_text()
        
  )
}


