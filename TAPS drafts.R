library(plotly)

colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070')

fig <- plot_ly(mydata_clean[mydata_clean$Date<as.POSIXct('2024-06-12'),], x = ~Lng, y = ~Lat, z = ~Depth, color = ~`Block #` , size = ~Water, colors = colors,
               marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150))
fig <- fig %>% layout(title = 'Life Expectancy v. Per Capita GDP, 2007',
                      scene = list(xaxis = list(title = 'GDP per capita (2000 dollars)',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                range = c(2.003297660701705, 5.191505530708712),
                                                type = 'log',
                                                zerolinewidth = 1,
                                                ticklen = 5,
                                                gridwidth = 2),
                                   yaxis = list(title = 'Life Expectancy (years)',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                range = c(36.12621671352166, 91.72921793264332),
                                                zerolinewidth = 1,
                                                ticklen = 5,
                                                gridwith = 2),
                                   zaxis = list(title = 'Population',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                type = 'log',
                                                zerolinewidth = 1,
                                                ticklen = 5,
                                                gridwith = 2)),
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      plot_bgcolor = 'rgb(243, 243, 243)')

fig


fig <- plot_ly(mydata_clean[mydata_clean$Date<as.POSIXct('2024-06-12'),], x = ~Lng, y = ~Lat, z = ~Depth,
               marker = list(color = ~Water, colorscale = c('#4AC6B7','#FFE1A1'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Longitude'),
                                   yaxis = list(title = 'Latitude'),
                                   zaxis = list(title = 'Depth(inches)')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Volumetric Water Content (m3/m3)',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))
fig

library(sf)
my_sf <- read_sf('C:/Users/Weiqiang/Downloads/ksu_taps_2024_plots.geojson')
geo <- as.data.frame(cbind(my_sf$Plot_ID, matrix(rapply(my_sf$geometry, function(x) head(x, 1)), ncol = 2, byrow = T)))
colnames(geo) = c('Plot_ID', 'Lng', 'Lat')

library(ggplot2)
p <- ggplot(my_sf) +
  geom_sf(fill = "#69b3a2", color = "white" , aes(fill = TRT_ID)) +
  theme_void()
ggplotly(p)


library(readxl)
mydata <- read_excel("C:/Users/Weiqiang/Downloads/24 KSU TAPS Neutron Tube Readings_VWC.xlsx", 
                                                     sheet = "Sheet1", range = "A3:M693")
library(tidyverse)
mydata_clean <- mydata %>% pivot_longer(cols = 4:13, names_to = 'Depth', values_to = 'Water') %>% left_join(geo, by = c(`Plot #`='Plot_ID'))



temp <- split(mydata, factor(sort(rank(row.names(mydata))%%5)))
sorted_temp <- lapply(temp, function(df){
  df[order(df$`Plot #`),]
})
z <- matrix(sorted_temp$'0'$'6', ncol = 6, byrow = T)
reversed_z <- log(z[nrow(z):1, ])

z2 <- matrix(sorted_temp$'0'$'18', ncol = 6, byrow = T)
reversed_z2 <- log(z2[nrow(z2):1, ])

z3 <- matrix(sorted_temp$'0'$'30', ncol = 6, byrow = T)
reversed_z3 <- log(z3[nrow(z3):1, ])

z4 <- matrix(sorted_temp$'0'$'78', ncol = 6, byrow = T)
reversed_z4 <- log(z4[nrow(z4):1, ])

z5 <- matrix(sorted_temp$'0'$'114', ncol = 6, byrow = T)
reversed_z5 <- log(z5[nrow(z5):1, ])

fig <- plot_ly(showscale = FALSE)
fig <- fig %>% add_surface(z = ~reversed_z)
fig <- fig %>% add_surface(z = ~reversed_z2, opacity = 0.98)
fig <- fig %>% add_surface(z = ~reversed_z3, opacity = 0.88)
fig <- fig %>% add_surface(z = ~reversed_z4, opacity = 0.78)
fig <- fig %>% add_surface(z = ~reversed_z5, opacity = 0.68)

fig