library(tidyverse)
library(magick)
library(magrittr)

list.files(path='2020_pngs/', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_scale(1000) %>% 
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("2020_monthly_rainfall.gif") # write to current dir

list.files(path='2021_pngs/', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_scale(1000) %>% 
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("2021_monthly_rainfall.gif") # write to current dir

list.files(path='2022_pngs/', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_scale(1000) %>% 
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("2022_monthly_rainfall.gif") # write to current dir