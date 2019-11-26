library(purrr) # 0.3.3


dat = structure(list(id = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 3L), .Label = c("a", "b", "c"), class = "factor"), 
    var1 = c(4, 2.7, 3.4, 2.7, 4.6, 2.9, 2.2, 4.5, 4.6, 2.4, 
    3, 3.8, 2.5, 4, 3.6, 2.7, 4.5, 4.1, 4.2, 2.2, 4.9, 4.4, 3.6, 
    3.3, 2.7, 3.9, 4.9, 4.9, 4.3, 3.4), var2 = c(6, 22.3, 19.4, 
    22.8, 18.6, 14.2, 10.9, 22.7, 22.4, 11.7, 6, 13.3, 12.5, 
    6.3, 13.6, 20.5, 23.6, 10.9, 8.9, 20.9, 23.7, 15.9, 22.1, 
    11.6, 22, 17.7, 21, 20.8, 16.7, 21.4)), class = "data.frame", row.names = c(NA, 
-30L))

head(dat)


dat_list = split(dat, dat$id)
dat_list


map(dat_list, ~lm(var1 ~ var2, data = .x) )


r2 = function(data) {
     fit = lm(var1 ~ var2, data = data)
     
     broom::glance(fit)
}


map_dfr(dat_list, r2, .id = "id")


mtcars_cylam = split(mtcars, list(mtcars$cyl, mtcars$am) )
mtcars_cylam[1:2]

