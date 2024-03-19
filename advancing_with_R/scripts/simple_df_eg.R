## code by Dr Mark Andrews (github.com/mark-andrews)

library(tidyverse)

Df_1 <- tibble(x = c(1, 2, 3),
               y = c(2, 7, 1),
               z = c(0, 2, 7))

Df_2 <- tibble(y = c(5, 7),
               z = c(6, 7),
               x = c(1, 2))

Df_3 <- tibble(a = c(5, 6, 1),
               b = c('a', 'b', 'c'),
               c = c(T, T, F))

Df_a <- tibble(x = c(1, 2, 3),
               y = c('a', 'b', 'c'))

Df_b <- tibble(x = c(2, 3, 4),
               z = c('d', 'e', 'f'))

Df_4 <- tibble(x = c(1, 2, 3),
               y = c(2, 7, 1),
               z = c(0, 2, 7))

Df_5 <- tibble(a = c(1, 1, 7),
               b = c(2, 3, 7),
               c = c('a', 'b', 'c'))

Df_6 <- tibble(x = c(1, 2, 3),
               y = c(4, 5, 6),
               z = c(7, 8, 9))

Df_7 <- tibble(y = c(6, 7),
               z = c(9, 10),
               x = c(3, 4))

Df_8 <- tibble(x = c(1, 2, 2, 3, 4, 5, 5, 6, 7, 8, 8))
