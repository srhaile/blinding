options(scipen = 8)

library(tidyverse)
library(broom)

# Hypothetical a)
crossing(trt = c("A", "B"),
         guess = c("A", "B", "DK")) %>%
  mutate(n = c(0, 0, 100,
               0, 0, 100)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)

# Hypothetical b)
crossing(trt = c("A", "B"),
         guess = c("A", "B", "DK")) %>%
  mutate(n = c(0, 100, 0,
               100, 0, 0)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)

# Hypothetical c)
crossing(trt = c("A", "B"),
         guess = c("A", "B", "DK")) %>%
  mutate(n = c(50, 50, 0,
               50, 50, 0)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)

# Hypothetical d)
crossing(trt = c("A", "B"),
         guess = c("A", "B", "DK")) %>%
  mutate(n = c(50, 0, 50,
              0, 50, 50)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)

# Hypothetical e)
crossing(trt = c("A", "B"),
         guess = c("A", "B", "DK")) %>%
  mutate(n = c(15, 0, 85,
               0, 15, 85)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)

# Hypothetical f)
tmp <- crossing(trt = c("A", "B"),
                guess = c("A", "B", "DK")) %>%
  mutate(n = c(36, 12, 19,
               18, 6, 9)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) 
with(tmp, table(trt, guess))
with(tmp, table(trt))

tmp %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
tmp %>%
  filter(trt == "A") %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
tmp %>%
  filter(trt == "B") %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)

# Hypothetical f)
tmp <- crossing(trt = c("A", "B"),
         guess = c("A", "B", "DK")) %>%
  mutate(n = c(50, 17, 0,
               25, 8, 0)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) 
with(tmp, table(trt, guess))

tmp %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
tmp %>%
  filter(trt == "A") %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
tmp %>%
  filter(trt == "B") %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)


(0.493 * 67 + -0.515 * 33)/100
(0.493 + -0.515)/2












































































































