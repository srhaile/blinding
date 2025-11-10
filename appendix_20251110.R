library(tidyverse)
library(broom)

options(scipen = 10)

fn <- function(cellcounts = c(50, 50, 0,
                              50, 50, 0)){
  tmp <- crossing(trt = c("A", "B"),
           guess = c("A", "B", "DK")) %>%
    mutate(n = cellcounts) %>%
    mutate(w = case_when(guess == "DK" ~ 0,
                         trt == guess ~ 1,
                         trt != guess ~ -1)) %>%
    uncount(n) 

    mod <- lm(w ~ 1, data = tmp) %>%
      tidy(., conf.int = TRUE) %>%
      mutate(trt = "overall")
    modA <- lm(w ~ 1, data = tmp %>% filter(trt == "A")) %>%
      tidy(., conf.int = TRUE) %>%
      mutate(trt = "A")
    modB <- lm(w ~ 1, data = tmp %>% filter(trt == "B")) %>%
      tidy(., conf.int = TRUE) %>%
      mutate(trt = "B")
    bind_rows(mod, modA, modB) %>%
      select(trt, estimate, conf.low, conf.high)
}

# row 1
fn(c(50, 50, 0,
     50, 50, 0))
fn(c(60, 40, 0,
     50, 50, 0))
fn(c(70, 30, 0,
     50, 50, 0))
fn(c(80, 20, 0,
     50, 50, 0))
fn(c(90, 10, 0,
     50, 50, 0))
fn(c(100, 0, 0,
     50, 50, 0))

# row 3
a <- 45; b <- 100 - 2 * a;
print(c(a, b))
fn(c(a, a, b,
     a, a, b))

a <- 40; b <- 100 - 2 * a;
print(c(a, b))
fn(c(a, a, b,
     a, a, b))

a <- 35; b <- 100 - 2 * a;
print(c(a, b))
fn(c(a, a, b,
     a, a, b))

a <- 30; b <- 100 - 2 * a;
print(c(a, b))
fn(c(a, a, b,
     a, a, b))

a <- 25; b <- 100 - 2 * a;
print(c(a, b))
fn(c(a, a, b,
     a, a, b))

