options(scipen = 8)

library(tidyverse)
library(broom)

## real world data scenarios

# RWD a)
rwd_a <- crossing(trt = c("A", "B"),
         guess = c("A", "B", "DK")) %>%
  mutate(n = c(557, 427, 756,
               418, 573, 764)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
rwd_a

# RWD b)
rwd_b <- crossing(trt = c("A", "B"),
         guess = c("A", "B", "DK")) %>%
  mutate(n = c(15, 18, 0, 
               13, 18, 5)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)

rwd_b


# RWD c)
rwd_c1 <- crossing(trt = c("A", "B"),
         guess = c("def A", "poss A", "poss B", "def B", "DK")) %>%
  mutate(guess = factor(guess, c("def A", "poss A", "poss B", "def B", "DK"))) %>%
  arrange(trt, guess) %>%
  mutate(n = c(38, 44, 21, 4, 170,
               11, 16, 21, 8, 83)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == "A" & guess == "def A" ~ 1,
                       trt == "B" & guess == "def B" ~ 1,
                       trt == "B" & guess == "def A" ~ -1,
                       trt == "A" & guess == "def B" ~ -1,
                       trt == "A" & guess == "poss A" ~ 0.5,
                       trt == "B" & guess == "poss B" ~ 0.5,
                       trt == "B" & guess == "poss A" ~ -0.5,
                       trt == "A" & guess == "poss B" ~ -0.5)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
rwd_c1

rwd_c2 <- crossing(trt = c("A", "B"),
         guess = c("def A", "poss A", "poss B", "def B", "DK")) %>%
  mutate(guess = factor(guess, c("def A", "poss A", "poss B", "def B", "DK"))) %>%
  arrange(trt, guess) %>%
  mutate(n = c(38, 44, 21, 4, 170,
               11, 16, 21, 8, 83)) %>%
  mutate(w = case_when(guess == "DK" ~ NA,
                       trt == "A" & guess == "def A" ~ 1,
                       trt == "B" & guess == "def B" ~ 1,
                       trt == "B" & guess == "def A" ~ -1,
                       trt == "A" & guess == "def B" ~ -1,
                       trt == "A" & guess == "poss A" ~ 0.5,
                       trt == "B" & guess == "poss B" ~ 0.5,
                       trt == "B" & guess == "poss A" ~ -0.5,
                       trt == "A" & guess == "poss B" ~ -0.5)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
rwd_c2

tmp <- crossing(trt = c("A", "B"),
                   guess = c("def A", "poss A", "poss B", "def B", "DK")) %>%
  mutate(guess = factor(guess, c("def A", "poss A", "poss B", "def B", "DK"))) %>%
  arrange(trt, guess) %>%
  mutate(n = c(38, 44, 21, 4, 170,
               11, 16, 21, 8, 83)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == "A" & guess == "def A" ~ 1,
                       trt == "B" & guess == "def B" ~ 1,
                       trt == "B" & guess == "def A" ~ -1,
                       trt == "A" & guess == "def B" ~ -1,
                       trt == "A" & guess == "poss A" ~ 0.5,
                       trt == "B" & guess == "poss B" ~ 0.5,
                       trt == "B" & guess == "poss A" ~ -0.5,
                       trt == "A" & guess == "poss B" ~ -0.5)) %>%
  uncount(n) 
rwd_c3 <- tmp %>%
  group_by(trt) %>%
  nest() %>%
  mutate(out = map(data, ~ lm(w ~ 1, data = .)),
         res = map(out, ~ tidy(., conf.int = TRUE))) %>%
  select(-data, -out) %>%
  unnest(res)
rwd_c3

# RWD d)
rwd_d1 <- crossing(trt = c("A", "B"),
         guess = c("def A", "poss A", "poss B", "def B", "DK")) %>%
  mutate(guess = factor(guess, c("def A", "poss A", "poss B", "def B", "DK"))) %>%
  arrange(trt, guess) %>%
  mutate(n = c(0, 4, 3, 0, 32,
               0, 6, 3, 0, 32)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == "A" & guess == "def A" ~ 1,
                       trt == "B" & guess == "def B" ~ 1,
                       trt == "B" & guess == "def A" ~ -1,
                       trt == "A" & guess == "def B" ~ -1,
                       trt == "A" & guess == "poss A" ~ 0.5,
                       trt == "B" & guess == "poss B" ~ 0.5,
                       trt == "B" & guess == "poss A" ~ -0.5,
                       trt == "A" & guess == "poss B" ~ -0.5)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
rwd_d1

rwd_d2 <- crossing(trt = c("A", "B"),
         guess = c("def A", "poss A", "poss B", "def B", "DK")) %>%
  mutate(guess = factor(guess, c("def A", "poss A", "poss B", "def B", "DK"))) %>%
  arrange(trt, guess) %>%
  mutate(n = c(0, 4, 3, 0, 32,
               0, 6, 3, 0, 32)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == "A" & guess == "def A" ~ 1,
                       trt == "B" & guess == "def B" ~ 1,
                       trt == "B" & guess == "def A" ~ -1,
                       trt == "A" & guess == "def B" ~ -1,
                       trt == "A" & guess == "poss A" ~ 0.75,
                       trt == "B" & guess == "poss B" ~ 0.75,
                       trt == "B" & guess == "poss A" ~ -0.75,
                       trt == "A" & guess == "poss B" ~ -0.75)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
rwd_d2

# RWD e)
rwd_e <- crossing(trt = c("A", "B"),
         guess = c("A", "B")) %>%
  mutate(n = c(42, 0, 
               19, 2)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt != guess ~ -1)) %>%
  uncount(n) %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
rwd_e 

# RWD f)
tmp <- crossing(trt = c("A", "B", "C"),
         guess = c("A", "B", "C", "DK")) %>%
  mutate(n = c(41, 66, 30, 44, 
               27, 72, 24, 51, 
               22, 36, 64, 52)) %>%
  mutate(w = case_when(guess == "DK" ~ 0,
                       trt == guess ~ 1,
                       trt %in% c("A", "B") & guess == "C" ~ -1,
                       trt == "C" & guess %in% c("A", "B") ~ -1,
                       trt == "A" & guess == "B" ~ 0.5,
                       trt == "B" & guess == "A" ~ 0.5)) %>%
  uncount(n)

rwd_f1 <- tmp %>%
  lm(w ~ 1, data = .) %>%
  tidy(., conf.int = TRUE)
rwd_f1

rwd_f2 <- tmp %>%
  group_by(trt) %>%
  nest() %>%
  mutate(out = map(data, ~ lm(w ~ 1, data = .)),
         res = map(out, ~ tidy(., conf.int = TRUE))) %>%
  select(-data, -out) %>%
  unnest(res)
rwd_f2

### comparison

bind_rows(rwd_a %>% mutate(scenario = "RWD a"),
          rwd_b %>% mutate(scenario = "RWD b"),
          rwd_c1 %>% mutate(scenario = "RWD c", opt = 1),
          rwd_c2 %>% mutate(scenario = "RWD c", opt = 2),
          rwd_c3 %>% mutate(scenario = "RWD c", opt = 3),
          rwd_d1 %>% mutate(scenario = "RWD d", opt = 1),
          rwd_d2 %>% mutate(scenario = "RWD d", opt = 2),
          rwd_e %>% mutate(scenario = "RWD e"),
          rwd_f1 %>% mutate(scenario = "RWD f", opt = 1),
          rwd_f2 %>% mutate(scenario = "RWD f", opt = 2)) %>%
  mutate(opt = as.character(opt)) %>%
  replace_na(list(opt = "1", trt = "overall")) %>%
  mutate(version = paste(opt, trt, sep = "")) %>%
  ggplot(aes(scenario, estimate, 
             ymin = conf.low, ymax = conf.high,
             linetype = opt, color = trt)) + 
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  ylab("weighted blinding index") +
  scale_y_continuous(limits = c(-1, 1),
                     breaks = c(-1, 0, 1),
                     labels = c("opposite\nguessing", 
                                "blinded", 
                                "correct\nguessing"),
                     minor_breaks = seq(-1, 1, 0.2)) +
  theme_bw() + 
  theme(legend.position = "bottom")
