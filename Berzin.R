# 1 -----------------------------------------------------------------------
library(foreach)
library(tidyverse)
theme_set(theme_bw())
marg1 <- function(k){
  LETTERS[1:length(k)] |> 
    sample(size = sum(k), prob = k, replace = TRUE) |> 
    unique() |> 
    length()
}
parallel::makeCluster(parallel::detectCores()-1)

# 2 -----------------------------------------------------------------------
k <- sort(c(6, 2, 1, 1, 3, 5, 5, 1, 1,  2, 22, 205, 9, 2, 1))

k.rand <- foreach(1:9999, .combine = c) %dopar%{
  marg1(k)
}

k.rand <- data.frame(s = k.rand) |> 
  dplyr::mutate(mg = (s-1)/log(sum(k))) |> 
  tibble::as_tibble()
k.M <- (length(k)-1)/log(sum(k))
k.ci <- k.rand |> 
  dplyr::pull(mg) |> 
  quantile(probs = c(0.025, 0.975))
data.frame(name = "Kosh", 
           lower = k.M - (max(k.ci) - min(k.ci)), 
           Margalef = k.M, 
           upper = k.M + (max(k.ci) - min(k.ci)))

ggplot(k.rand, aes(x = mg)) + 
  geom_density() + 
  geom_vline(xintercept = mean(k.ci), color = "red", size = 1.5) +
  geom_vline(xintercept = k.ci, color = "blue", linetype = "dashed") + 
  labs(x = "Margalef", y = "Probablity", title = "Kosh, nboot = 9999", 
       subtitle = "CI is not centred at the pic!")

# 3 -----------------------------------------------------------------------
s <- sort(c(48, 4, 2, 285, 7, 4, 3, 71, 1, 5, 69, 36, 141, 18, 93, 4, 1, 1, 10, 1, 1))

s.rand <- foreach(1:9999, .combine = c) %dopar%{
  marg1(s)
}

s.rand <- data.frame(s = s.rand) |> 
  dplyr::mutate(mg = (s-1)/log(sum(s))) |> 
  tibble::as_tibble()
s.M <- (length(s)-1)/log(sum(s))
s.ci <- s.rand |> 
  dplyr::pull(mg) |> 
  quantile(probs = c(0.025, 0.975))
data.frame(name = "Shem", 
           lower = s.M - (max(s.ci) - min(s.ci)), 
           Margalef = s.M, 
           upper = s.M + (max(s.ci) - min(s.ci)))

ggplot(s.rand, aes(x = mg)) + 
  geom_density() + 
  geom_vline(xintercept = mean(s.ci), color = "red", size = 1.5) +
  geom_vline(xintercept = s.ci, color = "blue", linetype = "dashed") + 
  labs(x = "Margalef", y = "Probablity", title = "Shem, nboot = 9999", 
       subtitle = "CI is not centred at the pic!")

