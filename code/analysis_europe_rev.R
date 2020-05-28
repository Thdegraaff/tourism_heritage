library("tidyverse")
library("readxl")
library("haven")
library("brms")
library("rcartocolor")

data2 <- read_excel("./data/src/DB_For Revisions.xlsx", sheet = "DB_Final")

d <- data2 %>% 
  #select(n_arrivals, dist, nuts2_code_1, nuts2_code_2, pop_1, pop_2, n_monuments_1, n_monuments_2) %>%
  drop_na(Arrivals) %>%
  filter(d_nuts_level != "NUTS0")
  #mutate(dist = if_else(dist <= 1, 70, dist))
d <- data.frame(d)
d$nights<- round(d$Arrivals * d$Nights.Spent.per.arrivals)
d$logdist <- log(d$DISTANCE.GIS_km + 0.001) 
d$logpop1 <- log(d$Population_Origin)
d$logpop2 <- log(d$Population_Destination)
d$loggdp_pc_o <- log(d$GDP_origin..million/d$logpop1)
d$loggdp_pc_d <- log(d$GDP_destination..million./d$logpop2)
d$logarea <- log(d$Area..square.km.)
d$logmonuments <- log(d$Monuments+1) 
d$loglandscapes <- log(d$Landscapes + 1)
d$logmuseums <- log(d$Museums+1)
d$logmuseums_pc <- log( (d$Museums ) / (d$logpop2/10000) )
d$logmonuments_pc <- log(d$Monuments/ (d$logpop2/10000) ) 
d$loglandscapes_pc <- log(d$Landscapes + 1/ (d$logpop2/10000) )
d$logch <- log(d$Museums + d$Monuments)
d$WHS_cultural <- factor(d$WHS_cultural, ordered = TRUE)

d <- d %>% select(nights, logdist, logpop1, logpop2, loggdp_pc_o, loggdp_pc_d, logarea,
                  logmonuments, loglandscapes, logmuseums, origin_code, destination_code, logch,
                  logmuseums_pc, loglandscapes_pc, logmonuments_pc, WHS_total, WHS_cultural, WHS_natural, WHS_mixed)
d <- drop_na(d)
d <- d %>% filter(logdist > 0)

d$obs <- 1:nrow(d)

m1_rev <- brm(nights ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
            logmonuments + loglandscapes + logmuseums +
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)
          ),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3,
          control = list(adapt_delta = 0.95))

m2_rev <- brm(nights  ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
            mo(WHS_cultural) +
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3,
          control = list(adapt_delta = 0.95))

m3_rev <- brm(nights  ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
            mo(WHS_cultural) +
            logdist:mo(WHS_cultural) + 
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3,
          control = list(adapt_delta = 0.95))

save(m1_rev, file = "./output/m1_rev.rda")
save(m2_rev, file = "./output/m2_rev.rda")
save(m3_rev, file = "./output/m3_rev.rda")

mcmc_plot(m1b, pars = c("^b_", "^sd_")) +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(hjust = 0))

ggplot(data = d, aes(x = log(Arrivals) ) ) + 
  geom_histogram(color="black", fill="red", alpha = 0.5) + 
  theme_bw() + 
  labs(x         = "Tourist arrivals (in log)",
       y         = "Number",
       caption   = "Source: own calculations")

con <- conditional_effects(m2, "WHS_cultural" ) 
dfcon <- con[[1]]
ggplot(data = dfcon, aes(y = estimate__, x = effect1__)) +
  geom_point(size = 4, colour = "red", alpha = 0.5) + 
  geom_line(aes(group = 1), colour = "red", size = 2, alpha = 0.5) +
  geom_errorbar(aes(ymin=lower__, ymax=upper__), colour="black", width=.1)+
  theme_minimal() + 
  labs(#title     = "Marginal effect of distance on arrivals by the number of UNESCO World Heritage Sites", 
    #subtitle  = "All other variables are fixed at mean values", 
    x         = "Number of UNESCO World Heritage Sites",
    y         = "Tourist arrivals ",
    caption   = "Source: own calculations")

con <- conditional_effects(m3, "logdist:WHS_cultural" ) 
dfcon <- con[[1]]
ggplot(data = dfcon, aes(y = log(estimate__), x = effect1__, colour = effect2__) )  + 
  geom_smooth(se = FALSE) + 
  theme_minimal() +  
  #xlim(6, 8.5) + 
  labs(#title     = "Marginal effect of distance on arrivals by the number of UNESCO World Heritage Sites", 
    #subtitle  = "All other variables are fixed at mean values", 
    col       = "Number of\nUNESCO World\nHeritage Sites",
    x         = "Distance (in log)",
    y         = "Tourist arrivals (in log)",
    caption   = "Source: own calculations")