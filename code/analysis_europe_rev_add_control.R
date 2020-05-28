library("tidyverse")
library("readxl")
library("haven")
library("brms")
library("rcartocolor")

set.seed(100)

data2 <- read_excel("./data/src/DB_For Revisions.xlsx", sheet = "DB_Final")

d <- data2 %>% 
  #select(n_arrivals, dist, nuts2_code_1, nuts2_code_2, pop_1, pop_2, n_monuments_1, n_monuments_2) %>%
  drop_na(Arrivals) %>%
  filter(d_nuts_level != "NUTS0")
#mutate(dist = if_else(dist <= 1, 70, dist))
d <- data.frame(d)
d$Arrivals <- round(d$Arrivals)
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
d$cooling <- d$Cooling.degree.days/100
d$coastal <- d$Coastal.Region
d$metro <- d$Metropolitan.Areas

d <- d %>% select(Arrivals, logdist, logpop1, logpop2, loggdp_pc_o, loggdp_pc_d, logarea,
                  logmonuments, loglandscapes, logmuseums, origin_code, destination_code, logch,
                  logmuseums_pc, loglandscapes_pc, logmonuments_pc, WHS_total, WHS_cultural, WHS_natural, WHS_mixed,
                  cooling, coastal, metro)
d <- drop_na(d)
d <- d %>% filter(logdist > 0)

d$obs <- 1:nrow(d)

m1_add <- brm(Arrivals ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
            cooling + coastal + metro + 
            logmonuments + loglandscapes + logmuseums +
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)
          ),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3,
          control = list(adapt_delta = 0.95))

m2_add <- brm(Arrivals  ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
            cooling + coastal + metro + 
            mo(WHS_cultural) +
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3,
          control = list(adapt_delta = 0.95))


m3_add <- brm(Arrivals ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
            cooling + coastal + metro + 
            mo(WHS_cultural) +
            logdist:mo(WHS_cultural) + 
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3,
          control = list(adapt_delta = 0.95))


save(m1_add, file = "./output/m1_rev_add.rda")
save(m2_add, file = "./output/m2_rev_add.rda")
save(m3_add, file = "./output/m3_rev_add.rda")