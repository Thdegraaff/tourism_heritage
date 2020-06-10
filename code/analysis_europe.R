library("tidyverse")
library("readxl")
library("haven")
library("brms")
library("rcartocolor")

set.seed(100)

data <- read_excel("./data/src/DB_For Revisions.xlsx", sheet = "DB_Final")

d <- data %>% 
  drop_na(Arrivals) %>%
  filter(d_nuts_level != "NUTS0")
d <- data.frame(d)
d$Arrivals<- round(d$Arrivals)
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

d <- d %>% select(Arrivals, logdist, logpop1, logpop2, loggdp_pc_o, loggdp_pc_d, logarea,
                  logmonuments, loglandscapes, logmuseums, origin_code, destination_code, logch,
                  logmuseums_pc, loglandscapes_pc, logmonuments_pc, WHS_total, WHS_cultural, WHS_natural, WHS_mixed)
d <- drop_na(d)
d <- d %>% filter(logdist > 0)

d$obs <- 1:nrow(d)

# Just to check
m_ols <- lm(log(Arrivals + 1) ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
              logmonuments + loglandscapes + logmuseums, data = d)

m1 <- brm(Arrivals ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
            logmonuments + loglandscapes + logmuseums +
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)
          ),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3,
          control = list(adapt_delta = 0.95))

m2 <- brm(Arrivals ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
            mo(WHS_cultural) +
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3,
          control = list(adapt_delta = 0.95))

m3 <- brm(Arrivals  ~ logpop1 + logpop2 + logdist + loggdp_pc_o + loggdp_pc_d + logarea +
            mo(WHS_cultural) +
            logdist:mo(WHS_cultural) + 
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3,
          control = list(adapt_delta = 0.95))

m_fixed <- brm(Arrivals ~ 0 +  logdist + 
                 factor(origin_code) + factor(destination_code),
          prior = c(prior(normal(0, 2), class = b),
                    prior(gamma(0.01, 0.01), class = shape)),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3)

m_varying <- brm(Arrivals ~  logdist + 
            (1 | origin_code) + (1 | destination_code),
          prior = c(prior(normal(0, 2), class = Intercept),
                    prior(normal(0, 2), class = b),
                    prior(cauchy(0, 1), class = sd),
                    prior(gamma(0.01, 0.01), class = shape)),
          family = negbinomial(), data = d, iter = 5000, warmup = 2000, cores = 3, chains = 3)

save(m1, file = "./output/m1.rda")
save(m2, file = "./output/m2.rda")
save(m3, file = "./output/m3.rda")
save(m_fixed, file = "./output/m_fixed.rda")
save(m_varying, file = "./output/m_varying.rda")

load("./output/m1.rda")
load("./output/m2.rda")
load

waic_compare_s <- WAIC(m_fixed, m_varying)
model_weights(m_fixed, m_varying, weights = "waic")

waic_compare_models <- WAIC(m1, m2, m3)
model_weights(m1, m2, m3, weights = "waic")

temp <- ranef(m3)[[1]]

regional_effects <- as.data.frame(ranef(m3)[[1]])
regional_effects$nuts_2 <- row.names(regional_effects) 
write_csv(regional_effects, "./data/derived/regional_effects.csv")

mcmc_plot(m1b, pars = c("^b_", "^sd_")) +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(hjust = 0))

hist <- ggplot(data = d, aes(x = log(Arrivals) ) ) + 
  geom_histogram(color="black", fill="red", alpha = 0.5) + 
  theme_bw() + 
  labs(x         = "Tourist arrivals (in log)",
       y         = "Number",
       caption   = "Source: Eurostat & national statistical offices")
hist
pdf("./fig/Fig1.pdf", width = 6, height = 4)
hist
dev.off()

con <- conditional_effects(m2, "WHS_cultural" ) 
dfcon <- con[[1]]
marg <- ggplot(data = dfcon, aes(y = estimate__, x = effect1__)) +
  geom_point(size = 4, colour = "red", alpha = 0.5) + 
  geom_line(aes(group = 1), colour = "red", size = 2, alpha = 0.5) +
  geom_errorbar(aes(ymin=lower__, ymax=upper__), colour="black", width=.1)+
  theme_minimal() + 
  labs(#title     = "Marginal effect of distance on arrivals by the number of UNESCO World Heritage Sites", 
    #subtitle  = "All other variables are fixed at mean values", 
    x         = "Number of UNESCO World Heritage Sites",
    y         = "Tourist arrivals ",
    caption   = "Source: own calculations")
marg
pdf("./fig/Fig3.pdf", width = 6, height = 4)
marg
dev.off()

con <- conditional_effects(m3, "logdist:WHS_cultural" ) 
dfcon <- con[[1]]
inter <- ggplot(data = dfcon, aes(y = log(estimate__), x = effect1__, colour = effect2__) )  + 
  geom_smooth(se = FALSE) + 
  theme_minimal() +  
    #xlim(6, 8.5) + 
  labs(#title     = "Marginal effect of distance on arrivals by the number of UNESCO World Heritage Sites", 
    #subtitle  = "All other variables are fixed at mean values", 
    col       = "Number of\nUNESCO World\nHeritage Sites",
    x         = "Distance (in log)",
    y         = "Tourist arrivals (in log)",
    caption   = "Source: own calculations")
inter
pdf("./fig/Fig4.pdf", width = 6, height = 4)
inter
dev.off()
