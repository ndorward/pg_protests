setwd("~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data")

library(dplyr)
library(ggplot2)
library(MASS) #for nb models
library(lme4) #for multilevel models
library(htmlTable) #for tables
library(car) #for vif
library(stargazer) #for regression tables
library(sjPlot) #for marginal effects and interaction plots
library(broom) #for extracting coefficients 
library(brms)#for multivariate model 
library(cmdstanr)#interface to stan backen for brm models
library(ggcorrplot)#for correlation matrix


#read smod africa 
smod_africa = readRDS("smod_africa.rds")

#read panel data with cities > 100K pop and countries > 5 cities
smod_panel = readRDS("smod_panel.rds") %>%
  dplyr::select(-wp_city_rank) %>%
  dplyr::mutate(year = as.character(year), 
                city_year = as.character(paste(ID_HDC_G0, year, sep = "-"))) %>%
  dplyr::select(city_year, ID_HDC_G0, year, everything()) %>%
  dplyr::arrange(ID_HDC_G0, year) %>%
  dplyr::rename(educationMean = Meaneducation) %>%
  dplyr::group_by(GID_0, year) %>%
  dplyr::mutate(wp_city_rank = dense_rank(desc(world_pop)), 
                log_city_rank = log(wp_city_rank)) %>%
  ungroup()

#centre within countries for level 2 (city-level) variables and grand mean for l1 (country variables)
#smod_panel = smod_panel %>% 
#  mutate_at(c("world_pop_growth", "world_pop_growth_abs", "log_world_pop", "log_world_pop_dens",   
#              "educationMean", "log_imr"),
#            funs(c(misty::center(., type = "CWC", cluster = GID_0)))) %>%
#  mutate_at(c("v2x_libdem", "v2csprtcpt", "v2x_freexp", "polity2", 
#              "democ", "autoc", "log_wb_gdp",
#              "wb_gdp_growth", "natural_increase"), 
#            funs(c(misty::center(., type = "CGM"))))

country_city = smod_panel %>%
  group_by(GID_0, ID_HDC_G0) %>%
  summarise(city_count = n()) %>%
  group_by(GID_0) %>%
  summarise(city_count = n()) %>%
  filter(city_count > 2)

smod_panel = smod_panel %>%
  filter(GID_0 %in% country_city$GID_0)

###############
# Null models #
###############

null1 = glm.nb(acled_count ~ 1, 
               data = smod_panel)

null2 = glmer.nb(acled_count ~ (1|ID_HDC_G0), 
                 data = smod_panel) 
2*(logLik(null1) - logLik(null2)) #worse than null

null3 = glmer.nb(acled_count ~ (1|ID_HDC_G0/GID_0),
                 data = smod_panel) #convergence warning
2*(logLik(null2) - logLik(null3)) #not significant

null4 = glmer.nb(acled_count ~ (1|GID_0), 
                 data = smod_panel)
2*(logLik(null2) - logLik(null4)) # better than city clustering

null5 = glmer.nb(acled_count ~ (1|GID_0) + (1|year), 
                 data = smod_panel)
2*(logLik(null4) - logLik(null5)) #worse than just country 

null6 = glmer.nb(acled_count ~ (1|GID_0/year), 
                 data = smod_panel)
2*(logLik(null5) - logLik(null6)) #better than country and year random effects.  
2*(logLik(null4) - logLik(null6)) #worse than country random effects

###################################################################
# Baseline demographic models and controlled covariates - Table 1 #
###################################################################

model1a = glmer.nb(acled_count_lead_1 ~ log_world_pop + educationMean + log_imr + election_event 
                 + log_wb_gdp + wb_gdp_growth + ACD_conflict_year +(1|GID_0/year), 
                 data = smod_panel, 
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                 nAGQ = 0)

model1b = glmer.nb(acled_count_lead_1 ~ log_world_pop + educationMean + log_imr + election_event 
                   + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0) +(1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)

model1b = glmer.nb(acled_count_lead_1 ~ world_pop_growth + educationMean + log_imr + election_event 
                   + log_wb_gdp + wb_gdp_growth + ACD_conflict_year +(1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)

model1c = glmer.nb(acled_count_lead_1 ~ log_world_pop + world_pop_growth + educationMean + log_imr 
                   + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year +(1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)

#table 1
stargazer(model1a, model1b, model1c, type = "html", single.row = TRUE)

#embelish and put on same axis. 
figure1a = plot_model(model1c, type = "pred", terms = "log_world_pop", transform = NULL)
figure1a = plot(figure1a[[1]]) + 
  ggtitle("") + 
  xlab("Population size (log)") + 
  ylab("Predicted protest events") +
  theme_bw() + 
  theme(legend.position="bottom", 
        text = element_text(family = "Times", size = 18), 
        plot.title = element_text(size = 20)) 
ggsave(figure1a, filename = "figure1a.png", dpi = 400, 
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")

figure1b = plot_model(model1c, type = "pred", terms = "world_pop_growth")
figure1b = plot(figure1b[[1]]) + 
  ggtitle("") + 
  xlab("Population growth (%)") + 
  ylab("Predicted protest events") +
  theme_bw() + 
  theme(legend.position="bottom", 
        text = element_text(family = "Times", size = 18), 
        plot.title = element_text(size = 20)) 
ggsave(figure1b, filename = "figure1b.png", dpi = 400, 
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")

#################################################################
# Baseline political models and controlled covariates - Table 2 #
#################################################################

model2a = glmer.nb(acled_count_lead_1 ~ polity2 + educationMean + log_imr + election_event 
                   + log_wb_gdp + wb_gdp_growth + ACD_conflict_year +(1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)

model2b = glmer.nb(acled_count_lead_1 ~ polity2 + I(polity2^2) + educationMean + log_imr + election_event 
                   + log_wb_gdp + wb_gdp_growth + ACD_conflict_year +(1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)

figure2 = plot_model(model2b, type = "pred", terms = "polity2", transform = NULL)
figure2 = plot(figure2[[1]]) + 
  ggtitle("") + 
  xlab("Regime type (21-point Polity scale)") + 
  ylab("Predicted protest events") +
  theme_bw() + 
  theme(legend.position="bottom", 
        text = element_text(family = "Times", size = 18), 
        plot.title = element_text(size = 20)) 
ggsave(figure2, filename = "test.png", dpi = 400, width = 7, height = 6,
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")

model2c = glmer.nb(acled_count_lead_1 ~ v2csprtcpt + educationMean + log_imr + election_event 
                   + log_wb_gdp + wb_gdp_growth + ACD_conflict_year +(1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)


civ_soc = plot_model(model2c, type = "pred", terms = "v2csprtcpt", transform = NULL)
civ_soc = plot(civ_soc[[1]]) + 
  ggtitle("") + 
  xlab("Civil society organisation") + 
  ylab("Predicted protest events") +
  theme_bw() + 
  theme(legend.position="bottom", 
        text = element_text(family = "Times", size = 18), 
        plot.title = element_text(size = 20)) 
ggsave(civ_soc, filename = "civ_soc.png", dpi = 400, 
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")


model2d = glmer.nb(acled_count_lead_1 ~ factor(capital_or_major_city) + educationMean + log_imr + election_event 
                   + log_wb_gdp + wb_gdp_growth + ACD_conflict_year +(1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)

cap_cit = plot_model(model2d, type = "pred", terms = "capital_or_major_city", transform = NULL)
cap_cit = plot(cap_cit[[1]]) + 
  ggtitle("") + 
  xlab("Significant city") + 
  ylab("Predicted protest events") +
  theme_bw() + 
  theme(legend.position="bottom", 
        text = element_text(family = "Times", size = 18), 
        plot.title = element_text(size = 20)) 
ggsave(cap_cit, filename = "cap_cit.png", dpi = 400, 
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")


#with city rank - correlated with city size - but say we ran it. 
model2e = glmer.nb(acled_count_lead_1 ~ log_city_rank + educationMean + log_imr + election_event 
                   + log_wb_gdp + wb_gdp_growth + ACD_conflict_year +(1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)

model2f = glmer.nb(acled_count_lead_1 ~ polity2 + I(polity2^2) + v2csprtcpt + capital_or_major_city 
                   + educationMean + log_imr + election_event + log_wb_gdp + wb_gdp_growth 
                   + ACD_conflict_year + (1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)

#table 2
stargazer(model2a, model2b, model2c, model2d, model2f, type = "html", single.row = TRUE)

#################################################
# Full models and interaction effects - Table 3 #
#################################################

model3a = glmer.nb(acled_count_lead_1 ~ log_world_pop + world_pop_growth + polity2 + I(polity2^2) 
                   + v2csprtcpt + capital_or_major_city + educationMean + log_imr 
                   + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                   data = smod_panel, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   nAGQ = 0)

plot_model(model3a)

#interaction 1) pop size and regime 
model3b = update(model3a , ~ . + polity2:log_world_pop)

#interaction 2) pop growth and regime 
model3c = update(model3a , ~ . + polity2:world_pop_growth)

#interaction 3) regime and civil soc 
model3d = update(model3a , ~ . + polity2:v2csprtcpt)

model3e = update(model3a , ~ . + log_world_pop:capital_or_major_city)


#table 3
stargazer(model3a, model3b, model3c, model3d, type = "html", single.row	= TRUE)

#interaction plots
pop_pol = plot_model(model3b, type = "pred", terms = c("log_world_pop", "polity2"), 
                           ci.lvl = .5, colors = "bw", legend.title = "Polity 2 Score", 
                           axis.title = c("Total Population (log)", "Predicted protest events"), 
                           title = "")
figure3a = pop_pol + theme_bw() +
  theme(legend.position="bottom", 
        text = element_text(family = "Times", size = 18), 
        plot.title = element_text(size = 20)) 
ggsave(figure3a, filename = "figure3a.png", dpi = 400, width = 7, height = 6,
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")


growth_pop = plot_model(model3c, type = "pred", terms = c("world_pop_growth", "polity2"), 
                     ci.lvl = .8, colors = "bw", legend.title = "Polity 2 Score", 
                     axis.title = c("Population growth (%)", "Predicted protest events"), 
                     title = "")
figure3b = growth_pop + theme_bw() +
  theme(legend.position="bottom", 
        text = element_text(family = "Times", size = 18), 
        plot.title = element_text(size = 20)) 
ggsave(figure3b, filename = "figure3b.png", dpi = 400, width = 7, height = 6,
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")


civ_pol = plot_model(model3d, type = "pred", terms = c("v2csprtcpt", "polity2"), 
                        ci.lvl = .8, colors = "bw", legend.title = "Polity 2 Score", 
                        axis.title = c("Civil society", "Predicted protest events"), 
                        title = "")
figure3c = civ_pol + theme_bw() +
  theme(legend.position="bottom", 
        text = element_text(family = "Times", size = 18), 
        plot.title = element_text(size = 20)) 
ggsave(figure3c, filename = "figure3c.png", dpi = 400, width = 7, height = 6,
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")



####################
# Appendix models #
###################

# country city table 
country_city_table = smod_panel %>%
  dplyr::group_by(NAME_0) %>%
  summarise(country_city_count = sum(unique(ID_HDC_G0))) %>%
  arrange(country_city_count)

htmlTable(country_city_table)


# 1) correlation matrix ---------------------------------------------------

descriptive_table = smod_panel %>%
  dplyr::ungroup() %>%
  dplyr::select(c(log_world_pop, world_pop_growth, polity2, v2csprtcpt, educationMean, log_imr, 
                  log_wb_gdp, wb_gdp_growth)) %>%
  psych::describe() %>%
  dplyr::select(c(mean, sd, median, min, max, se)) %>%
  round(digits = 3)
htmlTable(descriptive_table)

corr_matrix = smod_panel %>%
  dplyr::ungroup() %>%
  dplyr::select(c(log_world_pop, world_pop_growth, polity2, v2csprtcpt, educationMean, log_imr, 
                  log_wb_gdp, wb_gdp_growth)) %>%
  cor() %>%
  ggcorrplot(., hc.order = TRUE, type = "lower", lab = TRUE)

#VIF table 
vif_table = vif(model3a) %>% as.data.frame() %>%
  `colnames<-`("VIF") %>%
  round(3)
htmlTable(vif_table)


# 2) per-capita protest models --------------------------------------------

smod_panel = smod_panel %>%
  dplyr::group_by(ID_HDC_G0) %>%
  dplyr::mutate(event_per_capita = acled_count/world_pop,
                event_per_capita_log = log(event_per_capita),
                event_per_capita_log_lead = lead(event_per_capita_log),
                event_per_capita_lead = lead(event_per_capita)) %>%
  ungroup()


wp_pc_eventsa = glmer.nb(event_per_capita_lead ~ log_world_pop + world_pop_growth + educationMean + log_imr 
                          + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                          data = smod_panel, 
                          control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                          nAGQ = 0)

wp_pc_eventsb = glmer.nb(event_per_capita_lead ~ log_world_pop + world_pop_growth + polity2 + I(polity2^2) 
                          + v2csprtcpt + capital_or_major_city + educationMean + log_imr 
                          + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                          data = smod_panel, 
                          control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                          nAGQ = 0)

stargazer(wp_pc_eventsa, wp_pc_eventsb, type = "html", single.row	= TRUE)


# 2) lag models -----------------------------------------------------------

lag_growth = glmer.nb(acled_count ~ world_pop_growth + lag(world_pop_growth, n = 1) 
                      + lag(world_pop_growth, n = 2) + lag(world_pop_growth, n = 3)
                      + lag(world_pop_growth, n = 4) + lag(world_pop_growth, n = 5)
                      + lag(world_pop_growth, n = 6) + lag(world_pop_growth, n = 7)
                      + lag(world_pop_growth, n = 8) + lag(world_pop_growth, n = 9)
                      + lag(world_pop_growth, n = 10) 
                      + log_world_pop + polity2 + I(polity2^2) + v2csprtcpt 
                      + capital_or_major_city + educationMean + log_imr + election_event + log_wb_gdp 
                      + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                      data = smod_panel, 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      nAGQ = 0)

test = as.data.frame(fixef(lag_growth))

coeff = broom.mixed::tidy(lag_growth, conf.int = T) %>% as.data.frame() %>%
    dplyr::select(term, estimate, conf.low, conf.high) %>%
    dplyr::filter(str_detect(term, 'world_pop_growth')) %>%
    dplyr::mutate(lag_distance = c(0:-10), 
                  term = "world_pop_growth")
  
#coefficient plot 
coefficent_plot = ggplot(coeff, aes()) + 
  geom_hline(aes(yintercept=0), linetype="dashed") +
  geom_linerange(aes(x = lag_distance, ymin = conf.low, ymax = conf.high),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = lag_distance, y = estimate, ymin = conf.low, ymax = conf.high),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_x_continuous(name="Years lagged", limits=c(-10, 0.2), 
                     breaks = c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0)) +
  ylab("Estimate") +
  theme_bw() + 
  theme(legend.position="bottom", text = element_text(family="Times", size = 18)) +
  scale_color_manual(name="",
                     labels=c("Population growth"),
                     values=c("black"))

ggsave(coefficent_plot, filename = "coefficent_plot.png", dpi = 400, 
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")

# 3) absolute population growth -------------------------------------------
wp_abs_growtha = glmer.nb(acled_count_lead_1 ~ log_world_pop + world_pop_growth_abs + educationMean + log_imr 
                          + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                          data = smod_panel, 
                          control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                          nAGQ = 0)

wp_abs_growthb = glmer.nb(acled_count_lead_1 ~ log_world_pop + world_pop_growth_abs + polity2 + I(polity2^2) 
                          + v2csprtcpt + capital_or_major_city + educationMean + log_imr 
                          + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                          data = smod_panel, 
                          control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                          nAGQ = 0)

stargazer(wp_abs_growtha, wp_abs_growthb, type = "html", single.row	= TRUE)

# 4) alternative population data ------------------------------------------

#GPW - don't run ? - probably lack of within country variation ... 
smod_panel_gpw = readRDS("smod_panel_gpw.rds") %>%
  dplyr::mutate(year = as.character(year), 
                city_year = as.character(paste(ID_HDC_G0, year, sep = "-"))) %>%
  dplyr::select(city_year, ID_HDC_G0, year, everything()) %>%
  dplyr::arrange(ID_HDC_G0, year) %>%
  dplyr::rename(educationMean = Meaneducation)

gpw_modela = glmer.nb(acled_count_lead_1 ~ log_gpw_pop + gpw_pop_growth + educationMean + log_imr 
                      + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                      data = smod_panel_gpw, 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      nAGQ = 0)

gpw_modelb = glmer.nb(acled_count_lead_1 ~ log_gpw_pop + gpw_pop_growth + polity2 + I(polity2^2) 
                      + v2csprtcpt + capital_or_major_city + educationMean + log_imr 
                      + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                      data = smod_panel_gpw, 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      nAGQ = 0)

#GHS
smod_panel_ghs = readRDS("smod_panel_ghs.rds") %>%
  dplyr::mutate(year = as.character(year), 
                city_year = as.character(paste(ID_HDC_G0, year, sep = "-"))) %>%
  dplyr::select(city_year, ID_HDC_G0, year, everything()) %>%
  dplyr::arrange(ID_HDC_G0, year) %>%
  dplyr::rename(educationMean = Meaneducation)

ghs_modela = glmer.nb(acled_count_lead_1 ~ log_ghs_pop + ghs_pop_growth + educationMean + log_imr 
                      + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                      data = smod_panel_ghs, 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      nAGQ = 0)

ghs_modelb = glmer.nb(acled_count_lead_1 ~ log_ghs_pop + ghs_pop_growth + polity2 + I(polity2^2) 
                      + v2csprtcpt + capital_or_major_city + educationMean + log_imr 
                      + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                      data = smod_panel_ghs, 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      nAGQ = 0)

#LandScan
smod_panel_ls = readRDS("smod_panel_ls.rds") %>%
  dplyr::mutate(year = as.character(year), 
                city_year = as.character(paste(ID_HDC_G0, year, sep = "-"))) %>%
  dplyr::select(city_year, ID_HDC_G0, year, everything()) %>%
  dplyr::arrange(ID_HDC_G0, year) %>%
  dplyr::rename(educationMean = Meaneducation)

ls_modela = glmer.nb(acled_count_lead_1 ~ log_ls_pop + ls_pop_growth + educationMean + log_imr 
                      + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                      data = smod_panel_ls, 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      nAGQ = 0)

ls_modelb = glmer.nb(acled_count_lead_1 ~ log_ls_pop + ls_pop_growth + polity2 + I(polity2^2) 
                      + v2csprtcpt + capital_or_major_city + educationMean + log_imr 
                      + election_event + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                      data = smod_panel_ls, 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      nAGQ = 0)

#alt population data table
stargazer(gpw_modela, gpw_modelb, ghs_modela, ghs_modelb, ls_modela, ls_modelb, type = "html", 
          single.row = T)


# 5) alternative boundary data --------------------------------------------

#africapolis
africapolis = readRDS("africapolis_panel.rds") %>%
  dplyr::mutate(year = as.character(year), 
                city_year = as.character(paste(agglosID, year, sep = "-")), 
                educationMean = (male_education + female_education)/2) %>%
  dplyr::select(city_year, agglosID, year, everything()) %>%
  dplyr::arrange(agglosID, year)

africapolis_modela = glmer.nb(acled_count_lead_1 ~ log_world_pop + world_pop_growth + educationMean
                              + log_imr + election_event + log_wb_gdp + wb_gdp_growth 
                              + ACD_conflict_year + (1|GID_0/year), 
                              data = africapolis, 
                              control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                              nAGQ = 0)

africapolis_modelb = glmer.nb(acled_count_lead_1 ~ polity2 + I(polity2^2) + v2csprtcpt 
                              + capital_or_major_city + educationMean + log_imr + election_event 
                              + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                              data = africapolis, 
                              control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                              nAGQ = 0)

africapolis_modelc = glmer.nb(acled_count_lead_1 ~ log_world_pop + world_pop_growth + polity2 
                              + I(polity2^2)  + v2csprtcpt + capital_or_major_city + educationMean 
                              + log_imr + election_event + log_wb_gdp + wb_gdp_growth 
                              + ACD_conflict_year + (1|GID_0/year), 
                              data = africapolis, 
                              control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                              nAGQ = 0)

stargazer(africapolis_modela, africapolis_modelb, africapolis_modelc, type = "html", 
          single.row = T)


# 6) disagregated acled data ----------------------------------------------

#acled event types - as multivariate model 
smod_panel = smod_panel %>%
  dplyr::mutate(acled_protest_count_lead = lead(acled_protest_count), 
                acled_riot_count_lead = lead(acled_riot_count))

riots_protests = brm(mvbind(acled_protest_count_lead, acled_riot_count_lead) ~ log_world_pop 
                     + world_pop_growth + polity2 + I(polity2^2) + v2csprtcpt + capital_or_major_city 
                     + educationMean + log_imr + election_event + log_wb_gdp + wb_gdp_growth 
                     + ACD_conflict_year + (1 |p| GID_0) + (1 |q| year),
                     data = smod_panel, 
                     family = negbinomial(),
                     warmup = 2500, iter = 5000, 
                     chains = 2, cores = 8, threads = threading(4),
                     inits = "0", 
                     backend = "cmdstanr", control = list(adapt_delta = .99, max_treedepth = 20),
                     seed = 117)

tab_model(riots_protests, transform = NULL)

#table for multivariate model
mvbind_table = as.data.frame(fixef(riots_protests)) %>%
  round(digits = 2) %>%
  unite("CI 95%", c("Q2.5", "Q97.5"), sep = ", ", remove = FALSE) %>%
  dplyr::select(-c(Est.Error, "Q2.5", "Q97.5")) %>%
  dplyr::mutate("CI 95%" = str_trim(`CI 95%`, side = "both"), 
                "CI 95%" = paste0("[",format(unlist(.["CI 95%"])),"]"))
htmlTable(mvbind_table)


# 7) alternative protest data ---------------------------------------------

#scad
smod_panel = smod_panel %>%
  dplyr::mutate(scad_count_lead = lead(scad_count))

scad_modela = glmer.nb(scad_count_lead ~ log_world_pop + world_pop_growth + educationMean
                       + log_imr + election_event + log_wb_gdp + wb_gdp_growth 
                       + ACD_conflict_year + (1|GID_0/year), 
                       data = smod_panel, 
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                       nAGQ = 0)

scad_modelb = glmer.nb(scad_count_lead ~ polity2 + I(polity2^2) + v2csprtcpt 
                       + capital_or_major_city + educationMean + log_imr + election_event 
                       + log_wb_gdp + wb_gdp_growth + ACD_conflict_year + (1|GID_0/year), 
                       data = smod_panel, 
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                       nAGQ = 0)

scad_modelc = glmer.nb(scad_count_lead ~ log_world_pop + world_pop_growth + polity2 
                       + I(polity2^2)  + v2csprtcpt + capital_or_major_city + educationMean 
                       + log_imr + election_event + log_wb_gdp + wb_gdp_growth 
                       + ACD_conflict_year + (1|GID_0/year), 
                       data = smod_panel, 
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                       nAGQ = 0)

stargazer(scad_modela, scad_modelb, scad_modelc, type = "html", single.row = T)

save.image("urban_protest_panel_analysis")

# population growth histograms
wp_growth = ggplot(smod_panel, aes(world_pop_growth)) + geom_histogram() + facet_wrap(~GID_0, scales = "free")
ghs_growth = ggplot(smod_panel_ghs, aes(ghs_pop_growth)) + geom_histogram() + facet_wrap(~GID_0, scales = "free")
gpw_growth = ggplot(smod_panel_gpw, aes(gpw_pop_growth)) + geom_histogram() + facet_wrap(~GID_0, scales = "free")
ls_growth = ggplot(smod_panel_ls, aes(ls_pop_growth)) + geom_histogram() + facet_wrap(~GID_0, scales = "free")

ggsave(wp_growth, filename = "wp_growth.png", dpi = 400, 
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")
ggsave(ghs_growth, filename = "ghs_growth.png", dpi = 400, 
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")
ggsave(gpw_growth, filename = "gpw_growth.png", dpi = 400, 
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")
ggsave(ls_growth, filename = "ls_growth.png", dpi = 400, 
       path = "~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data/Figures")
