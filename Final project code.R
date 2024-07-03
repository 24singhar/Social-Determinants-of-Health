library(ggplot2)
library(dplyr)
library(olsrr)
library(ggcorrplot)
library(GGally)

#Loading CSV file in
sdoh_2019<-read.csv("C:/ST 540/Final/SDOH_2019.csv")

#Quick summary of file
dim(sdoh_2019)
summary(sdoh_2019)
colnames(sdoh_2019)

#Using dplyr package to rename, filter, and select data for exploratory analysis
sdoh_2019_updated<-sdoh_2019 %>%
  rename(unemployment = AHRF_UNEMPLOYED_RATE,
         rf_3 = CRE_RATE_RISK3,
         gini = ACS_GINI_INDEX,
         below_poverty = SAIPE_PCT_POV,
         lt_hs = ACS_PCT_LT_HS,
         rec_cen = CCBP_FCRSC_RATE,
         pc_access = CCBP_PHYS_RATE,
         grocery_access = CCBP_SOGS_RATE,
         part_conc = WUSTL_AVG_PM25,
         medicaid = ACS_PCT_MEDICAID_ANY,
         private_ins = ACS_PCT_PRIVATE_ANY,
         uninsured = ACS_PCT_UNINSURED,
         cardiac_ic_per1k = AHRF_CARDIAC_IC_HOSP_RATE,
         cardio_per1k = AHRF_CARDIOVAS_SPEC_RATE,
         nurse_spec_per1k = AHRF_CLIN_NURSE_SPEC_RATE,
         pc_per1k = AHRF_GEN_INTERNAL_MED_RATE,
         prev_med_per1k = AHRF_GEN_PREV_RATE,
         hosp_per100k = AHRF_HOSPS_RATE,
         ment_short = AHRF_HPSA_MENTAL,
         spec_per1k = AHRF_MED_SPEC_RATE,
         ment_serv_per1k = AMFAR_MHFAC_RATE,
         heart_death_per100k = CDCA_HEART_DTH_RATE_ABOVE35,
         ment_prov_per100k = CHR_MENTAL_PROV_RATE,
         premature_death = CHR_PREMAT_DEATH_RATE,
         state = STATE,
         region = REGION,
         county = COUNTY,
         med_income = SAIPE_MEDIAN_HH_INCOME,
         
) %>%
  select(state,
         region,
         county,
         premature_death,
         uninsured,
         med_income,
         unemployment,
         lt_hs,
         rec_cen,
         grocery_access,
         part_conc,
         below_poverty,
         pc_per1k,
  ) %>%
filter(!is.na(region),
       region!="")

#Summary of updated data
dim(sdoh_2019_updated)
summary(sdoh_2019_updated)

#Create a new subset for the linear regression model
sdoh_sub <- sdoh_2019_updated[,c('premature_death', 
                                 'uninsured', 
                                 'med_income', 
                                 'grocery_access',
                                 'unemployment',
                                 'part_conc',
                                 'lt_hs',
                                 'rec_cen',
                                 'below_poverty',
                                 'pc_per1k'
)]

#Summary of subset of data
dim(sdoh_sub)
summary(sdoh_sub)

# southeast only subset for visualization
sdoh_se <- sdoh_2019_updated %>%
  filter(region == "South",
         state != "District of Columbia")


#Summary of southeast data
dim(sdoh_se)
summary(sdoh_se)

# Exploratory analysis using updated data
# Histograms of each variable
#premature death (right-skewed)
predeat_hist <- ggplot(sdoh_2019_updated, aes(x = premature_death))+
  geom_histogram()+
  labs(title="Premature Death Rate")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))

#Save image
ggsave("predeat_hist.png", predeat_hist, width = 10, height = 6, dpi = 300)

#median income (right-skewed)
medinc_hist <- ggplot(sdoh_2019_updated, aes(x = med_income))+
  geom_histogram()+
  labs(title="Median Income")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))

#Save image
ggsave("medinc_hist.png", medinc_hist, width = 10, height = 6, dpi = 300)

#poverty rate (right_skewed)
pov_hist <- ggplot(sdoh_2019_updated, aes(x = below_poverty))+
  geom_histogram()+
  labs(title="Poverty Rate")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))
#Save image
ggsave("pov_hist.png", pov_hist, width = 10, height = 6, dpi = 300)

#unemployment (right-skewed)
unem_hist <- ggplot(sdoh_2019_updated, aes(x = unemployment))+
  geom_histogram()+
  labs(title="Unemployment Rate")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))
#Save image
ggsave("unem_hist.png", unem_hist, width = 10, height = 6, dpi = 300)

#exercise access (right_skewed)
fitcen_hist <- ggplot(sdoh_2019_updated, aes(x = rec_cen))+
  geom_histogram()+
  labs(title="Fitness Center Access")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))
#Save image
ggsave("fitcen_hist.png", fitcen_hist, width = 10, height = 6, dpi = 300)


#food access (not normal)
foodacc_hist <- ggplot(sdoh_2019_updated, aes(x = grocery_access))+
  geom_histogram()+
  labs(title="Food Access")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))
#Save image
ggsave("foodacc_hist.png", foodacc_hist, width = 10, height = 6, dpi = 300)

#primary care access (not normal)
pcacc_hist <- ggplot(sdoh_2019_updated, aes(x = pc_per1k))+
  geom_histogram()+
  labs(title="Primary Care Access")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))
#Save image
ggsave("pcacc_hist.png", pcacc_hist, width = 10, height = 6, dpi = 300)

#uninsured (not normal)
uninsur_hist <- ggplot(sdoh_2019_updated, aes(x = uninsured))+
  geom_histogram()+
  labs(title="Uninsured Rate")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))
#Save image
ggsave("uninsur_hist.png", uninsur_hist, width = 10, height = 6, dpi = 300)

#environment (left_skewed)
partcon_hist <- ggplot(sdoh_2019_updated, aes(x = part_conc))+
  geom_histogram()+
  labs(title="Particulate Matter Concentration")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))
#Save image
ggsave("partcon_hist.png", partcon_hist, width = 10, height = 6, dpi = 300)

#education (right-skewed)
lths_hist <- ggplot(sdoh_2019_updated, aes(x = lt_hs))+
  geom_histogram()+
  labs(title="Educational Attainment less than HS")+
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))
#Save image
ggsave("lths_hist.png", lths_hist, width = 10, height = 6, dpi = 300)

# create a correlation plot of all variables 
sdoh_mat <- cor(sdoh_sub, use = "complete.obs")
ggcorrplot(sdoh_mat, 
           type='lower',
           lab = TRUE, 
           lab_size = 3, 
           colors = c("blue", "white", "red"))
#gives correlations rounded to two decimal places
round(sdoh_mat,2)
#save image of correlation plot
ggsave("corr_mat.png", corr_mat, width=10, height=6, dpi=300)

#boxplot of premature death rates by region
premdeatreg_plot <- ggplot(sdoh_2019_updated, aes(y=premature_death, x = region)) +
  geom_boxplot()+
  labs(title = "Premature Death by Region")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
#Save Image
ggsave("pdreg_plot.png", premdeatreg_plot, width=10, height=6, dpi=300)

#region premature death rate anova
predeat_anova <- aov(premature_death ~ region, data = sdoh_2019_updated)
summary(predeat_anova)
# Tukey HSD test for comparing group means pairwise
TukeyHSD(predeat_anova)

#boxplot of premature death rates by state (south only)
premdeatstat_plot <- ggplot(sdoh_se, aes(y=premature_death, x = state)) +
  geom_boxplot()+
  labs(title="Premature Death Rate by State")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

premdeatstat_plot
#Save Image
ggsave("pdstat_plot.png", premdeatstat_plot, width=10, height=6, dpi=300)


#uninsured and premature_death
uninsured_plot <- ggplot(sdoh_2019_updated, aes(x=uninsured, y = premature_death, color= region)) +
  geom_smooth(method="lm", color='red')+
  geom_point(alpha = 0.5)+
  xlab("Uninsured")+
  ylab("Premature Death Rate Per 100K")+
  scale_x_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

#Saving the graphic
ggsave("uninisured_plot.png", uninsured_plot, width = 10, height = 6, dpi = 300)

#median income and premature_death
medinc_plot <- ggplot(sdoh_2019_updated, aes(x=med_income, y = premature_death, color= region)) +
  geom_smooth(method="lm", color='red')+
  geom_point(alpha = 0.5)+
  xlab("Median Income")+
  ylab("Premature Death Rate Per 100K")+
  scale_x_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

#Saving the graphic
ggsave("medinc_plot.png", medinc_plot, width = 10, height = 6, dpi = 300)

#Food access and premature death
foodaccess_plot <- ggplot(sdoh_updated_2019, aes(x=grocery_access, y = premature_death, color= region)) +
  geom_smooth(method="lm", color='red')+
  geom_point(alpha = 0.5)+
  xlab("Food Access")+
  ylab("Premature Death Rate Per 100K")+
  scale_x_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

#Saving the graphic
ggsave("foodaccess_plot.png", foodaccess_plot, width = 10, height = 6, dpi = 300)

#unemployment and premature death
unempl_plot <- ggplot(sdoh_2019_updated, aes(x=unemployment, y = premature_death, color= region)) +
  geom_smooth(method="lm", color='red')+
  geom_point(alpha = 0.5)+
  xlab("Unemployment Rate")+
  ylab("Premature Death Rate Per 100K")+
  scale_x_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

#Saving the graphic
ggsave("unempl_plot.png", unempl_plot, width = 10, height = 6, dpi = 300)

#environment and premature death
env_plot <- ggplot(sdoh_2019_updated, aes(x=part_conc, y = premature_death, color= region)) +
  geom_smooth(method="lm", color='red')+
  geom_point(alpha = 0.5)+
  xlab("PM 2.5 Concentration")+
  ylab("Premature Death Rate Per 100K")+
  scale_x_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

#Saving the graphic
ggsave("env_plot.png", env_plot, width = 10, height = 6, dpi = 300)

#education and premature death
educ_plot <- ggplot(sdoh_2019_updated, aes(x=lt_hs, y = premature_death, color= region)) +
  geom_smooth(method="lm", color='red')+
  geom_point(alpha = 0.5)+
  xlab("Percent Population Less than HS Grad")+
  ylab("Premature Death Rate Per 100K")+
  scale_x_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

#Saving the graphic
ggsave("educ_plot.png", educ_plot, width = 10, height = 6, dpi = 300)

#exercise access and premature death
fitn_plot <- ggplot(sdoh_2019_updated, aes(x=rec_cen, y = premature_death, color= region)) +
  geom_smooth(method="lm", color='red')+
  geom_point(alpha = 0.5)+
  xlab("Fitness Centers per 1K")+
  ylab("Premature Death Rate Per 100K")+
  scale_x_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

#Saving the graphic
ggsave("fitn_plot.png", fitn_plot, width = 10, height = 6, dpi = 300)

#poverty and premature death
pov_plot <- ggplot(sdoh_2019_updated, aes(x=below_poverty, y = premature_death, color= region)) +
  geom_smooth(method="lm", color='red')+
  geom_point(alpha = 0.5)+
  xlab("Percent Population below Poverty Line")+
  ylab("Premature Death Rate Per 100K")+
  scale_x_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

#Saving the graphic
ggsave("pov_plot.png", pov_plot, width = 10, height = 6, dpi = 300)

#primary care access and premature death
pc_plot <- ggplot(sdoh_2019_updated, aes(x=pc_per1k, y = premature_death, color= region)) +
  geom_smooth(method="lm", color='red')+
  geom_point(alpha = 0.5)+
  xlab("Primary Care Providers per 1K")+
  ylab("Premature Death Rate Per 100K")+
  scale_x_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))

#Saving the graphic
ggsave("pc_plot.png", pc_plot, width = 10, height = 6, dpi = 300)

#running a linear regression of premature death and all other variables as predicators
lm.all <- lm(premature_death~., data=sdoh_sub)
summary(lm.all)
#print out the qq plot
par(mfrow=c(2,2))
plot(lm.all)

#Saving the graphic
ggsave("qq_plot.png", qq_plot, width=10, length = 8)
#Run best subset selection function from olsrr package
ols_step_best_subset(lm.all)
#best subset selection
out<-ols_step_best_subset(lm.all)
plot(out)

# 5-variable model
lm.best<-lm(premature_death~med_income+
              grocery_access+
              part_conc+
              lt_hs+
              below_poverty,
            data=sdoh_sub)
#visualize fit of model
par(mfrow=c(2,2))
plot(lm.best)

# 6-variable model
lm.best1<-lm(premature_death~med_income+
              grocery_access+
              part_conc+
              lt_hs+
              below_poverty+
              rec_cen,
             data=sdoh_sub)
#visualize fit of model
par(mfrow=c(2,2))
plot(lm.best1)

#Show coefficients of selected model
lm.best$coefficients

shapiro.test(residuals(lm.best))
