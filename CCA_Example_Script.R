## 
library(CCA)
library(matrixStats)
library(reshape2)
library(ggplot2)
library(GGally)
library(tidyr)
library(dplyr)
library(CCP)
library(FDRestimation)
library(corrplot)

# Tract residuals 
# "tracts"  contains all tract information, "explanatory" contains pollution exposure data as well as confounding data.
Input<- tracts
Output<- explanatory

Output <- Output %>% relocate(IMD, .after = last_col()) # reorder may be necessary for adjustment

# Turn into matrix format
Input=as.matrix(Input)
Output=as.matrix(Output)

# Loop create linear models and extract residuals # change output to 1:2 for male/female analysis.
residuals_list_1_3 <- list()

for (i in 1:210) {
  model <- lm(Input[, i] ~ Output[, 1:3])
  residuals <- residuals(model)
  residuals_list_1_3[[i]] <- residuals
}



# Creating dataframe for residuals of Output variables 1:3
residuals_df_1_3 <- data.frame(matrix(nrow = nrow(Input), ncol = 210))
colnames(residuals_df_1_3) <- c("FA_ac", "FA_af_l", "FA_af_r", "FA_ar_l", "FA_ar_r", "FA_atr_l", "FA_atr_r",
                                "FA_cbd_l", "FA_cbd_r", "FA_cbp_l", "FA_cbp_r", "FA_cbt_l", "FA_cbt_r",
                                "FA_cst_l", "FA_cst_r", "FA_fa_l", "FA_fa_r", "FA_fma", "FA_fmi",
                                "FA_fx_l", "FA_fx_r", "FA_ifo_l", "FA_ifo_r", "FA_ilf_l", "FA_ilf_r",
                                "FA_mcp", "FA_mdlf_l", "FA_mdlf_r", "FA_or_l", "FA_or_r", "FA_slf1_l",
                                "FA_slf1_r", "FA_slf2_l", "FA_slf2_r", "FA_slf3_l", "FA_slf3_r", "FA_str_l",
                                "FA_str_r", "FA_uf_l", "FA_uf_r", "FA_vof_l", "FA_vof_r", "fintra_ac",
                                "fintra_af_l", "fintra_af_r", "fintra_ar_l", "fintra_ar_r", "fintra_atr_l",
                                "fintra_atr_r", "fintra_cbd_l", "fintra_cbd_r", "fintra_cbp_l", "fintra_cbp_r",
                                "fintra_cbt_l", "fintra_cbt_r", "fintra_cst_l", "fintra_cst_r", "fintra_fa_l",
                                "fintra_fa_r", "fintra_fma", "fintra_fmi", "fintra_fx_l", "fintra_fx_r",
                                "fintra_ifo_l", "fintra_ifo_r", "fintra_ilf_l", "fintra_ilf_r", "fintra_mcp",
                                "fintra_mdlf_l", "fintra_mdlf_r", "fintra_or_l", "fintra_or_r", "fintra_slf1_l",
                                "fintra_slf1_r", "fintra_slf2_l", "fintra_slf2_r", "fintra_slf3_l", "fintra_slf3_r",
                                "fintra_str_l", "fintra_str_r", "fintra_uf_l", "fintra_uf_r", "fintra_vof_l",
                                "fintra_vof_r", "fiso_ac", "fiso_af_l", "fiso_af_r", "fiso_ar_l", "fiso_ar_r",
                                "fiso_atr_l", "fiso_atr_r", "fiso_cbd_l", "fiso_cbd_r", "fiso_cbp_l",
                                "fiso_cbp_r", "fiso_cbt_l", "fiso_cbt_r", "fiso_cst_l", "fiso_cst_r", "fiso_fa_l",
                                "fiso_fa_r", "fiso_fma", "fiso_fmi", "fiso_fx_l", "fiso_fx_r", "fiso_ifo_l",
                                "fiso_ifo_r", "fiso_ilf_l", "fiso_ilf_r", "fiso_mcp", "fiso_mdlf_l",
                                "fiso_mdlf_r", "fiso_or_l", "fiso_or_r", "fiso_slf1_l", "fiso_slf1_r",
                                "fiso_slf2_l", "fiso_slf2_r", "fiso_slf3_l", "fiso_slf3_r", "fiso_str_l",
                                "fiso_str_r", "fiso_uf_l", "fiso_uf_r", "fiso_vof_l", "fiso_vof_r",
                                "MD_ac", "MD_af_l", "MD_af_r", "MD_ar_l", "MD_ar_r", "MD_atr_l", "MD_atr_r",
                                "MD_cbd_l", "MD_cbd_r", "MD_cbp_l", "MD_cbp_r", "MD_cbt_l", "MD_cbt_r",
                                "MD_cst_l", "MD_cst_r", "MD_fa_l", "MD_fa_r", "MD_fma", "MD_fmi", "MD_fx_l",
                                "MD_fx_r", "MD_ifo_l", "MD_ifo_r", "MD_ilf_l", "MD_ilf_r", "MD_mcp",
                                "MD_mdlf_l", "MD_mdlf_r", "MD_or_l", "MD_or_r", "MD_slf1_l", "MD_slf1_r",
                                "MD_slf2_l", "MD_slf2_r", "MD_slf3_l", "MD_slf3_r", "MD_str_l", "MD_str_r",
                                "MD_uf_l", "MD_uf_r", "MD_vof_l", "MD_vof_r", "OD_ac", "OD_af_l", "OD_af_r",
                                "OD_ar_l", "OD_ar_r", "OD_atr_l", "OD_atr_r", "OD_cbd_l", "OD_cbd_r", "OD_cbp_l",
                                "OD_cbp_r", "OD_cbt_l", "OD_cbt_r", "OD_cst_l", "OD_cst_r", "OD_fa_l", "OD_fa_r",
                                "OD_fma", "OD_fmi", "OD_fx_l", "OD_fx_r", "OD_ifo_l", "OD_ifo_r", "OD_ilf_l",
                                "OD_ilf_r", "OD_mcp", "OD_mdlf_l", "OD_mdlf_r", "OD_or_l", "OD_or_r", "OD_slf1_l",
                                "OD_slf1_r", "OD_slf2_l", "OD_slf2_r", "OD_slf3_l", "OD_slf3_r", "OD_str_l",
                                "OD_str_r", "OD_uf_l", "OD_uf_r", "OD_vof_l", "OD_vof_r"
)

# Filling residuals of Output variables 1:3 into dataframe
for (i in 1:210) {
  residuals_df_1_3[, i] <- residuals_list_1_3[[i]]
}

residual_tracts <- residuals_df_1_3

#Entire gestation: Residuals of pollution, adjust for IMD 
model17 <- lm(Output[,4]~ Output[,7])
model18 <- lm(Output[,5]~ Output[,7])
model19 <- lm(Output[,6]~ Output[,7])


#Trimester specific analysis: Residuals of pollution, adjust for IMD and seasonality
model17 <- lm(Output[,4]~ Output[,7:8])
model18 <- lm(Output[,5]~ Output[,7:8])
model19 <- lm(Output[,6]~ Output[,7:8])


#rename and combine
resid_NO2 <- model17$residuals
resid_PM10 <- model18$residuals
resid_PM25 <- model19$residuals

residual_polls <- bind_cols(resid_NO2, resid_PM10, resid_PM25)

## 
## script to bootstrap CCA - tracts

# select the variables
# Residual tracts
residual_tracts <- residual_tracts %>%
  dplyr::select(1:210)

# Residual_polls
residual_polls <- residual_polls %>%
  dplyr::select(1:3)

# Enter two matrices here 
Input<- residual_tracts
Output<- residual_polls


## Permutation analysis
Input_Rand<-c()
Output_Rand<-c()
cc_rand<-c()
cor_comp_2<-c()

cc1<-cc(Input,Output)
cor_rand<-cc1$cor

for(i in 1:1000) { 
  Input_Rand_1<-Input
  for (c in 1:ncol(Input)){
    Input_Rand_1[,c] <- Input[sample(nrow(Input)),c]}
  
  Output_Rand_1<-Output
  for (c in 1:ncol(Output)){
    Output_Rand_1[,c] <- Output[sample(nrow(Output)),c]}
  
  cc_rand<-cc(Input_Rand_1,Output_Rand_1)
  cor_rand<-cbind(cor_rand,cc_rand$cor)
}

# Checking permutation significance - 95th percentile

View(cor_rand)

probs <- c(0.95)
random <- rowQuantiles(cor_rand, probs = probs) 
probs_1 <- c(0.5)
mean <- rowQuantiles(cor_rand, probs = probs_1)
probs_2 <- c(0.05)
random2 <- rowQuantiles(cor_rand, probs = probs_2)

# add plot to compare mode to random
# Visualising modes

real <- cor_rand[,1, drop = FALSE]
mode <- c(1,2,3)
dat1 <- data.frame(random, real, mode, random2, mean)
dat2 <- melt(dat1,  id.vars = 'mode', variable.name = 'series')

#create line plot for each column in data frame
Mode_plot <- ggplot(dat2, aes(mode, value)) +
  geom_line(aes(colour = series)) + 
  geom_point() + 
  scale_x_continuous(breaks = 1:3) + 
  theme(legend.title = element_blank()) + 
  scale_colour_manual(labels = c("95th percentile", "observed correlation", "5th percentile", "median"), values = c("blue", "red", "green", "yellow")) + 
  theme_bw()+
  theme(plot.title = element_text(size = 22, hjust = 0.5), legend.key.size = unit(1, 'cm')) +
  theme(axis.text = element_text(size = 12, face ="bold"), axis.title = element_blank())
Mode_plot

# No significant modes were found, so the analysis would end here. 
# I have included the rest of the script below. This would only be executed if significant modes were found.

# Script to loop cor.test - CCA - FDR correction

# Data sets need to be loaded as matrices. 
# Run one at a time
# Tracts
matrix1<- as.matrix(cc1$scores$xscores[,1]) #1-3
matrix2<-as.matrix(Input)##

# Pollutants
# matrix1<- as.matrix(cc1$scores$yscores[,1]) #1-3 
# matrix2<-as.matrix(Output)##

# correlation, p-values, confidence intervals

cor_mat<-matrix(nrow = dim(matrix1)[2], ncol = dim(matrix2)[2])
p_mat<-cor_mat
low_conf <- cor_mat
high_conf <- cor_mat

for(i in 1:dim(matrix1)[2]){
  for(j in 1:dim(matrix2)[2]){
    temp<-cor.test(as.matrix(matrix1[,i]),as.matrix(matrix2[,j]))
    cor_mat[i,j]<-temp$estimate
    p_mat[i,j]<-temp$p.value
    low_conf[i,j] <- temp$conf.int[1]   
    high_conf[i,j] <- temp$conf.int[2]
    p_mat_fdr<-FDRestimation::p.fdr(p_mat) 
  }
}

# Plot correlation - this visualises all variables
# In the case of tracts it would have to be limited to those that pass FDR since n = 210 or the visualisation would 
# have to be changed
corrplot(cor_mat, tl.pos = 'n', tl.srt = 0, tl.offset = 15, cl.cex = 0.8, addCoef.col = 'black')

# Check FDR corrected p-values
p_mat_fdr

# Check correlation
cor_mat
