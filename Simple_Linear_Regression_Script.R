# The following script runs a simple linear regression model with pollutants on one side and MRI metrics on the other. 
# Data collation and processing, including exclusions, took place prior to this script.

# Load required libraries.
library(tidyverse)
library(moderndive)
library(purrr)
library(datapasta)

# Rename dataset and round numbers, if necessary.
# dHCP_structural_data <- Importedfile %>% 
#   mutate_if(is.numeric, round) # round up numeric variables in the data frame to integer

summary(dHCP_structural_data) # basic summary check

# Statistical model and loop - different variations for different inputs
var_e <- c("PM25", "PM10", "NO2")

var_h <- c("WM.rel","GM.rel",	"Amygdala.rel",	"Hippocampus.rel",	"Cerebellum.rel",	"DGM.rel", "Ventricles.rel", "Brainstem.rel", "Thalamus.rel")

#var_h <- c("LILF_FA", "LILF_MD", "LILF_AD",	"LILF_RD",	"RILF_FA",	"RILF_MD",	"RILF_AD",	"RILF_RD",	"LUNC_FA",	"LUNC_MD",	"LUNC_AD",	"LUNC_RD",	"RUNC_FA",	"RUNC_MD",	"RUNC_AD",	"RUNC_RD",	"LIFO_FA",	"LIFO_MD",	"LIFO_AD",	"LIFO_RD",	"RIFO_FA",	"RIFO_MD",	"RIFO_AD",	"RIFO_RD",	"LSLF_FA",	"LSLF_MD",	"LSLF_AD",	"LSLF_RD",	"RSLF_FA",	"RSLF_MD",	"RSLF_AD",	"RSLF_RD",	"LCST_FA",	"LCST_MD",	"LCST_AD",	"LCST_RD",	"RCST_FA",	"RCST_MD",	"RCST_AD",	"RCST_RD",	"CC_FA",	"CC_MD",	"CC_AD",	"CC_RD") 

# var_h <- c("Rel_WM","Rel_GM",	"Rel_Amygdala_Hyppocampus",	"Rel_Cerebellum",	"Rel_Basal_Ganglia",	"Rel_Ventricle", "Rel_Brainstem", "Rel_extra_csf")

# var_h <- c(
#   "FA_ac", "FA_af_l", "FA_af_r", "FA_ar_l", "FA_ar_r", "FA_atr_l", "FA_atr_r",
#   "FA_cbd_l", "FA_cbd_r", "FA_cbp_l", "FA_cbp_r", "FA_cbt_l", "FA_cbt_r",
#   "FA_cst_l", "FA_cst_r", "FA_fa_l", "FA_fa_r", "FA_fma", "FA_fmi", "FA_fx_l",
#   "FA_fx_r", "FA_ifo_l", "FA_ifo_r", "FA_ilf_l", "FA_ilf_r", "FA_mcp",
#   "FA_mdlf_l", "FA_mdlf_r", "FA_or_l", "FA_or_r", "FA_slf1_l", "FA_slf1_r",
#   "FA_slf2_l", "FA_slf2_r", "FA_slf3_l", "FA_slf3_r", "FA_str_l", "FA_str_r",
#   "FA_uf_l", "FA_uf_r", "FA_vof_l", "FA_vof_r", "fintra_ac", "fintra_af_l",
#   "fintra_af_r", "fintra_ar_l", "fintra_ar_r", "fintra_atr_l", "fintra_atr_r",
#   "fintra_cbd_l", "fintra_cbd_r", "fintra_cbp_l", "fintra_cbp_r", "fintra_cbt_l",
#   "fintra_cbt_r", "fintra_cst_l", "fintra_cst_r", "fintra_fa_l", "fintra_fa_r",
#   "fintra_fma", "fintra_fmi", "fintra_fx_l", "fintra_fx_r", "fintra_ifo_l",
#   "fintra_ifo_r", "fintra_ilf_l", "fintra_ilf_r", "fintra_mcp", "fintra_mdlf_l",
#   "fintra_mdlf_r", "fintra_or_l", "fintra_or_r", "fintra_slf1_l", "fintra_slf1_r",
#   "fintra_slf2_l", "fintra_slf2_r", "fintra_slf3_l", "fintra_slf3_r",
#   "fintra_str_l", "fintra_str_r", "fintra_uf_l", "fintra_uf_r", "fintra_vof_l",
#   "fintra_vof_r", "fiso_ac", "fiso_af_l", "fiso_af_r", "fiso_ar_l", "fiso_ar_r",
#   "fiso_atr_l", "fiso_atr_r", "fiso_cbd_l", "fiso_cbd_r", "fiso_cbp_l",
#   "fiso_cbp_r", "fiso_cbt_l", "fiso_cbt_r", "fiso_cst_l", "fiso_cst_r",
#   "fiso_fa_l", "fiso_fa_r", "fiso_fma", "fiso_fmi", "fiso_fx_l", "fiso_fx_r",
#   "fiso_ifo_l", "fiso_ifo_r", "fiso_ilf_l", "fiso_ilf_r", "fiso_mcp",
#   "fiso_mdlf_l", "fiso_mdlf_r", "fiso_or_l", "fiso_or_r", "fiso_slf1_l",
#   "fiso_slf1_r", "fiso_slf2_l", "fiso_slf2_r", "fiso_slf3_l", "fiso_slf3_r",
#   "fiso_str_l", "fiso_str_r", "fiso_uf_l", "fiso_uf_r", "fiso_vof_l", "fiso_vof_r",
#   "MD_ac", "MD_af_l", "MD_af_r", "MD_ar_l", "MD_ar_r", "MD_atr_l", "MD_atr_r",
#   "MD_cbd_l", "MD_cbd_r", "MD_cbp_l", "MD_cbp_r", "MD_cbt_l", "MD_cbt_r",
#   "MD_cst_l", "MD_cst_r", "MD_fa_l", "MD_fa_r", "MD_fma", "MD_fmi", "MD_fx_l",
#   "MD_fx_r", "MD_ifo_l", "MD_ifo_r", "MD_ilf_l", "MD_ilf_r", "MD_mcp",
#   "MD_mdlf_l", "MD_mdlf_r", "MD_or_l", "MD_or_r", "MD_slf1_l", "MD_slf1_r",
#   "MD_slf2_l", "MD_slf2_r", "MD_slf3_l", "MD_slf3_r", "MD_str_l", "MD_str_r",
#   "MD_uf_l", "MD_uf_r", "MD_vof_l", "MD_vof_r", "OD_ac", "OD_af_l", "OD_af_r",
#   "OD_ar_l", "OD_ar_r", "OD_atr_l", "OD_atr_r", "OD_cbd_l", "OD_cbd_r",
#   "OD_cbp_l", "OD_cbp_r", "OD_cbt_l", "OD_cbt_r", "OD_cst_l", "OD_cst_r",
#   "OD_fa_l", "OD_fa_r", "OD_fma", "OD_fmi", "OD_fx_l", "OD_fx_r", "OD_ifo_l",
#   "OD_ifo_r", "OD_ilf_l", "OD_ilf_r", "OD_mcp", "OD_mdlf_l", "OD_mdlf_r",
#   "OD_or_l", "OD_or_r", "OD_slf1_l", "OD_slf1_r", "OD_slf2_l", "OD_slf2_r",
#   "OD_slf3_l", "OD_slf3_r", "OD_str_l", "OD_str_r", "OD_uf_l", "OD_uf_r",
#   "OD_vof_l", "OD_vof_r"
# )

# Loop regression model over dependent and independent variables, including covariates.

mark=0
for(i in var_h)
{
  for(j in var_e)
  {
    m <- lm(Data_sheet[,i] ~ Data_sheet[,j] + Age_at_scan + Age_at_birth + IMD + Sex, data = Data_sheet) # Build up simple Linear Regression (SLR) model
    tmpr <- cbind(y = i, x = j, get_regression_table(m, digits = 7)[2, 2:7]) # Only extract the parameters for pollutant j
    if(mark==0) {model_output <- tmpr; mark=1} else {model_output <- rbind(model_output,tmpr)}
  }
}

# Adjust results for multiple comparisons using Benjamini-Hochberg method (BH)
model_output <- model_output %>% 
  mutate(q_value = round(p.adjust(p_value, "BH"), 3)) # Other FDR method options: c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")

# Write result to CSV.
write.csv(model_output, "SLR_Results.csv", row.names = FALSE)
