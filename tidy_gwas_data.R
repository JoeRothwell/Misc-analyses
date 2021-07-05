# Tidy up raw data
# From: https://marinalearning.netlify.app/2021/03/22/setting-up-multivariable-mendelian-randomization-analysis/


# raw file location
raw_gwas_file <- paste0(data_path, 'adult_bmi_GWAS_raw_pipeline_output.txt.gz')

gwas_outcome_format <-
  vroom(raw_gwas_file,       # vroom is faster than fread!
        #  only read in columns that we need and 
        col_select = c("SNP","BETA","SE",
                       "ALLELE1","ALLELE0","A1FREQ",
                       "P_BOLT_LMM_INF")) %>% 
  # format data into the 'outcome' format right away
  format_data(., type = "outcome",
              snp_col = "SNP",
              beta_col = "BETA",
              se_col = "SE",
              effect_allele_col = "ALLELE1",
              other_allele_col = "ALLELE0",
              eaf_col = "A1FREQ",
              pval_col = "P_BOLT_LMM_INF") %>% 
  # store trait name in the data frame (this will make later analysis easier)
  mutate(outcome = 'Adult BMI')

# save this tidy and formatted file so that it cam be used directly in MR analysis later
vroom_write(gwas_outcome_format, path = paste0(data_path,'adult_bmi_GWAS_tidy_outcome.txt.gz'))

# from the file that we read in, extract the tophits and save them in tophits file; 
# this file will also be used directly in MR analysis later
tophits <- 
  gwas_outcome_format %>% 
  filter(pval.outcome < 5e-8) %>% 
  convert_outcome_to_exposure() %>% 
  clump_data(., clump_r2 = 0.001)

write_tsv(tophits, path = paste0(data_path, 'adult_bmi_tophits.tsv'))