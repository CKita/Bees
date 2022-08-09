# Bees

Supplement to the chapter 1 of the dissertation "A meta-analysis of the effects of pesticides on bees and their pollination service in the context of intensive agricultural practices".  

Ecological Synthesis Lab (SintECO).

Authors: Cristina A. Kita, Laura C. Leal, Marco A. R. Mello

E-mail: c.akemikita@gmail.com

Run in R version 4.2.0 (2022-04-22 ucrt) -- "Vigorous Calisthenics"

Disclaimer: You may freely use the software and data provided here for commercial or non-commercial purposes at your own risk. We assume no responsibility or liability for the use of this material, convey no license or title under any patent, copyright, or mask work right to the product. We reserve the right to make changes in the material without notification. We also make no representation or warranty that such application will be suitable for the specified use without further testing or modification. If this material helps you produce any academic work (paper, book, chapter, monograph, dissertation, report or similar), please acknowledge the authors and cite the source.

# Functionality 

The data and scripts provided here aim at making our study fully reproducible. You will find code to reproduce both the analysis and the figures, as well as the main supplementary material.

# List of folders and files 

Bee survival (folder)

1. Data (folder)
 
    a. dados_sobrevivencia.csv -> data set with raw data.
    b. data.comp.csv -> data set with raw data and effect sizes.
    c. data.comp.2 -> data set with raw abd effect sizes.
    d. survival_sensi_out2.csv -> data set used in sensibility test. 
 
 2. Figures (folder)
 
    a. survival.tiff -> overall effect size of pesticide application on bee survival.
    b. survival_sensibility_test.tiff -> overall effect size of pesticide application on bee survival without outliers. 
   
 3. Code (folder)

    a. effect_sizes_model_survival.R -> main script in a tutorial format to help you reproduce the analyses and figures.  
   
   

Lethal and Sublethal (folder)

1. Data (folder)
    
    a. let_sublet.csv -> data set with all raw data and effect sizes used. 
    b. let.csv -> data set with raw and effect sizes of lethal.
    c. sub.csv -> data set with raw and effect sizes of sublethal.
    d. sub_sensi_out.csv -> data set used in sensibility test.
     
  1.1 effect sizes (folder):
      
      - categorical (folder)
         a. cat.csv -> data set with raw categorical data.
         b. effect_sizes_cat.csv  -> data set with categorical data and effect sizes. 
         c. just_effect_sizes.csv -> data set only with effect sizes.
         d. planilha_bruta.csv -> all raw data. 
   
      - continuous (folder)
         a. continuous.csv -> data set with raw continuous data. 
         b. effect_sizes_cont.csv -> data set with raw continuous data and effect sizes. 
  
2. Figures (folder)
   
     a. overall.tiff -> overall effect size of pesticide application on the bee community and its pollination service.
     b. let_sublet.tiff -> mean effect sizes of lethal and sublethal effects of pesticide application on the bee community and its pollination service.
     c. sensibility_test_sublet.tiff -> mean sublethal effect size of pesticide application on the bee community and its pollination service without outliers. 

3. Code (folder)
     
     a. script_modelos_letal_subletal.R -> main script in a tutorial format to help you reproduce the analyses and figures.
   
   3.1 effect sizes(folder)
   
       - categorical (folder)
       
         a. effect_sizes_cat.R -> script in a tutorial format to help you calculate the categorical effect sizes.
         
       - continuous (folder)
       
         a. effect_sizes_cont.R -> script in a tutorial format to help you calculate the continuos effect sizes.
         


Papers.xlsx -> List of papers used in this meta-analysis

            
# Instructions 

1. Run the main scripts to calculate the mean effect sizes of survival and lethal and sublethal effects;

2. Follow the instructions provided in the script.


