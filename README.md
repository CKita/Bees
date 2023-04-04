# Bees

Supplement to chapter 1 of the M.Sc. dissertation "A meta-analysis of the effects of pesticides on bees and their pollination service in the context of intensive agricultural practices". Graduate School in Ecology, Institute of Biosciences, University of São Paulo, Brazil.

[Ecological Synthesis Lab](https://marcomellolab.wordpress.com) (SintECO).

Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello.

E-mail: [c.akemikita\@gmail.com](mailto:c.akemikita@gmail.com){.email}.

First published on August 19th, 2022.

[![DOI](https://zenodo.org/badge/513542240.svg)](https://zenodo.org/badge/latestdoi/513542240)

Run in R version 4.2.2 (2022-10-31) -- "Innocent and Trusting".

Disclaimer: You may freely use the software and data provided here for commercial or non-commercial purposes at your own risk. We assume no responsibility or liability for the use of this material, convey no license or title under any patent, copyright, or mask work right to the product. We reserve the right to make changes in the material without notification. We also make no representation or warranty that such application will be suitable for the specified use without further testing or modification. If this material helps you produce any academic work (paper, book, chapter, monograph, dissertation, thesis, report, talk, keynote, lecture or similar), please acknowledge the authors and cite the source.

## Functionality

The data and scripts provided here aim at making our study fully reproducible. You will find code to reproduce both the analyses and the figures, as well as the main supplementary material.

## List of folders and files

### **Lethal effect (folder)**

1.  Data (folder)

    a.  lethal.csv -\> data frame with raw data.

    b.  lethal_effects.csv -\> data frame with raw data and effect sizes.

    c.  let_sensi_out.csv -\> data frame used in the sensibility test.

2.  Figures (folder)

    a.  lethal.tiff -\> mean lethal effect size of pesticide application on bees.

    b.  lethal_sensibility_test.tiff -\> mean lethal effect size of pesticide application on bees without outliers.

    c.  publication_bias.tiff -\> visual analysis of publication bias.

3.  Code (folder)

    a.  Lethal effect.R -\> main script formatted as a tutorial to help you reproduce the analyses and figures.

### **Sublethal effect (folder)**

1.  Data (folder)

    a.  sub.csv -\> data frame with raw data and effect sizes.
   
2.  Figures (folder)

    a.  sublethal.tiff -\> mean sublethal effect size of pesticide application on bees.
   
    b.  publication_bias.tiff -\> visual analysis of publication bias.

3.  Code (folder)

    a.  Sublethal effect.R -\> main script formatted as a tutorial to help you reproduce the analyses and figures.

### **Consequences of for bee communities (folder)**

1.  Data (folder)

    a.  consequences.csv -\> data frame with raw data and effect sizes.

2.  Figures (folder)

    a.  consequences_let_sub.tiff -\> mean effect sizes of the consequences of lethal and sublethal effects for the bee communities that could impair their pollination service within agricultural crop fields.

    b.  publication_bias.tiff -\> visual analysis of publication bias.

3.  Code (folder)

    a.  Consequences.R -\> main script formatted as a tutorial to help you reproduce the analyses and figures.

### **Effect_sizes_Hedges (folder)**

*categorical (folder)*

-   Data (folder)

    a.  cat.csv -\> data frame with raw categorical data.
   
-   Code (folder)

    a.  effect_sizes_cat.R -\> script formatted as a tutorial to help you calculate categorical effect sizes.

-   Results (folder)

    a.  effect_sizes_cat.csv -\> data frame with raw categorical data and effect sizes.

*continuous (folder)*

-   Data (folder)

    a.  continuous.csv -\> data frame with raw categorical data.
    
-   Code (folder)

    a.  effect_sizes_cont.R -\> script formatted as a tutorial to help you calculate continuous effect sizes.

-   Results (folder)

    a.  effect_sizes_cont.csv -\> data frame with raw continuous data and effect sizes.

### **Study sites (folder)**

1.  Data (folder)

    a.  sites.csv -\> data frame with attributes of the study sites where data collection was carried out of all effect sizes.
    
    b.  sites_country -\> data frame with attributes of the study sites where data collection was carried out of per studies used in meta-analysis. 

2.  Figures (folder)

    a.  sites.png -\> geographic distribution of the study sites and number of effect size values recorded per country.

    b.  study sites.png -\> geographic distribution of the study sites.

    c.  effect sizes.png -\> number of effect size values recorded per country of all studies used in our meta-analysis.

    d.  effect sizes lethal.png -\> number of lethal effect size values recorded per country.

    e.  effect sizes sublethal.png -\> number of sublethal effect size values recorded per country.

    f.  effect sizes consequences.png -\> number of effect size values of the consequences of lethal and sublethal effects recorded per country.
    
    g.  studies.png -\> number of studies per country.

3.  Code (folder)

    a.  Sites.R -\> script formatted as a tutorial to help you reproduce the figure.

### **Papers (folder)**

1.  Code (folder)

    a.  litserchr.R -\> script formatted as a tutorial to help you reproduce the list of papers included in our meta-analysis.

2.  Data (folder)

    a.  records_wos.csv -\> list of papers exported from Web of Science database.

    b.  records_scopus.csv -\> list of papers exported from Scopus database. 
     
    c.  duplicate_records_removed -\> list of papers from Web of Science and Scopus after duplicate removal. 

    f.  Papers included in meta-analysis -\> final list of papers included in meta-analysis.

### **Figure (folder)**

1.  Data (folder)

    a.  consequences.csv -\> data frame with raw data and effect sizes.
    
    b.  lethal_effects.csv -\> data frame with raw data and effect sizes.
    
    a.  sub.csv -\> data frame with raw data and effect sizes.

2.  Figures (folder)

    a.  mean_effect_sizes.png -\> lethal and sublethal pesticide effects on bees, and their consequences for bee communities in crops.

3.  Code (folder)

    a.  Mean effect sizes.R -\> main script formatted as a tutorial to help you reproduce the analyses and figures.

    
## Instructions

1.  Choose between the type of analysis you want to reproduce (lethal, sublethal or consequences) and go to the respective folder;

2.  Run the main script of each folder to calculate the mean effect sizes;

3.  Follow the instructions provided in each script.

## Feedback

If you have any questions, corrections, or suggestions, please feel free to open an [issue](https://github.com/CKita/Bees/issues) or make a [pull request](https://github.com/CKita/Bees/pulls).

## Acknowledgments

We thank the authors of all studies included in our meta-analysis, who made this synthesis possible. Isabel Santos and Tereza Giannini made invaluable suggestions to study design. Astrid Kleinert, Osmar Malaspina, and Roberta Nocelli helped us see our findings from a broader perspective. CAK thanks the Coordination for the Improvement of Higher Education Personnel (CAPES, 88887.470293/2019-00) and the Graduate School in Ecology of the University of São Paulo (PPGE/IB-USP) for the M.Sc. scholarship. MARM was funded by the Alexander von Humboldt Foundation (AvH, 3.2-BRA/1134644 and 3.4–1134644–BRA-GA), National Council for Scientific and Technological Development (CNPq, 304498/2019-0), São Paulo Research Foundation (FAPESP, 2018/20695-7), and Dean of Research of the University of São Paulo (PRP-USP, 18.1.660.41.7). We also thank the Stack Overflow community (https://stackoverflow.com/), where we solve most of our coding dilemmas. 
