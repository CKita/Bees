# Bees

Supplement to chapter 1 of the M.Sc. dissertation "A meta-analysis of the effects of pesticides on bees and their pollination service in the context of intensive agricultural practices". Graduate School in Ecology, Institute of Biosciences, University of São Paulo, Brazil.

[Ecological Synthesis Lab](https://marcomellolab.wordpress.com) (SintECO).

Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello.

E-mail: [c.akemikita\@gmail.com](mailto:c.akemikita@gmail.com){.email}.

Fisrt published on August 19th, 2022.

Run in R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid".

Disclaimer: You may freely use the software and data provided here for commercial or non-commercial purposes at your own risk. We assume no responsibility or liability for the use of this material, convey no license or title under any patent, copyright, or mask work right to the product. We reserve the right to make changes in the material without notification. We also make no representation or warranty that such application will be suitable for the specified use without further testing or modification. If this material helps you produce any academic work (paper, book, chapter, monograph, dissertation, thesis, report, talk, keynote, lecture or similar), please acknowledge the authors and cite the source.

## Functionality

The data and scripts provided here aim at making our study fully reproducible. You will find code to reproduce both the analysis and the figures, as well as the main supplementary material.

## List of folders and files

### **Bee survival (folder)**

1.  Data (folder)

    a.  dados_sobrevivencia.csv -\> data frame with raw data.

    b.  data.comp.csv -\> data frame with raw data and effect sizes.

    c.  data.comp.2 -\> data frame with raw and effect sizes.

    d.  survival_sensi_out2.csv -\> data frame used in the sensibility test.

2.  Figures (folder)

    a.  survival.tiff -\> overall effect size of pesticide application on bee survival.

    b.  survival_sensibility_test.tiff -\> overall effect size of pesticide application on bee survival without outliers.

3.  Code (folder)

    a.  effect_sizes_model_survival.R -\> main script formatted as a tutorial to help you reproduce the analyses and figures.

### **Lethal and Sublethal (folder)**

1.  Data (folder)

    a.  let_sublet.csv -\> data frame with raw data and effect sizes.

    b.  let.csv -\> data frame with raw data and effect sizes of lethal effects.

    c.  sub.csv -\> data frame with raw data and effect sizes of sublethal effects.

    d.  sub_sensi_out.csv -\> data frame used in the sensibility test.

    *effect sizes (folder)*

    -   categorical (folder)

        a.  cat.csv -\> data frame with raw categorical data.

        b.  effect_sizes_cat.csv -\> data frame with categorical data and effect sizes.

        c.  planilha_bruta.csv -\> raw data.

    -   continuous (folder)

        a.  continuous.csv -\> data frame with raw continuous data.

        b.  effect_sizes_cont.csv -\> data frame with raw continuous data and effect sizes.

2.  Figures (folder)

    a.  overall.tiff -\> overall effect sizes of pesticide application on the bee community and its pollination service.

    b.  let_sublet.tiff -\> mean effect sizes of lethal and sublethal effects of pesticide application on the bee community and its pollination service.

    c.  sensibility_test_sublet.tiff -\> mean sublethal effect sizes of pesticide application on the bee community and its pollination service without outliers.

3.  Code (folder)

    a.  script_modelos_letal_subletal.R -\> main script formatted as a tutorial to help you reproduce the analyses and figures.

    *effect sizes(folder)*

    -   categorical (folder)

        a.  effect_sizes_cat.R -\> script formatted as a tutorial to help you calculate categorical effect sizes.

    -   continuous (folder)

        a.  effect_sizes_cont.R -\> script formatted as a tutorial to help you calculate continuous effect sizes.

### **Study sites (folder)**

1.  Data (folder)

    a.  sites.csv -\> data frame with attributes of the study sites where data collection was carried out.

2.  Figures (folder)

    a.  sites.png -\> geographic distribution of the study sites and number of effect size values recorded per country.

3.  Code (folder)

    a.  Sites.R -\> script formatted as a tutorial to help you reproduce the figure.

### **Papers.xlsx (file)**

List of papers included in our meta-analysis.

## Instructions

1.  Choose between the two types of analyses (survival vs. lethal/sublethal) and go to the respective folder;

2.  Run the main script of each folder to calculate the mean effect sizes;

3.  Follow the instructions provided in each script.

## Feedback

If you have any questions, corrections, or suggestions, please feel free to open an [issue](https://github.com/CKita/Bees/issues) or make a [pull request](https://github.com/CKita/Bees/pulls).

## Acknowledgments

We are sincerely grateful to the authors of all studies included in our meta-analysis, whose effort made this synthesis possible. Tereza C. Giannini and Isabel A. Santos, members of the thesis committee of CAK, made invaluable suggestions to study design and interpretation of results. CAK thanks the Coordination for the Improvement of Higher Education Personnel (CAPES, 88887.470293/2019-00) and the Graduate School in Ecology of the University of São Paulo (PPGE-IB-USP) for the M.Sc. scholarship. MARM was funded by the Alexander von Humboldt Foundation (AvH, 3.4-8151/15037 and 3.2-BRA/1134644), National Council for Scientific and Technological Development (CNPq, 304498/2019-0), São Paulo Research Foundation (FAPESP, 2018/20695-7), and Dean of Research of the University of São Paulo (PRP-USP, 18.1.660.41.7). We also thank the [Stack Overflow](https://stackoverflow.com/) community, where we solve most of our coding dilemmas.
