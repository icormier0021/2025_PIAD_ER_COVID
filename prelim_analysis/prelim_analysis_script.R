#Preliminary analysis for the 2025 PIAD project on Maternal ER, COVID-19 experiences and infant socioemotional development
#Author: Isaac Cormier
#Date: 2025-10-31

#Install packages
packages <- c("haven", "here", "dplyr", "flextable", "purrr", "broom", "ggplot2")

if(!require("pacman"))install.packages("pacman")

pacman::p_load(char = packages, character.only = TRUE)

#Read data
df <- read_sav(here("data" ,"COVID-Preg-35MreducedPIAD.sav")) %>% 
        rename(COVID_Comp_Total = AvgObjCOVID40rescale)

#Define key variables
outcomes <- c(
        "BITSEA_Comp_Total_15M",
        "BITSEA_Comp_Total_24M",
        "BITSEA_Comp_Total_35M"
)

predictors <- c("DERS_Total", "COVID_Comp_Total")

#Check missing values
missing <- df %>%
        select(all_of(c(outcomes, predictors))) %>% 
        summarise(across(everything(), ~sum(is.na(.))))

flextable(missing)

#Plot regression diagnostics to check that assumptions are met and run shapiro-wilks test
check_assumptions <- function(data, outcome) {
        model <- lm(as.formula(paste0(outcome, " ~ DERS_Total + COVID_Comp_Total")), data=data)
        #Save plot as png file
        outfile <- here("prelim_analysis", "plots", paste0(outcome, "_diagnostic_plot.png"))
        dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
        png(outfile, width = 1200, height = 1200)
        par(mfrow = c(2,2))
        plot(model)
        dev.off()
        #Send plot to viewer
        par(mfrow = c(2,2))
        plot(model)
        
        #Print shaprio-wilk test results from residuals
        resid <- residuals(model)
        shapWilk <- shapiro.test(resid)
        print(shapWilk)
}

check_assumptions(df, "BITSEA_Comp_Total_15M")
check_assumptions(df, "BITSEA_Comp_Total_24M")
check_assumptions(df, "BITSEA_Comp_Total_35M")


#Run the regression model

run_bitsea_model <- function(data, outcome, standardize_predictors=FALSE) {
        vars <- c(outcome, "DERS_Total", "COVID_Comp_Total")
        data <- data[, vars]
        
        if (isTRUE(standardize_predictors)){
                std_preds <- scale(select(data, DERS_Total, COVID_Comp_Total)) #standardize the predictors
                std_preds <- as.data.frame(std_preds) #convert the standardized predictors back to df
                
                data$DERS_Total <- std_preds$DERS_Total
                data$COVID_Comp_Total <- std_preds$COVID_Comp_Total
        }
        #Main effect
        main_fml <- as.formula(paste0(outcome, " ~ DERS_Total + COVID_Comp_Total"))
        main_model <- lm(main_fml, data=data)
        #Interaction model
        int_fml <- as.formula(paste0(outcome, " ~ DERS_Total * COVID_Comp_Total"))
        int_model <- lm(int_fml, data=data)
        
        list(
                outcome = outcome, 
                main_model = main_model,
                int_model = int_model
        )
}

model_list <- map(outcomes, ~run_bitsea_model(df, .x))
names(model_list) <- outcomes

std_model_list <- map(outcomes, ~run_bitsea_model(df, .x, TRUE))
names(std_model_list) <- outcomes

#main-effects for 15M
summary(model_list[["BITSEA_Comp_Total_15M"]]$main_model)
#standardized main-effects for 15M
summary(std_model_list[["BITSEA_Comp_Total_15M"]]$main_model)
#interaction-effects for 15M
summary(model_list[["BITSEA_Comp_Total_15M"]]$int_model)
#standardized interaction-effects for 15M
summary(std_model_list[["BITSEA_Comp_Total_15M"]]$int_model)


#main-effects for 24M
summary(model_list[["BITSEA_Comp_Total_24M"]]$main_model)
#standardized main-effects for 15M
summary(std_model_list[["BITSEA_Comp_Total_24M"]]$main_model)
#interaction-effects for 15M
summary(model_list[["BITSEA_Comp_Total_24M"]]$int_model)
#standardized interaction-effects for 15M
summary(std_model_list[["BITSEA_Comp_Total_24M"]]$int_model)


#main-effects for 35M
summary(model_list[["BITSEA_Comp_Total_35M"]]$main_model)
#standardized main-effects for 15M
summary(std_model_list[["BITSEA_Comp_Total_35M"]]$main_model)
#interaction-effects for 15M
summary(model_list[["BITSEA_Comp_Total_35M"]]$int_model)
#standardized interaction-effects for 15M
summary(std_model_list[["BITSEA_Comp_Total_35M"]]$int_model)


