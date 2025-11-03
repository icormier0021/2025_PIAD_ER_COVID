outcome <- "BITSEA_Comp_Total_15M"
main_fml <- 
main_model <- lm(as.formula(paste0(outcome, " ~ COVID_Comp_Total + DERS_Total")), data=df)


df <- df %>% 
        mutate(
                DERS_Total_Std = scale(DERS_Total),
                COVID_Comp_Total_Std = scale(COVID_Comp_Total)
        )

ggplot(df) +
        aes(x = DERS_Total, y = BITSEA_Comp_Total_15M, size = COVID_Comp_Total) +
        geom_point() +
        scale_color_gradient() +
        labs(
                y = "BITSEA Comp Total at 15M",
                x = "DERS_Total Score",
                size = "COVID-19 Experience Comp Score"
        ) +
        theme_minimal()
outcomes <- c(
        "BITSEA_Comp_Total_15M",
        "BITSEA_Comp_Total_24M",
        "BITSEA_Comp_Total_35M"
)


for (outcome in outcomes) {
        p <- ggplot(df) +
                aes(x = DERS_Total, y = .data[[outcome]], colour = COVID_Comp_Total) +
                geom_point() +
                scale_color_gradient() +
                labs(
                        y = paste0(outcome),
                        x = "DERS_Total Score",
                        colour = "COVID-19 Experience Comp Score",
                        title = paste0("Association between COVID-19 Experiences, Total DERS, and ", outcome)
                ) +
                theme_minimal()
        print(p)
}

library(parameters)
library(performance)
install.packages("parameters")
install.packages("performance")
packageVersion("parameters")
packageVersion("performance")
model_parameters(main_model, summary = TRUE)
model_performance(main_model, summary = TRUE)

library(report)
report_text(main_model)


----
        run_bitsea_model <- function(data, outcome, standardize_predictors=FALSE) {
                data_name <- deparse(substitute(data))
                
                if (isTRUE(standardize_predictors)){
                        data <- data %>% 
                                mutate(
                                        DERS_Total       = as.numeric(scale(DERS_Total)),
                                        COVID_Comp_Total = as.numeric(scale(COVID_Comp_Total))
                                )
                }
                #Main effect model
                fml_main <- as.formula(paste0(outcome, " ~ COVID_Comp_Total + DERS_Total"))
                fml_int  <- as.formula(paste0(outcome, " ~ COVID_Comp_Total * DERS_Total"))
                
                main_model <- lm(fml_main, data=data)
                #Interaction effect model
                int_model <- lm(fml_int, data=data)
                main_model$call <- call("lm", fml_main, as.name(data_name))
                int_model$call <-  call("lm", fml_int, as.name(data_name))
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
-----


