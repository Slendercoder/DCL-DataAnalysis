source("Model_Plots.R")

###############################################

True_model_color = "#E69F00"
Recovered_model_color = "#009E73"
Corrected_and_recovered_model_color = "#F0E442"

model_to_recover = "../Python Codes/Simulations/M5_full.csv"
model_recovered = "../Python Codes/Model_recovery/M5_recovered.csv"
model_corrected_and_recovered = "../Python Codes/Model_recovery/M5_corrected_and_recovered.csv"
  
plot_ModelRecovery_Dyad(model_to_recover, 
                        model_recovered, 
                        True_model_color, 
                        Recovered_model_color)

plot_Model_correction_and_recovery_Dyad(model_to_recover, 
                                        model_recovered, 
                                        model_corrected_and_recovered, 
                                        True_model_color, 
                                        Recovered_model_color,
                                        Corrected_and_recovered_model_color)
