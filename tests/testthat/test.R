

test_that("effect_size_additive works as expected", {
  expect_equal(effect_size_additive(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                    Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                    Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                    StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                    StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                    StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                    StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                    StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                    StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                    StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                    StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                    StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                    Significance_Level = 0.05)$Interaction_Effect_Size[1], -0.4949691)

  expect_equal(effect_size_additive(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                    Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                    Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                    StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                    StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                    StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                    StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                    StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                    StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                    StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                    StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                    StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                    Significance_Level = 0.10)$Interaction_CI_Lower[1], -2.040716)


  expect_equal(effect_size_additive(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                    Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                    Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                    StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                    StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                    StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                    StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                    StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                    StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                    StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                    StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                    StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                    Significance_Level = 0.75)$Null_Model[1], "Additive")



  expect_equal(effect_size_additive(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                    Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                    Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                    StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                    StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                    StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                    StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                    StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                    StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                    StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                    StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                    StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                    Significance_Level = 0.075,
                                    Small_Sample_Correction = FALSE)$Interaction_Effect_Size[1], -0.528717)

  expect_equal(effect_size_additive(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                    Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                    Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                    StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                    StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                    StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                    StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                    StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                    StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                    StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                    StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                    StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                    Significance_Level = 0.01,
                                    Small_Sample_Correction = FALSE)$Interaction_CI_Upper[1], 2.058339)


  expect_equal(effect_size_additive(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                    Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                    Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                    StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                    StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                    StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                    StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                    StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                    StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                    StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                    StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                    StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                    Significance_Level = 0.50,
                                    Small_Sample_Correction = FALSE)$Null_Model[1], "Additive")
})

test_that("effect_size_multiplicative works as expected", {
  expect_equal(effect_size_multiplicative(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                    Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                    Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                    StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                    StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                    StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                    StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                    StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                    StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                    StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                    StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                    StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                    Significance_Level = 0.05)$Interaction_Effect_Size[1], -0.07050078)

  expect_equal(effect_size_multiplicative(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                    Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                    Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                    StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                    StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                    StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                    StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                    StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                    StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                    StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                    StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                    StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                    Significance_Level = 0.05)$Interaction_CI_Lower[1], -0.2353033)


  expect_equal(effect_size_multiplicative(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                    Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                    Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                    StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                    StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                    StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                    StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                    StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                    StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                    StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                    StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                    StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                    Significance_Level = 0.75)$Null_Model[1], "Multiplicative")
})

test_that("critical_effect_size_additive works as expected", {
  expect_equal(critical_effect_size_additive(Control_N     = 4,
                              StressorA_N   = 4,
                              StressorB_N   = 4,
                              StressorsAB_N = 4), 1.939734)

  expect_equal(critical_effect_size_additive(Control_N     = 3,
                              StressorA_N   = 3,
                              StressorB_N   = 3,
                              StressorsAB_N = 3,
                              Small_Sample_Correction = FALSE,
                              Significance_Level = 0.10), 2.016364)


  expect_equal(critical_effect_size_additive(Control_N     = 46,
                              StressorA_N   = 23,
                              StressorB_N   = 73,
                              StressorsAB_N = 4,
                              Small_Sample_Correction = TRUE,
                              Significance_Level = 0.50), 0.3850782)
})

test_that("classify_interactions works as expected", {
  expect_equal(classify_interactions(effect_size_additive(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                           Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                           Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                           StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                           StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                           StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                           StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                           StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                           StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                           StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                           StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                           StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                           Significance_Level = 0.05),
                                  assign_reversals = TRUE,
                                  remove_directionality = FALSE)$Interaction_Classification[1], "Null")

  expect_equal(classify_interactions(effect_size_additive(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                                          Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                                          Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                                          StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                                          StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                                          StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                                          StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                                          StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                                          StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                                          StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                                          StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                                          StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                                          Significance_Level = 0.5),
                                     assign_reversals = FALSE,
                                     remove_directionality = TRUE)$Interaction_Classification[1], "Null")

  expect_equal(classify_interactions(effect_size_multiplicative(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                                          Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                                          Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                                          StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                                          StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                                          StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                                          StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                                          StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                                          StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                                          StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                                          StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                                          StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                                          Significance_Level = 0.01),
                                     assign_reversals = TRUE,
                                     remove_directionality = TRUE)$Interaction_Classification[1], "Null")

  expect_equal(classify_interactions(effect_size_additive(Control_N         = multiplestressR::survival$Sample_Size_Control[1],
                                                          Control_SD        = multiplestressR::survival$Standard_Deviation_Control[1],
                                                          Control_Mean      = multiplestressR::survival$Mean_Control[1],
                                                          StressorA_N       = multiplestressR::survival$Sample_Size_Temperature[1],
                                                          StressorA_SD      = multiplestressR::survival$Standard_Deviation_Temperature[1],
                                                          StressorA_Mean    = multiplestressR::survival$Mean_Temperature[1],
                                                          StressorB_N       = multiplestressR::survival$Sample_Size_pH[1],
                                                          StressorB_SD      = multiplestressR::survival$Standard_Deviation_pH[1],
                                                          StressorB_Mean    = multiplestressR::survival$Mean_pH[1],
                                                          StressorsAB_N     = multiplestressR::survival$Sample_Size_Temperature_pH[1],
                                                          StressorsAB_SD    = multiplestressR::survival$Standard_Deviation_Temperature_pH[1],
                                                          StressorsAB_Mean  = multiplestressR::survival$Mean_Temperature_pH[1],
                                                          Significance_Level = 0.005),
                                     assign_reversals = FALSE,
                                     remove_directionality = FALSE)$Interaction_Classification[1], "Null")
})
