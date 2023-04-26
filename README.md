# Rising CO2 level and the fecundity of forest tree
[The data was collected  from the experiment conducted by LaDeau and Clark (2001) in Duke Forest, in the Piedmont region of North Carolina.]


## Introduction
The researchers observed the correlation between pine’s stem size and number of cones. Besides, pines can have different performance when the environment consists of different atmosphere. This report presents an model analysis which assesses 2 different predicted models to estimate the numbers of cones by diameter of pine trees. Moreover, to test whether two atmosphere treatments (AMB & CO2) can enhance the prediction models without considering treatment effect into the cones production prediction models. 


## Methods
The data was collected  from the experiment conducted by LaDeau and Clark (2001) in Duke Forest, in the Piedmont region of North Carolina. The treatment is releasing CO2 from the vertical pipes, about 14 m height, to keep the atmosphere 200 𝝁l/liter CO2 higher than ambient group (AMB). To estimate cones production by pine size, generalized linear model (GLM) using a Poisson error distribution and a log-link-function (model 1): 

conesi ~ Pois (λi)
log(λi) = β0 + β1 ∙ log(diami)

Where λi presents the prediction of number of cones from the i-th tree; diami presents the diameter of the treei; β0 presents the intercept and β1 presents the marginal effect of log of diami. The prediction of exponential numbers of cones (λi) is obtained by the negative log-likelihood of a Poisson distribution.

However, LaDeau and Clark (2001) suggested the correlation between number of cones and tree diameter is direct square relationship, therefore, an alternative model is below (model2):

conesi ~ Pois (λi)
λi = β ∙ diami2

Where λi presents the prediction of number of cones from the i-th tree; diami presents the diameter of the i-th tree; β presents the marginal effect of diami square. The prediction of numbers of cones (λi) is obtained by the negative log-likelihood of a Poisson distribution.

The data analysis was conduct by R version 1.4.1717. At first, to conduct the two basic models without concerning treatment effect (model 1 & 2), the negative log-likelihood function was used. Next steps, using maximum-likelihood function to get the values of parameters. After building 2 different basic models for the same dataset, I built the models which input the different treatments (CO2 and AMB) as a prediction variable to predict cone production (model 1.1 & 2.1). Maximum-likelihood was again estimated the all parameter values, β0, β1, β2, Tmt (AMB) & Tmt (CO2).

Afterwards, the models were selected by Akaike’s Information Criterion (AIC). The four models had been compared each other and the one with the lowest AIC was selected as the best model in this analysis process.

At the end, the uncertainty of all parameters in the selected best model, which was selected by AIC, were estimated by bootstrapping resampled method, 95% of confidence interval and standard error. Therefore, we can conclude the best predicted model for the data of pine diameter and number of cones. 





## Conclusion
According to our analysis result, the prediction model performed better when the treatment effect is considered as a part of model than without considerimg it. Besides, model 1 can provide a better prediction of cone production than model 2, which means the relationship between tree sizes and numbers of cones can be explained better by logarithm than square. Therefore, we can conclude that larger pine tree can produce more cones during the pine production period. Moreover, we got the same result as LaDeau & Clark (2001), when CO2 concentration is higher in the atmosphere, forest trees can produce more cones than the atmosphere is normally ambient CO2 concentration. 

## Reference
LaDeau, S. L, & Clark, J. S. (2001). Raising CO2 levels and the fecundity of forest tree. Science 292, 95; DOI: 10.1126/science.1057547 

