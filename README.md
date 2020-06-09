# demand-forecasting
Demand forecasting in retail is defined as the act of predicting how much of a 
specific product or service, customers will want to purchase during a defined time 
period. This method of predictive analytics helps retailers understand how much 
stock to have on hand at a given time. Many traditional and advanced forecasting 
tools are available, but applying them to a large number of products or customers 
is not manageable. Thus, historical product demand of ABC retail custommer is 
used to cluster products with similar demand trends. Once product segments are 
identified, a representative product is choosen to illustrate the products within 
the segments. Thus a manageable number of forecasting models can be built to forecast 
the monthly demand of the customers. 

## Approach

The following methodolgies represent an initial procedure to forecast customer demand.
It follows basic concepts and models well described in the literature and already 
employed for this task. Its aim is to describe a baseline upon which more informations 
can be added, and more complex models can be developed.

### Data mining
The data found in the 'physical_network.csv' file represent 216725 observations for 40 variables, and
span over 3 years. Before forecasting, different features were selected and data were partially cleaned for analysis as follow.

* A feature (variable) is selected if only it is described and not redundant with the information contained in another feature. 
* For the purpose of data mining and analysis, some new features are created: 
  + DOW: Day of the week (numeric) 
  + Month: month of the year (numeric) 
  + Year: year (numeric)

* the minimum number of features is then selected for forecasting. The aim is to give a minimal complexity to the model and maximise computing time.
* Outliers are present in the data. However, without discussion with manufacturers or consumers, it is difficult to delete these outliers without facing the possibility of deleting important informations. Thus the choice was made not to delete them. However a script is present in the document to filter them out for future development.
* Finally, some noise were present at the start and the end of the timeseries. Not knowing if those months were fully covered, the decision is made to filter them out of the analysis, thus only retaining fully covered months. 

### Forecasting
Data are decomposed and checked for seasonality and trend patterns before applying forecasting. Depending of the characteristic of the data, different forecasting methods are applied and compared. 

* ARIMA (Autoregressive Integrated Moving Average) 
* ETS (Exponential Smoothing)  
* drift, and naïve 

The Akaike Information Critera (AIC) is used to test the goodness of fit and the 
simplicity/parcimony of the models and accuracy of predictions are calculated (RMSE, MAE, MAPE, MASE).
The best model is compared to the original data after splitting them in a train and test set. The test set was set to 4 months, given the high variability of the data. Only the model selected for each group is presented in this document, although the other models are present in the code script. 
