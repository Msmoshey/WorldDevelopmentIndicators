# WorldDevelopmentIndicators
The aim of this project was to investigate the relationship between WDI indicators and the severity of the COVID-19 pandemic.
## World Development Indicators 
Thsi project includes a selection of the World Development Indicators (WDI), derived from a primary World Bank database, and information about the casualties of the COVID-19 pandemic. The aim was to investigate the relationship between WDI indicators and the severity of the COVID-19 pandemic.
The analysis of this dataset required the implementation of regression, clustering and classification techniques on the aforementioned data set. The task was designed to test ability to apply regression, clustering and classification algorithms and the ability to evaluate and interpret the results.
- Using descriptive statistics (both graphical and numerical representations) the dataset was analysed and an appropriate table and graphs were generated as summary and appropriate graphs
-  Clustering algorithms (leaving out Continent, and Covid deaths) was implemented. 
-  Transformed Covid deaths into a binary variable. Fitted a logistic regression model using the
remaining variables to predict high COVID causalities. 
- Transformed Covid deaths into a categorical variable with 4 possible labels. Considered whether
some manipulation of the dataset should be implemented before applying learning algorithms. Implemented QDA, LDA and logistic regression for this multiclass classification problem and finally compared the results using appropriate validation techniques and performance metrics. 
## About Dataset 
The dataset includes the following World Bank Indicator variables
- Code: Indicator Name
- Country Name: The name of the country
- Continent: Continent to which the country belongs to
- Covid_deaths. Number of COVID deaths per 1M people as of 1st March 2022
- Life_expec: Life expectancy at birth, total (years)
- Elect_access: Access to electricity (\% of population)
- NNI Adjusted: net national income (annual \% growth)
- NNI_capita: Adjusted net national income per capita (annual \% growth)
- Mortality: Mortality rate infant (per 1,000 live births)
- Primary: Primary completion rate, total (\% of relevant age group)
- Pop_growth: Population growth (annual \%)
- Pop_densit: Population density (people per sq. km of land area)
- Pop_total: Population, total
- Health_exp_capita: Current health expenditure per capita, PPP (current international \$)
- Health_exp: Current health expenditure (\% of GDP)
- Unemployment: Unemployment, total (\% of total labor force) (national estimate)
- GDPgrowth: GDP growth (annual \%)
- GDPcapita: GDP per capita, PPP (current international \$)
- Birth_rate: Birth rate, crude (per 1,000 people)
- Water_services: People using safely managed drinking water services (\% of population)
- Education: Compulsory education, duration (years)
## Skills and Tools Used 
- Descriptive Analysis
- Correlation Matrix
- Multiclass Logistic Regression
- QDA 
- LDA 
- Kmeans Clustering 
## Libraries Used 
- Dplyr
- Cluster
- MASS
- ggplot2
- ISLR
- nnet
- BinaryLogic
- Tidyverse
