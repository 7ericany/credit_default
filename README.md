<object data="https://github.com/7ericany/credit_default/blob/main/poster.pdf" type="application/pdf" width="100%"> 
</object>

## Link to Google Drive 
https://drive.google.com/drive/folders/10h8mCIVqBw-NZzA0HmOvoISMbtZywqY4?usp=sharing

#### Kaggle data
https://www.kaggle.com/competitions/home-credit-default-risk/data

## TODO
1. Encode all categorical features in training data (done)
	- Details in `categorical_mapping.csv`
	- Encode missing values to -1, as a new category
	- Ordinally encode categoriacl features to 1, 2, 3, ... . Can be further encoded to one_hot
	- None missing left in categorical features
	
2. Missing value in numerical features
	- percentage: drop > threshold; non-important interpretation
	- imputation (MICE)

3. **Visualization**
	- Imbalance
	- Grouped? multi-level
	- Density curve (kde, histogram)
	- outlier, boxplot
	- Pairwise correlation -> feature selection
		- dropping might be a problem
		- topology sort
	- heatmap; redundancy (constant feature)

4. Modeling
	- Dimension reduction, factor analysis (failed KMO test)
	- Logistic, forward/backward; cross validation
	- multi collinearity (VIF)
