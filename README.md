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
	- imputation (maybe not)

3. **Visualization**
	- Grouped? multi-level
	- Density curve (kde, histogram)
	- outlier, boxplot
	- correlation -> feature selection
	- heatmap; redundancy (constant feature)

4. Modeling
	- Logistic
	forward/backward
	- multi collinearity (VIF)