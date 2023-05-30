import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.feature_selection import RFE
from sklearn.feature_selection import RFECV
from sklearn.metrics import r2_score, mean_squared_error
from scipy.stats import linregress
import matplotlib.pyplot as plt
import pandas as pd

data = pd.read_csv(r'C:\Users\mande\Documents\PDF\lidar_course\JNP\JP21_plots.csv', index_col=False)

# Assuming your data is stored in a variable called 'data'
target_column = 'avg_cbh_m'  # Replace with the actual target column name
feature_columns = data.columns[11:46]  # Select all columns starting from the fourth column as features

# Separate the features and target variable
X = data[feature_columns]
y = data[target_column]

# Create a linear regression model
regression_model = LinearRegression()

# Perform Recursive Feature Elimination (RFE)
selector = RFE(regression_model, n_features_to_select=3)  # Select the desired number of features

# Fit RFE on the data
selector.fit(X, y)

# Get the selected feature names
selected_feature_names = X.columns[selector.support_]

# Create a new DataFrame with only the selected features
selected_features = X[selected_feature_names]

# Fit the model to the selected features
regression_model.fit(selected_features, y)

# Print the coefficients and the equation
coefficients = regression_model.coef_
intercept = regression_model.intercept_
equation = f"{target_column} = {intercept:.4f} + "
for feature, coef in zip(selected_feature_names, coefficients):
    equation += f"{coef:.4f}*{feature} + "
equation = equation[:-3]  # Remove the extra ' + ' at the end
print("Equation:", equation)

# Calculate R-squared
y_pred = regression_model.predict(selected_features)
r_squared = r2_score(y, y_pred)
print("R-squared:", r_squared)

# Calculate mean squared error (MSE)
mse = mean_squared_error(y, y_pred)
print("Mean Squared Error (MSE):", mse)

# Calculate root mean squared error (RMSE)
rmse = np.sqrt(mse)
print("Root Mean Squared Error (RMSE):", rmse)

# Calculate p-values using linregress
p_values = []
for feature in selected_feature_names:
    slope, intercept, r_value, p_value, std_err = linregress(data[feature], y)
    p_values.append(p_value)

print("P-values:")
for feature, p_value in zip(selected_feature_names, p_values):
    print(f"{feature}: {p_value:.4f}")

# Calculate predicted values
y_pred = regression_model.predict(selected_features)

# Plot predicted values vs actual values with a line passing through the origin (0,0)
plt.scatter(y, y_pred, color='white', edgecolor='black')
plt.plot([0, max(max(y), max(y_pred))], [0, max(max(y), max(y_pred))], color='black')
plt.xlabel('Average CBH (m)')
plt.ylabel('Predicted CBH (m)')# kg/m$^2$

# Save the plot as a PNG file
plt.savefig(r'C:\Users\mande\Documents\PDF\lidar_course\JNP\JP_21_predicted_vs_actual_CBH.png')
plt.show()
\


