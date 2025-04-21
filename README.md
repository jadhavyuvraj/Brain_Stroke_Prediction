# 🧠 Brain Stroke Prediction Using R

This project leverages machine learning models to predict the likelihood of a brain stroke based on various patient health metrics. Built in R, the pipeline covers everything from data cleaning to model evaluation using both Logistic Regression and Random Forest techniques.

---

## 📁 Dataset

- Source: `Health Stroke.csv`
- Features include: age, gender, hypertension, heart disease, marital status, work type, residence type, average glucose level, BMI, smoking status, and stroke outcome.

---

## 🔧 Workflow

### 1. **Data Preprocessing**
- Handled missing BMI values by imputing with gender-wise mean.
- Converted categorical variables into factors.
- Visualized missing values using `naniar`.

### 2. **Exploratory Data Analysis**
- Plotted histograms of key features (`age`, `bmi`, `avg_glucose_level`) to observe trends in stroke cases.
- Analyzed stroke distribution across different demographics.

### 3. **Modeling**

#### 🧮 Logistic Regression
- Built initial GLM using relevant features.
- Optimized with stepwise selection (BIC criterion).
- Evaluated with ROC curve and McFadden's R².

#### 🌲 Random Forest
- Trained using various `mtry` values to find the best performance.
- Visualized error rates over different numbers of trees.
- Extracted feature importance.
- Applied class balancing using the `ROSE` package to address imbalance in stroke occurrences.

### 4. **Evaluation**
- Used `caret`'s `confusionMatrix` for accuracy, sensitivity, and specificity.
- Plotted ROC curves and calculated AUC scores.

---

## 📊 Results

- **Logistic Regression**: Provided interpretability and solid baseline performance.
- **Random Forest**: Achieved higher accuracy and better handling of nonlinear relationships.
- **AUC Score**: Calculated using `ROCR` to assess model performance.
- **Feature Importance**: Visualized to understand which factors contribute most to stroke prediction.

---

## 📦 Libraries Used

- `tidyverse`
- `naniar`
- `randomForest`
- `caret`
- `ROSE`
- `ROCR`

---

## 📌 Conclusion

This project demonstrates how machine learning models, coupled with thoughtful data preprocessing and balancing techniques, can effectively predict brain strokes. It highlights the importance of model tuning and evaluation using appropriate metrics.

---

## 🚀 How to Run

1. Clone the repo.
2. Make sure all listed R packages are installed.
3. Replace the dataset path if necessary in `Code.R`.
4. Run the script in your R environment.

---

## 📬 Contact

For suggestions or collaborations, feel free to reach out!
