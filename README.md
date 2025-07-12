# final_exam_ox

# Summative Assignment

**Due 21 Jul by 5:59**  
**Points 0**  
**Available 4 Jul at 6:00 - 21 Jul at 5:59**

---

### Course: R Programming for Data Science: Introduction

Please complete the following questions to test your understanding of the concepts covered in the "R Programming for Data Science: Introduction" course. Please submit your code/answers for each question after the question itself. Feel free to refer to the course material and conduct additional research as needed.

---

## Overview:

You are going to use a dataset that was collected and made freely available by the "National Institute of Diabetes and Digestive and Kidney Diseases" as part of the Pima Indians Diabetes Database. The dataset is available to download as part of this assignment via this link:

https://drive.google.com/file/d/1qDkGULtjrXw-H4OPXgPZH6PnVcPBHzYK/view?usp=sharing

The dataset contains the following variables (i.e. columns):

- Pregnancies: Number of times pregnant  
- Glucose: Plasma glucose concentration over 2 hours in an oral glucose tolerance test  
- BloodPressure: Diastolic blood pressure (mm Hg)  
- SkinThickness: Triceps skin fold thickness (mm)  
- Insulin: 2-Hour serum insulin (mu U/ml)  
- BMI: Body mass index (weight in kg/(height in m)2)  
- DiabetesPedigreeFunction: Diabetes pedigree function (a function which scores likelihood of diabetes based on family history)  
- Age: Age (years)  
- Outcome: 0 if non-diabetic, 1 if diabetic  

---

## Answer the following questions by writing and providing the required R code to perform the listed tasks (Please make sure you follow best coding practices explained throughout the course. Also, feel free to perform intermediary steps if you think they are needed):

1- Load the dataset, generate a numeric statistical summary of all its variables.

2- The dataset contains missing values. Sometimes those values are blank and some other times a question mark has been used to indicate them. If any variable has more than 20% of its values missing, drop that variable. 

3- For the remaining variables, Replace all missing values in each variable by the minimum value in that variable.

Now the dataset should be full without any missing values.

4- Some variables will contain 0 values but those values don’t make sense. In this dataset, 0 values only make sense in the Pregnancies and Outcome columns. Drop any row that contains one or more 0’s in any column(s) other than Pregnancies and Outcome.

5- Group the data based on the Outcome column and generate the mean and median values for all other columns.

6- Generate a heatmap plot that shows the correlation values between all variables in the dataset.

7- Generate a scatter plot showing Glucose in the x-axis and BMI in the y-axis. Colourise the markers by Outcome (e.g. markers representing diabetic individuals should be red and markers representing non-diabetic individuals should be blue).

8- Create a new column in the dataset to categorise BMI into groups. For example, you can create three categories: "Underweight" for BMI < 23, "Normal Weight" for BMI between 23 to 29 inclusive, and "Overweight" for BMI > 29. You can choose your own range of values if you wish but make sure you avoid weight value overlapping and ambiguity.

9- Group the data by BMI category and calculate the percentage of individuals with diabetes within each category. Your code should display the BMI categories and the corresponding percentage of individuals with diabetes.

10- Generate a scatter matrix plot showing all variables vs each other with histograms on the diagonal. Feel free to use existing open-source R packages.

---

### Tip(s):
write comments to explain your code and avoid hard-coding values (i.e. make sure your code is as generic as possible).

---

## Submission Guidelines:

- Submission deadline is July 20th, 2025.  
- Document your R code in a well-structured report. We recommend you answer the questions in order.  
- Make sure you submit either a Word Document or a PDF file.  
- We will run your code, so make sure the code is plain-text and not a screenshot (you can include screenshots of the results).  
- Make sure you include your Declaration of Authorship form.  
- Submit your report by emailing it to (Please send your report as an attachment): weeklyclasses@conted.ox.ac.uk  

---

### Note:
Feel free to explore additional resources, documentation, and online tutorials to enhance your understanding and complete the tasks effectively.

**All the best with your assignment!**
