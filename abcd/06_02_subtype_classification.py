"""
The purpose of this section is to construct a classifier that can be used to
assign subtype labels to new patients without needing to re-cluster the data.

Reference papers that have done this or a similar procedure are listed below:
- https://jamanetwork.com/journals/jamanetworkopen/article-abstract/2814991

The methods in the paper are described as:

- Informed data reduction was performed by a feature subset selection
algorithm, recursive feature elimination, which is a stepwise feature
elimination method that finds the optimal set of features for a given
classification function.
- The algorithm started with the full set of 471 features and iteratively
removed the least relevant.
- Recursive feature elimination was also run on a 100 multistart setup. Each
run performed a random 5 times, 5-fold, stratified CV
- SVM and logistic regression used to guide the search process
- A final classification algorithm was derived over training data using the
feature subset with highest classification accuracy.
- This classifier logistic regression + L2-norm regularization and balanced
class learning through adjusting weights inversely proportional to class
frequencies in the data.

"""

import pandas as pd
import numpy as np
import datetime
from sklearn import svm, linear_model as lm
from sklearn.metrics import classification_report, confusion_matrix
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.tree import DecisionTreeClassifier
from matplotlib import pyplot as plt


# Helper functions{{{
# FUNCTION: path_maker{{{
def path_maker(
    path_dir, date=False, root_dir="/home/prashanth/Documents/research/"
):
    """
    Function-maker for formatting absolute path strings from filenames
    Args:
        - path_dir (str): The target directory to append as a prefix, starting
        from the root directory.
        - date (bool): If True, prefixes filename with today's date.
        - root_dir (str): The project root directory and starting point of all
        absolute paths.
    Returns:
        - full_path (str): An absolute path pointing for that file in the
        processed data directory.
    """

    def path_fn(filename, date=False):
        if date:
            current_date = datetime.datetime.now().strftime("%Y_%m_%d")
            filename = current_date + "_" + filename
        full_path = root_dir + path_dir + filename
        return full_path
    return path_fn
# }}}
# }}}

# Importing data{{{
proc_path = path_maker("data/abcd/results/processed/")  # processed data
sub_path = path_maker("data/abcd/subjects/")  # subject lists

data = pd.read_csv(proc_path("2024_09_25_mtbi_classification_df.csv"))
# }}}

# Inspecting data{{{
data.head()

data.info()  # like dplyr::glimpse()
# }}}

# Assigning train/test/val splits{{{
"""
Note that `id()` can be used to confirm whether or not two variables have the
same memory addresses.
"""

subjects = data["subjectkey"].copy()

print(id(data["subjectkey"]))
print(id(subjects))

# Train: 80%, Test: 20%
train_subjects, test_subjects = train_test_split(
    subjects, test_size=0.2, random_state=43
)

# Outputting the results of the splits
print("Training Subjects:", train_subjects)
print("Test Subjects:", test_subjects)

# Exporting split subjects
train_subjects.to_csv(sub_path("classifier_train_subs.csv", True), index=False)
val_subjects.to_csv(sub_path("classifier_val_subs.csv", True), index=False)
test_subjects.to_csv(sub_path("classifier_test_subs.csv", True), index=False)

# Re-importing split subjects
train_subjects = pd.read_csv(sub_path("2024_09_25_classifier_train_subs.csv"))
val_subjects = pd.read_csv(sub_path("2024_09_25_classifier_val_subs.csv"))
test_subjects = pd.read_csv(sub_path("2024_09_25_classifier_test_subs.csv"))
# }}}

# Cleaning (dummying & splitting) data
data_clean = pd.get_dummies(data, columns=["race"], drop_first=True)

train_idx = data_clean["subjectkey"].isin(train_subjects["subjectkey"])
val_idx = data_clean["subjectkey"].isin(val_subjects["subjectkey"])
test_idx = data_clean["subjectkey"].isin(test_subjects["subjectkey"])

train_data = data_clean[train_idx]
val_data = data_clean[val_idx]
test_data = data_clean[test_idx]

train_data.to_csv(proc_path("classifier_train_data.csv", True), index=False)
val_data.to_csv(proc_path("classifier_val_data.csv", True), index=False)
test_data.to_csv(proc_path("classifier_test_data.csv", True), index=False)

train_data = pd.read_csv(proc_path("2024_09_25_classifier_train_data.csv"))
val_data = pd.read_csv(proc_path("2024_09_25_classifier_val_data.csv"))
test_data = pd.read_csv(proc_path("2024_09_25_classifier_test_data.csv"))

train_X = train_data.drop(columns=["subjectkey", "cluster"])
train_y = train_data["cluster"]

val_X = val_data.drop(columns=["subjectkey", "cluster"])
val_y = val_data["cluster"]

# test_X = test_data.drop(columns = ['subjectkey', 'cluster'])
# test_y = test_data['cluster']

type(train_X)

mdl_svm = svm.SVC()
mdl_rf = RandomForestClassifier()
mdl_dt = DecisionTreeClassifier()

mdl_svm.fit(train_X, train_y)
mdl_rf.fit(train_X, train_y)
mdl_dt.fit(train_X, train_y)

svm_pred_y = mdl_svm.predict(val_X)
rf_pred_y = mdl_rf.predict(val_X)
dt_pred_y = mdl_dt.predict(val_X)


# param_grid = {'C': [0.1, 1, 10, 100], 'gamma': [1, 0.1, 0.01, 0.001], 'kernel': ['rbf']}
# grid = GridSearchCV(svm.SVC(), param_grid, refit=True, verbose=2)
# grid.fit(train_X, train_y)
#
# print(grid.best_params_)


print(classification_report(val_y, svm_pred_y))

print(classification_report(val_y, dt_pred_y))

print(classification_report(val_y, rf_pred_y))


print(confusion_matrix(val_y, svm_pred_y))
print(confusion_matrix(val_y, rf_pred_y))

## Plotting the results
# plt.scatter(X[:, 0], X[:, 1], c=y, s=30, cmap="autumn")
# plt.scatter(X_test[:, 0], X_test[:, 1], c="blue", s=50, marker="x")
# plt.title("SVM Classification on Toy Dataset")
# plt.xlabel("Feature 1")
# plt.ylabel("Feature 2")
# plt.show()


from sklearn.svm import SVC
from sklearn.model_selection import StratifiedKFold
from sklearn.feature_selection import RFECV

svc = SVC(kernel="linear")
rfecv = RFECV(
    estimator=mdl_rf, step=1, cv=StratifiedKFold(5), scoring="accuracy"
)

rfecv.fit(train_X, train_y)

print("Optimal number of features : %d" % rfecv.n_features_)

rfecv.support_

train_X.iloc[:, rfecv.support_].columns

train_X.columns


features = np.array(train_X)[:, rfecv.support_]


# Plot number of features VS. cross-validation scores
plt.figure()
plt.xlabel("Number of features selected")
plt.ylabel("Cross validation score (nb of correct classifications)")
plt.plot(
    range(1, len(rfecv.cv_results_["mean_test_score"]) + 1),
    rfecv.cv_results_["mean_test_score"],
)
plt.show()
