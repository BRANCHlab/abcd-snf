from sklearn.datasets import load_wine
from sklearn.model_selection import train_test_split, GridSearchCV

# from sklearn.model_selection import train_test_split, GridSearchCV
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.metrics import accuracy_score, classification_report
# import matplotlib.pyplot as plt

wine = load_wine()

x_train, x_test, y_train, y_test = train_test_split(
    wine.data, wine.target, test_size=0.3, random_state=42
)

param_grid = {"max_depth": [None, 1, 3, 5, 10]}

grid_search = GridSearchCV(
    RandomForestClassifier(),
    param_grid,
    cv=6,
    scoring="accuracy",
    return_train_score=True,
)
grid_search.fit(x_train, y_train)

grid_search.best_estimator_

best_clf = grid_search.best_estimator_
y_train_pred = best_clf.predict(x_train)
y_test_pred = best_clf.predict(x_test)

train_accuracy = accuracy_score(y_train, y_train_pred)
test_accuracy = accuracy_score(y_test, y_test_pred)

print(f"Best Params: {grid_search.best_params_}")
print(f"Train Accuracy: {train_accuracy}")
print(f"Test Accuracy: {test_accuracy}")
print(classification_report(y_test, y_test_pred))

depths = param_grid["max_depth"]

train_scores = grid_search.cv_results_["mean_train_score"]
test_scores = grid_search.cv_results_["mean_test_score"]

plt.plot(depths, train_scores, label="Train Accuracy")
plt.plot(depths, test_scores, label="Test Accuracy")
plt.xlabel("Tree Depth")
plt.ylabel("Accuracy")
plt.legend()
plt.show()


###############################################################################


# from sklearn.datasets import load_wine
# from sklearn.model_selection import cross_val_score, train_test_split, StratifiedKFold
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.tree import DecisionTreeClassifier, plot_tree
# from sklearn.metrics import accuracy_score
#
## Load data
# wine = load_wine()
# x, y = wine.data, wine.target
#
## Split data
# x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=42)
#
# cv = StratifiedKFold(n_splits = 10, shuffle=True, random_state=42)
#
## Train classifier
# rf_clf = RandomForestClassifier()
#
# scores = cross_val_score(rf_clf, X, y, cv=cv)
#
# scores
#
# rf_clf.fit(x_train, y_train)
#
## Predict
# rf_y_pred = rf_clf.predict(x_test)
#
## Accuracy
# accuracy = accuracy_score(y_test, rf_y_pred)
# print(f"Accuracy: {accuracy:.2f}")
#
#
#
# decision_tree_clf = DecisionTreeClassifier()
#
# decision_tree_clf.fit(x_train, y_train)
#
# y_pred = decision_tree_clf.predict(x_test)
#
# accuracy_score(y_test, y_pred)
#
#
## Plot the decision tree
# plt.figure(figsize=(20,10))
# plot_tree(decision_tree_clf, filled=True, feature_names=wine.feature_names, class_names=wine.target_names)
# plt.show()
#
# print(wine.feature_names)
