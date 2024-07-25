from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, classification_report
import matplotlib.pyplot as plt

data = load_iris()
X_train, X_test, y_train, y_test = train_test_split(
    data.data, data.target, test_size=0.2, random_state=42
)

X_train.shape[0]

param_grid = {"max_depth": [None, 1, 3, 5, 10]}
grid_search = GridSearchCV(
    RandomForestClassifier(),
    param_grid,
    cv=6,
    scoring="accuracy",
    return_train_score=True,
)
grid_search.fit(X_train, y_train)

best_clf = grid_search.best_estimator_
y_train_pred = best_clf.predict(X_train)
y_test_pred = best_clf.predict(X_test)

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
