# Evaluation of xG Models in Football
**Data from:** Understat

[Report published in Pages](https://mat126.github.io/Evaluation-of-xG-Models-in-Football/)

In this project, I explored various statistical models to evaluate expected goals (xG) in football, aiming to identify the most suitable model. The evaluation was performed by considering **Open Play** and **Set Piece** actions separately, further dividing the data between **Foot** and **Head** shots.

## Models used:
- **Logit** (logistic regression)
- **Logit with quadratic interactions**
- **Random Forest**
- **Bagging**
- **Neural Network (Neural Net)**

## Methodology
There is no "perfect model" that works in all situations, and the choice of the best model depends on the context and specific goals. To assist in the evaluation, I have included both **ROC Curve** and **Calibration Plot** for each model in the `Final Results` folder, alongside the xG data calculated by Understat used as a reference.
