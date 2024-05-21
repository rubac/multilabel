
# -*- coding: utf-8 -*-


# install libraries
# !pip install simpletransformers
# !pip install scikit-learn
# !pip install torch

import pandas as pd
import numpy as np
import ast
from simpletransformers.classification import MultiLabelClassificationModel, MultiLabelClassificationArgs
from sklearn.metrics import  f1_score, accuracy_score, coverage_error
from simpletransformers.classification import ClassificationModel, ClassificationArgs
from sklearn.model_selection import train_test_split

import torch
cuda_available = torch.cuda.is_available()

# Multi-label metrics
# input: np arrays: y_true contains dummies, prob contains probabilities (or already 1 / 0 labels)

def zero_one_loss(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    row_indicators = np.logical_not(np.all(y_true == y_pred, axis = 1)) # axis = 1 will check for equality along rows.
    not_equal_count = np.sum(row_indicators)
    return not_equal_count/nsample

def hamming_loss(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    hl_num = np.sum(np.logical_xor(y_true, y_pred))
    hl_den = np.prod(y_true.shape)  # total n= #rows * #cols
    return hl_num/hl_den

# count number of predictions without any label (all below threshold)
def fraction_zero_label(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    my_nlabels =  np.sum(y_pred,axis=1)
    fraction = 1- (np.count_nonzero(my_nlabels) / nsample)
    return fraction

# count number of predictions with 1 label

def fraction_one_label(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    my_nlabels =  np.sum(y_pred,axis=1)
    np.count_nonzero((my_nlabels==1))
    # counts the number of non-zero labels, but the only non zero number is 1
    fraction = np.count_nonzero((my_nlabels==1)) / nsample
    return fraction

# count number of predictions with 2 labels
def fraction_two_label(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    my_nlabels =  np.sum(y_pred,axis=1)
    np.count_nonzero((my_nlabels==2))
    # counts the number of non-zero labels, but the only non zero number is 1
    fraction = np.count_nonzero((my_nlabels==2)) / nsample
    return fraction

# count number of predictions with 3 labels
def fraction_three_label(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    my_nlabels =  np.sum(y_pred,axis=1)
    np.count_nonzero((my_nlabels==2))
    # counts the number of non-zero labels, but the only non zero number is 1
    fraction = np.count_nonzero((my_nlabels==3)) / nsample
    return fraction

    from sklearn.metrics import  accuracy_score

def av_labels_correct(labels, preds):
    return accuracy_score(labels, np.round(preds))

df = pd.read_csv(r'C:\Users\rbach\Documents\multilabel_ruben\all_concat.csv')
selected_columns = df.iloc[:, 2:12]

labels = selected_columns.values.tolist()
df['labels'] = labels

nlabels=10
threshold=0.5
train_args=MultiLabelClassificationArgs(
    use_early_stopping = True,
    early_stopping_delta = 0.01,
    early_stopping_metric = "mcc",
    early_stopping_metric_minimize = False,
    early_stopping_patience = 5,
    evaluate_during_training_steps = 1000,
    manual_seed = 4,
    save_steps = -1,
    save_model_every_epoch = False,
    overwrite_output_dir= True
    )

# p_epochs= [1,2] for testing
# p_lr = [1e-3, 1e-4] for testing

p_epochs= [5, 10, 15]

p_lr = [1e-3, 1e-4, 1e-5]

# Empty lists to store test results for all splits (best model only)
test_perf = []

# 2 random splits and run the experiment --- increase when running experiment for real
# for split_index in range(2):   for testing
for split_index in range(2):
    # Create a new random split of the data
    train_df, val_df, test_df = np.split(df.sample(frac=1, random_state=split_index), [int(.6*len(df)), int(.8*len(df))])
    validation_results = []

    # Initialize variables to store optimal hyperparameters
    best_lr = None
    best_epochs = None
    best_val_acc = 0.0  # Initialize with a low value
    av_acc_new = []

    # Iterate through hyperparameter combinations
    for epochs in p_epochs:
        for lr in p_lr:
            train_args.learning_rate = lr
            train_args.num_train_epochs = epochs

            model = MultiLabelClassificationModel(
                "bert", "bert-base-german-cased",
                num_labels=nlabels,
                use_cuda=cuda_available,
                args=train_args
            )

            model.train_model(train_df)

            # Evaluate the model on the validation set
            val_result, val_model_outputs, val_wrong_predictions = model.eval_model(val_df, zero_one_loss=zero_one_loss, hamming_loss=hamming_loss, av_acc=av_labels_correct)
            av_acc_new = val_result["av_acc"]
            # Check if this combination is better than the previous best
            # We evaluate on average accuracy, which is no different from zero one loss (av_acc = 1 - zero_one_loss))
            if av_acc_new > best_val_acc:
                best_val_acc = av_acc_new
                best_lr = lr
                best_epochs = epochs

    # Train a new model with the best hyperparameters on the full training data
    train_args.learning_rate = best_lr
    train_args.num_train_epochs = best_epochs
    model = MultiLabelClassificationModel(
        "bert", "bert-base-german-cased",
        num_labels=nlabels,
        use_cuda=cuda_available,
        args=train_args
    )
    model.train_model(train_df)

    # Evaluate the model on the test set
    test_result, test_model_outputs, test_wrong_predictions  = model.eval_model(test_df, zero_one_loss=zero_one_loss, hamming_loss=hamming_loss, av_acc=av_labels_correct)
    # Append validation and test results for this split

    test_perf.append({
        'split_index': split_index,
        'learning_rate': best_lr,
        'epochs': best_epochs,
        'accuracy': test_result["av_acc"],
        'zero_one_loss': test_result["zero_one_loss"],
        'hamming_loss': test_result["hamming_loss"]
    })
    print(f"Split {split_index}: Finished with lr={best_lr} and epochs={best_epochs}")
    print(test_perf)

# At end, write test results to a CSV file
test_results_df = pd.DataFrame(test_perf)
test_results_df.to_csv(r"C:\Users\rbach\Documents\multilabel_ruben\test_results.csv", index=False)




# -*- coding: utf-8 -*-
"""Untitled1.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1z_oqp-UnPVIhDtZGuQ53v9BUxsxwyupn
"""

# install libraries
# !pip install simpletransformers
# !pip install scikit-learn
# !pip install torch

import pandas as pd
import numpy as np
import ast
from simpletransformers.classification import MultiLabelClassificationModel, MultiLabelClassificationArgs
from sklearn.metrics import  f1_score, accuracy_score, coverage_error
from simpletransformers.classification import ClassificationModel, ClassificationArgs
from sklearn.model_selection import train_test_split

# from google.colab import drive
# drive.mount('/content/drive')

# %cd /content/drive/My Drive/Colab Notebooks/multilabel/
import torch
cuda_available = torch.cuda.is_available()

# Multi-label metrics
# input: np arrays: y_true contains dummies, prob contains probabilities (or already 1 / 0 labels)

def zero_one_loss(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    row_indicators = np.logical_not(np.all(y_true == y_pred, axis = 1)) # axis = 1 will check for equality along rows.
    not_equal_count = np.sum(row_indicators)
    return not_equal_count/nsample

def hamming_loss(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    hl_num = np.sum(np.logical_xor(y_true, y_pred))
    hl_den = np.prod(y_true.shape)  # total n= #rows * #cols
    return hl_num/hl_den

# count number of predictions without any label (all below threshold)
def fraction_zero_label(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    my_nlabels =  np.sum(y_pred,axis=1)
    fraction = 1- (np.count_nonzero(my_nlabels) / nsample)
    return fraction

# count number of predictions with 1 label

def fraction_one_label(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    my_nlabels =  np.sum(y_pred,axis=1)
    np.count_nonzero((my_nlabels==1))
    # counts the number of non-zero labels, but the only non zero number is 1
    fraction = np.count_nonzero((my_nlabels==1)) / nsample
    return fraction

# count number of predictions with 2 labels
def fraction_two_label(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    my_nlabels =  np.sum(y_pred,axis=1)
    np.count_nonzero((my_nlabels==2))
    # counts the number of non-zero labels, but the only non zero number is 1
    fraction = np.count_nonzero((my_nlabels==2)) / nsample
    return fraction

# count number of predictions with 3 labels
def fraction_three_label(y_true, prob):
    y_pred=np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    my_nlabels =  np.sum(y_pred,axis=1)
    np.count_nonzero((my_nlabels==2))
    # counts the number of non-zero labels, but the only non zero number is 1
    fraction = np.count_nonzero((my_nlabels==3)) / nsample
    return fraction

    from sklearn.metrics import  accuracy_score

def av_labels_correct(labels, preds):
    return accuracy_score(labels, np.round(preds))

df = pd.read_csv(r'C:\Users\rbach\Documents\multilabel_ruben\happy_onebox.csv')
selected_columns = df.iloc[:, 2:12]

labels = selected_columns.values.tolist()
df['labels'] = labels

nlabels=10
threshold=0.5
train_args=MultiLabelClassificationArgs(
    use_early_stopping = True,
    early_stopping_delta = 0.01,
    early_stopping_metric = "mcc",
    early_stopping_metric_minimize = False,
    early_stopping_patience = 5,
    evaluate_during_training_steps = 1000,
    manual_seed = 4,
    save_steps = -1,
    save_model_every_epoch = False,
    overwrite_output_dir= True
    )

# p_epochs= [1,2] for testing
# p_lr = [1e-3, 1e-4] for testing

p_epochs= [5, 10, 15]

p_lr = [1e-3, 1e-4, 1e-5]

# Empty lists to store test results for all splits (best model only)
test_perf = []

# 2 random splits and run the experiment --- increase when running experiment for real
# for split_index in range(2):   for testing
for split_index in range(2):
    # Create a new random split of the data
    train_df, val_df, test_df = np.split(df.sample(frac=1, random_state=split_index), [int(.6*len(df)), int(.8*len(df))])
    validation_results = []

    # Initialize variables to store optimal hyperparameters
    best_lr = None
    best_epochs = None
    best_val_acc = 0.0  # Initialize with a low value
    av_acc_new = []

    # Iterate through hyperparameter combinations
    for epochs in p_epochs:
        for lr in p_lr:
            train_args.learning_rate = lr
            train_args.num_train_epochs = epochs

            model = MultiLabelClassificationModel(
                "bert", "bert-base-german-cased",
                num_labels=nlabels,
                use_cuda=cuda_available,
                args=train_args
            )

            model.train_model(train_df)

            # Evaluate the model on the validation set
            val_result, val_model_outputs, val_wrong_predictions = model.eval_model(val_df, zero_one_loss=zero_one_loss, hamming_loss=hamming_loss, av_acc=av_labels_correct)
            av_acc_new = val_result["av_acc"]
            # Check if this combination is better than the previous best
            # We evaluate on average accuracy, which is no different from zero one loss (av_acc = 1 - zero_one_loss))
            if av_acc_new > best_val_acc:
                best_val_acc = av_acc_new
                best_lr = lr
                best_epochs = epochs

    # Train a new model with the best hyperparameters on the full training data
    train_args.learning_rate = best_lr
    train_args.num_train_epochs = best_epochs
    model = MultiLabelClassificationModel(
        "bert", "bert-base-german-cased",
        num_labels=nlabels,
        use_cuda=cuda_available,
        args=train_args
    )
    model.train_model(train_df)

    # Evaluate the model on the test set
    test_result, test_model_outputs, test_wrong_predictions  = model.eval_model(test_df, zero_one_loss=zero_one_loss, hamming_loss=hamming_loss, av_acc=av_labels_correct)
    # Append validation and test results for this split

    test_perf.append({
        'split_index': split_index,
        'learning_rate': best_lr,
        'epochs': best_epochs,
        'accuracy': test_result["av_acc"],
        'zero_one_loss': test_result["zero_one_loss"],
        'hamming_loss': test_result["hamming_loss"]
    })
    print(f"Split {split_index}: Finished with lr={best_lr} and epochs={best_epochs}")
    print(test_perf)
    
    # At end of each split, write test results to a CSV file
    test_results_df = pd.DataFrame(test_perf)
    test_results_df.to_csv(r"C:\Users\rbach\Documents\multilabel_ruben\results\test_results_multi.csv", index=False)

# Final write to ensure all results are saved
test_results_df = pd.DataFrame(test_perf)
test_results_df.to_csv(r"C:\Users\rbach\Documents\multilabel_ruben\results\test_results_multi.csv", index=False)
