# -*- coding: utf-8 -*-


# !pip install simpletransformers
# !pip freeze | grep simpletransformers
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
from simpletransformers.classification import MultiLabelClassificationModel, MultiLabelClassificationArgs
from sklearn.metrics import f1_score, precision_score, recall_score, roc_auc_score, accuracy_score


import torch
cuda_available = torch.cuda.is_available()
cuda_available

import datetime

# add date and time to name of csv to avoid overwriting csvs
now = datetime.datetime.now()
timestamp = now.strftime("%Y%m%d_%H%M%S")
filename = f"C:\\downloads\\ruben_results\\test_results_concat_fivebox_{timestamp}.csv"

def f1_multiclass(labels, preds):
    return f1_score(labels, preds, average='micro')
def convert_to_int(lst):
    return [int(x) for x in lst]
def replace_with_1(x):
    return 1 if x != 0 else x
def av_labels_correct(labels, preds):
    return accuracy_score(labels, np.round(preds))
# def acc_zero_one_loss(y_true, y_pred):
#     if isinstance(y_true, pd.DataFrame):
#        y_true = np.array(y_true['labels'].tolist())
#     if isinstance(y_pred, pd.DataFrame):
#         y_pred = np.array(y_pred['labels'].tolist())
#     nsample = len(y_true)
#     row_indicators = np.logical_not(np.all(y_true == y_pred, axis=1))
#     not_equal_count = np.sum(row_indicators)
#     return 1 - (not_equal_count / nsample)
def zero_one_loss(labels, preds):
    return 1- accuracy_score(labels, np.round(preds))

# def zero_one_loss(y_true, y_pred):
#     if isinstance(y_true, pd.DataFrame):
#         y_true = np.array(y_true['labels'].tolist())
#     if isinstance(y_pred, pd.DataFrame):
#         y_pred = np.array(y_pred['labels'].tolist())
#     nsample = len(y_true)
#     row_indicators = np.logical_not(np.all(y_true == y_pred, axis=1))
#     not_equal_count = np.sum(row_indicators)
#     return not_equal_count / nsample
def hamming_loss(y_true, y_pred):
    y_true = np.array(y_true)
    y_pred = np.array(y_pred)
    hl_num = np.sum(np.logical_xor(y_true, y_pred))
    hl_den = y_true.size  # total n = #rows * #cols
    return hl_num / hl_den
df = pd.read_csv(r'C:\Users\rbach\Documents\multilabel_ruben\data\all_concat.csv')
df.head()

df = df[df['exp_cond'] == 'five box']
df = df.drop(columns=['exp_cond'])

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

p_epochs= [5, 10, 15]

p_lr = [1e-3, 1e-4, 1e-5]

# Empty lists to store test results for all splits (best model only)
test_perf = []

for split_index in range(100):
    # Create a new random split of the data
    train_df, temp_df = train_test_split(df, test_size=0.4, random_state=split_index)
    val_df, test_df = train_test_split(temp_df, test_size=0.5, random_state=split_index)
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
            print(av_acc_new)
            # Check if this combination is better than the previous best
            # We evaluate on average accuracy, which is no different from zero one loss (av_acc = 1 - zero_one_loss))
            if av_acc_new > best_val_acc:
                best_val_acc = av_acc_new
                best_lr = lr
                best_epochs = epochs
            print(best_val_acc)
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
    test_results_df.to_csv(filename, index=False)

# Final write to ensure all results are saved
test_results_df = pd.DataFrame(test_perf)
test_results_df.to_csv(filename, index=False)
