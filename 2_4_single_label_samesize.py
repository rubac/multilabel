


# -*- coding: utf-8 -*-


# !pip install simpletransformers
# !pip freeze | grep simpletransformers
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd


import torch
cuda_available = torch.cuda.is_available()
cuda_available
from simpletransformers.classification import ClassificationModel, ClassificationArgs
from sklearn.metrics import f1_score, precision_score, recall_score, roc_auc_score, accuracy_score

def f1_multiclass(labels, preds):
    return f1_score(labels, preds, average='micro')
def convert_to_int(lst):
    return [int(x) for x in lst]
def replace_with_1(x):
    return 1 if x != 0 else x
def acc_zero_one_loss(y_true, y_pred):
    if isinstance(y_true, pd.DataFrame):
        y_true = np.array(y_true['labels'].tolist())
    if isinstance(y_pred, pd.DataFrame):
        y_pred = np.array(y_pred['labels'].tolist())
    nsample = len(y_true)
    row_indicators = np.logical_not(np.all(y_true == y_pred, axis=1))
    not_equal_count = np.sum(row_indicators)
    return 1 - (not_equal_count / nsample)
def zero_one_loss(y_true, y_pred):
    if isinstance(y_true, pd.DataFrame):
        y_true = np.array(y_true['labels'].tolist())
    if isinstance(y_pred, pd.DataFrame):
        y_pred = np.array(y_pred['labels'].tolist())
    nsample = len(y_true)
    row_indicators = np.logical_not(np.all(y_true == y_pred, axis=1))
    not_equal_count = np.sum(row_indicators)
    return not_equal_count / nsample
def hamming_loss_new(y_true, y_pred):
    hl_num = np.sum(np.logical_xor(y_true, y_pred))
    hl_den = y_true.size  # total n = #rows * #cols
    return hl_num / hl_den

# Commented out IPython magic to ensure Python compatibility.
df = pd.read_csv(r'C:\Users\rbach\Documents\multilabel_ruben\data\all_single.csv')
df = df[['text', 'new_label_1', "lfdn"]]
df['label'] = pd.factorize(df['new_label_1'])[0]
df = df.drop(columns=['new_label_1'])
df = df[['text', 'label', "lfdn"]]
df.head(5)

label_mapping = {
        0: "v_zero",
        1: "v_one",
        2: "v_two",
        3: "v_three",
        4: "v_four",
        5: "v_five",
        6: "v_six",
        7: "v_seven",
        8: "v_eight",
        9: "v_nine"
    }

threshold = .5

# Define hyperparameters
train_args = ClassificationArgs(
    use_early_stopping=True,
    early_stopping_delta=0.01,
    early_stopping_metric="mcc",
    early_stopping_metric_minimize=False,
    early_stopping_patience=5,
    evaluate_during_training_steps=1000,
    manual_seed=4,
    save_steps=-1,
    save_model_every_epoch=False,
    overwrite_output_dir=True,
    use_multiprocessing=True,
    labels_list=[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
)
# p_epochs= [1, 2] # for testing
# p_lr = [1e-3, 1e-4] # for testing

p_epochs= [5, 10, 15]
p_lr = [1e-3, 1e-4, 1e-5]
# Empty lists to store test results for all splits (best model only)
test_perf = []

# 2 random splits and run the experiment --- increase when running experiment for real
# for split_index in range(2):   for testing
for split_index in range(100):
    # Create a new random split of the data with size of multilabel data
    df = df.sample(n=977, random_state=split_index)
    # train_df, val_df, test_df = np.split(df.sample(frac=1, random_state=split_index, stratified = ), [int(.6*len(df)), int(.8*len(df))])
    train_df, temp_df = train_test_split(df, test_size=0.4, random_state=split_index, stratify=df['label'])

    # Split the temporary data into validation and test
    val_df, test_df = train_test_split(temp_df, test_size=0.5, random_state=split_index, stratify=temp_df['label'])

    validation_results = []

    # Initialize variables to store optimal hyperparameters
    best_lr = None
    best_epochs = None
    best_val_acc = 0.0  # Initialize with a low value
    acc_new = []

    # Iterate through hyperparameter combinations
    for epochs in p_epochs:
        for lr in p_lr:
            train_args.learning_rate = lr
            train_args.num_train_epochs = epochs

            # Create a ClassificationModel
            model = ClassificationModel(
            "bert", "bert-base-german-cased",
            num_labels=10,
            use_cuda=cuda_available,
            args=train_args
            )

            # Train the model
            model.train_model(train_df)


            # Evaluate the model on the validation set
            val_result, val_model_outputs, val_wrong_predictions = model.eval_model(val_df, f1=f1_multiclass, acc=accuracy_score)
            # We evaluate on average accuracy, which is no different from zero one loss (av_acc = 1 - zero_one_loss))
            if val_result["acc"] > best_val_acc:
                best_val_acc = val_result["acc"]
                best_lr = lr
                best_epochs = epochs

    # Train a new model with the best hyperparameters on the full training data
    train_args.learning_rate = best_lr
    train_args.num_train_epochs = best_epochs
    model = ClassificationModel(
            "bert", "bert-base-german-cased",
            num_labels=10,
            use_cuda=cuda_available,
            args=train_args
            )
    model.train_model(train_df)

    # Evaluate the model on the test set
    test_result, test_model_outputs, test_wrong_predictions  = model.eval_model(test_df, f1=f1_multiclass, acc=accuracy_score)
    # Append validation and test results for this split
    test_df_list = test_df['text'].astype(str).values.tolist()
    predictions, raw_outputs = model.predict(test_df_list)
    test_df["predicted_label"] = predictions
    test_df['label'] = test_df['label'].map(label_mapping)
    test_df['predicted_label'] = test_df['predicted_label'].map(label_mapping)
    wide_pred = test_df.pivot_table(index='lfdn', columns='predicted_label', values='label', aggfunc='first')
    wide_pred = wide_pred.applymap(lambda x: 1 if not pd.isna(x) else x)
    result_df = test_df.groupby('lfdn', group_keys=False)['text'].apply(lambda x: ', '.join(str(val) for val in x if not pd.isna(val))).reset_index()
    wide_pred['text'] = result_df['text']
    wide_true = test_df.pivot_table(index='lfdn', columns='label', values='predicted_label', aggfunc='first')
    wide_true = wide_true.applymap(lambda x: 1 if not pd.isna(x) else x)
    wide_true['text'] = result_df['text']
    wide_true.fillna(0, inplace=True)
    wide_pred.fillna(0, inplace=True)
    columns_true = wide_true.columns
    columns_pred = wide_pred.columns
    missing_columns = [col for col in columns_true if col not in columns_pred]
    for col in columns_true:
      if col not in columns_pred:
        wide_pred[col] = 0
    missing_columns = [col for col in columns_pred if col not in columns_true]
    for col in columns_pred:
      if col not in columns_true:
        wide_true[col] = 0

    wide_pred_list = wide_pred.drop(['text'], axis=1)
    wide_pred_list = wide_pred_list.apply(lambda x: x.values.tolist(), axis=1)
    wide_pred_list = pd.DataFrame(wide_pred_list)
    wide_pred_list.columns = ['labels']
    wide_pred_list['labels'] = wide_pred_list['labels'].apply(convert_to_int)
    wide_true = wide_true.applymap(replace_with_1)
    wide_true_list = wide_true.drop(['text'], axis=1)
    wide_true_list = wide_true_list.apply(lambda x: x.values.tolist(), axis=1)
    wide_true_list = pd.DataFrame(wide_true_list)
    wide_true_list.columns = ['labels']
    wide_true_list = np.array(wide_true_list['labels'].tolist())
    wide_pred_list = np.array(wide_pred_list['labels'].tolist())
    test_perf.append({
        'split_index': split_index,
        'learning_rate': best_lr,
        'epochs': best_epochs,
        'accuracy': acc_zero_one_loss(wide_true_list, wide_pred_list),
        'zero_one_loss': zero_one_loss(wide_true_list, wide_pred_list),
        'hamming_loss': hamming_loss_new(wide_true_list, wide_pred_list)
    })
    # At end of each split, write test results to a CSV file
    test_results_df = pd.DataFrame(test_perf)
    test_results_df.to_csv(r"C:\Users\rbach\Documents\multilabel_ruben\results\test_results_single_samesize.csv", index=False)


# At end, write test results to a CSV file
test_results_df = pd.DataFrame(test_perf)
test_results_df.to_csv(r"C:\Users\rbach\Documents\multilabel_ruben\results\test_results_single_samesize.csv", index=False)
