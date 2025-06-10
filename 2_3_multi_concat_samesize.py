# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
import ast
from simpletransformers.classification import MultiLabelClassificationModel, MultiLabelClassificationArgs
from sklearn.metrics import  f1_score, accuracy_score, coverage_error
from sklearn.model_selection import train_test_split
import torch
import datetime
import os
import gc

# Ensure a safe temporary directory
os.environ["TMPDIR"] = "C:\\Users\\rbach\\Documents\\my_temp"
os.makedirs(os.environ["TMPDIR"], exist_ok=True)

cuda_available = torch.cuda.is_available()

# Add date and time to filename
now = datetime.datetime.now()
timestamp = now.strftime("%Y%m%d_%H%M%S")
filename = f"C:\\downloads\\ruben_results\\test_results_concat_samesize_{timestamp}.csv"

# Multi-label metrics
def zero_one_loss(y_true, prob):
    y_pred = np.where(prob > threshold, 1, 0)
    nsample = len(y_true)
    row_indicators = np.logical_not(np.all(y_true == y_pred, axis=1))
    not_equal_count = np.sum(row_indicators)
    return not_equal_count / nsample

def hamming_loss(y_true, prob):
    y_pred = np.where(prob > threshold, 1, 0)
    hl_num = np.sum(np.logical_xor(y_true, y_pred))
    hl_den = np.prod(y_true.shape)
    return hl_num / hl_den

def av_labels_correct(labels, preds):
    return accuracy_score(labels, np.round(preds))

def macro_f1(y_true, probs):
    y_pred = np.where(probs > threshold, 1, 0)
    return f1_score(y_true, y_pred, average='macro')

# Load and prepare data
df = pd.read_csv(r'C:\Users\rbach\Documents\multilabel_ruben\data\all_concat.csv')
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

### select random n=553 participants, no need to account for multiple answers per respondent because one box condition
sampled_df = df.sample(n=553, replace=False, random_state=1)

for split_index in range(81, 100):
    # Create a new random split of the data
    train_df, temp_df = train_test_split(sampled_df, test_size=0.4, random_state=split_index)
    val_df, test_df = train_test_split(temp_df, test_size=0.5, random_state=split_index)
    validation_results = []

    # Initialize variables to store optimal hyperparameters
    best_lr = None
    best_epochs = None
    best_val_acc = 0.0  # Initialize with a low value
    av_acc_new = []

    for epochs in p_epochs:
        for lr in p_lr:
            # Unique output directory for each run
            output_dir = f"C:\\downloads\\ruben_results\\outputs\\split_{split_index}_ep{epochs}_lr{str(lr).replace('.', '')}"
            os.makedirs(output_dir, exist_ok=True)

            train_args = MultiLabelClassificationArgs(
                output_dir=output_dir,
                overwrite_output_dir=True,
                use_early_stopping=True,
                early_stopping_delta=0.01,
                early_stopping_metric="mcc",
                early_stopping_metric_minimize=False,
                early_stopping_patience=5,
                evaluate_during_training_steps=1000,
                manual_seed=4,
                save_steps=-1,
                save_model_every_epoch=False,
                num_train_epochs=epochs,
                learning_rate=lr
            )

            model = MultiLabelClassificationModel(
                "bert", "bert-base-german-cased",
                num_labels=nlabels,
                use_cuda=cuda_available,
                args=train_args
            )

            model.train_model(train_df)

            try:
                val_result, _, _ = model.eval_model(
                    val_df,
                    zero_one_loss=zero_one_loss,
                    hamming_loss=hamming_loss,
                    av_acc=av_labels_correct,
                    macro_f1=macro_f1
                )
            except PermissionError as e:
                print(f"Permission error: {e}")
                raise

            av_acc_new = val_result["av_acc"]

            if av_acc_new > best_val_acc:
                best_val_acc = av_acc_new
                best_lr = lr
                best_epochs = epochs

            # Clean up
            del model
            gc.collect()
            torch.cuda.empty_cache()

    # Train best model on full training data
    output_dir = f"C:\\downloads\\ruben_results\\outputs\\split_{split_index}_final"
    os.makedirs(output_dir, exist_ok=True)

    train_args = MultiLabelClassificationArgs(
        output_dir=output_dir,
        overwrite_output_dir=True,
        use_early_stopping=True,
        early_stopping_delta=0.01,
        early_stopping_metric="mcc",
        early_stopping_metric_minimize=False,
        early_stopping_patience=5,
        evaluate_during_training_steps=1000,
        manual_seed=4,
        save_steps=-1,
        save_model_every_epoch=False,
        num_train_epochs=best_epochs,
        learning_rate=best_lr
    )

    model = MultiLabelClassificationModel(
        "bert", "bert-base-german-cased",
        num_labels=nlabels,
        use_cuda=cuda_available,
        args=train_args
    )

    model.train_model(train_df)

    test_result, _, _ = model.eval_model(
        test_df,
        zero_one_loss=zero_one_loss,
        hamming_loss=hamming_loss,
        av_acc=av_labels_correct,
        macro_f1=macro_f1
    )

    test_perf.append({
        'split_index': split_index,
        'learning_rate': best_lr,
        'epochs': best_epochs,
        'accuracy_av': test_result["av_acc"],
        'zero_one_loss': test_result["zero_one_loss"],
        'hamming_loss': test_result["hamming_loss"],
        'macro_f1': test_result["macro_f1"]
    })

    # Save interim results
    pd.DataFrame(test_perf).to_csv(filename, index=False)

    # Final cleanup
    del model
    gc.collect()
    torch.cuda.empty_cache()

# Final save
pd.DataFrame(test_perf).to_csv(filename, index=False)
