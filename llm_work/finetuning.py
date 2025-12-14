"""
This file does the finetuning work necessary for the classification of
Yelp reviews that do or do not mention that the place they are for offers
live music.

NOTE: Everything here is highly based on Stephen Hansen's GitHub tutorial
at https://github.com/sekhansen/columbia_lectures_2025/blob/main/code/03_classification_bert.ipynb
and his paper REMOTE WORK ACROSS JOBS, COMPANIES, AND SPACE (Hansen et al.)
"""

import pandas as pd
import numpy as np
import torch
import random

from transformers import AutoModel, BertModel, AutoTokenizer, AutoModelForSequenceClassification, pipeline, TrainingArguments, Trainer, utils
from transformers.pipelines.base import KeyDataset
from datasets import load_dataset, Dataset, DatasetDict
import evaluate

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import cross_val_score, train_test_split
from sklearn.metrics import f1_score, precision_score, recall_score, accuracy_score

# ============== SETUP ==============
# Set seeds for reproducibility
def set_seed(seed=42):
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

# Set seed at the beginning
set_seed(42)

# setup running on GPU
device = torch.device("cuda" if torch.cuda.is_available() else "mps" if torch.backends.mps.is_available() else "cpu")
print(f"Using device: {device}")
print(f"CUDA available: {torch.cuda.is_available()}")
if torch.cuda.is_available():
    print(f"CUDA device count: {torch.cuda.device_count()}")
    print(f"CUDA device name: {torch.cuda.get_device_name(0)}")
elif torch.backends.mps.is_available():
    print("Using Apple Metal Performance Shaders (MPS)")

# ============== load data ==============
df = pd.read_csv("./finetuning_reviews.csv", encoding='utf-8')

# ============== MODEL ==============
# uses cased to distinguish proper nouns
model_name = "distilbert-base-cased"

tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModel.from_pretrained(model_name)

# put model in evaluation model (we will not do any training)
model = model.eval()

# ============== FORMATTING ==============
# get labels
labels = df[["live_music"]]

# create list with all the indexes of available sentences
sent_idxs = list(range(0, len(labels)))

# perform a train/test split
train_idxs, test_idxs = train_test_split(sent_idxs, test_size=0.2, random_state=92)
print(f" Train sentences: {len(train_idxs)}\n", f"Test sentences: {len(test_idxs)}")

# ============== format train data ===================
df_finetune = df.loc[train_idxs].copy()

df_finetune = df_finetune[["sentences", "live_music"]]
df_finetune.columns = ["sentences", "label"]

# labels to ints
df_finetune["label"] = df_finetune["label"].astype(int)

# ============== format test data ==============
df_test = df.loc[test_idxs].copy()

df_test = df_test[["sentences", "live_music"]]
df_test.columns = ["sentences", "label"]

# transform labels into integers
df_test["label"] = df_test["label"].astype(int)

# ============== FINETUNING ==============
# transform data into Dataset class
finetune_dataset = Dataset.from_pandas(df_finetune)
test_dataset = Dataset.from_pandas(df_test)


# tokenize the dataset
def tokenize_function(examples):
    return tokenizer(examples["text"], max_length=512, padding="max_length", truncation=True)

# batched=True is key for training
tokenized_ft = finetune_dataset.map(tokenize_function, batched=True)
tokenized_test = test_dataset.map(tokenize_function, batched=True)

# load the model for finetunning. 
model_ft = AutoModelForSequenceClassification.from_pretrained(
    model_name,
    num_labels = 2
)


training_args = TrainingArguments(
    output_dir="./",                # path to save model
    learning_rate=5e-5,             # small learning rates
    num_train_epochs=2,             # number of finetuning passes
    per_device_train_batch_size=8,  # batch size per GPU
    per_device_eval_batch_size=8,   # batch size per GPU
    eval_strategy="epoch",          # See metrics during training
    save_strategy="no",             # Don't save checkpoints
    report_to="none",               # Don't report model estimation externally
)


# model performance metrics

metric_precision = evaluate.load("precision")
metric_recall = evaluate.load("recall")
metric_f1 = evaluate.load("f1")
metric_accuracy = evaluate.load("accuracy")

def compute_metrics(eval_pred):
    logits, labels = eval_pred
    predictions = np.argmax(logits, axis=-1)

    precision = metric_precision.compute(predictions=predictions, references=labels, average="micro")["precision"]
    recall = metric_recall.compute(predictions=predictions, references=labels, average="micro")["recall"]
    f1 = metric_f1.compute(predictions=predictions, references=labels, average="micro")["f1"]
    accuracy = metric_accuracy.compute(predictions=predictions, references=labels)["accuracy"]

    return {"precision": precision, "recall": recall, "f1": f1, "accuracy": accuracy}


# by default the Trainer will use MSEloss from (torch.nn) for regression and
# CrossEntropy loss for classification
trainer = Trainer(
    model=model_ft,
    args=training_args,
    train_dataset=tokenized_ft,
    eval_dataset=tokenized_ft,  # in-sample evaluation
    compute_metrics=compute_metrics
)

print(f"Training samples: {len(tokenized_ft)}")
print(f"Batches per epoch: {len(tokenized_ft) / 8}")