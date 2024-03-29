import csv
import numpy as np
import pandas as pd

# データセットの読み込み
def load_data(data_path):
    return pd.read_csv(data_path, encoding='utf-8')

#  日付の古い順にソートする
def time_date_sort(df, column):
    df[column] = df[column].str.extract('(.+)T', expand=True)
    df = df.sort_values(by=column)
    df = df.set_index(column)
    return df

# 指定した列を削除する
def drop_column(df, column):
    return df.drop(column, axis=1)

# あるカラムで条件が一致する行を抽出
def extract_rows(df, column, data):
    return df[df[column] == data]

# 学習で扱うデータの作成
def multi_dataset(df, column):
    x = df
    x = x.shift()
    x = x.dropna(how='any')

    y = df[column]
    y = y.shift(-1)
    y = y.dropna(how='any')
    return x, y

def make_dataset(df, column):
    x = df
    y = df[column]
    return x, y
