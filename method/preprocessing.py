import numpy as np
import pandas as pd

# ラベルの欠損値の処理
def format_label(y):
    y = y.fillna(method='ffill')
    y = np.array(y).reshape(-1, 1)
    return y

def format_feature(df, column, kind):
    if(kind == "before"):
        df[column] = df[column].fillna(method = 'ffill')
    elif(kind == "median"):
        df[column] = df[column].fillna(df[column].median())
    else:
        print("新しい欠損値の埋め方を記述して")

    return df


#  スケーリング　→ 今回は正規化で試してみる
def feature_scaling(df, scaler):
    scaler.fit(df)  # スケーリングに使用する最小／最大値を計算する。
    return scaler.transform(df)  # Xを0～1の範囲にスケーリングする。

#  スケーリングしたものをもとに戻す
def restore_scaling(df, scaler):
    df = scaler.inverse_transform(df)
    return pd.DataFrame(df) 

