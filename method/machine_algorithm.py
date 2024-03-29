from sklearn import linear_model
from sklearn.svm import SVR
from sklearn.svm import LinearSVR
from sklearn.linear_model import Lasso
from sklearn.linear_model import Ridge
from sklearn.ensemble import RandomForestRegressor as RFR
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import preprocessing as pp
import dataset as ds 

def machine_learning(train_data):

    # 使用した特徴量
    #train_columns = train_data.columns

    # データセット作成
    X, y = ds.make_dataset(train_data, "CustNum")

    # 学習で扱えるデータにするために，　numpy配列に変換
    X = np.array(X).reshape(-1, len(X.columns))
    y = np.array(y).reshape(-1, 1)

    X = X[:len(train_data)-1, :]
    y = y[1:]

    scaler = MinMaxScaler()  # データを0～1の範囲にスケールするための関数。

    X = pp.feature_scaling(X, scaler)
    y = pp.feature_scaling(y, scaler)

    print(X)

    # 訓練データに扱うデータの割合
    #split_num = int(len(X)*0.9)
    split_num = 970 
    # split_num = 940 

    # データの分割
    X_train = X[:split_num, :]
    X_test  = X[split_num:, :]
    y_train = y[:split_num]
    y_test  = y[split_num:]

    # 扱った特徴量の属性ベクトルの抽出
    feature_vector = train_data[split_num:].shift()
    feature_vector = feature_vector.dropna(how='any')

    # テストデータの答えのスケーリングを直す
    y_test = pp.restore_scaling(y_test, scaler)
    y_test = pd.DataFrame(y_test)
    y_test.index = feature_vector.index

    # カラム名を見やすくしたい
    # columns = ['前日の曜日', '前日の売り上げ',
    #        '前日の来客数','前日のgoogle閲覧数','当日の観光客予測数','前日のtabelog閲覧数',
    #        '前日のtabelog評価','前日のgoogle評価','売り上げの1週間の移動平均']


    # それぞれの学習アルゴリズムで予測
    reg_model, reg_predict, reg_error = linear_regression(X_train, y_train, X_test, y_test, scaler)
    reg_predict = pd.DataFrame(reg_predict)
    make_plot(y_test, reg_predict)

    svr_model, svr_predict, svr_error = linear_svr(X_train, y_train, X_test, y_test, scaler)
    svr_predict = pd.DataFrame(svr_predict)
    make_plot(y_test, svr_predict)

    ridge_model, ridge_predict, redge_error = ridge(X_train, y_train, X_test, y_test, scaler)
    ridge_predict = pd.DataFrame(ridge_predict)
    make_plot(y_test, ridge_predict)

    lasso_model, lasso_predict, lasso_error = lasso(X_train, y_train, X_test, y_test, scaler, 0.001)
    lasso_predict = pd.DataFrame(lasso_predict)
    make_plot(y_test, lasso_predict)

    # random_model, random_predict, random_error = random_forest(X_train, y_train, X_test, y_test, scaler)
    # random_predict = pd.DataFrame(random_predict)
    # make_plot(y_test, random_predict)

    reg_error_plot = reg_error.plot(title='Regression Error Rate', figsize=(12, 8), grid=True)
    reg_error_plot.set_xlabel("Date")
    reg_error_plot.set_ylabel("Error Rate %")

    svr_error_plot = svr_error.plot(title='SVR Error Rate', figsize=(12, 8), grid=True)
    svr_error_plot.set_xlabel("Date")
    svr_error_plot.set_ylabel("Error Rate %")

    ridge_error_plot = svr_error.plot(title='Ridge Error Rate', figsize=(12, 8), grid=True)
    ridge_error_plot.set_xlabel("Date")
    ridge_error_plot.set_ylabel("Error Rate %")

    lasso_error_plot = svr_error.plot(title='Lasso Error Rate', figsize=(12, 8), grid=True)
    lasso_error_plot.set_xlabel("Date")
    lasso_error_plot.set_ylabel("Error Rate %")

    # random_error_plot = svr_error.plot(title='Random Forest Error Rate', figsize=(12, 8), grid=True)
    # random_error_plot.set_xlabel("Date")
    # random_error_plot.set_ylabel("Error Rate %")

    # return reg_model, svr_model, ridge_model, lasso_model, random_model
    return reg_model, svr_model, ridge_model, lasso_model

def random_forest(X_train, y_train, X_test, y_test, scaler):
    
    model = RFR(n_jobs=-1, random_state=50)
    model.fit(X_train, y_train)

    predict = model.predict(X_test)
    predict = np.array(predict).reshape(-1, 1)
    predict = pp.restore_scaling(predict, scaler)

    predict.index = y_test.index

    mse = mean_squared_error(y_test, predict)
    rmse = np.sqrt(mse)

    error_rate = np.fabs((y_test - predict)) / y_test * 100
    error_rate.columns = ['regression error rate']

    print("-------------------------------------------------------------")
    print("ランダムフォレストでの予測結果")

    print("決定係数：{}".format(r2_score(y_test, predict)))
    print("MSE(平均二乗誤差)　:  {}".format(mse))
    print("RMSE(平均平方二乗誤差) : {}".format(rmse))
    print("-------------------------------------------------------------")

    return model, predict, error_rate



# 線形回帰アルゴリズム
def linear_regression(X_train, y_train, X_test, y_test, scaler):

    # 学習
    model = linear_model.LinearRegression()
    model.fit(X_train, y_train)

    # 予測
    predict = model.predict(X_test)

    # スケールされた値を元に戻す
    predict = pp.restore_scaling(predict, scaler)

    predict.index = y_test.index

    mse = mean_squared_error(y_test, predict)
    rmse = np.sqrt(mse)

    error_rate = np.fabs((y_test - predict)) / y_test * 100
    error_rate.columns = ['regression error rate']

    print("-------------------------------------------------------------")
    print("線形回帰での予測結果")

    print("決定係数：{}".format(r2_score(y_test, predict)))
    print("MSE(平均二乗誤差)　:  {}".format(mse))
    print("RMSE(平均平方二乗誤差) : {}".format(rmse))
    print("-------------------------------------------------------------")

    return model, predict, error_rate

# SVR
def linear_svr(X_train, y_train, X_test, y_test, scaler):

    model = LinearSVR()
    model.fit(X_train, y_train)

    predict = model.predict(X_test)

    predict = np.array(predict).reshape(-1, 1)
    predict = pp.restore_scaling(predict, scaler)

    predict.index = y_test.index

    mse = mean_squared_error(y_test, predict)
    rmse = np.sqrt(mse)

    error_rate = np.fabs((y_test - predict)) / y_test * 100
    error_rate.columns = ['svr error rate']

    print("-------------------------------------------------------------")
    print("線形SVRでの予測結果")

    print("決定係数：{}".format(r2_score(y_test, predict)))
    print("MSE(平均二乗誤差)　:  {}".format(mse))
    print("RMSE(平均平方二乗誤差) : {}".format(rmse))
    print("-------------------------------------------------------------")

    return model, predict, error_rate

# Lasso
def lasso(X_train, y_train, X_test, y_test, scaler, alpha):

    model = Lasso(alpha=alpha)
    model.fit(X_train, y_train)

    predict = model.predict(X_test)

    predict = np.array(predict).reshape(-1, 1)
    predict = pp.restore_scaling(predict, scaler)

    predict.index = y_test.index

    mse = mean_squared_error(y_test, predict)
    rmse = np.sqrt(mse)

    error_rate = np.fabs((y_test - predict)) / y_test * 100
    error_rate.columns = ['lasso error rate']

    print("-------------------------------------------------------------")
    print("Lasso回帰での予測結果")

    print("決定係数：{}".format(r2_score(y_test, predict)))
    print("MSE(平均二乗誤差)　:  {}".format(mse))
    print("RMSE(平均平方二乗誤差) : {}".format(rmse))
    print("-------------------------------------------------------------")

    return model, predict, error_rate

# Ridge
def ridge(X_train, y_train, X_test, y_test, scaler):

    model = Ridge()
    model.fit(X_train, y_train)

    predict = model.predict(X_test)

    predict = np.array(predict).reshape(-1, 1)
    predict = pp.restore_scaling(predict, scaler)

    predict.index = y_test.index

    mse = mean_squared_error(y_test, predict)
    rmse = np.sqrt(mse)

    error_rate = np.fabs((y_test - predict)) / y_test * 100
    error_rate.columns = ['ridge error rate']

    print("-------------------------------------------------------------")
    print("Ridge回帰での予測結果")

    print("決定係数：{}".format(r2_score(y_test, predict)))
    print("MSE(平均二乗誤差)　:  {}".format(mse))
    print("RMSE(平均平方二乗誤差) : {}".format(rmse))
    print("-------------------------------------------------------------")

    return model, predict, error_rate

# 線形回帰の係数と切片
def coefficient_df(model, columns):
    if(len(model.coef_) != 1):
        coef_list = np.array([model.coef_])
        coef = pd.DataFrame(coef_list)
    else:
        coef = pd.DataFrame(model.coef_)

    coef.columns = columns

    # 切片
    coef['切片'] = model.intercept_

    return coef

def compare_predict(answer, predict, df):
    predict_size = len(df)-len(predict)

    answer.index = df[predict_size:].index
    predict.index = df[predict_size:].index
    
    compare = pd.concat([answer, predict], axis=1)
    compare.columns = ["Answer", "Predict"]

    return compare
    

# 精度の高いものと低いものを分類する
def accuracy_rate(df, answer, predict, rate):
    df_rate = pd.DataFrame()
    df_gap = pd.DataFrame()

    for i, (A, P) in enumerate(zip(df[answer], df[predict])):
        min_num = min(A, P)
        max_num = max(A, P)

        if(min_num/max_num >= (rate/100)):
            df_rate = df_rate.append(df.iloc[i])
        else:
            df_gap = df_gap.append(df.iloc[i])

    return df_rate, df_gap

def make_plot(answer, predict):
    plt.figure(figsize=(15,8))

    plt.plot(answer, label = 'Answer')
    plt.plot(predict, label = 'Prediction')

    plt.xlabel("Date")
    plt.ylabel("Customer Num")

    plt.xticks(rotation=70)
    plt.legend(loc='best')
    plt.grid(True)
    
    plt.show()
    plt.close()

