import pandas as pd
import numpy as np
import csv as csv
from sklearn.ensemble import RandomForestClassifier

# Data cleanup
train_df = pd.read_csv('../csv/train.csv', header=0)        # Load the train file into a dataframe
#test_df = pd.read_csv('../csv/test.csv', header=0)        # Load the train file into a dataframe

def formatTemp(temp):
    if temp < 10:
        return 1
    elif temp >= 10 and temp < 20:
        return 2
    elif temp >= 20 and temp < 30:
        return 3
    elif temp >= 30:
        return 4

def formatHumidity(humid):
    if humid < 25:
        return 1
    elif humid >= 25 and humid < 50:
        return 2
    elif humid >= 50 and humid < 75:
        return 3
    elif humid >= 75:
        return 4

def formatWindSpeed(ws):
    if ws < 7:
        return 1
    elif ws >= 7 and ws < 10:
        return 2
    elif ws >= 10 and ws < 13:
        return 3
    elif ws >= 13 and ws < 16:
        return 4
    elif ws >= 16 and ws < 19:
        return 5
    elif ws >= 19 and ws < 30:
        return 6
    elif ws >= 30:
        return 7


train_df['temp2'] = train_df['temp'].map(formatTemp).astype(int)
train_df['atemp2'] = train_df['atemp'].map(formatTemp).astype(int)
train_df['humidity2'] = train_df['humidity'].map(formatHumidity).astype(int)
train_df['windspeed2'] = train_df['windspeed'].map(formatWindSpeed).astype(int)
train_df['datetime2'] = pd.to_datetime(train_df['datetime'], dayfirst=True)

print train_df['datetime2'][1]
temp =  type(train_df["datetime2"][1].time())
print temp.hour()
