import sys
import warnings
import numpy as np
import pandas as pd
warnings.filterwarnings('ignore')
vwap = lambda x: np.ma.average(x, weights=df.loc[x.index, "v"])
df = pd.read_csv(sys.stdin, names=["s", "p", "v"], usecols=[2,4,5])
df = df.groupby("s").agg(vwap=("p", vwap), volume=("v", "sum")).fillna(0)
# print(df)
print(df.to_json(indent=4, orient="index"))
