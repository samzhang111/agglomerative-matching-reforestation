import pandas as pd
from pyprojroot.here import here

balances = here("matching/output/").glob("**/all_balance_*.csv")
balance_dfs = []

for i, balfile in enumerate(balances):
    if i % 100 == 0:
        print(i, balfile)
    balance = pd.read_csv(balfile)
    balance_dfs.append(balance)

df_bal = pd.concat(balance_dfs)

df_bal.to_csv(here("summarize/output/balances.csv"), index=False)

results = here("matching/output/").glob("**/all_results_*.csv")
results_dfs = []

for i, result_file in enumerate(results):
    _, _, seed, _ = result_file.stem.split('_')
    if i % 100 == 0:
        print(i, seed, result_file)
    result = pd.read_csv(result_file)
    result['seed'] = int(seed)
    results_dfs.append(result)

df = pd.concat(results_dfs)
df.to_csv(here("summarize/output/results.csv"), index=False)
