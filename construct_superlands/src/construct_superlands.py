import sys
import pandas as pd
from pyprojroot.here import here
from annoy import AnnoyIndex
import numpy as np


fn = sys.argv[1]
seed = int(sys.argv[2])
index_size = int(sys.argv[3])

try:
    # 0 or 1
    use_weights = int(sys.argv[3])
except IndexError:
    use_weights = 1


def closest_subset_with_averaging(target, vectors, weights, components_to_sum=1):
    """
    Find a subset of vectors that adheres to custom summation and averaging rules
    to get as close as possible to the target.

    Args:
    - target (numpy array): N-dimensional target vector
    - vectors (numpy array): Array of N-dimensional vectors
    - weights (numpy array): N-dimensional weight vector for covariate weights

    Returns:
    - list: Indices of the chosen vectors
    """
    current_sum = np.array([0.0] * target.shape[0])
    chosen_indices = []

    # Create an array of indices corresponding to the vectors
    indices = np.arange(vectors.shape[0])

    while len(vectors) > 0 and not np.allclose(current_sum[:components_to_sum], target[:components_to_sum]):
        # Calculate potential sums for the first components_to_sum components
        potential_sums = vectors.copy()
        potential_sums[:, :components_to_sum] += current_sum[:components_to_sum]

        # For the components_to_sum-th to Nth components, compute the potential weighted averages
        weight_totals = vectors[:, 0] + current_sum[0]
        for i in range(components_to_sum, target.shape[0]):
            potential_sums[:, i] = (vectors[:, 0] * vectors[:, i] + current_sum[0] * current_sum[i]) / weight_totals

        # Calculate weighted distances to target for each potential sum
        distances = np.linalg.norm(potential_sums * weights - target * weights, axis=1)
        best_index = np.argmin(distances)

        # If adding the best vector makes the solution worse, we're done
        if distances[best_index] > np.linalg.norm(current_sum - target):
            break

        # Update the weighted averages for the components_to_sum-th to Nth components
        current_sum[components_to_sum:] = (current_sum[0] * current_sum[components_to_sum:] + vectors[best_index, 0] * vectors[best_index, components_to_sum:]) / (current_sum[0] + vectors[best_index, 0])

        # Update current sum for the first components_to_sum components
        current_sum[:components_to_sum] += vectors[best_index, :components_to_sum]
        chosen_indices.append(indices[best_index])

        # Remove the selected vector and its index from consideration
        vectors = np.delete(vectors, best_index, axis=0)
        indices = np.delete(indices, best_index, axis=0)

    return chosen_indices


# Hacky global variable: this needs to be reset before runs
exclude_ids = set()

# Covariates and weights
covariates = {
    'area_final': 1,
    'forest1985_perc': 2 if use_weights else 1,
    'pop_dens': 2 if use_weights else 1,
    'temp': 1,
    'precip': 1,
    'elevation': 1,
    'slope': 1,
    'dist_roads': 1,
    'dist_rivers': 1,
    'dist_cities': 1
}
covariate_names = []
weights = []
for k, v in covariates.items():
    covariate_names.append(k)
    weights.append(v)
weights = np.array(weights)

def make_superland_for_treatment_land(ta, area_col='area_final', cols_to_sum=['area_final', 'persistent_rest', 'ephemeral_rest'], outcomes=['persistent_rest', 'ephemeral_rest'], cols_to_drop=['id'], covariate_names=covariate_names, weights=weights):
    #ta_use = ta.drop(cols_to_drop + outcomes)
    #control_use = control_with_area.drop(cols_to_drop + outcomes, axis=1)

    ta_use = ta[covariate_names]
    control_use = control_with_area[covariate_names]

    t = ta_use.drop(cols_to_sum, errors='ignore')
    nearest_results = set(control_index.get_nns_by_vector(t.values, index_size)) - exclude_ids
    nearest = np.array(list(nearest_results))
    candidates = control_use.loc[nearest].to_numpy()
    component_lands = closest_subset_with_averaging(ta_use.values, candidates, weights)

    df_super = control_with_area.loc[nearest[component_lands]]

    # Weighted average of those columns
    df_super_combined = df_super.drop(cols_to_sum + outcomes + cols_to_drop, axis=1).multiply(df_super[area_col], axis='index').sum(axis=0) / df_super[area_col].sum()

    # Sum of the remaining columns
    for col in cols_to_sum:
        df_super_combined[col] = df_super[col].sum()

    # Treatment ID
    df_super_combined['treatment_id'] = ta.name

    # Control IDs
    df_super_combined['control_ids'] = df_super.agg({'id': list})['id']
    exclude_ids.update(df_super_combined['control_ids'])

    return df_super_combined


df = pd.read_csv(here(f"import/output/{fn}.csv"))
df = df.drop(['land_type2'], axis=1)
df = df.set_index(df['id'])

print(f"File {fn} has length {len(df)}")

area_col='area_final'
cols_to_sum=['area_final', 'persistent_rest', 'ephemeral_rest']
outcomes=['persistent_rest', 'ephemeral_rest']
cols_to_drop = list(set(df.columns) - set(covariate_names).union(cols_to_sum).union(outcomes))
print("Columns to drop: ", cols_to_drop)

#treat = df[df.treat == 1].drop(['treat'] + cols_to_sum + cols_to_drop, axis=1)
treat = df[df.treat == 1][covariate_names].drop(cols_to_sum, axis=1, errors='ignore')
treat_with_area = df[df.treat == 1].drop(['treat'], axis=1)

#control = df[df.treat == 0].drop(['treat'] + cols_to_sum + cols_to_drop, axis=1)
control = df[df.treat == 0][covariate_names].drop(cols_to_sum, axis=1, errors='ignore')
control_with_area = df[df.treat == 0].drop(['treat'], axis=1)

print(f"Building index for {fn}")
control_index = AnnoyIndex(control.shape[1], 'euclidean') # Remove the index and the "treatment" columns
control_index.set_seed(seed)
for i, (ix, row) in enumerate(control.iterrows()):
    if i % 10000 == 0:
        print('.', end='')
    control_index.add_item(ix, row.values)

control_index.build(10) # 10 trees

print(f"Forming superlands for {fn}")
exclude_ids = set()
superlands = []

for i, ta in treat_with_area.iterrows():
    if i % 100 == 0:
        print('.', end='')
    result = make_superland_for_treatment_land(ta)
    if result is not None:
        superlands.append((ta, result, sum((ta - result)**2)))

df_superlands = pd.DataFrame([x[1] for x in superlands]).dropna()
print(f"Done. Formed {len(df_superlands)} superlands for {fn}")

df_superlands.to_csv(here(f"construct_superlands/output/{seed}/{fn}_superlands_{index_size}_{seed}.csv"), index=False)
