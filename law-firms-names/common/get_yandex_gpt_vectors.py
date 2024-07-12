import csv
import time

import requests
import numpy as np
import tqdm


def get_embedding(text: str) -> np.array:
    query_data = {
        "modelUri": query_uri,
        "text": text,
    }

    return np.array(
        requests.post(embed_url, json=query_data, headers=headers).json()["embedding"]
    )


FOLDER_ID = "b1guv73jumjli9it2kum"
IAM_TOKEN = "t1.9euelZqJjcqMjsySz4yakpqbnY3Lju3rnpWam5yPi5WQks2YyMyOiorOzpjl9Pd-EzpN-e9jbBrS3fT3PkI3TfnvY2wa0s3n9euelZrMypqYk5KVmcyPjsmKyIqOkO_8xeuelZrMypqYk5KVmcyPjsmKyIqOkA.A1u_ruhFatIschBArQEDgMiZJVotdF36Tu2IjEw_cJ3UO5ruz8qQSAMbGdwK6afYF_gUg6nqJ44ns04DxVByDw"

query_uri = f"emb://{FOLDER_ID}/text-search-query/latest"
embed_url = "https://llm.api.cloud.yandex.net:443/foundationModels/v1/textEmbedding"
headers = {"Content-Type": "application/json", "Authorization": f"Bearer {IAM_TOKEN}", "x-folder-id": f"{FOLDER_ID}"}

with open("names.csv") as f:
    reader = csv.DictReader(f)
    names = [row["name"] for row in reader]

missing = []
with open("names_vectors.csv", "w") as f:
    fieldnames = ["name"] + [f"dim_{i}" for i in range(256)]
    writer = csv.DictWriter(f, fieldnames=fieldnames)

    writer.writeheader()

    for name in tqdm.tqdm(names):
        try:
            emb = get_embedding(name)
        except:
            missing.append(name)

        row = dict(zip(fieldnames, [name] + list(emb)))
        writer.writerow(row)

with open("missing.csv", "w") as f:
    for name in missing:
        print(name, file=f)
