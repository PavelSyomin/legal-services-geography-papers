import csv
import os
import time

import requests
import numpy as np
import tqdm


def get_embedding(query_uri: str, embed_url: str, text: str) -> np.array:
    query_data = {
        "modelUri": query_uri,
        "text": text,
    }

    return np.array(
        requests.post(embed_url, json=query_data, headers=headers).json()["embedding"]
    )


def get_iam_token(oauth_token: str) -> str:
    resp = requests.post(
        "https://iam.api.cloud.yandex.net/iam/v1/tokens",
        data=str({'yandexPassportOauthToken': oauth_token})
    )

    if not resp.ok:
        raise RuntimeError("Failed to get IAM token for Yandex Cloud")

    return resp.json()["iamToken"]


folder_id = os.environ.get("FOLDER_ID")
oauth_token = os.environ.get("OAUTH_TOKEN")

if not (folder_id and oauth_token):
    raise RuntimeError("FOLDER_ID and OAUTH_TOKEN env vars must be set")

iam_token = get_iam_token(oauth_token)
query_uri = f"emb://{folder_id}/text-search-query/latest"
embed_url = "https://llm.api.cloud.yandex.net:443/foundationModels/v1/textEmbedding"
headers = {"Content-Type": "application/json", "Authorization": f"Bearer {iam_token}", "x-folder-id": f"{folder_id}"}

with open("lemmas.csv") as f:
    reader = csv.DictReader(f)
    names = [row["name"] for row in reader]

missing = []
with open("lemmas-vectors.csv", "w") as f:
    fieldnames = ["name"] + [f"dim_{i}" for i in range(256)]
    writer = csv.DictWriter(f, fieldnames=fieldnames)

    writer.writeheader()

    for name in tqdm.tqdm(names):
        try:
            emb = get_embedding(query_uri, embed_url, name)
        except Exception:
            missing.append(name)

        row = dict(zip(fieldnames, [name] + list(emb)))
        writer.writerow(row)

with open("missing.csv", "w") as f:
    for name in missing:
        print(name, file=f)
