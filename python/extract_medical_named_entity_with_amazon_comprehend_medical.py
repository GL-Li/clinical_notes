"""
This script is to extract medical entities using Amazon Comprehed Medical.

Warning: keep in mind that the service is very expensive.
Think carefully about the cost before using the service.

You need a AWS account and has enabled the AWS CLI.
Check for details here:
https://docs.aws.amazon.com/cli/latest/userguide/install-cliv1.html
"""

import boto3
import pandas as pd
import json
from tqdm import tqdm

# use your own AWS account's region_name
client = boto3.client(service_name='comprehendmedical',
                      region_name='us-east-1')

# we will only extract medical entites (mes) of clinical notes of two
# specialties: gastroenterology and neurology
dat = pd.read_csv("data/mtsample_gastroenterology_neurology.csv")
notes = list(dat.note)

# Think thrice before ruiing, very expensive
# download and save result
mes = []
for note in tqdm(notes):
    me = client.detect_entities_v2(Text=note)
    me = me["Entities"]
    mes.append(me)

# save the extracted medical entities. Again, expensive
with open("comprehend_medical_neurology_gastroenterology.txt", "w") as f:
    json.dump(mes, f)

# to use the extracted data, reload the saved json
with open("comprehend_medical_neurology_gastroenterology.txt", "r") as f:
    mes_loaded = json.load(f)
