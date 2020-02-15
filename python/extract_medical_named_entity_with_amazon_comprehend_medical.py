"""
This script is to extract medical entities using Amazon Comprehed Medical.

Warning: keep in mind that the service is very expensive.
Think carefully about the cost before using the service.

You need a AWS account and has enabled the AWS CLI.
Check for details here:
https://docs.aws.amazon.com/cli/latest/userguide/install-cliv1.html
"""
#%% load modules and connect to Amazon Comprehend Medical
import boto3
import json
from tqdm import tqdm

# custom module
from utilities import read_notes

# use your own AWS account's region_name
client = boto3.client(service_name='comprehendmedical',
                      region_name='us-east-1')

# we will only extract medical entites (mes) of clinical notes of two
# specialties: gastroenterology and neurology
#dat = pd.read_csv("data/mtsample_gastroenterology_neurology.csv")
#notes = list(dat.note)

#%% prepare data
gas_neu_urol = read_notes("../data/mtsamples_scraped.csv",
                  specialties=["Gastroenterology", "Neurology", "Urology"],
                  randomize=False,
                  clean=False)
notes = list(gas_neu_urol.note)


#%% run comprehend medical and save result
# Think thrice before ruiing, very expensive
# save result after extraction
confirm = input("Type I am not drunk to run Amazon Comprehend Medical: ")

if confirm == "I am not drunk":
    mes = []
    for note in tqdm(notes):
        me = client.detect_entities_v2(Text=note)
        me = me["Entities"]
        mes.append(me)



# save the extracted medical entities. Again, expensive
with open("comprehend_medical_gastroenterology_neurology_urology.txt", "w") as f:
    json.dump(mes, f)

## to use the extracted data, reload the saved json
#with open("comprehend_medical_neurology_gastroenterology.txt", "r") as f:
#    mes_loaded = json.load(f)
