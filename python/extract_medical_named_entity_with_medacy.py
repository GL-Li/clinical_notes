#%% load modules and model
from medacy.model.model import Model
import pandas as pd
from tqdm import tqdm
import json

mdl = Model.load_external('medacy_model_clinical_notes')

#%% prepare data

dat = pd.read_csv("data/mtsamples_gastroenterology_neurology_urology.csv")
notes = list(dat.note)

#%% get annotaions
# an annotation looks like:
# [('Drug', 1405, 1413, 'peroxide'),
# ('Drug', 2016, 2022, 'Vicryl'),
# ('Route', 2023, 2035, 'subcutaneous')]
# So the element of index 3 is the keyword


def get_annotations(notes):
    # notes: medical notes as a list of strings
    mes = []
    for note in tqdm(notes):
        annotation = mdl.predict(note)
        mes.append(annotation)
    return(mes)


annotations = get_annotations(notes)

#%% get medical entities from annotations
def get_medacy_me(annotations):
    # annotations: list of medaCy annotations
    mes = []
    for annotation in annotations:
        me = [ann[3].replace(" ", "-") for ann in list(annotation)]
        me = " ".join(me)
        mes.append(me)
    return(mes)


mes = get_medacy_me(annotations)

#%% save results
with open("data/medacy_gastroenterology_neurology_urology.txt", "w") as f:
    json.dump(mes, f)
