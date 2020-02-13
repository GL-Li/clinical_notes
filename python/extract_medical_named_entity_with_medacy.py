from medacy.model.model import Model
import pandas as pd
from tqdm import tqdm
import json

mdl = Model.load_external('medacy_model_clinical_notes')

dat = pd.read_csv("../data/mtsample_gastroenterology_neurology.csv")
notes = list(dat.note)

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

annotations = get_mes(notes)

category = []
for ann in annotations:
    cat = [me[0] for me in list(ann)]
    category += cat