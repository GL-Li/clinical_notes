# -*- coding: utf-8 -*-
"""
Common functions
"""

import pandas as pd

def read_notes(csv_file, 
               specialties = None,
               y_label = None, 
               cols_keep = ["specialty", "note"],
               randomize = True,
               duplicate_rm = True, 
               clean = True,
               id = True):
    """
    read clinical notes and add label y to the original data
    
    Arguments
    ---------
      csv_file: string, path to the the data file
      specialties: string vector, selected specialties such as
        c("Gastroenterology", "Neurology")
      cols_keep: string, columns in the orignial data to keep, "all" to keep
        all columns.
      randomize: boolean, randomize row (sample) orders to break grouping
      y_label: boolean, if TRUE, add a class label 0, 1, 2, ... to each note
      duplicate_rm: boolean, remove rows if duplicated in column note
      clean: boolean, if TRUE add missing space after ".", for example,
        "abscess.PROCEDURE".
      id: boolean, add id to each sample after removing duplicates

    Return:
      a data.frame
    """
    
    dat = pd.read_csv(csv_file)
    if specialties is not None:
        dat = dat.query('specialty in @specialties')  # @var
    if cols_keep != "all":
        dat = dat[cols_keep]   # var select columns by variable
    if randomize:
        dat = dat.sample(frac=1)
    if clean:
        # missing space after ".", for example "abscess.PROCEDURE"
        dat["note"] = dat.note.str.replace(".", ". ")
    if y_label:
        dat["y"] = dat.specialty.astype("category").cat.codes
    if duplicate_rm:
        nrow_0 = dat.shape[0]
        dat = dat.drop_duplicates(subset="note")
        nrow_1 = dat.shape[0]
        print("Deleted " + str(nrow_0 - nrow_1) + " rows with duplicated notes")
    if id:
        dat["id"] = range(dat.shape[0])
        columns = ["id"] + list(dat.columns[dat.columns != "id"])
        dat = dat[columns]
        
    return(dat)
