#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb 14 20:58:49 2020

@author: gl
"""
#%% load data
import pandas as pd
import json

mt = pd.read_csv("data/mtsamples_gastroenterology_neurology_urology.csv")

with open("data/comprehend_medical_gastroenterology_neurology_urology.txt") as f:
    amazon = json.load(f)
    
with open("data/medacy_gastroenterology_neurology_urology.txt") as f:
    medacy_bow = json.load(f)
    
#%% get medical terms from one Comprehend Medical entity
def get_amazon_bow(me):
    # me: medical entities extracted from Amazon Comprehend Medical 
    # like aaa["Entities"]
    text = [dic["Text"] for dic in me]
    traits = [dic["Traits"] for dic in me]  # trait including negation
    
    negation = []
    for ele in traits:
        if len(ele) == 0:
            negation.append("")
        else:
            count = 0
            for dic in ele:
                if dic["Name"] == "NEGATION":
                    count += 1
            if count == 0:
                negation.append("")
            else:
                negation.append("not ")
    
    # attache "-1" to the text if it is negative
    bow = [n + t for t, n in zip(text, negation)]
    
    return(bow)


#%% get amazon bag of words
amazon_bow = []
for me in amazon:
    amazon_bow.append(", ".join(get_amazon_bow(me)))
    
#%% add amazon_bow and medacy_bow to notes
mt["amazon_me"] = amazon_bow
mt["medacy_me"] = medacy_bow
mt = mt[['id', 'amazon_me', 'medacy_me', 'specialty', 'note']]
mt.to_csv("data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv", index=False)
