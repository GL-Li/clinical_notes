"""
Generate word embedding matrix of clinical notes using pre-trained BioWordVec.

The word embedding matrix is saved as csv file for transfer learning in R.
"""
#%% load packages
from gensim.models import KeyedVectors
import numpy as np
import pandas as pd
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras.preprocessing.text import Tokenizer


#%% load biowordvec
# use limit to set the amount of top words to read. Far exceed 16GB memory if read all.
# loading the top 5M words takes 6.5G memory, comfortable for my 16GB laptop
dir_biowordvec = "/home/gl/data/pre-trained-word-embedding/bio-word-vector/"
model = KeyedVectors.load_word2vec_format(
    fname=dir_biowordvec + "BioWordVec_PubMed_MIMICIII_d200.vec.bin", 
    binary=True, 
    limit=int(5E6)        
)

# check the words in the model
model_words = model.vocab.keys()


#%% get pretrained embedding matrix of tokens of gas-neu-uro
# we will use keras to tokenize the note as later on they are used by keras

def get_embedding_matrix(corpus, max_words=5000, fname=None):
    """
    Get embedding matrix of corpus using BioWordVec

    Parameters
    ----------
    corpus: string iterable
    max_words: int, number of top frequent words to keep
    fname: str, file path if want to save the martix to a text file

    Return
    ------
    No return but a text file may be saved
    """
    tk = Tokenizer(5000)  # keep top 5000 to save space
    tk.fit_on_texts(corpus)
    word_index = tk.word_index
    tokens = list(word_index.keys())

    # get the matrix of my own words
    # as np.zeros if a word is not in the model vocabulary

    token_embeddings = []
    not_in_model = []
    for wd in tokens:
        if wd in model_words:
            token_embeddings.append(model.get_vector(wd))
        else:
            token_embeddings.append(np.zeros(200))
            not_in_model.append(wd)
    print(f"{len(not_in_model)} tokens are not in the model's vocab")
    print("These tokens are asigned array of zeros. They are: ")
    print(not_in_model)      
  
    token_df = pd.DataFrame(token_embeddings) 
    token_df.index = tokens

    # save my_embeddings as text file
    if fname is not None:
        token_df.to_csv(fname, header=False)
    

mt = pd.read_csv("data/amazon_medacy_mtsamples_gastr_neuro_urolo.csv")
# Get pretrained embedding matrix of original notes
# clean to be consistent with R read_notes
notes = mt.note.str.replace(".", ". ").drop_duplicates(False)
file_name = "../data/gas_neu_uro_token_embeddings_note.csv"
get_embedding_matrix(notes, fname=file_name)

# get pretrained embedding matrix of Amazon medical entities
amzn = mt.amazon_me.drop_duplicates(False)
file_name = "../data/gas_neu_uro_token_embeddings_amazon.csv"
get_embedding_matrix(amzn, fname=file_name)
