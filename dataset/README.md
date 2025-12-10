todo
  
* upload dataset *V*
* documentation of dataset creation
  * google doc
  * code
  * bookmarks
* code dataset creation
* statistical check dataset ??


# Dataset creation
The dataset contains *words* and *nonwords*

For *words*, we used code to automatically retrieve fitting stimuli, and for nonwords, we used a combination of manual nonword-creation (Han) and manual selection from a nonword database (Jasmin).
Both were then manually checked by all team members to ensure that their respective stimuli conditions were met.

## Words
Conditions for words to be included:
* length maximum 7 characters
* part-of-speech (POS) noun
* Log frequences higher than 2
* non-cognates


Datasets used in word collection:
Stimuli and their valence: BRM-emot-submit.csv https://link.springer.com/article/10.3758/s13428-012-0314-x
Frequency: SUBTLEXusfrequencyabove1.xls https://www.ugent.be/pp/experimentele-psychologie/en/research/documents/subtlexus/overview.htm
Cognates: CogNet-v1.0.tsv https://github.com/kbatsuren/CogNet/blob/master/CogNet-v1.0.zip

The aforementioned datasets were automatically parsed (see code {LINK]) and only fitting instances were retained. These instances were then filtered into one of three categorical valences:
positive (valence of 7 or higher), negative (valence of 3 or lower), and neutral (valence between (or equal to) 4.5 and 5.5).
The remainder of words were not retained, as we did not want fuzzy edge cases, which would have made the analysis noisy.

Counters per category (positive, negative, neutral valence) were implemented as well to ensure more than 30 instances each.
The objective behind that was to leave room for potential deletion of faulty instances (instances that do not truly fit the conditions) and still have enough instances in the end.

The resulting words were then manually checked to ensure that no faulty words were included (i.e. faulty assignment of "noun" as POS or "non-cognate" as cognate status).
This then resulted in 30 words per category: postive, negative, and neutral (90 in total).

## Nonwords
Conditions
* target language (English):
  * correspond to phonotactical and orthographical rules of the target language
  * correspond to formation rules of nouns in target language
* length maximum 7 characters

Han used manual perturbation of existing english words (*i think*), e.g., replacing characters in existing words, e.g., "trouble" $\to$ "trooble".
Jasmin used the ARC nonword database from Macquarie University (Sydney-Australia: http://www.cogsci.mq.edu.au/research/resources/nwdb/nwdb.html), enabling:
* only orthographically existing onsets
* only orthographically existing bodies \
 with a maximum length of 7 characters.

Both were combined and the resulting nonwords were then also manually checked to ensure that all aforementioned conditions were met.

# Statistical Testing of the dataset
todo




