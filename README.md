# Ablauf des Experiments

Zur Erinnerung hier noch einmal unser festgelegter Versuchsablauf. Ihr findet hier auch alle Links, ich hoffe das macht es uns ein bisschen einfacher :)

## LexTALE-Test (Erfassung des englischen Wortschatzniveaus)

* Zum LexTALE-Test: https://www.lextale.com/takethetest.html
> Nicht vergessen, sich das finale Ergebnis aufzuschreiben \
  $\to$ diese Zahl wird später im Fragebogen abgefragt!

## Fragebogen (Demographische Daten & Sprachhintergrund)
* Zum Fragebogen: https://survey.ifkw.lmu.de/valence_l1_english/
> Nicht vergessen, den Probanden eine Probandennummer zuzuweisen \
  $\to$ diese Zahl wird später im Fragebogen abgefragt!

* Zur Orientierung (*Probandennummern*):

| Wer? | Wie viele? | Probandennummern |
---|---|---|
| Jasmin | min. 10 Personen | 1000 - 1999 |
| Feven | min. 10 Personen | 2000 - 2999 |
| Han Wen | min. 10 Personen | 3000 - 3999 |
| Yueming | min. 10 Personen | 4000 - 4999 |


## OpenSesame-Experiment (Lexical Decision Task)
* Experiment durchführen
* die aktuelle Version des Experiments (Stand: 30.11.25, *final*) findet ihr hier auf GitHub

> sicherstellen, dass wir alle diese Version verwenden (Version: "LDT_Gruppe1_final")


# Was gibt es hier sonst noch zu sehen?
* background: preliminary literature review
* dataset
  * **README.md**: overview over dataset creation
  * **dataset_creation.ipynb**: code notebook used for dataset creation
  * **experiment_stimuli_dataframe.csv**: final dataset used in the experiment
  * **statistical_significance.R**: code used for statistical testing of stimuli
* experiment constituents
  * **LDT_Gruppe1_final.osexp**: OpenSesame experiment
  * **questionaire (valence_l1_english).pdf**: pdf version of subject questionaire
* experiment raw data
  * **subject files**: folder to store all subject files for easier access
  * **data_valence_l1_english_2026.xlsx**: questionaire output dataset
  * **LexTALE_results_to_dataset.ipynb**: parses questionaire output dataset, takes only VP Codes and LexTALE results, outputs new dataset consisting only of the two
  * **LexTALE.xlsx**: dataset consisting only of VP Codes and LexTALE results
  * **README.md**: specifies file formats and prerequisites for next session (Jan 9th)










