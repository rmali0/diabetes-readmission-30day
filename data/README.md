# Data

`diabetic_data.csv.gz` is the **Diabetes 130-US Hospitals for Years 1999-2008**
dataset from the UCI Machine Learning Repository, gzipped and otherwise unchanged.

- Source: https://archive.ics.uci.edu/dataset/296/diabetes+130-us+hospitals+for+years+1999-2008
- License: Creative Commons Attribution 4.0 International (CC BY 4.0)
- Size: 101,766 inpatient encounters, 50 columns (3.2 MB gzipped)
- Origin: de-identified encounters from the Cerner Health Facts database, 130 US hospitals
  and integrated delivery networks

Citation:

> Strack B, DeShazo JP, Gennings C, Olmo JL, Ventura S, Cios KJ, Clore JN.
> Impact of HbA1c measurement on hospital readmission rates: analysis of 70,000
> clinical database patient records. *BioMed Research International*.
> 2014;2014:781670. https://doi.org/10.1155/2014/781670

## Notes on the file

- Missing values are coded as `?` and are read as missing.
- `readmitted` is `<30`, `>30` or `NO`. The outcome here is `<30`.
- `diag_1` to `diag_3` are ICD-9-CM codes stored as text. Leading zeros were
  dropped in the source (`8` is 008), and V and E codes appear as `V57`, `E909`, etc.
- `number_inpatient`, `number_emergency` and `number_outpatient` count visits in
  the year before the encounter.
- Discharge disposition codes used for the death/hospice exclusion (from
  `IDS_mapping.csv` in the UCI download, which is not included here):
  11 Expired; 13 Hospice/home; 14 Hospice/medical facility;
  19 Expired at home; 20 Expired in a medical facility; 21 Expired, place unknown.

## Getting the file again

Download the zip from the UCI page above, extract `diabetic_data.csv`, and gzip it:

```sh
gzip -k diabetic_data.csv && mv diabetic_data.csv.gz data/
```
