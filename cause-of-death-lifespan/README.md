This folder contains several scripts and charts to show causes of death across age groups, using data from the United States.

The data can be found on the [CDC Wonder's database](https://wonder.cdc.gov/).

Each script below includes code to create charts on the relative share of deaths from each cause, the number of deaths from each cause, and the death rate from each cause. These are shown across age groups.

### Top-level cause of death categories

| Chart description  | My downloaded data | Script | How to update |
|------------|-------------------|------------------|---------------|
| Causes of death, by age | [Link to data](https://wonder.cdc.gov/controller/saved/D158/D387F463) | [Link to script](https://github.com/saloni-nd/scientific-discovery/blob/main/cause-of-death-lifespan/cause-of-death-lifespan-area-chart-both-sexes.R) | [CDC Wonder](https://wonder.cdc.gov/) > "Underlying cause of death" > Group results by: Single-Year Ages, ICD chapter |
| Causes of death, by age and sex | [Link to data](https://wonder.cdc.gov/controller/saved/D158/D387F462) | [Link to script](https://github.com/saloni-nd/scientific-discovery/blob/main/cause-of-death-lifespan/cause-of-death-lifespan-area-chart-by-sex.R) | [CDC Wonder](https://wonder.cdc.gov/) > "Underlying cause of death" > Group results by: Single-Year Ages, Gender, ICD chapter |

---

### Focusing on external cause of death categories

The following two charts also depend on data from the previous charts above. 

For example, "Causes of death (external causes), by age" requires data from "Causes of death, by age". Similarly, "Causes of death (external causes), by age and sex" requires data from "Causes of death, by age and sex".

| Chart description  | My downloaded data | Script | How to update |
|------------|-------------------|------------------|---------------|
| Causes of death (external causes), by age | [Link to data](https://wonder.cdc.gov/controller/saved/D158/D387F464) | [Link to script](https://github.com/saloni-nd/scientific-discovery/blob/main/cause-of-death-lifespan/causes-of-death-both-sexes-external-causes-focus.R) | [CDC Wonder](https://wonder.cdc.gov/) > "Underlying cause of death" > Group results by: Single-Year Ages, ICD subchapter > Select cause of death: "V01-Y89 (External causes of morbidity and mortality)" |
| Causes of death (external causes), by age and sex | [Link to data](https://wonder.cdc.gov/controller/saved/D158/D387F465) | [Link to script](https://github.com/saloni-nd/scientific-discovery/blob/main/cause-of-death-lifespan/causes-of-death-by-sex-external-causes-focus.R) | [CDC Wonder](https://wonder.cdc.gov/) > "Underlying cause of death" > Group results by: Single-Year Ages, Gender, ICD subchapter > Select cause of death: "V01-Y89 (External causes of morbidity and mortality)" |
