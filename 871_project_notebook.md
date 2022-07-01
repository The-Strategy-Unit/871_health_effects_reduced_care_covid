---
editor_options: 
  markdown: 
    wrap: 80
---

## TODO
isoweek 2022-13, rerun main SQL scripts


## R scripts

running order\

-   001 sample_ed_provs.R
-   002 plot_activity_trends.R
-   003 spc_diag_patterns.R
-   004 initial_diagnosis_grping.R
-   005 update_diagnosis_grping.R
-   006 plot_profiles_diagnosis_grps.R

depends on spc_diag_patterns.R\

-   topn_diagnosis_increased_table.Rmd

depends on update_diagnosis_grping.R\

-   common_infections_of_childhood_table.Rmd










# notes on proposal

disruption to primary care - SC marker conditions - a surgical condition, MH
condition, a medical condition possible marker conditions - hypertension,
anemia, NOT cancer, cataracts (Martin Weale work, leads to loneliness,
depressio, falls etc.), eating disorders

What's emerging now that is different to pre-Covid? in terms of patterns of
morbidity presenting to health services

Mechanisms: Direct effect of COVID-19 Long Covid Response i.e. lockdown effect
on depression, loneliness Delayed planned care (effect on management of LTC)
Can't think of correct word - RSV outbreaks

QOF data might be useful

## Part 1: What has happened to hospital activity since the pandemic?

Analysis of hospital activity trends (inpatient and ED visits) since the
COVID-19 pandemic. What was the scale of reduction for different types of
activity during the different phases of the pandemic. Has activity returned to
'normal'? Use algorithm to cut inpatient activity. Where there differences by
STP or was the experience similar in all regions?

### issues log for part 1

algorithm to cut inpatient activity - did it work? assigning activity to STP and
treatment of non-STP activity switch from AE dataset to ECDS dataset - break in
series? avoided for diagnosis analysis as both periods taken from ECDS
completeness of diagnosis coding in ECDS - selection of providers with high
level of coding

are there 42 STPs?

## dates

switch over from AE to ECDS on 2019-04-01. Handily, iso week 14 began on 1 April
2019. So iso week 13 from AE and iso week 14 from ECDS - means no missing data.
handily iso week 1 2018 starts on Jan-1 2018 \# which periods - show on plot \#
validate ed diag \# diag_dat totals will be different incl non-assigned stps \#
diag_dat - select best providers for coding

## inpatient admissions

-   appropriateness of admission group algorithm
-   APCS_Last_Ep_Ind used over Last_Episode_in_Spell_Indicator which incldues 1,
    2, 9 and NULL values
-   APCE use over APCS as APCS includes all diagnoses in a single (comma
    seperated) field

# ED visits

-   type 1 only
-   appropriateness of arrival mode algorithm
-   ECDS used from 2019-04-01
-   Der_Dupe_Flag used
-   selecting providers that code most activity

## 2021-12-09 meeting notes

### ED visits

data validation + type 1 only + switch from AE to ECDS for trends prior to
pandemic + assigning activity to STPs (CCG codes mix of 5 and 3 char) + x3
non-STP codes in NCDR lookup

activity trends + Gloucestershire STP coding everything as walkin + zscores,
(x - u) / sd (position/size of drop relative to own distribution/trend); red is
England + national lockdowns, how to divide up - wave 1, summer lull, wave 2,
gradual recovery + crude comparison - June-21 to Oct-31 (w25 - w43) June-21 step
4 all legal limits on social contact removed + uncoded activity (diagnosis
missing) + n = 138 providers + benchmark is code 80%+ of all visits in both
periods

crude comparison + t20 by visits from p2 compared with ref

refined comparison + u \> 1.5; u \< .7

### inpatient admissions

data validation

activity trends

social contact removed

## sql

substring index could be replaced with join to seperate diagnosis table

<https://www.theguardian.com/society/2021/dec/13/the-secret-nhs-trust-boss-the-strain-on-hospitals-is-visible-and-visceral>
This isn't just Covid, though Covid is a factor. And this isn't just about
people's difficulties accessing primary care. It's partly because people coming
into hospital are much more severely ill than before. Our stroke team say
they're seeing people coming in who haven't been accessing some of the
preventive interventions that might have stopped them getting so ill. And our
gastro-intestinal specialists are seeing far more people with stage 3 or stage 4
cancers than previously. So there are things that haven't been picked up. That
has a consequential impact on hospitals because patients' length of stay and
recovery period are greater. the missing 2 million what happened to people not
seen during peak of pandemic

<https://www.theguardian.com/society/2022/jan/12/rsv-virus-infection-uk-rise-children-struggling-to-breathe>

## DIABETES AUDIT

<https://www.diabetes.org.uk/professionals/resources/national-diabetes-audit>

# National Diabetes Audit

<https://digital.nhs.uk/data-and-information/clinical-audits-and-registries/national-diabetes-audit>

[MLCSU-BI-SQL] [LocalFeeds] [Reporting].[NationalDiabetesAudit_NDA_Core_Data]
