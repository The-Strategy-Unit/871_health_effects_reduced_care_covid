

-- core data table is unique (key) PatientId + AUDIT_YEAR
select
	distinct(PatientId), count(CSURowNumber) cnt
from
	[LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
where
	AUDIT_YEAR = '202122E2'
group by
	PatientId
order by
	cnt

-- BP table is different
select
	distinct(PatientId), count(CSURowNumber) cnt
from
	[LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_BP]
where
	AUDIT_YEAR = '202122E2'
group by
	PatientId
order by
	cnt

-- for example, this patient seemingly has their BP taken every day 
select * from [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_BP] where AUDIT_YEAR = '202122E2' and PatientId = '61295727' order by BP_DATE
-- only the most recent BP date should appear in the core data table
select * from [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] where AUDIT_YEAR = '202122E2' and PatientId = '61295727'




-- new diagnoses type-2 diabetes, mx timeseries
SELECT
	MIN(CONVERT(DATE, CLEAN_DIAGNOSIS_DATE)),
	MAX(CONVERT(DATE, CLEAN_DIAGNOSIS_DATE))
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
WHERE
	COUNTRY = 'England'
	AND AUDIT_YEAR = '202021'
	AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) >= '2020-01-01'

SELECT
	AUDIT_YEAR,
	MIN(CONVERT(DATE, CLEAN_DIAGNOSIS_DATE)),
	MAX(CONVERT(DATE, CLEAN_DIAGNOSIS_DATE))
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
WHERE
	COUNTRY = 'England'
GROUP BY
	AUDIT_YEAR
ORDER BY
	AUDIT_YEAR

SELECT
	CASE
		WHEN AUDIT_YEAR = '202122E2' THEN '202122'
		ELSE AUDIT_YEAR
	END audityr,
	LEFT(CLEAN_DIAGNOSIS_DATE, 4) + '-' + SUBSTRING(CLEAN_DIAGNOSIS_DATE, 6, 2) dt,
	COUNT(*) cases
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
WHERE
	COUNTRY = 'England'
	AND DERIVED_CLEAN_DIABETES_TYPE = '2'
	AND CLEAN_DIAGNOSIS_DATE != ''
	AND AUDIT_YEAR NOT IN ('202021E4', '202122E1')
	AND (
		(AUDIT_YEAR = '201415' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) >= '2014-01-01' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) <= '2014-12-31')
		OR
		(AUDIT_YEAR = '201516' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) >= '2015-01-01' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) <= '2015-12-31')
		OR
		(AUDIT_YEAR = '201617' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) >= '2016-01-01' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) <= '2016-12-31')
		OR
		(AUDIT_YEAR = '201718' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) >= '2017-01-01' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) <= '2017-12-31')
		OR
		(AUDIT_YEAR = '201819' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) >= '2018-01-01' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) <= '2018-12-31')
		OR
		(AUDIT_YEAR = '201920' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) >= '2019-01-01' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) <= '2019-12-31')
		OR
		(AUDIT_YEAR = '202021' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) >= '2020-01-01' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) <= '2020-12-31')
		OR
		AUDIT_YEAR = '202122E2' AND CONVERT(DATE, CLEAN_DIAGNOSIS_DATE) >= '2021-01-01' 
	)
GROUP BY
	CASE
		WHEN AUDIT_YEAR = '202122E2' THEN '202122'
		ELSE AUDIT_YEAR
	END,
	LEFT(CLEAN_DIAGNOSIS_DATE, 4) + '-' + SUBSTRING(CLEAN_DIAGNOSIS_DATE, 6, 2)
ORDER BY
	audityr,
	dt




-- BP measurements taken, mx timeseries
SELECT
	audityr,
	dt,
	COUNT(id) bpmts
FROM
	(
SELECT
	CASE
		WHEN tb1.AUDIT_YEAR = '202122E2' THEN '202122'
		ELSE tb1.AUDIT_YEAR
	END audityr,
	LEFT(tb1.BP_DATE, 4) + '-' + SUBSTRING(tb1.BP_DATE, 6, 2) dt,
	tb1.PatientId id,
	tb2.*
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_BP] tb1
	LEFT OUTER JOIN [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb2
		 ON tb1.PatientId = tb2.PatientId AND tb1.AUDIT_YEAR = tb2.AUDIT_YEAR
WHERE
	tb2.COUNTRY = 'England'
	AND tb2.DERIVED_CLEAN_DIABETES_TYPE = '2'
	AND (tb1.AUDIT_YEAR != '202122E1' AND tb1.SubmittedFile != 'NDA_BP_Extract_2020_21_E4.csv')
	AND (tb1.CLEAN_SYSTOLIC_VALUE IS NOT NULL AND tb1.CLEAN_DIASTOLIC_VALUE IS NOT NULL)
	AND (
		(tb1.AUDIT_YEAR = '201617' AND CONVERT(DATE, tb1.BP_DATE) <= '2016-12-31')
		OR
		(tb1.AUDIT_YEAR = '201718' AND CONVERT(DATE, tb1.BP_DATE) <= '2017-12-31')
		OR
		(tb1.AUDIT_YEAR = '201819' AND CONVERT(DATE, tb1.BP_DATE) <= '2018-12-31')
		OR
		(tb1.AUDIT_YEAR = '201920' AND CONVERT(DATE, tb1.BP_DATE) <= '2019-12-31')
		OR
		(tb1.AUDIT_YEAR = '202021' AND CONVERT(DATE, tb1.BP_DATE) <= '2020-12-31')
		OR
		tb1.AUDIT_YEAR = '202122E2'
	)
) tba
GROUP BY
	audityr,
	dt
ORDER BY
	audityr,
	dt




-- HBa1C measurements taken, mx timeseries
SELECT
	audityr,
	dt,
	COUNT(id) hbamts
FROM
	(
SELECT
	CASE
		WHEN tb1.AUDIT_YEAR = '202122E2' THEN '202122'
		ELSE tb1.AUDIT_YEAR
	END audityr,
	CAST(YEAR(tb1.HBA1C_DATE) AS VARCHAR) + '-' + RIGHT('0' + CAST(MONTH(tb1.HBA1C_DATE) AS VARCHAR(2)), 2) dt,
	tb1.PatientId id,
	tb2.*
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_HBA1C] tb1
	LEFT OUTER JOIN [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb2
		 ON tb1.PatientId = tb2.PatientId AND tb1.AUDIT_YEAR = tb2.AUDIT_YEAR
WHERE
	tb2.COUNTRY = 'England'
	AND tb2.DERIVED_CLEAN_DIABETES_TYPE = '2'
	AND (tb1.AUDIT_YEAR != '202122E1' AND tb1.SubmittedFile != 'NDA_BP_Extract_2020_21_E4.csv')
	AND (tb1.CLEAN_MMOL_HBA1C_VALUE IS NOT NULL)
	AND (
		(tb1.AUDIT_YEAR = '201617' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2016-12-31')
		OR
		(tb1.AUDIT_YEAR = '201718' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2017-12-31')
		OR
		(tb1.AUDIT_YEAR = '201819' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2018-12-31')
		OR
		(tb1.AUDIT_YEAR = '201920' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2019-12-31')
		OR
		(tb1.AUDIT_YEAR = '202021' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2020-12-31')
		OR
		tb1.AUDIT_YEAR = '202122E2'
	)
) tba
GROUP BY
	audityr,
	dt
ORDER BY
	audityr,
	dt




-- of all HBa1C measurements taken what proportion report value lower than or equal to 58 
SELECT
	audityr,
	dt,
	SUM(
		CASE WHEN hba <= 58 THEN 1
		ELSE 0
		END
	) lowhba,
	COUNT(id) hbamts
FROM
	(
SELECT
	CASE
		WHEN tb1.AUDIT_YEAR = '202122E2' THEN '202122'
		ELSE tb1.AUDIT_YEAR
	END audityr,
	CAST(YEAR(tb1.HBA1C_DATE) AS VARCHAR) + '-' + RIGHT('0' + CAST(MONTH(tb1.HBA1C_DATE) AS VARCHAR(2)), 2) dt,
	tb1.PatientId id,
	CONVERT(INT, CAST(tb1.CLEAN_MMOL_HBA1C_VALUE AS FLOAT)) hba,
	tb2.*
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_HBA1C] tb1
	LEFT OUTER JOIN [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb2
		 ON tb1.PatientId = tb2.PatientId AND tb1.AUDIT_YEAR = tb2.AUDIT_YEAR
WHERE
	tb2.COUNTRY = 'England'
	AND tb2.DERIVED_CLEAN_DIABETES_TYPE = '2'
	AND (tb1.AUDIT_YEAR != '202122E1' AND tb1.SubmittedFile != 'NDA_BP_Extract_2020_21_E4.csv')
	AND (tb1.CLEAN_MMOL_HBA1C_VALUE IS NOT NULL)
	AND (
		(tb1.AUDIT_YEAR = '201617' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2016-12-31')
		OR
		(tb1.AUDIT_YEAR = '201718' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2017-12-31')
		OR
		(tb1.AUDIT_YEAR = '201819' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2018-12-31')
		OR
		(tb1.AUDIT_YEAR = '201920' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2019-12-31')
		OR
		(tb1.AUDIT_YEAR = '202021' AND CONVERT(DATE, tb1.HBA1C_DATE) <= '2020-12-31')
		OR
		tb1.AUDIT_YEAR = '202122E2'
	)
) tba
GROUP BY
	audityr,
	dt
ORDER BY
	audityr,
	dt


-- mean BP measurements
SELECT
	audityr,
	dt,
	AVG(sbp) mnsbp,
	AVG(dbp) mndbp,
	COUNT(id) bpmts
FROM
	(
SELECT
	CASE
		WHEN tb1.AUDIT_YEAR = '202122E2' THEN '202122'
		ELSE tb1.AUDIT_YEAR
	END audityr,
	CAST(YEAR(tb1.BP_DATE) AS VARCHAR) + '-' + RIGHT('0' + CAST(MONTH(tb1.BP_DATE) AS VARCHAR(2)), 2) dt,
	tb1.PatientId id,
	CAST(tb1.[CLEAN_SYSTOLIC_VALUE] AS FLOAT) sbp,
	CAST(tb1.[CLEAN_DIASTOLIC_VALUE] AS FLOAT) dbp,
	tb2.*
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_BP] tb1
	LEFT OUTER JOIN [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb2
		 ON tb1.PatientId = tb2.PatientId AND tb1.AUDIT_YEAR = tb2.AUDIT_YEAR
WHERE
	tb2.COUNTRY = 'England'
	AND tb2.DERIVED_CLEAN_DIABETES_TYPE = '2'
	AND (tb1.AUDIT_YEAR != '202122E1' AND tb1.SubmittedFile != 'NDA_BP_Extract_2020_21_E4.csv')
	AND (tb1.CLEAN_SYSTOLIC_VALUE IS NOT NULL)
	AND (tb1.CLEAN_DIASTOLIC_VALUE IS NOT NULL)
	AND (
		(tb1.AUDIT_YEAR = '201617' AND CONVERT(DATE, tb1.BP_DATE) <= '2016-12-31')
		OR
		(tb1.AUDIT_YEAR = '201718' AND CONVERT(DATE, tb1.BP_DATE) <= '2017-12-31')
		OR
		(tb1.AUDIT_YEAR = '201819' AND CONVERT(DATE, tb1.BP_DATE) <= '2018-12-31')
		OR
		(tb1.AUDIT_YEAR = '201920' AND CONVERT(DATE, tb1.BP_DATE) <= '2019-12-31')
		OR
		(tb1.AUDIT_YEAR = '202021' AND CONVERT(DATE, tb1.BP_DATE) <= '2020-12-31')
		OR
		tb1.AUDIT_YEAR = '202122E2'
	)
) tba
GROUP BY
	audityr,
	dt
ORDER BY
	audityr,
	dt











-- HBa1C measurements per person ....
-- do patientIDs map over audit years?
--has previous records
SELECT
	tba.*,
	tb2.AUDIT_YEAR,
	tb2.DERIVED_CLEAN_BIRTH_YEAR,
	tb2.DERIVED_CLEAN_DIAGNOSIS_YEAR
FROM (
SELECT
	DISTINCT patientID id,
	DERIVED_CLEAN_BIRTH_YEAR,
	DERIVED_CLEAN_DIAGNOSIS_YEAR
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb1
WHERE
	AUDIT_YEAR = '202122E2'
	AND PatientId = '33153039'
) tba
	LEFT OUTER JOIN [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb2
	ON tba.id = tb2.PatientId AND tb2.AUDIT_YEAR NOT IN ('202122E1', '202122E2')

-- no previous records
SELECT
	tba.*,
	tb2.AUDIT_YEAR,
	tb2.DERIVED_CLEAN_BIRTH_YEAR,
	tb2.DERIVED_CLEAN_DIAGNOSIS_YEAR
FROM (
SELECT
	DISTINCT patientID id,
	DERIVED_CLEAN_BIRTH_YEAR,
	DERIVED_CLEAN_DIAGNOSIS_YEAR
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb1
WHERE
	AUDIT_YEAR = '202122E2'
	AND PatientId = '57672780'
) tba
	LEFT JOIN [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb2
	ON tba.id = tb2.PatientId AND tb2.AUDIT_YEAR NOT IN ('202122E1', '202122E2')

--do they look like the same people?
SELECT
	tba.*,
	tb2.AUDIT_YEAR,
	tb2.DERIVED_CLEAN_BIRTH_YEAR,
	tb2.DERIVED_CLEAN_DIAGNOSIS_YEAR,
	tba.DERIVED_CLEAN_BIRTH_YEAR - tb2.DERIVED_CLEAN_BIRTH_YEAR chk
FROM (
SELECT
	DISTINCT patientID id,
	DERIVED_CLEAN_BIRTH_YEAR,
	DERIVED_CLEAN_DIAGNOSIS_YEAR
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb1
WHERE
	AUDIT_YEAR = '202122E2'
) tba
	LEFT JOIN [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb2
	ON tba.id = tb2.PatientId AND tb2.AUDIT_YEAR NOT IN ('202122E1', '202122E2')
WHERE
	tba.DERIVED_CLEAN_BIRTH_YEAR - tb2.DERIVED_CLEAN_BIRTH_YEAR != 0
ORDER BY
	id,
	chk

--2 types - but overall small proportion of all records
--birth year and diagnosis year are zeros previously
select * from [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] where patientId = '45565411' order by AUDIT_YEAR
--birth year and diagnosis year are populated previously but with a different value
select * from [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] where patientId = '166353513' order by AUDIT_YEAR



--duplicates?!
select * from [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] where PatientId = '55783908'
select * from [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_BP] where PatientId = '55783908'

--audit year does not match submitted file!
select [AUDIT_YEAR], SubmittedFile, count(*) from [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_BP] group by [AUDIT_YEAR], SubmittedFile

select [AUDIT_YEAR], SubmittedFile, count(*) from [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_HBA1C] group by [AUDIT_YEAR], SubmittedFile

--join to DPP
SELECT 
      tb1.*, tb2.*
      
  FROM [LocalFeeds].[Reporting].[National_NDADPP_GoldenRecord] tb1
  left outer join [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb2
  on tb1.PatientId = tb2.PatientId
  
 WHERE
	tb1.patientID = '51908481'


--can i link to IP/OP/AE?
--looks not!
	SELECT 
      tb1.*, tb2.*
      
  FROM [LocalFeeds].[Reporting].[National_NDADPP_GoldenRecord] tb1
  left outer join [EAT_Reporting].[dbo].[tbOutpatient] tb2
  on tb1.PatientId = tb2.[NHSNumber]
  
 WHERE
	tb1.patientID = '51908481'

	