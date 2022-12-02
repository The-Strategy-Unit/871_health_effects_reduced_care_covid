
USE [LocalFeeds];
GO


SELECT 
	[AUDIT_YEAR],
	COUNT(*)
FROM
	[LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
GROUP BY
	[AUDIT_YEAR]
ORDER BY
	[AUDIT_YEAR]


SELECT
	AUDIT_YEAR,
	ReceivedDate,
	COUNT(*)
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
GROUP BY
	AUDIT_YEAR,
	ReceivedDate
ORDER BY
	ReceivedDate


--matches NHSD 
SELECT
	DERIVED_CLEAN_DIABETES_TYPE,
	COUNT(*)
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
WHERE
	AUDIT_YEAR = '202021'
	AND COUNTRY = 'England'
GROUP BY
	DERIVED_CLEAN_DIABETES_TYPE
ORDER BY
	DERIVED_CLEAN_DIABETES_TYPE


SELECT
	CASE
		WHEN AUDIT_YEAR = '202122E1' THEN '202122'
		ELSE AUDIT_YEAR
	END audityr,
	LEFT(CLEAN_DIAGNOSIS_DATE, 4) + '-' + SUBSTRING(CLEAN_DIAGNOSIS_DATE, 6, 2) dt,
	COUNT(*) cases
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
WHERE
	COUNTRY = 'England'
	AND DERIVED_CLEAN_DIABETES_TYPE = '2'
	AND CLEAN_DIAGNOSIS_DATE != ''
	AND AUDIT_YEAR != '202021E4'
GROUP BY
	CASE
		WHEN AUDIT_YEAR = '202122E1' THEN '202122'
		ELSE AUDIT_YEAR
	END,
	LEFT(CLEAN_DIAGNOSIS_DATE, 4) + '-' + SUBSTRING(CLEAN_DIAGNOSIS_DATE, 6, 2)
ORDER BY
	audityr,
	dt


SELECT
	CASE
		WHEN AUDIT_YEAR = '202122E1' THEN '202122'
		ELSE AUDIT_YEAR
	END audityr,
	COUNT([PatientId])
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
WHERE
	COUNTRY = 'England'
	AND DERIVED_CLEAN_DIABETES_TYPE = '2'
	AND CLEAN_DIAGNOSIS_DATE != ''
	AND AUDIT_YEAR != '202021E4'
GROUP BY
	CASE
		WHEN AUDIT_YEAR = '202122E1' THEN '202122'
		ELSE AUDIT_YEAR
	END
ORDER BY
	audityr


SELECT
	CASE
		WHEN AUDIT_YEAR = '202122E1' THEN '202122'
		ELSE AUDIT_YEAR
	END audityr,
	COUNT(CSURowNumber) measures,
	COUNT(DISTINCT(PatientId)) pats
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Cholesterol]
WHERE
	AUDIT_YEAR != '202021E4'
GROUP BY
	CASE
		WHEN AUDIT_YEAR = '202122E1' THEN '202122'
		ELSE AUDIT_YEAR
	END
ORDER BY
	audityr


SELECT
	CASE
		WHEN AUDIT_YEAR = '202122E1' THEN '202122'
		ELSE AUDIT_YEAR
	END audityr,
	SUM([HBA1C_&lt;=7.5%OR58MMOL]) lt58,
	COUNT(PatientId) pats
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
WHERE
	COUNTRY = 'England'
	AND DERIVED_CLEAN_DIABETES_TYPE = '2'
	AND CLEAN_DIAGNOSIS_DATE != ''
	AND AUDIT_YEAR != '202021E4'
GROUP BY
	CASE
		WHEN AUDIT_YEAR = '202122E1' THEN '202122'
		ELSE AUDIT_YEAR
	END
ORDER BY
	audityr

-- looks useful
select * from [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] where AUDIT_YEAR = '202122E1'

SELECT
	tb1.*,
	tb2.*
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] tb1
	LEFT OUTER JOIN [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_BP] tb2
		 ON tb1.PatientId = tb2.PatientId
WHERE
	tb1.PatientId = '55783908'


--matches NHSD 
SELECT
	AUDIT_YEAR,
	MAX(CONVERT(DATE, CLEAN_DIAGNOSIS_DATE))
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
WHERE
	COUNTRY = 'England'
GROUP BY
	AUDIT_YEAR
ORDER BY
	AUDIT_YEAR

SELECT
	--CCG_LHB_CODE,
	--COUNT(*)
	*
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data]
WHERE
	AUDIT_YEAR = '202122E1'
	AND COUNTRY = 'England'
	AND DERIVED_CLEAN_DIABETES_TYPE = '1'
	AND CCG_LHB_CODE = '00N'


GROUP BY
	CCG_LHB_CODE
