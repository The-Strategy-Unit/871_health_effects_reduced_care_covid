-- Record level ED visits by key variables incl. diagnosis from ECDS in NCDR warehouse
-- https://stackoverflow.com/questions/26926271/sql-get-iso-year-for-iso-week
-- F1 or ctrl + shift + p - MS SQL: Connect - complete connection profile
-- ctrl + shift + e to run the query from VS Code
-- F1 - MS SQL: Cancel query - to cancel a query while it's running

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP VIEW [dbo].[v_ed_871];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO

CREATE VIEW [dbo].[v_ed_871] AS

SELECT
	YEAR(DATEADD(day, 26 - DATEPART(ISO_WEEK, Arrival_Date), Arrival_Date)) isoyr,
	DATEPART(ISO_WEEK, Arrival_Date) isowk,
	CASE
		WHEN Sex = '1' THEN 'm'
		WHEN Sex = '2' THEN 'f'
		WHEN Sex IN ('0', '9') THEN 'NA'
		ELSE Sex
	END	sex,
	Age_at_Start_of_Episode age,
	-- arrival mode algorithm (NULLS to walkin)
	CASE
		WHEN AEA_Arrival_Mode = '1' THEN 'amb'
		WHEN AEA_Arrival_Mode = '2' THEN 'walkin'
		WHEN AEA_Arrival_Mode IS NULL THEN 'walkin'
		ELSE AEA_Arrival_Mode
	END arrmode,
	-- Commissioner_Code,
	CASE
		WHEN coalesce(tb3.STP_Code, tb2.STP_Code) IS NULL THEN Commissioner_Code
		ELSE coalesce(tb3.STP_Code, tb2.STP_Code) 
	END stpcode,
	CASE
		WHEN coalesce(tb3.STP_Name, tb2.STP_Name) IS NULL THEN Commissioner_Code
		ELSE coalesce(tb3.STP_Name, tb2.STP_Name)
	END stpname,
    Provider_Code procd,
	'NA' diagcd,
	'NA' diagnm,
	'aea' dataset,
	AEA_Ident id

FROM
	[NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_AEA] tb1
		-- some CCG codes are char(5) and include trailing 00
	LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies] tb2
		ON LEFT(tb1.Commissioner_Code, 3) = tb2.Organisation_Code
	LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies] tb3
		ON tb1.Commissioner_Code = tb3.Organisation_Code
WHERE
	(AEA_Department_Type = '01' OR AEA_Department_Type = '1')
	AND Arrival_Date >= '2018-01-01'
	AND Arrival_Date <= '2019-03-31'

UNION ALL

SELECT
	YEAR(DATEADD(day, 26 - DATEPART(ISO_WEEK, Arrival_Date), Arrival_Date)) isoyr,
	DATEPART(ISO_WEEK, Arrival_Date) isowk,
	CASE
		WHEN Sex = '1' THEN 'm'
		WHEN Sex = '2' THEN 'f'
		WHEN Sex IN ('9', 'X') THEN 'NA'
		ELSE Sex
	END	sex,
	Age_At_Arrival age,
	-- arrival mode algorithm (NULLS to walkin)
	CASE
		WHEN EC_Arrival_Mode_SNOMED_CT IN ('1048031000000100', '1048081000000101', '1048021000000102', '1048051000000107', '1048041000000109') THEN 'amb'
		WHEN EC_Arrival_Mode_SNOMED_CT IN ('1047991000000102', '1048071000000103', '1048061000000105', '1048001000000106') THEN 'walkin'
		WHEN EC_Arrival_Mode_SNOMED_CT IS NULL THEN 'walkin'
		ELSE EC_Arrival_Mode_SNOMED_CT
	END arrmode,
	-- Commissioner_Code,
	CASE
		WHEN coalesce(tb3.STP_Code, tb2.STP_Code) IS NULL THEN Commissioner_Code
		ELSE coalesce(tb3.STP_Code, tb2.STP_Code) 
	END stpcode,
	CASE
		WHEN coalesce(tb3.STP_Name, tb2.STP_Name) IS NULL THEN Commissioner_Code
		ELSE coalesce(tb3.STP_Name, tb2.STP_Name)
	END stpname,
    Provider_Code procd,
	[NHSE_Sandbox_StrategyUnit].[dbo].[f_substring_index](Der_EC_Diagnosis_All, ',', 1) diagcd,
	tb4.DiagnosisDescription diagnm,
    'ecds' dataset,
	EC_Ident id

FROM
	[NHSE_SUSPlus_Live].[dbo].[tbl_Data_SUS_EC] tb1
	-- some CCG codes are char(5) and include trailing 00
	LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies] tb2
		ON LEFT(tb1.Commissioner_Code, 3) = tb2.Organisation_Code
	LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies] tb3
		ON tb1.Commissioner_Code = tb3.Organisation_Code
    -- pull diagnosis codes
	LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Diagnosis] tb4
		ON [NHSE_Sandbox_StrategyUnit].[dbo].[f_substring_index](Der_EC_Diagnosis_All, ',', 1) = tb4.DiagnosisCode

WHERE 	
	(EC_Department_Type = '01' OR EC_Department_Type = '1')
	AND Der_Dupe_Flag = 0
	AND Arrival_Date >= '2019-04-01'
	AND Arrival_Date <= '2022-03-31';

GO

