-- Record level inpatient admissions by key variables incl. diagnosis from SUS+ Live in NCDR warehouse
-- https://stackoverflow.com/questions/26926271/sql-get-iso-year-for-iso-week
-- F1 or ctrl + shift + p - MS SQL: Connect - complete connection profile
-- ctrl + shift + e to run the query from VS Code
-- F1 - MS SQL: Cancel query - to cancel a query while it's running

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP VIEW [dbo].[v_ip_871];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO

CREATE VIEW [dbo].[v_ip_871] AS

SELECT
	YEAR(DATEADD(day, 26 - DATEPART(ISO_WEEK, Discharge_Date), Discharge_Date)) isoyr,
	DATEPART(ISO_WEEK, Discharge_Date) isowk,
    CASE
		WHEN Sex = '1' THEN 'm'
		WHEN Sex = '2' THEN 'f'
		WHEN Sex IN ('9', '0', 'X') THEN 'NA'
		ELSE Sex
	END	sex,
	Age_on_Admission age,
	-- admission group algorithm
	CASE
		WHEN Patient_Classification IN ('3', '4') THEN 'reg'
		WHEN Admission_Method IN ('82', '83') THEN 'birth'
		WHEN (Admission_Method LIKE '3%' OR Treatment_Function_Code IN ('501', '560')) AND Age_on_Admission < 56 THEN 'maternity'
		WHEN (Age_on_Admission < 18 AND Admission_Method LIKE '2%') THEN 'paeds-emerg'
		WHEN (Age_on_Admission < 18 AND Admission_Method IN ('11', '12', '13') AND Patient_Classification = '1') THEN 'paeds-elec'
		WHEN Admission_Method LIKE '2%' THEN 'emerg'
		WHEN Admission_Method = '81' THEN 'transfer'
		WHEN Admission_Method IN ('11', '12', '13') AND Patient_Classification = '1' THEN 'ordelec'
		WHEN Admission_Method IN ('11', '12', '13') AND Patient_Classification = '2' THEN 'daycase'
		ELSE 'NA'
	END admigrp,
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
	Der_Primary_Diagnosis_Code diagcd,
	tb4.ICD10_L4_Code diagl4cd,
	tb4.ICD10_L4_Desc diagl4nm,
	tb4.ICD10_L3_Code diagl3cd,
    tb4.ICD10_L3_Desc diagl3nm,
    tb4.ICD10_L2_Code diagl2cd,
    tb4.ICD10_L2_Desc diagl2nm,
    tb4.ICD10_L1_Code diagl1cd,
    tb4.ICD10_L1_Desc diagl1nm,
    tb4.ICD10_Chapter_No chpno,
    tb4.ICD10_Chapter_Code chpcd,
    tb4.ICD10_Chapter_Desc chpnm,
	APCE_Ident id
	
FROM
	[NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCE] tb1
	-- some CCG codes are char(5) and include trailing 00
	LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies] tb2
		ON LEFT(tb1.Commissioner_Code, 3) = tb2.Organisation_Code
	LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies] tb3
		ON tb1.Commissioner_Code = tb3.Organisation_Code
	-- pull diagnosis codes
	LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_ClinCode_ICD10_5ed] tb4
		ON LEFT(tb1.Der_Primary_Diagnosis_Code, 4) = tb4.ICD10_L4_Code

WHERE
	Discharge_Date >= '2018-01-01'
	-- AND Discharge_Date <= '2022-03-31'
	AND APCS_Last_Ep_Ind = '1'

GO

