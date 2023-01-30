-- Procedure to create timeseries count for inpatient emergency admissions for a set of diagnosis codes from SUS+ Live in NCDR warehouse
-- p creates a grouping variable for a set of diagnosis codes - data pulled from 'v_ip_871'

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP PROCEDURE [dbo].[p_get_ip_diags_set];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO

CREATE PROCEDURE [dbo].[p_get_ip_diags_set]
@StartWk INT,
@EndWk INT,
@DiagCodes NVARCHAR(400)

AS

SELECT
	isoyr,
	isowk,
	CASE
		WHEN isoyr = 2019 AND isowk >= @StartWk AND isowk <= @EndWk THEN 'p1'
		WHEN isoyr = 2021 AND isowk >= @StartWk AND isowk <= @EndWk THEN 'p2'
		ELSE 'NA'
	END tmper,
	--stpcode,
	--stpname,
	sex,
	age,
	admigrp,
	diagcd,
	diagl4cd,
	diagl4nm,
	diagl3cd,
    diagl3nm,
    diagl2cd,
    diagl2nm,
    diagl1cd,
    diagl1nm,
    chpno,
    chpcd,
    chpnm,
	COUNT(id) n_admi
	
FROM
	[NHSE_Sandbox_StrategyUnit].[dbo].[v_ip_871]

WHERE
	admigrp IN ('emerg', 'paeds-emerg')
	AND diagl4cd IN (SELECT VALUE FROM STRING_SPLIT(@DiagCodes, ','))

GROUP BY
	isoyr,
	isowk,
	CASE
		WHEN isoyr = 2019 AND isowk >= @StartWk AND isowk <= @EndWk THEN 'p1'
		WHEN isoyr = 2021 AND isowk >= @StartWk AND isowk <= @EndWk THEN 'p2'
		ELSE 'NA'
	END,
	--stpcode,
	--stpname,
	sex,
	age,
	admigrp,
	diagcd,
	diagl4cd,
	diagl4nm,
	diagl3cd,
    diagl3nm,
    diagl2cd,
    diagl2nm,
    diagl1cd,
    diagl1nm,
    chpno,
    chpcd,
    chpnm;

GO

USE [NHSE_Sandbox_StrategyUnit];
GO

DECLARE @StartWk INT,
		@EndWk INT,
		@DiagCodes NVARCHAR(400);

SET @StartWk = 25;
SET @EndWk = 43;

-- alcohol
--SET @DiagCodes = 'K700,E538,K858,K704,K292,E512,K760,K703,K746';
-- pregnancy
--SET @DiagCodes = 'O368,O046,Z351,O088,O211,O219,O036,O281';
-- social isolation
--SET @DiagCodes = 'R930';
-- late presentation
--SET @DiagCodes = 'C672,D500,R935,K822,E878,C629,I421,K352,D630';
-- exacerbation
--SET @DiagCodes = 'I132,I130,E141,T403,E611,E113,N800,E274,E103,I509,N921,E111,D509,Z436,I083,Z092,K510,N813,I10X,K433,E161,I208,I710,E149,E272,E271,G834,E114,D638,E871,R030,J849';
-- back conditions
--SET @DiagCodes = 'M484,M472';
-- postoperative problems
--SET @DiagCodes = 'Z488,Z090,Z480,T835,T831';
-- common infections
SET @DiagCodes = 'J210,J121,J211,J123,B341,J218,J219,J122,B000';
-- eating disorders
--SET @DiagCodes = 'F509,F508,F500,R630';

EXEC [dbo].[p_get_ip_diags_set] @StartWk, @EndWk, @DiagCodes;

GO

