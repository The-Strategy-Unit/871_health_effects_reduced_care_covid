-- Procedure to create timeseries count for ED visits for a set of diagnosis codes from SUS+ Live in NCDR warehouse
-- p creates a grouping variable for a set of diagnosis codes - data pulled from 'v_ip_871'

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP PROCEDURE [dbo].[p_get_ed_diags_set];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO

CREATE PROCEDURE [dbo].[p_get_ed_diags_set]
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
	-- stpcode,
	-- stpname,
	dataset,
	procd,
	sex,
	age,
	arrmode,
	diagcd,
	diagnm,
	COUNT(id) n_visits

FROM
	[NHSE_Sandbox_StrategyUnit].[dbo].[v_ed_871]

WHERE
    diagcd IN (SELECT VALUE FROM STRING_SPLIT(@DiagCodes, ','))

GROUP BY
	isoyr,
	isowk,
	CASE
		WHEN isoyr = 2019 AND isowk >= @StartWk AND isowk <= @EndWk THEN 'p1'
		WHEN isoyr = 2022 AND isowk >= @StartWk AND isowk <= @EndWk THEN 'p2'
		ELSE 'NA'
	END,
	-- stpcode,
	-- stpname,
	dataset,
	procd,
	sex,
	age,
	arrmode,
	diagcd,
	diagnm;

GO

USE [NHSE_Sandbox_StrategyUnit];
GO

DECLARE @StartWk INT,
		@EndWk INT,
		@DiagCodes NVARCHAR(400);

SET @StartWk = 25;
SET @EndWk = 43;

-- eating disorders
--SET @DiagCodes = '72366004'
-- closed fracture of nasal bones
--SET @DiagCodes = '81639003'
-- radiculopathy
SET @DiagCodes = ('128196005,54404000')

EXEC [dbo].[p_get_ed_diags_set]@StartWk, @EndWk, @DiagCodes;

GO

