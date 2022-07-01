-- Procedure to compare diagnoses counts for ED visits in 2 time periods from ECDS in NCDR warehouse
-- p creates a grouping variable for 2 distinct user-defined time periods - data pulled from 'v_ed_871'

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP PROCEDURE [dbo].[p_get_ed_diags];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO

CREATE PROCEDURE [dbo].[p_get_ed_diags]
@StartWk INT,
@EndWk INT

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
	CASE
		WHEN age < 18 THEN 'child'
		WHEN age >= 700 THEN 'child'
		WHEN age >= 110 THEN 'NA'
		WHEN age > 17 THEN 'adult'
		ELSE 'NA'
	END age,
	arrmode,
	diagcd,
	diagnm,
	COUNT(id) n_visits

FROM
	[NHSE_Sandbox_StrategyUnit].[dbo].[v_ed_871]

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
	CASE
		WHEN age < 18 THEN 'child'
		WHEN age >= 700 THEN 'child'
		WHEN age >= 110 THEN 'NA'
		WHEN age > 17 THEN 'adult'
		ELSE 'NA'
	END,
	arrmode,
	diagcd,
	diagnm;

GO

USE [NHSE_Sandbox_StrategyUnit];
GO

DECLARE @StartWk INT,
		@EndWk INT;

SET @StartWk = 25;
SET @EndWk = 43;

EXEC [dbo].[p_get_ed_diags] @StartWk, @EndWk;

