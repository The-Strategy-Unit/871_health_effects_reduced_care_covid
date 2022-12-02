-- Procedure to create timeseries count for different admission groups from SUS+ Live in NCDR warehouse
-- p creates a grouping variable for 2 distinct user-defined time periods - data pulled from 'v_ip_871'

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP PROCEDURE [dbo].[p_get_ip_admigrps];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO

CREATE PROCEDURE [dbo].[p_get_ip_admigrps]
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
	sex,
	CASE
		WHEN age < 18 THEN 'child'
		WHEN age >= 110 THEN 'NA'
		WHEN age > 17 THEN 'adult'
		ELSE 'NA'
	END age,
	admigrp,
	COUNT(id) n_admi
	
FROM
	[NHSE_Sandbox_StrategyUnit].[dbo].[v_ip_871]

GROUP BY
	isoyr,
	isowk,
	CASE
		WHEN isoyr = 2019 AND isowk >= @StartWk AND isowk <= @EndWk THEN 'p1'
		WHEN isoyr = 2021 AND isowk >= @StartWk AND isowk <= @EndWk THEN 'p2'
		ELSE 'NA'
	END,
	-- stpcode,
	-- stpname,
	sex,
	CASE
		WHEN age < 18 THEN 'child'
		WHEN age >= 110 THEN 'NA'
		WHEN age > 17 THEN 'adult'
		ELSE 'NA'
	END,
	admigrp;

GO

USE [NHSE_Sandbox_StrategyUnit];
GO

DECLARE @StartWk INT,
		@EndWk INT;

SET @StartWk = 25;
SET @EndWk = 43;

EXEC [dbo].[p_get_ip_admigrps] @StartWk, @EndWk;

