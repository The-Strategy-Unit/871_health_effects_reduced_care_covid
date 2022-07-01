-- Pull list of STP codes and names from ODS reference table in NCDR warehouse

USE [NHSE_SUSPlus_Live];
GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO

SELECT
    DISTINCT STP_Code stpcode,
    STP_Name stpname

FROM
    [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies];

GO

