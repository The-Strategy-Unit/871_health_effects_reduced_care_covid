-- Function to reproduce the functionality of SUBSTRING_INDEX from MySql
-- f saved in [NHSE_Sandbox_StrategyUnit] db in NCDR warehouse
-- https://stackoverflow.com/questions/23854724/sql-server-equivalent-of-substring-index-function-in-mysql

USE [NHSE_Sandbox_StrategyUnit];
GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO
SET NOCOUNT ON;
GO

-- function to reproduce the functionality of SUBSTRING_INDEX from MySql
CREATE FUNCTION dbo.f_substring_index(@InString  NVARCHAR(Max),
                                      @Delimiter NVARCHAR(Max),
                                      @Count     INT)
RETURNS NVARCHAR(200)
AS
BEGIN
    DECLARE @Pos INT;
    DECLARE @DelimiterOffsets TABLE
    (
         i      INT IDENTITY(1, 1) NOT NULL,
         offset INT NOT NULL
    );

    -- if @Count is zero, we return '' as per spec
    IF @Count = 0
    BEGIN
        RETURN '';
    END;

    DECLARE @OrigLength      INT = LEN(@InString);
    DECLARE @DelimiterLength INT = LEN(@Delimiter);

    -- prime the pump
    SET @Pos = Charindex(@Delimiter, @InString, 1);

    -- if the delimiter does not exist in @InString, return the whole string
    IF @Pos = 0
    BEGIN
        RETURN @InString;
    END;

    -- put all delimiter offsets into @DelimiterOffsets, they get numbered automatically
    DECLARE @CurrentOffset INT = 0;
    WHILE @Pos > 0
    BEGIN
        SET @CurrentOffset = @Pos;

        INSERT INTO @DelimiterOffsets
                    (offset)
             VALUES (@CurrentOffset);

        SET @Pos = Charindex(@Delimiter, @InString, @CurrentOffset + @DelimiterLength);
    END;

    -- this number is guaranteed to be > 0.
    DECLARE @DelimitersFound INT = (SELECT Count(*) FROM @DelimiterOffsets);

    -- if they requested more delimiters than were found, return the whole string, as per spec
    IF Abs(@Count) > @DelimitersFound
    BEGIN
        RETURN @InString;
    END;

    DECLARE @StartSubstring INT = 0;
    DECLARE @EndSubstring   INT = @OrigLength;

    -- OK, now return the part they requested
    IF @Count > 0
    BEGIN
        SET @EndSubstring = (SELECT offset 
                               FROM @DelimiterOffsets 
                              WHERE i = @Count);
    END
    ELSE
    BEGIN
        SET @StartSubstring = (SELECT offset + @DelimiterLength 
                                 FROM @DelimiterOffsets 
                                WHERE i = (@DelimitersFound + @Count + 1));
    END;

    RETURN Substring(@InString, @StartSubstring, @EndSubstring);
END; 

