-- Synthetic Data Project
-- SQL scripts to generate objects referenced in verification measure functions
-- Author:
-- Version:  5/14/2017

-- Copyright 2017

-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
-- associated documentation files (the "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
-- following conditions:

-- The above copyright notice and this permission notice shall be included in all copies or substantial
-- portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
-- LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-- authentic OPM observation table
create table [dbo].[CPDFDODStatusJdF2012](
	[ObsID] [int] NOT NULL,
	[PseudoID] [int] NOT NULL,
	[FY] [smallint] NOT NULL,
	[Agency] [varchar](4) NOT NULL,
	[FileDate] [smalldatetime] NOT NULL,
	[WorkSchedule] [varchar](1) NOT NULL,
	[Tenure] [varchar](1) NOT NULL,
	[TypeAppointment] [varchar](2) NOT NULL,
	[PositionOccupied] [varchar](1) NOT NULL,
	[FLSA] [varchar](1) NOT NULL,
	[BargainingUnit] [varchar](4) NOT NULL,
	[SupervisoryStatus] [varchar](1) NOT NULL,
	[FunctionalClass] [varchar](2) NOT NULL,
	[Occupation] [varchar](4) NOT NULL,
	[OccupationalCategory] [varchar](1) NOT NULL,
	[PayPlan] [varchar](2) NOT NULL,
	[Grade] [varchar](2) NOT NULL,
	[StepRate] [varchar](2) NOT NULL,
	[PayStatus] [varchar](1) NOT NULL,
	[PayRateDeterminant] [varchar](1) NOT NULL,
	[SpecialPayTableID] [varchar](4) NOT NULL,
	[PayBasis] [varchar](2) NOT NULL,
	[BasicPay] [real] NOT NULL,
	[LocalityAdjustment] [real] NOT NULL,
	[AdjustedBasicPay] [real] NOT NULL,
	[SupervisoryDifferential] [real] NOT NULL,
	[ServiceComputationDate] [smalldatetime] NULL,
	[RetentionAllowance] [real] NOT NULL,
	[RatingOfRecordPattern] [varchar](1) NOT NULL,
	[RatingOfRecordPeriod] [varchar](6) NOT NULL,
	[DutyStation] [varchar](9) NOT NULL,
	[CSA] [varchar](3) NOT NULL,
	[CMSARetro] [varchar](2) NOT NULL,
	[LocalityPayArea] [varchar](2) NOT NULL,
	[EducationLevel] [varchar](2) NOT NULL,
	[InstructionalProgram] [varchar](6) NOT NULL,
	[CreditableMilitaryService] [smallint] NULL,
	[AgeRange] [varchar](6) NOT NULL,
	[YearsSinceDegreeRange] [varchar](6) NOT NULL,
 CONSTRAINT [PKCPDFDODStatusJdF2012ObsID] PRIMARY KEY NONCLUSTERED 
(
	[ObsID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]


-- authentic OPM observation supplemental columns (JdF 2014 FOIA request)
create table [dbo].[CPDFNonDODStatusJdF2014](
	[ObsID] [int] NOT NULL,
	[PoliticalAppointeeType] [varchar](1) NOT NULL,
	[Sex] [varchar](1) NOT NULL,
	[Race] [varchar](1) NOT NULL,
	[ERIBridge] [varchar](1) NOT NULL,
 CONSTRAINT [PKCPDFNonDODStatusJdF2014ObsID] PRIMARY KEY CLUSTERED 
(
	[ObsID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]


-- synthetic observation table
create table [dbo].[CPDFNonDODStatusDIBBS2017v0_07](
	[ObsID] [int] NOT NULL,
	[PseudoID] [int] NOT NULL,
	[FY] [smallint] NOT NULL,
	[Agency] [varchar](4) NOT NULL,
	[Sex] [varchar](1) NOT NULL,
	[Race] [varchar](1) NOT NULL,
	[ERIBridge] [varchar](1) NOT NULL,
	[EducationLevel] [varchar](2) NOT NULL,
	[AgeRange] [varchar](6) NOT NULL,
	[YearsSinceDegreeRange] [varchar](6) NOT NULL,
	[CreditableMilitaryService] [varchar](2) NOT NULL,
	[Occupation] [varchar](4) NOT NULL,
	[InstructionalProgram] [varchar](6) NOT NULL,
	[OccupationalCategory] [varchar](1) NOT NULL,
	[FunctionalClass] [varchar](2) NOT NULL,
	[FLSA] [varchar](1) NOT NULL,
	[TypeAppointment] [varchar](2) NOT NULL,
	[PoliticalAppointeeType] [varchar](2) NOT NULL,
	[PositionOccupied] [varchar](1) NOT NULL,
	[Tenure] [varchar](1) NOT NULL,
	[SupervisoryStatus] [varchar](1) NOT NULL,
	[BargainingUnit] [varchar](4) NOT NULL,
	[PayPlan] [varchar](2) NOT NULL,
	[Grade] [varchar](2) NOT NULL,
	[StepRate] [varchar](2) NOT NULL,
	[PayBasis] [varchar](2) NOT NULL,
	[WorkSchedule] [varchar](1) NOT NULL,
	[PayRateDeterminant] [varchar](1) NOT NULL,
	[LocalityAdjustment] [varchar](2) NOT NULL,
	[BasicPay] [real] NULL,
	[RetentionAllowance] [real] NULL,
	[SupervisoryDifferential] [real] NULL,
	[P_Race] [varchar](1) NOT NULL,
	[Q_Race] [varchar](1) NOT NULL,
 CONSTRAINT [PKCPDFNonDODStatusDIBBS2017v0_07ObsID] PRIMARY KEY NONCLUSTERED 
(
	[ObsID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]


-- view to retrieve synthetic OPM observations from specified version source data
create view [dbo].[CPDFNonDODStatusDIBBS2016] as select * from CPDFNonDODStatusDIBBS2017v0_07


-- stored procedure to retrieve synthetic and authentic OPM observations for model fitting and analysis
create proc [dbo].[DIBBSData] @style varchar(100), @DataVersion varchar(10)='v_06', @StartFY smallint=1988, @EndFY smallint=2011,
            @PayPlan varchar(20)='', @WorkSchedule varchar(10)='', @SampleNthRecord int=0, @Source varchar(25)='',
            @SelectCol varchar(1023)='', @WhereCol varchar(1023)='', @WhereClause varchar(1023)='' as

-- retrieve OPM authentic (JdF 2012 FOIA) and DIBBS synthetic data for comparison 
-- note that, at present, @DataVersion is merely documentary
-- SQL Server does not support temporary views (create view #DIBBS as ...), so specifying the
-- actual source table (CPDFNonDODStatusDIBBS2016v0_02, v_03, etc.) is not possible
-- copying all necessary rows and columns (25m X 15) into a temporary table to be used in the following
-- queries takes waaaaaaay tooooooo loooooooong
-- at present the DIBBS source version is specified in the CPDFNonDODStatusDIBBS2016 view, which is
-- referenced in subsequent queries

-- note that the synthetic data source (CPDFNonDODStatusDIBBS...) is a view that
-- specifies the physical record source - be sure to verify that the view references
-- required data version

-- data elements and nomenclature are as defined by the U.S. Office of Personnel Management
-- for details see the OPM Guide to Data Standards:
-- https://www.opm.gov/policy-data-oversight/data-analysis-documentation/data-policy-guidance/reporting-guidance/part-a-human-resources.pdf

-- parameters:
-- @style .......... data set to produce
-- @Source ......... 'synthetic' or 'authentic'
-- @StartFY ........ include observations with FY >= this value
-- @EndFY .......... include observations with FY <= this value
-- @PayPlan ........ limit observations to specified pay plan or 'GSGrade<=15' for
--                   pay plan 'GS' limited to grade <= 15
-- @WorkSchedule ... limit observations to specified work schedule
-- @SelectCol ...... comma separated list of columns from CPDF table(s) to return
-- @WhereCol ....... comma separated list of columns from which to construct where clause
-- @WhereClause .... individual logical comparison clauses corresponding to @WhereCol columns
--                   [examples: >=25; in('ab', 'cd'); between 50 and 100]
--                   note that the actual where clause composed is a composite such that
--                   observations are limited to those satisfying all specified
--                   @WhereCol/@WhereClause comparisons ('and' is inserted between each)

set nocount on

if(@style='LargeFixedEffectsModelNonUniqueIDJdF')

  -- collect OPM authentic (JdF 2012 FOIA) observations for large fixed effects regression model
  -- duplicate observations per pseudo ID per year returned
  -- limit to pay plan specified in @PayPlan (first two positions)
  -- limit to grade <= 15 when @PayPlan='GSGrade<=15'
  -- limit to full time observations when @WorkSchedule='FullTime'
  -- limit to valid age, education, sex, race, occupation, work schedule, and bureau
  -- limit to basic pay>0
  select PseudoID,
         cpdf.FY,
         age.AgeRangeMid as Age,
         power(convert(float, age.AgeRangeMid), 2) as AgeSq,
         cpdf2.Sex,
         case when(cpdf2.Race='A' or cpdf2.Race='' and cpdf2.ERIBridge='A')then 'A'
              when(cpdf2.Race in('B', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q')
                   or cpdf2.Race='' and  cpdf2.ERIBridge in ('B', 'D'))then 'B'
              when(cpdf2.Race='C' or cpdf2.Race='' and  cpdf2.ERIBridge='C')then 'C'
              when(cpdf2.Race='D'
                   or cpdf2.Race='' and  cpdf2.ERIBridge in('G', 'H', 'I', 'J', 'K', 'L', 'M'))then 'D'
              when(cpdf2.Race='E' or cpdf2.Race='' and cpdf2.ERIBridge='E')then 'E'
              else ''
         end as Race,
         case when(cpdf.SupervisoryStatus in('', '8'))then 0 else 1 end as Supervisor,
         cpdf.WorkSchedule,
         cpdf.OccupationalCategory,
         cpdf.Occupation,
         polind.BureauID,
         ed.EducationYears,
         log(cpdf.BasicPay*fy.CPIMultiplier) as lnBasicPay
  from   CPDFNonDODStatusJdF2012 cpdf join CPDFNonDODStatusJdF2014 cpdf2 on cpdf.ObsID=cpdf2.ObsID
         join FiscalYear fy on cpdf.FY=fy.FY
         join AgeRange age on cpdf.AgeRange=age.AgeRange
         join EducationLevel ed on cpdf.EducationLevel=ed.EducationLevel
         join PoliticalIndicators polind on cpdf.Agency=polind.Agency
  where  cpdf.FY between @StartFY and @EndFY
         and age.AgeRangeMid is not null
         and ed.EducationYears is not null
         and cpdf2.Sex in('F', 'M')
         and (cpdf.PayPlan=left(@PayPlan, 2) or @PayPlan='')
         and (case when(isnumeric(cpdf.Grade)=1)then cpdf.Grade else 99 end <= 15 and @PayPlan='GSGrade<=15' or @PayPlan='')
         and (cpdf.WorkSchedule in('B', 'F') and @WorkSchedule='FullTime' or @WorkSchedule='')
         --and cpdf.OccupationalCategory<>''
         and cpdf.Occupation<>''
         and (cpdf2.Race in('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q')
             or cpdf2.Race='' and cpdf2.ERIBridge in('A', 'B', 'C', 'D', 'E', 'G', 'H', 'I', 'J', 'K', 'L', 'M'))
         and cpdf.BasicPay>0
         and isnull(polind.BureauID, '')<>''
         -- sample
         and (@SampleNthRecord=0 or cpdf.ObsID%@SampleNthRecord=0)

else if(@style='LargeFixedEffectsModelNonUniqueIDDIBBS')

  -- collect DIBBS synthetic OPM observations for large fixed effects regression model
  -- duplicate observations per pseudo ID per year returned
  -- limit to pay plan specified in @PayPlan (first two positions)
  -- limit to grade <= 15 when @PayPlan='GSGrade<=15'
  -- limit to full time observations when @WorkSchedule='FullTime'
  -- limit to valid age, education, sex, race, occupation, work schedule, and bureau
  -- limit to basic pay>0
  select PseudoID,
         cpdf.FY,
         age.AgeRangeMid as Age,
         power(convert(float, age.AgeRangeMid), 2) as AgeSq,
         cpdf.Sex,
         case when(cpdf.Race='A' or cpdf.Race='' and cpdf.ERIBridge='A')then 'A'
              when(cpdf.Race in('B', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q')
                   or cpdf.Race='' and cpdf.ERIBridge in ('B', 'D'))then 'B'
              when(cpdf.Race='C' or cpdf.Race='' and  cpdf.ERIBridge='C')then 'C'
              when(cpdf.Race='D'
                   or cpdf.Race='' and cpdf.ERIBridge in('G', 'H', 'I', 'J', 'K', 'L', 'M'))then 'D'
              when(cpdf.Race='E' or cpdf.Race='' and cpdf.ERIBridge='E')then 'E'
              else ''
         end as Race,
         case when(cpdf.SupervisoryStatus in('', '8'))then 0 else 1 end as Supervisor,
         cpdf.WorkSchedule,
         cpdf.OccupationalCategory,
         cpdf.Occupation,
         polind.BureauID,
         ed.EducationYears,
         log(cpdf.BasicPay*fy.CPIMultiplier) as lnBasicPay
  from   CPDFNonDODStatusDIBBS2016 cpdf
         join FiscalYear fy on cpdf.FY=fy.FY
         join AgeRange age on cpdf.AgeRange=age.AgeRange
         join EducationLevel ed on cpdf.EducationLevel=ed.EducationLevel
         join PoliticalIndicators polind on cpdf.Agency=polind.Agency
  where  cpdf.FY between @StartFY and @EndFY
         and age.AgeRangeMid is not null
         and ed.EducationYears is not null
         and cpdf.Sex in('F', 'M')
         and (cpdf.PayPlan=left(@PayPlan, 2) or @PayPlan='')
         and (case when(isnumeric(cpdf.Grade)=1)then cpdf.Grade else 99 end <= 15 and @PayPlan='GSGrade<=15' or @PayPlan='')
         and (cpdf.WorkSchedule in('B', 'F') and @WorkSchedule='FullTime' or @WorkSchedule='')
         --and cpdf.OccupationalCategory<>''
         and cpdf.Occupation<>''
         and (cpdf.Race in('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q')
             or cpdf.Race='' and cpdf.ERIBridge in('A', 'B', 'C', 'D', 'E', 'G', 'H', 'I', 'J', 'K', 'L', 'M'))
         and cpdf.BasicPay>0
         and isnull(polind.BureauID, '')<>''
         -- sample
         and (@SampleNthRecord=0 or cpdf.ObsID%@SampleNthRecord=0)

else if(@style='VerificationData')

  -- verification server data soure
  -- this is the primary retrieval mechanism for authentic and synthetic observation retrieval
  -- it is modeled after LargeFixedEffectsModelNonUniqueIDJdF and LargeFixedEffectsModelNonUniqueIDDIBBS, but
  -- is parameterized, allowing flexible user specified and programmatic filtering and data transformation

  begin

    -- collect authentic and synthetic OPM  observations for verification server models
    -- limit to pay plan specified in @PayPlan (first two positions)
    -- limit to grade <= 15 when @PayPlan='GSGrade<=15'
    -- limit to full time observations when @WorkSchedule='FullTime'
    -- limit to valid elements in selected coumns
    -- limit to basic pay>0 (OPM recommeded practice, "valid records have non-zero pay")

    -- note that two where clauses are applied:  one to the initial subquery that
    -- collects observations (@sqlsubwhere) and applies data transformations
    -- and one (@WhereClause, supplied in proc call) to the results of the subquery
    -- using standardized column identifiers as used in the supplied @WhereClause

    -- @WhereCol format is col1,col2, ...
    -- @WhereClause format is clause1,clause2, ...
    -- each vi,clausei pair is applied (product of ands) to the final row set
    -- each vi is selected in the inner subquery to be available to outer where clause 

    -- standardize col request format
    -- omit spaces
    select @SelectCol=replace(@SelectCol, ' ', '')
    -- omit redundant commas
    while(@SelectCol like '%,,%')
      select @SelectCol=replace(@SelectCol, ',,', ',')
    -- add leading and terminating commas so that all cols fully delimited
    -- this avoids ambiguities, such as in 'page,' and 'age,'
    if(left(@SelectCol, 1)<>',')
      select @SelectCol=','+@SelectCol
    if(right(@SelectCol, 1)<>',')
      select @SelectCol=@SelectCol+','

    -- parse where clause columns and parameters
    -- squeeze spaces and test for col names
    select @WhereCol=replace(@WhereCol, ' ','')
    if(len(@WhereCol)>0)
      begin
        -- omit redundant commas
        while(@WhereCol like '%,,%')
          select @WhereCol=replace(@WhereCol, ',,', ',')
        while(@WhereClause like '%,,%')
          select @WhereClause=replace(@WhereClause, ',,', ',')
        -- append trailing, delimiting commas
        if(right(@WhereCol,1)<>',')
          select @WhereCol=@WhereCol+','
        if(right(@WhereClause,1)<>',')
          select @WhereClause=@WhereClause+','
        -- initialize select cols and outer where clause
        select @sqlsubselect='', @sqlwhere2='1=1'
        -- find delimeters for first col and clause
        select @p1=1, @p2=charindex(',', @WhereCol), @p3=1, @p4=charindex(',', @WhereClause)
        while(@p2>0)
          begin
            select @col=substring(@WhereCol, @p1, @p2-@p1)
            -- add col to sub-select set if not in requested set
            if(charindex(','+@col+',', @SelectCol)=0 and charindex(','+@col+',', @sqlsubselect)=0)
              select @sqlsubselect=@sqlsubselect+@col+','
            -- compose where clause
            select @sqlwhere2=@sqlwhere2+' and '+@col+substring(@WhereClause, @p3, @p4-@p3)
            -- locate next col and clause
            select @p1=@p2+1, @p2=charindex(',', @WhereCol, @p2+1), @p3=@p4+1, @p4=charindex(',', @WhereClause, @p4+1)
          end
        end
    else
      select @sqlsubselect='', @sqlwhere2=''

    -- compose subquery select statement
    -- begin with columns specified in where clause but not in select cols and
    -- requested columns
    -- translate simple col requests (sex, race, age, education, supervisor) to
    -- actual source and translation requirements
    -- omit transformed cols from subquery list after applying translation
    select @sqlsubselect=@sqlsubselect+@SelectCol
    -- qualify fiscal year (it appears in cpdf and fy)
    select @sqlsubselect = replace(@sqlsubselect, ',FY,', ',cpdf.FY,')
    -- select sex from 2014 observations for authentic observations
    -- note the absence of a table qualifier, that applies when authentic obs requested
    -- this accomodates the synthetic data set, which has sex and race in the primary CPDF table
    -- sex and race are unambiguous since they appear either in primary CPDF or in th 2014 table,
    -- but not both
    -- select @sqlsubselect = replace(@sqlsubselect, ',Sex,', ',cpdf2.Sex,')
    -- return race from 2014 observations
    -- convert OPM supplied race and ERI Bridge values to a single Race value using
    -- OPM supplied conversion rules
    select @sqlsubselect = replace(@sqlsubselect, ',Race,',',
            case when(Race=''A'' or Race='''' and ERIBridge=''A'')then ''A''
                when(Race in(''B'', ''F'', ''G'', ''H'', ''J'', ''K'', ''L'', ''M'', ''N'', ''P'', ''Q'')
                      or Race='''' and  ERIBridge in (''B'', ''D''))then ''B''
                when(Race=''C'' or Race='''' and  ERIBridge=''C'')then ''C''
                when(Race=''D''
                      or Race='''' and  ERIBridge in(''G'', ''H'', ''I'', ''J'', ''K'', ''L'', ''M''))then ''D''
                when(Race=''E'' or Race='''' and ERIBridge=''E'')then ''E''
                else ''''
            end as Race,')
    -- return midpoint of age level bracket
    select @sqlsubselect = replace(@sqlsubselect, ',Age,', ',age.AgeRangeMid as Age,')
    -- return Duke HC years of education mapped from OPM education level
    select @sqlsubselect = replace(@sqlsubselect, ',EducationYears,', ',ed.EducationYears,')
    -- return Duke assigned bureau ID corresponding to OPM agency
    select @sqlsubselect = replace(@sqlsubselect, ',BureauID,', ',polind.BureauID,')
    -- return supervisor indicator
    select @sqlsubselect = replace(@sqlsubselect, ',Supervisor,',
           ',case when(SupervisoryStatus in('''', ''8''))then 0 else 1 end as Supervisor,')
    -- return CPI adjusted basic pay
    select @sqlsubselect = replace(@sqlsubselect, ',BasicPay,',
           ',cpdf.BasicPay*fy.CPIMultiplier as BasicPay,')
    -- omit leading, trailing, and dplicate commas
    if(left(@sqlsubselect, 1)=',')
      select @sqlsubselect=right(@sqlsubselect, len(@sqlsubselect)-1)
    if(right(@sqlsubselect, 1)=',')
      select @sqlsubselect=left(@sqlsubselect, len(@sqlsubselect)-1)
    select @sqlsubselect=replace(@sqlsubselect, ',,', ',')

    -- compose subquery from clause using requested source
    select @sqlsubfrom = case when(@Source='Authentic')then
                                'CPDFNonDODStatusJdF2012 cpdf '
                              when(@Source='Synthetic')then
                                'CPDFNonDODStatusDIBBS2016 cpdf '
                              else 'cpdf'
                         end +
            -- join to 2104 observations only for authentic request
            case when(@source='Authentic' and (@SelectCol like '%,Sex,%' or @SelectCol like '%,Race,%'))then
                   ' join CPDFNonDODStatusJdF2014 cpdf2 on cpdf.ObsID=cpdf2.ObsID '
                 else ''
            end +
            case when(@SelectCol like '%,Age,%')then
                   ' join AgeRange age on cpdf.AgeRange=age.AgeRange '
                 else ''
            end +
            case when(@SelectCol like '%,EducationYears,%')then
                   ' join EducationLevel ed on cpdf.EducationLevel=ed.EducationLevel '
                 else ''
            end +
            case when(@SelectCol like '%,BureauID,%')then
                   ' join PoliticalIndicators polind on cpdf.Agency=polind.Agency '
                 else ''
            end +
            case when(@SelectCol like '%,BasicPay,%')then
                   -- retrieve CPI multiplier for corrseponding year     
                   ' join FiscalYear fy on cpdf.FY=fy.FY '
                 else ''
            end

    -- compose subquery where clause
    select @sqlsubwhere='1=1'
    -- year range
    if(@StartFY>0)
      select @sqlsubwhere=@sqlsubwhere + ' and cpdf.FY>=' + convert(varchar(10), @StartFY)
    if(@EndFY<9999)
      select @sqlsubwhere=@sqlsubwhere + ' and cpdf.FY<=' + convert(varchar(10), @EndFY)
    -- sex
    if(@SelectCol like '%Sex,%')
      select @sqlsubwhere=@sqlsubwhere + ' and Sex in(''F'', ''M'')'
    -- race
    if(@SelectCol like '%Race,%')
      select @sqlsubwhere=@sqlsubwhere +
              ' and (Race in(''A'', ''B'', ''C'', ''D'', ''E'', ''F'', ''G'', ''H'', ''J'', ''K'', ''L'', ''M'', ''N'', ''P'', ''Q'')
                    or Race='''' and ERIBridge in(''A'', ''B'', ''C'', ''D'', ''E'', ''G'', ''H'', ''I'', ''J'', ''K'', ''L'', ''M''))'
    -- age
    if(@SelectCol like '%Age,%')
      select @sqlsubwhere=@sqlsubwhere + ' and age.AgeRangeMid is not null'
    -- ed
    if(@SelectCol like '%EducationYears,%')
      select @sqlsubwhere=@sqlsubwhere + ' and ed.EducationYears is not null'
    -- pay plan
    if(@PayPlan='GSGrade<=15')
      select @sqlsubwhere=@sqlsubwhere + ' and cpdf.PayPlan=''GS'' and case when(isnumeric(cpdf.Grade)=1)then cpdf.Grade else 99 end <= 15'
    else if(@PayPlan<>'')
      select @sqlsubwhere=@sqlsubwhere + ' and cpdf.PayPlan=''' + @PayPlan + ''''
    -- work schedule
    if(@WorkSchedule='FullTime')
      select @sqlsubwhere=@sqlsubwhere + ' and cpdf.WorkSchedule in(''B'', ''F'')'
    -- occupation
    if(@SelectCol like '%OccupationalCategory,%')
      select @sqlsubwhere=@sqlsubwhere + ' and cpdf.OccupationalCategory<>'''''
    if(@SelectCol like '%Occupational,%')
      select @sqlsubwhere=@sqlsubwhere + ' and cpdf.Occupation<>'''''
    -- bureau
    if(@SelectCol like '%BureauID,%')
      select @sqlsubwhere=@sqlsubwhere + ' and isnull(polind.BureauID, '''')<>'''''
    -- basic pay
    select @sqlsubwhere=@sqlsubwhere + ' and cpdf.BasicPay>0'

    -- construct outer SQL statement to execute subquery and apply @WhereClause
    select @sqltext = 'select * from ( select ' + @sqlsubselect + char(13) + char(10) +
                      ' from ' + @sqlsubfrom + char(13) + char(10) +
                      ' where ' + @sqlsubwhere +' ) obs ' + char(13) + char(10) +
                      case when(@sqlwhere2<>'')then ' where ' + @sqlwhere2 else '' end

    -- print @sqltext
    exec(@sqltext)

  end
