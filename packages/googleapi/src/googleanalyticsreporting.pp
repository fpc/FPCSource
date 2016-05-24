unit googleanalyticsreporting;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TPivotHeader = Class;
  TMetric = Class;
  TColumnHeader = Class;
  TDynamicSegment = Class;
  TMetricHeader = Class;
  TReport = Class;
  TSegmentFilterClause = Class;
  TDimensionFilter = Class;
  TSegmentDimensionFilter = Class;
  TReportRequest = Class;
  TSimpleSegment = Class;
  TSegmentDefinition = Class;
  TSegmentMetricFilter = Class;
  TReportData = Class;
  TGetReportsRequest = Class;
  TOrderBy = Class;
  TCohort = Class;
  TOrFiltersForSegment = Class;
  TSequenceSegment = Class;
  TSegmentFilter = Class;
  TPivotHeaderEntry = Class;
  TDimensionFilterClause = Class;
  TSegmentSequenceStep = Class;
  TPivot = Class;
  TDateRangeValues = Class;
  TMetricFilterClause = Class;
  TSegment = Class;
  TDateRange = Class;
  TReportRow = Class;
  TCohortGroup = Class;
  TGetReportsResponse = Class;
  TMetricHeaderEntry = Class;
  TMetricFilter = Class;
  TDimension = Class;
  TPivotValueRegion = Class;
  TPivotHeaderArray = Array of TPivotHeader;
  TMetricArray = Array of TMetric;
  TColumnHeaderArray = Array of TColumnHeader;
  TDynamicSegmentArray = Array of TDynamicSegment;
  TMetricHeaderArray = Array of TMetricHeader;
  TReportArray = Array of TReport;
  TSegmentFilterClauseArray = Array of TSegmentFilterClause;
  TDimensionFilterArray = Array of TDimensionFilter;
  TSegmentDimensionFilterArray = Array of TSegmentDimensionFilter;
  TReportRequestArray = Array of TReportRequest;
  TSimpleSegmentArray = Array of TSimpleSegment;
  TSegmentDefinitionArray = Array of TSegmentDefinition;
  TSegmentMetricFilterArray = Array of TSegmentMetricFilter;
  TReportDataArray = Array of TReportData;
  TGetReportsRequestArray = Array of TGetReportsRequest;
  TOrderByArray = Array of TOrderBy;
  TCohortArray = Array of TCohort;
  TOrFiltersForSegmentArray = Array of TOrFiltersForSegment;
  TSequenceSegmentArray = Array of TSequenceSegment;
  TSegmentFilterArray = Array of TSegmentFilter;
  TPivotHeaderEntryArray = Array of TPivotHeaderEntry;
  TDimensionFilterClauseArray = Array of TDimensionFilterClause;
  TSegmentSequenceStepArray = Array of TSegmentSequenceStep;
  TPivotArray = Array of TPivot;
  TDateRangeValuesArray = Array of TDateRangeValues;
  TMetricFilterClauseArray = Array of TMetricFilterClause;
  TSegmentArray = Array of TSegment;
  TDateRangeArray = Array of TDateRange;
  TReportRowArray = Array of TReportRow;
  TCohortGroupArray = Array of TCohortGroup;
  TGetReportsResponseArray = Array of TGetReportsResponse;
  TMetricHeaderEntryArray = Array of TMetricHeaderEntry;
  TMetricFilterArray = Array of TMetricFilter;
  TDimensionArray = Array of TDimension;
  TPivotValueRegionArray = Array of TPivotValueRegion;
  //Anonymous types, using auto-generated names
  TPivotHeaderTypepivotHeaderEntriesArray = Array of TPivotHeaderEntry;
  TMetricHeaderTypemetricHeaderEntriesArray = Array of TMetricHeaderEntry;
  TMetricHeaderTypepivotHeadersArray = Array of TPivotHeader;
  TReportRequestTypedimensionsArray = Array of TDimension;
  TReportRequestTypemetricFilterClausesArray = Array of TMetricFilterClause;
  TReportRequestTypedimensionFilterClausesArray = Array of TDimensionFilterClause;
  TReportRequestTypepivotsArray = Array of TPivot;
  TReportRequestTypedateRangesArray = Array of TDateRange;
  TReportRequestTypesegmentsArray = Array of TSegment;
  TReportRequestTypemetricsArray = Array of TMetric;
  TReportRequestTypeorderBysArray = Array of TOrderBy;
  TSimpleSegmentTypeorFiltersForSegmentArray = Array of TOrFiltersForSegment;
  TSegmentDefinitionTypesegmentFiltersArray = Array of TSegmentFilter;
  TReportDataTypemaximumsArray = Array of TDateRangeValues;
  TReportDataTypeminimumsArray = Array of TDateRangeValues;
  TReportDataTyperowsArray = Array of TReportRow;
  TReportDataTypetotalsArray = Array of TDateRangeValues;
  TGetReportsRequestTypereportRequestsArray = Array of TReportRequest;
  TOrFiltersForSegmentTypesegmentFilterClausesArray = Array of TSegmentFilterClause;
  TSequenceSegmentTypesegmentSequenceStepsArray = Array of TSegmentSequenceStep;
  TDimensionFilterClauseTypefiltersArray = Array of TDimensionFilter;
  TSegmentSequenceStepTypeorFiltersForSegmentArray = Array of TOrFiltersForSegment;
  TPivotTypedimensionsArray = Array of TDimension;
  TPivotTypemetricsArray = Array of TMetric;
  TPivotTypedimensionFilterClausesArray = Array of TDimensionFilterClause;
  TDateRangeValuesTypepivotValueRegionsArray = Array of TPivotValueRegion;
  TMetricFilterClauseTypefiltersArray = Array of TMetricFilter;
  TReportRowTypemetricsArray = Array of TDateRangeValues;
  TCohortGroupTypecohortsArray = Array of TCohort;
  TGetReportsResponseTypereportsArray = Array of TReport;
  
  { --------------------------------------------------------------------
    TPivotHeader
    --------------------------------------------------------------------}
  
  TPivotHeader = Class(TGoogleBaseObject)
  Private
    FtotalPivotGroupsCount : integer;
    FpivotHeaderEntries : TPivotHeaderTypepivotHeaderEntriesArray;
  Protected
    //Property setters
    Procedure SettotalPivotGroupsCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetpivotHeaderEntries(AIndex : Integer; const AValue : TPivotHeaderTypepivotHeaderEntriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property totalPivotGroupsCount : integer Index 0 Read FtotalPivotGroupsCount Write SettotalPivotGroupsCount;
    Property pivotHeaderEntries : TPivotHeaderTypepivotHeaderEntriesArray Index 8 Read FpivotHeaderEntries Write SetpivotHeaderEntries;
  end;
  TPivotHeaderClass = Class of TPivotHeader;
  
  { --------------------------------------------------------------------
    TMetric
    --------------------------------------------------------------------}
  
  TMetric = Class(TGoogleBaseObject)
  Private
    Falias : String;
    FformattingType : String;
    Fexpression : String;
  Protected
    //Property setters
    Procedure Setalias(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattingType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexpression(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property alias : String Index 0 Read Falias Write Setalias;
    Property formattingType : String Index 8 Read FformattingType Write SetformattingType;
    Property expression : String Index 16 Read Fexpression Write Setexpression;
  end;
  TMetricClass = Class of TMetric;
  
  { --------------------------------------------------------------------
    TColumnHeader
    --------------------------------------------------------------------}
  
  TColumnHeader = Class(TGoogleBaseObject)
  Private
    Fdimensions : TStringArray;
    FmetricHeader : TMetricHeader;
  Protected
    //Property setters
    Procedure Setdimensions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetmetricHeader(AIndex : Integer; const AValue : TMetricHeader); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensions : TStringArray Index 0 Read Fdimensions Write Setdimensions;
    Property metricHeader : TMetricHeader Index 8 Read FmetricHeader Write SetmetricHeader;
  end;
  TColumnHeaderClass = Class of TColumnHeader;
  
  { --------------------------------------------------------------------
    TDynamicSegment
    --------------------------------------------------------------------}
  
  TDynamicSegment = Class(TGoogleBaseObject)
  Private
    FsessionSegment : TSegmentDefinition;
    Fname : String;
    FuserSegment : TSegmentDefinition;
  Protected
    //Property setters
    Procedure SetsessionSegment(AIndex : Integer; const AValue : TSegmentDefinition); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserSegment(AIndex : Integer; const AValue : TSegmentDefinition); virtual;
  Public
  Published
    Property sessionSegment : TSegmentDefinition Index 0 Read FsessionSegment Write SetsessionSegment;
    Property name : String Index 8 Read Fname Write Setname;
    Property userSegment : TSegmentDefinition Index 16 Read FuserSegment Write SetuserSegment;
  end;
  TDynamicSegmentClass = Class of TDynamicSegment;
  
  { --------------------------------------------------------------------
    TMetricHeader
    --------------------------------------------------------------------}
  
  TMetricHeader = Class(TGoogleBaseObject)
  Private
    FmetricHeaderEntries : TMetricHeaderTypemetricHeaderEntriesArray;
    FpivotHeaders : TMetricHeaderTypepivotHeadersArray;
  Protected
    //Property setters
    Procedure SetmetricHeaderEntries(AIndex : Integer; const AValue : TMetricHeaderTypemetricHeaderEntriesArray); virtual;
    Procedure SetpivotHeaders(AIndex : Integer; const AValue : TMetricHeaderTypepivotHeadersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property metricHeaderEntries : TMetricHeaderTypemetricHeaderEntriesArray Index 0 Read FmetricHeaderEntries Write SetmetricHeaderEntries;
    Property pivotHeaders : TMetricHeaderTypepivotHeadersArray Index 8 Read FpivotHeaders Write SetpivotHeaders;
  end;
  TMetricHeaderClass = Class of TMetricHeader;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    FcolumnHeader : TColumnHeader;
    Fdata : TReportData;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetcolumnHeader(AIndex : Integer; const AValue : TColumnHeader); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : TReportData); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property columnHeader : TColumnHeader Index 0 Read FcolumnHeader Write SetcolumnHeader;
    Property data : TReportData Index 8 Read Fdata Write Setdata;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TSegmentFilterClause
    --------------------------------------------------------------------}
  
  TSegmentFilterClause = Class(TGoogleBaseObject)
  Private
    FdimensionFilter : TSegmentDimensionFilter;
    FmetricFilter : TSegmentMetricFilter;
    F_not : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdimensionFilter(AIndex : Integer; const AValue : TSegmentDimensionFilter); virtual;
    Procedure SetmetricFilter(AIndex : Integer; const AValue : TSegmentMetricFilter); virtual;
    Procedure Set_not(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property dimensionFilter : TSegmentDimensionFilter Index 0 Read FdimensionFilter Write SetdimensionFilter;
    Property metricFilter : TSegmentMetricFilter Index 8 Read FmetricFilter Write SetmetricFilter;
    Property _not : boolean Index 16 Read F_not Write Set_not;
  end;
  TSegmentFilterClauseClass = Class of TSegmentFilterClause;
  
  { --------------------------------------------------------------------
    TDimensionFilter
    --------------------------------------------------------------------}
  
  TDimensionFilter = Class(TGoogleBaseObject)
  Private
    FdimensionName : String;
    F_operator : String;
    FcaseSensitive : boolean;
    Fexpressions : TStringArray;
    F_not : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdimensionName(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_operator(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcaseSensitive(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setexpressions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Set_not(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensionName : String Index 0 Read FdimensionName Write SetdimensionName;
    Property _operator : String Index 8 Read F_operator Write Set_operator;
    Property caseSensitive : boolean Index 16 Read FcaseSensitive Write SetcaseSensitive;
    Property expressions : TStringArray Index 24 Read Fexpressions Write Setexpressions;
    Property _not : boolean Index 32 Read F_not Write Set_not;
  end;
  TDimensionFilterClass = Class of TDimensionFilter;
  
  { --------------------------------------------------------------------
    TSegmentDimensionFilter
    --------------------------------------------------------------------}
  
  TSegmentDimensionFilter = Class(TGoogleBaseObject)
  Private
    FmaxComparisonValue : String;
    FdimensionName : String;
    FcaseSensitive : boolean;
    F_operator : String;
    Fexpressions : TStringArray;
    FminComparisonValue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetmaxComparisonValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdimensionName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcaseSensitive(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Set_operator(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexpressions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetminComparisonValue(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property maxComparisonValue : String Index 0 Read FmaxComparisonValue Write SetmaxComparisonValue;
    Property dimensionName : String Index 8 Read FdimensionName Write SetdimensionName;
    Property caseSensitive : boolean Index 16 Read FcaseSensitive Write SetcaseSensitive;
    Property _operator : String Index 24 Read F_operator Write Set_operator;
    Property expressions : TStringArray Index 32 Read Fexpressions Write Setexpressions;
    Property minComparisonValue : String Index 40 Read FminComparisonValue Write SetminComparisonValue;
  end;
  TSegmentDimensionFilterClass = Class of TSegmentDimensionFilter;
  
  { --------------------------------------------------------------------
    TReportRequest
    --------------------------------------------------------------------}
  
  TReportRequest = Class(TGoogleBaseObject)
  Private
    FcohortGroup : TCohortGroup;
    Fdimensions : TReportRequestTypedimensionsArray;
    FmetricFilterClauses : TReportRequestTypemetricFilterClausesArray;
    FhideTotals : boolean;
    FincludeEmptyRows : boolean;
    FdimensionFilterClauses : TReportRequestTypedimensionFilterClausesArray;
    Fpivots : TReportRequestTypepivotsArray;
    FdateRanges : TReportRequestTypedateRangesArray;
    Fsegments : TReportRequestTypesegmentsArray;
    FsamplingLevel : String;
    Fmetrics : TReportRequestTypemetricsArray;
    FpageSize : integer;
    ForderBys : TReportRequestTypeorderBysArray;
    FfiltersExpression : String;
    FhideValueRanges : boolean;
    FviewId : String;
    FpageToken : String;
  Protected
    //Property setters
    Procedure SetcohortGroup(AIndex : Integer; const AValue : TCohortGroup); virtual;
    Procedure Setdimensions(AIndex : Integer; const AValue : TReportRequestTypedimensionsArray); virtual;
    Procedure SetmetricFilterClauses(AIndex : Integer; const AValue : TReportRequestTypemetricFilterClausesArray); virtual;
    Procedure SethideTotals(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetincludeEmptyRows(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetdimensionFilterClauses(AIndex : Integer; const AValue : TReportRequestTypedimensionFilterClausesArray); virtual;
    Procedure Setpivots(AIndex : Integer; const AValue : TReportRequestTypepivotsArray); virtual;
    Procedure SetdateRanges(AIndex : Integer; const AValue : TReportRequestTypedateRangesArray); virtual;
    Procedure Setsegments(AIndex : Integer; const AValue : TReportRequestTypesegmentsArray); virtual;
    Procedure SetsamplingLevel(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; const AValue : TReportRequestTypemetricsArray); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetorderBys(AIndex : Integer; const AValue : TReportRequestTypeorderBysArray); virtual;
    Procedure SetfiltersExpression(AIndex : Integer; const AValue : String); virtual;
    Procedure SethideValueRanges(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetviewId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property cohortGroup : TCohortGroup Index 0 Read FcohortGroup Write SetcohortGroup;
    Property dimensions : TReportRequestTypedimensionsArray Index 8 Read Fdimensions Write Setdimensions;
    Property metricFilterClauses : TReportRequestTypemetricFilterClausesArray Index 16 Read FmetricFilterClauses Write SetmetricFilterClauses;
    Property hideTotals : boolean Index 24 Read FhideTotals Write SethideTotals;
    Property includeEmptyRows : boolean Index 32 Read FincludeEmptyRows Write SetincludeEmptyRows;
    Property dimensionFilterClauses : TReportRequestTypedimensionFilterClausesArray Index 40 Read FdimensionFilterClauses Write SetdimensionFilterClauses;
    Property pivots : TReportRequestTypepivotsArray Index 48 Read Fpivots Write Setpivots;
    Property dateRanges : TReportRequestTypedateRangesArray Index 56 Read FdateRanges Write SetdateRanges;
    Property segments : TReportRequestTypesegmentsArray Index 64 Read Fsegments Write Setsegments;
    Property samplingLevel : String Index 72 Read FsamplingLevel Write SetsamplingLevel;
    Property metrics : TReportRequestTypemetricsArray Index 80 Read Fmetrics Write Setmetrics;
    Property pageSize : integer Index 88 Read FpageSize Write SetpageSize;
    Property orderBys : TReportRequestTypeorderBysArray Index 96 Read ForderBys Write SetorderBys;
    Property filtersExpression : String Index 104 Read FfiltersExpression Write SetfiltersExpression;
    Property hideValueRanges : boolean Index 112 Read FhideValueRanges Write SethideValueRanges;
    Property viewId : String Index 120 Read FviewId Write SetviewId;
    Property pageToken : String Index 128 Read FpageToken Write SetpageToken;
  end;
  TReportRequestClass = Class of TReportRequest;
  
  { --------------------------------------------------------------------
    TSimpleSegment
    --------------------------------------------------------------------}
  
  TSimpleSegment = Class(TGoogleBaseObject)
  Private
    ForFiltersForSegment : TSimpleSegmentTypeorFiltersForSegmentArray;
  Protected
    //Property setters
    Procedure SetorFiltersForSegment(AIndex : Integer; const AValue : TSimpleSegmentTypeorFiltersForSegmentArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property orFiltersForSegment : TSimpleSegmentTypeorFiltersForSegmentArray Index 0 Read ForFiltersForSegment Write SetorFiltersForSegment;
  end;
  TSimpleSegmentClass = Class of TSimpleSegment;
  
  { --------------------------------------------------------------------
    TSegmentDefinition
    --------------------------------------------------------------------}
  
  TSegmentDefinition = Class(TGoogleBaseObject)
  Private
    FsegmentFilters : TSegmentDefinitionTypesegmentFiltersArray;
  Protected
    //Property setters
    Procedure SetsegmentFilters(AIndex : Integer; const AValue : TSegmentDefinitionTypesegmentFiltersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property segmentFilters : TSegmentDefinitionTypesegmentFiltersArray Index 0 Read FsegmentFilters Write SetsegmentFilters;
  end;
  TSegmentDefinitionClass = Class of TSegmentDefinition;
  
  { --------------------------------------------------------------------
    TSegmentMetricFilter
    --------------------------------------------------------------------}
  
  TSegmentMetricFilter = Class(TGoogleBaseObject)
  Private
    FmetricName : String;
    F_operator : String;
    FcomparisonValue : String;
    Fscope : String;
    FmaxComparisonValue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetmetricName(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_operator(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcomparisonValue(AIndex : Integer; const AValue : String); virtual;
    Procedure Setscope(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxComparisonValue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metricName : String Index 0 Read FmetricName Write SetmetricName;
    Property _operator : String Index 8 Read F_operator Write Set_operator;
    Property comparisonValue : String Index 16 Read FcomparisonValue Write SetcomparisonValue;
    Property scope : String Index 24 Read Fscope Write Setscope;
    Property maxComparisonValue : String Index 32 Read FmaxComparisonValue Write SetmaxComparisonValue;
  end;
  TSegmentMetricFilterClass = Class of TSegmentMetricFilter;
  
  { --------------------------------------------------------------------
    TReportData
    --------------------------------------------------------------------}
  
  TReportData = Class(TGoogleBaseObject)
  Private
    FrowCount : integer;
    FsamplingSpaceSizes : TStringArray;
    Fmaximums : TReportDataTypemaximumsArray;
    FsamplesReadCounts : TStringArray;
    Fminimums : TReportDataTypeminimumsArray;
    Frows : TReportDataTyperowsArray;
    Ftotals : TReportDataTypetotalsArray;
    FisDataGolden : boolean;
  Protected
    //Property setters
    Procedure SetrowCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetsamplingSpaceSizes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setmaximums(AIndex : Integer; const AValue : TReportDataTypemaximumsArray); virtual;
    Procedure SetsamplesReadCounts(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setminimums(AIndex : Integer; const AValue : TReportDataTypeminimumsArray); virtual;
    Procedure Setrows(AIndex : Integer; const AValue : TReportDataTyperowsArray); virtual;
    Procedure Settotals(AIndex : Integer; const AValue : TReportDataTypetotalsArray); virtual;
    Procedure SetisDataGolden(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property rowCount : integer Index 0 Read FrowCount Write SetrowCount;
    Property samplingSpaceSizes : TStringArray Index 8 Read FsamplingSpaceSizes Write SetsamplingSpaceSizes;
    Property maximums : TReportDataTypemaximumsArray Index 16 Read Fmaximums Write Setmaximums;
    Property samplesReadCounts : TStringArray Index 24 Read FsamplesReadCounts Write SetsamplesReadCounts;
    Property minimums : TReportDataTypeminimumsArray Index 32 Read Fminimums Write Setminimums;
    Property rows : TReportDataTyperowsArray Index 40 Read Frows Write Setrows;
    Property totals : TReportDataTypetotalsArray Index 48 Read Ftotals Write Settotals;
    Property isDataGolden : boolean Index 56 Read FisDataGolden Write SetisDataGolden;
  end;
  TReportDataClass = Class of TReportData;
  
  { --------------------------------------------------------------------
    TGetReportsRequest
    --------------------------------------------------------------------}
  
  TGetReportsRequest = Class(TGoogleBaseObject)
  Private
    FreportRequests : TGetReportsRequestTypereportRequestsArray;
  Protected
    //Property setters
    Procedure SetreportRequests(AIndex : Integer; const AValue : TGetReportsRequestTypereportRequestsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property reportRequests : TGetReportsRequestTypereportRequestsArray Index 0 Read FreportRequests Write SetreportRequests;
  end;
  TGetReportsRequestClass = Class of TGetReportsRequest;
  
  { --------------------------------------------------------------------
    TOrderBy
    --------------------------------------------------------------------}
  
  TOrderBy = Class(TGoogleBaseObject)
  Private
    FsortOrder : String;
    ForderType : String;
    FfieldName : String;
  Protected
    //Property setters
    Procedure SetsortOrder(AIndex : Integer; const AValue : String); virtual;
    Procedure SetorderType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfieldName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property sortOrder : String Index 0 Read FsortOrder Write SetsortOrder;
    Property orderType : String Index 8 Read ForderType Write SetorderType;
    Property fieldName : String Index 16 Read FfieldName Write SetfieldName;
  end;
  TOrderByClass = Class of TOrderBy;
  
  { --------------------------------------------------------------------
    TCohort
    --------------------------------------------------------------------}
  
  TCohort = Class(TGoogleBaseObject)
  Private
    F_type : String;
    FdateRange : TDateRange;
    Fname : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdateRange(AIndex : Integer; const AValue : TDateRange); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property dateRange : TDateRange Index 8 Read FdateRange Write SetdateRange;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TCohortClass = Class of TCohort;
  
  { --------------------------------------------------------------------
    TOrFiltersForSegment
    --------------------------------------------------------------------}
  
  TOrFiltersForSegment = Class(TGoogleBaseObject)
  Private
    FsegmentFilterClauses : TOrFiltersForSegmentTypesegmentFilterClausesArray;
  Protected
    //Property setters
    Procedure SetsegmentFilterClauses(AIndex : Integer; const AValue : TOrFiltersForSegmentTypesegmentFilterClausesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property segmentFilterClauses : TOrFiltersForSegmentTypesegmentFilterClausesArray Index 0 Read FsegmentFilterClauses Write SetsegmentFilterClauses;
  end;
  TOrFiltersForSegmentClass = Class of TOrFiltersForSegment;
  
  { --------------------------------------------------------------------
    TSequenceSegment
    --------------------------------------------------------------------}
  
  TSequenceSegment = Class(TGoogleBaseObject)
  Private
    FfirstStepShouldMatchFirstHit : boolean;
    FsegmentSequenceSteps : TSequenceSegmentTypesegmentSequenceStepsArray;
  Protected
    //Property setters
    Procedure SetfirstStepShouldMatchFirstHit(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetsegmentSequenceSteps(AIndex : Integer; const AValue : TSequenceSegmentTypesegmentSequenceStepsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property firstStepShouldMatchFirstHit : boolean Index 0 Read FfirstStepShouldMatchFirstHit Write SetfirstStepShouldMatchFirstHit;
    Property segmentSequenceSteps : TSequenceSegmentTypesegmentSequenceStepsArray Index 8 Read FsegmentSequenceSteps Write SetsegmentSequenceSteps;
  end;
  TSequenceSegmentClass = Class of TSequenceSegment;
  
  { --------------------------------------------------------------------
    TSegmentFilter
    --------------------------------------------------------------------}
  
  TSegmentFilter = Class(TGoogleBaseObject)
  Private
    FsequenceSegment : TSequenceSegment;
    F_not : boolean;
    FsimpleSegment : TSimpleSegment;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetsequenceSegment(AIndex : Integer; const AValue : TSequenceSegment); virtual;
    Procedure Set_not(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetsimpleSegment(AIndex : Integer; const AValue : TSimpleSegment); virtual;
  Public
  Published
    Property sequenceSegment : TSequenceSegment Index 0 Read FsequenceSegment Write SetsequenceSegment;
    Property _not : boolean Index 8 Read F_not Write Set_not;
    Property simpleSegment : TSimpleSegment Index 16 Read FsimpleSegment Write SetsimpleSegment;
  end;
  TSegmentFilterClass = Class of TSegmentFilter;
  
  { --------------------------------------------------------------------
    TPivotHeaderEntry
    --------------------------------------------------------------------}
  
  TPivotHeaderEntry = Class(TGoogleBaseObject)
  Private
    FdimensionNames : TStringArray;
    FdimensionValues : TStringArray;
    Fmetric : TMetricHeaderEntry;
  Protected
    //Property setters
    Procedure SetdimensionNames(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetdimensionValues(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setmetric(AIndex : Integer; const AValue : TMetricHeaderEntry); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensionNames : TStringArray Index 0 Read FdimensionNames Write SetdimensionNames;
    Property dimensionValues : TStringArray Index 8 Read FdimensionValues Write SetdimensionValues;
    Property metric : TMetricHeaderEntry Index 16 Read Fmetric Write Setmetric;
  end;
  TPivotHeaderEntryClass = Class of TPivotHeaderEntry;
  
  { --------------------------------------------------------------------
    TDimensionFilterClause
    --------------------------------------------------------------------}
  
  TDimensionFilterClause = Class(TGoogleBaseObject)
  Private
    F_operator : String;
    Ffilters : TDimensionFilterClauseTypefiltersArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_operator(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfilters(AIndex : Integer; const AValue : TDimensionFilterClauseTypefiltersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property _operator : String Index 0 Read F_operator Write Set_operator;
    Property filters : TDimensionFilterClauseTypefiltersArray Index 8 Read Ffilters Write Setfilters;
  end;
  TDimensionFilterClauseClass = Class of TDimensionFilterClause;
  
  { --------------------------------------------------------------------
    TSegmentSequenceStep
    --------------------------------------------------------------------}
  
  TSegmentSequenceStep = Class(TGoogleBaseObject)
  Private
    FmatchType : String;
    ForFiltersForSegment : TSegmentSequenceStepTypeorFiltersForSegmentArray;
  Protected
    //Property setters
    Procedure SetmatchType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetorFiltersForSegment(AIndex : Integer; const AValue : TSegmentSequenceStepTypeorFiltersForSegmentArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property matchType : String Index 0 Read FmatchType Write SetmatchType;
    Property orFiltersForSegment : TSegmentSequenceStepTypeorFiltersForSegmentArray Index 8 Read ForFiltersForSegment Write SetorFiltersForSegment;
  end;
  TSegmentSequenceStepClass = Class of TSegmentSequenceStep;
  
  { --------------------------------------------------------------------
    TPivot
    --------------------------------------------------------------------}
  
  TPivot = Class(TGoogleBaseObject)
  Private
    Fdimensions : TPivotTypedimensionsArray;
    Fmetrics : TPivotTypemetricsArray;
    FmaxGroupCount : integer;
    FdimensionFilterClauses : TPivotTypedimensionFilterClausesArray;
    FstartGroup : integer;
  Protected
    //Property setters
    Procedure Setdimensions(AIndex : Integer; const AValue : TPivotTypedimensionsArray); virtual;
    Procedure Setmetrics(AIndex : Integer; const AValue : TPivotTypemetricsArray); virtual;
    Procedure SetmaxGroupCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetdimensionFilterClauses(AIndex : Integer; const AValue : TPivotTypedimensionFilterClausesArray); virtual;
    Procedure SetstartGroup(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensions : TPivotTypedimensionsArray Index 0 Read Fdimensions Write Setdimensions;
    Property metrics : TPivotTypemetricsArray Index 8 Read Fmetrics Write Setmetrics;
    Property maxGroupCount : integer Index 16 Read FmaxGroupCount Write SetmaxGroupCount;
    Property dimensionFilterClauses : TPivotTypedimensionFilterClausesArray Index 24 Read FdimensionFilterClauses Write SetdimensionFilterClauses;
    Property startGroup : integer Index 32 Read FstartGroup Write SetstartGroup;
  end;
  TPivotClass = Class of TPivot;
  
  { --------------------------------------------------------------------
    TDateRangeValues
    --------------------------------------------------------------------}
  
  TDateRangeValues = Class(TGoogleBaseObject)
  Private
    Fvalues : TStringArray;
    FpivotValueRegions : TDateRangeValuesTypepivotValueRegionsArray;
  Protected
    //Property setters
    Procedure Setvalues(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetpivotValueRegions(AIndex : Integer; const AValue : TDateRangeValuesTypepivotValueRegionsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property values : TStringArray Index 0 Read Fvalues Write Setvalues;
    Property pivotValueRegions : TDateRangeValuesTypepivotValueRegionsArray Index 8 Read FpivotValueRegions Write SetpivotValueRegions;
  end;
  TDateRangeValuesClass = Class of TDateRangeValues;
  
  { --------------------------------------------------------------------
    TMetricFilterClause
    --------------------------------------------------------------------}
  
  TMetricFilterClause = Class(TGoogleBaseObject)
  Private
    F_operator : String;
    Ffilters : TMetricFilterClauseTypefiltersArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_operator(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfilters(AIndex : Integer; const AValue : TMetricFilterClauseTypefiltersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property _operator : String Index 0 Read F_operator Write Set_operator;
    Property filters : TMetricFilterClauseTypefiltersArray Index 8 Read Ffilters Write Setfilters;
  end;
  TMetricFilterClauseClass = Class of TMetricFilterClause;
  
  { --------------------------------------------------------------------
    TSegment
    --------------------------------------------------------------------}
  
  TSegment = Class(TGoogleBaseObject)
  Private
    FdynamicSegment : TDynamicSegment;
    FsegmentId : String;
  Protected
    //Property setters
    Procedure SetdynamicSegment(AIndex : Integer; const AValue : TDynamicSegment); virtual;
    Procedure SetsegmentId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property dynamicSegment : TDynamicSegment Index 0 Read FdynamicSegment Write SetdynamicSegment;
    Property segmentId : String Index 8 Read FsegmentId Write SetsegmentId;
  end;
  TSegmentClass = Class of TSegment;
  
  { --------------------------------------------------------------------
    TDateRange
    --------------------------------------------------------------------}
  
  TDateRange = Class(TGoogleBaseObject)
  Private
    FstartDate : String;
    FendDate : String;
  Protected
    //Property setters
    Procedure SetstartDate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendDate(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property startDate : String Index 0 Read FstartDate Write SetstartDate;
    Property endDate : String Index 8 Read FendDate Write SetendDate;
  end;
  TDateRangeClass = Class of TDateRange;
  
  { --------------------------------------------------------------------
    TReportRow
    --------------------------------------------------------------------}
  
  TReportRow = Class(TGoogleBaseObject)
  Private
    Fdimensions : TStringArray;
    Fmetrics : TReportRowTypemetricsArray;
  Protected
    //Property setters
    Procedure Setdimensions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setmetrics(AIndex : Integer; const AValue : TReportRowTypemetricsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dimensions : TStringArray Index 0 Read Fdimensions Write Setdimensions;
    Property metrics : TReportRowTypemetricsArray Index 8 Read Fmetrics Write Setmetrics;
  end;
  TReportRowClass = Class of TReportRow;
  
  { --------------------------------------------------------------------
    TCohortGroup
    --------------------------------------------------------------------}
  
  TCohortGroup = Class(TGoogleBaseObject)
  Private
    FlifetimeValue : boolean;
    Fcohorts : TCohortGroupTypecohortsArray;
  Protected
    //Property setters
    Procedure SetlifetimeValue(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setcohorts(AIndex : Integer; const AValue : TCohortGroupTypecohortsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property lifetimeValue : boolean Index 0 Read FlifetimeValue Write SetlifetimeValue;
    Property cohorts : TCohortGroupTypecohortsArray Index 8 Read Fcohorts Write Setcohorts;
  end;
  TCohortGroupClass = Class of TCohortGroup;
  
  { --------------------------------------------------------------------
    TGetReportsResponse
    --------------------------------------------------------------------}
  
  TGetReportsResponse = Class(TGoogleBaseObject)
  Private
    Freports : TGetReportsResponseTypereportsArray;
  Protected
    //Property setters
    Procedure Setreports(AIndex : Integer; const AValue : TGetReportsResponseTypereportsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property reports : TGetReportsResponseTypereportsArray Index 0 Read Freports Write Setreports;
  end;
  TGetReportsResponseClass = Class of TGetReportsResponse;
  
  { --------------------------------------------------------------------
    TMetricHeaderEntry
    --------------------------------------------------------------------}
  
  TMetricHeaderEntry = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Fname : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TMetricHeaderEntryClass = Class of TMetricHeaderEntry;
  
  { --------------------------------------------------------------------
    TMetricFilter
    --------------------------------------------------------------------}
  
  TMetricFilter = Class(TGoogleBaseObject)
  Private
    FmetricName : String;
    F_operator : String;
    FcomparisonValue : String;
    F_not : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetmetricName(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_operator(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcomparisonValue(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_not(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property metricName : String Index 0 Read FmetricName Write SetmetricName;
    Property _operator : String Index 8 Read F_operator Write Set_operator;
    Property comparisonValue : String Index 16 Read FcomparisonValue Write SetcomparisonValue;
    Property _not : boolean Index 24 Read F_not Write Set_not;
  end;
  TMetricFilterClass = Class of TMetricFilter;
  
  { --------------------------------------------------------------------
    TDimension
    --------------------------------------------------------------------}
  
  TDimension = Class(TGoogleBaseObject)
  Private
    FhistogramBuckets : TStringArray;
    Fname : String;
  Protected
    //Property setters
    Procedure SethistogramBuckets(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property histogramBuckets : TStringArray Index 0 Read FhistogramBuckets Write SethistogramBuckets;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TDimensionClass = Class of TDimension;
  
  { --------------------------------------------------------------------
    TPivotValueRegion
    --------------------------------------------------------------------}
  
  TPivotValueRegion = Class(TGoogleBaseObject)
  Private
    Fvalues : TStringArray;
  Protected
    //Property setters
    Procedure Setvalues(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property values : TStringArray Index 0 Read Fvalues Write Setvalues;
  end;
  TPivotValueRegionClass = Class of TPivotValueRegion;
  
  { --------------------------------------------------------------------
    TReportsResource
    --------------------------------------------------------------------}
  
  TReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function BatchGet(aGetReportsRequest : TGetReportsRequest) : TGetReportsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAnalyticsreportingAPI
    --------------------------------------------------------------------}
  
  TAnalyticsreportingAPI = Class(TGoogleAPI)
  Private
    FReportsInstance : TReportsResource;
    Function GetReportsInstance : TReportsResource;virtual;
  Public
    //Override class functions with API info
    Class Function APIName : String; override;
    Class Function APIVersion : String; override;
    Class Function APIRevision : String; override;
    Class Function APIID : String; override;
    Class Function APITitle : String; override;
    Class Function APIDescription : String; override;
    Class Function APIOwnerDomain : String; override;
    Class Function APIOwnerName : String; override;
    Class Function APIIcon16 : String; override;
    Class Function APIIcon32 : String; override;
    Class Function APIdocumentationLink : String; override;
    Class Function APIrootUrl : string; override;
    Class Function APIbasePath : string;override;
    Class Function APIbaseURL : String;override;
    Class Function APIProtocol : string;override;
    Class Function APIservicePath : string;override;
    Class Function APIbatchPath : String;override;
    Class Function APIAuthScopes : TScopeInfoArray;override;
    Class Function APINeedsAuth : Boolean;override;
    Class Procedure RegisterAPIResources; override;
    //Add create function for resources
    Function CreateReportsResource(AOwner : TComponent) : TReportsResource;virtual;overload;
    Function CreateReportsResource : TReportsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ReportsResource : TReportsResource Read GetReportsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TPivotHeader
  --------------------------------------------------------------------}


Procedure TPivotHeader.SettotalPivotGroupsCount(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalPivotGroupsCount=AValue) then exit;
  FtotalPivotGroupsCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotHeader.SetpivotHeaderEntries(AIndex : Integer; const AValue : TPivotHeaderTypepivotHeaderEntriesArray); 

begin
  If (FpivotHeaderEntries=AValue) then exit;
  FpivotHeaderEntries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPivotHeader.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'pivotheaderentries' : SetLength(FpivotHeaderEntries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetric
  --------------------------------------------------------------------}


Procedure TMetric.Setalias(AIndex : Integer; const AValue : String); 

begin
  If (Falias=AValue) then exit;
  Falias:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetric.SetformattingType(AIndex : Integer; const AValue : String); 

begin
  If (FformattingType=AValue) then exit;
  FformattingType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetric.Setexpression(AIndex : Integer; const AValue : String); 

begin
  If (Fexpression=AValue) then exit;
  Fexpression:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColumnHeader
  --------------------------------------------------------------------}


Procedure TColumnHeader.Setdimensions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColumnHeader.SetmetricHeader(AIndex : Integer; const AValue : TMetricHeader); 

begin
  If (FmetricHeader=AValue) then exit;
  FmetricHeader:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TColumnHeader.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dimensions' : SetLength(Fdimensions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDynamicSegment
  --------------------------------------------------------------------}


Procedure TDynamicSegment.SetsessionSegment(AIndex : Integer; const AValue : TSegmentDefinition); 

begin
  If (FsessionSegment=AValue) then exit;
  FsessionSegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDynamicSegment.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDynamicSegment.SetuserSegment(AIndex : Integer; const AValue : TSegmentDefinition); 

begin
  If (FuserSegment=AValue) then exit;
  FuserSegment:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetricHeader
  --------------------------------------------------------------------}


Procedure TMetricHeader.SetmetricHeaderEntries(AIndex : Integer; const AValue : TMetricHeaderTypemetricHeaderEntriesArray); 

begin
  If (FmetricHeaderEntries=AValue) then exit;
  FmetricHeaderEntries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricHeader.SetpivotHeaders(AIndex : Integer; const AValue : TMetricHeaderTypepivotHeadersArray); 

begin
  If (FpivotHeaders=AValue) then exit;
  FpivotHeaders:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMetricHeader.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'metricheaderentries' : SetLength(FmetricHeaderEntries,ALength);
  'pivotheaders' : SetLength(FpivotHeaders,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReport
  --------------------------------------------------------------------}


Procedure TReport.SetcolumnHeader(AIndex : Integer; const AValue : TColumnHeader); 

begin
  If (FcolumnHeader=AValue) then exit;
  FcolumnHeader:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setdata(AIndex : Integer; const AValue : TReportData); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSegmentFilterClause
  --------------------------------------------------------------------}


Procedure TSegmentFilterClause.SetdimensionFilter(AIndex : Integer; const AValue : TSegmentDimensionFilter); 

begin
  If (FdimensionFilter=AValue) then exit;
  FdimensionFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentFilterClause.SetmetricFilter(AIndex : Integer; const AValue : TSegmentMetricFilter); 

begin
  If (FmetricFilter=AValue) then exit;
  FmetricFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentFilterClause.Set_not(AIndex : Integer; const AValue : boolean); 

begin
  If (F_not=AValue) then exit;
  F_not:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSegmentFilterClause.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_not' : Result:='not';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDimensionFilter
  --------------------------------------------------------------------}


Procedure TDimensionFilter.SetdimensionName(AIndex : Integer; const AValue : String); 

begin
  If (FdimensionName=AValue) then exit;
  FdimensionName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionFilter.Set_operator(AIndex : Integer; const AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionFilter.SetcaseSensitive(AIndex : Integer; const AValue : boolean); 

begin
  If (FcaseSensitive=AValue) then exit;
  FcaseSensitive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionFilter.Setexpressions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fexpressions=AValue) then exit;
  Fexpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionFilter.Set_not(AIndex : Integer; const AValue : boolean); 

begin
  If (F_not=AValue) then exit;
  F_not:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDimensionFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  '_not' : Result:='not';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDimensionFilter.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'expressions' : SetLength(Fexpressions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSegmentDimensionFilter
  --------------------------------------------------------------------}


Procedure TSegmentDimensionFilter.SetmaxComparisonValue(AIndex : Integer; const AValue : String); 

begin
  If (FmaxComparisonValue=AValue) then exit;
  FmaxComparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentDimensionFilter.SetdimensionName(AIndex : Integer; const AValue : String); 

begin
  If (FdimensionName=AValue) then exit;
  FdimensionName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentDimensionFilter.SetcaseSensitive(AIndex : Integer; const AValue : boolean); 

begin
  If (FcaseSensitive=AValue) then exit;
  FcaseSensitive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentDimensionFilter.Set_operator(AIndex : Integer; const AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentDimensionFilter.Setexpressions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fexpressions=AValue) then exit;
  Fexpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentDimensionFilter.SetminComparisonValue(AIndex : Integer; const AValue : String); 

begin
  If (FminComparisonValue=AValue) then exit;
  FminComparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSegmentDimensionFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSegmentDimensionFilter.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'expressions' : SetLength(Fexpressions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReportRequest
  --------------------------------------------------------------------}


Procedure TReportRequest.SetcohortGroup(AIndex : Integer; const AValue : TCohortGroup); 

begin
  If (FcohortGroup=AValue) then exit;
  FcohortGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.Setdimensions(AIndex : Integer; const AValue : TReportRequestTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetmetricFilterClauses(AIndex : Integer; const AValue : TReportRequestTypemetricFilterClausesArray); 

begin
  If (FmetricFilterClauses=AValue) then exit;
  FmetricFilterClauses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SethideTotals(AIndex : Integer; const AValue : boolean); 

begin
  If (FhideTotals=AValue) then exit;
  FhideTotals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetincludeEmptyRows(AIndex : Integer; const AValue : boolean); 

begin
  If (FincludeEmptyRows=AValue) then exit;
  FincludeEmptyRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetdimensionFilterClauses(AIndex : Integer; const AValue : TReportRequestTypedimensionFilterClausesArray); 

begin
  If (FdimensionFilterClauses=AValue) then exit;
  FdimensionFilterClauses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.Setpivots(AIndex : Integer; const AValue : TReportRequestTypepivotsArray); 

begin
  If (Fpivots=AValue) then exit;
  Fpivots:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetdateRanges(AIndex : Integer; const AValue : TReportRequestTypedateRangesArray); 

begin
  If (FdateRanges=AValue) then exit;
  FdateRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.Setsegments(AIndex : Integer; const AValue : TReportRequestTypesegmentsArray); 

begin
  If (Fsegments=AValue) then exit;
  Fsegments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetsamplingLevel(AIndex : Integer; const AValue : String); 

begin
  If (FsamplingLevel=AValue) then exit;
  FsamplingLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.Setmetrics(AIndex : Integer; const AValue : TReportRequestTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetorderBys(AIndex : Integer; const AValue : TReportRequestTypeorderBysArray); 

begin
  If (ForderBys=AValue) then exit;
  ForderBys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetfiltersExpression(AIndex : Integer; const AValue : String); 

begin
  If (FfiltersExpression=AValue) then exit;
  FfiltersExpression:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SethideValueRanges(AIndex : Integer; const AValue : boolean); 

begin
  If (FhideValueRanges=AValue) then exit;
  FhideValueRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetviewId(AIndex : Integer; const AValue : String); 

begin
  If (FviewId=AValue) then exit;
  FviewId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReportRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dimensions' : SetLength(Fdimensions,ALength);
  'metricfilterclauses' : SetLength(FmetricFilterClauses,ALength);
  'dimensionfilterclauses' : SetLength(FdimensionFilterClauses,ALength);
  'pivots' : SetLength(Fpivots,ALength);
  'dateranges' : SetLength(FdateRanges,ALength);
  'segments' : SetLength(Fsegments,ALength);
  'metrics' : SetLength(Fmetrics,ALength);
  'orderbys' : SetLength(ForderBys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSimpleSegment
  --------------------------------------------------------------------}


Procedure TSimpleSegment.SetorFiltersForSegment(AIndex : Integer; const AValue : TSimpleSegmentTypeorFiltersForSegmentArray); 

begin
  If (ForFiltersForSegment=AValue) then exit;
  ForFiltersForSegment:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSimpleSegment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'orfiltersforsegment' : SetLength(ForFiltersForSegment,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSegmentDefinition
  --------------------------------------------------------------------}


Procedure TSegmentDefinition.SetsegmentFilters(AIndex : Integer; const AValue : TSegmentDefinitionTypesegmentFiltersArray); 

begin
  If (FsegmentFilters=AValue) then exit;
  FsegmentFilters:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSegmentDefinition.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'segmentfilters' : SetLength(FsegmentFilters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSegmentMetricFilter
  --------------------------------------------------------------------}


Procedure TSegmentMetricFilter.SetmetricName(AIndex : Integer; const AValue : String); 

begin
  If (FmetricName=AValue) then exit;
  FmetricName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentMetricFilter.Set_operator(AIndex : Integer; const AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentMetricFilter.SetcomparisonValue(AIndex : Integer; const AValue : String); 

begin
  If (FcomparisonValue=AValue) then exit;
  FcomparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentMetricFilter.Setscope(AIndex : Integer; const AValue : String); 

begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentMetricFilter.SetmaxComparisonValue(AIndex : Integer; const AValue : String); 

begin
  If (FmaxComparisonValue=AValue) then exit;
  FmaxComparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSegmentMetricFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TReportData
  --------------------------------------------------------------------}


Procedure TReportData.SetrowCount(AIndex : Integer; const AValue : integer); 

begin
  If (FrowCount=AValue) then exit;
  FrowCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportData.SetsamplingSpaceSizes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsamplingSpaceSizes=AValue) then exit;
  FsamplingSpaceSizes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportData.Setmaximums(AIndex : Integer; const AValue : TReportDataTypemaximumsArray); 

begin
  If (Fmaximums=AValue) then exit;
  Fmaximums:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportData.SetsamplesReadCounts(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsamplesReadCounts=AValue) then exit;
  FsamplesReadCounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportData.Setminimums(AIndex : Integer; const AValue : TReportDataTypeminimumsArray); 

begin
  If (Fminimums=AValue) then exit;
  Fminimums:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportData.Setrows(AIndex : Integer; const AValue : TReportDataTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportData.Settotals(AIndex : Integer; const AValue : TReportDataTypetotalsArray); 

begin
  If (Ftotals=AValue) then exit;
  Ftotals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportData.SetisDataGolden(AIndex : Integer; const AValue : boolean); 

begin
  If (FisDataGolden=AValue) then exit;
  FisDataGolden:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReportData.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'samplingspacesizes' : SetLength(FsamplingSpaceSizes,ALength);
  'maximums' : SetLength(Fmaximums,ALength);
  'samplesreadcounts' : SetLength(FsamplesReadCounts,ALength);
  'minimums' : SetLength(Fminimums,ALength);
  'rows' : SetLength(Frows,ALength);
  'totals' : SetLength(Ftotals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetReportsRequest
  --------------------------------------------------------------------}


Procedure TGetReportsRequest.SetreportRequests(AIndex : Integer; const AValue : TGetReportsRequestTypereportRequestsArray); 

begin
  If (FreportRequests=AValue) then exit;
  FreportRequests:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetReportsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'reportrequests' : SetLength(FreportRequests,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOrderBy
  --------------------------------------------------------------------}


Procedure TOrderBy.SetsortOrder(AIndex : Integer; const AValue : String); 

begin
  If (FsortOrder=AValue) then exit;
  FsortOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderBy.SetorderType(AIndex : Integer; const AValue : String); 

begin
  If (ForderType=AValue) then exit;
  ForderType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrderBy.SetfieldName(AIndex : Integer; const AValue : String); 

begin
  If (FfieldName=AValue) then exit;
  FfieldName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCohort
  --------------------------------------------------------------------}


Procedure TCohort.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCohort.SetdateRange(AIndex : Integer; const AValue : TDateRange); 

begin
  If (FdateRange=AValue) then exit;
  FdateRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCohort.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCohort.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TOrFiltersForSegment
  --------------------------------------------------------------------}


Procedure TOrFiltersForSegment.SetsegmentFilterClauses(AIndex : Integer; const AValue : TOrFiltersForSegmentTypesegmentFilterClausesArray); 

begin
  If (FsegmentFilterClauses=AValue) then exit;
  FsegmentFilterClauses:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOrFiltersForSegment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'segmentfilterclauses' : SetLength(FsegmentFilterClauses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSequenceSegment
  --------------------------------------------------------------------}


Procedure TSequenceSegment.SetfirstStepShouldMatchFirstHit(AIndex : Integer; const AValue : boolean); 

begin
  If (FfirstStepShouldMatchFirstHit=AValue) then exit;
  FfirstStepShouldMatchFirstHit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSequenceSegment.SetsegmentSequenceSteps(AIndex : Integer; const AValue : TSequenceSegmentTypesegmentSequenceStepsArray); 

begin
  If (FsegmentSequenceSteps=AValue) then exit;
  FsegmentSequenceSteps:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSequenceSegment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'segmentsequencesteps' : SetLength(FsegmentSequenceSteps,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSegmentFilter
  --------------------------------------------------------------------}


Procedure TSegmentFilter.SetsequenceSegment(AIndex : Integer; const AValue : TSequenceSegment); 

begin
  If (FsequenceSegment=AValue) then exit;
  FsequenceSegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentFilter.Set_not(AIndex : Integer; const AValue : boolean); 

begin
  If (F_not=AValue) then exit;
  F_not:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentFilter.SetsimpleSegment(AIndex : Integer; const AValue : TSimpleSegment); 

begin
  If (FsimpleSegment=AValue) then exit;
  FsimpleSegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSegmentFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_not' : Result:='not';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPivotHeaderEntry
  --------------------------------------------------------------------}


Procedure TPivotHeaderEntry.SetdimensionNames(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdimensionNames=AValue) then exit;
  FdimensionNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotHeaderEntry.SetdimensionValues(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdimensionValues=AValue) then exit;
  FdimensionValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotHeaderEntry.Setmetric(AIndex : Integer; const AValue : TMetricHeaderEntry); 

begin
  If (Fmetric=AValue) then exit;
  Fmetric:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPivotHeaderEntry.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dimensionnames' : SetLength(FdimensionNames,ALength);
  'dimensionvalues' : SetLength(FdimensionValues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDimensionFilterClause
  --------------------------------------------------------------------}


Procedure TDimensionFilterClause.Set_operator(AIndex : Integer; const AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionFilterClause.Setfilters(AIndex : Integer; const AValue : TDimensionFilterClauseTypefiltersArray); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDimensionFilterClause.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDimensionFilterClause.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'filters' : SetLength(Ffilters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSegmentSequenceStep
  --------------------------------------------------------------------}


Procedure TSegmentSequenceStep.SetmatchType(AIndex : Integer; const AValue : String); 

begin
  If (FmatchType=AValue) then exit;
  FmatchType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentSequenceStep.SetorFiltersForSegment(AIndex : Integer; const AValue : TSegmentSequenceStepTypeorFiltersForSegmentArray); 

begin
  If (ForFiltersForSegment=AValue) then exit;
  ForFiltersForSegment:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSegmentSequenceStep.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'orfiltersforsegment' : SetLength(ForFiltersForSegment,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPivot
  --------------------------------------------------------------------}


Procedure TPivot.Setdimensions(AIndex : Integer; const AValue : TPivotTypedimensionsArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivot.Setmetrics(AIndex : Integer; const AValue : TPivotTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivot.SetmaxGroupCount(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxGroupCount=AValue) then exit;
  FmaxGroupCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivot.SetdimensionFilterClauses(AIndex : Integer; const AValue : TPivotTypedimensionFilterClausesArray); 

begin
  If (FdimensionFilterClauses=AValue) then exit;
  FdimensionFilterClauses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivot.SetstartGroup(AIndex : Integer; const AValue : integer); 

begin
  If (FstartGroup=AValue) then exit;
  FstartGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPivot.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dimensions' : SetLength(Fdimensions,ALength);
  'metrics' : SetLength(Fmetrics,ALength);
  'dimensionfilterclauses' : SetLength(FdimensionFilterClauses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDateRangeValues
  --------------------------------------------------------------------}


Procedure TDateRangeValues.Setvalues(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDateRangeValues.SetpivotValueRegions(AIndex : Integer; const AValue : TDateRangeValuesTypepivotValueRegionsArray); 

begin
  If (FpivotValueRegions=AValue) then exit;
  FpivotValueRegions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDateRangeValues.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  'pivotvalueregions' : SetLength(FpivotValueRegions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetricFilterClause
  --------------------------------------------------------------------}


Procedure TMetricFilterClause.Set_operator(AIndex : Integer; const AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricFilterClause.Setfilters(AIndex : Integer; const AValue : TMetricFilterClauseTypefiltersArray); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMetricFilterClause.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMetricFilterClause.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'filters' : SetLength(Ffilters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSegment
  --------------------------------------------------------------------}


Procedure TSegment.SetdynamicSegment(AIndex : Integer; const AValue : TDynamicSegment); 

begin
  If (FdynamicSegment=AValue) then exit;
  FdynamicSegment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegment.SetsegmentId(AIndex : Integer; const AValue : String); 

begin
  If (FsegmentId=AValue) then exit;
  FsegmentId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDateRange
  --------------------------------------------------------------------}


Procedure TDateRange.SetstartDate(AIndex : Integer; const AValue : String); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDateRange.SetendDate(AIndex : Integer; const AValue : String); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportRow
  --------------------------------------------------------------------}


Procedure TReportRow.Setdimensions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportRow.Setmetrics(AIndex : Integer; const AValue : TReportRowTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReportRow.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dimensions' : SetLength(Fdimensions,ALength);
  'metrics' : SetLength(Fmetrics,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCohortGroup
  --------------------------------------------------------------------}


Procedure TCohortGroup.SetlifetimeValue(AIndex : Integer; const AValue : boolean); 

begin
  If (FlifetimeValue=AValue) then exit;
  FlifetimeValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCohortGroup.Setcohorts(AIndex : Integer; const AValue : TCohortGroupTypecohortsArray); 

begin
  If (Fcohorts=AValue) then exit;
  Fcohorts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCohortGroup.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'cohorts' : SetLength(Fcohorts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetReportsResponse
  --------------------------------------------------------------------}


Procedure TGetReportsResponse.Setreports(AIndex : Integer; const AValue : TGetReportsResponseTypereportsArray); 

begin
  If (Freports=AValue) then exit;
  Freports:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetReportsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'reports' : SetLength(Freports,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetricHeaderEntry
  --------------------------------------------------------------------}


Procedure TMetricHeaderEntry.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricHeaderEntry.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMetricHeaderEntry.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TMetricFilter
  --------------------------------------------------------------------}


Procedure TMetricFilter.SetmetricName(AIndex : Integer; const AValue : String); 

begin
  If (FmetricName=AValue) then exit;
  FmetricName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricFilter.Set_operator(AIndex : Integer; const AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricFilter.SetcomparisonValue(AIndex : Integer; const AValue : String); 

begin
  If (FcomparisonValue=AValue) then exit;
  FcomparisonValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricFilter.Set_not(AIndex : Integer; const AValue : boolean); 

begin
  If (F_not=AValue) then exit;
  F_not:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMetricFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  '_not' : Result:='not';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDimension
  --------------------------------------------------------------------}


Procedure TDimension.SethistogramBuckets(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FhistogramBuckets=AValue) then exit;
  FhistogramBuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimension.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDimension.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'histogrambuckets' : SetLength(FhistogramBuckets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPivotValueRegion
  --------------------------------------------------------------------}


Procedure TPivotValueRegion.Setvalues(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPivotValueRegion.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReportsResource
  --------------------------------------------------------------------}


Class Function TReportsResource.ResourceName : String;

begin
  Result:='reports';
end;

Class Function TReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TanalyticsreportingAPI;
end;

Function TReportsResource.BatchGet(aGetReportsRequest : TGetReportsRequest) : TGetReportsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v4/reports:batchGet';
  _Methodid   = 'analyticsreporting.reports.batchGet';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aGetReportsRequest,TGetReportsResponse) as TGetReportsResponse;
end;



{ --------------------------------------------------------------------
  TAnalyticsreportingAPI
  --------------------------------------------------------------------}

Class Function TAnalyticsreportingAPI.APIName : String;

begin
  Result:='analyticsreporting';
end;

Class Function TAnalyticsreportingAPI.APIVersion : String;

begin
  Result:='v4';
end;

Class Function TAnalyticsreportingAPI.APIRevision : String;

begin
  Result:='20160512';
end;

Class Function TAnalyticsreportingAPI.APIID : String;

begin
  Result:='analyticsreporting:v4';
end;

Class Function TAnalyticsreportingAPI.APITitle : String;

begin
  Result:='Google Analytics Reporting API';
end;

Class Function TAnalyticsreportingAPI.APIDescription : String;

begin
  Result:='Accesses Analytics report data.';
end;

Class Function TAnalyticsreportingAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAnalyticsreportingAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAnalyticsreportingAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TAnalyticsreportingAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TAnalyticsreportingAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/analytics/devguides/reporting/core/v4/';
end;

Class Function TAnalyticsreportingAPI.APIrootUrl : string;

begin
  Result:='https://analyticsreporting.googleapis.com/';
end;

Class Function TAnalyticsreportingAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TAnalyticsreportingAPI.APIbaseURL : String;

begin
  Result:='https://analyticsreporting.googleapis.com/';
end;

Class Function TAnalyticsreportingAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAnalyticsreportingAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TAnalyticsreportingAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAnalyticsreportingAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/analytics.readonly';
  Result[0].Description:='View your Google Analytics data';
  Result[1].Name:='https://www.googleapis.com/auth/analytics';
  Result[1].Description:='View and manage your Google Analytics data';
  
end;

Class Function TAnalyticsreportingAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAnalyticsreportingAPI.RegisterAPIResources;

begin
  TPivotHeader.RegisterObject;
  TMetric.RegisterObject;
  TColumnHeader.RegisterObject;
  TDynamicSegment.RegisterObject;
  TMetricHeader.RegisterObject;
  TReport.RegisterObject;
  TSegmentFilterClause.RegisterObject;
  TDimensionFilter.RegisterObject;
  TSegmentDimensionFilter.RegisterObject;
  TReportRequest.RegisterObject;
  TSimpleSegment.RegisterObject;
  TSegmentDefinition.RegisterObject;
  TSegmentMetricFilter.RegisterObject;
  TReportData.RegisterObject;
  TGetReportsRequest.RegisterObject;
  TOrderBy.RegisterObject;
  TCohort.RegisterObject;
  TOrFiltersForSegment.RegisterObject;
  TSequenceSegment.RegisterObject;
  TSegmentFilter.RegisterObject;
  TPivotHeaderEntry.RegisterObject;
  TDimensionFilterClause.RegisterObject;
  TSegmentSequenceStep.RegisterObject;
  TPivot.RegisterObject;
  TDateRangeValues.RegisterObject;
  TMetricFilterClause.RegisterObject;
  TSegment.RegisterObject;
  TDateRange.RegisterObject;
  TReportRow.RegisterObject;
  TCohortGroup.RegisterObject;
  TGetReportsResponse.RegisterObject;
  TMetricHeaderEntry.RegisterObject;
  TMetricFilter.RegisterObject;
  TDimension.RegisterObject;
  TPivotValueRegion.RegisterObject;
end;


Function TAnalyticsreportingAPI.GetReportsInstance : TReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TAnalyticsreportingAPI.CreateReportsResource : TReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TAnalyticsreportingAPI.CreateReportsResource(AOwner : TComponent) : TReportsResource;

begin
  Result:=TReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TAnalyticsreportingAPI.RegisterAPI;
end.
