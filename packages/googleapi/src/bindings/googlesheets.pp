unit googlesheets;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAddNamedRangeResponse = Class;
  TUpdateProtectedRangeRequest = Class;
  TPadding = Class;
  TMergeCellsRequest = Class;
  TAddSheetResponse = Class;
  TPivotGroupValueMetadata = Class;
  TUpdateEmbeddedObjectPositionResponse = Class;
  TUpdateConditionalFormatRuleRequest = Class;
  TTextFormat = Class;
  TUpdateChartSpecRequest = Class;
  TGridCoordinate = Class;
  TDeleteFilterViewRequest = Class;
  TBatchUpdateValuesResponse = Class;
  TUpdateNamedRangeRequest = Class;
  TUpdateValuesResponse = Class;
  TSpreadsheetProperties = Class;
  TCellData = Class;
  TUnmergeCellsRequest = Class;
  TTextToColumnsRequest = Class;
  TAddProtectedRangeResponse = Class;
  TBooleanCondition = Class;
  TDeleteProtectedRangeRequest = Class;
  TBasicChartDomain = Class;
  TDimensionRange = Class;
  TResponse = Class;
  TAddConditionalFormatRuleRequest = Class;
  TFilterView = Class;
  TSortRangeRequest = Class;
  TTextFormatRun = Class;
  TUpdateFilterViewRequest = Class;
  TUpdateConditionalFormatRuleResponse = Class;
  TFilterCriteria = Class;
  TDeleteDimensionRequest = Class;
  TPivotTable = Class;
  TDataValidationRule = Class;
  TUpdateSpreadsheetPropertiesRequest = Class;
  TChartSourceRange = Class;
  TBatchUpdateValuesRequest = Class;
  TClearBasicFilterRequest = Class;
  TConditionalFormatRule = Class;
  TUpdateBordersRequest = Class;
  TPivotFilterCriteria = Class;
  TBorders = Class;
  TEmbeddedChart = Class;
  TColor = Class;
  TAddSheetRequest = Class;
  TAddProtectedRangeRequest = Class;
  TValueRange = Class;
  TFindReplaceResponse = Class;
  TCellFormat = Class;
  TMoveDimensionRequest = Class;
  TBasicChartAxis = Class;
  TPivotGroupSortValueBucket = Class;
  TDimensionProperties = Class;
  TEmbeddedObjectPosition = Class;
  TInterpolationPoint = Class;
  TErrorValue = Class;
  TDuplicateFilterViewRequest = Class;
  TBatchUpdateSpreadsheetRequest = Class;
  TSheetProperties = Class;
  TProtectedRange = Class;
  TDeleteConditionalFormatRuleRequest = Class;
  TChartSpec = Class;
  TSourceAndDestination = Class;
  TConditionValue = Class;
  TPasteDataRequest = Class;
  TFindReplaceRequest = Class;
  TSortSpec = Class;
  TCopySheetToAnotherSpreadsheetRequest = Class;
  TNumberFormat = Class;
  TUpdateDimensionPropertiesRequest = Class;
  TEditors = Class;
  TSpreadsheet = Class;
  TGridData = Class;
  TPivotValue = Class;
  TBasicFilter = Class;
  TDuplicateSheetRequest = Class;
  TAddFilterViewResponse = Class;
  TDuplicateSheetResponse = Class;
  TBorder = Class;
  TAddNamedRangeRequest = Class;
  TAddChartResponse = Class;
  TAppendCellsRequest = Class;
  TRowData = Class;
  TBasicChartSeries = Class;
  TRepeatCellRequest = Class;
  TBasicChartSpec = Class;
  TNamedRange = Class;
  TSetBasicFilterRequest = Class;
  TUpdateEmbeddedObjectPositionRequest = Class;
  TAutoResizeDimensionsRequest = Class;
  TDuplicateFilterViewResponse = Class;
  TPivotGroup = Class;
  TGridRange = Class;
  TDeleteSheetRequest = Class;
  TChartData = Class;
  TSheet = Class;
  TCopyPasteRequest = Class;
  TUpdateCellsRequest = Class;
  TExtendedValue = Class;
  TBatchUpdateSpreadsheetResponse = Class;
  TGradientRule = Class;
  TCutPasteRequest = Class;
  TOverlayPosition = Class;
  TAutoFillRequest = Class;
  TPieChartSpec = Class;
  TUpdateSheetPropertiesRequest = Class;
  TBooleanRule = Class;
  TAppendDimensionRequest = Class;
  TAddFilterViewRequest = Class;
  TGridProperties = Class;
  TDeleteNamedRangeRequest = Class;
  TAddChartRequest = Class;
  TSetDataValidationRequest = Class;
  TRequest = Class;
  TBatchGetValuesResponse = Class;
  TInsertDimensionRequest = Class;
  TDeleteEmbeddedObjectRequest = Class;
  TDeleteConditionalFormatRuleResponse = Class;
  TAddNamedRangeResponseArray = Array of TAddNamedRangeResponse;
  TUpdateProtectedRangeRequestArray = Array of TUpdateProtectedRangeRequest;
  TPaddingArray = Array of TPadding;
  TMergeCellsRequestArray = Array of TMergeCellsRequest;
  TAddSheetResponseArray = Array of TAddSheetResponse;
  TPivotGroupValueMetadataArray = Array of TPivotGroupValueMetadata;
  TUpdateEmbeddedObjectPositionResponseArray = Array of TUpdateEmbeddedObjectPositionResponse;
  TUpdateConditionalFormatRuleRequestArray = Array of TUpdateConditionalFormatRuleRequest;
  TTextFormatArray = Array of TTextFormat;
  TUpdateChartSpecRequestArray = Array of TUpdateChartSpecRequest;
  TGridCoordinateArray = Array of TGridCoordinate;
  TDeleteFilterViewRequestArray = Array of TDeleteFilterViewRequest;
  TBatchUpdateValuesResponseArray = Array of TBatchUpdateValuesResponse;
  TUpdateNamedRangeRequestArray = Array of TUpdateNamedRangeRequest;
  TUpdateValuesResponseArray = Array of TUpdateValuesResponse;
  TSpreadsheetPropertiesArray = Array of TSpreadsheetProperties;
  TCellDataArray = Array of TCellData;
  TUnmergeCellsRequestArray = Array of TUnmergeCellsRequest;
  TTextToColumnsRequestArray = Array of TTextToColumnsRequest;
  TAddProtectedRangeResponseArray = Array of TAddProtectedRangeResponse;
  TBooleanConditionArray = Array of TBooleanCondition;
  TDeleteProtectedRangeRequestArray = Array of TDeleteProtectedRangeRequest;
  TBasicChartDomainArray = Array of TBasicChartDomain;
  TDimensionRangeArray = Array of TDimensionRange;
  TResponseArray = Array of TResponse;
  TAddConditionalFormatRuleRequestArray = Array of TAddConditionalFormatRuleRequest;
  TFilterViewArray = Array of TFilterView;
  TSortRangeRequestArray = Array of TSortRangeRequest;
  TTextFormatRunArray = Array of TTextFormatRun;
  TUpdateFilterViewRequestArray = Array of TUpdateFilterViewRequest;
  TUpdateConditionalFormatRuleResponseArray = Array of TUpdateConditionalFormatRuleResponse;
  TFilterCriteriaArray = Array of TFilterCriteria;
  TDeleteDimensionRequestArray = Array of TDeleteDimensionRequest;
  TPivotTableArray = Array of TPivotTable;
  TDataValidationRuleArray = Array of TDataValidationRule;
  TUpdateSpreadsheetPropertiesRequestArray = Array of TUpdateSpreadsheetPropertiesRequest;
  TChartSourceRangeArray = Array of TChartSourceRange;
  TBatchUpdateValuesRequestArray = Array of TBatchUpdateValuesRequest;
  TClearBasicFilterRequestArray = Array of TClearBasicFilterRequest;
  TConditionalFormatRuleArray = Array of TConditionalFormatRule;
  TUpdateBordersRequestArray = Array of TUpdateBordersRequest;
  TPivotFilterCriteriaArray = Array of TPivotFilterCriteria;
  TBordersArray = Array of TBorders;
  TEmbeddedChartArray = Array of TEmbeddedChart;
  TColorArray = Array of TColor;
  TAddSheetRequestArray = Array of TAddSheetRequest;
  TAddProtectedRangeRequestArray = Array of TAddProtectedRangeRequest;
  TValueRangeArray = Array of TValueRange;
  TFindReplaceResponseArray = Array of TFindReplaceResponse;
  TCellFormatArray = Array of TCellFormat;
  TMoveDimensionRequestArray = Array of TMoveDimensionRequest;
  TBasicChartAxisArray = Array of TBasicChartAxis;
  TPivotGroupSortValueBucketArray = Array of TPivotGroupSortValueBucket;
  TDimensionPropertiesArray = Array of TDimensionProperties;
  TEmbeddedObjectPositionArray = Array of TEmbeddedObjectPosition;
  TInterpolationPointArray = Array of TInterpolationPoint;
  TErrorValueArray = Array of TErrorValue;
  TDuplicateFilterViewRequestArray = Array of TDuplicateFilterViewRequest;
  TBatchUpdateSpreadsheetRequestArray = Array of TBatchUpdateSpreadsheetRequest;
  TSheetPropertiesArray = Array of TSheetProperties;
  TProtectedRangeArray = Array of TProtectedRange;
  TDeleteConditionalFormatRuleRequestArray = Array of TDeleteConditionalFormatRuleRequest;
  TChartSpecArray = Array of TChartSpec;
  TSourceAndDestinationArray = Array of TSourceAndDestination;
  TConditionValueArray = Array of TConditionValue;
  TPasteDataRequestArray = Array of TPasteDataRequest;
  TFindReplaceRequestArray = Array of TFindReplaceRequest;
  TSortSpecArray = Array of TSortSpec;
  TCopySheetToAnotherSpreadsheetRequestArray = Array of TCopySheetToAnotherSpreadsheetRequest;
  TNumberFormatArray = Array of TNumberFormat;
  TUpdateDimensionPropertiesRequestArray = Array of TUpdateDimensionPropertiesRequest;
  TEditorsArray = Array of TEditors;
  TSpreadsheetArray = Array of TSpreadsheet;
  TGridDataArray = Array of TGridData;
  TPivotValueArray = Array of TPivotValue;
  TBasicFilterArray = Array of TBasicFilter;
  TDuplicateSheetRequestArray = Array of TDuplicateSheetRequest;
  TAddFilterViewResponseArray = Array of TAddFilterViewResponse;
  TDuplicateSheetResponseArray = Array of TDuplicateSheetResponse;
  TBorderArray = Array of TBorder;
  TAddNamedRangeRequestArray = Array of TAddNamedRangeRequest;
  TAddChartResponseArray = Array of TAddChartResponse;
  TAppendCellsRequestArray = Array of TAppendCellsRequest;
  TRowDataArray = Array of TRowData;
  TBasicChartSeriesArray = Array of TBasicChartSeries;
  TRepeatCellRequestArray = Array of TRepeatCellRequest;
  TBasicChartSpecArray = Array of TBasicChartSpec;
  TNamedRangeArray = Array of TNamedRange;
  TSetBasicFilterRequestArray = Array of TSetBasicFilterRequest;
  TUpdateEmbeddedObjectPositionRequestArray = Array of TUpdateEmbeddedObjectPositionRequest;
  TAutoResizeDimensionsRequestArray = Array of TAutoResizeDimensionsRequest;
  TDuplicateFilterViewResponseArray = Array of TDuplicateFilterViewResponse;
  TPivotGroupArray = Array of TPivotGroup;
  TGridRangeArray = Array of TGridRange;
  TDeleteSheetRequestArray = Array of TDeleteSheetRequest;
  TChartDataArray = Array of TChartData;
  TSheetArray = Array of TSheet;
  TCopyPasteRequestArray = Array of TCopyPasteRequest;
  TUpdateCellsRequestArray = Array of TUpdateCellsRequest;
  TExtendedValueArray = Array of TExtendedValue;
  TBatchUpdateSpreadsheetResponseArray = Array of TBatchUpdateSpreadsheetResponse;
  TGradientRuleArray = Array of TGradientRule;
  TCutPasteRequestArray = Array of TCutPasteRequest;
  TOverlayPositionArray = Array of TOverlayPosition;
  TAutoFillRequestArray = Array of TAutoFillRequest;
  TPieChartSpecArray = Array of TPieChartSpec;
  TUpdateSheetPropertiesRequestArray = Array of TUpdateSheetPropertiesRequest;
  TBooleanRuleArray = Array of TBooleanRule;
  TAppendDimensionRequestArray = Array of TAppendDimensionRequest;
  TAddFilterViewRequestArray = Array of TAddFilterViewRequest;
  TGridPropertiesArray = Array of TGridProperties;
  TDeleteNamedRangeRequestArray = Array of TDeleteNamedRangeRequest;
  TAddChartRequestArray = Array of TAddChartRequest;
  TSetDataValidationRequestArray = Array of TSetDataValidationRequest;
  TRequestArray = Array of TRequest;
  TBatchGetValuesResponseArray = Array of TBatchGetValuesResponse;
  TInsertDimensionRequestArray = Array of TInsertDimensionRequest;
  TDeleteEmbeddedObjectRequestArray = Array of TDeleteEmbeddedObjectRequest;
  TDeleteConditionalFormatRuleResponseArray = Array of TDeleteConditionalFormatRuleResponse;
  //Anonymous types, using auto-generated names
  TFilterViewTypecriteria = Class;
  TPivotTableTypecriteria = Class;
  TBasicFilterTypecriteria = Class;
  TBatchUpdateValuesResponseTyperesponsesArray = Array of TUpdateValuesResponse;
  TCellDataTypetextFormatRunsArray = Array of TTextFormatRun;
  TBooleanConditionTypevaluesArray = Array of TConditionValue;
  TFilterViewTypesortSpecsArray = Array of TSortSpec;
  TSortRangeRequestTypesortSpecsArray = Array of TSortSpec;
  TPivotTableTypecolumnsArray = Array of TPivotGroup;
  TPivotTableTyperowsArray = Array of TPivotGroup;
  TPivotTableTypevaluesArray = Array of TPivotValue;
  TChartSourceRangeTypesourcesArray = Array of TGridRange;
  TBatchUpdateValuesRequestTypedataArray = Array of TValueRange;
  TConditionalFormatRuleTyperangesArray = Array of TGridRange;
  TValueRangeTypevaluesArray = Array of TTJSONSchemaArray;
  TPivotGroupSortValueBucketTypebucketsArray = Array of TExtendedValue;
  TBatchUpdateSpreadsheetRequestTyperequestsArray = Array of TRequest;
  TProtectedRangeTypeunprotectedRangesArray = Array of TGridRange;
  TSpreadsheetTypesheetsArray = Array of TSheet;
  TSpreadsheetTypenamedRangesArray = Array of TNamedRange;
  TGridDataTypecolumnMetadataArray = Array of TDimensionProperties;
  TGridDataTyperowDataArray = Array of TRowData;
  TGridDataTyperowMetadataArray = Array of TDimensionProperties;
  TBasicFilterTypesortSpecsArray = Array of TSortSpec;
  TAppendCellsRequestTyperowsArray = Array of TRowData;
  TRowDataTypevaluesArray = Array of TCellData;
  TBasicChartSpecTypedomainsArray = Array of TBasicChartDomain;
  TBasicChartSpecTypeseriesArray = Array of TBasicChartSeries;
  TBasicChartSpecTypeaxisArray = Array of TBasicChartAxis;
  TPivotGroupTypevalueMetadataArray = Array of TPivotGroupValueMetadata;
  TSheetTypechartsArray = Array of TEmbeddedChart;
  TSheetTypefilterViewsArray = Array of TFilterView;
  TSheetTypeconditionalFormatsArray = Array of TConditionalFormatRule;
  TSheetTypeprotectedRangesArray = Array of TProtectedRange;
  TSheetTypemergesArray = Array of TGridRange;
  TSheetTypedataArray = Array of TGridData;
  TUpdateCellsRequestTyperowsArray = Array of TRowData;
  TBatchUpdateSpreadsheetResponseTyperepliesArray = Array of TResponse;
  TBatchGetValuesResponseTypevalueRangesArray = Array of TValueRange;
  
  { --------------------------------------------------------------------
    TAddNamedRangeResponse
    --------------------------------------------------------------------}
  
  TAddNamedRangeResponse = Class(TGoogleBaseObject)
  Private
    FnamedRange : TNamedRange;
  Protected
    //Property setters
    Procedure SetnamedRange(AIndex : Integer; const AValue : TNamedRange); virtual;
  Public
  Published
    Property namedRange : TNamedRange Index 0 Read FnamedRange Write SetnamedRange;
  end;
  TAddNamedRangeResponseClass = Class of TAddNamedRangeResponse;
  
  { --------------------------------------------------------------------
    TUpdateProtectedRangeRequest
    --------------------------------------------------------------------}
  
  TUpdateProtectedRangeRequest = Class(TGoogleBaseObject)
  Private
    FprotectedRange : TProtectedRange;
    Ffields : String;
  Protected
    //Property setters
    Procedure SetprotectedRange(AIndex : Integer; const AValue : TProtectedRange); virtual;
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property protectedRange : TProtectedRange Index 0 Read FprotectedRange Write SetprotectedRange;
    Property fields : String Index 8 Read Ffields Write Setfields;
  end;
  TUpdateProtectedRangeRequestClass = Class of TUpdateProtectedRangeRequest;
  
  { --------------------------------------------------------------------
    TPadding
    --------------------------------------------------------------------}
  
  TPadding = Class(TGoogleBaseObject)
  Private
    Fright : integer;
    Fleft : integer;
    Ftop : integer;
    Fbottom : integer;
  Protected
    //Property setters
    Procedure Setright(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setleft(AIndex : Integer; const AValue : integer); virtual;
    Procedure Settop(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setbottom(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property right : integer Index 0 Read Fright Write Setright;
    Property left : integer Index 8 Read Fleft Write Setleft;
    Property top : integer Index 16 Read Ftop Write Settop;
    Property bottom : integer Index 24 Read Fbottom Write Setbottom;
  end;
  TPaddingClass = Class of TPadding;
  
  { --------------------------------------------------------------------
    TMergeCellsRequest
    --------------------------------------------------------------------}
  
  TMergeCellsRequest = Class(TGoogleBaseObject)
  Private
    FmergeType : String;
    Frange : TGridRange;
  Protected
    //Property setters
    Procedure SetmergeType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
  Public
  Published
    Property mergeType : String Index 0 Read FmergeType Write SetmergeType;
    Property range : TGridRange Index 8 Read Frange Write Setrange;
  end;
  TMergeCellsRequestClass = Class of TMergeCellsRequest;
  
  { --------------------------------------------------------------------
    TAddSheetResponse
    --------------------------------------------------------------------}
  
  TAddSheetResponse = Class(TGoogleBaseObject)
  Private
    Fproperties : TSheetProperties;
  Protected
    //Property setters
    Procedure Setproperties(AIndex : Integer; const AValue : TSheetProperties); virtual;
  Public
  Published
    Property properties : TSheetProperties Index 0 Read Fproperties Write Setproperties;
  end;
  TAddSheetResponseClass = Class of TAddSheetResponse;
  
  { --------------------------------------------------------------------
    TPivotGroupValueMetadata
    --------------------------------------------------------------------}
  
  TPivotGroupValueMetadata = Class(TGoogleBaseObject)
  Private
    Fvalue : TExtendedValue;
    Fcollapsed : boolean;
  Protected
    //Property setters
    Procedure Setvalue(AIndex : Integer; const AValue : TExtendedValue); virtual;
    Procedure Setcollapsed(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property value : TExtendedValue Index 0 Read Fvalue Write Setvalue;
    Property collapsed : boolean Index 8 Read Fcollapsed Write Setcollapsed;
  end;
  TPivotGroupValueMetadataClass = Class of TPivotGroupValueMetadata;
  
  { --------------------------------------------------------------------
    TUpdateEmbeddedObjectPositionResponse
    --------------------------------------------------------------------}
  
  TUpdateEmbeddedObjectPositionResponse = Class(TGoogleBaseObject)
  Private
    Fposition : TEmbeddedObjectPosition;
  Protected
    //Property setters
    Procedure Setposition(AIndex : Integer; const AValue : TEmbeddedObjectPosition); virtual;
  Public
  Published
    Property position : TEmbeddedObjectPosition Index 0 Read Fposition Write Setposition;
  end;
  TUpdateEmbeddedObjectPositionResponseClass = Class of TUpdateEmbeddedObjectPositionResponse;
  
  { --------------------------------------------------------------------
    TUpdateConditionalFormatRuleRequest
    --------------------------------------------------------------------}
  
  TUpdateConditionalFormatRuleRequest = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
    Frule : TConditionalFormatRule;
    Findex : integer;
    FnewIndex : integer;
  Protected
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setrule(AIndex : Integer; const AValue : TConditionalFormatRule); virtual;
    Procedure Setindex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnewIndex(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
    Property rule : TConditionalFormatRule Index 8 Read Frule Write Setrule;
    Property index : integer Index 16 Read Findex Write Setindex;
    Property newIndex : integer Index 24 Read FnewIndex Write SetnewIndex;
  end;
  TUpdateConditionalFormatRuleRequestClass = Class of TUpdateConditionalFormatRuleRequest;
  
  { --------------------------------------------------------------------
    TTextFormat
    --------------------------------------------------------------------}
  
  TTextFormat = Class(TGoogleBaseObject)
  Private
    Fbold : boolean;
    Fitalic : boolean;
    FforegroundColor : TColor;
    FfontFamily : String;
    Fstrikethrough : boolean;
    FfontSize : integer;
    Funderline : boolean;
  Protected
    //Property setters
    Procedure Setbold(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setitalic(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetforegroundColor(AIndex : Integer; const AValue : TColor); virtual;
    Procedure SetfontFamily(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstrikethrough(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetfontSize(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setunderline(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property bold : boolean Index 0 Read Fbold Write Setbold;
    Property italic : boolean Index 8 Read Fitalic Write Setitalic;
    Property foregroundColor : TColor Index 16 Read FforegroundColor Write SetforegroundColor;
    Property fontFamily : String Index 24 Read FfontFamily Write SetfontFamily;
    Property strikethrough : boolean Index 32 Read Fstrikethrough Write Setstrikethrough;
    Property fontSize : integer Index 40 Read FfontSize Write SetfontSize;
    Property underline : boolean Index 48 Read Funderline Write Setunderline;
  end;
  TTextFormatClass = Class of TTextFormat;
  
  { --------------------------------------------------------------------
    TUpdateChartSpecRequest
    --------------------------------------------------------------------}
  
  TUpdateChartSpecRequest = Class(TGoogleBaseObject)
  Private
    FchartId : integer;
    Fspec : TChartSpec;
  Protected
    //Property setters
    Procedure SetchartId(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setspec(AIndex : Integer; const AValue : TChartSpec); virtual;
  Public
  Published
    Property chartId : integer Index 0 Read FchartId Write SetchartId;
    Property spec : TChartSpec Index 8 Read Fspec Write Setspec;
  end;
  TUpdateChartSpecRequestClass = Class of TUpdateChartSpecRequest;
  
  { --------------------------------------------------------------------
    TGridCoordinate
    --------------------------------------------------------------------}
  
  TGridCoordinate = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
    FrowIndex : integer;
    FcolumnIndex : integer;
  Protected
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetrowIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetcolumnIndex(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
    Property rowIndex : integer Index 8 Read FrowIndex Write SetrowIndex;
    Property columnIndex : integer Index 16 Read FcolumnIndex Write SetcolumnIndex;
  end;
  TGridCoordinateClass = Class of TGridCoordinate;
  
  { --------------------------------------------------------------------
    TDeleteFilterViewRequest
    --------------------------------------------------------------------}
  
  TDeleteFilterViewRequest = Class(TGoogleBaseObject)
  Private
    FfilterId : integer;
  Protected
    //Property setters
    Procedure SetfilterId(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property filterId : integer Index 0 Read FfilterId Write SetfilterId;
  end;
  TDeleteFilterViewRequestClass = Class of TDeleteFilterViewRequest;
  
  { --------------------------------------------------------------------
    TBatchUpdateValuesResponse
    --------------------------------------------------------------------}
  
  TBatchUpdateValuesResponse = Class(TGoogleBaseObject)
  Private
    FtotalUpdatedSheets : integer;
    FtotalUpdatedColumns : integer;
    Fresponses : TBatchUpdateValuesResponseTyperesponsesArray;
    FtotalUpdatedCells : integer;
    FspreadsheetId : String;
    FtotalUpdatedRows : integer;
  Protected
    //Property setters
    Procedure SettotalUpdatedSheets(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettotalUpdatedColumns(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setresponses(AIndex : Integer; const AValue : TBatchUpdateValuesResponseTyperesponsesArray); virtual;
    Procedure SettotalUpdatedCells(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetspreadsheetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalUpdatedRows(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property totalUpdatedSheets : integer Index 0 Read FtotalUpdatedSheets Write SettotalUpdatedSheets;
    Property totalUpdatedColumns : integer Index 8 Read FtotalUpdatedColumns Write SettotalUpdatedColumns;
    Property responses : TBatchUpdateValuesResponseTyperesponsesArray Index 16 Read Fresponses Write Setresponses;
    Property totalUpdatedCells : integer Index 24 Read FtotalUpdatedCells Write SettotalUpdatedCells;
    Property spreadsheetId : String Index 32 Read FspreadsheetId Write SetspreadsheetId;
    Property totalUpdatedRows : integer Index 40 Read FtotalUpdatedRows Write SettotalUpdatedRows;
  end;
  TBatchUpdateValuesResponseClass = Class of TBatchUpdateValuesResponse;
  
  { --------------------------------------------------------------------
    TUpdateNamedRangeRequest
    --------------------------------------------------------------------}
  
  TUpdateNamedRangeRequest = Class(TGoogleBaseObject)
  Private
    Ffields : String;
    FnamedRange : TNamedRange;
  Protected
    //Property setters
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnamedRange(AIndex : Integer; const AValue : TNamedRange); virtual;
  Public
  Published
    Property fields : String Index 0 Read Ffields Write Setfields;
    Property namedRange : TNamedRange Index 8 Read FnamedRange Write SetnamedRange;
  end;
  TUpdateNamedRangeRequestClass = Class of TUpdateNamedRangeRequest;
  
  { --------------------------------------------------------------------
    TUpdateValuesResponse
    --------------------------------------------------------------------}
  
  TUpdateValuesResponse = Class(TGoogleBaseObject)
  Private
    FupdatedRange : String;
    FupdatedColumns : integer;
    FspreadsheetId : String;
    FupdatedRows : integer;
    FupdatedCells : integer;
  Protected
    //Property setters
    Procedure SetupdatedRange(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdatedColumns(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetspreadsheetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdatedRows(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetupdatedCells(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property updatedRange : String Index 0 Read FupdatedRange Write SetupdatedRange;
    Property updatedColumns : integer Index 8 Read FupdatedColumns Write SetupdatedColumns;
    Property spreadsheetId : String Index 16 Read FspreadsheetId Write SetspreadsheetId;
    Property updatedRows : integer Index 24 Read FupdatedRows Write SetupdatedRows;
    Property updatedCells : integer Index 32 Read FupdatedCells Write SetupdatedCells;
  end;
  TUpdateValuesResponseClass = Class of TUpdateValuesResponse;
  
  { --------------------------------------------------------------------
    TSpreadsheetProperties
    --------------------------------------------------------------------}
  
  TSpreadsheetProperties = Class(TGoogleBaseObject)
  Private
    FtimeZone : String;
    FautoRecalc : String;
    Flocale : String;
    Ftitle : String;
    FdefaultFormat : TCellFormat;
  Protected
    //Property setters
    Procedure SettimeZone(AIndex : Integer; const AValue : String); virtual;
    Procedure SetautoRecalc(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocale(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdefaultFormat(AIndex : Integer; const AValue : TCellFormat); virtual;
  Public
  Published
    Property timeZone : String Index 0 Read FtimeZone Write SettimeZone;
    Property autoRecalc : String Index 8 Read FautoRecalc Write SetautoRecalc;
    Property locale : String Index 16 Read Flocale Write Setlocale;
    Property title : String Index 24 Read Ftitle Write Settitle;
    Property defaultFormat : TCellFormat Index 32 Read FdefaultFormat Write SetdefaultFormat;
  end;
  TSpreadsheetPropertiesClass = Class of TSpreadsheetProperties;
  
  { --------------------------------------------------------------------
    TCellData
    --------------------------------------------------------------------}
  
  TCellData = Class(TGoogleBaseObject)
  Private
    Fhyperlink : String;
    FeffectiveFormat : TCellFormat;
    Fnote : String;
    FformattedValue : String;
    FuserEnteredValue : TExtendedValue;
    FdataValidation : TDataValidationRule;
    FuserEnteredFormat : TCellFormat;
    FpivotTable : TPivotTable;
    FtextFormatRuns : TCellDataTypetextFormatRunsArray;
    FeffectiveValue : TExtendedValue;
  Protected
    //Property setters
    Procedure Sethyperlink(AIndex : Integer; const AValue : String); virtual;
    Procedure SeteffectiveFormat(AIndex : Integer; const AValue : TCellFormat); virtual;
    Procedure Setnote(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserEnteredValue(AIndex : Integer; const AValue : TExtendedValue); virtual;
    Procedure SetdataValidation(AIndex : Integer; const AValue : TDataValidationRule); virtual;
    Procedure SetuserEnteredFormat(AIndex : Integer; const AValue : TCellFormat); virtual;
    Procedure SetpivotTable(AIndex : Integer; const AValue : TPivotTable); virtual;
    Procedure SettextFormatRuns(AIndex : Integer; const AValue : TCellDataTypetextFormatRunsArray); virtual;
    Procedure SeteffectiveValue(AIndex : Integer; const AValue : TExtendedValue); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property hyperlink : String Index 0 Read Fhyperlink Write Sethyperlink;
    Property effectiveFormat : TCellFormat Index 8 Read FeffectiveFormat Write SeteffectiveFormat;
    Property note : String Index 16 Read Fnote Write Setnote;
    Property formattedValue : String Index 24 Read FformattedValue Write SetformattedValue;
    Property userEnteredValue : TExtendedValue Index 32 Read FuserEnteredValue Write SetuserEnteredValue;
    Property dataValidation : TDataValidationRule Index 40 Read FdataValidation Write SetdataValidation;
    Property userEnteredFormat : TCellFormat Index 48 Read FuserEnteredFormat Write SetuserEnteredFormat;
    Property pivotTable : TPivotTable Index 56 Read FpivotTable Write SetpivotTable;
    Property textFormatRuns : TCellDataTypetextFormatRunsArray Index 64 Read FtextFormatRuns Write SettextFormatRuns;
    Property effectiveValue : TExtendedValue Index 72 Read FeffectiveValue Write SeteffectiveValue;
  end;
  TCellDataClass = Class of TCellData;
  
  { --------------------------------------------------------------------
    TUnmergeCellsRequest
    --------------------------------------------------------------------}
  
  TUnmergeCellsRequest = Class(TGoogleBaseObject)
  Private
    Frange : TGridRange;
  Protected
    //Property setters
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
  Public
  Published
    Property range : TGridRange Index 0 Read Frange Write Setrange;
  end;
  TUnmergeCellsRequestClass = Class of TUnmergeCellsRequest;
  
  { --------------------------------------------------------------------
    TTextToColumnsRequest
    --------------------------------------------------------------------}
  
  TTextToColumnsRequest = Class(TGoogleBaseObject)
  Private
    Fsource : TGridRange;
    Fdelimiter : String;
    FdelimiterType : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure Setdelimiter(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdelimiterType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TGridRange Index 0 Read Fsource Write Setsource;
    Property delimiter : String Index 8 Read Fdelimiter Write Setdelimiter;
    Property delimiterType : String Index 16 Read FdelimiterType Write SetdelimiterType;
  end;
  TTextToColumnsRequestClass = Class of TTextToColumnsRequest;
  
  { --------------------------------------------------------------------
    TAddProtectedRangeResponse
    --------------------------------------------------------------------}
  
  TAddProtectedRangeResponse = Class(TGoogleBaseObject)
  Private
    FprotectedRange : TProtectedRange;
  Protected
    //Property setters
    Procedure SetprotectedRange(AIndex : Integer; const AValue : TProtectedRange); virtual;
  Public
  Published
    Property protectedRange : TProtectedRange Index 0 Read FprotectedRange Write SetprotectedRange;
  end;
  TAddProtectedRangeResponseClass = Class of TAddProtectedRangeResponse;
  
  { --------------------------------------------------------------------
    TBooleanCondition
    --------------------------------------------------------------------}
  
  TBooleanCondition = Class(TGoogleBaseObject)
  Private
    Fvalues : TBooleanConditionTypevaluesArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setvalues(AIndex : Integer; const AValue : TBooleanConditionTypevaluesArray); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property values : TBooleanConditionTypevaluesArray Index 0 Read Fvalues Write Setvalues;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TBooleanConditionClass = Class of TBooleanCondition;
  
  { --------------------------------------------------------------------
    TDeleteProtectedRangeRequest
    --------------------------------------------------------------------}
  
  TDeleteProtectedRangeRequest = Class(TGoogleBaseObject)
  Private
    FprotectedRangeId : integer;
  Protected
    //Property setters
    Procedure SetprotectedRangeId(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property protectedRangeId : integer Index 0 Read FprotectedRangeId Write SetprotectedRangeId;
  end;
  TDeleteProtectedRangeRequestClass = Class of TDeleteProtectedRangeRequest;
  
  { --------------------------------------------------------------------
    TBasicChartDomain
    --------------------------------------------------------------------}
  
  TBasicChartDomain = Class(TGoogleBaseObject)
  Private
    Fdomain : TChartData;
  Protected
    //Property setters
    Procedure Setdomain(AIndex : Integer; const AValue : TChartData); virtual;
  Public
  Published
    Property domain : TChartData Index 0 Read Fdomain Write Setdomain;
  end;
  TBasicChartDomainClass = Class of TBasicChartDomain;
  
  { --------------------------------------------------------------------
    TDimensionRange
    --------------------------------------------------------------------}
  
  TDimensionRange = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
    FendIndex : integer;
    FstartIndex : integer;
    Fdimension : String;
  Protected
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetendIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetstartIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setdimension(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
    Property endIndex : integer Index 8 Read FendIndex Write SetendIndex;
    Property startIndex : integer Index 16 Read FstartIndex Write SetstartIndex;
    Property dimension : String Index 24 Read Fdimension Write Setdimension;
  end;
  TDimensionRangeClass = Class of TDimensionRange;
  
  { --------------------------------------------------------------------
    TResponse
    --------------------------------------------------------------------}
  
  TResponse = Class(TGoogleBaseObject)
  Private
    FupdateEmbeddedObjectPosition : TUpdateEmbeddedObjectPositionResponse;
    FaddFilterView : TAddFilterViewResponse;
    FaddSheet : TAddSheetResponse;
    FfindReplace : TFindReplaceResponse;
    FaddProtectedRange : TAddProtectedRangeResponse;
    FupdateConditionalFormatRule : TUpdateConditionalFormatRuleResponse;
    FaddChart : TAddChartResponse;
    FdeleteConditionalFormatRule : TDeleteConditionalFormatRuleResponse;
    FduplicateSheet : TDuplicateSheetResponse;
    FduplicateFilterView : TDuplicateFilterViewResponse;
    FaddNamedRange : TAddNamedRangeResponse;
  Protected
    //Property setters
    Procedure SetupdateEmbeddedObjectPosition(AIndex : Integer; const AValue : TUpdateEmbeddedObjectPositionResponse); virtual;
    Procedure SetaddFilterView(AIndex : Integer; const AValue : TAddFilterViewResponse); virtual;
    Procedure SetaddSheet(AIndex : Integer; const AValue : TAddSheetResponse); virtual;
    Procedure SetfindReplace(AIndex : Integer; const AValue : TFindReplaceResponse); virtual;
    Procedure SetaddProtectedRange(AIndex : Integer; const AValue : TAddProtectedRangeResponse); virtual;
    Procedure SetupdateConditionalFormatRule(AIndex : Integer; const AValue : TUpdateConditionalFormatRuleResponse); virtual;
    Procedure SetaddChart(AIndex : Integer; const AValue : TAddChartResponse); virtual;
    Procedure SetdeleteConditionalFormatRule(AIndex : Integer; const AValue : TDeleteConditionalFormatRuleResponse); virtual;
    Procedure SetduplicateSheet(AIndex : Integer; const AValue : TDuplicateSheetResponse); virtual;
    Procedure SetduplicateFilterView(AIndex : Integer; const AValue : TDuplicateFilterViewResponse); virtual;
    Procedure SetaddNamedRange(AIndex : Integer; const AValue : TAddNamedRangeResponse); virtual;
  Public
  Published
    Property updateEmbeddedObjectPosition : TUpdateEmbeddedObjectPositionResponse Index 0 Read FupdateEmbeddedObjectPosition Write SetupdateEmbeddedObjectPosition;
    Property addFilterView : TAddFilterViewResponse Index 8 Read FaddFilterView Write SetaddFilterView;
    Property addSheet : TAddSheetResponse Index 16 Read FaddSheet Write SetaddSheet;
    Property findReplace : TFindReplaceResponse Index 24 Read FfindReplace Write SetfindReplace;
    Property addProtectedRange : TAddProtectedRangeResponse Index 32 Read FaddProtectedRange Write SetaddProtectedRange;
    Property updateConditionalFormatRule : TUpdateConditionalFormatRuleResponse Index 40 Read FupdateConditionalFormatRule Write SetupdateConditionalFormatRule;
    Property addChart : TAddChartResponse Index 48 Read FaddChart Write SetaddChart;
    Property deleteConditionalFormatRule : TDeleteConditionalFormatRuleResponse Index 56 Read FdeleteConditionalFormatRule Write SetdeleteConditionalFormatRule;
    Property duplicateSheet : TDuplicateSheetResponse Index 64 Read FduplicateSheet Write SetduplicateSheet;
    Property duplicateFilterView : TDuplicateFilterViewResponse Index 72 Read FduplicateFilterView Write SetduplicateFilterView;
    Property addNamedRange : TAddNamedRangeResponse Index 80 Read FaddNamedRange Write SetaddNamedRange;
  end;
  TResponseClass = Class of TResponse;
  
  { --------------------------------------------------------------------
    TAddConditionalFormatRuleRequest
    --------------------------------------------------------------------}
  
  TAddConditionalFormatRuleRequest = Class(TGoogleBaseObject)
  Private
    Frule : TConditionalFormatRule;
    Findex : integer;
  Protected
    //Property setters
    Procedure Setrule(AIndex : Integer; const AValue : TConditionalFormatRule); virtual;
    Procedure Setindex(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property rule : TConditionalFormatRule Index 0 Read Frule Write Setrule;
    Property index : integer Index 8 Read Findex Write Setindex;
  end;
  TAddConditionalFormatRuleRequestClass = Class of TAddConditionalFormatRuleRequest;
  
  { --------------------------------------------------------------------
    TFilterViewTypecriteria
    --------------------------------------------------------------------}
  
  TFilterViewTypecriteria = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TFilterViewTypecriteriaClass = Class of TFilterViewTypecriteria;
  
  { --------------------------------------------------------------------
    TFilterView
    --------------------------------------------------------------------}
  
  TFilterView = Class(TGoogleBaseObject)
  Private
    Ftitle : String;
    FnamedRangeId : String;
    FsortSpecs : TFilterViewTypesortSpecsArray;
    Frange : TGridRange;
    Fcriteria : TFilterViewTypecriteria;
    FfilterViewId : integer;
  Protected
    //Property setters
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnamedRangeId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsortSpecs(AIndex : Integer; const AValue : TFilterViewTypesortSpecsArray); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure Setcriteria(AIndex : Integer; const AValue : TFilterViewTypecriteria); virtual;
    Procedure SetfilterViewId(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property title : String Index 0 Read Ftitle Write Settitle;
    Property namedRangeId : String Index 8 Read FnamedRangeId Write SetnamedRangeId;
    Property sortSpecs : TFilterViewTypesortSpecsArray Index 16 Read FsortSpecs Write SetsortSpecs;
    Property range : TGridRange Index 24 Read Frange Write Setrange;
    Property criteria : TFilterViewTypecriteria Index 32 Read Fcriteria Write Setcriteria;
    Property filterViewId : integer Index 40 Read FfilterViewId Write SetfilterViewId;
  end;
  TFilterViewClass = Class of TFilterView;
  
  { --------------------------------------------------------------------
    TSortRangeRequest
    --------------------------------------------------------------------}
  
  TSortRangeRequest = Class(TGoogleBaseObject)
  Private
    Frange : TGridRange;
    FsortSpecs : TSortRangeRequestTypesortSpecsArray;
  Protected
    //Property setters
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure SetsortSpecs(AIndex : Integer; const AValue : TSortRangeRequestTypesortSpecsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property range : TGridRange Index 0 Read Frange Write Setrange;
    Property sortSpecs : TSortRangeRequestTypesortSpecsArray Index 8 Read FsortSpecs Write SetsortSpecs;
  end;
  TSortRangeRequestClass = Class of TSortRangeRequest;
  
  { --------------------------------------------------------------------
    TTextFormatRun
    --------------------------------------------------------------------}
  
  TTextFormatRun = Class(TGoogleBaseObject)
  Private
    FstartIndex : integer;
    Fformat : TTextFormat;
  Protected
    //Property setters
    Procedure SetstartIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setformat(AIndex : Integer; const AValue : TTextFormat); virtual;
  Public
  Published
    Property startIndex : integer Index 0 Read FstartIndex Write SetstartIndex;
    Property format : TTextFormat Index 8 Read Fformat Write Setformat;
  end;
  TTextFormatRunClass = Class of TTextFormatRun;
  
  { --------------------------------------------------------------------
    TUpdateFilterViewRequest
    --------------------------------------------------------------------}
  
  TUpdateFilterViewRequest = Class(TGoogleBaseObject)
  Private
    Ffilter : TFilterView;
    Ffields : String;
  Protected
    //Property setters
    Procedure Setfilter(AIndex : Integer; const AValue : TFilterView); virtual;
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property filter : TFilterView Index 0 Read Ffilter Write Setfilter;
    Property fields : String Index 8 Read Ffields Write Setfields;
  end;
  TUpdateFilterViewRequestClass = Class of TUpdateFilterViewRequest;
  
  { --------------------------------------------------------------------
    TUpdateConditionalFormatRuleResponse
    --------------------------------------------------------------------}
  
  TUpdateConditionalFormatRuleResponse = Class(TGoogleBaseObject)
  Private
    FoldIndex : integer;
    FnewRule : TConditionalFormatRule;
    FoldRule : TConditionalFormatRule;
    FnewIndex : integer;
  Protected
    //Property setters
    Procedure SetoldIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnewRule(AIndex : Integer; const AValue : TConditionalFormatRule); virtual;
    Procedure SetoldRule(AIndex : Integer; const AValue : TConditionalFormatRule); virtual;
    Procedure SetnewIndex(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property oldIndex : integer Index 0 Read FoldIndex Write SetoldIndex;
    Property newRule : TConditionalFormatRule Index 8 Read FnewRule Write SetnewRule;
    Property oldRule : TConditionalFormatRule Index 16 Read FoldRule Write SetoldRule;
    Property newIndex : integer Index 24 Read FnewIndex Write SetnewIndex;
  end;
  TUpdateConditionalFormatRuleResponseClass = Class of TUpdateConditionalFormatRuleResponse;
  
  { --------------------------------------------------------------------
    TFilterCriteria
    --------------------------------------------------------------------}
  
  TFilterCriteria = Class(TGoogleBaseObject)
  Private
    Fcondition : TBooleanCondition;
    FhiddenValues : TStringArray;
  Protected
    //Property setters
    Procedure Setcondition(AIndex : Integer; const AValue : TBooleanCondition); virtual;
    Procedure SethiddenValues(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property condition : TBooleanCondition Index 0 Read Fcondition Write Setcondition;
    Property hiddenValues : TStringArray Index 8 Read FhiddenValues Write SethiddenValues;
  end;
  TFilterCriteriaClass = Class of TFilterCriteria;
  
  { --------------------------------------------------------------------
    TDeleteDimensionRequest
    --------------------------------------------------------------------}
  
  TDeleteDimensionRequest = Class(TGoogleBaseObject)
  Private
    Frange : TDimensionRange;
  Protected
    //Property setters
    Procedure Setrange(AIndex : Integer; const AValue : TDimensionRange); virtual;
  Public
  Published
    Property range : TDimensionRange Index 0 Read Frange Write Setrange;
  end;
  TDeleteDimensionRequestClass = Class of TDeleteDimensionRequest;
  
  { --------------------------------------------------------------------
    TPivotTableTypecriteria
    --------------------------------------------------------------------}
  
  TPivotTableTypecriteria = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPivotTableTypecriteriaClass = Class of TPivotTableTypecriteria;
  
  { --------------------------------------------------------------------
    TPivotTable
    --------------------------------------------------------------------}
  
  TPivotTable = Class(TGoogleBaseObject)
  Private
    FvalueLayout : String;
    Fcolumns : TPivotTableTypecolumnsArray;
    Fsource : TGridRange;
    Frows : TPivotTableTyperowsArray;
    Fvalues : TPivotTableTypevaluesArray;
    Fcriteria : TPivotTableTypecriteria;
  Protected
    //Property setters
    Procedure SetvalueLayout(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcolumns(AIndex : Integer; const AValue : TPivotTableTypecolumnsArray); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure Setrows(AIndex : Integer; const AValue : TPivotTableTyperowsArray); virtual;
    Procedure Setvalues(AIndex : Integer; const AValue : TPivotTableTypevaluesArray); virtual;
    Procedure Setcriteria(AIndex : Integer; const AValue : TPivotTableTypecriteria); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property valueLayout : String Index 0 Read FvalueLayout Write SetvalueLayout;
    Property columns : TPivotTableTypecolumnsArray Index 8 Read Fcolumns Write Setcolumns;
    Property source : TGridRange Index 16 Read Fsource Write Setsource;
    Property rows : TPivotTableTyperowsArray Index 24 Read Frows Write Setrows;
    Property values : TPivotTableTypevaluesArray Index 32 Read Fvalues Write Setvalues;
    Property criteria : TPivotTableTypecriteria Index 40 Read Fcriteria Write Setcriteria;
  end;
  TPivotTableClass = Class of TPivotTable;
  
  { --------------------------------------------------------------------
    TDataValidationRule
    --------------------------------------------------------------------}
  
  TDataValidationRule = Class(TGoogleBaseObject)
  Private
    Fcondition : TBooleanCondition;
    FinputMessage : String;
    FshowCustomUi : boolean;
    Fstrict : boolean;
  Protected
    //Property setters
    Procedure Setcondition(AIndex : Integer; const AValue : TBooleanCondition); virtual;
    Procedure SetinputMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetshowCustomUi(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setstrict(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property condition : TBooleanCondition Index 0 Read Fcondition Write Setcondition;
    Property inputMessage : String Index 8 Read FinputMessage Write SetinputMessage;
    Property showCustomUi : boolean Index 16 Read FshowCustomUi Write SetshowCustomUi;
    Property strict : boolean Index 24 Read Fstrict Write Setstrict;
  end;
  TDataValidationRuleClass = Class of TDataValidationRule;
  
  { --------------------------------------------------------------------
    TUpdateSpreadsheetPropertiesRequest
    --------------------------------------------------------------------}
  
  TUpdateSpreadsheetPropertiesRequest = Class(TGoogleBaseObject)
  Private
    Ffields : String;
    Fproperties : TSpreadsheetProperties;
  Protected
    //Property setters
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TSpreadsheetProperties); virtual;
  Public
  Published
    Property fields : String Index 0 Read Ffields Write Setfields;
    Property properties : TSpreadsheetProperties Index 8 Read Fproperties Write Setproperties;
  end;
  TUpdateSpreadsheetPropertiesRequestClass = Class of TUpdateSpreadsheetPropertiesRequest;
  
  { --------------------------------------------------------------------
    TChartSourceRange
    --------------------------------------------------------------------}
  
  TChartSourceRange = Class(TGoogleBaseObject)
  Private
    Fsources : TChartSourceRangeTypesourcesArray;
  Protected
    //Property setters
    Procedure Setsources(AIndex : Integer; const AValue : TChartSourceRangeTypesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property sources : TChartSourceRangeTypesourcesArray Index 0 Read Fsources Write Setsources;
  end;
  TChartSourceRangeClass = Class of TChartSourceRange;
  
  { --------------------------------------------------------------------
    TBatchUpdateValuesRequest
    --------------------------------------------------------------------}
  
  TBatchUpdateValuesRequest = Class(TGoogleBaseObject)
  Private
    FvalueInputOption : String;
    Fdata : TBatchUpdateValuesRequestTypedataArray;
  Protected
    //Property setters
    Procedure SetvalueInputOption(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : TBatchUpdateValuesRequestTypedataArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property valueInputOption : String Index 0 Read FvalueInputOption Write SetvalueInputOption;
    Property data : TBatchUpdateValuesRequestTypedataArray Index 8 Read Fdata Write Setdata;
  end;
  TBatchUpdateValuesRequestClass = Class of TBatchUpdateValuesRequest;
  
  { --------------------------------------------------------------------
    TClearBasicFilterRequest
    --------------------------------------------------------------------}
  
  TClearBasicFilterRequest = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
  Protected
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
  end;
  TClearBasicFilterRequestClass = Class of TClearBasicFilterRequest;
  
  { --------------------------------------------------------------------
    TConditionalFormatRule
    --------------------------------------------------------------------}
  
  TConditionalFormatRule = Class(TGoogleBaseObject)
  Private
    FgradientRule : TGradientRule;
    FbooleanRule : TBooleanRule;
    Franges : TConditionalFormatRuleTyperangesArray;
  Protected
    //Property setters
    Procedure SetgradientRule(AIndex : Integer; const AValue : TGradientRule); virtual;
    Procedure SetbooleanRule(AIndex : Integer; const AValue : TBooleanRule); virtual;
    Procedure Setranges(AIndex : Integer; const AValue : TConditionalFormatRuleTyperangesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property gradientRule : TGradientRule Index 0 Read FgradientRule Write SetgradientRule;
    Property booleanRule : TBooleanRule Index 8 Read FbooleanRule Write SetbooleanRule;
    Property ranges : TConditionalFormatRuleTyperangesArray Index 16 Read Franges Write Setranges;
  end;
  TConditionalFormatRuleClass = Class of TConditionalFormatRule;
  
  { --------------------------------------------------------------------
    TUpdateBordersRequest
    --------------------------------------------------------------------}
  
  TUpdateBordersRequest = Class(TGoogleBaseObject)
  Private
    Fright : TBorder;
    FinnerVertical : TBorder;
    Ftop : TBorder;
    FinnerHorizontal : TBorder;
    Frange : TGridRange;
    Fbottom : TBorder;
    Fleft : TBorder;
  Protected
    //Property setters
    Procedure Setright(AIndex : Integer; const AValue : TBorder); virtual;
    Procedure SetinnerVertical(AIndex : Integer; const AValue : TBorder); virtual;
    Procedure Settop(AIndex : Integer; const AValue : TBorder); virtual;
    Procedure SetinnerHorizontal(AIndex : Integer; const AValue : TBorder); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure Setbottom(AIndex : Integer; const AValue : TBorder); virtual;
    Procedure Setleft(AIndex : Integer; const AValue : TBorder); virtual;
  Public
  Published
    Property right : TBorder Index 0 Read Fright Write Setright;
    Property innerVertical : TBorder Index 8 Read FinnerVertical Write SetinnerVertical;
    Property top : TBorder Index 16 Read Ftop Write Settop;
    Property innerHorizontal : TBorder Index 24 Read FinnerHorizontal Write SetinnerHorizontal;
    Property range : TGridRange Index 32 Read Frange Write Setrange;
    Property bottom : TBorder Index 40 Read Fbottom Write Setbottom;
    Property left : TBorder Index 48 Read Fleft Write Setleft;
  end;
  TUpdateBordersRequestClass = Class of TUpdateBordersRequest;
  
  { --------------------------------------------------------------------
    TPivotFilterCriteria
    --------------------------------------------------------------------}
  
  TPivotFilterCriteria = Class(TGoogleBaseObject)
  Private
    FvisibleValues : TStringArray;
  Protected
    //Property setters
    Procedure SetvisibleValues(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property visibleValues : TStringArray Index 0 Read FvisibleValues Write SetvisibleValues;
  end;
  TPivotFilterCriteriaClass = Class of TPivotFilterCriteria;
  
  { --------------------------------------------------------------------
    TBorders
    --------------------------------------------------------------------}
  
  TBorders = Class(TGoogleBaseObject)
  Private
    Fleft : TBorder;
    Fright : TBorder;
    Ftop : TBorder;
    Fbottom : TBorder;
  Protected
    //Property setters
    Procedure Setleft(AIndex : Integer; const AValue : TBorder); virtual;
    Procedure Setright(AIndex : Integer; const AValue : TBorder); virtual;
    Procedure Settop(AIndex : Integer; const AValue : TBorder); virtual;
    Procedure Setbottom(AIndex : Integer; const AValue : TBorder); virtual;
  Public
  Published
    Property left : TBorder Index 0 Read Fleft Write Setleft;
    Property right : TBorder Index 8 Read Fright Write Setright;
    Property top : TBorder Index 16 Read Ftop Write Settop;
    Property bottom : TBorder Index 24 Read Fbottom Write Setbottom;
  end;
  TBordersClass = Class of TBorders;
  
  { --------------------------------------------------------------------
    TEmbeddedChart
    --------------------------------------------------------------------}
  
  TEmbeddedChart = Class(TGoogleBaseObject)
  Private
    FchartId : integer;
    Fposition : TEmbeddedObjectPosition;
    Fspec : TChartSpec;
  Protected
    //Property setters
    Procedure SetchartId(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setposition(AIndex : Integer; const AValue : TEmbeddedObjectPosition); virtual;
    Procedure Setspec(AIndex : Integer; const AValue : TChartSpec); virtual;
  Public
  Published
    Property chartId : integer Index 0 Read FchartId Write SetchartId;
    Property position : TEmbeddedObjectPosition Index 8 Read Fposition Write Setposition;
    Property spec : TChartSpec Index 16 Read Fspec Write Setspec;
  end;
  TEmbeddedChartClass = Class of TEmbeddedChart;
  
  { --------------------------------------------------------------------
    TColor
    --------------------------------------------------------------------}
  
  TColor = Class(TGoogleBaseObject)
  Private
    Fgreen : integer;
    Fblue : integer;
    Fred : integer;
    Falpha : integer;
  Protected
    //Property setters
    Procedure Setgreen(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setblue(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setred(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setalpha(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property green : integer Index 0 Read Fgreen Write Setgreen;
    Property blue : integer Index 8 Read Fblue Write Setblue;
    Property red : integer Index 16 Read Fred Write Setred;
    Property alpha : integer Index 24 Read Falpha Write Setalpha;
  end;
  TColorClass = Class of TColor;
  
  { --------------------------------------------------------------------
    TAddSheetRequest
    --------------------------------------------------------------------}
  
  TAddSheetRequest = Class(TGoogleBaseObject)
  Private
    Fproperties : TSheetProperties;
  Protected
    //Property setters
    Procedure Setproperties(AIndex : Integer; const AValue : TSheetProperties); virtual;
  Public
  Published
    Property properties : TSheetProperties Index 0 Read Fproperties Write Setproperties;
  end;
  TAddSheetRequestClass = Class of TAddSheetRequest;
  
  { --------------------------------------------------------------------
    TAddProtectedRangeRequest
    --------------------------------------------------------------------}
  
  TAddProtectedRangeRequest = Class(TGoogleBaseObject)
  Private
    FprotectedRange : TProtectedRange;
  Protected
    //Property setters
    Procedure SetprotectedRange(AIndex : Integer; const AValue : TProtectedRange); virtual;
  Public
  Published
    Property protectedRange : TProtectedRange Index 0 Read FprotectedRange Write SetprotectedRange;
  end;
  TAddProtectedRangeRequestClass = Class of TAddProtectedRangeRequest;
  
  { --------------------------------------------------------------------
    TValueRange
    --------------------------------------------------------------------}
  
  TValueRange = Class(TGoogleBaseObject)
  Private
    Fvalues : TValueRangeTypevaluesArray;
    Frange : String;
    FmajorDimension : String;
  Protected
    //Property setters
    Procedure Setvalues(AIndex : Integer; const AValue : TValueRangeTypevaluesArray); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmajorDimension(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property values : TValueRangeTypevaluesArray Index 0 Read Fvalues Write Setvalues;
    Property range : String Index 8 Read Frange Write Setrange;
    Property majorDimension : String Index 16 Read FmajorDimension Write SetmajorDimension;
  end;
  TValueRangeClass = Class of TValueRange;
  
  { --------------------------------------------------------------------
    TFindReplaceResponse
    --------------------------------------------------------------------}
  
  TFindReplaceResponse = Class(TGoogleBaseObject)
  Private
    FvaluesChanged : integer;
    FrowsChanged : integer;
    FoccurrencesChanged : integer;
    FformulasChanged : integer;
    FsheetsChanged : integer;
  Protected
    //Property setters
    Procedure SetvaluesChanged(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetrowsChanged(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetoccurrencesChanged(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetformulasChanged(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetsheetsChanged(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property valuesChanged : integer Index 0 Read FvaluesChanged Write SetvaluesChanged;
    Property rowsChanged : integer Index 8 Read FrowsChanged Write SetrowsChanged;
    Property occurrencesChanged : integer Index 16 Read FoccurrencesChanged Write SetoccurrencesChanged;
    Property formulasChanged : integer Index 24 Read FformulasChanged Write SetformulasChanged;
    Property sheetsChanged : integer Index 32 Read FsheetsChanged Write SetsheetsChanged;
  end;
  TFindReplaceResponseClass = Class of TFindReplaceResponse;
  
  { --------------------------------------------------------------------
    TCellFormat
    --------------------------------------------------------------------}
  
  TCellFormat = Class(TGoogleBaseObject)
  Private
    FhorizontalAlignment : String;
    FhyperlinkDisplayType : String;
    Fborders : TBorders;
    FtextDirection : String;
    FtextFormat : TTextFormat;
    Fpadding : TPadding;
    FnumberFormat : TNumberFormat;
    FwrapStrategy : String;
    FbackgroundColor : TColor;
    FverticalAlignment : String;
  Protected
    //Property setters
    Procedure SethorizontalAlignment(AIndex : Integer; const AValue : String); virtual;
    Procedure SethyperlinkDisplayType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setborders(AIndex : Integer; const AValue : TBorders); virtual;
    Procedure SettextDirection(AIndex : Integer; const AValue : String); virtual;
    Procedure SettextFormat(AIndex : Integer; const AValue : TTextFormat); virtual;
    Procedure Setpadding(AIndex : Integer; const AValue : TPadding); virtual;
    Procedure SetnumberFormat(AIndex : Integer; const AValue : TNumberFormat); virtual;
    Procedure SetwrapStrategy(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbackgroundColor(AIndex : Integer; const AValue : TColor); virtual;
    Procedure SetverticalAlignment(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property horizontalAlignment : String Index 0 Read FhorizontalAlignment Write SethorizontalAlignment;
    Property hyperlinkDisplayType : String Index 8 Read FhyperlinkDisplayType Write SethyperlinkDisplayType;
    Property borders : TBorders Index 16 Read Fborders Write Setborders;
    Property textDirection : String Index 24 Read FtextDirection Write SettextDirection;
    Property textFormat : TTextFormat Index 32 Read FtextFormat Write SettextFormat;
    Property padding : TPadding Index 40 Read Fpadding Write Setpadding;
    Property numberFormat : TNumberFormat Index 48 Read FnumberFormat Write SetnumberFormat;
    Property wrapStrategy : String Index 56 Read FwrapStrategy Write SetwrapStrategy;
    Property backgroundColor : TColor Index 64 Read FbackgroundColor Write SetbackgroundColor;
    Property verticalAlignment : String Index 72 Read FverticalAlignment Write SetverticalAlignment;
  end;
  TCellFormatClass = Class of TCellFormat;
  
  { --------------------------------------------------------------------
    TMoveDimensionRequest
    --------------------------------------------------------------------}
  
  TMoveDimensionRequest = Class(TGoogleBaseObject)
  Private
    Fsource : TDimensionRange;
    FdestinationIndex : integer;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TDimensionRange); virtual;
    Procedure SetdestinationIndex(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property source : TDimensionRange Index 0 Read Fsource Write Setsource;
    Property destinationIndex : integer Index 8 Read FdestinationIndex Write SetdestinationIndex;
  end;
  TMoveDimensionRequestClass = Class of TMoveDimensionRequest;
  
  { --------------------------------------------------------------------
    TBasicChartAxis
    --------------------------------------------------------------------}
  
  TBasicChartAxis = Class(TGoogleBaseObject)
  Private
    Fposition : String;
    Ftitle : String;
    Fformat : TTextFormat;
  Protected
    //Property setters
    Procedure Setposition(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; const AValue : TTextFormat); virtual;
  Public
  Published
    Property position : String Index 0 Read Fposition Write Setposition;
    Property title : String Index 8 Read Ftitle Write Settitle;
    Property format : TTextFormat Index 16 Read Fformat Write Setformat;
  end;
  TBasicChartAxisClass = Class of TBasicChartAxis;
  
  { --------------------------------------------------------------------
    TPivotGroupSortValueBucket
    --------------------------------------------------------------------}
  
  TPivotGroupSortValueBucket = Class(TGoogleBaseObject)
  Private
    Fbuckets : TPivotGroupSortValueBucketTypebucketsArray;
    FvaluesIndex : integer;
  Protected
    //Property setters
    Procedure Setbuckets(AIndex : Integer; const AValue : TPivotGroupSortValueBucketTypebucketsArray); virtual;
    Procedure SetvaluesIndex(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property buckets : TPivotGroupSortValueBucketTypebucketsArray Index 0 Read Fbuckets Write Setbuckets;
    Property valuesIndex : integer Index 8 Read FvaluesIndex Write SetvaluesIndex;
  end;
  TPivotGroupSortValueBucketClass = Class of TPivotGroupSortValueBucket;
  
  { --------------------------------------------------------------------
    TDimensionProperties
    --------------------------------------------------------------------}
  
  TDimensionProperties = Class(TGoogleBaseObject)
  Private
    FhiddenByUser : boolean;
    FpixelSize : integer;
    FhiddenByFilter : boolean;
  Protected
    //Property setters
    Procedure SethiddenByUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetpixelSize(AIndex : Integer; const AValue : integer); virtual;
    Procedure SethiddenByFilter(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property hiddenByUser : boolean Index 0 Read FhiddenByUser Write SethiddenByUser;
    Property pixelSize : integer Index 8 Read FpixelSize Write SetpixelSize;
    Property hiddenByFilter : boolean Index 16 Read FhiddenByFilter Write SethiddenByFilter;
  end;
  TDimensionPropertiesClass = Class of TDimensionProperties;
  
  { --------------------------------------------------------------------
    TEmbeddedObjectPosition
    --------------------------------------------------------------------}
  
  TEmbeddedObjectPosition = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
    FoverlayPosition : TOverlayPosition;
    FnewSheet : boolean;
  Protected
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetoverlayPosition(AIndex : Integer; const AValue : TOverlayPosition); virtual;
    Procedure SetnewSheet(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
    Property overlayPosition : TOverlayPosition Index 8 Read FoverlayPosition Write SetoverlayPosition;
    Property newSheet : boolean Index 16 Read FnewSheet Write SetnewSheet;
  end;
  TEmbeddedObjectPositionClass = Class of TEmbeddedObjectPosition;
  
  { --------------------------------------------------------------------
    TInterpolationPoint
    --------------------------------------------------------------------}
  
  TInterpolationPoint = Class(TGoogleBaseObject)
  Private
    Fvalue : String;
    Fcolor : TColor;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcolor(AIndex : Integer; const AValue : TColor); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property value : String Index 0 Read Fvalue Write Setvalue;
    Property color : TColor Index 8 Read Fcolor Write Setcolor;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TInterpolationPointClass = Class of TInterpolationPoint;
  
  { --------------------------------------------------------------------
    TErrorValue
    --------------------------------------------------------------------}
  
  TErrorValue = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Fmessage : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property message : String Index 8 Read Fmessage Write Setmessage;
  end;
  TErrorValueClass = Class of TErrorValue;
  
  { --------------------------------------------------------------------
    TDuplicateFilterViewRequest
    --------------------------------------------------------------------}
  
  TDuplicateFilterViewRequest = Class(TGoogleBaseObject)
  Private
    FfilterId : integer;
  Protected
    //Property setters
    Procedure SetfilterId(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property filterId : integer Index 0 Read FfilterId Write SetfilterId;
  end;
  TDuplicateFilterViewRequestClass = Class of TDuplicateFilterViewRequest;
  
  { --------------------------------------------------------------------
    TBatchUpdateSpreadsheetRequest
    --------------------------------------------------------------------}
  
  TBatchUpdateSpreadsheetRequest = Class(TGoogleBaseObject)
  Private
    Frequests : TBatchUpdateSpreadsheetRequestTyperequestsArray;
  Protected
    //Property setters
    Procedure Setrequests(AIndex : Integer; const AValue : TBatchUpdateSpreadsheetRequestTyperequestsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property requests : TBatchUpdateSpreadsheetRequestTyperequestsArray Index 0 Read Frequests Write Setrequests;
  end;
  TBatchUpdateSpreadsheetRequestClass = Class of TBatchUpdateSpreadsheetRequest;
  
  { --------------------------------------------------------------------
    TSheetProperties
    --------------------------------------------------------------------}
  
  TSheetProperties = Class(TGoogleBaseObject)
  Private
    Ftitle : String;
    Findex : integer;
    Fhidden : boolean;
    FgridProperties : TGridProperties;
    FsheetId : integer;
    FrightToLeft : boolean;
    FtabColor : TColor;
    FsheetType : String;
  Protected
    //Property setters
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setindex(AIndex : Integer; const AValue : integer); virtual;
    Procedure Sethidden(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetgridProperties(AIndex : Integer; const AValue : TGridProperties); virtual;
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetrightToLeft(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SettabColor(AIndex : Integer; const AValue : TColor); virtual;
    Procedure SetsheetType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property title : String Index 0 Read Ftitle Write Settitle;
    Property index : integer Index 8 Read Findex Write Setindex;
    Property hidden : boolean Index 16 Read Fhidden Write Sethidden;
    Property gridProperties : TGridProperties Index 24 Read FgridProperties Write SetgridProperties;
    Property sheetId : integer Index 32 Read FsheetId Write SetsheetId;
    Property rightToLeft : boolean Index 40 Read FrightToLeft Write SetrightToLeft;
    Property tabColor : TColor Index 48 Read FtabColor Write SettabColor;
    Property sheetType : String Index 56 Read FsheetType Write SetsheetType;
  end;
  TSheetPropertiesClass = Class of TSheetProperties;
  
  { --------------------------------------------------------------------
    TProtectedRange
    --------------------------------------------------------------------}
  
  TProtectedRange = Class(TGoogleBaseObject)
  Private
    FunprotectedRanges : TProtectedRangeTypeunprotectedRangesArray;
    Fdescription : String;
    FnamedRangeId : String;
    FrequestingUserCanEdit : boolean;
    Feditors : TEditors;
    FprotectedRangeId : integer;
    FwarningOnly : boolean;
    Frange : TGridRange;
  Protected
    //Property setters
    Procedure SetunprotectedRanges(AIndex : Integer; const AValue : TProtectedRangeTypeunprotectedRangesArray); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnamedRangeId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequestingUserCanEdit(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Seteditors(AIndex : Integer; const AValue : TEditors); virtual;
    Procedure SetprotectedRangeId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetwarningOnly(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property unprotectedRanges : TProtectedRangeTypeunprotectedRangesArray Index 0 Read FunprotectedRanges Write SetunprotectedRanges;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property namedRangeId : String Index 16 Read FnamedRangeId Write SetnamedRangeId;
    Property requestingUserCanEdit : boolean Index 24 Read FrequestingUserCanEdit Write SetrequestingUserCanEdit;
    Property editors : TEditors Index 32 Read Feditors Write Seteditors;
    Property protectedRangeId : integer Index 40 Read FprotectedRangeId Write SetprotectedRangeId;
    Property warningOnly : boolean Index 48 Read FwarningOnly Write SetwarningOnly;
    Property range : TGridRange Index 56 Read Frange Write Setrange;
  end;
  TProtectedRangeClass = Class of TProtectedRange;
  
  { --------------------------------------------------------------------
    TDeleteConditionalFormatRuleRequest
    --------------------------------------------------------------------}
  
  TDeleteConditionalFormatRuleRequest = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
    Findex : integer;
  Protected
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setindex(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
    Property index : integer Index 8 Read Findex Write Setindex;
  end;
  TDeleteConditionalFormatRuleRequestClass = Class of TDeleteConditionalFormatRuleRequest;
  
  { --------------------------------------------------------------------
    TChartSpec
    --------------------------------------------------------------------}
  
  TChartSpec = Class(TGoogleBaseObject)
  Private
    FhiddenDimensionStrategy : String;
    FbasicChart : TBasicChartSpec;
    Ftitle : String;
    FpieChart : TPieChartSpec;
  Protected
    //Property setters
    Procedure SethiddenDimensionStrategy(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbasicChart(AIndex : Integer; const AValue : TBasicChartSpec); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpieChart(AIndex : Integer; const AValue : TPieChartSpec); virtual;
  Public
  Published
    Property hiddenDimensionStrategy : String Index 0 Read FhiddenDimensionStrategy Write SethiddenDimensionStrategy;
    Property basicChart : TBasicChartSpec Index 8 Read FbasicChart Write SetbasicChart;
    Property title : String Index 16 Read Ftitle Write Settitle;
    Property pieChart : TPieChartSpec Index 24 Read FpieChart Write SetpieChart;
  end;
  TChartSpecClass = Class of TChartSpec;
  
  { --------------------------------------------------------------------
    TSourceAndDestination
    --------------------------------------------------------------------}
  
  TSourceAndDestination = Class(TGoogleBaseObject)
  Private
    Fsource : TGridRange;
    FfillLength : integer;
    Fdimension : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure SetfillLength(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setdimension(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TGridRange Index 0 Read Fsource Write Setsource;
    Property fillLength : integer Index 8 Read FfillLength Write SetfillLength;
    Property dimension : String Index 16 Read Fdimension Write Setdimension;
  end;
  TSourceAndDestinationClass = Class of TSourceAndDestination;
  
  { --------------------------------------------------------------------
    TConditionValue
    --------------------------------------------------------------------}
  
  TConditionValue = Class(TGoogleBaseObject)
  Private
    FrelativeDate : String;
    FuserEnteredValue : String;
  Protected
    //Property setters
    Procedure SetrelativeDate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserEnteredValue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property relativeDate : String Index 0 Read FrelativeDate Write SetrelativeDate;
    Property userEnteredValue : String Index 8 Read FuserEnteredValue Write SetuserEnteredValue;
  end;
  TConditionValueClass = Class of TConditionValue;
  
  { --------------------------------------------------------------------
    TPasteDataRequest
    --------------------------------------------------------------------}
  
  TPasteDataRequest = Class(TGoogleBaseObject)
  Private
    Fdata : String;
    Fcoordinate : TGridCoordinate;
    Fdelimiter : String;
    F_type : String;
    Fhtml : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdata(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcoordinate(AIndex : Integer; const AValue : TGridCoordinate); virtual;
    Procedure Setdelimiter(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Sethtml(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property data : String Index 0 Read Fdata Write Setdata;
    Property coordinate : TGridCoordinate Index 8 Read Fcoordinate Write Setcoordinate;
    Property delimiter : String Index 16 Read Fdelimiter Write Setdelimiter;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property html : boolean Index 32 Read Fhtml Write Sethtml;
  end;
  TPasteDataRequestClass = Class of TPasteDataRequest;
  
  { --------------------------------------------------------------------
    TFindReplaceRequest
    --------------------------------------------------------------------}
  
  TFindReplaceRequest = Class(TGoogleBaseObject)
  Private
    Ffind : String;
    Freplacement : String;
    FsearchByRegex : boolean;
    FsheetId : integer;
    FallSheets : boolean;
    FmatchCase : boolean;
    FincludeFormulas : boolean;
    Frange : TGridRange;
    FmatchEntireCell : boolean;
  Protected
    //Property setters
    Procedure Setfind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreplacement(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsearchByRegex(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetallSheets(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetmatchCase(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetincludeFormulas(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure SetmatchEntireCell(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property find : String Index 0 Read Ffind Write Setfind;
    Property replacement : String Index 8 Read Freplacement Write Setreplacement;
    Property searchByRegex : boolean Index 16 Read FsearchByRegex Write SetsearchByRegex;
    Property sheetId : integer Index 24 Read FsheetId Write SetsheetId;
    Property allSheets : boolean Index 32 Read FallSheets Write SetallSheets;
    Property matchCase : boolean Index 40 Read FmatchCase Write SetmatchCase;
    Property includeFormulas : boolean Index 48 Read FincludeFormulas Write SetincludeFormulas;
    Property range : TGridRange Index 56 Read Frange Write Setrange;
    Property matchEntireCell : boolean Index 64 Read FmatchEntireCell Write SetmatchEntireCell;
  end;
  TFindReplaceRequestClass = Class of TFindReplaceRequest;
  
  { --------------------------------------------------------------------
    TSortSpec
    --------------------------------------------------------------------}
  
  TSortSpec = Class(TGoogleBaseObject)
  Private
    FsortOrder : String;
    FdimensionIndex : integer;
  Protected
    //Property setters
    Procedure SetsortOrder(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdimensionIndex(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property sortOrder : String Index 0 Read FsortOrder Write SetsortOrder;
    Property dimensionIndex : integer Index 8 Read FdimensionIndex Write SetdimensionIndex;
  end;
  TSortSpecClass = Class of TSortSpec;
  
  { --------------------------------------------------------------------
    TCopySheetToAnotherSpreadsheetRequest
    --------------------------------------------------------------------}
  
  TCopySheetToAnotherSpreadsheetRequest = Class(TGoogleBaseObject)
  Private
    FdestinationSpreadsheetId : String;
  Protected
    //Property setters
    Procedure SetdestinationSpreadsheetId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property destinationSpreadsheetId : String Index 0 Read FdestinationSpreadsheetId Write SetdestinationSpreadsheetId;
  end;
  TCopySheetToAnotherSpreadsheetRequestClass = Class of TCopySheetToAnotherSpreadsheetRequest;
  
  { --------------------------------------------------------------------
    TNumberFormat
    --------------------------------------------------------------------}
  
  TNumberFormat = Class(TGoogleBaseObject)
  Private
    Fpattern : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setpattern(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property pattern : String Index 0 Read Fpattern Write Setpattern;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TNumberFormatClass = Class of TNumberFormat;
  
  { --------------------------------------------------------------------
    TUpdateDimensionPropertiesRequest
    --------------------------------------------------------------------}
  
  TUpdateDimensionPropertiesRequest = Class(TGoogleBaseObject)
  Private
    Ffields : String;
    Frange : TDimensionRange;
    Fproperties : TDimensionProperties;
  Protected
    //Property setters
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TDimensionRange); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TDimensionProperties); virtual;
  Public
  Published
    Property fields : String Index 0 Read Ffields Write Setfields;
    Property range : TDimensionRange Index 8 Read Frange Write Setrange;
    Property properties : TDimensionProperties Index 16 Read Fproperties Write Setproperties;
  end;
  TUpdateDimensionPropertiesRequestClass = Class of TUpdateDimensionPropertiesRequest;
  
  { --------------------------------------------------------------------
    TEditors
    --------------------------------------------------------------------}
  
  TEditors = Class(TGoogleBaseObject)
  Private
    Fusers : TStringArray;
    Fgroups : TStringArray;
    FdomainUsersCanEdit : boolean;
  Protected
    //Property setters
    Procedure Setusers(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setgroups(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetdomainUsersCanEdit(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property users : TStringArray Index 0 Read Fusers Write Setusers;
    Property groups : TStringArray Index 8 Read Fgroups Write Setgroups;
    Property domainUsersCanEdit : boolean Index 16 Read FdomainUsersCanEdit Write SetdomainUsersCanEdit;
  end;
  TEditorsClass = Class of TEditors;
  
  { --------------------------------------------------------------------
    TSpreadsheet
    --------------------------------------------------------------------}
  
  TSpreadsheet = Class(TGoogleBaseObject)
  Private
    FspreadsheetId : String;
    Fproperties : TSpreadsheetProperties;
    Fsheets : TSpreadsheetTypesheetsArray;
    FnamedRanges : TSpreadsheetTypenamedRangesArray;
  Protected
    //Property setters
    Procedure SetspreadsheetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TSpreadsheetProperties); virtual;
    Procedure Setsheets(AIndex : Integer; const AValue : TSpreadsheetTypesheetsArray); virtual;
    Procedure SetnamedRanges(AIndex : Integer; const AValue : TSpreadsheetTypenamedRangesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property spreadsheetId : String Index 0 Read FspreadsheetId Write SetspreadsheetId;
    Property properties : TSpreadsheetProperties Index 8 Read Fproperties Write Setproperties;
    Property sheets : TSpreadsheetTypesheetsArray Index 16 Read Fsheets Write Setsheets;
    Property namedRanges : TSpreadsheetTypenamedRangesArray Index 24 Read FnamedRanges Write SetnamedRanges;
  end;
  TSpreadsheetClass = Class of TSpreadsheet;
  
  { --------------------------------------------------------------------
    TGridData
    --------------------------------------------------------------------}
  
  TGridData = Class(TGoogleBaseObject)
  Private
    FcolumnMetadata : TGridDataTypecolumnMetadataArray;
    FrowData : TGridDataTyperowDataArray;
    FstartRow : integer;
    FrowMetadata : TGridDataTyperowMetadataArray;
    FstartColumn : integer;
  Protected
    //Property setters
    Procedure SetcolumnMetadata(AIndex : Integer; const AValue : TGridDataTypecolumnMetadataArray); virtual;
    Procedure SetrowData(AIndex : Integer; const AValue : TGridDataTyperowDataArray); virtual;
    Procedure SetstartRow(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetrowMetadata(AIndex : Integer; const AValue : TGridDataTyperowMetadataArray); virtual;
    Procedure SetstartColumn(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property columnMetadata : TGridDataTypecolumnMetadataArray Index 0 Read FcolumnMetadata Write SetcolumnMetadata;
    Property rowData : TGridDataTyperowDataArray Index 8 Read FrowData Write SetrowData;
    Property startRow : integer Index 16 Read FstartRow Write SetstartRow;
    Property rowMetadata : TGridDataTyperowMetadataArray Index 24 Read FrowMetadata Write SetrowMetadata;
    Property startColumn : integer Index 32 Read FstartColumn Write SetstartColumn;
  end;
  TGridDataClass = Class of TGridData;
  
  { --------------------------------------------------------------------
    TPivotValue
    --------------------------------------------------------------------}
  
  TPivotValue = Class(TGoogleBaseObject)
  Private
    Fformula : String;
    FsourceColumnOffset : integer;
    FsummarizeFunction : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setformula(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceColumnOffset(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetsummarizeFunction(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property formula : String Index 0 Read Fformula Write Setformula;
    Property sourceColumnOffset : integer Index 8 Read FsourceColumnOffset Write SetsourceColumnOffset;
    Property summarizeFunction : String Index 16 Read FsummarizeFunction Write SetsummarizeFunction;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TPivotValueClass = Class of TPivotValue;
  
  { --------------------------------------------------------------------
    TBasicFilterTypecriteria
    --------------------------------------------------------------------}
  
  TBasicFilterTypecriteria = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TBasicFilterTypecriteriaClass = Class of TBasicFilterTypecriteria;
  
  { --------------------------------------------------------------------
    TBasicFilter
    --------------------------------------------------------------------}
  
  TBasicFilter = Class(TGoogleBaseObject)
  Private
    Fcriteria : TBasicFilterTypecriteria;
    Frange : TGridRange;
    FsortSpecs : TBasicFilterTypesortSpecsArray;
  Protected
    //Property setters
    Procedure Setcriteria(AIndex : Integer; const AValue : TBasicFilterTypecriteria); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure SetsortSpecs(AIndex : Integer; const AValue : TBasicFilterTypesortSpecsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property criteria : TBasicFilterTypecriteria Index 0 Read Fcriteria Write Setcriteria;
    Property range : TGridRange Index 8 Read Frange Write Setrange;
    Property sortSpecs : TBasicFilterTypesortSpecsArray Index 16 Read FsortSpecs Write SetsortSpecs;
  end;
  TBasicFilterClass = Class of TBasicFilter;
  
  { --------------------------------------------------------------------
    TDuplicateSheetRequest
    --------------------------------------------------------------------}
  
  TDuplicateSheetRequest = Class(TGoogleBaseObject)
  Private
    FsourceSheetId : integer;
    FnewSheetId : integer;
    FinsertSheetIndex : integer;
    FnewSheetName : String;
  Protected
    //Property setters
    Procedure SetsourceSheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnewSheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetinsertSheetIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnewSheetName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property sourceSheetId : integer Index 0 Read FsourceSheetId Write SetsourceSheetId;
    Property newSheetId : integer Index 8 Read FnewSheetId Write SetnewSheetId;
    Property insertSheetIndex : integer Index 16 Read FinsertSheetIndex Write SetinsertSheetIndex;
    Property newSheetName : String Index 24 Read FnewSheetName Write SetnewSheetName;
  end;
  TDuplicateSheetRequestClass = Class of TDuplicateSheetRequest;
  
  { --------------------------------------------------------------------
    TAddFilterViewResponse
    --------------------------------------------------------------------}
  
  TAddFilterViewResponse = Class(TGoogleBaseObject)
  Private
    Ffilter : TFilterView;
  Protected
    //Property setters
    Procedure Setfilter(AIndex : Integer; const AValue : TFilterView); virtual;
  Public
  Published
    Property filter : TFilterView Index 0 Read Ffilter Write Setfilter;
  end;
  TAddFilterViewResponseClass = Class of TAddFilterViewResponse;
  
  { --------------------------------------------------------------------
    TDuplicateSheetResponse
    --------------------------------------------------------------------}
  
  TDuplicateSheetResponse = Class(TGoogleBaseObject)
  Private
    Fproperties : TSheetProperties;
  Protected
    //Property setters
    Procedure Setproperties(AIndex : Integer; const AValue : TSheetProperties); virtual;
  Public
  Published
    Property properties : TSheetProperties Index 0 Read Fproperties Write Setproperties;
  end;
  TDuplicateSheetResponseClass = Class of TDuplicateSheetResponse;
  
  { --------------------------------------------------------------------
    TBorder
    --------------------------------------------------------------------}
  
  TBorder = Class(TGoogleBaseObject)
  Private
    Fstyle : String;
    Fwidth : integer;
    Fcolor : TColor;
  Protected
    //Property setters
    Procedure Setstyle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setcolor(AIndex : Integer; const AValue : TColor); virtual;
  Public
  Published
    Property style : String Index 0 Read Fstyle Write Setstyle;
    Property width : integer Index 8 Read Fwidth Write Setwidth;
    Property color : TColor Index 16 Read Fcolor Write Setcolor;
  end;
  TBorderClass = Class of TBorder;
  
  { --------------------------------------------------------------------
    TAddNamedRangeRequest
    --------------------------------------------------------------------}
  
  TAddNamedRangeRequest = Class(TGoogleBaseObject)
  Private
    FnamedRange : TNamedRange;
  Protected
    //Property setters
    Procedure SetnamedRange(AIndex : Integer; const AValue : TNamedRange); virtual;
  Public
  Published
    Property namedRange : TNamedRange Index 0 Read FnamedRange Write SetnamedRange;
  end;
  TAddNamedRangeRequestClass = Class of TAddNamedRangeRequest;
  
  { --------------------------------------------------------------------
    TAddChartResponse
    --------------------------------------------------------------------}
  
  TAddChartResponse = Class(TGoogleBaseObject)
  Private
    Fchart : TEmbeddedChart;
  Protected
    //Property setters
    Procedure Setchart(AIndex : Integer; const AValue : TEmbeddedChart); virtual;
  Public
  Published
    Property chart : TEmbeddedChart Index 0 Read Fchart Write Setchart;
  end;
  TAddChartResponseClass = Class of TAddChartResponse;
  
  { --------------------------------------------------------------------
    TAppendCellsRequest
    --------------------------------------------------------------------}
  
  TAppendCellsRequest = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
    Frows : TAppendCellsRequestTyperowsArray;
    Ffields : String;
  Protected
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setrows(AIndex : Integer; const AValue : TAppendCellsRequestTyperowsArray); virtual;
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
    Property rows : TAppendCellsRequestTyperowsArray Index 8 Read Frows Write Setrows;
    Property fields : String Index 16 Read Ffields Write Setfields;
  end;
  TAppendCellsRequestClass = Class of TAppendCellsRequest;
  
  { --------------------------------------------------------------------
    TRowData
    --------------------------------------------------------------------}
  
  TRowData = Class(TGoogleBaseObject)
  Private
    Fvalues : TRowDataTypevaluesArray;
  Protected
    //Property setters
    Procedure Setvalues(AIndex : Integer; const AValue : TRowDataTypevaluesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property values : TRowDataTypevaluesArray Index 0 Read Fvalues Write Setvalues;
  end;
  TRowDataClass = Class of TRowData;
  
  { --------------------------------------------------------------------
    TBasicChartSeries
    --------------------------------------------------------------------}
  
  TBasicChartSeries = Class(TGoogleBaseObject)
  Private
    Fseries : TChartData;
    FtargetAxis : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setseries(AIndex : Integer; const AValue : TChartData); virtual;
    Procedure SettargetAxis(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property series : TChartData Index 0 Read Fseries Write Setseries;
    Property targetAxis : String Index 8 Read FtargetAxis Write SettargetAxis;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TBasicChartSeriesClass = Class of TBasicChartSeries;
  
  { --------------------------------------------------------------------
    TRepeatCellRequest
    --------------------------------------------------------------------}
  
  TRepeatCellRequest = Class(TGoogleBaseObject)
  Private
    Fcell : TCellData;
    Ffields : String;
    Frange : TGridRange;
  Protected
    //Property setters
    Procedure Setcell(AIndex : Integer; const AValue : TCellData); virtual;
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
  Public
  Published
    Property cell : TCellData Index 0 Read Fcell Write Setcell;
    Property fields : String Index 8 Read Ffields Write Setfields;
    Property range : TGridRange Index 16 Read Frange Write Setrange;
  end;
  TRepeatCellRequestClass = Class of TRepeatCellRequest;
  
  { --------------------------------------------------------------------
    TBasicChartSpec
    --------------------------------------------------------------------}
  
  TBasicChartSpec = Class(TGoogleBaseObject)
  Private
    FchartType : String;
    Fdomains : TBasicChartSpecTypedomainsArray;
    FheaderCount : integer;
    Fseries : TBasicChartSpecTypeseriesArray;
    FlegendPosition : String;
    Faxis : TBasicChartSpecTypeaxisArray;
  Protected
    //Property setters
    Procedure SetchartType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdomains(AIndex : Integer; const AValue : TBasicChartSpecTypedomainsArray); virtual;
    Procedure SetheaderCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setseries(AIndex : Integer; const AValue : TBasicChartSpecTypeseriesArray); virtual;
    Procedure SetlegendPosition(AIndex : Integer; const AValue : String); virtual;
    Procedure Setaxis(AIndex : Integer; const AValue : TBasicChartSpecTypeaxisArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property chartType : String Index 0 Read FchartType Write SetchartType;
    Property domains : TBasicChartSpecTypedomainsArray Index 8 Read Fdomains Write Setdomains;
    Property headerCount : integer Index 16 Read FheaderCount Write SetheaderCount;
    Property series : TBasicChartSpecTypeseriesArray Index 24 Read Fseries Write Setseries;
    Property legendPosition : String Index 32 Read FlegendPosition Write SetlegendPosition;
    Property axis : TBasicChartSpecTypeaxisArray Index 40 Read Faxis Write Setaxis;
  end;
  TBasicChartSpecClass = Class of TBasicChartSpec;
  
  { --------------------------------------------------------------------
    TNamedRange
    --------------------------------------------------------------------}
  
  TNamedRange = Class(TGoogleBaseObject)
  Private
    FnamedRangeId : String;
    Frange : TGridRange;
    Fname : String;
  Protected
    //Property setters
    Procedure SetnamedRangeId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property namedRangeId : String Index 0 Read FnamedRangeId Write SetnamedRangeId;
    Property range : TGridRange Index 8 Read Frange Write Setrange;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TNamedRangeClass = Class of TNamedRange;
  
  { --------------------------------------------------------------------
    TSetBasicFilterRequest
    --------------------------------------------------------------------}
  
  TSetBasicFilterRequest = Class(TGoogleBaseObject)
  Private
    Ffilter : TBasicFilter;
  Protected
    //Property setters
    Procedure Setfilter(AIndex : Integer; const AValue : TBasicFilter); virtual;
  Public
  Published
    Property filter : TBasicFilter Index 0 Read Ffilter Write Setfilter;
  end;
  TSetBasicFilterRequestClass = Class of TSetBasicFilterRequest;
  
  { --------------------------------------------------------------------
    TUpdateEmbeddedObjectPositionRequest
    --------------------------------------------------------------------}
  
  TUpdateEmbeddedObjectPositionRequest = Class(TGoogleBaseObject)
  Private
    FnewPosition : TEmbeddedObjectPosition;
    FobjectId : integer;
    Ffields : String;
  Protected
    //Property setters
    Procedure SetnewPosition(AIndex : Integer; const AValue : TEmbeddedObjectPosition); virtual;
    Procedure SetobjectId(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property newPosition : TEmbeddedObjectPosition Index 0 Read FnewPosition Write SetnewPosition;
    Property objectId : integer Index 8 Read FobjectId Write SetobjectId;
    Property fields : String Index 16 Read Ffields Write Setfields;
  end;
  TUpdateEmbeddedObjectPositionRequestClass = Class of TUpdateEmbeddedObjectPositionRequest;
  
  { --------------------------------------------------------------------
    TAutoResizeDimensionsRequest
    --------------------------------------------------------------------}
  
  TAutoResizeDimensionsRequest = Class(TGoogleBaseObject)
  Private
    Fdimensions : TDimensionRange;
  Protected
    //Property setters
    Procedure Setdimensions(AIndex : Integer; const AValue : TDimensionRange); virtual;
  Public
  Published
    Property dimensions : TDimensionRange Index 0 Read Fdimensions Write Setdimensions;
  end;
  TAutoResizeDimensionsRequestClass = Class of TAutoResizeDimensionsRequest;
  
  { --------------------------------------------------------------------
    TDuplicateFilterViewResponse
    --------------------------------------------------------------------}
  
  TDuplicateFilterViewResponse = Class(TGoogleBaseObject)
  Private
    Ffilter : TFilterView;
  Protected
    //Property setters
    Procedure Setfilter(AIndex : Integer; const AValue : TFilterView); virtual;
  Public
  Published
    Property filter : TFilterView Index 0 Read Ffilter Write Setfilter;
  end;
  TDuplicateFilterViewResponseClass = Class of TDuplicateFilterViewResponse;
  
  { --------------------------------------------------------------------
    TPivotGroup
    --------------------------------------------------------------------}
  
  TPivotGroup = Class(TGoogleBaseObject)
  Private
    FsortOrder : String;
    FsourceColumnOffset : integer;
    FshowTotals : boolean;
    FvalueBucket : TPivotGroupSortValueBucket;
    FvalueMetadata : TPivotGroupTypevalueMetadataArray;
  Protected
    //Property setters
    Procedure SetsortOrder(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceColumnOffset(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetshowTotals(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetvalueBucket(AIndex : Integer; const AValue : TPivotGroupSortValueBucket); virtual;
    Procedure SetvalueMetadata(AIndex : Integer; const AValue : TPivotGroupTypevalueMetadataArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property sortOrder : String Index 0 Read FsortOrder Write SetsortOrder;
    Property sourceColumnOffset : integer Index 8 Read FsourceColumnOffset Write SetsourceColumnOffset;
    Property showTotals : boolean Index 16 Read FshowTotals Write SetshowTotals;
    Property valueBucket : TPivotGroupSortValueBucket Index 24 Read FvalueBucket Write SetvalueBucket;
    Property valueMetadata : TPivotGroupTypevalueMetadataArray Index 32 Read FvalueMetadata Write SetvalueMetadata;
  end;
  TPivotGroupClass = Class of TPivotGroup;
  
  { --------------------------------------------------------------------
    TGridRange
    --------------------------------------------------------------------}
  
  TGridRange = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
    FstartColumnIndex : integer;
    FstartRowIndex : integer;
    FendRowIndex : integer;
    FendColumnIndex : integer;
  Protected
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetstartColumnIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetstartRowIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetendRowIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetendColumnIndex(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
    Property startColumnIndex : integer Index 8 Read FstartColumnIndex Write SetstartColumnIndex;
    Property startRowIndex : integer Index 16 Read FstartRowIndex Write SetstartRowIndex;
    Property endRowIndex : integer Index 24 Read FendRowIndex Write SetendRowIndex;
    Property endColumnIndex : integer Index 32 Read FendColumnIndex Write SetendColumnIndex;
  end;
  TGridRangeClass = Class of TGridRange;
  
  { --------------------------------------------------------------------
    TDeleteSheetRequest
    --------------------------------------------------------------------}
  
  TDeleteSheetRequest = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
  Protected
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
  end;
  TDeleteSheetRequestClass = Class of TDeleteSheetRequest;
  
  { --------------------------------------------------------------------
    TChartData
    --------------------------------------------------------------------}
  
  TChartData = Class(TGoogleBaseObject)
  Private
    FsourceRange : TChartSourceRange;
  Protected
    //Property setters
    Procedure SetsourceRange(AIndex : Integer; const AValue : TChartSourceRange); virtual;
  Public
  Published
    Property sourceRange : TChartSourceRange Index 0 Read FsourceRange Write SetsourceRange;
  end;
  TChartDataClass = Class of TChartData;
  
  { --------------------------------------------------------------------
    TSheet
    --------------------------------------------------------------------}
  
  TSheet = Class(TGoogleBaseObject)
  Private
    Fproperties : TSheetProperties;
    Fcharts : TSheetTypechartsArray;
    FfilterViews : TSheetTypefilterViewsArray;
    FconditionalFormats : TSheetTypeconditionalFormatsArray;
    FprotectedRanges : TSheetTypeprotectedRangesArray;
    FbasicFilter : TBasicFilter;
    Fmerges : TSheetTypemergesArray;
    Fdata : TSheetTypedataArray;
  Protected
    //Property setters
    Procedure Setproperties(AIndex : Integer; const AValue : TSheetProperties); virtual;
    Procedure Setcharts(AIndex : Integer; const AValue : TSheetTypechartsArray); virtual;
    Procedure SetfilterViews(AIndex : Integer; const AValue : TSheetTypefilterViewsArray); virtual;
    Procedure SetconditionalFormats(AIndex : Integer; const AValue : TSheetTypeconditionalFormatsArray); virtual;
    Procedure SetprotectedRanges(AIndex : Integer; const AValue : TSheetTypeprotectedRangesArray); virtual;
    Procedure SetbasicFilter(AIndex : Integer; const AValue : TBasicFilter); virtual;
    Procedure Setmerges(AIndex : Integer; const AValue : TSheetTypemergesArray); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : TSheetTypedataArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property properties : TSheetProperties Index 0 Read Fproperties Write Setproperties;
    Property charts : TSheetTypechartsArray Index 8 Read Fcharts Write Setcharts;
    Property filterViews : TSheetTypefilterViewsArray Index 16 Read FfilterViews Write SetfilterViews;
    Property conditionalFormats : TSheetTypeconditionalFormatsArray Index 24 Read FconditionalFormats Write SetconditionalFormats;
    Property protectedRanges : TSheetTypeprotectedRangesArray Index 32 Read FprotectedRanges Write SetprotectedRanges;
    Property basicFilter : TBasicFilter Index 40 Read FbasicFilter Write SetbasicFilter;
    Property merges : TSheetTypemergesArray Index 48 Read Fmerges Write Setmerges;
    Property data : TSheetTypedataArray Index 56 Read Fdata Write Setdata;
  end;
  TSheetClass = Class of TSheet;
  
  { --------------------------------------------------------------------
    TCopyPasteRequest
    --------------------------------------------------------------------}
  
  TCopyPasteRequest = Class(TGoogleBaseObject)
  Private
    FpasteType : String;
    FpasteOrientation : String;
    Fsource : TGridRange;
    Fdestination : TGridRange;
  Protected
    //Property setters
    Procedure SetpasteType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpasteOrientation(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure Setdestination(AIndex : Integer; const AValue : TGridRange); virtual;
  Public
  Published
    Property pasteType : String Index 0 Read FpasteType Write SetpasteType;
    Property pasteOrientation : String Index 8 Read FpasteOrientation Write SetpasteOrientation;
    Property source : TGridRange Index 16 Read Fsource Write Setsource;
    Property destination : TGridRange Index 24 Read Fdestination Write Setdestination;
  end;
  TCopyPasteRequestClass = Class of TCopyPasteRequest;
  
  { --------------------------------------------------------------------
    TUpdateCellsRequest
    --------------------------------------------------------------------}
  
  TUpdateCellsRequest = Class(TGoogleBaseObject)
  Private
    Frows : TUpdateCellsRequestTyperowsArray;
    Ffields : String;
    Fstart : TGridCoordinate;
    Frange : TGridRange;
  Protected
    //Property setters
    Procedure Setrows(AIndex : Integer; const AValue : TUpdateCellsRequestTyperowsArray); virtual;
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : TGridCoordinate); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property rows : TUpdateCellsRequestTyperowsArray Index 0 Read Frows Write Setrows;
    Property fields : String Index 8 Read Ffields Write Setfields;
    Property start : TGridCoordinate Index 16 Read Fstart Write Setstart;
    Property range : TGridRange Index 24 Read Frange Write Setrange;
  end;
  TUpdateCellsRequestClass = Class of TUpdateCellsRequest;
  
  { --------------------------------------------------------------------
    TExtendedValue
    --------------------------------------------------------------------}
  
  TExtendedValue = Class(TGoogleBaseObject)
  Private
    FformulaValue : String;
    FerrorValue : TErrorValue;
    FboolValue : boolean;
    FnumberValue : double;
    FstringValue : String;
  Protected
    //Property setters
    Procedure SetformulaValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SeterrorValue(AIndex : Integer; const AValue : TErrorValue); virtual;
    Procedure SetboolValue(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetnumberValue(AIndex : Integer; const AValue : double); virtual;
    Procedure SetstringValue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property formulaValue : String Index 0 Read FformulaValue Write SetformulaValue;
    Property errorValue : TErrorValue Index 8 Read FerrorValue Write SeterrorValue;
    Property boolValue : boolean Index 16 Read FboolValue Write SetboolValue;
    Property numberValue : double Index 24 Read FnumberValue Write SetnumberValue;
    Property stringValue : String Index 32 Read FstringValue Write SetstringValue;
  end;
  TExtendedValueClass = Class of TExtendedValue;
  
  { --------------------------------------------------------------------
    TBatchUpdateSpreadsheetResponse
    --------------------------------------------------------------------}
  
  TBatchUpdateSpreadsheetResponse = Class(TGoogleBaseObject)
  Private
    FspreadsheetId : String;
    Freplies : TBatchUpdateSpreadsheetResponseTyperepliesArray;
  Protected
    //Property setters
    Procedure SetspreadsheetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreplies(AIndex : Integer; const AValue : TBatchUpdateSpreadsheetResponseTyperepliesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property spreadsheetId : String Index 0 Read FspreadsheetId Write SetspreadsheetId;
    Property replies : TBatchUpdateSpreadsheetResponseTyperepliesArray Index 8 Read Freplies Write Setreplies;
  end;
  TBatchUpdateSpreadsheetResponseClass = Class of TBatchUpdateSpreadsheetResponse;
  
  { --------------------------------------------------------------------
    TGradientRule
    --------------------------------------------------------------------}
  
  TGradientRule = Class(TGoogleBaseObject)
  Private
    Fmaxpoint : TInterpolationPoint;
    Fmidpoint : TInterpolationPoint;
    Fminpoint : TInterpolationPoint;
  Protected
    //Property setters
    Procedure Setmaxpoint(AIndex : Integer; const AValue : TInterpolationPoint); virtual;
    Procedure Setmidpoint(AIndex : Integer; const AValue : TInterpolationPoint); virtual;
    Procedure Setminpoint(AIndex : Integer; const AValue : TInterpolationPoint); virtual;
  Public
  Published
    Property maxpoint : TInterpolationPoint Index 0 Read Fmaxpoint Write Setmaxpoint;
    Property midpoint : TInterpolationPoint Index 8 Read Fmidpoint Write Setmidpoint;
    Property minpoint : TInterpolationPoint Index 16 Read Fminpoint Write Setminpoint;
  end;
  TGradientRuleClass = Class of TGradientRule;
  
  { --------------------------------------------------------------------
    TCutPasteRequest
    --------------------------------------------------------------------}
  
  TCutPasteRequest = Class(TGoogleBaseObject)
  Private
    FpasteType : String;
    Fsource : TGridRange;
    Fdestination : TGridCoordinate;
  Protected
    //Property setters
    Procedure SetpasteType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure Setdestination(AIndex : Integer; const AValue : TGridCoordinate); virtual;
  Public
  Published
    Property pasteType : String Index 0 Read FpasteType Write SetpasteType;
    Property source : TGridRange Index 8 Read Fsource Write Setsource;
    Property destination : TGridCoordinate Index 16 Read Fdestination Write Setdestination;
  end;
  TCutPasteRequestClass = Class of TCutPasteRequest;
  
  { --------------------------------------------------------------------
    TOverlayPosition
    --------------------------------------------------------------------}
  
  TOverlayPosition = Class(TGoogleBaseObject)
  Private
    FwidthPixels : integer;
    FanchorCell : TGridCoordinate;
    FoffsetXPixels : integer;
    FheightPixels : integer;
    FoffsetYPixels : integer;
  Protected
    //Property setters
    Procedure SetwidthPixels(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetanchorCell(AIndex : Integer; const AValue : TGridCoordinate); virtual;
    Procedure SetoffsetXPixels(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetheightPixels(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetoffsetYPixels(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property widthPixels : integer Index 0 Read FwidthPixels Write SetwidthPixels;
    Property anchorCell : TGridCoordinate Index 8 Read FanchorCell Write SetanchorCell;
    Property offsetXPixels : integer Index 16 Read FoffsetXPixels Write SetoffsetXPixels;
    Property heightPixels : integer Index 24 Read FheightPixels Write SetheightPixels;
    Property offsetYPixels : integer Index 32 Read FoffsetYPixels Write SetoffsetYPixels;
  end;
  TOverlayPositionClass = Class of TOverlayPosition;
  
  { --------------------------------------------------------------------
    TAutoFillRequest
    --------------------------------------------------------------------}
  
  TAutoFillRequest = Class(TGoogleBaseObject)
  Private
    FuseAlternateSeries : boolean;
    Frange : TGridRange;
    FsourceAndDestination : TSourceAndDestination;
  Protected
    //Property setters
    Procedure SetuseAlternateSeries(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
    Procedure SetsourceAndDestination(AIndex : Integer; const AValue : TSourceAndDestination); virtual;
  Public
  Published
    Property useAlternateSeries : boolean Index 0 Read FuseAlternateSeries Write SetuseAlternateSeries;
    Property range : TGridRange Index 8 Read Frange Write Setrange;
    Property sourceAndDestination : TSourceAndDestination Index 16 Read FsourceAndDestination Write SetsourceAndDestination;
  end;
  TAutoFillRequestClass = Class of TAutoFillRequest;
  
  { --------------------------------------------------------------------
    TPieChartSpec
    --------------------------------------------------------------------}
  
  TPieChartSpec = Class(TGoogleBaseObject)
  Private
    FlegendPosition : String;
    Fseries : TChartData;
    FpieHole : double;
    FthreeDimensional : boolean;
    Fdomain : TChartData;
  Protected
    //Property setters
    Procedure SetlegendPosition(AIndex : Integer; const AValue : String); virtual;
    Procedure Setseries(AIndex : Integer; const AValue : TChartData); virtual;
    Procedure SetpieHole(AIndex : Integer; const AValue : double); virtual;
    Procedure SetthreeDimensional(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setdomain(AIndex : Integer; const AValue : TChartData); virtual;
  Public
  Published
    Property legendPosition : String Index 0 Read FlegendPosition Write SetlegendPosition;
    Property series : TChartData Index 8 Read Fseries Write Setseries;
    Property pieHole : double Index 16 Read FpieHole Write SetpieHole;
    Property threeDimensional : boolean Index 24 Read FthreeDimensional Write SetthreeDimensional;
    Property domain : TChartData Index 32 Read Fdomain Write Setdomain;
  end;
  TPieChartSpecClass = Class of TPieChartSpec;
  
  { --------------------------------------------------------------------
    TUpdateSheetPropertiesRequest
    --------------------------------------------------------------------}
  
  TUpdateSheetPropertiesRequest = Class(TGoogleBaseObject)
  Private
    Ffields : String;
    Fproperties : TSheetProperties;
  Protected
    //Property setters
    Procedure Setfields(AIndex : Integer; const AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TSheetProperties); virtual;
  Public
  Published
    Property fields : String Index 0 Read Ffields Write Setfields;
    Property properties : TSheetProperties Index 8 Read Fproperties Write Setproperties;
  end;
  TUpdateSheetPropertiesRequestClass = Class of TUpdateSheetPropertiesRequest;
  
  { --------------------------------------------------------------------
    TBooleanRule
    --------------------------------------------------------------------}
  
  TBooleanRule = Class(TGoogleBaseObject)
  Private
    Fcondition : TBooleanCondition;
    Fformat : TCellFormat;
  Protected
    //Property setters
    Procedure Setcondition(AIndex : Integer; const AValue : TBooleanCondition); virtual;
    Procedure Setformat(AIndex : Integer; const AValue : TCellFormat); virtual;
  Public
  Published
    Property condition : TBooleanCondition Index 0 Read Fcondition Write Setcondition;
    Property format : TCellFormat Index 8 Read Fformat Write Setformat;
  end;
  TBooleanRuleClass = Class of TBooleanRule;
  
  { --------------------------------------------------------------------
    TAppendDimensionRequest
    --------------------------------------------------------------------}
  
  TAppendDimensionRequest = Class(TGoogleBaseObject)
  Private
    FsheetId : integer;
    F_length : integer;
    Fdimension : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetsheetId(AIndex : Integer; const AValue : integer); virtual;
    Procedure Set_length(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setdimension(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property sheetId : integer Index 0 Read FsheetId Write SetsheetId;
    Property _length : integer Index 8 Read F_length Write Set_length;
    Property dimension : String Index 16 Read Fdimension Write Setdimension;
  end;
  TAppendDimensionRequestClass = Class of TAppendDimensionRequest;
  
  { --------------------------------------------------------------------
    TAddFilterViewRequest
    --------------------------------------------------------------------}
  
  TAddFilterViewRequest = Class(TGoogleBaseObject)
  Private
    Ffilter : TFilterView;
  Protected
    //Property setters
    Procedure Setfilter(AIndex : Integer; const AValue : TFilterView); virtual;
  Public
  Published
    Property filter : TFilterView Index 0 Read Ffilter Write Setfilter;
  end;
  TAddFilterViewRequestClass = Class of TAddFilterViewRequest;
  
  { --------------------------------------------------------------------
    TGridProperties
    --------------------------------------------------------------------}
  
  TGridProperties = Class(TGoogleBaseObject)
  Private
    FrowCount : integer;
    FcolumnCount : integer;
    FfrozenRowCount : integer;
    FfrozenColumnCount : integer;
    FhideGridlines : boolean;
  Protected
    //Property setters
    Procedure SetrowCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetcolumnCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetfrozenRowCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetfrozenColumnCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SethideGridlines(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property rowCount : integer Index 0 Read FrowCount Write SetrowCount;
    Property columnCount : integer Index 8 Read FcolumnCount Write SetcolumnCount;
    Property frozenRowCount : integer Index 16 Read FfrozenRowCount Write SetfrozenRowCount;
    Property frozenColumnCount : integer Index 24 Read FfrozenColumnCount Write SetfrozenColumnCount;
    Property hideGridlines : boolean Index 32 Read FhideGridlines Write SethideGridlines;
  end;
  TGridPropertiesClass = Class of TGridProperties;
  
  { --------------------------------------------------------------------
    TDeleteNamedRangeRequest
    --------------------------------------------------------------------}
  
  TDeleteNamedRangeRequest = Class(TGoogleBaseObject)
  Private
    FnamedRangeId : String;
  Protected
    //Property setters
    Procedure SetnamedRangeId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property namedRangeId : String Index 0 Read FnamedRangeId Write SetnamedRangeId;
  end;
  TDeleteNamedRangeRequestClass = Class of TDeleteNamedRangeRequest;
  
  { --------------------------------------------------------------------
    TAddChartRequest
    --------------------------------------------------------------------}
  
  TAddChartRequest = Class(TGoogleBaseObject)
  Private
    Fchart : TEmbeddedChart;
  Protected
    //Property setters
    Procedure Setchart(AIndex : Integer; const AValue : TEmbeddedChart); virtual;
  Public
  Published
    Property chart : TEmbeddedChart Index 0 Read Fchart Write Setchart;
  end;
  TAddChartRequestClass = Class of TAddChartRequest;
  
  { --------------------------------------------------------------------
    TSetDataValidationRequest
    --------------------------------------------------------------------}
  
  TSetDataValidationRequest = Class(TGoogleBaseObject)
  Private
    Frule : TDataValidationRule;
    Frange : TGridRange;
  Protected
    //Property setters
    Procedure Setrule(AIndex : Integer; const AValue : TDataValidationRule); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TGridRange); virtual;
  Public
  Published
    Property rule : TDataValidationRule Index 0 Read Frule Write Setrule;
    Property range : TGridRange Index 8 Read Frange Write Setrange;
  end;
  TSetDataValidationRequestClass = Class of TSetDataValidationRequest;
  
  { --------------------------------------------------------------------
    TRequest
    --------------------------------------------------------------------}
  
  TRequest = Class(TGoogleBaseObject)
  Private
    FupdateEmbeddedObjectPosition : TUpdateEmbeddedObjectPositionRequest;
    FdeleteNamedRange : TDeleteNamedRangeRequest;
    FupdateNamedRange : TUpdateNamedRangeRequest;
    FaddFilterView : TAddFilterViewRequest;
    FupdateSpreadsheetProperties : TUpdateSpreadsheetPropertiesRequest;
    FappendDimension : TAppendDimensionRequest;
    FunmergeCells : TUnmergeCellsRequest;
    FupdateProtectedRange : TUpdateProtectedRangeRequest;
    FdeleteFilterView : TDeleteFilterViewRequest;
    FclearBasicFilter : TClearBasicFilterRequest;
    FsortRange : TSortRangeRequest;
    FrepeatCell : TRepeatCellRequest;
    FsetDataValidation : TSetDataValidationRequest;
    FupdateCells : TUpdateCellsRequest;
    FaddSheet : TAddSheetRequest;
    FupdateFilterView : TUpdateFilterViewRequest;
    FupdateSheetProperties : TUpdateSheetPropertiesRequest;
    FupdateDimensionProperties : TUpdateDimensionPropertiesRequest;
    FdeleteSheet : TDeleteSheetRequest;
    FfindReplace : TFindReplaceRequest;
    FaddProtectedRange : TAddProtectedRangeRequest;
    FdeleteProtectedRange : TDeleteProtectedRangeRequest;
    FupdateConditionalFormatRule : TUpdateConditionalFormatRuleRequest;
    FsetBasicFilter : TSetBasicFilterRequest;
    FmergeCells : TMergeCellsRequest;
    FaddChart : TAddChartRequest;
    FdeleteConditionalFormatRule : TDeleteConditionalFormatRuleRequest;
    FupdateChartSpec : TUpdateChartSpecRequest;
    FdeleteDimension : TDeleteDimensionRequest;
    FdeleteEmbeddedObject : TDeleteEmbeddedObjectRequest;
    FpasteData : TPasteDataRequest;
    FaddConditionalFormatRule : TAddConditionalFormatRuleRequest;
    FupdateBorders : TUpdateBordersRequest;
    FautoResizeDimensions : TAutoResizeDimensionsRequest;
    FduplicateSheet : TDuplicateSheetRequest;
    FduplicateFilterView : TDuplicateFilterViewRequest;
    FcutPaste : TCutPasteRequest;
    FappendCells : TAppendCellsRequest;
    FaddNamedRange : TAddNamedRangeRequest;
    FautoFill : TAutoFillRequest;
    FmoveDimension : TMoveDimensionRequest;
    FtextToColumns : TTextToColumnsRequest;
    FinsertDimension : TInsertDimensionRequest;
    FcopyPaste : TCopyPasteRequest;
  Protected
    //Property setters
    Procedure SetupdateEmbeddedObjectPosition(AIndex : Integer; const AValue : TUpdateEmbeddedObjectPositionRequest); virtual;
    Procedure SetdeleteNamedRange(AIndex : Integer; const AValue : TDeleteNamedRangeRequest); virtual;
    Procedure SetupdateNamedRange(AIndex : Integer; const AValue : TUpdateNamedRangeRequest); virtual;
    Procedure SetaddFilterView(AIndex : Integer; const AValue : TAddFilterViewRequest); virtual;
    Procedure SetupdateSpreadsheetProperties(AIndex : Integer; const AValue : TUpdateSpreadsheetPropertiesRequest); virtual;
    Procedure SetappendDimension(AIndex : Integer; const AValue : TAppendDimensionRequest); virtual;
    Procedure SetunmergeCells(AIndex : Integer; const AValue : TUnmergeCellsRequest); virtual;
    Procedure SetupdateProtectedRange(AIndex : Integer; const AValue : TUpdateProtectedRangeRequest); virtual;
    Procedure SetdeleteFilterView(AIndex : Integer; const AValue : TDeleteFilterViewRequest); virtual;
    Procedure SetclearBasicFilter(AIndex : Integer; const AValue : TClearBasicFilterRequest); virtual;
    Procedure SetsortRange(AIndex : Integer; const AValue : TSortRangeRequest); virtual;
    Procedure SetrepeatCell(AIndex : Integer; const AValue : TRepeatCellRequest); virtual;
    Procedure SetsetDataValidation(AIndex : Integer; const AValue : TSetDataValidationRequest); virtual;
    Procedure SetupdateCells(AIndex : Integer; const AValue : TUpdateCellsRequest); virtual;
    Procedure SetaddSheet(AIndex : Integer; const AValue : TAddSheetRequest); virtual;
    Procedure SetupdateFilterView(AIndex : Integer; const AValue : TUpdateFilterViewRequest); virtual;
    Procedure SetupdateSheetProperties(AIndex : Integer; const AValue : TUpdateSheetPropertiesRequest); virtual;
    Procedure SetupdateDimensionProperties(AIndex : Integer; const AValue : TUpdateDimensionPropertiesRequest); virtual;
    Procedure SetdeleteSheet(AIndex : Integer; const AValue : TDeleteSheetRequest); virtual;
    Procedure SetfindReplace(AIndex : Integer; const AValue : TFindReplaceRequest); virtual;
    Procedure SetaddProtectedRange(AIndex : Integer; const AValue : TAddProtectedRangeRequest); virtual;
    Procedure SetdeleteProtectedRange(AIndex : Integer; const AValue : TDeleteProtectedRangeRequest); virtual;
    Procedure SetupdateConditionalFormatRule(AIndex : Integer; const AValue : TUpdateConditionalFormatRuleRequest); virtual;
    Procedure SetsetBasicFilter(AIndex : Integer; const AValue : TSetBasicFilterRequest); virtual;
    Procedure SetmergeCells(AIndex : Integer; const AValue : TMergeCellsRequest); virtual;
    Procedure SetaddChart(AIndex : Integer; const AValue : TAddChartRequest); virtual;
    Procedure SetdeleteConditionalFormatRule(AIndex : Integer; const AValue : TDeleteConditionalFormatRuleRequest); virtual;
    Procedure SetupdateChartSpec(AIndex : Integer; const AValue : TUpdateChartSpecRequest); virtual;
    Procedure SetdeleteDimension(AIndex : Integer; const AValue : TDeleteDimensionRequest); virtual;
    Procedure SetdeleteEmbeddedObject(AIndex : Integer; const AValue : TDeleteEmbeddedObjectRequest); virtual;
    Procedure SetpasteData(AIndex : Integer; const AValue : TPasteDataRequest); virtual;
    Procedure SetaddConditionalFormatRule(AIndex : Integer; const AValue : TAddConditionalFormatRuleRequest); virtual;
    Procedure SetupdateBorders(AIndex : Integer; const AValue : TUpdateBordersRequest); virtual;
    Procedure SetautoResizeDimensions(AIndex : Integer; const AValue : TAutoResizeDimensionsRequest); virtual;
    Procedure SetduplicateSheet(AIndex : Integer; const AValue : TDuplicateSheetRequest); virtual;
    Procedure SetduplicateFilterView(AIndex : Integer; const AValue : TDuplicateFilterViewRequest); virtual;
    Procedure SetcutPaste(AIndex : Integer; const AValue : TCutPasteRequest); virtual;
    Procedure SetappendCells(AIndex : Integer; const AValue : TAppendCellsRequest); virtual;
    Procedure SetaddNamedRange(AIndex : Integer; const AValue : TAddNamedRangeRequest); virtual;
    Procedure SetautoFill(AIndex : Integer; const AValue : TAutoFillRequest); virtual;
    Procedure SetmoveDimension(AIndex : Integer; const AValue : TMoveDimensionRequest); virtual;
    Procedure SettextToColumns(AIndex : Integer; const AValue : TTextToColumnsRequest); virtual;
    Procedure SetinsertDimension(AIndex : Integer; const AValue : TInsertDimensionRequest); virtual;
    Procedure SetcopyPaste(AIndex : Integer; const AValue : TCopyPasteRequest); virtual;
  Public
  Published
    Property updateEmbeddedObjectPosition : TUpdateEmbeddedObjectPositionRequest Index 0 Read FupdateEmbeddedObjectPosition Write SetupdateEmbeddedObjectPosition;
    Property deleteNamedRange : TDeleteNamedRangeRequest Index 8 Read FdeleteNamedRange Write SetdeleteNamedRange;
    Property updateNamedRange : TUpdateNamedRangeRequest Index 16 Read FupdateNamedRange Write SetupdateNamedRange;
    Property addFilterView : TAddFilterViewRequest Index 24 Read FaddFilterView Write SetaddFilterView;
    Property updateSpreadsheetProperties : TUpdateSpreadsheetPropertiesRequest Index 32 Read FupdateSpreadsheetProperties Write SetupdateSpreadsheetProperties;
    Property appendDimension : TAppendDimensionRequest Index 40 Read FappendDimension Write SetappendDimension;
    Property unmergeCells : TUnmergeCellsRequest Index 48 Read FunmergeCells Write SetunmergeCells;
    Property updateProtectedRange : TUpdateProtectedRangeRequest Index 56 Read FupdateProtectedRange Write SetupdateProtectedRange;
    Property deleteFilterView : TDeleteFilterViewRequest Index 64 Read FdeleteFilterView Write SetdeleteFilterView;
    Property clearBasicFilter : TClearBasicFilterRequest Index 72 Read FclearBasicFilter Write SetclearBasicFilter;
    Property sortRange : TSortRangeRequest Index 80 Read FsortRange Write SetsortRange;
    Property repeatCell : TRepeatCellRequest Index 88 Read FrepeatCell Write SetrepeatCell;
    Property setDataValidation : TSetDataValidationRequest Index 96 Read FsetDataValidation Write SetsetDataValidation;
    Property updateCells : TUpdateCellsRequest Index 104 Read FupdateCells Write SetupdateCells;
    Property addSheet : TAddSheetRequest Index 112 Read FaddSheet Write SetaddSheet;
    Property updateFilterView : TUpdateFilterViewRequest Index 120 Read FupdateFilterView Write SetupdateFilterView;
    Property updateSheetProperties : TUpdateSheetPropertiesRequest Index 128 Read FupdateSheetProperties Write SetupdateSheetProperties;
    Property updateDimensionProperties : TUpdateDimensionPropertiesRequest Index 136 Read FupdateDimensionProperties Write SetupdateDimensionProperties;
    Property deleteSheet : TDeleteSheetRequest Index 144 Read FdeleteSheet Write SetdeleteSheet;
    Property findReplace : TFindReplaceRequest Index 152 Read FfindReplace Write SetfindReplace;
    Property addProtectedRange : TAddProtectedRangeRequest Index 160 Read FaddProtectedRange Write SetaddProtectedRange;
    Property deleteProtectedRange : TDeleteProtectedRangeRequest Index 168 Read FdeleteProtectedRange Write SetdeleteProtectedRange;
    Property updateConditionalFormatRule : TUpdateConditionalFormatRuleRequest Index 176 Read FupdateConditionalFormatRule Write SetupdateConditionalFormatRule;
    Property setBasicFilter : TSetBasicFilterRequest Index 184 Read FsetBasicFilter Write SetsetBasicFilter;
    Property mergeCells : TMergeCellsRequest Index 192 Read FmergeCells Write SetmergeCells;
    Property addChart : TAddChartRequest Index 200 Read FaddChart Write SetaddChart;
    Property deleteConditionalFormatRule : TDeleteConditionalFormatRuleRequest Index 208 Read FdeleteConditionalFormatRule Write SetdeleteConditionalFormatRule;
    Property updateChartSpec : TUpdateChartSpecRequest Index 216 Read FupdateChartSpec Write SetupdateChartSpec;
    Property deleteDimension : TDeleteDimensionRequest Index 224 Read FdeleteDimension Write SetdeleteDimension;
    Property deleteEmbeddedObject : TDeleteEmbeddedObjectRequest Index 232 Read FdeleteEmbeddedObject Write SetdeleteEmbeddedObject;
    Property pasteData : TPasteDataRequest Index 240 Read FpasteData Write SetpasteData;
    Property addConditionalFormatRule : TAddConditionalFormatRuleRequest Index 248 Read FaddConditionalFormatRule Write SetaddConditionalFormatRule;
    Property updateBorders : TUpdateBordersRequest Index 256 Read FupdateBorders Write SetupdateBorders;
    Property autoResizeDimensions : TAutoResizeDimensionsRequest Index 264 Read FautoResizeDimensions Write SetautoResizeDimensions;
    Property duplicateSheet : TDuplicateSheetRequest Index 272 Read FduplicateSheet Write SetduplicateSheet;
    Property duplicateFilterView : TDuplicateFilterViewRequest Index 280 Read FduplicateFilterView Write SetduplicateFilterView;
    Property cutPaste : TCutPasteRequest Index 288 Read FcutPaste Write SetcutPaste;
    Property appendCells : TAppendCellsRequest Index 296 Read FappendCells Write SetappendCells;
    Property addNamedRange : TAddNamedRangeRequest Index 304 Read FaddNamedRange Write SetaddNamedRange;
    Property autoFill : TAutoFillRequest Index 312 Read FautoFill Write SetautoFill;
    Property moveDimension : TMoveDimensionRequest Index 320 Read FmoveDimension Write SetmoveDimension;
    Property textToColumns : TTextToColumnsRequest Index 328 Read FtextToColumns Write SettextToColumns;
    Property insertDimension : TInsertDimensionRequest Index 336 Read FinsertDimension Write SetinsertDimension;
    Property copyPaste : TCopyPasteRequest Index 344 Read FcopyPaste Write SetcopyPaste;
  end;
  TRequestClass = Class of TRequest;
  
  { --------------------------------------------------------------------
    TBatchGetValuesResponse
    --------------------------------------------------------------------}
  
  TBatchGetValuesResponse = Class(TGoogleBaseObject)
  Private
    FvalueRanges : TBatchGetValuesResponseTypevalueRangesArray;
    FspreadsheetId : String;
  Protected
    //Property setters
    Procedure SetvalueRanges(AIndex : Integer; const AValue : TBatchGetValuesResponseTypevalueRangesArray); virtual;
    Procedure SetspreadsheetId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property valueRanges : TBatchGetValuesResponseTypevalueRangesArray Index 0 Read FvalueRanges Write SetvalueRanges;
    Property spreadsheetId : String Index 8 Read FspreadsheetId Write SetspreadsheetId;
  end;
  TBatchGetValuesResponseClass = Class of TBatchGetValuesResponse;
  
  { --------------------------------------------------------------------
    TInsertDimensionRequest
    --------------------------------------------------------------------}
  
  TInsertDimensionRequest = Class(TGoogleBaseObject)
  Private
    FinheritFromBefore : boolean;
    Frange : TDimensionRange;
  Protected
    //Property setters
    Procedure SetinheritFromBefore(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TDimensionRange); virtual;
  Public
  Published
    Property inheritFromBefore : boolean Index 0 Read FinheritFromBefore Write SetinheritFromBefore;
    Property range : TDimensionRange Index 8 Read Frange Write Setrange;
  end;
  TInsertDimensionRequestClass = Class of TInsertDimensionRequest;
  
  { --------------------------------------------------------------------
    TDeleteEmbeddedObjectRequest
    --------------------------------------------------------------------}
  
  TDeleteEmbeddedObjectRequest = Class(TGoogleBaseObject)
  Private
    FobjectId : integer;
  Protected
    //Property setters
    Procedure SetobjectId(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property objectId : integer Index 0 Read FobjectId Write SetobjectId;
  end;
  TDeleteEmbeddedObjectRequestClass = Class of TDeleteEmbeddedObjectRequest;
  
  { --------------------------------------------------------------------
    TDeleteConditionalFormatRuleResponse
    --------------------------------------------------------------------}
  
  TDeleteConditionalFormatRuleResponse = Class(TGoogleBaseObject)
  Private
    Frule : TConditionalFormatRule;
  Protected
    //Property setters
    Procedure Setrule(AIndex : Integer; const AValue : TConditionalFormatRule); virtual;
  Public
  Published
    Property rule : TConditionalFormatRule Index 0 Read Frule Write Setrule;
  end;
  TDeleteConditionalFormatRuleResponseClass = Class of TDeleteConditionalFormatRuleResponse;
  
  { --------------------------------------------------------------------
    TSpreadsheetsValuesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSpreadsheetsValuesResource, method Update
  
  TSpreadsheetsValuesUpdateOptions = Record
    valueInputOption : String;
  end;
  
  
  //Optional query Options for TSpreadsheetsValuesResource, method Get
  
  TSpreadsheetsValuesGetOptions = Record
    valueRenderOption : String;
    dateTimeRenderOption : String;
    majorDimension : String;
  end;
  
  
  //Optional query Options for TSpreadsheetsValuesResource, method BatchGet
  
  TSpreadsheetsValuesBatchGetOptions = Record
    ranges : String;
    valueRenderOption : String;
    dateTimeRenderOption : String;
    majorDimension : String;
  end;
  
  TSpreadsheetsValuesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Update(spreadsheetId: string; range: string; aValueRange : TValueRange; AQuery : string  = '') : TUpdateValuesResponse;
    Function Update(spreadsheetId: string; range: string; aValueRange : TValueRange; AQuery : TSpreadsheetsValuesupdateOptions) : TUpdateValuesResponse;
    Function Get(spreadsheetId: string; range: string; AQuery : string  = '') : TValueRange;
    Function Get(spreadsheetId: string; range: string; AQuery : TSpreadsheetsValuesgetOptions) : TValueRange;
    Function BatchGet(spreadsheetId: string; AQuery : string  = '') : TBatchGetValuesResponse;
    Function BatchGet(spreadsheetId: string; AQuery : TSpreadsheetsValuesbatchGetOptions) : TBatchGetValuesResponse;
    Function BatchUpdate(spreadsheetId: string; aBatchUpdateValuesRequest : TBatchUpdateValuesRequest) : TBatchUpdateValuesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSpreadsheetsSheetsResource
    --------------------------------------------------------------------}
  
  TSpreadsheetsSheetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CopyTo(sheetId: integer; spreadsheetId: string; aCopySheetToAnotherSpreadsheetRequest : TCopySheetToAnotherSpreadsheetRequest) : TSheetProperties;
  end;
  
  
  { --------------------------------------------------------------------
    TSpreadsheetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSpreadsheetsResource, method Get
  
  TSpreadsheetsGetOptions = Record
    ranges : String;
    includeGridData : boolean;
  end;
  
  TSpreadsheetsResource = Class(TGoogleResource)
  Private
    FValuesInstance : TSpreadsheetsValuesResource;
    FSheetsInstance : TSpreadsheetsSheetsResource;
    Function GetValuesInstance : TSpreadsheetsValuesResource;virtual;
    Function GetSheetsInstance : TSpreadsheetsSheetsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aSpreadsheet : TSpreadsheet) : TSpreadsheet;overload;
    Function Get(spreadsheetId: string; AQuery : string  = '') : TSpreadsheet;
    Function Get(spreadsheetId: string; AQuery : TSpreadsheetsgetOptions) : TSpreadsheet;
    Function BatchUpdate(spreadsheetId: string; aBatchUpdateSpreadsheetRequest : TBatchUpdateSpreadsheetRequest) : TBatchUpdateSpreadsheetResponse;
    Function CreateValuesResource(AOwner : TComponent) : TSpreadsheetsValuesResource;virtual;overload;
    Function CreateValuesResource : TSpreadsheetsValuesResource;virtual;overload;
    Function CreateSheetsResource(AOwner : TComponent) : TSpreadsheetsSheetsResource;virtual;overload;
    Function CreateSheetsResource : TSpreadsheetsSheetsResource;virtual;overload;
    Property ValuesResource : TSpreadsheetsValuesResource Read GetValuesInstance;
    Property SheetsResource : TSpreadsheetsSheetsResource Read GetSheetsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TSheetsAPI
    --------------------------------------------------------------------}
  
  TSheetsAPI = Class(TGoogleAPI)
  Private
    FSpreadsheetsValuesInstance : TSpreadsheetsValuesResource;
    FSpreadsheetsSheetsInstance : TSpreadsheetsSheetsResource;
    FSpreadsheetsInstance : TSpreadsheetsResource;
    Function GetSpreadsheetsValuesInstance : TSpreadsheetsValuesResource;virtual;
    Function GetSpreadsheetsSheetsInstance : TSpreadsheetsSheetsResource;virtual;
    Function GetSpreadsheetsInstance : TSpreadsheetsResource;virtual;
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
    Function CreateSpreadsheetsValuesResource(AOwner : TComponent) : TSpreadsheetsValuesResource;virtual;overload;
    Function CreateSpreadsheetsValuesResource : TSpreadsheetsValuesResource;virtual;overload;
    Function CreateSpreadsheetsSheetsResource(AOwner : TComponent) : TSpreadsheetsSheetsResource;virtual;overload;
    Function CreateSpreadsheetsSheetsResource : TSpreadsheetsSheetsResource;virtual;overload;
    Function CreateSpreadsheetsResource(AOwner : TComponent) : TSpreadsheetsResource;virtual;overload;
    Function CreateSpreadsheetsResource : TSpreadsheetsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property SpreadsheetsValuesResource : TSpreadsheetsValuesResource Read GetSpreadsheetsValuesInstance;
    Property SpreadsheetsSheetsResource : TSpreadsheetsSheetsResource Read GetSpreadsheetsSheetsInstance;
    Property SpreadsheetsResource : TSpreadsheetsResource Read GetSpreadsheetsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAddNamedRangeResponse
  --------------------------------------------------------------------}


Procedure TAddNamedRangeResponse.SetnamedRange(AIndex : Integer; const AValue : TNamedRange); 

begin
  If (FnamedRange=AValue) then exit;
  FnamedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateProtectedRangeRequest
  --------------------------------------------------------------------}


Procedure TUpdateProtectedRangeRequest.SetprotectedRange(AIndex : Integer; const AValue : TProtectedRange); 

begin
  If (FprotectedRange=AValue) then exit;
  FprotectedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateProtectedRangeRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPadding
  --------------------------------------------------------------------}


Procedure TPadding.Setright(AIndex : Integer; const AValue : integer); 

begin
  If (Fright=AValue) then exit;
  Fright:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPadding.Setleft(AIndex : Integer; const AValue : integer); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPadding.Settop(AIndex : Integer; const AValue : integer); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPadding.Setbottom(AIndex : Integer; const AValue : integer); 

begin
  If (Fbottom=AValue) then exit;
  Fbottom:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMergeCellsRequest
  --------------------------------------------------------------------}


Procedure TMergeCellsRequest.SetmergeType(AIndex : Integer; const AValue : String); 

begin
  If (FmergeType=AValue) then exit;
  FmergeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMergeCellsRequest.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddSheetResponse
  --------------------------------------------------------------------}


Procedure TAddSheetResponse.Setproperties(AIndex : Integer; const AValue : TSheetProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPivotGroupValueMetadata
  --------------------------------------------------------------------}


Procedure TPivotGroupValueMetadata.Setvalue(AIndex : Integer; const AValue : TExtendedValue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotGroupValueMetadata.Setcollapsed(AIndex : Integer; const AValue : boolean); 

begin
  If (Fcollapsed=AValue) then exit;
  Fcollapsed:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateEmbeddedObjectPositionResponse
  --------------------------------------------------------------------}


Procedure TUpdateEmbeddedObjectPositionResponse.Setposition(AIndex : Integer; const AValue : TEmbeddedObjectPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateConditionalFormatRuleRequest
  --------------------------------------------------------------------}


Procedure TUpdateConditionalFormatRuleRequest.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateConditionalFormatRuleRequest.Setrule(AIndex : Integer; const AValue : TConditionalFormatRule); 

begin
  If (Frule=AValue) then exit;
  Frule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateConditionalFormatRuleRequest.Setindex(AIndex : Integer; const AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateConditionalFormatRuleRequest.SetnewIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FnewIndex=AValue) then exit;
  FnewIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTextFormat
  --------------------------------------------------------------------}


Procedure TTextFormat.Setbold(AIndex : Integer; const AValue : boolean); 

begin
  If (Fbold=AValue) then exit;
  Fbold:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTextFormat.Setitalic(AIndex : Integer; const AValue : boolean); 

begin
  If (Fitalic=AValue) then exit;
  Fitalic:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTextFormat.SetforegroundColor(AIndex : Integer; const AValue : TColor); 

begin
  If (FforegroundColor=AValue) then exit;
  FforegroundColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTextFormat.SetfontFamily(AIndex : Integer; const AValue : String); 

begin
  If (FfontFamily=AValue) then exit;
  FfontFamily:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTextFormat.Setstrikethrough(AIndex : Integer; const AValue : boolean); 

begin
  If (Fstrikethrough=AValue) then exit;
  Fstrikethrough:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTextFormat.SetfontSize(AIndex : Integer; const AValue : integer); 

begin
  If (FfontSize=AValue) then exit;
  FfontSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTextFormat.Setunderline(AIndex : Integer; const AValue : boolean); 

begin
  If (Funderline=AValue) then exit;
  Funderline:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateChartSpecRequest
  --------------------------------------------------------------------}


Procedure TUpdateChartSpecRequest.SetchartId(AIndex : Integer; const AValue : integer); 

begin
  If (FchartId=AValue) then exit;
  FchartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateChartSpecRequest.Setspec(AIndex : Integer; const AValue : TChartSpec); 

begin
  If (Fspec=AValue) then exit;
  Fspec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGridCoordinate
  --------------------------------------------------------------------}


Procedure TGridCoordinate.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridCoordinate.SetrowIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FrowIndex=AValue) then exit;
  FrowIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridCoordinate.SetcolumnIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FcolumnIndex=AValue) then exit;
  FcolumnIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeleteFilterViewRequest
  --------------------------------------------------------------------}


Procedure TDeleteFilterViewRequest.SetfilterId(AIndex : Integer; const AValue : integer); 

begin
  If (FfilterId=AValue) then exit;
  FfilterId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchUpdateValuesResponse
  --------------------------------------------------------------------}


Procedure TBatchUpdateValuesResponse.SettotalUpdatedSheets(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalUpdatedSheets=AValue) then exit;
  FtotalUpdatedSheets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchUpdateValuesResponse.SettotalUpdatedColumns(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalUpdatedColumns=AValue) then exit;
  FtotalUpdatedColumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchUpdateValuesResponse.Setresponses(AIndex : Integer; const AValue : TBatchUpdateValuesResponseTyperesponsesArray); 

begin
  If (Fresponses=AValue) then exit;
  Fresponses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchUpdateValuesResponse.SettotalUpdatedCells(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalUpdatedCells=AValue) then exit;
  FtotalUpdatedCells:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchUpdateValuesResponse.SetspreadsheetId(AIndex : Integer; const AValue : String); 

begin
  If (FspreadsheetId=AValue) then exit;
  FspreadsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchUpdateValuesResponse.SettotalUpdatedRows(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalUpdatedRows=AValue) then exit;
  FtotalUpdatedRows:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchUpdateValuesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'responses' : SetLength(Fresponses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUpdateNamedRangeRequest
  --------------------------------------------------------------------}


Procedure TUpdateNamedRangeRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateNamedRangeRequest.SetnamedRange(AIndex : Integer; const AValue : TNamedRange); 

begin
  If (FnamedRange=AValue) then exit;
  FnamedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateValuesResponse
  --------------------------------------------------------------------}


Procedure TUpdateValuesResponse.SetupdatedRange(AIndex : Integer; const AValue : String); 

begin
  If (FupdatedRange=AValue) then exit;
  FupdatedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateValuesResponse.SetupdatedColumns(AIndex : Integer; const AValue : integer); 

begin
  If (FupdatedColumns=AValue) then exit;
  FupdatedColumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateValuesResponse.SetspreadsheetId(AIndex : Integer; const AValue : String); 

begin
  If (FspreadsheetId=AValue) then exit;
  FspreadsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateValuesResponse.SetupdatedRows(AIndex : Integer; const AValue : integer); 

begin
  If (FupdatedRows=AValue) then exit;
  FupdatedRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateValuesResponse.SetupdatedCells(AIndex : Integer; const AValue : integer); 

begin
  If (FupdatedCells=AValue) then exit;
  FupdatedCells:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSpreadsheetProperties
  --------------------------------------------------------------------}


Procedure TSpreadsheetProperties.SettimeZone(AIndex : Integer; const AValue : String); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSpreadsheetProperties.SetautoRecalc(AIndex : Integer; const AValue : String); 

begin
  If (FautoRecalc=AValue) then exit;
  FautoRecalc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSpreadsheetProperties.Setlocale(AIndex : Integer; const AValue : String); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSpreadsheetProperties.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSpreadsheetProperties.SetdefaultFormat(AIndex : Integer; const AValue : TCellFormat); 

begin
  If (FdefaultFormat=AValue) then exit;
  FdefaultFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCellData
  --------------------------------------------------------------------}


Procedure TCellData.Sethyperlink(AIndex : Integer; const AValue : String); 

begin
  If (Fhyperlink=AValue) then exit;
  Fhyperlink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellData.SeteffectiveFormat(AIndex : Integer; const AValue : TCellFormat); 

begin
  If (FeffectiveFormat=AValue) then exit;
  FeffectiveFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellData.Setnote(AIndex : Integer; const AValue : String); 

begin
  If (Fnote=AValue) then exit;
  Fnote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellData.SetformattedValue(AIndex : Integer; const AValue : String); 

begin
  If (FformattedValue=AValue) then exit;
  FformattedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellData.SetuserEnteredValue(AIndex : Integer; const AValue : TExtendedValue); 

begin
  If (FuserEnteredValue=AValue) then exit;
  FuserEnteredValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellData.SetdataValidation(AIndex : Integer; const AValue : TDataValidationRule); 

begin
  If (FdataValidation=AValue) then exit;
  FdataValidation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellData.SetuserEnteredFormat(AIndex : Integer; const AValue : TCellFormat); 

begin
  If (FuserEnteredFormat=AValue) then exit;
  FuserEnteredFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellData.SetpivotTable(AIndex : Integer; const AValue : TPivotTable); 

begin
  If (FpivotTable=AValue) then exit;
  FpivotTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellData.SettextFormatRuns(AIndex : Integer; const AValue : TCellDataTypetextFormatRunsArray); 

begin
  If (FtextFormatRuns=AValue) then exit;
  FtextFormatRuns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellData.SeteffectiveValue(AIndex : Integer; const AValue : TExtendedValue); 

begin
  If (FeffectiveValue=AValue) then exit;
  FeffectiveValue:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCellData.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'textformatruns' : SetLength(FtextFormatRuns,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUnmergeCellsRequest
  --------------------------------------------------------------------}


Procedure TUnmergeCellsRequest.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTextToColumnsRequest
  --------------------------------------------------------------------}


Procedure TTextToColumnsRequest.Setsource(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTextToColumnsRequest.Setdelimiter(AIndex : Integer; const AValue : String); 

begin
  If (Fdelimiter=AValue) then exit;
  Fdelimiter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTextToColumnsRequest.SetdelimiterType(AIndex : Integer; const AValue : String); 

begin
  If (FdelimiterType=AValue) then exit;
  FdelimiterType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddProtectedRangeResponse
  --------------------------------------------------------------------}


Procedure TAddProtectedRangeResponse.SetprotectedRange(AIndex : Integer; const AValue : TProtectedRange); 

begin
  If (FprotectedRange=AValue) then exit;
  FprotectedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBooleanCondition
  --------------------------------------------------------------------}


Procedure TBooleanCondition.Setvalues(AIndex : Integer; const AValue : TBooleanConditionTypevaluesArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooleanCondition.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBooleanCondition.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBooleanCondition.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeleteProtectedRangeRequest
  --------------------------------------------------------------------}


Procedure TDeleteProtectedRangeRequest.SetprotectedRangeId(AIndex : Integer; const AValue : integer); 

begin
  If (FprotectedRangeId=AValue) then exit;
  FprotectedRangeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBasicChartDomain
  --------------------------------------------------------------------}


Procedure TBasicChartDomain.Setdomain(AIndex : Integer; const AValue : TChartData); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDimensionRange
  --------------------------------------------------------------------}


Procedure TDimensionRange.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionRange.SetendIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FendIndex=AValue) then exit;
  FendIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionRange.SetstartIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionRange.Setdimension(AIndex : Integer; const AValue : String); 

begin
  If (Fdimension=AValue) then exit;
  Fdimension:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResponse
  --------------------------------------------------------------------}


Procedure TResponse.SetupdateEmbeddedObjectPosition(AIndex : Integer; const AValue : TUpdateEmbeddedObjectPositionResponse); 

begin
  If (FupdateEmbeddedObjectPosition=AValue) then exit;
  FupdateEmbeddedObjectPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetaddFilterView(AIndex : Integer; const AValue : TAddFilterViewResponse); 

begin
  If (FaddFilterView=AValue) then exit;
  FaddFilterView:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetaddSheet(AIndex : Integer; const AValue : TAddSheetResponse); 

begin
  If (FaddSheet=AValue) then exit;
  FaddSheet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetfindReplace(AIndex : Integer; const AValue : TFindReplaceResponse); 

begin
  If (FfindReplace=AValue) then exit;
  FfindReplace:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetaddProtectedRange(AIndex : Integer; const AValue : TAddProtectedRangeResponse); 

begin
  If (FaddProtectedRange=AValue) then exit;
  FaddProtectedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetupdateConditionalFormatRule(AIndex : Integer; const AValue : TUpdateConditionalFormatRuleResponse); 

begin
  If (FupdateConditionalFormatRule=AValue) then exit;
  FupdateConditionalFormatRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetaddChart(AIndex : Integer; const AValue : TAddChartResponse); 

begin
  If (FaddChart=AValue) then exit;
  FaddChart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetdeleteConditionalFormatRule(AIndex : Integer; const AValue : TDeleteConditionalFormatRuleResponse); 

begin
  If (FdeleteConditionalFormatRule=AValue) then exit;
  FdeleteConditionalFormatRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetduplicateSheet(AIndex : Integer; const AValue : TDuplicateSheetResponse); 

begin
  If (FduplicateSheet=AValue) then exit;
  FduplicateSheet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetduplicateFilterView(AIndex : Integer; const AValue : TDuplicateFilterViewResponse); 

begin
  If (FduplicateFilterView=AValue) then exit;
  FduplicateFilterView:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResponse.SetaddNamedRange(AIndex : Integer; const AValue : TAddNamedRangeResponse); 

begin
  If (FaddNamedRange=AValue) then exit;
  FaddNamedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddConditionalFormatRuleRequest
  --------------------------------------------------------------------}


Procedure TAddConditionalFormatRuleRequest.Setrule(AIndex : Integer; const AValue : TConditionalFormatRule); 

begin
  If (Frule=AValue) then exit;
  Frule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddConditionalFormatRuleRequest.Setindex(AIndex : Integer; const AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterViewTypecriteria
  --------------------------------------------------------------------}


Class Function TFilterViewTypecriteria.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TFilterView
  --------------------------------------------------------------------}


Procedure TFilterView.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterView.SetnamedRangeId(AIndex : Integer; const AValue : String); 

begin
  If (FnamedRangeId=AValue) then exit;
  FnamedRangeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterView.SetsortSpecs(AIndex : Integer; const AValue : TFilterViewTypesortSpecsArray); 

begin
  If (FsortSpecs=AValue) then exit;
  FsortSpecs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterView.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterView.Setcriteria(AIndex : Integer; const AValue : TFilterViewTypecriteria); 

begin
  If (Fcriteria=AValue) then exit;
  Fcriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterView.SetfilterViewId(AIndex : Integer; const AValue : integer); 

begin
  If (FfilterViewId=AValue) then exit;
  FfilterViewId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFilterView.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sortspecs' : SetLength(FsortSpecs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSortRangeRequest
  --------------------------------------------------------------------}


Procedure TSortRangeRequest.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSortRangeRequest.SetsortSpecs(AIndex : Integer; const AValue : TSortRangeRequestTypesortSpecsArray); 

begin
  If (FsortSpecs=AValue) then exit;
  FsortSpecs:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSortRangeRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sortspecs' : SetLength(FsortSpecs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTextFormatRun
  --------------------------------------------------------------------}


Procedure TTextFormatRun.SetstartIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTextFormatRun.Setformat(AIndex : Integer; const AValue : TTextFormat); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateFilterViewRequest
  --------------------------------------------------------------------}


Procedure TUpdateFilterViewRequest.Setfilter(AIndex : Integer; const AValue : TFilterView); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateFilterViewRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateConditionalFormatRuleResponse
  --------------------------------------------------------------------}


Procedure TUpdateConditionalFormatRuleResponse.SetoldIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FoldIndex=AValue) then exit;
  FoldIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateConditionalFormatRuleResponse.SetnewRule(AIndex : Integer; const AValue : TConditionalFormatRule); 

begin
  If (FnewRule=AValue) then exit;
  FnewRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateConditionalFormatRuleResponse.SetoldRule(AIndex : Integer; const AValue : TConditionalFormatRule); 

begin
  If (FoldRule=AValue) then exit;
  FoldRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateConditionalFormatRuleResponse.SetnewIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FnewIndex=AValue) then exit;
  FnewIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilterCriteria
  --------------------------------------------------------------------}


Procedure TFilterCriteria.Setcondition(AIndex : Integer; const AValue : TBooleanCondition); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilterCriteria.SethiddenValues(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FhiddenValues=AValue) then exit;
  FhiddenValues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFilterCriteria.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'hiddenvalues' : SetLength(FhiddenValues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeleteDimensionRequest
  --------------------------------------------------------------------}


Procedure TDeleteDimensionRequest.Setrange(AIndex : Integer; const AValue : TDimensionRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPivotTableTypecriteria
  --------------------------------------------------------------------}


Class Function TPivotTableTypecriteria.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPivotTable
  --------------------------------------------------------------------}


Procedure TPivotTable.SetvalueLayout(AIndex : Integer; const AValue : String); 

begin
  If (FvalueLayout=AValue) then exit;
  FvalueLayout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotTable.Setcolumns(AIndex : Integer; const AValue : TPivotTableTypecolumnsArray); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotTable.Setsource(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotTable.Setrows(AIndex : Integer; const AValue : TPivotTableTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotTable.Setvalues(AIndex : Integer; const AValue : TPivotTableTypevaluesArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotTable.Setcriteria(AIndex : Integer; const AValue : TPivotTableTypecriteria); 

begin
  If (Fcriteria=AValue) then exit;
  Fcriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPivotTable.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'columns' : SetLength(Fcolumns,ALength);
  'rows' : SetLength(Frows,ALength);
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDataValidationRule
  --------------------------------------------------------------------}


Procedure TDataValidationRule.Setcondition(AIndex : Integer; const AValue : TBooleanCondition); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataValidationRule.SetinputMessage(AIndex : Integer; const AValue : String); 

begin
  If (FinputMessage=AValue) then exit;
  FinputMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataValidationRule.SetshowCustomUi(AIndex : Integer; const AValue : boolean); 

begin
  If (FshowCustomUi=AValue) then exit;
  FshowCustomUi:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataValidationRule.Setstrict(AIndex : Integer; const AValue : boolean); 

begin
  If (Fstrict=AValue) then exit;
  Fstrict:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateSpreadsheetPropertiesRequest
  --------------------------------------------------------------------}


Procedure TUpdateSpreadsheetPropertiesRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateSpreadsheetPropertiesRequest.Setproperties(AIndex : Integer; const AValue : TSpreadsheetProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChartSourceRange
  --------------------------------------------------------------------}


Procedure TChartSourceRange.Setsources(AIndex : Integer; const AValue : TChartSourceRangeTypesourcesArray); 

begin
  If (Fsources=AValue) then exit;
  Fsources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TChartSourceRange.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sources' : SetLength(Fsources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBatchUpdateValuesRequest
  --------------------------------------------------------------------}


Procedure TBatchUpdateValuesRequest.SetvalueInputOption(AIndex : Integer; const AValue : String); 

begin
  If (FvalueInputOption=AValue) then exit;
  FvalueInputOption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchUpdateValuesRequest.Setdata(AIndex : Integer; const AValue : TBatchUpdateValuesRequestTypedataArray); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchUpdateValuesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'data' : SetLength(Fdata,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TClearBasicFilterRequest
  --------------------------------------------------------------------}


Procedure TClearBasicFilterRequest.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConditionalFormatRule
  --------------------------------------------------------------------}


Procedure TConditionalFormatRule.SetgradientRule(AIndex : Integer; const AValue : TGradientRule); 

begin
  If (FgradientRule=AValue) then exit;
  FgradientRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConditionalFormatRule.SetbooleanRule(AIndex : Integer; const AValue : TBooleanRule); 

begin
  If (FbooleanRule=AValue) then exit;
  FbooleanRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConditionalFormatRule.Setranges(AIndex : Integer; const AValue : TConditionalFormatRuleTyperangesArray); 

begin
  If (Franges=AValue) then exit;
  Franges:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TConditionalFormatRule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'ranges' : SetLength(Franges,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUpdateBordersRequest
  --------------------------------------------------------------------}


Procedure TUpdateBordersRequest.Setright(AIndex : Integer; const AValue : TBorder); 

begin
  If (Fright=AValue) then exit;
  Fright:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateBordersRequest.SetinnerVertical(AIndex : Integer; const AValue : TBorder); 

begin
  If (FinnerVertical=AValue) then exit;
  FinnerVertical:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateBordersRequest.Settop(AIndex : Integer; const AValue : TBorder); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateBordersRequest.SetinnerHorizontal(AIndex : Integer; const AValue : TBorder); 

begin
  If (FinnerHorizontal=AValue) then exit;
  FinnerHorizontal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateBordersRequest.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateBordersRequest.Setbottom(AIndex : Integer; const AValue : TBorder); 

begin
  If (Fbottom=AValue) then exit;
  Fbottom:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateBordersRequest.Setleft(AIndex : Integer; const AValue : TBorder); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPivotFilterCriteria
  --------------------------------------------------------------------}


Procedure TPivotFilterCriteria.SetvisibleValues(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FvisibleValues=AValue) then exit;
  FvisibleValues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPivotFilterCriteria.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'visiblevalues' : SetLength(FvisibleValues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBorders
  --------------------------------------------------------------------}


Procedure TBorders.Setleft(AIndex : Integer; const AValue : TBorder); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBorders.Setright(AIndex : Integer; const AValue : TBorder); 

begin
  If (Fright=AValue) then exit;
  Fright:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBorders.Settop(AIndex : Integer; const AValue : TBorder); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBorders.Setbottom(AIndex : Integer; const AValue : TBorder); 

begin
  If (Fbottom=AValue) then exit;
  Fbottom:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmbeddedChart
  --------------------------------------------------------------------}


Procedure TEmbeddedChart.SetchartId(AIndex : Integer; const AValue : integer); 

begin
  If (FchartId=AValue) then exit;
  FchartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmbeddedChart.Setposition(AIndex : Integer; const AValue : TEmbeddedObjectPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmbeddedChart.Setspec(AIndex : Integer; const AValue : TChartSpec); 

begin
  If (Fspec=AValue) then exit;
  Fspec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColor
  --------------------------------------------------------------------}


Procedure TColor.Setgreen(AIndex : Integer; const AValue : integer); 

begin
  If (Fgreen=AValue) then exit;
  Fgreen:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColor.Setblue(AIndex : Integer; const AValue : integer); 

begin
  If (Fblue=AValue) then exit;
  Fblue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColor.Setred(AIndex : Integer; const AValue : integer); 

begin
  If (Fred=AValue) then exit;
  Fred:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColor.Setalpha(AIndex : Integer; const AValue : integer); 

begin
  If (Falpha=AValue) then exit;
  Falpha:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddSheetRequest
  --------------------------------------------------------------------}


Procedure TAddSheetRequest.Setproperties(AIndex : Integer; const AValue : TSheetProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddProtectedRangeRequest
  --------------------------------------------------------------------}


Procedure TAddProtectedRangeRequest.SetprotectedRange(AIndex : Integer; const AValue : TProtectedRange); 

begin
  If (FprotectedRange=AValue) then exit;
  FprotectedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TValueRange
  --------------------------------------------------------------------}


Procedure TValueRange.Setvalues(AIndex : Integer; const AValue : TValueRangeTypevaluesArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValueRange.Setrange(AIndex : Integer; const AValue : String); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValueRange.SetmajorDimension(AIndex : Integer; const AValue : String); 

begin
  If (FmajorDimension=AValue) then exit;
  FmajorDimension:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TValueRange.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFindReplaceResponse
  --------------------------------------------------------------------}


Procedure TFindReplaceResponse.SetvaluesChanged(AIndex : Integer; const AValue : integer); 

begin
  If (FvaluesChanged=AValue) then exit;
  FvaluesChanged:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceResponse.SetrowsChanged(AIndex : Integer; const AValue : integer); 

begin
  If (FrowsChanged=AValue) then exit;
  FrowsChanged:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceResponse.SetoccurrencesChanged(AIndex : Integer; const AValue : integer); 

begin
  If (FoccurrencesChanged=AValue) then exit;
  FoccurrencesChanged:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceResponse.SetformulasChanged(AIndex : Integer; const AValue : integer); 

begin
  If (FformulasChanged=AValue) then exit;
  FformulasChanged:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceResponse.SetsheetsChanged(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetsChanged=AValue) then exit;
  FsheetsChanged:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCellFormat
  --------------------------------------------------------------------}


Procedure TCellFormat.SethorizontalAlignment(AIndex : Integer; const AValue : String); 

begin
  If (FhorizontalAlignment=AValue) then exit;
  FhorizontalAlignment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellFormat.SethyperlinkDisplayType(AIndex : Integer; const AValue : String); 

begin
  If (FhyperlinkDisplayType=AValue) then exit;
  FhyperlinkDisplayType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellFormat.Setborders(AIndex : Integer; const AValue : TBorders); 

begin
  If (Fborders=AValue) then exit;
  Fborders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellFormat.SettextDirection(AIndex : Integer; const AValue : String); 

begin
  If (FtextDirection=AValue) then exit;
  FtextDirection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellFormat.SettextFormat(AIndex : Integer; const AValue : TTextFormat); 

begin
  If (FtextFormat=AValue) then exit;
  FtextFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellFormat.Setpadding(AIndex : Integer; const AValue : TPadding); 

begin
  If (Fpadding=AValue) then exit;
  Fpadding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellFormat.SetnumberFormat(AIndex : Integer; const AValue : TNumberFormat); 

begin
  If (FnumberFormat=AValue) then exit;
  FnumberFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellFormat.SetwrapStrategy(AIndex : Integer; const AValue : String); 

begin
  If (FwrapStrategy=AValue) then exit;
  FwrapStrategy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellFormat.SetbackgroundColor(AIndex : Integer; const AValue : TColor); 

begin
  If (FbackgroundColor=AValue) then exit;
  FbackgroundColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCellFormat.SetverticalAlignment(AIndex : Integer; const AValue : String); 

begin
  If (FverticalAlignment=AValue) then exit;
  FverticalAlignment:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMoveDimensionRequest
  --------------------------------------------------------------------}


Procedure TMoveDimensionRequest.Setsource(AIndex : Integer; const AValue : TDimensionRange); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoveDimensionRequest.SetdestinationIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FdestinationIndex=AValue) then exit;
  FdestinationIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBasicChartAxis
  --------------------------------------------------------------------}


Procedure TBasicChartAxis.Setposition(AIndex : Integer; const AValue : String); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicChartAxis.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicChartAxis.Setformat(AIndex : Integer; const AValue : TTextFormat); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPivotGroupSortValueBucket
  --------------------------------------------------------------------}


Procedure TPivotGroupSortValueBucket.Setbuckets(AIndex : Integer; const AValue : TPivotGroupSortValueBucketTypebucketsArray); 

begin
  If (Fbuckets=AValue) then exit;
  Fbuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotGroupSortValueBucket.SetvaluesIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FvaluesIndex=AValue) then exit;
  FvaluesIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPivotGroupSortValueBucket.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'buckets' : SetLength(Fbuckets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDimensionProperties
  --------------------------------------------------------------------}


Procedure TDimensionProperties.SethiddenByUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FhiddenByUser=AValue) then exit;
  FhiddenByUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionProperties.SetpixelSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpixelSize=AValue) then exit;
  FpixelSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDimensionProperties.SethiddenByFilter(AIndex : Integer; const AValue : boolean); 

begin
  If (FhiddenByFilter=AValue) then exit;
  FhiddenByFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmbeddedObjectPosition
  --------------------------------------------------------------------}


Procedure TEmbeddedObjectPosition.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmbeddedObjectPosition.SetoverlayPosition(AIndex : Integer; const AValue : TOverlayPosition); 

begin
  If (FoverlayPosition=AValue) then exit;
  FoverlayPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmbeddedObjectPosition.SetnewSheet(AIndex : Integer; const AValue : boolean); 

begin
  If (FnewSheet=AValue) then exit;
  FnewSheet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInterpolationPoint
  --------------------------------------------------------------------}


Procedure TInterpolationPoint.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInterpolationPoint.Setcolor(AIndex : Integer; const AValue : TColor); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInterpolationPoint.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TInterpolationPoint.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TErrorValue
  --------------------------------------------------------------------}


Procedure TErrorValue.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorValue.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TErrorValue.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDuplicateFilterViewRequest
  --------------------------------------------------------------------}


Procedure TDuplicateFilterViewRequest.SetfilterId(AIndex : Integer; const AValue : integer); 

begin
  If (FfilterId=AValue) then exit;
  FfilterId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchUpdateSpreadsheetRequest
  --------------------------------------------------------------------}


Procedure TBatchUpdateSpreadsheetRequest.Setrequests(AIndex : Integer; const AValue : TBatchUpdateSpreadsheetRequestTyperequestsArray); 

begin
  If (Frequests=AValue) then exit;
  Frequests:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchUpdateSpreadsheetRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'requests' : SetLength(Frequests,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSheetProperties
  --------------------------------------------------------------------}


Procedure TSheetProperties.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheetProperties.Setindex(AIndex : Integer; const AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheetProperties.Sethidden(AIndex : Integer; const AValue : boolean); 

begin
  If (Fhidden=AValue) then exit;
  Fhidden:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheetProperties.SetgridProperties(AIndex : Integer; const AValue : TGridProperties); 

begin
  If (FgridProperties=AValue) then exit;
  FgridProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheetProperties.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheetProperties.SetrightToLeft(AIndex : Integer; const AValue : boolean); 

begin
  If (FrightToLeft=AValue) then exit;
  FrightToLeft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheetProperties.SettabColor(AIndex : Integer; const AValue : TColor); 

begin
  If (FtabColor=AValue) then exit;
  FtabColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheetProperties.SetsheetType(AIndex : Integer; const AValue : String); 

begin
  If (FsheetType=AValue) then exit;
  FsheetType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProtectedRange
  --------------------------------------------------------------------}


Procedure TProtectedRange.SetunprotectedRanges(AIndex : Integer; const AValue : TProtectedRangeTypeunprotectedRangesArray); 

begin
  If (FunprotectedRanges=AValue) then exit;
  FunprotectedRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProtectedRange.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProtectedRange.SetnamedRangeId(AIndex : Integer; const AValue : String); 

begin
  If (FnamedRangeId=AValue) then exit;
  FnamedRangeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProtectedRange.SetrequestingUserCanEdit(AIndex : Integer; const AValue : boolean); 

begin
  If (FrequestingUserCanEdit=AValue) then exit;
  FrequestingUserCanEdit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProtectedRange.Seteditors(AIndex : Integer; const AValue : TEditors); 

begin
  If (Feditors=AValue) then exit;
  Feditors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProtectedRange.SetprotectedRangeId(AIndex : Integer; const AValue : integer); 

begin
  If (FprotectedRangeId=AValue) then exit;
  FprotectedRangeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProtectedRange.SetwarningOnly(AIndex : Integer; const AValue : boolean); 

begin
  If (FwarningOnly=AValue) then exit;
  FwarningOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProtectedRange.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProtectedRange.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'unprotectedranges' : SetLength(FunprotectedRanges,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeleteConditionalFormatRuleRequest
  --------------------------------------------------------------------}


Procedure TDeleteConditionalFormatRuleRequest.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeleteConditionalFormatRuleRequest.Setindex(AIndex : Integer; const AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChartSpec
  --------------------------------------------------------------------}


Procedure TChartSpec.SethiddenDimensionStrategy(AIndex : Integer; const AValue : String); 

begin
  If (FhiddenDimensionStrategy=AValue) then exit;
  FhiddenDimensionStrategy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChartSpec.SetbasicChart(AIndex : Integer; const AValue : TBasicChartSpec); 

begin
  If (FbasicChart=AValue) then exit;
  FbasicChart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChartSpec.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChartSpec.SetpieChart(AIndex : Integer; const AValue : TPieChartSpec); 

begin
  If (FpieChart=AValue) then exit;
  FpieChart:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceAndDestination
  --------------------------------------------------------------------}


Procedure TSourceAndDestination.Setsource(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceAndDestination.SetfillLength(AIndex : Integer; const AValue : integer); 

begin
  If (FfillLength=AValue) then exit;
  FfillLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceAndDestination.Setdimension(AIndex : Integer; const AValue : String); 

begin
  If (Fdimension=AValue) then exit;
  Fdimension:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConditionValue
  --------------------------------------------------------------------}


Procedure TConditionValue.SetrelativeDate(AIndex : Integer; const AValue : String); 

begin
  If (FrelativeDate=AValue) then exit;
  FrelativeDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConditionValue.SetuserEnteredValue(AIndex : Integer; const AValue : String); 

begin
  If (FuserEnteredValue=AValue) then exit;
  FuserEnteredValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPasteDataRequest
  --------------------------------------------------------------------}


Procedure TPasteDataRequest.Setdata(AIndex : Integer; const AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPasteDataRequest.Setcoordinate(AIndex : Integer; const AValue : TGridCoordinate); 

begin
  If (Fcoordinate=AValue) then exit;
  Fcoordinate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPasteDataRequest.Setdelimiter(AIndex : Integer; const AValue : String); 

begin
  If (Fdelimiter=AValue) then exit;
  Fdelimiter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPasteDataRequest.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPasteDataRequest.Sethtml(AIndex : Integer; const AValue : boolean); 

begin
  If (Fhtml=AValue) then exit;
  Fhtml:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPasteDataRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFindReplaceRequest
  --------------------------------------------------------------------}


Procedure TFindReplaceRequest.Setfind(AIndex : Integer; const AValue : String); 

begin
  If (Ffind=AValue) then exit;
  Ffind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceRequest.Setreplacement(AIndex : Integer; const AValue : String); 

begin
  If (Freplacement=AValue) then exit;
  Freplacement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceRequest.SetsearchByRegex(AIndex : Integer; const AValue : boolean); 

begin
  If (FsearchByRegex=AValue) then exit;
  FsearchByRegex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceRequest.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceRequest.SetallSheets(AIndex : Integer; const AValue : boolean); 

begin
  If (FallSheets=AValue) then exit;
  FallSheets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceRequest.SetmatchCase(AIndex : Integer; const AValue : boolean); 

begin
  If (FmatchCase=AValue) then exit;
  FmatchCase:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceRequest.SetincludeFormulas(AIndex : Integer; const AValue : boolean); 

begin
  If (FincludeFormulas=AValue) then exit;
  FincludeFormulas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceRequest.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindReplaceRequest.SetmatchEntireCell(AIndex : Integer; const AValue : boolean); 

begin
  If (FmatchEntireCell=AValue) then exit;
  FmatchEntireCell:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSortSpec
  --------------------------------------------------------------------}


Procedure TSortSpec.SetsortOrder(AIndex : Integer; const AValue : String); 

begin
  If (FsortOrder=AValue) then exit;
  FsortOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSortSpec.SetdimensionIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FdimensionIndex=AValue) then exit;
  FdimensionIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCopySheetToAnotherSpreadsheetRequest
  --------------------------------------------------------------------}


Procedure TCopySheetToAnotherSpreadsheetRequest.SetdestinationSpreadsheetId(AIndex : Integer; const AValue : String); 

begin
  If (FdestinationSpreadsheetId=AValue) then exit;
  FdestinationSpreadsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNumberFormat
  --------------------------------------------------------------------}


Procedure TNumberFormat.Setpattern(AIndex : Integer; const AValue : String); 

begin
  If (Fpattern=AValue) then exit;
  Fpattern:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNumberFormat.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TNumberFormat.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TUpdateDimensionPropertiesRequest
  --------------------------------------------------------------------}


Procedure TUpdateDimensionPropertiesRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateDimensionPropertiesRequest.Setrange(AIndex : Integer; const AValue : TDimensionRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateDimensionPropertiesRequest.Setproperties(AIndex : Integer; const AValue : TDimensionProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEditors
  --------------------------------------------------------------------}


Procedure TEditors.Setusers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEditors.Setgroups(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fgroups=AValue) then exit;
  Fgroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEditors.SetdomainUsersCanEdit(AIndex : Integer; const AValue : boolean); 

begin
  If (FdomainUsersCanEdit=AValue) then exit;
  FdomainUsersCanEdit:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEditors.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'users' : SetLength(Fusers,ALength);
  'groups' : SetLength(Fgroups,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSpreadsheet
  --------------------------------------------------------------------}


Procedure TSpreadsheet.SetspreadsheetId(AIndex : Integer; const AValue : String); 

begin
  If (FspreadsheetId=AValue) then exit;
  FspreadsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSpreadsheet.Setproperties(AIndex : Integer; const AValue : TSpreadsheetProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSpreadsheet.Setsheets(AIndex : Integer; const AValue : TSpreadsheetTypesheetsArray); 

begin
  If (Fsheets=AValue) then exit;
  Fsheets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSpreadsheet.SetnamedRanges(AIndex : Integer; const AValue : TSpreadsheetTypenamedRangesArray); 

begin
  If (FnamedRanges=AValue) then exit;
  FnamedRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSpreadsheet.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sheets' : SetLength(Fsheets,ALength);
  'namedranges' : SetLength(FnamedRanges,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGridData
  --------------------------------------------------------------------}


Procedure TGridData.SetcolumnMetadata(AIndex : Integer; const AValue : TGridDataTypecolumnMetadataArray); 

begin
  If (FcolumnMetadata=AValue) then exit;
  FcolumnMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridData.SetrowData(AIndex : Integer; const AValue : TGridDataTyperowDataArray); 

begin
  If (FrowData=AValue) then exit;
  FrowData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridData.SetstartRow(AIndex : Integer; const AValue : integer); 

begin
  If (FstartRow=AValue) then exit;
  FstartRow:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridData.SetrowMetadata(AIndex : Integer; const AValue : TGridDataTyperowMetadataArray); 

begin
  If (FrowMetadata=AValue) then exit;
  FrowMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridData.SetstartColumn(AIndex : Integer; const AValue : integer); 

begin
  If (FstartColumn=AValue) then exit;
  FstartColumn:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGridData.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'columnmetadata' : SetLength(FcolumnMetadata,ALength);
  'rowdata' : SetLength(FrowData,ALength);
  'rowmetadata' : SetLength(FrowMetadata,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPivotValue
  --------------------------------------------------------------------}


Procedure TPivotValue.Setformula(AIndex : Integer; const AValue : String); 

begin
  If (Fformula=AValue) then exit;
  Fformula:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotValue.SetsourceColumnOffset(AIndex : Integer; const AValue : integer); 

begin
  If (FsourceColumnOffset=AValue) then exit;
  FsourceColumnOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotValue.SetsummarizeFunction(AIndex : Integer; const AValue : String); 

begin
  If (FsummarizeFunction=AValue) then exit;
  FsummarizeFunction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotValue.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBasicFilterTypecriteria
  --------------------------------------------------------------------}


Class Function TBasicFilterTypecriteria.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TBasicFilter
  --------------------------------------------------------------------}


Procedure TBasicFilter.Setcriteria(AIndex : Integer; const AValue : TBasicFilterTypecriteria); 

begin
  If (Fcriteria=AValue) then exit;
  Fcriteria:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicFilter.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicFilter.SetsortSpecs(AIndex : Integer; const AValue : TBasicFilterTypesortSpecsArray); 

begin
  If (FsortSpecs=AValue) then exit;
  FsortSpecs:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBasicFilter.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sortspecs' : SetLength(FsortSpecs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDuplicateSheetRequest
  --------------------------------------------------------------------}


Procedure TDuplicateSheetRequest.SetsourceSheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsourceSheetId=AValue) then exit;
  FsourceSheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDuplicateSheetRequest.SetnewSheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FnewSheetId=AValue) then exit;
  FnewSheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDuplicateSheetRequest.SetinsertSheetIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FinsertSheetIndex=AValue) then exit;
  FinsertSheetIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDuplicateSheetRequest.SetnewSheetName(AIndex : Integer; const AValue : String); 

begin
  If (FnewSheetName=AValue) then exit;
  FnewSheetName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddFilterViewResponse
  --------------------------------------------------------------------}


Procedure TAddFilterViewResponse.Setfilter(AIndex : Integer; const AValue : TFilterView); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDuplicateSheetResponse
  --------------------------------------------------------------------}


Procedure TDuplicateSheetResponse.Setproperties(AIndex : Integer; const AValue : TSheetProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBorder
  --------------------------------------------------------------------}


Procedure TBorder.Setstyle(AIndex : Integer; const AValue : String); 

begin
  If (Fstyle=AValue) then exit;
  Fstyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBorder.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBorder.Setcolor(AIndex : Integer; const AValue : TColor); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddNamedRangeRequest
  --------------------------------------------------------------------}


Procedure TAddNamedRangeRequest.SetnamedRange(AIndex : Integer; const AValue : TNamedRange); 

begin
  If (FnamedRange=AValue) then exit;
  FnamedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddChartResponse
  --------------------------------------------------------------------}


Procedure TAddChartResponse.Setchart(AIndex : Integer; const AValue : TEmbeddedChart); 

begin
  If (Fchart=AValue) then exit;
  Fchart:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppendCellsRequest
  --------------------------------------------------------------------}


Procedure TAppendCellsRequest.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppendCellsRequest.Setrows(AIndex : Integer; const AValue : TAppendCellsRequestTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppendCellsRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAppendCellsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRowData
  --------------------------------------------------------------------}


Procedure TRowData.Setvalues(AIndex : Integer; const AValue : TRowDataTypevaluesArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRowData.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBasicChartSeries
  --------------------------------------------------------------------}


Procedure TBasicChartSeries.Setseries(AIndex : Integer; const AValue : TChartData); 

begin
  If (Fseries=AValue) then exit;
  Fseries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicChartSeries.SettargetAxis(AIndex : Integer; const AValue : String); 

begin
  If (FtargetAxis=AValue) then exit;
  FtargetAxis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicChartSeries.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBasicChartSeries.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRepeatCellRequest
  --------------------------------------------------------------------}


Procedure TRepeatCellRequest.Setcell(AIndex : Integer; const AValue : TCellData); 

begin
  If (Fcell=AValue) then exit;
  Fcell:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRepeatCellRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRepeatCellRequest.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBasicChartSpec
  --------------------------------------------------------------------}


Procedure TBasicChartSpec.SetchartType(AIndex : Integer; const AValue : String); 

begin
  If (FchartType=AValue) then exit;
  FchartType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicChartSpec.Setdomains(AIndex : Integer; const AValue : TBasicChartSpecTypedomainsArray); 

begin
  If (Fdomains=AValue) then exit;
  Fdomains:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicChartSpec.SetheaderCount(AIndex : Integer; const AValue : integer); 

begin
  If (FheaderCount=AValue) then exit;
  FheaderCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicChartSpec.Setseries(AIndex : Integer; const AValue : TBasicChartSpecTypeseriesArray); 

begin
  If (Fseries=AValue) then exit;
  Fseries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicChartSpec.SetlegendPosition(AIndex : Integer; const AValue : String); 

begin
  If (FlegendPosition=AValue) then exit;
  FlegendPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicChartSpec.Setaxis(AIndex : Integer; const AValue : TBasicChartSpecTypeaxisArray); 

begin
  If (Faxis=AValue) then exit;
  Faxis:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBasicChartSpec.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'domains' : SetLength(Fdomains,ALength);
  'series' : SetLength(Fseries,ALength);
  'axis' : SetLength(Faxis,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TNamedRange
  --------------------------------------------------------------------}


Procedure TNamedRange.SetnamedRangeId(AIndex : Integer; const AValue : String); 

begin
  If (FnamedRangeId=AValue) then exit;
  FnamedRangeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNamedRange.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNamedRange.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetBasicFilterRequest
  --------------------------------------------------------------------}


Procedure TSetBasicFilterRequest.Setfilter(AIndex : Integer; const AValue : TBasicFilter); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateEmbeddedObjectPositionRequest
  --------------------------------------------------------------------}


Procedure TUpdateEmbeddedObjectPositionRequest.SetnewPosition(AIndex : Integer; const AValue : TEmbeddedObjectPosition); 

begin
  If (FnewPosition=AValue) then exit;
  FnewPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateEmbeddedObjectPositionRequest.SetobjectId(AIndex : Integer; const AValue : integer); 

begin
  If (FobjectId=AValue) then exit;
  FobjectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateEmbeddedObjectPositionRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoResizeDimensionsRequest
  --------------------------------------------------------------------}


Procedure TAutoResizeDimensionsRequest.Setdimensions(AIndex : Integer; const AValue : TDimensionRange); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDuplicateFilterViewResponse
  --------------------------------------------------------------------}


Procedure TDuplicateFilterViewResponse.Setfilter(AIndex : Integer; const AValue : TFilterView); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPivotGroup
  --------------------------------------------------------------------}


Procedure TPivotGroup.SetsortOrder(AIndex : Integer; const AValue : String); 

begin
  If (FsortOrder=AValue) then exit;
  FsortOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotGroup.SetsourceColumnOffset(AIndex : Integer; const AValue : integer); 

begin
  If (FsourceColumnOffset=AValue) then exit;
  FsourceColumnOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotGroup.SetshowTotals(AIndex : Integer; const AValue : boolean); 

begin
  If (FshowTotals=AValue) then exit;
  FshowTotals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotGroup.SetvalueBucket(AIndex : Integer; const AValue : TPivotGroupSortValueBucket); 

begin
  If (FvalueBucket=AValue) then exit;
  FvalueBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPivotGroup.SetvalueMetadata(AIndex : Integer; const AValue : TPivotGroupTypevalueMetadataArray); 

begin
  If (FvalueMetadata=AValue) then exit;
  FvalueMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPivotGroup.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'valuemetadata' : SetLength(FvalueMetadata,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGridRange
  --------------------------------------------------------------------}


Procedure TGridRange.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridRange.SetstartColumnIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FstartColumnIndex=AValue) then exit;
  FstartColumnIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridRange.SetstartRowIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FstartRowIndex=AValue) then exit;
  FstartRowIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridRange.SetendRowIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FendRowIndex=AValue) then exit;
  FendRowIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridRange.SetendColumnIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FendColumnIndex=AValue) then exit;
  FendColumnIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeleteSheetRequest
  --------------------------------------------------------------------}


Procedure TDeleteSheetRequest.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChartData
  --------------------------------------------------------------------}


Procedure TChartData.SetsourceRange(AIndex : Integer; const AValue : TChartSourceRange); 

begin
  If (FsourceRange=AValue) then exit;
  FsourceRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSheet
  --------------------------------------------------------------------}


Procedure TSheet.Setproperties(AIndex : Integer; const AValue : TSheetProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheet.Setcharts(AIndex : Integer; const AValue : TSheetTypechartsArray); 

begin
  If (Fcharts=AValue) then exit;
  Fcharts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheet.SetfilterViews(AIndex : Integer; const AValue : TSheetTypefilterViewsArray); 

begin
  If (FfilterViews=AValue) then exit;
  FfilterViews:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheet.SetconditionalFormats(AIndex : Integer; const AValue : TSheetTypeconditionalFormatsArray); 

begin
  If (FconditionalFormats=AValue) then exit;
  FconditionalFormats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheet.SetprotectedRanges(AIndex : Integer; const AValue : TSheetTypeprotectedRangesArray); 

begin
  If (FprotectedRanges=AValue) then exit;
  FprotectedRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheet.SetbasicFilter(AIndex : Integer; const AValue : TBasicFilter); 

begin
  If (FbasicFilter=AValue) then exit;
  FbasicFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheet.Setmerges(AIndex : Integer; const AValue : TSheetTypemergesArray); 

begin
  If (Fmerges=AValue) then exit;
  Fmerges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSheet.Setdata(AIndex : Integer; const AValue : TSheetTypedataArray); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSheet.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'charts' : SetLength(Fcharts,ALength);
  'filterviews' : SetLength(FfilterViews,ALength);
  'conditionalformats' : SetLength(FconditionalFormats,ALength);
  'protectedranges' : SetLength(FprotectedRanges,ALength);
  'merges' : SetLength(Fmerges,ALength);
  'data' : SetLength(Fdata,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCopyPasteRequest
  --------------------------------------------------------------------}


Procedure TCopyPasteRequest.SetpasteType(AIndex : Integer; const AValue : String); 

begin
  If (FpasteType=AValue) then exit;
  FpasteType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCopyPasteRequest.SetpasteOrientation(AIndex : Integer; const AValue : String); 

begin
  If (FpasteOrientation=AValue) then exit;
  FpasteOrientation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCopyPasteRequest.Setsource(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCopyPasteRequest.Setdestination(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateCellsRequest
  --------------------------------------------------------------------}


Procedure TUpdateCellsRequest.Setrows(AIndex : Integer; const AValue : TUpdateCellsRequestTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateCellsRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateCellsRequest.Setstart(AIndex : Integer; const AValue : TGridCoordinate); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateCellsRequest.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUpdateCellsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExtendedValue
  --------------------------------------------------------------------}


Procedure TExtendedValue.SetformulaValue(AIndex : Integer; const AValue : String); 

begin
  If (FformulaValue=AValue) then exit;
  FformulaValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExtendedValue.SeterrorValue(AIndex : Integer; const AValue : TErrorValue); 

begin
  If (FerrorValue=AValue) then exit;
  FerrorValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExtendedValue.SetboolValue(AIndex : Integer; const AValue : boolean); 

begin
  If (FboolValue=AValue) then exit;
  FboolValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExtendedValue.SetnumberValue(AIndex : Integer; const AValue : double); 

begin
  If (FnumberValue=AValue) then exit;
  FnumberValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExtendedValue.SetstringValue(AIndex : Integer; const AValue : String); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchUpdateSpreadsheetResponse
  --------------------------------------------------------------------}


Procedure TBatchUpdateSpreadsheetResponse.SetspreadsheetId(AIndex : Integer; const AValue : String); 

begin
  If (FspreadsheetId=AValue) then exit;
  FspreadsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchUpdateSpreadsheetResponse.Setreplies(AIndex : Integer; const AValue : TBatchUpdateSpreadsheetResponseTyperepliesArray); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchUpdateSpreadsheetResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'replies' : SetLength(Freplies,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGradientRule
  --------------------------------------------------------------------}


Procedure TGradientRule.Setmaxpoint(AIndex : Integer; const AValue : TInterpolationPoint); 

begin
  If (Fmaxpoint=AValue) then exit;
  Fmaxpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGradientRule.Setmidpoint(AIndex : Integer; const AValue : TInterpolationPoint); 

begin
  If (Fmidpoint=AValue) then exit;
  Fmidpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGradientRule.Setminpoint(AIndex : Integer; const AValue : TInterpolationPoint); 

begin
  If (Fminpoint=AValue) then exit;
  Fminpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCutPasteRequest
  --------------------------------------------------------------------}


Procedure TCutPasteRequest.SetpasteType(AIndex : Integer; const AValue : String); 

begin
  If (FpasteType=AValue) then exit;
  FpasteType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCutPasteRequest.Setsource(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCutPasteRequest.Setdestination(AIndex : Integer; const AValue : TGridCoordinate); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOverlayPosition
  --------------------------------------------------------------------}


Procedure TOverlayPosition.SetwidthPixels(AIndex : Integer; const AValue : integer); 

begin
  If (FwidthPixels=AValue) then exit;
  FwidthPixels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOverlayPosition.SetanchorCell(AIndex : Integer; const AValue : TGridCoordinate); 

begin
  If (FanchorCell=AValue) then exit;
  FanchorCell:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOverlayPosition.SetoffsetXPixels(AIndex : Integer; const AValue : integer); 

begin
  If (FoffsetXPixels=AValue) then exit;
  FoffsetXPixels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOverlayPosition.SetheightPixels(AIndex : Integer; const AValue : integer); 

begin
  If (FheightPixels=AValue) then exit;
  FheightPixels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOverlayPosition.SetoffsetYPixels(AIndex : Integer; const AValue : integer); 

begin
  If (FoffsetYPixels=AValue) then exit;
  FoffsetYPixels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoFillRequest
  --------------------------------------------------------------------}


Procedure TAutoFillRequest.SetuseAlternateSeries(AIndex : Integer; const AValue : boolean); 

begin
  If (FuseAlternateSeries=AValue) then exit;
  FuseAlternateSeries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoFillRequest.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoFillRequest.SetsourceAndDestination(AIndex : Integer; const AValue : TSourceAndDestination); 

begin
  If (FsourceAndDestination=AValue) then exit;
  FsourceAndDestination:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPieChartSpec
  --------------------------------------------------------------------}


Procedure TPieChartSpec.SetlegendPosition(AIndex : Integer; const AValue : String); 

begin
  If (FlegendPosition=AValue) then exit;
  FlegendPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPieChartSpec.Setseries(AIndex : Integer; const AValue : TChartData); 

begin
  If (Fseries=AValue) then exit;
  Fseries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPieChartSpec.SetpieHole(AIndex : Integer; const AValue : double); 

begin
  If (FpieHole=AValue) then exit;
  FpieHole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPieChartSpec.SetthreeDimensional(AIndex : Integer; const AValue : boolean); 

begin
  If (FthreeDimensional=AValue) then exit;
  FthreeDimensional:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPieChartSpec.Setdomain(AIndex : Integer; const AValue : TChartData); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateSheetPropertiesRequest
  --------------------------------------------------------------------}


Procedure TUpdateSheetPropertiesRequest.Setfields(AIndex : Integer; const AValue : String); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUpdateSheetPropertiesRequest.Setproperties(AIndex : Integer; const AValue : TSheetProperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBooleanRule
  --------------------------------------------------------------------}


Procedure TBooleanRule.Setcondition(AIndex : Integer; const AValue : TBooleanCondition); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBooleanRule.Setformat(AIndex : Integer; const AValue : TCellFormat); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppendDimensionRequest
  --------------------------------------------------------------------}


Procedure TAppendDimensionRequest.SetsheetId(AIndex : Integer; const AValue : integer); 

begin
  If (FsheetId=AValue) then exit;
  FsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppendDimensionRequest.Set_length(AIndex : Integer; const AValue : integer); 

begin
  If (F_length=AValue) then exit;
  F_length:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppendDimensionRequest.Setdimension(AIndex : Integer; const AValue : String); 

begin
  If (Fdimension=AValue) then exit;
  Fdimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAppendDimensionRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_length' : Result:='length';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAddFilterViewRequest
  --------------------------------------------------------------------}


Procedure TAddFilterViewRequest.Setfilter(AIndex : Integer; const AValue : TFilterView); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGridProperties
  --------------------------------------------------------------------}


Procedure TGridProperties.SetrowCount(AIndex : Integer; const AValue : integer); 

begin
  If (FrowCount=AValue) then exit;
  FrowCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridProperties.SetcolumnCount(AIndex : Integer; const AValue : integer); 

begin
  If (FcolumnCount=AValue) then exit;
  FcolumnCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridProperties.SetfrozenRowCount(AIndex : Integer; const AValue : integer); 

begin
  If (FfrozenRowCount=AValue) then exit;
  FfrozenRowCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridProperties.SetfrozenColumnCount(AIndex : Integer; const AValue : integer); 

begin
  If (FfrozenColumnCount=AValue) then exit;
  FfrozenColumnCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGridProperties.SethideGridlines(AIndex : Integer; const AValue : boolean); 

begin
  If (FhideGridlines=AValue) then exit;
  FhideGridlines:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeleteNamedRangeRequest
  --------------------------------------------------------------------}


Procedure TDeleteNamedRangeRequest.SetnamedRangeId(AIndex : Integer; const AValue : String); 

begin
  If (FnamedRangeId=AValue) then exit;
  FnamedRangeId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddChartRequest
  --------------------------------------------------------------------}


Procedure TAddChartRequest.Setchart(AIndex : Integer; const AValue : TEmbeddedChart); 

begin
  If (Fchart=AValue) then exit;
  Fchart:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetDataValidationRequest
  --------------------------------------------------------------------}


Procedure TSetDataValidationRequest.Setrule(AIndex : Integer; const AValue : TDataValidationRule); 

begin
  If (Frule=AValue) then exit;
  Frule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetDataValidationRequest.Setrange(AIndex : Integer; const AValue : TGridRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRequest
  --------------------------------------------------------------------}


Procedure TRequest.SetupdateEmbeddedObjectPosition(AIndex : Integer; const AValue : TUpdateEmbeddedObjectPositionRequest); 

begin
  If (FupdateEmbeddedObjectPosition=AValue) then exit;
  FupdateEmbeddedObjectPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetdeleteNamedRange(AIndex : Integer; const AValue : TDeleteNamedRangeRequest); 

begin
  If (FdeleteNamedRange=AValue) then exit;
  FdeleteNamedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateNamedRange(AIndex : Integer; const AValue : TUpdateNamedRangeRequest); 

begin
  If (FupdateNamedRange=AValue) then exit;
  FupdateNamedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetaddFilterView(AIndex : Integer; const AValue : TAddFilterViewRequest); 

begin
  If (FaddFilterView=AValue) then exit;
  FaddFilterView:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateSpreadsheetProperties(AIndex : Integer; const AValue : TUpdateSpreadsheetPropertiesRequest); 

begin
  If (FupdateSpreadsheetProperties=AValue) then exit;
  FupdateSpreadsheetProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetappendDimension(AIndex : Integer; const AValue : TAppendDimensionRequest); 

begin
  If (FappendDimension=AValue) then exit;
  FappendDimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetunmergeCells(AIndex : Integer; const AValue : TUnmergeCellsRequest); 

begin
  If (FunmergeCells=AValue) then exit;
  FunmergeCells:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateProtectedRange(AIndex : Integer; const AValue : TUpdateProtectedRangeRequest); 

begin
  If (FupdateProtectedRange=AValue) then exit;
  FupdateProtectedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetdeleteFilterView(AIndex : Integer; const AValue : TDeleteFilterViewRequest); 

begin
  If (FdeleteFilterView=AValue) then exit;
  FdeleteFilterView:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetclearBasicFilter(AIndex : Integer; const AValue : TClearBasicFilterRequest); 

begin
  If (FclearBasicFilter=AValue) then exit;
  FclearBasicFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetsortRange(AIndex : Integer; const AValue : TSortRangeRequest); 

begin
  If (FsortRange=AValue) then exit;
  FsortRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetrepeatCell(AIndex : Integer; const AValue : TRepeatCellRequest); 

begin
  If (FrepeatCell=AValue) then exit;
  FrepeatCell:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetsetDataValidation(AIndex : Integer; const AValue : TSetDataValidationRequest); 

begin
  If (FsetDataValidation=AValue) then exit;
  FsetDataValidation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateCells(AIndex : Integer; const AValue : TUpdateCellsRequest); 

begin
  If (FupdateCells=AValue) then exit;
  FupdateCells:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetaddSheet(AIndex : Integer; const AValue : TAddSheetRequest); 

begin
  If (FaddSheet=AValue) then exit;
  FaddSheet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateFilterView(AIndex : Integer; const AValue : TUpdateFilterViewRequest); 

begin
  If (FupdateFilterView=AValue) then exit;
  FupdateFilterView:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateSheetProperties(AIndex : Integer; const AValue : TUpdateSheetPropertiesRequest); 

begin
  If (FupdateSheetProperties=AValue) then exit;
  FupdateSheetProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateDimensionProperties(AIndex : Integer; const AValue : TUpdateDimensionPropertiesRequest); 

begin
  If (FupdateDimensionProperties=AValue) then exit;
  FupdateDimensionProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetdeleteSheet(AIndex : Integer; const AValue : TDeleteSheetRequest); 

begin
  If (FdeleteSheet=AValue) then exit;
  FdeleteSheet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetfindReplace(AIndex : Integer; const AValue : TFindReplaceRequest); 

begin
  If (FfindReplace=AValue) then exit;
  FfindReplace:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetaddProtectedRange(AIndex : Integer; const AValue : TAddProtectedRangeRequest); 

begin
  If (FaddProtectedRange=AValue) then exit;
  FaddProtectedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetdeleteProtectedRange(AIndex : Integer; const AValue : TDeleteProtectedRangeRequest); 

begin
  If (FdeleteProtectedRange=AValue) then exit;
  FdeleteProtectedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateConditionalFormatRule(AIndex : Integer; const AValue : TUpdateConditionalFormatRuleRequest); 

begin
  If (FupdateConditionalFormatRule=AValue) then exit;
  FupdateConditionalFormatRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetsetBasicFilter(AIndex : Integer; const AValue : TSetBasicFilterRequest); 

begin
  If (FsetBasicFilter=AValue) then exit;
  FsetBasicFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetmergeCells(AIndex : Integer; const AValue : TMergeCellsRequest); 

begin
  If (FmergeCells=AValue) then exit;
  FmergeCells:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetaddChart(AIndex : Integer; const AValue : TAddChartRequest); 

begin
  If (FaddChart=AValue) then exit;
  FaddChart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetdeleteConditionalFormatRule(AIndex : Integer; const AValue : TDeleteConditionalFormatRuleRequest); 

begin
  If (FdeleteConditionalFormatRule=AValue) then exit;
  FdeleteConditionalFormatRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateChartSpec(AIndex : Integer; const AValue : TUpdateChartSpecRequest); 

begin
  If (FupdateChartSpec=AValue) then exit;
  FupdateChartSpec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetdeleteDimension(AIndex : Integer; const AValue : TDeleteDimensionRequest); 

begin
  If (FdeleteDimension=AValue) then exit;
  FdeleteDimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetdeleteEmbeddedObject(AIndex : Integer; const AValue : TDeleteEmbeddedObjectRequest); 

begin
  If (FdeleteEmbeddedObject=AValue) then exit;
  FdeleteEmbeddedObject:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetpasteData(AIndex : Integer; const AValue : TPasteDataRequest); 

begin
  If (FpasteData=AValue) then exit;
  FpasteData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetaddConditionalFormatRule(AIndex : Integer; const AValue : TAddConditionalFormatRuleRequest); 

begin
  If (FaddConditionalFormatRule=AValue) then exit;
  FaddConditionalFormatRule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetupdateBorders(AIndex : Integer; const AValue : TUpdateBordersRequest); 

begin
  If (FupdateBorders=AValue) then exit;
  FupdateBorders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetautoResizeDimensions(AIndex : Integer; const AValue : TAutoResizeDimensionsRequest); 

begin
  If (FautoResizeDimensions=AValue) then exit;
  FautoResizeDimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetduplicateSheet(AIndex : Integer; const AValue : TDuplicateSheetRequest); 

begin
  If (FduplicateSheet=AValue) then exit;
  FduplicateSheet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetduplicateFilterView(AIndex : Integer; const AValue : TDuplicateFilterViewRequest); 

begin
  If (FduplicateFilterView=AValue) then exit;
  FduplicateFilterView:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetcutPaste(AIndex : Integer; const AValue : TCutPasteRequest); 

begin
  If (FcutPaste=AValue) then exit;
  FcutPaste:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetappendCells(AIndex : Integer; const AValue : TAppendCellsRequest); 

begin
  If (FappendCells=AValue) then exit;
  FappendCells:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetaddNamedRange(AIndex : Integer; const AValue : TAddNamedRangeRequest); 

begin
  If (FaddNamedRange=AValue) then exit;
  FaddNamedRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetautoFill(AIndex : Integer; const AValue : TAutoFillRequest); 

begin
  If (FautoFill=AValue) then exit;
  FautoFill:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetmoveDimension(AIndex : Integer; const AValue : TMoveDimensionRequest); 

begin
  If (FmoveDimension=AValue) then exit;
  FmoveDimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SettextToColumns(AIndex : Integer; const AValue : TTextToColumnsRequest); 

begin
  If (FtextToColumns=AValue) then exit;
  FtextToColumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetinsertDimension(AIndex : Integer; const AValue : TInsertDimensionRequest); 

begin
  If (FinsertDimension=AValue) then exit;
  FinsertDimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequest.SetcopyPaste(AIndex : Integer; const AValue : TCopyPasteRequest); 

begin
  If (FcopyPaste=AValue) then exit;
  FcopyPaste:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchGetValuesResponse
  --------------------------------------------------------------------}


Procedure TBatchGetValuesResponse.SetvalueRanges(AIndex : Integer; const AValue : TBatchGetValuesResponseTypevalueRangesArray); 

begin
  If (FvalueRanges=AValue) then exit;
  FvalueRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchGetValuesResponse.SetspreadsheetId(AIndex : Integer; const AValue : String); 

begin
  If (FspreadsheetId=AValue) then exit;
  FspreadsheetId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchGetValuesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'valueranges' : SetLength(FvalueRanges,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInsertDimensionRequest
  --------------------------------------------------------------------}


Procedure TInsertDimensionRequest.SetinheritFromBefore(AIndex : Integer; const AValue : boolean); 

begin
  If (FinheritFromBefore=AValue) then exit;
  FinheritFromBefore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInsertDimensionRequest.Setrange(AIndex : Integer; const AValue : TDimensionRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeleteEmbeddedObjectRequest
  --------------------------------------------------------------------}


Procedure TDeleteEmbeddedObjectRequest.SetobjectId(AIndex : Integer; const AValue : integer); 

begin
  If (FobjectId=AValue) then exit;
  FobjectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeleteConditionalFormatRuleResponse
  --------------------------------------------------------------------}


Procedure TDeleteConditionalFormatRuleResponse.Setrule(AIndex : Integer; const AValue : TConditionalFormatRule); 

begin
  If (Frule=AValue) then exit;
  Frule:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSpreadsheetsValuesResource
  --------------------------------------------------------------------}


Class Function TSpreadsheetsValuesResource.ResourceName : String;

begin
  Result:='values';
end;

Class Function TSpreadsheetsValuesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsheetsAPI;
end;

Function TSpreadsheetsValuesResource.Update(spreadsheetId: string; range: string; aValueRange : TValueRange; AQuery : string = '') : TUpdateValuesResponse;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v4/spreadsheets/{spreadsheetId}/values/{range}';
  _Methodid   = 'sheets.spreadsheets.values.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['spreadsheetId',spreadsheetId,'range',range]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aValueRange,TUpdateValuesResponse) as TUpdateValuesResponse;
end;


Function TSpreadsheetsValuesResource.Update(spreadsheetId: string; range: string; aValueRange : TValueRange; AQuery : TSpreadsheetsValuesupdateOptions) : TUpdateValuesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'valueInputOption',AQuery.valueInputOption);
  Result:=Update(spreadsheetId,range,aValueRange,_Q);
end;

Function TSpreadsheetsValuesResource.Get(spreadsheetId: string; range: string; AQuery : string = '') : TValueRange;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v4/spreadsheets/{spreadsheetId}/values/{range}';
  _Methodid   = 'sheets.spreadsheets.values.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['spreadsheetId',spreadsheetId,'range',range]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TValueRange) as TValueRange;
end;


Function TSpreadsheetsValuesResource.Get(spreadsheetId: string; range: string; AQuery : TSpreadsheetsValuesgetOptions) : TValueRange;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'valueRenderOption',AQuery.valueRenderOption);
  AddToQuery(_Q,'dateTimeRenderOption',AQuery.dateTimeRenderOption);
  AddToQuery(_Q,'majorDimension',AQuery.majorDimension);
  Result:=Get(spreadsheetId,range,_Q);
end;

Function TSpreadsheetsValuesResource.BatchGet(spreadsheetId: string; AQuery : string = '') : TBatchGetValuesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v4/spreadsheets/{spreadsheetId}/values:batchGet';
  _Methodid   = 'sheets.spreadsheets.values.batchGet';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['spreadsheetId',spreadsheetId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBatchGetValuesResponse) as TBatchGetValuesResponse;
end;


Function TSpreadsheetsValuesResource.BatchGet(spreadsheetId: string; AQuery : TSpreadsheetsValuesbatchGetOptions) : TBatchGetValuesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ranges',AQuery.ranges);
  AddToQuery(_Q,'valueRenderOption',AQuery.valueRenderOption);
  AddToQuery(_Q,'dateTimeRenderOption',AQuery.dateTimeRenderOption);
  AddToQuery(_Q,'majorDimension',AQuery.majorDimension);
  Result:=BatchGet(spreadsheetId,_Q);
end;

Function TSpreadsheetsValuesResource.BatchUpdate(spreadsheetId: string; aBatchUpdateValuesRequest : TBatchUpdateValuesRequest) : TBatchUpdateValuesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v4/spreadsheets/{spreadsheetId}/values:batchUpdate';
  _Methodid   = 'sheets.spreadsheets.values.batchUpdate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['spreadsheetId',spreadsheetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBatchUpdateValuesRequest,TBatchUpdateValuesResponse) as TBatchUpdateValuesResponse;
end;



{ --------------------------------------------------------------------
  TSpreadsheetsSheetsResource
  --------------------------------------------------------------------}


Class Function TSpreadsheetsSheetsResource.ResourceName : String;

begin
  Result:='sheets';
end;

Class Function TSpreadsheetsSheetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsheetsAPI;
end;

Function TSpreadsheetsSheetsResource.CopyTo(sheetId: integer; spreadsheetId: string; aCopySheetToAnotherSpreadsheetRequest : TCopySheetToAnotherSpreadsheetRequest) : TSheetProperties;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v4/spreadsheets/{spreadsheetId}/sheets/{sheetId}:copyTo';
  _Methodid   = 'sheets.spreadsheets.sheets.copyTo';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['sheetId',sheetId,'spreadsheetId',spreadsheetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCopySheetToAnotherSpreadsheetRequest,TSheetProperties) as TSheetProperties;
end;



{ --------------------------------------------------------------------
  TSpreadsheetsResource
  --------------------------------------------------------------------}


Class Function TSpreadsheetsResource.ResourceName : String;

begin
  Result:='spreadsheets';
end;

Class Function TSpreadsheetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsheetsAPI;
end;

Function TSpreadsheetsResource.Create(aSpreadsheet : TSpreadsheet) : TSpreadsheet;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v4/spreadsheets';
  _Methodid   = 'sheets.spreadsheets.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSpreadsheet,TSpreadsheet) as TSpreadsheet;
end;

Function TSpreadsheetsResource.Get(spreadsheetId: string; AQuery : string = '') : TSpreadsheet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v4/spreadsheets/{spreadsheetId}';
  _Methodid   = 'sheets.spreadsheets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['spreadsheetId',spreadsheetId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSpreadsheet) as TSpreadsheet;
end;


Function TSpreadsheetsResource.Get(spreadsheetId: string; AQuery : TSpreadsheetsgetOptions) : TSpreadsheet;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ranges',AQuery.ranges);
  AddToQuery(_Q,'includeGridData',AQuery.includeGridData);
  Result:=Get(spreadsheetId,_Q);
end;

Function TSpreadsheetsResource.BatchUpdate(spreadsheetId: string; aBatchUpdateSpreadsheetRequest : TBatchUpdateSpreadsheetRequest) : TBatchUpdateSpreadsheetResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v4/spreadsheets/{spreadsheetId}:batchUpdate';
  _Methodid   = 'sheets.spreadsheets.batchUpdate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['spreadsheetId',spreadsheetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBatchUpdateSpreadsheetRequest,TBatchUpdateSpreadsheetResponse) as TBatchUpdateSpreadsheetResponse;
end;



Function TSpreadsheetsResource.GetValuesInstance : TSpreadsheetsValuesResource;

begin
  if (FValuesInstance=Nil) then
    FValuesInstance:=CreateValuesResource;
  Result:=FValuesInstance;
end;

Function TSpreadsheetsResource.CreateValuesResource : TSpreadsheetsValuesResource;

begin
  Result:=CreateValuesResource(Self);
end;


Function TSpreadsheetsResource.CreateValuesResource(AOwner : TComponent) : TSpreadsheetsValuesResource;

begin
  Result:=TSpreadsheetsValuesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TSpreadsheetsResource.GetSheetsInstance : TSpreadsheetsSheetsResource;

begin
  if (FSheetsInstance=Nil) then
    FSheetsInstance:=CreateSheetsResource;
  Result:=FSheetsInstance;
end;

Function TSpreadsheetsResource.CreateSheetsResource : TSpreadsheetsSheetsResource;

begin
  Result:=CreateSheetsResource(Self);
end;


Function TSpreadsheetsResource.CreateSheetsResource(AOwner : TComponent) : TSpreadsheetsSheetsResource;

begin
  Result:=TSpreadsheetsSheetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TSheetsAPI
  --------------------------------------------------------------------}

Class Function TSheetsAPI.APIName : String;

begin
  Result:='sheets';
end;

Class Function TSheetsAPI.APIVersion : String;

begin
  Result:='v4';
end;

Class Function TSheetsAPI.APIRevision : String;

begin
  Result:='20160519';
end;

Class Function TSheetsAPI.APIID : String;

begin
  Result:='sheets:v4';
end;

Class Function TSheetsAPI.APITitle : String;

begin
  Result:='Google Sheets API';
end;

Class Function TSheetsAPI.APIDescription : String;

begin
  Result:='Reads and writes Google Sheets.';
end;

Class Function TSheetsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TSheetsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TSheetsAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TSheetsAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TSheetsAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/sheets/';
end;

Class Function TSheetsAPI.APIrootUrl : string;

begin
  Result:='https://sheets.googleapis.com/';
end;

Class Function TSheetsAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TSheetsAPI.APIbaseURL : String;

begin
  Result:='https://sheets.googleapis.com/';
end;

Class Function TSheetsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TSheetsAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TSheetsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TSheetsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/drive.readonly';
  Result[0].Description:='View the files in your Google Drive';
  Result[1].Name:='https://www.googleapis.com/auth/spreadsheets.readonly';
  Result[1].Description:='View your Google Spreadsheets';
  Result[2].Name:='https://www.googleapis.com/auth/drive';
  Result[2].Description:='View and manage the files in your Google Drive';
  Result[3].Name:='https://www.googleapis.com/auth/spreadsheets';
  Result[3].Description:='View and manage your spreadsheets in Google Drive';
  
end;

Class Function TSheetsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TSheetsAPI.RegisterAPIResources;

begin
  TAddNamedRangeResponse.RegisterObject;
  TUpdateProtectedRangeRequest.RegisterObject;
  TPadding.RegisterObject;
  TMergeCellsRequest.RegisterObject;
  TAddSheetResponse.RegisterObject;
  TPivotGroupValueMetadata.RegisterObject;
  TUpdateEmbeddedObjectPositionResponse.RegisterObject;
  TUpdateConditionalFormatRuleRequest.RegisterObject;
  TTextFormat.RegisterObject;
  TUpdateChartSpecRequest.RegisterObject;
  TGridCoordinate.RegisterObject;
  TDeleteFilterViewRequest.RegisterObject;
  TBatchUpdateValuesResponse.RegisterObject;
  TUpdateNamedRangeRequest.RegisterObject;
  TUpdateValuesResponse.RegisterObject;
  TSpreadsheetProperties.RegisterObject;
  TCellData.RegisterObject;
  TUnmergeCellsRequest.RegisterObject;
  TTextToColumnsRequest.RegisterObject;
  TAddProtectedRangeResponse.RegisterObject;
  TBooleanCondition.RegisterObject;
  TDeleteProtectedRangeRequest.RegisterObject;
  TBasicChartDomain.RegisterObject;
  TDimensionRange.RegisterObject;
  TResponse.RegisterObject;
  TAddConditionalFormatRuleRequest.RegisterObject;
  TFilterViewTypecriteria.RegisterObject;
  TFilterView.RegisterObject;
  TSortRangeRequest.RegisterObject;
  TTextFormatRun.RegisterObject;
  TUpdateFilterViewRequest.RegisterObject;
  TUpdateConditionalFormatRuleResponse.RegisterObject;
  TFilterCriteria.RegisterObject;
  TDeleteDimensionRequest.RegisterObject;
  TPivotTableTypecriteria.RegisterObject;
  TPivotTable.RegisterObject;
  TDataValidationRule.RegisterObject;
  TUpdateSpreadsheetPropertiesRequest.RegisterObject;
  TChartSourceRange.RegisterObject;
  TBatchUpdateValuesRequest.RegisterObject;
  TClearBasicFilterRequest.RegisterObject;
  TConditionalFormatRule.RegisterObject;
  TUpdateBordersRequest.RegisterObject;
  TPivotFilterCriteria.RegisterObject;
  TBorders.RegisterObject;
  TEmbeddedChart.RegisterObject;
  TColor.RegisterObject;
  TAddSheetRequest.RegisterObject;
  TAddProtectedRangeRequest.RegisterObject;
  TValueRange.RegisterObject;
  TFindReplaceResponse.RegisterObject;
  TCellFormat.RegisterObject;
  TMoveDimensionRequest.RegisterObject;
  TBasicChartAxis.RegisterObject;
  TPivotGroupSortValueBucket.RegisterObject;
  TDimensionProperties.RegisterObject;
  TEmbeddedObjectPosition.RegisterObject;
  TInterpolationPoint.RegisterObject;
  TErrorValue.RegisterObject;
  TDuplicateFilterViewRequest.RegisterObject;
  TBatchUpdateSpreadsheetRequest.RegisterObject;
  TSheetProperties.RegisterObject;
  TProtectedRange.RegisterObject;
  TDeleteConditionalFormatRuleRequest.RegisterObject;
  TChartSpec.RegisterObject;
  TSourceAndDestination.RegisterObject;
  TConditionValue.RegisterObject;
  TPasteDataRequest.RegisterObject;
  TFindReplaceRequest.RegisterObject;
  TSortSpec.RegisterObject;
  TCopySheetToAnotherSpreadsheetRequest.RegisterObject;
  TNumberFormat.RegisterObject;
  TUpdateDimensionPropertiesRequest.RegisterObject;
  TEditors.RegisterObject;
  TSpreadsheet.RegisterObject;
  TGridData.RegisterObject;
  TPivotValue.RegisterObject;
  TBasicFilterTypecriteria.RegisterObject;
  TBasicFilter.RegisterObject;
  TDuplicateSheetRequest.RegisterObject;
  TAddFilterViewResponse.RegisterObject;
  TDuplicateSheetResponse.RegisterObject;
  TBorder.RegisterObject;
  TAddNamedRangeRequest.RegisterObject;
  TAddChartResponse.RegisterObject;
  TAppendCellsRequest.RegisterObject;
  TRowData.RegisterObject;
  TBasicChartSeries.RegisterObject;
  TRepeatCellRequest.RegisterObject;
  TBasicChartSpec.RegisterObject;
  TNamedRange.RegisterObject;
  TSetBasicFilterRequest.RegisterObject;
  TUpdateEmbeddedObjectPositionRequest.RegisterObject;
  TAutoResizeDimensionsRequest.RegisterObject;
  TDuplicateFilterViewResponse.RegisterObject;
  TPivotGroup.RegisterObject;
  TGridRange.RegisterObject;
  TDeleteSheetRequest.RegisterObject;
  TChartData.RegisterObject;
  TSheet.RegisterObject;
  TCopyPasteRequest.RegisterObject;
  TUpdateCellsRequest.RegisterObject;
  TExtendedValue.RegisterObject;
  TBatchUpdateSpreadsheetResponse.RegisterObject;
  TGradientRule.RegisterObject;
  TCutPasteRequest.RegisterObject;
  TOverlayPosition.RegisterObject;
  TAutoFillRequest.RegisterObject;
  TPieChartSpec.RegisterObject;
  TUpdateSheetPropertiesRequest.RegisterObject;
  TBooleanRule.RegisterObject;
  TAppendDimensionRequest.RegisterObject;
  TAddFilterViewRequest.RegisterObject;
  TGridProperties.RegisterObject;
  TDeleteNamedRangeRequest.RegisterObject;
  TAddChartRequest.RegisterObject;
  TSetDataValidationRequest.RegisterObject;
  TRequest.RegisterObject;
  TBatchGetValuesResponse.RegisterObject;
  TInsertDimensionRequest.RegisterObject;
  TDeleteEmbeddedObjectRequest.RegisterObject;
  TDeleteConditionalFormatRuleResponse.RegisterObject;
end;


Function TSheetsAPI.GetSpreadsheetsValuesInstance : TSpreadsheetsValuesResource;

begin
  if (FSpreadsheetsValuesInstance=Nil) then
    FSpreadsheetsValuesInstance:=CreateSpreadsheetsValuesResource;
  Result:=FSpreadsheetsValuesInstance;
end;

Function TSheetsAPI.CreateSpreadsheetsValuesResource : TSpreadsheetsValuesResource;

begin
  Result:=CreateSpreadsheetsValuesResource(Self);
end;


Function TSheetsAPI.CreateSpreadsheetsValuesResource(AOwner : TComponent) : TSpreadsheetsValuesResource;

begin
  Result:=TSpreadsheetsValuesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TSheetsAPI.GetSpreadsheetsSheetsInstance : TSpreadsheetsSheetsResource;

begin
  if (FSpreadsheetsSheetsInstance=Nil) then
    FSpreadsheetsSheetsInstance:=CreateSpreadsheetsSheetsResource;
  Result:=FSpreadsheetsSheetsInstance;
end;

Function TSheetsAPI.CreateSpreadsheetsSheetsResource : TSpreadsheetsSheetsResource;

begin
  Result:=CreateSpreadsheetsSheetsResource(Self);
end;


Function TSheetsAPI.CreateSpreadsheetsSheetsResource(AOwner : TComponent) : TSpreadsheetsSheetsResource;

begin
  Result:=TSpreadsheetsSheetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TSheetsAPI.GetSpreadsheetsInstance : TSpreadsheetsResource;

begin
  if (FSpreadsheetsInstance=Nil) then
    FSpreadsheetsInstance:=CreateSpreadsheetsResource;
  Result:=FSpreadsheetsInstance;
end;

Function TSheetsAPI.CreateSpreadsheetsResource : TSpreadsheetsResource;

begin
  Result:=CreateSpreadsheetsResource(Self);
end;


Function TSheetsAPI.CreateSpreadsheetsResource(AOwner : TComponent) : TSpreadsheetsResource;

begin
  Result:=TSpreadsheetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TSheetsAPI.RegisterAPI;
end.
