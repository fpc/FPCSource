unit googlebigquery;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TJsonValue = TJSONSchema;
  TBigtableColumn = Class;
  TBigtableColumnFamily = Class;
  TBigtableOptions = Class;
  TCsvOptions = Class;
  TDataset = Class;
  TDatasetList = Class;
  TDatasetReference = Class;
  TErrorProto = Class;
  TExplainQueryStage = Class;
  TExplainQueryStep = Class;
  TExternalDataConfiguration = Class;
  TGetQueryResultsResponse = Class;
  TGoogleSheetsOptions = Class;
  TJob = Class;
  TJobCancelResponse = Class;
  TJobConfiguration = Class;
  TJobConfigurationExtract = Class;
  TJobConfigurationLoad = Class;
  TJobConfigurationQuery = Class;
  TJobConfigurationTableCopy = Class;
  TJobList = Class;
  TJobReference = Class;
  TJobStatistics = Class;
  TJobStatistics2 = Class;
  TJobStatistics3 = Class;
  TJobStatistics4 = Class;
  TJobStatus = Class;
  TJsonObject = Class;
  TProjectList = Class;
  TProjectReference = Class;
  TQueryRequest = Class;
  TQueryResponse = Class;
  TStreamingbuffer = Class;
  TTable = Class;
  TTableCell = Class;
  TTableDataInsertAllRequest = Class;
  TTableDataInsertAllResponse = Class;
  TTableDataList = Class;
  TTableFieldSchema = Class;
  TTableList = Class;
  TTableReference = Class;
  TTableRow = Class;
  TTableSchema = Class;
  TTimePartitioning = Class;
  TUserDefinedFunctionResource = Class;
  TViewDefinition = Class;
  TBigtableColumnArray = Array of TBigtableColumn;
  TBigtableColumnFamilyArray = Array of TBigtableColumnFamily;
  TBigtableOptionsArray = Array of TBigtableOptions;
  TCsvOptionsArray = Array of TCsvOptions;
  TDatasetArray = Array of TDataset;
  TDatasetListArray = Array of TDatasetList;
  TDatasetReferenceArray = Array of TDatasetReference;
  TErrorProtoArray = Array of TErrorProto;
  TExplainQueryStageArray = Array of TExplainQueryStage;
  TExplainQueryStepArray = Array of TExplainQueryStep;
  TExternalDataConfigurationArray = Array of TExternalDataConfiguration;
  TGetQueryResultsResponseArray = Array of TGetQueryResultsResponse;
  TGoogleSheetsOptionsArray = Array of TGoogleSheetsOptions;
  TJobArray = Array of TJob;
  TJobCancelResponseArray = Array of TJobCancelResponse;
  TJobConfigurationArray = Array of TJobConfiguration;
  TJobConfigurationExtractArray = Array of TJobConfigurationExtract;
  TJobConfigurationLoadArray = Array of TJobConfigurationLoad;
  TJobConfigurationQueryArray = Array of TJobConfigurationQuery;
  TJobConfigurationTableCopyArray = Array of TJobConfigurationTableCopy;
  TJobListArray = Array of TJobList;
  TJobReferenceArray = Array of TJobReference;
  TJobStatisticsArray = Array of TJobStatistics;
  TJobStatistics2Array = Array of TJobStatistics2;
  TJobStatistics3Array = Array of TJobStatistics3;
  TJobStatistics4Array = Array of TJobStatistics4;
  TJobStatusArray = Array of TJobStatus;
  TJsonObjectArray = Array of TJsonObject;
  TProjectListArray = Array of TProjectList;
  TProjectReferenceArray = Array of TProjectReference;
  TQueryRequestArray = Array of TQueryRequest;
  TQueryResponseArray = Array of TQueryResponse;
  TStreamingbufferArray = Array of TStreamingbuffer;
  TTableArray = Array of TTable;
  TTableCellArray = Array of TTableCell;
  TTableDataInsertAllRequestArray = Array of TTableDataInsertAllRequest;
  TTableDataInsertAllResponseArray = Array of TTableDataInsertAllResponse;
  TTableDataListArray = Array of TTableDataList;
  TTableFieldSchemaArray = Array of TTableFieldSchema;
  TTableListArray = Array of TTableList;
  TTableReferenceArray = Array of TTableReference;
  TTableRowArray = Array of TTableRow;
  TTableSchemaArray = Array of TTableSchema;
  TTimePartitioningArray = Array of TTimePartitioning;
  TUserDefinedFunctionResourceArray = Array of TUserDefinedFunctionResource;
  TViewDefinitionArray = Array of TViewDefinition;
  //Anonymous types, using auto-generated names
  TDatasetTypeaccessItem = Class;
  TDatasetListTypedatasetsItem = Class;
  TJobConfigurationQueryTypetableDefinitions = Class;
  TJobListTypejobsItem = Class;
  TProjectListTypeprojectsItem = Class;
  TTableDataInsertAllRequestTyperowsItem = Class;
  TTableDataInsertAllResponseTypeinsertErrorsItem = Class;
  TTableListTypetablesItem = Class;
  TBigtableColumnFamilyTypecolumnsArray = Array of TBigtableColumn;
  TBigtableOptionsTypecolumnFamiliesArray = Array of TBigtableColumnFamily;
  TDatasetTypeaccessArray = Array of TDatasetTypeaccessItem;
  TDatasetListTypedatasetsArray = Array of TDatasetListTypedatasetsItem;
  TExplainQueryStageTypestepsArray = Array of TExplainQueryStep;
  TGetQueryResultsResponseTypeerrorsArray = Array of TErrorProto;
  TGetQueryResultsResponseTyperowsArray = Array of TTableRow;
  TJobConfigurationQueryTypeuserDefinedFunctionResourcesArray = Array of TUserDefinedFunctionResource;
  TJobConfigurationTableCopyTypesourceTablesArray = Array of TTableReference;
  TJobListTypejobsArray = Array of TJobListTypejobsItem;
  TJobStatistics2TypequeryPlanArray = Array of TExplainQueryStage;
  TJobStatistics2TypereferencedTablesArray = Array of TTableReference;
  TJobStatusTypeerrorsArray = Array of TErrorProto;
  TProjectListTypeprojectsArray = Array of TProjectListTypeprojectsItem;
  TQueryResponseTypeerrorsArray = Array of TErrorProto;
  TQueryResponseTyperowsArray = Array of TTableRow;
  TTableDataInsertAllRequestTyperowsArray = Array of TTableDataInsertAllRequestTyperowsItem;
  TTableDataInsertAllResponseTypeinsertErrorsItemTypeerrorsArray = Array of TErrorProto;
  TTableDataInsertAllResponseTypeinsertErrorsArray = Array of TTableDataInsertAllResponseTypeinsertErrorsItem;
  TTableDataListTyperowsArray = Array of TTableRow;
  TTableFieldSchemaTypefieldsArray = Array of TTableFieldSchema;
  TTableListTypetablesArray = Array of TTableListTypetablesItem;
  TTableRowTypefArray = Array of TTableCell;
  TTableSchemaTypefieldsArray = Array of TTableFieldSchema;
  TViewDefinitionTypeuserDefinedFunctionResourcesArray = Array of TUserDefinedFunctionResource;
  
  { --------------------------------------------------------------------
    TBigtableColumn
    --------------------------------------------------------------------}
  
  TBigtableColumn = Class(TGoogleBaseObject)
  Private
    Fencoding : String;
    FfieldName : String;
    FonlyReadLatest : boolean;
    FqualifierEncoded : String;
    FqualifierString : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setencoding(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfieldName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetonlyReadLatest(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetqualifierEncoded(AIndex : Integer; const AValue : String); virtual;
    Procedure SetqualifierString(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property encoding : String Index 0 Read Fencoding Write Setencoding;
    Property fieldName : String Index 8 Read FfieldName Write SetfieldName;
    Property onlyReadLatest : boolean Index 16 Read FonlyReadLatest Write SetonlyReadLatest;
    Property qualifierEncoded : String Index 24 Read FqualifierEncoded Write SetqualifierEncoded;
    Property qualifierString : String Index 32 Read FqualifierString Write SetqualifierString;
    Property _type : String Index 40 Read F_type Write Set_type;
  end;
  TBigtableColumnClass = Class of TBigtableColumn;
  
  { --------------------------------------------------------------------
    TBigtableColumnFamily
    --------------------------------------------------------------------}
  
  TBigtableColumnFamily = Class(TGoogleBaseObject)
  Private
    Fcolumns : TBigtableColumnFamilyTypecolumnsArray;
    Fencoding : String;
    FfamilyId : String;
    FonlyReadLatest : boolean;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcolumns(AIndex : Integer; const AValue : TBigtableColumnFamilyTypecolumnsArray); virtual;
    Procedure Setencoding(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfamilyId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetonlyReadLatest(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property columns : TBigtableColumnFamilyTypecolumnsArray Index 0 Read Fcolumns Write Setcolumns;
    Property encoding : String Index 8 Read Fencoding Write Setencoding;
    Property familyId : String Index 16 Read FfamilyId Write SetfamilyId;
    Property onlyReadLatest : boolean Index 24 Read FonlyReadLatest Write SetonlyReadLatest;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TBigtableColumnFamilyClass = Class of TBigtableColumnFamily;
  
  { --------------------------------------------------------------------
    TBigtableOptions
    --------------------------------------------------------------------}
  
  TBigtableOptions = Class(TGoogleBaseObject)
  Private
    FcolumnFamilies : TBigtableOptionsTypecolumnFamiliesArray;
    FignoreUnspecifiedColumnFamilies : boolean;
  Protected
    //Property setters
    Procedure SetcolumnFamilies(AIndex : Integer; const AValue : TBigtableOptionsTypecolumnFamiliesArray); virtual;
    Procedure SetignoreUnspecifiedColumnFamilies(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property columnFamilies : TBigtableOptionsTypecolumnFamiliesArray Index 0 Read FcolumnFamilies Write SetcolumnFamilies;
    Property ignoreUnspecifiedColumnFamilies : boolean Index 8 Read FignoreUnspecifiedColumnFamilies Write SetignoreUnspecifiedColumnFamilies;
  end;
  TBigtableOptionsClass = Class of TBigtableOptions;
  
  { --------------------------------------------------------------------
    TCsvOptions
    --------------------------------------------------------------------}
  
  TCsvOptions = Class(TGoogleBaseObject)
  Private
    FallowJaggedRows : boolean;
    FallowQuotedNewlines : boolean;
    Fencoding : String;
    FfieldDelimiter : String;
    Fquote : String;
    FskipLeadingRows : String;
  Protected
    //Property setters
    Procedure SetallowJaggedRows(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetallowQuotedNewlines(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setencoding(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfieldDelimiter(AIndex : Integer; const AValue : String); virtual;
    Procedure Setquote(AIndex : Integer; const AValue : String); virtual;
    Procedure SetskipLeadingRows(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property allowJaggedRows : boolean Index 0 Read FallowJaggedRows Write SetallowJaggedRows;
    Property allowQuotedNewlines : boolean Index 8 Read FallowQuotedNewlines Write SetallowQuotedNewlines;
    Property encoding : String Index 16 Read Fencoding Write Setencoding;
    Property fieldDelimiter : String Index 24 Read FfieldDelimiter Write SetfieldDelimiter;
    Property quote : String Index 32 Read Fquote Write Setquote;
    Property skipLeadingRows : String Index 40 Read FskipLeadingRows Write SetskipLeadingRows;
  end;
  TCsvOptionsClass = Class of TCsvOptions;
  
  { --------------------------------------------------------------------
    TDatasetTypeaccessItem
    --------------------------------------------------------------------}
  
  TDatasetTypeaccessItem = Class(TGoogleBaseObject)
  Private
    Fdomain : String;
    FgroupByEmail : String;
    Frole : String;
    FspecialGroup : String;
    FuserByEmail : String;
    Fview : TTableReference;
  Protected
    //Property setters
    Procedure Setdomain(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgroupByEmail(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrole(AIndex : Integer; const AValue : String); virtual;
    Procedure SetspecialGroup(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserByEmail(AIndex : Integer; const AValue : String); virtual;
    Procedure Setview(AIndex : Integer; const AValue : TTableReference); virtual;
  Public
  Published
    Property domain : String Index 0 Read Fdomain Write Setdomain;
    Property groupByEmail : String Index 8 Read FgroupByEmail Write SetgroupByEmail;
    Property role : String Index 16 Read Frole Write Setrole;
    Property specialGroup : String Index 24 Read FspecialGroup Write SetspecialGroup;
    Property userByEmail : String Index 32 Read FuserByEmail Write SetuserByEmail;
    Property view : TTableReference Index 40 Read Fview Write Setview;
  end;
  TDatasetTypeaccessItemClass = Class of TDatasetTypeaccessItem;
  
  { --------------------------------------------------------------------
    TDataset
    --------------------------------------------------------------------}
  
  TDataset = Class(TGoogleBaseObject)
  Private
    Faccess : TDatasetTypeaccessArray;
    FcreationTime : String;
    FdatasetReference : TDatasetReference;
    FdefaultTableExpirationMs : String;
    Fdescription : String;
    Fetag : String;
    FfriendlyName : String;
    Fid : String;
    Fkind : String;
    FlastModifiedTime : String;
    Flocation : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setaccess(AIndex : Integer; const AValue : TDatasetTypeaccessArray); virtual;
    Procedure SetcreationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdatasetReference(AIndex : Integer; const AValue : TDatasetReference); virtual;
    Procedure SetdefaultTableExpirationMs(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfriendlyName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property access : TDatasetTypeaccessArray Index 0 Read Faccess Write Setaccess;
    Property creationTime : String Index 8 Read FcreationTime Write SetcreationTime;
    Property datasetReference : TDatasetReference Index 16 Read FdatasetReference Write SetdatasetReference;
    Property defaultTableExpirationMs : String Index 24 Read FdefaultTableExpirationMs Write SetdefaultTableExpirationMs;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property etag : String Index 40 Read Fetag Write Setetag;
    Property friendlyName : String Index 48 Read FfriendlyName Write SetfriendlyName;
    Property id : String Index 56 Read Fid Write Setid;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property lastModifiedTime : String Index 72 Read FlastModifiedTime Write SetlastModifiedTime;
    Property location : String Index 80 Read Flocation Write Setlocation;
    Property selfLink : String Index 88 Read FselfLink Write SetselfLink;
  end;
  TDatasetClass = Class of TDataset;
  
  { --------------------------------------------------------------------
    TDatasetListTypedatasetsItem
    --------------------------------------------------------------------}
  
  TDatasetListTypedatasetsItem = Class(TGoogleBaseObject)
  Private
    FdatasetReference : TDatasetReference;
    FfriendlyName : String;
    Fid : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetdatasetReference(AIndex : Integer; const AValue : TDatasetReference); virtual;
    Procedure SetfriendlyName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property datasetReference : TDatasetReference Index 0 Read FdatasetReference Write SetdatasetReference;
    Property friendlyName : String Index 8 Read FfriendlyName Write SetfriendlyName;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
  end;
  TDatasetListTypedatasetsItemClass = Class of TDatasetListTypedatasetsItem;
  
  { --------------------------------------------------------------------
    TDatasetList
    --------------------------------------------------------------------}
  
  TDatasetList = Class(TGoogleBaseObject)
  Private
    Fdatasets : TDatasetListTypedatasetsArray;
    Fetag : String;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setdatasets(AIndex : Integer; const AValue : TDatasetListTypedatasetsArray); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasets : TDatasetListTypedatasetsArray Index 0 Read Fdatasets Write Setdatasets;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TDatasetListClass = Class of TDatasetList;
  
  { --------------------------------------------------------------------
    TDatasetReference
    --------------------------------------------------------------------}
  
  TDatasetReference = Class(TGoogleBaseObject)
  Private
    FdatasetId : String;
    FprojectId : String;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
  end;
  TDatasetReferenceClass = Class of TDatasetReference;
  
  { --------------------------------------------------------------------
    TErrorProto
    --------------------------------------------------------------------}
  
  TErrorProto = Class(TGoogleBaseObject)
  Private
    FdebugInfo : String;
    Flocation : String;
    Fmessage : String;
    Freason : String;
  Protected
    //Property setters
    Procedure SetdebugInfo(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreason(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property debugInfo : String Index 0 Read FdebugInfo Write SetdebugInfo;
    Property location : String Index 8 Read Flocation Write Setlocation;
    Property message : String Index 16 Read Fmessage Write Setmessage;
    Property reason : String Index 24 Read Freason Write Setreason;
  end;
  TErrorProtoClass = Class of TErrorProto;
  
  { --------------------------------------------------------------------
    TExplainQueryStage
    --------------------------------------------------------------------}
  
  TExplainQueryStage = Class(TGoogleBaseObject)
  Private
    FcomputeRatioAvg : double;
    FcomputeRatioMax : double;
    Fid : String;
    Fname : String;
    FreadRatioAvg : double;
    FreadRatioMax : double;
    FrecordsRead : String;
    FrecordsWritten : String;
    Fsteps : TExplainQueryStageTypestepsArray;
    FwaitRatioAvg : double;
    FwaitRatioMax : double;
    FwriteRatioAvg : double;
    FwriteRatioMax : double;
  Protected
    //Property setters
    Procedure SetcomputeRatioAvg(AIndex : Integer; const AValue : double); virtual;
    Procedure SetcomputeRatioMax(AIndex : Integer; const AValue : double); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreadRatioAvg(AIndex : Integer; const AValue : double); virtual;
    Procedure SetreadRatioMax(AIndex : Integer; const AValue : double); virtual;
    Procedure SetrecordsRead(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrecordsWritten(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsteps(AIndex : Integer; const AValue : TExplainQueryStageTypestepsArray); virtual;
    Procedure SetwaitRatioAvg(AIndex : Integer; const AValue : double); virtual;
    Procedure SetwaitRatioMax(AIndex : Integer; const AValue : double); virtual;
    Procedure SetwriteRatioAvg(AIndex : Integer; const AValue : double); virtual;
    Procedure SetwriteRatioMax(AIndex : Integer; const AValue : double); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property computeRatioAvg : double Index 0 Read FcomputeRatioAvg Write SetcomputeRatioAvg;
    Property computeRatioMax : double Index 8 Read FcomputeRatioMax Write SetcomputeRatioMax;
    Property id : String Index 16 Read Fid Write Setid;
    Property name : String Index 24 Read Fname Write Setname;
    Property readRatioAvg : double Index 32 Read FreadRatioAvg Write SetreadRatioAvg;
    Property readRatioMax : double Index 40 Read FreadRatioMax Write SetreadRatioMax;
    Property recordsRead : String Index 48 Read FrecordsRead Write SetrecordsRead;
    Property recordsWritten : String Index 56 Read FrecordsWritten Write SetrecordsWritten;
    Property steps : TExplainQueryStageTypestepsArray Index 64 Read Fsteps Write Setsteps;
    Property waitRatioAvg : double Index 72 Read FwaitRatioAvg Write SetwaitRatioAvg;
    Property waitRatioMax : double Index 80 Read FwaitRatioMax Write SetwaitRatioMax;
    Property writeRatioAvg : double Index 88 Read FwriteRatioAvg Write SetwriteRatioAvg;
    Property writeRatioMax : double Index 96 Read FwriteRatioMax Write SetwriteRatioMax;
  end;
  TExplainQueryStageClass = Class of TExplainQueryStage;
  
  { --------------------------------------------------------------------
    TExplainQueryStep
    --------------------------------------------------------------------}
  
  TExplainQueryStep = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fsubsteps : TStringArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsubsteps(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property substeps : TStringArray Index 8 Read Fsubsteps Write Setsubsteps;
  end;
  TExplainQueryStepClass = Class of TExplainQueryStep;
  
  { --------------------------------------------------------------------
    TExternalDataConfiguration
    --------------------------------------------------------------------}
  
  TExternalDataConfiguration = Class(TGoogleBaseObject)
  Private
    Fautodetect : boolean;
    FbigtableOptions : TBigtableOptions;
    Fcompression : String;
    FcsvOptions : TCsvOptions;
    FgoogleSheetsOptions : TGoogleSheetsOptions;
    FignoreUnknownValues : boolean;
    FmaxBadRecords : integer;
    Fschema : TTableSchema;
    FsourceFormat : String;
    FsourceUris : TStringArray;
  Protected
    //Property setters
    Procedure Setautodetect(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetbigtableOptions(AIndex : Integer; const AValue : TBigtableOptions); virtual;
    Procedure Setcompression(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcsvOptions(AIndex : Integer; const AValue : TCsvOptions); virtual;
    Procedure SetgoogleSheetsOptions(AIndex : Integer; const AValue : TGoogleSheetsOptions); virtual;
    Procedure SetignoreUnknownValues(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetmaxBadRecords(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setschema(AIndex : Integer; const AValue : TTableSchema); virtual;
    Procedure SetsourceFormat(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceUris(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property autodetect : boolean Index 0 Read Fautodetect Write Setautodetect;
    Property bigtableOptions : TBigtableOptions Index 8 Read FbigtableOptions Write SetbigtableOptions;
    Property compression : String Index 16 Read Fcompression Write Setcompression;
    Property csvOptions : TCsvOptions Index 24 Read FcsvOptions Write SetcsvOptions;
    Property googleSheetsOptions : TGoogleSheetsOptions Index 32 Read FgoogleSheetsOptions Write SetgoogleSheetsOptions;
    Property ignoreUnknownValues : boolean Index 40 Read FignoreUnknownValues Write SetignoreUnknownValues;
    Property maxBadRecords : integer Index 48 Read FmaxBadRecords Write SetmaxBadRecords;
    Property schema : TTableSchema Index 56 Read Fschema Write Setschema;
    Property sourceFormat : String Index 64 Read FsourceFormat Write SetsourceFormat;
    Property sourceUris : TStringArray Index 72 Read FsourceUris Write SetsourceUris;
  end;
  TExternalDataConfigurationClass = Class of TExternalDataConfiguration;
  
  { --------------------------------------------------------------------
    TGetQueryResultsResponse
    --------------------------------------------------------------------}
  
  TGetQueryResultsResponse = Class(TGoogleBaseObject)
  Private
    FcacheHit : boolean;
    Ferrors : TGetQueryResultsResponseTypeerrorsArray;
    Fetag : String;
    FjobComplete : boolean;
    FjobReference : TJobReference;
    Fkind : String;
    FpageToken : String;
    Frows : TGetQueryResultsResponseTyperowsArray;
    Fschema : TTableSchema;
    FtotalBytesProcessed : String;
    FtotalRows : String;
  Protected
    //Property setters
    Procedure SetcacheHit(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Seterrors(AIndex : Integer; const AValue : TGetQueryResultsResponseTypeerrorsArray); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure SetjobComplete(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetjobReference(AIndex : Integer; const AValue : TJobReference); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; const AValue : TGetQueryResultsResponseTyperowsArray); virtual;
    Procedure Setschema(AIndex : Integer; const AValue : TTableSchema); virtual;
    Procedure SettotalBytesProcessed(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalRows(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property cacheHit : boolean Index 0 Read FcacheHit Write SetcacheHit;
    Property errors : TGetQueryResultsResponseTypeerrorsArray Index 8 Read Ferrors Write Seterrors;
    Property etag : String Index 16 Read Fetag Write Setetag;
    Property jobComplete : boolean Index 24 Read FjobComplete Write SetjobComplete;
    Property jobReference : TJobReference Index 32 Read FjobReference Write SetjobReference;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property pageToken : String Index 48 Read FpageToken Write SetpageToken;
    Property rows : TGetQueryResultsResponseTyperowsArray Index 56 Read Frows Write Setrows;
    Property schema : TTableSchema Index 64 Read Fschema Write Setschema;
    Property totalBytesProcessed : String Index 72 Read FtotalBytesProcessed Write SettotalBytesProcessed;
    Property totalRows : String Index 80 Read FtotalRows Write SettotalRows;
  end;
  TGetQueryResultsResponseClass = Class of TGetQueryResultsResponse;
  
  { --------------------------------------------------------------------
    TGoogleSheetsOptions
    --------------------------------------------------------------------}
  
  TGoogleSheetsOptions = Class(TGoogleBaseObject)
  Private
    FskipLeadingRows : String;
  Protected
    //Property setters
    Procedure SetskipLeadingRows(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property skipLeadingRows : String Index 0 Read FskipLeadingRows Write SetskipLeadingRows;
  end;
  TGoogleSheetsOptionsClass = Class of TGoogleSheetsOptions;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    Fconfiguration : TJobConfiguration;
    Fetag : String;
    Fid : String;
    FjobReference : TJobReference;
    Fkind : String;
    FselfLink : String;
    Fstatistics : TJobStatistics;
    Fstatus : TJobStatus;
    Fuser_email : String;
  Protected
    //Property setters
    Procedure Setconfiguration(AIndex : Integer; const AValue : TJobConfiguration); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetjobReference(AIndex : Integer; const AValue : TJobReference); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatistics(AIndex : Integer; const AValue : TJobStatistics); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : TJobStatus); virtual;
    Procedure Setuser_email(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property configuration : TJobConfiguration Index 0 Read Fconfiguration Write Setconfiguration;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property jobReference : TJobReference Index 24 Read FjobReference Write SetjobReference;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property selfLink : String Index 40 Read FselfLink Write SetselfLink;
    Property statistics : TJobStatistics Index 48 Read Fstatistics Write Setstatistics;
    Property status : TJobStatus Index 56 Read Fstatus Write Setstatus;
    Property user_email : String Index 64 Read Fuser_email Write Setuser_email;
  end;
  TJobClass = Class of TJob;
  
  { --------------------------------------------------------------------
    TJobCancelResponse
    --------------------------------------------------------------------}
  
  TJobCancelResponse = Class(TGoogleBaseObject)
  Private
    Fjob : TJob;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setjob(AIndex : Integer; const AValue : TJob); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property job : TJob Index 0 Read Fjob Write Setjob;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TJobCancelResponseClass = Class of TJobCancelResponse;
  
  { --------------------------------------------------------------------
    TJobConfiguration
    --------------------------------------------------------------------}
  
  TJobConfiguration = Class(TGoogleBaseObject)
  Private
    Fcopy : TJobConfigurationTableCopy;
    FdryRun : boolean;
    Fextract : TJobConfigurationExtract;
    Fload : TJobConfigurationLoad;
    Fquery : TJobConfigurationQuery;
  Protected
    //Property setters
    Procedure Setcopy(AIndex : Integer; const AValue : TJobConfigurationTableCopy); virtual;
    Procedure SetdryRun(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setextract(AIndex : Integer; const AValue : TJobConfigurationExtract); virtual;
    Procedure Setload(AIndex : Integer; const AValue : TJobConfigurationLoad); virtual;
    Procedure Setquery(AIndex : Integer; const AValue : TJobConfigurationQuery); virtual;
  Public
  Published
    Property copy : TJobConfigurationTableCopy Index 0 Read Fcopy Write Setcopy;
    Property dryRun : boolean Index 8 Read FdryRun Write SetdryRun;
    Property extract : TJobConfigurationExtract Index 16 Read Fextract Write Setextract;
    Property load : TJobConfigurationLoad Index 24 Read Fload Write Setload;
    Property query : TJobConfigurationQuery Index 32 Read Fquery Write Setquery;
  end;
  TJobConfigurationClass = Class of TJobConfiguration;
  
  { --------------------------------------------------------------------
    TJobConfigurationExtract
    --------------------------------------------------------------------}
  
  TJobConfigurationExtract = Class(TGoogleBaseObject)
  Private
    Fcompression : String;
    FdestinationFormat : String;
    FdestinationUri : String;
    FdestinationUris : TStringArray;
    FfieldDelimiter : String;
    FprintHeader : boolean;
    FsourceTable : TTableReference;
  Protected
    //Property setters
    Procedure Setcompression(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdestinationFormat(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdestinationUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdestinationUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetfieldDelimiter(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprintHeader(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetsourceTable(AIndex : Integer; const AValue : TTableReference); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property compression : String Index 0 Read Fcompression Write Setcompression;
    Property destinationFormat : String Index 8 Read FdestinationFormat Write SetdestinationFormat;
    Property destinationUri : String Index 16 Read FdestinationUri Write SetdestinationUri;
    Property destinationUris : TStringArray Index 24 Read FdestinationUris Write SetdestinationUris;
    Property fieldDelimiter : String Index 32 Read FfieldDelimiter Write SetfieldDelimiter;
    Property printHeader : boolean Index 40 Read FprintHeader Write SetprintHeader;
    Property sourceTable : TTableReference Index 48 Read FsourceTable Write SetsourceTable;
  end;
  TJobConfigurationExtractClass = Class of TJobConfigurationExtract;
  
  { --------------------------------------------------------------------
    TJobConfigurationLoad
    --------------------------------------------------------------------}
  
  TJobConfigurationLoad = Class(TGoogleBaseObject)
  Private
    FallowJaggedRows : boolean;
    FallowQuotedNewlines : boolean;
    Fautodetect : boolean;
    FcreateDisposition : String;
    FdestinationTable : TTableReference;
    Fencoding : String;
    FfieldDelimiter : String;
    FignoreUnknownValues : boolean;
    FmaxBadRecords : integer;
    FprojectionFields : TStringArray;
    Fquote : String;
    Fschema : TTableSchema;
    FschemaInline : String;
    FschemaInlineFormat : String;
    FskipLeadingRows : integer;
    FsourceFormat : String;
    FsourceUris : TStringArray;
    FwriteDisposition : String;
  Protected
    //Property setters
    Procedure SetallowJaggedRows(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetallowQuotedNewlines(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setautodetect(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcreateDisposition(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdestinationTable(AIndex : Integer; const AValue : TTableReference); virtual;
    Procedure Setencoding(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfieldDelimiter(AIndex : Integer; const AValue : String); virtual;
    Procedure SetignoreUnknownValues(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetmaxBadRecords(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetprojectionFields(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setquote(AIndex : Integer; const AValue : String); virtual;
    Procedure Setschema(AIndex : Integer; const AValue : TTableSchema); virtual;
    Procedure SetschemaInline(AIndex : Integer; const AValue : String); virtual;
    Procedure SetschemaInlineFormat(AIndex : Integer; const AValue : String); virtual;
    Procedure SetskipLeadingRows(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetsourceFormat(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetwriteDisposition(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property allowJaggedRows : boolean Index 0 Read FallowJaggedRows Write SetallowJaggedRows;
    Property allowQuotedNewlines : boolean Index 8 Read FallowQuotedNewlines Write SetallowQuotedNewlines;
    Property autodetect : boolean Index 16 Read Fautodetect Write Setautodetect;
    Property createDisposition : String Index 24 Read FcreateDisposition Write SetcreateDisposition;
    Property destinationTable : TTableReference Index 32 Read FdestinationTable Write SetdestinationTable;
    Property encoding : String Index 40 Read Fencoding Write Setencoding;
    Property fieldDelimiter : String Index 48 Read FfieldDelimiter Write SetfieldDelimiter;
    Property ignoreUnknownValues : boolean Index 56 Read FignoreUnknownValues Write SetignoreUnknownValues;
    Property maxBadRecords : integer Index 64 Read FmaxBadRecords Write SetmaxBadRecords;
    Property projectionFields : TStringArray Index 72 Read FprojectionFields Write SetprojectionFields;
    Property quote : String Index 80 Read Fquote Write Setquote;
    Property schema : TTableSchema Index 88 Read Fschema Write Setschema;
    Property schemaInline : String Index 96 Read FschemaInline Write SetschemaInline;
    Property schemaInlineFormat : String Index 104 Read FschemaInlineFormat Write SetschemaInlineFormat;
    Property skipLeadingRows : integer Index 112 Read FskipLeadingRows Write SetskipLeadingRows;
    Property sourceFormat : String Index 120 Read FsourceFormat Write SetsourceFormat;
    Property sourceUris : TStringArray Index 128 Read FsourceUris Write SetsourceUris;
    Property writeDisposition : String Index 136 Read FwriteDisposition Write SetwriteDisposition;
  end;
  TJobConfigurationLoadClass = Class of TJobConfigurationLoad;
  
  { --------------------------------------------------------------------
    TJobConfigurationQueryTypetableDefinitions
    --------------------------------------------------------------------}
  
  TJobConfigurationQueryTypetableDefinitions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TJobConfigurationQueryTypetableDefinitionsClass = Class of TJobConfigurationQueryTypetableDefinitions;
  
  { --------------------------------------------------------------------
    TJobConfigurationQuery
    --------------------------------------------------------------------}
  
  TJobConfigurationQuery = Class(TGoogleBaseObject)
  Private
    FallowLargeResults : boolean;
    FcreateDisposition : String;
    FdefaultDataset : TDatasetReference;
    FdestinationTable : TTableReference;
    FflattenResults : boolean;
    FmaximumBillingTier : integer;
    FpreserveNulls : boolean;
    Fpriority : String;
    Fquery : String;
    FtableDefinitions : TJobConfigurationQueryTypetableDefinitions;
    FuseLegacySql : boolean;
    FuseQueryCache : boolean;
    FuserDefinedFunctionResources : TJobConfigurationQueryTypeuserDefinedFunctionResourcesArray;
    FwriteDisposition : String;
  Protected
    //Property setters
    Procedure SetallowLargeResults(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcreateDisposition(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdefaultDataset(AIndex : Integer; const AValue : TDatasetReference); virtual;
    Procedure SetdestinationTable(AIndex : Integer; const AValue : TTableReference); virtual;
    Procedure SetflattenResults(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetmaximumBillingTier(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetpreserveNulls(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setpriority(AIndex : Integer; const AValue : String); virtual;
    Procedure Setquery(AIndex : Integer; const AValue : String); virtual;
    Procedure SettableDefinitions(AIndex : Integer; const AValue : TJobConfigurationQueryTypetableDefinitions); virtual;
    Procedure SetuseLegacySql(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetuseQueryCache(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetuserDefinedFunctionResources(AIndex : Integer; const AValue : TJobConfigurationQueryTypeuserDefinedFunctionResourcesArray); virtual;
    Procedure SetwriteDisposition(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property allowLargeResults : boolean Index 0 Read FallowLargeResults Write SetallowLargeResults;
    Property createDisposition : String Index 8 Read FcreateDisposition Write SetcreateDisposition;
    Property defaultDataset : TDatasetReference Index 16 Read FdefaultDataset Write SetdefaultDataset;
    Property destinationTable : TTableReference Index 24 Read FdestinationTable Write SetdestinationTable;
    Property flattenResults : boolean Index 32 Read FflattenResults Write SetflattenResults;
    Property maximumBillingTier : integer Index 40 Read FmaximumBillingTier Write SetmaximumBillingTier;
    Property preserveNulls : boolean Index 48 Read FpreserveNulls Write SetpreserveNulls;
    Property priority : String Index 56 Read Fpriority Write Setpriority;
    Property query : String Index 64 Read Fquery Write Setquery;
    Property tableDefinitions : TJobConfigurationQueryTypetableDefinitions Index 72 Read FtableDefinitions Write SettableDefinitions;
    Property useLegacySql : boolean Index 80 Read FuseLegacySql Write SetuseLegacySql;
    Property useQueryCache : boolean Index 88 Read FuseQueryCache Write SetuseQueryCache;
    Property userDefinedFunctionResources : TJobConfigurationQueryTypeuserDefinedFunctionResourcesArray Index 96 Read FuserDefinedFunctionResources Write SetuserDefinedFunctionResources;
    Property writeDisposition : String Index 104 Read FwriteDisposition Write SetwriteDisposition;
  end;
  TJobConfigurationQueryClass = Class of TJobConfigurationQuery;
  
  { --------------------------------------------------------------------
    TJobConfigurationTableCopy
    --------------------------------------------------------------------}
  
  TJobConfigurationTableCopy = Class(TGoogleBaseObject)
  Private
    FcreateDisposition : String;
    FdestinationTable : TTableReference;
    FsourceTable : TTableReference;
    FsourceTables : TJobConfigurationTableCopyTypesourceTablesArray;
    FwriteDisposition : String;
  Protected
    //Property setters
    Procedure SetcreateDisposition(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdestinationTable(AIndex : Integer; const AValue : TTableReference); virtual;
    Procedure SetsourceTable(AIndex : Integer; const AValue : TTableReference); virtual;
    Procedure SetsourceTables(AIndex : Integer; const AValue : TJobConfigurationTableCopyTypesourceTablesArray); virtual;
    Procedure SetwriteDisposition(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property createDisposition : String Index 0 Read FcreateDisposition Write SetcreateDisposition;
    Property destinationTable : TTableReference Index 8 Read FdestinationTable Write SetdestinationTable;
    Property sourceTable : TTableReference Index 16 Read FsourceTable Write SetsourceTable;
    Property sourceTables : TJobConfigurationTableCopyTypesourceTablesArray Index 24 Read FsourceTables Write SetsourceTables;
    Property writeDisposition : String Index 32 Read FwriteDisposition Write SetwriteDisposition;
  end;
  TJobConfigurationTableCopyClass = Class of TJobConfigurationTableCopy;
  
  { --------------------------------------------------------------------
    TJobListTypejobsItem
    --------------------------------------------------------------------}
  
  TJobListTypejobsItem = Class(TGoogleBaseObject)
  Private
    Fconfiguration : TJobConfiguration;
    FerrorResult : TErrorProto;
    Fid : String;
    FjobReference : TJobReference;
    Fkind : String;
    Fstate : String;
    Fstatistics : TJobStatistics;
    Fstatus : TJobStatus;
    Fuser_email : String;
  Protected
    //Property setters
    Procedure Setconfiguration(AIndex : Integer; const AValue : TJobConfiguration); virtual;
    Procedure SeterrorResult(AIndex : Integer; const AValue : TErrorProto); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetjobReference(AIndex : Integer; const AValue : TJobReference); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatistics(AIndex : Integer; const AValue : TJobStatistics); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : TJobStatus); virtual;
    Procedure Setuser_email(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property configuration : TJobConfiguration Index 0 Read Fconfiguration Write Setconfiguration;
    Property errorResult : TErrorProto Index 8 Read FerrorResult Write SeterrorResult;
    Property id : String Index 16 Read Fid Write Setid;
    Property jobReference : TJobReference Index 24 Read FjobReference Write SetjobReference;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property state : String Index 40 Read Fstate Write Setstate;
    Property statistics : TJobStatistics Index 48 Read Fstatistics Write Setstatistics;
    Property status : TJobStatus Index 56 Read Fstatus Write Setstatus;
    Property user_email : String Index 64 Read Fuser_email Write Setuser_email;
  end;
  TJobListTypejobsItemClass = Class of TJobListTypejobsItem;
  
  { --------------------------------------------------------------------
    TJobList
    --------------------------------------------------------------------}
  
  TJobList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fjobs : TJobListTypejobsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setjobs(AIndex : Integer; const AValue : TJobListTypejobsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property jobs : TJobListTypejobsArray Index 8 Read Fjobs Write Setjobs;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TJobListClass = Class of TJobList;
  
  { --------------------------------------------------------------------
    TJobReference
    --------------------------------------------------------------------}
  
  TJobReference = Class(TGoogleBaseObject)
  Private
    FjobId : String;
    FprojectId : String;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property jobId : String Index 0 Read FjobId Write SetjobId;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
  end;
  TJobReferenceClass = Class of TJobReference;
  
  { --------------------------------------------------------------------
    TJobStatistics
    --------------------------------------------------------------------}
  
  TJobStatistics = Class(TGoogleBaseObject)
  Private
    FcreationTime : String;
    FendTime : String;
    Fextract : TJobStatistics4;
    Fload : TJobStatistics3;
    Fquery : TJobStatistics2;
    FstartTime : String;
    FtotalBytesProcessed : String;
  Protected
    //Property setters
    Procedure SetcreationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setextract(AIndex : Integer; const AValue : TJobStatistics4); virtual;
    Procedure Setload(AIndex : Integer; const AValue : TJobStatistics3); virtual;
    Procedure Setquery(AIndex : Integer; const AValue : TJobStatistics2); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalBytesProcessed(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property creationTime : String Index 0 Read FcreationTime Write SetcreationTime;
    Property endTime : String Index 8 Read FendTime Write SetendTime;
    Property extract : TJobStatistics4 Index 16 Read Fextract Write Setextract;
    Property load : TJobStatistics3 Index 24 Read Fload Write Setload;
    Property query : TJobStatistics2 Index 32 Read Fquery Write Setquery;
    Property startTime : String Index 40 Read FstartTime Write SetstartTime;
    Property totalBytesProcessed : String Index 48 Read FtotalBytesProcessed Write SettotalBytesProcessed;
  end;
  TJobStatisticsClass = Class of TJobStatistics;
  
  { --------------------------------------------------------------------
    TJobStatistics2
    --------------------------------------------------------------------}
  
  TJobStatistics2 = Class(TGoogleBaseObject)
  Private
    FbillingTier : integer;
    FcacheHit : boolean;
    FqueryPlan : TJobStatistics2TypequeryPlanArray;
    FreferencedTables : TJobStatistics2TypereferencedTablesArray;
    Fschema : TTableSchema;
    FtotalBytesBilled : String;
    FtotalBytesProcessed : String;
  Protected
    //Property setters
    Procedure SetbillingTier(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetcacheHit(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetqueryPlan(AIndex : Integer; const AValue : TJobStatistics2TypequeryPlanArray); virtual;
    Procedure SetreferencedTables(AIndex : Integer; const AValue : TJobStatistics2TypereferencedTablesArray); virtual;
    Procedure Setschema(AIndex : Integer; const AValue : TTableSchema); virtual;
    Procedure SettotalBytesBilled(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalBytesProcessed(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property billingTier : integer Index 0 Read FbillingTier Write SetbillingTier;
    Property cacheHit : boolean Index 8 Read FcacheHit Write SetcacheHit;
    Property queryPlan : TJobStatistics2TypequeryPlanArray Index 16 Read FqueryPlan Write SetqueryPlan;
    Property referencedTables : TJobStatistics2TypereferencedTablesArray Index 24 Read FreferencedTables Write SetreferencedTables;
    Property schema : TTableSchema Index 32 Read Fschema Write Setschema;
    Property totalBytesBilled : String Index 40 Read FtotalBytesBilled Write SettotalBytesBilled;
    Property totalBytesProcessed : String Index 48 Read FtotalBytesProcessed Write SettotalBytesProcessed;
  end;
  TJobStatistics2Class = Class of TJobStatistics2;
  
  { --------------------------------------------------------------------
    TJobStatistics3
    --------------------------------------------------------------------}
  
  TJobStatistics3 = Class(TGoogleBaseObject)
  Private
    FinputFileBytes : String;
    FinputFiles : String;
    FoutputBytes : String;
    FoutputRows : String;
  Protected
    //Property setters
    Procedure SetinputFileBytes(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinputFiles(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoutputBytes(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoutputRows(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property inputFileBytes : String Index 0 Read FinputFileBytes Write SetinputFileBytes;
    Property inputFiles : String Index 8 Read FinputFiles Write SetinputFiles;
    Property outputBytes : String Index 16 Read FoutputBytes Write SetoutputBytes;
    Property outputRows : String Index 24 Read FoutputRows Write SetoutputRows;
  end;
  TJobStatistics3Class = Class of TJobStatistics3;
  
  { --------------------------------------------------------------------
    TJobStatistics4
    --------------------------------------------------------------------}
  
  TJobStatistics4 = Class(TGoogleBaseObject)
  Private
    FdestinationUriFileCounts : TStringArray;
  Protected
    //Property setters
    Procedure SetdestinationUriFileCounts(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property destinationUriFileCounts : TStringArray Index 0 Read FdestinationUriFileCounts Write SetdestinationUriFileCounts;
  end;
  TJobStatistics4Class = Class of TJobStatistics4;
  
  { --------------------------------------------------------------------
    TJobStatus
    --------------------------------------------------------------------}
  
  TJobStatus = Class(TGoogleBaseObject)
  Private
    FerrorResult : TErrorProto;
    Ferrors : TJobStatusTypeerrorsArray;
    Fstate : String;
  Protected
    //Property setters
    Procedure SeterrorResult(AIndex : Integer; const AValue : TErrorProto); virtual;
    Procedure Seterrors(AIndex : Integer; const AValue : TJobStatusTypeerrorsArray); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property errorResult : TErrorProto Index 0 Read FerrorResult Write SeterrorResult;
    Property errors : TJobStatusTypeerrorsArray Index 8 Read Ferrors Write Seterrors;
    Property state : String Index 16 Read Fstate Write Setstate;
  end;
  TJobStatusClass = Class of TJobStatus;
  
  { --------------------------------------------------------------------
    TJsonObject
    --------------------------------------------------------------------}
  
  TJsonObject = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TJsonObjectClass = Class of TJsonObject;
  
  { --------------------------------------------------------------------
    TProjectListTypeprojectsItem
    --------------------------------------------------------------------}
  
  TProjectListTypeprojectsItem = Class(TGoogleBaseObject)
  Private
    FfriendlyName : String;
    Fid : String;
    Fkind : String;
    FnumericId : String;
    FprojectReference : TProjectReference;
  Protected
    //Property setters
    Procedure SetfriendlyName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumericId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectReference(AIndex : Integer; const AValue : TProjectReference); virtual;
  Public
  Published
    Property friendlyName : String Index 0 Read FfriendlyName Write SetfriendlyName;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property numericId : String Index 24 Read FnumericId Write SetnumericId;
    Property projectReference : TProjectReference Index 32 Read FprojectReference Write SetprojectReference;
  end;
  TProjectListTypeprojectsItemClass = Class of TProjectListTypeprojectsItem;
  
  { --------------------------------------------------------------------
    TProjectList
    --------------------------------------------------------------------}
  
  TProjectList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fkind : String;
    FnextPageToken : String;
    Fprojects : TProjectListTypeprojectsArray;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprojects(AIndex : Integer; const AValue : TProjectListTypeprojectsArray); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property projects : TProjectListTypeprojectsArray Index 24 Read Fprojects Write Setprojects;
    Property totalItems : integer Index 32 Read FtotalItems Write SettotalItems;
  end;
  TProjectListClass = Class of TProjectList;
  
  { --------------------------------------------------------------------
    TProjectReference
    --------------------------------------------------------------------}
  
  TProjectReference = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
  Protected
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
  end;
  TProjectReferenceClass = Class of TProjectReference;
  
  { --------------------------------------------------------------------
    TQueryRequest
    --------------------------------------------------------------------}
  
  TQueryRequest = Class(TGoogleBaseObject)
  Private
    FdefaultDataset : TDatasetReference;
    FdryRun : boolean;
    Fkind : String;
    FmaxResults : integer;
    FpreserveNulls : boolean;
    Fquery : String;
    FtimeoutMs : integer;
    FuseLegacySql : boolean;
    FuseQueryCache : boolean;
  Protected
    //Property setters
    Procedure SetdefaultDataset(AIndex : Integer; const AValue : TDatasetReference); virtual;
    Procedure SetdryRun(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxResults(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetpreserveNulls(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setquery(AIndex : Integer; const AValue : String); virtual;
    Procedure SettimeoutMs(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetuseLegacySql(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetuseQueryCache(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property defaultDataset : TDatasetReference Index 0 Read FdefaultDataset Write SetdefaultDataset;
    Property dryRun : boolean Index 8 Read FdryRun Write SetdryRun;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property maxResults : integer Index 24 Read FmaxResults Write SetmaxResults;
    Property preserveNulls : boolean Index 32 Read FpreserveNulls Write SetpreserveNulls;
    Property query : String Index 40 Read Fquery Write Setquery;
    Property timeoutMs : integer Index 48 Read FtimeoutMs Write SettimeoutMs;
    Property useLegacySql : boolean Index 56 Read FuseLegacySql Write SetuseLegacySql;
    Property useQueryCache : boolean Index 64 Read FuseQueryCache Write SetuseQueryCache;
  end;
  TQueryRequestClass = Class of TQueryRequest;
  
  { --------------------------------------------------------------------
    TQueryResponse
    --------------------------------------------------------------------}
  
  TQueryResponse = Class(TGoogleBaseObject)
  Private
    FcacheHit : boolean;
    Ferrors : TQueryResponseTypeerrorsArray;
    FjobComplete : boolean;
    FjobReference : TJobReference;
    Fkind : String;
    FpageToken : String;
    Frows : TQueryResponseTyperowsArray;
    Fschema : TTableSchema;
    FtotalBytesProcessed : String;
    FtotalRows : String;
  Protected
    //Property setters
    Procedure SetcacheHit(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Seterrors(AIndex : Integer; const AValue : TQueryResponseTypeerrorsArray); virtual;
    Procedure SetjobComplete(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetjobReference(AIndex : Integer; const AValue : TJobReference); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; const AValue : TQueryResponseTyperowsArray); virtual;
    Procedure Setschema(AIndex : Integer; const AValue : TTableSchema); virtual;
    Procedure SettotalBytesProcessed(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalRows(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property cacheHit : boolean Index 0 Read FcacheHit Write SetcacheHit;
    Property errors : TQueryResponseTypeerrorsArray Index 8 Read Ferrors Write Seterrors;
    Property jobComplete : boolean Index 16 Read FjobComplete Write SetjobComplete;
    Property jobReference : TJobReference Index 24 Read FjobReference Write SetjobReference;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property pageToken : String Index 40 Read FpageToken Write SetpageToken;
    Property rows : TQueryResponseTyperowsArray Index 48 Read Frows Write Setrows;
    Property schema : TTableSchema Index 56 Read Fschema Write Setschema;
    Property totalBytesProcessed : String Index 64 Read FtotalBytesProcessed Write SettotalBytesProcessed;
    Property totalRows : String Index 72 Read FtotalRows Write SettotalRows;
  end;
  TQueryResponseClass = Class of TQueryResponse;
  
  { --------------------------------------------------------------------
    TStreamingbuffer
    --------------------------------------------------------------------}
  
  TStreamingbuffer = Class(TGoogleBaseObject)
  Private
    FestimatedBytes : String;
    FestimatedRows : String;
    FoldestEntryTime : String;
  Protected
    //Property setters
    Procedure SetestimatedBytes(AIndex : Integer; const AValue : String); virtual;
    Procedure SetestimatedRows(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoldestEntryTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property estimatedBytes : String Index 0 Read FestimatedBytes Write SetestimatedBytes;
    Property estimatedRows : String Index 8 Read FestimatedRows Write SetestimatedRows;
    Property oldestEntryTime : String Index 16 Read FoldestEntryTime Write SetoldestEntryTime;
  end;
  TStreamingbufferClass = Class of TStreamingbuffer;
  
  { --------------------------------------------------------------------
    TTable
    --------------------------------------------------------------------}
  
  TTable = Class(TGoogleBaseObject)
  Private
    FcreationTime : String;
    Fdescription : String;
    Fetag : String;
    FexpirationTime : String;
    FexternalDataConfiguration : TExternalDataConfiguration;
    FfriendlyName : String;
    Fid : String;
    Fkind : String;
    FlastModifiedTime : String;
    Flocation : String;
    FnumBytes : String;
    FnumLongTermBytes : String;
    FnumRows : String;
    Fschema : TTableSchema;
    FselfLink : String;
    FstreamingBuffer : TStreamingbuffer;
    FtableReference : TTableReference;
    FtimePartitioning : TTimePartitioning;
    F_type : String;
    Fview : TViewDefinition;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcreationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexpirationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexternalDataConfiguration(AIndex : Integer; const AValue : TExternalDataConfiguration); virtual;
    Procedure SetfriendlyName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlastModifiedTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumBytes(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumLongTermBytes(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumRows(AIndex : Integer; const AValue : String); virtual;
    Procedure Setschema(AIndex : Integer; const AValue : TTableSchema); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstreamingBuffer(AIndex : Integer; const AValue : TStreamingbuffer); virtual;
    Procedure SettableReference(AIndex : Integer; const AValue : TTableReference); virtual;
    Procedure SettimePartitioning(AIndex : Integer; const AValue : TTimePartitioning); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setview(AIndex : Integer; const AValue : TViewDefinition); virtual;
  Public
  Published
    Property creationTime : String Index 0 Read FcreationTime Write SetcreationTime;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property etag : String Index 16 Read Fetag Write Setetag;
    Property expirationTime : String Index 24 Read FexpirationTime Write SetexpirationTime;
    Property externalDataConfiguration : TExternalDataConfiguration Index 32 Read FexternalDataConfiguration Write SetexternalDataConfiguration;
    Property friendlyName : String Index 40 Read FfriendlyName Write SetfriendlyName;
    Property id : String Index 48 Read Fid Write Setid;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property lastModifiedTime : String Index 64 Read FlastModifiedTime Write SetlastModifiedTime;
    Property location : String Index 72 Read Flocation Write Setlocation;
    Property numBytes : String Index 80 Read FnumBytes Write SetnumBytes;
    Property numLongTermBytes : String Index 88 Read FnumLongTermBytes Write SetnumLongTermBytes;
    Property numRows : String Index 96 Read FnumRows Write SetnumRows;
    Property schema : TTableSchema Index 104 Read Fschema Write Setschema;
    Property selfLink : String Index 112 Read FselfLink Write SetselfLink;
    Property streamingBuffer : TStreamingbuffer Index 120 Read FstreamingBuffer Write SetstreamingBuffer;
    Property tableReference : TTableReference Index 128 Read FtableReference Write SettableReference;
    Property timePartitioning : TTimePartitioning Index 136 Read FtimePartitioning Write SettimePartitioning;
    Property _type : String Index 144 Read F_type Write Set_type;
    Property view : TViewDefinition Index 152 Read Fview Write Setview;
  end;
  TTableClass = Class of TTable;
  
  { --------------------------------------------------------------------
    TTableCell
    --------------------------------------------------------------------}
  
  TTableCell = Class(TGoogleBaseObject)
  Private
    Fv : TJSONSchema;
  Protected
    //Property setters
    Procedure Setv(AIndex : Integer; const AValue : TJSONSchema); virtual;
  Public
  Published
    Property v : TJSONSchema Index 0 Read Fv Write Setv;
  end;
  TTableCellClass = Class of TTableCell;
  
  { --------------------------------------------------------------------
    TTableDataInsertAllRequestTyperowsItem
    --------------------------------------------------------------------}
  
  TTableDataInsertAllRequestTyperowsItem = Class(TGoogleBaseObject)
  Private
    FinsertId : String;
    Fjson : TJsonObject;
  Protected
    //Property setters
    Procedure SetinsertId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setjson(AIndex : Integer; const AValue : TJsonObject); virtual;
  Public
  Published
    Property insertId : String Index 0 Read FinsertId Write SetinsertId;
    Property json : TJsonObject Index 8 Read Fjson Write Setjson;
  end;
  TTableDataInsertAllRequestTyperowsItemClass = Class of TTableDataInsertAllRequestTyperowsItem;
  
  { --------------------------------------------------------------------
    TTableDataInsertAllRequest
    --------------------------------------------------------------------}
  
  TTableDataInsertAllRequest = Class(TGoogleBaseObject)
  Private
    FignoreUnknownValues : boolean;
    Fkind : String;
    Frows : TTableDataInsertAllRequestTyperowsArray;
    FskipInvalidRows : boolean;
    FtemplateSuffix : String;
  Protected
    //Property setters
    Procedure SetignoreUnknownValues(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; const AValue : TTableDataInsertAllRequestTyperowsArray); virtual;
    Procedure SetskipInvalidRows(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SettemplateSuffix(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property ignoreUnknownValues : boolean Index 0 Read FignoreUnknownValues Write SetignoreUnknownValues;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property rows : TTableDataInsertAllRequestTyperowsArray Index 16 Read Frows Write Setrows;
    Property skipInvalidRows : boolean Index 24 Read FskipInvalidRows Write SetskipInvalidRows;
    Property templateSuffix : String Index 32 Read FtemplateSuffix Write SettemplateSuffix;
  end;
  TTableDataInsertAllRequestClass = Class of TTableDataInsertAllRequest;
  
  { --------------------------------------------------------------------
    TTableDataInsertAllResponseTypeinsertErrorsItem
    --------------------------------------------------------------------}
  
  TTableDataInsertAllResponseTypeinsertErrorsItem = Class(TGoogleBaseObject)
  Private
    Ferrors : TTableDataInsertAllResponseTypeinsertErrorsItemTypeerrorsArray;
    Findex : integer;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; const AValue : TTableDataInsertAllResponseTypeinsertErrorsItemTypeerrorsArray); virtual;
    Procedure Setindex(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property errors : TTableDataInsertAllResponseTypeinsertErrorsItemTypeerrorsArray Index 0 Read Ferrors Write Seterrors;
    Property index : integer Index 8 Read Findex Write Setindex;
  end;
  TTableDataInsertAllResponseTypeinsertErrorsItemClass = Class of TTableDataInsertAllResponseTypeinsertErrorsItem;
  
  { --------------------------------------------------------------------
    TTableDataInsertAllResponse
    --------------------------------------------------------------------}
  
  TTableDataInsertAllResponse = Class(TGoogleBaseObject)
  Private
    FinsertErrors : TTableDataInsertAllResponseTypeinsertErrorsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetinsertErrors(AIndex : Integer; const AValue : TTableDataInsertAllResponseTypeinsertErrorsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property insertErrors : TTableDataInsertAllResponseTypeinsertErrorsArray Index 0 Read FinsertErrors Write SetinsertErrors;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TTableDataInsertAllResponseClass = Class of TTableDataInsertAllResponse;
  
  { --------------------------------------------------------------------
    TTableDataList
    --------------------------------------------------------------------}
  
  TTableDataList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fkind : String;
    FpageToken : String;
    Frows : TTableDataListTyperowsArray;
    FtotalRows : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; const AValue : TTableDataListTyperowsArray); virtual;
    Procedure SettotalRows(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property pageToken : String Index 16 Read FpageToken Write SetpageToken;
    Property rows : TTableDataListTyperowsArray Index 24 Read Frows Write Setrows;
    Property totalRows : String Index 32 Read FtotalRows Write SettotalRows;
  end;
  TTableDataListClass = Class of TTableDataList;
  
  { --------------------------------------------------------------------
    TTableFieldSchema
    --------------------------------------------------------------------}
  
  TTableFieldSchema = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Ffields : TTableFieldSchemaTypefieldsArray;
    Fmode : String;
    Fname : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfields(AIndex : Integer; const AValue : TTableFieldSchemaTypefieldsArray); virtual;
    Procedure Setmode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property fields : TTableFieldSchemaTypefieldsArray Index 8 Read Ffields Write Setfields;
    Property mode : String Index 16 Read Fmode Write Setmode;
    Property name : String Index 24 Read Fname Write Setname;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TTableFieldSchemaClass = Class of TTableFieldSchema;
  
  { --------------------------------------------------------------------
    TTableListTypetablesItem
    --------------------------------------------------------------------}
  
  TTableListTypetablesItem = Class(TGoogleBaseObject)
  Private
    FfriendlyName : String;
    Fid : String;
    Fkind : String;
    FtableReference : TTableReference;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetfriendlyName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SettableReference(AIndex : Integer; const AValue : TTableReference); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property friendlyName : String Index 0 Read FfriendlyName Write SetfriendlyName;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property tableReference : TTableReference Index 24 Read FtableReference Write SettableReference;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TTableListTypetablesItemClass = Class of TTableListTypetablesItem;
  
  { --------------------------------------------------------------------
    TTableList
    --------------------------------------------------------------------}
  
  TTableList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fkind : String;
    FnextPageToken : String;
    Ftables : TTableListTypetablesArray;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Settables(AIndex : Integer; const AValue : TTableListTypetablesArray); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property tables : TTableListTypetablesArray Index 24 Read Ftables Write Settables;
    Property totalItems : integer Index 32 Read FtotalItems Write SettotalItems;
  end;
  TTableListClass = Class of TTableList;
  
  { --------------------------------------------------------------------
    TTableReference
    --------------------------------------------------------------------}
  
  TTableReference = Class(TGoogleBaseObject)
  Private
    FdatasetId : String;
    FprojectId : String;
    FtableId : String;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettableId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
    Property tableId : String Index 16 Read FtableId Write SettableId;
  end;
  TTableReferenceClass = Class of TTableReference;
  
  { --------------------------------------------------------------------
    TTableRow
    --------------------------------------------------------------------}
  
  TTableRow = Class(TGoogleBaseObject)
  Private
    Ff : TTableRowTypefArray;
  Protected
    //Property setters
    Procedure Setf(AIndex : Integer; const AValue : TTableRowTypefArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property f : TTableRowTypefArray Index 0 Read Ff Write Setf;
  end;
  TTableRowClass = Class of TTableRow;
  
  { --------------------------------------------------------------------
    TTableSchema
    --------------------------------------------------------------------}
  
  TTableSchema = Class(TGoogleBaseObject)
  Private
    Ffields : TTableSchemaTypefieldsArray;
  Protected
    //Property setters
    Procedure Setfields(AIndex : Integer; const AValue : TTableSchemaTypefieldsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property fields : TTableSchemaTypefieldsArray Index 0 Read Ffields Write Setfields;
  end;
  TTableSchemaClass = Class of TTableSchema;
  
  { --------------------------------------------------------------------
    TTimePartitioning
    --------------------------------------------------------------------}
  
  TTimePartitioning = Class(TGoogleBaseObject)
  Private
    FexpirationMs : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetexpirationMs(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property expirationMs : String Index 0 Read FexpirationMs Write SetexpirationMs;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TTimePartitioningClass = Class of TTimePartitioning;
  
  { --------------------------------------------------------------------
    TUserDefinedFunctionResource
    --------------------------------------------------------------------}
  
  TUserDefinedFunctionResource = Class(TGoogleBaseObject)
  Private
    FinlineCode : String;
    FresourceUri : String;
  Protected
    //Property setters
    Procedure SetinlineCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetresourceUri(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property inlineCode : String Index 0 Read FinlineCode Write SetinlineCode;
    Property resourceUri : String Index 8 Read FresourceUri Write SetresourceUri;
  end;
  TUserDefinedFunctionResourceClass = Class of TUserDefinedFunctionResource;
  
  { --------------------------------------------------------------------
    TViewDefinition
    --------------------------------------------------------------------}
  
  TViewDefinition = Class(TGoogleBaseObject)
  Private
    Fquery : String;
    FuserDefinedFunctionResources : TViewDefinitionTypeuserDefinedFunctionResourcesArray;
  Protected
    //Property setters
    Procedure Setquery(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserDefinedFunctionResources(AIndex : Integer; const AValue : TViewDefinitionTypeuserDefinedFunctionResourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property query : String Index 0 Read Fquery Write Setquery;
    Property userDefinedFunctionResources : TViewDefinitionTypeuserDefinedFunctionResourcesArray Index 8 Read FuserDefinedFunctionResources Write SetuserDefinedFunctionResources;
  end;
  TViewDefinitionClass = Class of TViewDefinition;
  
  { --------------------------------------------------------------------
    TDatasetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDatasetsResource, method Delete
  
  TDatasetsDeleteOptions = Record
    deleteContents : boolean;
  end;
  
  
  //Optional query Options for TDatasetsResource, method List
  
  TDatasetsListOptions = Record
    all : boolean;
    maxResults : integer;
    pageToken : String;
  end;
  
  TDatasetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(datasetId: string; projectId: string; AQuery : string  = '');
    Procedure Delete(datasetId: string; projectId: string; AQuery : TDatasetsdeleteOptions);
    Function Get(datasetId: string; projectId: string) : TDataset;
    Function Insert(projectId: string; aDataset : TDataset) : TDataset;
    Function List(projectId: string; AQuery : string  = '') : TDatasetList;
    Function List(projectId: string; AQuery : TDatasetslistOptions) : TDatasetList;
    Function Patch(datasetId: string; projectId: string; aDataset : TDataset) : TDataset;
    Function Update(datasetId: string; projectId: string; aDataset : TDataset) : TDataset;
  end;
  
  
  { --------------------------------------------------------------------
    TJobsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TJobsResource, method GetQueryResults
  
  TJobsGetQueryResultsOptions = Record
    maxResults : integer;
    pageToken : String;
    startIndex : String;
    timeoutMs : integer;
  end;
  
  
  //Optional query Options for TJobsResource, method List
  
  TJobsListOptions = Record
    allUsers : boolean;
    maxResults : integer;
    pageToken : String;
    projection : String;
    stateFilter : String;
  end;
  
  TJobsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Cancel(jobId: string; projectId: string) : TJobCancelResponse;
    Function Get(jobId: string; projectId: string) : TJob;
    Function GetQueryResults(jobId: string; projectId: string; AQuery : string  = '') : TGetQueryResultsResponse;
    Function GetQueryResults(jobId: string; projectId: string; AQuery : TJobsgetQueryResultsOptions) : TGetQueryResultsResponse;
    Function Insert(projectId: string; aJob : TJob) : TJob;
    Function List(projectId: string; AQuery : string  = '') : TJobList;
    Function List(projectId: string; AQuery : TJobslistOptions) : TJobList;
    Function Query(projectId: string; aQueryRequest : TQueryRequest) : TQueryResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsResource, method List
  
  TProjectsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TProjectList;
    Function List(AQuery : TProjectslistOptions) : TProjectList;
  end;
  
  
  { --------------------------------------------------------------------
    TTabledataResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTabledataResource, method List
  
  TTabledataListOptions = Record
    maxResults : integer;
    pageToken : String;
    startIndex : String;
  end;
  
  TTabledataResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function InsertAll(datasetId: string; projectId: string; tableId: string; aTableDataInsertAllRequest : TTableDataInsertAllRequest) : TTableDataInsertAllResponse;
    Function List(datasetId: string; projectId: string; tableId: string; AQuery : string  = '') : TTableDataList;
    Function List(datasetId: string; projectId: string; tableId: string; AQuery : TTabledatalistOptions) : TTableDataList;
  end;
  
  
  { --------------------------------------------------------------------
    TTablesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTablesResource, method List
  
  TTablesListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TTablesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(datasetId: string; projectId: string; tableId: string);
    Function Get(datasetId: string; projectId: string; tableId: string) : TTable;
    Function Insert(datasetId: string; projectId: string; aTable : TTable) : TTable;
    Function List(datasetId: string; projectId: string; AQuery : string  = '') : TTableList;
    Function List(datasetId: string; projectId: string; AQuery : TTableslistOptions) : TTableList;
    Function Patch(datasetId: string; projectId: string; tableId: string; aTable : TTable) : TTable;
    Function Update(datasetId: string; projectId: string; tableId: string; aTable : TTable) : TTable;
  end;
  
  
  { --------------------------------------------------------------------
    TBigqueryAPI
    --------------------------------------------------------------------}
  
  TBigqueryAPI = Class(TGoogleAPI)
  Private
    FDatasetsInstance : TDatasetsResource;
    FJobsInstance : TJobsResource;
    FProjectsInstance : TProjectsResource;
    FTabledataInstance : TTabledataResource;
    FTablesInstance : TTablesResource;
    Function GetDatasetsInstance : TDatasetsResource;virtual;
    Function GetJobsInstance : TJobsResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
    Function GetTabledataInstance : TTabledataResource;virtual;
    Function GetTablesInstance : TTablesResource;virtual;
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
    Function CreateDatasetsResource(AOwner : TComponent) : TDatasetsResource;virtual;overload;
    Function CreateDatasetsResource : TDatasetsResource;virtual;overload;
    Function CreateJobsResource(AOwner : TComponent) : TJobsResource;virtual;overload;
    Function CreateJobsResource : TJobsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    Function CreateTabledataResource(AOwner : TComponent) : TTabledataResource;virtual;overload;
    Function CreateTabledataResource : TTabledataResource;virtual;overload;
    Function CreateTablesResource(AOwner : TComponent) : TTablesResource;virtual;overload;
    Function CreateTablesResource : TTablesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property DatasetsResource : TDatasetsResource Read GetDatasetsInstance;
    Property JobsResource : TJobsResource Read GetJobsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
    Property TabledataResource : TTabledataResource Read GetTabledataInstance;
    Property TablesResource : TTablesResource Read GetTablesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TBigtableColumn
  --------------------------------------------------------------------}


Procedure TBigtableColumn.Setencoding(AIndex : Integer; const AValue : String); 

begin
  If (Fencoding=AValue) then exit;
  Fencoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableColumn.SetfieldName(AIndex : Integer; const AValue : String); 

begin
  If (FfieldName=AValue) then exit;
  FfieldName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableColumn.SetonlyReadLatest(AIndex : Integer; const AValue : boolean); 

begin
  If (FonlyReadLatest=AValue) then exit;
  FonlyReadLatest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableColumn.SetqualifierEncoded(AIndex : Integer; const AValue : String); 

begin
  If (FqualifierEncoded=AValue) then exit;
  FqualifierEncoded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableColumn.SetqualifierString(AIndex : Integer; const AValue : String); 

begin
  If (FqualifierString=AValue) then exit;
  FqualifierString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableColumn.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBigtableColumn.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TBigtableColumnFamily
  --------------------------------------------------------------------}


Procedure TBigtableColumnFamily.Setcolumns(AIndex : Integer; const AValue : TBigtableColumnFamilyTypecolumnsArray); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableColumnFamily.Setencoding(AIndex : Integer; const AValue : String); 

begin
  If (Fencoding=AValue) then exit;
  Fencoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableColumnFamily.SetfamilyId(AIndex : Integer; const AValue : String); 

begin
  If (FfamilyId=AValue) then exit;
  FfamilyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableColumnFamily.SetonlyReadLatest(AIndex : Integer; const AValue : boolean); 

begin
  If (FonlyReadLatest=AValue) then exit;
  FonlyReadLatest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableColumnFamily.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBigtableColumnFamily.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBigtableColumnFamily.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'columns' : SetLength(Fcolumns,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBigtableOptions
  --------------------------------------------------------------------}


Procedure TBigtableOptions.SetcolumnFamilies(AIndex : Integer; const AValue : TBigtableOptionsTypecolumnFamiliesArray); 

begin
  If (FcolumnFamilies=AValue) then exit;
  FcolumnFamilies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBigtableOptions.SetignoreUnspecifiedColumnFamilies(AIndex : Integer; const AValue : boolean); 

begin
  If (FignoreUnspecifiedColumnFamilies=AValue) then exit;
  FignoreUnspecifiedColumnFamilies:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBigtableOptions.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'columnfamilies' : SetLength(FcolumnFamilies,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCsvOptions
  --------------------------------------------------------------------}


Procedure TCsvOptions.SetallowJaggedRows(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowJaggedRows=AValue) then exit;
  FallowJaggedRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCsvOptions.SetallowQuotedNewlines(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowQuotedNewlines=AValue) then exit;
  FallowQuotedNewlines:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCsvOptions.Setencoding(AIndex : Integer; const AValue : String); 

begin
  If (Fencoding=AValue) then exit;
  Fencoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCsvOptions.SetfieldDelimiter(AIndex : Integer; const AValue : String); 

begin
  If (FfieldDelimiter=AValue) then exit;
  FfieldDelimiter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCsvOptions.Setquote(AIndex : Integer; const AValue : String); 

begin
  If (Fquote=AValue) then exit;
  Fquote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCsvOptions.SetskipLeadingRows(AIndex : Integer; const AValue : String); 

begin
  If (FskipLeadingRows=AValue) then exit;
  FskipLeadingRows:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatasetTypeaccessItem
  --------------------------------------------------------------------}


Procedure TDatasetTypeaccessItem.Setdomain(AIndex : Integer; const AValue : String); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetTypeaccessItem.SetgroupByEmail(AIndex : Integer; const AValue : String); 

begin
  If (FgroupByEmail=AValue) then exit;
  FgroupByEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetTypeaccessItem.Setrole(AIndex : Integer; const AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetTypeaccessItem.SetspecialGroup(AIndex : Integer; const AValue : String); 

begin
  If (FspecialGroup=AValue) then exit;
  FspecialGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetTypeaccessItem.SetuserByEmail(AIndex : Integer; const AValue : String); 

begin
  If (FuserByEmail=AValue) then exit;
  FuserByEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetTypeaccessItem.Setview(AIndex : Integer; const AValue : TTableReference); 

begin
  If (Fview=AValue) then exit;
  Fview:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataset
  --------------------------------------------------------------------}


Procedure TDataset.Setaccess(AIndex : Integer; const AValue : TDatasetTypeaccessArray); 

begin
  If (Faccess=AValue) then exit;
  Faccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetcreationTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetdatasetReference(AIndex : Integer; const AValue : TDatasetReference); 

begin
  If (FdatasetReference=AValue) then exit;
  FdatasetReference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetdefaultTableExpirationMs(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultTableExpirationMs=AValue) then exit;
  FdefaultTableExpirationMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetfriendlyName(AIndex : Integer; const AValue : String); 

begin
  If (FfriendlyName=AValue) then exit;
  FfriendlyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetlastModifiedTime(AIndex : Integer; const AValue : String); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.Setlocation(AIndex : Integer; const AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDataset.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'access' : SetLength(Faccess,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDatasetListTypedatasetsItem
  --------------------------------------------------------------------}


Procedure TDatasetListTypedatasetsItem.SetdatasetReference(AIndex : Integer; const AValue : TDatasetReference); 

begin
  If (FdatasetReference=AValue) then exit;
  FdatasetReference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetListTypedatasetsItem.SetfriendlyName(AIndex : Integer; const AValue : String); 

begin
  If (FfriendlyName=AValue) then exit;
  FfriendlyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetListTypedatasetsItem.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetListTypedatasetsItem.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatasetList
  --------------------------------------------------------------------}


Procedure TDatasetList.Setdatasets(AIndex : Integer; const AValue : TDatasetListTypedatasetsArray); 

begin
  If (Fdatasets=AValue) then exit;
  Fdatasets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetList.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDatasetList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'datasets' : SetLength(Fdatasets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDatasetReference
  --------------------------------------------------------------------}


Procedure TDatasetReference.SetdatasetId(AIndex : Integer; const AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatasetReference.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TErrorProto
  --------------------------------------------------------------------}


Procedure TErrorProto.SetdebugInfo(AIndex : Integer; const AValue : String); 

begin
  If (FdebugInfo=AValue) then exit;
  FdebugInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorProto.Setlocation(AIndex : Integer; const AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorProto.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorProto.Setreason(AIndex : Integer; const AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExplainQueryStage
  --------------------------------------------------------------------}


Procedure TExplainQueryStage.SetcomputeRatioAvg(AIndex : Integer; const AValue : double); 

begin
  If (FcomputeRatioAvg=AValue) then exit;
  FcomputeRatioAvg:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.SetcomputeRatioMax(AIndex : Integer; const AValue : double); 

begin
  If (FcomputeRatioMax=AValue) then exit;
  FcomputeRatioMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.SetreadRatioAvg(AIndex : Integer; const AValue : double); 

begin
  If (FreadRatioAvg=AValue) then exit;
  FreadRatioAvg:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.SetreadRatioMax(AIndex : Integer; const AValue : double); 

begin
  If (FreadRatioMax=AValue) then exit;
  FreadRatioMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.SetrecordsRead(AIndex : Integer; const AValue : String); 

begin
  If (FrecordsRead=AValue) then exit;
  FrecordsRead:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.SetrecordsWritten(AIndex : Integer; const AValue : String); 

begin
  If (FrecordsWritten=AValue) then exit;
  FrecordsWritten:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.Setsteps(AIndex : Integer; const AValue : TExplainQueryStageTypestepsArray); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.SetwaitRatioAvg(AIndex : Integer; const AValue : double); 

begin
  If (FwaitRatioAvg=AValue) then exit;
  FwaitRatioAvg:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.SetwaitRatioMax(AIndex : Integer; const AValue : double); 

begin
  If (FwaitRatioMax=AValue) then exit;
  FwaitRatioMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.SetwriteRatioAvg(AIndex : Integer; const AValue : double); 

begin
  If (FwriteRatioAvg=AValue) then exit;
  FwriteRatioAvg:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStage.SetwriteRatioMax(AIndex : Integer; const AValue : double); 

begin
  If (FwriteRatioMax=AValue) then exit;
  FwriteRatioMax:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExplainQueryStage.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'steps' : SetLength(Fsteps,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExplainQueryStep
  --------------------------------------------------------------------}


Procedure TExplainQueryStep.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExplainQueryStep.Setsubsteps(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fsubsteps=AValue) then exit;
  Fsubsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExplainQueryStep.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'substeps' : SetLength(Fsubsteps,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExternalDataConfiguration
  --------------------------------------------------------------------}


Procedure TExternalDataConfiguration.Setautodetect(AIndex : Integer; const AValue : boolean); 

begin
  If (Fautodetect=AValue) then exit;
  Fautodetect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalDataConfiguration.SetbigtableOptions(AIndex : Integer; const AValue : TBigtableOptions); 

begin
  If (FbigtableOptions=AValue) then exit;
  FbigtableOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalDataConfiguration.Setcompression(AIndex : Integer; const AValue : String); 

begin
  If (Fcompression=AValue) then exit;
  Fcompression:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalDataConfiguration.SetcsvOptions(AIndex : Integer; const AValue : TCsvOptions); 

begin
  If (FcsvOptions=AValue) then exit;
  FcsvOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalDataConfiguration.SetgoogleSheetsOptions(AIndex : Integer; const AValue : TGoogleSheetsOptions); 

begin
  If (FgoogleSheetsOptions=AValue) then exit;
  FgoogleSheetsOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalDataConfiguration.SetignoreUnknownValues(AIndex : Integer; const AValue : boolean); 

begin
  If (FignoreUnknownValues=AValue) then exit;
  FignoreUnknownValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalDataConfiguration.SetmaxBadRecords(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxBadRecords=AValue) then exit;
  FmaxBadRecords:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalDataConfiguration.Setschema(AIndex : Integer; const AValue : TTableSchema); 

begin
  If (Fschema=AValue) then exit;
  Fschema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalDataConfiguration.SetsourceFormat(AIndex : Integer; const AValue : String); 

begin
  If (FsourceFormat=AValue) then exit;
  FsourceFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalDataConfiguration.SetsourceUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExternalDataConfiguration.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourceuris' : SetLength(FsourceUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetQueryResultsResponse
  --------------------------------------------------------------------}


Procedure TGetQueryResultsResponse.SetcacheHit(AIndex : Integer; const AValue : boolean); 

begin
  If (FcacheHit=AValue) then exit;
  FcacheHit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.Seterrors(AIndex : Integer; const AValue : TGetQueryResultsResponseTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.SetjobComplete(AIndex : Integer; const AValue : boolean); 

begin
  If (FjobComplete=AValue) then exit;
  FjobComplete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.SetjobReference(AIndex : Integer; const AValue : TJobReference); 

begin
  If (FjobReference=AValue) then exit;
  FjobReference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.Setrows(AIndex : Integer; const AValue : TGetQueryResultsResponseTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.Setschema(AIndex : Integer; const AValue : TTableSchema); 

begin
  If (Fschema=AValue) then exit;
  Fschema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.SettotalBytesProcessed(AIndex : Integer; const AValue : String); 

begin
  If (FtotalBytesProcessed=AValue) then exit;
  FtotalBytesProcessed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetQueryResultsResponse.SettotalRows(AIndex : Integer; const AValue : String); 

begin
  If (FtotalRows=AValue) then exit;
  FtotalRows:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetQueryResultsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGoogleSheetsOptions
  --------------------------------------------------------------------}


Procedure TGoogleSheetsOptions.SetskipLeadingRows(AIndex : Integer; const AValue : String); 

begin
  If (FskipLeadingRows=AValue) then exit;
  FskipLeadingRows:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.Setconfiguration(AIndex : Integer; const AValue : TJobConfiguration); 

begin
  If (Fconfiguration=AValue) then exit;
  Fconfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetjobReference(AIndex : Integer; const AValue : TJobReference); 

begin
  If (FjobReference=AValue) then exit;
  FjobReference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setstatistics(AIndex : Integer; const AValue : TJobStatistics); 

begin
  If (Fstatistics=AValue) then exit;
  Fstatistics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setstatus(AIndex : Integer; const AValue : TJobStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setuser_email(AIndex : Integer; const AValue : String); 

begin
  If (Fuser_email=AValue) then exit;
  Fuser_email:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobCancelResponse
  --------------------------------------------------------------------}


Procedure TJobCancelResponse.Setjob(AIndex : Integer; const AValue : TJob); 

begin
  If (Fjob=AValue) then exit;
  Fjob:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobCancelResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobConfiguration
  --------------------------------------------------------------------}


Procedure TJobConfiguration.Setcopy(AIndex : Integer; const AValue : TJobConfigurationTableCopy); 

begin
  If (Fcopy=AValue) then exit;
  Fcopy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfiguration.SetdryRun(AIndex : Integer; const AValue : boolean); 

begin
  If (FdryRun=AValue) then exit;
  FdryRun:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfiguration.Setextract(AIndex : Integer; const AValue : TJobConfigurationExtract); 

begin
  If (Fextract=AValue) then exit;
  Fextract:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfiguration.Setload(AIndex : Integer; const AValue : TJobConfigurationLoad); 

begin
  If (Fload=AValue) then exit;
  Fload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfiguration.Setquery(AIndex : Integer; const AValue : TJobConfigurationQuery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobConfigurationExtract
  --------------------------------------------------------------------}


Procedure TJobConfigurationExtract.Setcompression(AIndex : Integer; const AValue : String); 

begin
  If (Fcompression=AValue) then exit;
  Fcompression:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationExtract.SetdestinationFormat(AIndex : Integer; const AValue : String); 

begin
  If (FdestinationFormat=AValue) then exit;
  FdestinationFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationExtract.SetdestinationUri(AIndex : Integer; const AValue : String); 

begin
  If (FdestinationUri=AValue) then exit;
  FdestinationUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationExtract.SetdestinationUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdestinationUris=AValue) then exit;
  FdestinationUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationExtract.SetfieldDelimiter(AIndex : Integer; const AValue : String); 

begin
  If (FfieldDelimiter=AValue) then exit;
  FfieldDelimiter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationExtract.SetprintHeader(AIndex : Integer; const AValue : boolean); 

begin
  If (FprintHeader=AValue) then exit;
  FprintHeader:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationExtract.SetsourceTable(AIndex : Integer; const AValue : TTableReference); 

begin
  If (FsourceTable=AValue) then exit;
  FsourceTable:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobConfigurationExtract.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'destinationuris' : SetLength(FdestinationUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobConfigurationLoad
  --------------------------------------------------------------------}


Procedure TJobConfigurationLoad.SetallowJaggedRows(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowJaggedRows=AValue) then exit;
  FallowJaggedRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetallowQuotedNewlines(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowQuotedNewlines=AValue) then exit;
  FallowQuotedNewlines:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.Setautodetect(AIndex : Integer; const AValue : boolean); 

begin
  If (Fautodetect=AValue) then exit;
  Fautodetect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetcreateDisposition(AIndex : Integer; const AValue : String); 

begin
  If (FcreateDisposition=AValue) then exit;
  FcreateDisposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetdestinationTable(AIndex : Integer; const AValue : TTableReference); 

begin
  If (FdestinationTable=AValue) then exit;
  FdestinationTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.Setencoding(AIndex : Integer; const AValue : String); 

begin
  If (Fencoding=AValue) then exit;
  Fencoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetfieldDelimiter(AIndex : Integer; const AValue : String); 

begin
  If (FfieldDelimiter=AValue) then exit;
  FfieldDelimiter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetignoreUnknownValues(AIndex : Integer; const AValue : boolean); 

begin
  If (FignoreUnknownValues=AValue) then exit;
  FignoreUnknownValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetmaxBadRecords(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxBadRecords=AValue) then exit;
  FmaxBadRecords:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetprojectionFields(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FprojectionFields=AValue) then exit;
  FprojectionFields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.Setquote(AIndex : Integer; const AValue : String); 

begin
  If (Fquote=AValue) then exit;
  Fquote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.Setschema(AIndex : Integer; const AValue : TTableSchema); 

begin
  If (Fschema=AValue) then exit;
  Fschema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetschemaInline(AIndex : Integer; const AValue : String); 

begin
  If (FschemaInline=AValue) then exit;
  FschemaInline:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetschemaInlineFormat(AIndex : Integer; const AValue : String); 

begin
  If (FschemaInlineFormat=AValue) then exit;
  FschemaInlineFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetskipLeadingRows(AIndex : Integer; const AValue : integer); 

begin
  If (FskipLeadingRows=AValue) then exit;
  FskipLeadingRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetsourceFormat(AIndex : Integer; const AValue : String); 

begin
  If (FsourceFormat=AValue) then exit;
  FsourceFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetsourceUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationLoad.SetwriteDisposition(AIndex : Integer; const AValue : String); 

begin
  If (FwriteDisposition=AValue) then exit;
  FwriteDisposition:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobConfigurationLoad.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'projectionfields' : SetLength(FprojectionFields,ALength);
  'sourceuris' : SetLength(FsourceUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobConfigurationQueryTypetableDefinitions
  --------------------------------------------------------------------}


Class Function TJobConfigurationQueryTypetableDefinitions.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TJobConfigurationQuery
  --------------------------------------------------------------------}


Procedure TJobConfigurationQuery.SetallowLargeResults(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowLargeResults=AValue) then exit;
  FallowLargeResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetcreateDisposition(AIndex : Integer; const AValue : String); 

begin
  If (FcreateDisposition=AValue) then exit;
  FcreateDisposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetdefaultDataset(AIndex : Integer; const AValue : TDatasetReference); 

begin
  If (FdefaultDataset=AValue) then exit;
  FdefaultDataset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetdestinationTable(AIndex : Integer; const AValue : TTableReference); 

begin
  If (FdestinationTable=AValue) then exit;
  FdestinationTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetflattenResults(AIndex : Integer; const AValue : boolean); 

begin
  If (FflattenResults=AValue) then exit;
  FflattenResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetmaximumBillingTier(AIndex : Integer; const AValue : integer); 

begin
  If (FmaximumBillingTier=AValue) then exit;
  FmaximumBillingTier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetpreserveNulls(AIndex : Integer; const AValue : boolean); 

begin
  If (FpreserveNulls=AValue) then exit;
  FpreserveNulls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.Setpriority(AIndex : Integer; const AValue : String); 

begin
  If (Fpriority=AValue) then exit;
  Fpriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.Setquery(AIndex : Integer; const AValue : String); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SettableDefinitions(AIndex : Integer; const AValue : TJobConfigurationQueryTypetableDefinitions); 

begin
  If (FtableDefinitions=AValue) then exit;
  FtableDefinitions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetuseLegacySql(AIndex : Integer; const AValue : boolean); 

begin
  If (FuseLegacySql=AValue) then exit;
  FuseLegacySql:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetuseQueryCache(AIndex : Integer; const AValue : boolean); 

begin
  If (FuseQueryCache=AValue) then exit;
  FuseQueryCache:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetuserDefinedFunctionResources(AIndex : Integer; const AValue : TJobConfigurationQueryTypeuserDefinedFunctionResourcesArray); 

begin
  If (FuserDefinedFunctionResources=AValue) then exit;
  FuserDefinedFunctionResources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationQuery.SetwriteDisposition(AIndex : Integer; const AValue : String); 

begin
  If (FwriteDisposition=AValue) then exit;
  FwriteDisposition:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobConfigurationQuery.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'userdefinedfunctionresources' : SetLength(FuserDefinedFunctionResources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobConfigurationTableCopy
  --------------------------------------------------------------------}


Procedure TJobConfigurationTableCopy.SetcreateDisposition(AIndex : Integer; const AValue : String); 

begin
  If (FcreateDisposition=AValue) then exit;
  FcreateDisposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationTableCopy.SetdestinationTable(AIndex : Integer; const AValue : TTableReference); 

begin
  If (FdestinationTable=AValue) then exit;
  FdestinationTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationTableCopy.SetsourceTable(AIndex : Integer; const AValue : TTableReference); 

begin
  If (FsourceTable=AValue) then exit;
  FsourceTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationTableCopy.SetsourceTables(AIndex : Integer; const AValue : TJobConfigurationTableCopyTypesourceTablesArray); 

begin
  If (FsourceTables=AValue) then exit;
  FsourceTables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobConfigurationTableCopy.SetwriteDisposition(AIndex : Integer; const AValue : String); 

begin
  If (FwriteDisposition=AValue) then exit;
  FwriteDisposition:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobConfigurationTableCopy.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourcetables' : SetLength(FsourceTables,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobListTypejobsItem
  --------------------------------------------------------------------}


Procedure TJobListTypejobsItem.Setconfiguration(AIndex : Integer; const AValue : TJobConfiguration); 

begin
  If (Fconfiguration=AValue) then exit;
  Fconfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListTypejobsItem.SeterrorResult(AIndex : Integer; const AValue : TErrorProto); 

begin
  If (FerrorResult=AValue) then exit;
  FerrorResult:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListTypejobsItem.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListTypejobsItem.SetjobReference(AIndex : Integer; const AValue : TJobReference); 

begin
  If (FjobReference=AValue) then exit;
  FjobReference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListTypejobsItem.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListTypejobsItem.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListTypejobsItem.Setstatistics(AIndex : Integer; const AValue : TJobStatistics); 

begin
  If (Fstatistics=AValue) then exit;
  Fstatistics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListTypejobsItem.Setstatus(AIndex : Integer; const AValue : TJobStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobListTypejobsItem.Setuser_email(AIndex : Integer; const AValue : String); 

begin
  If (Fuser_email=AValue) then exit;
  Fuser_email:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobList
  --------------------------------------------------------------------}


Procedure TJobList.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobList.Setjobs(AIndex : Integer; const AValue : TJobListTypejobsArray); 

begin
  If (Fjobs=AValue) then exit;
  Fjobs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'jobs' : SetLength(Fjobs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobReference
  --------------------------------------------------------------------}


Procedure TJobReference.SetjobId(AIndex : Integer; const AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobReference.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobStatistics
  --------------------------------------------------------------------}


Procedure TJobStatistics.SetcreationTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics.Setextract(AIndex : Integer; const AValue : TJobStatistics4); 

begin
  If (Fextract=AValue) then exit;
  Fextract:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics.Setload(AIndex : Integer; const AValue : TJobStatistics3); 

begin
  If (Fload=AValue) then exit;
  Fload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics.Setquery(AIndex : Integer; const AValue : TJobStatistics2); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics.SettotalBytesProcessed(AIndex : Integer; const AValue : String); 

begin
  If (FtotalBytesProcessed=AValue) then exit;
  FtotalBytesProcessed:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobStatistics2
  --------------------------------------------------------------------}


Procedure TJobStatistics2.SetbillingTier(AIndex : Integer; const AValue : integer); 

begin
  If (FbillingTier=AValue) then exit;
  FbillingTier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics2.SetcacheHit(AIndex : Integer; const AValue : boolean); 

begin
  If (FcacheHit=AValue) then exit;
  FcacheHit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics2.SetqueryPlan(AIndex : Integer; const AValue : TJobStatistics2TypequeryPlanArray); 

begin
  If (FqueryPlan=AValue) then exit;
  FqueryPlan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics2.SetreferencedTables(AIndex : Integer; const AValue : TJobStatistics2TypereferencedTablesArray); 

begin
  If (FreferencedTables=AValue) then exit;
  FreferencedTables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics2.Setschema(AIndex : Integer; const AValue : TTableSchema); 

begin
  If (Fschema=AValue) then exit;
  Fschema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics2.SettotalBytesBilled(AIndex : Integer; const AValue : String); 

begin
  If (FtotalBytesBilled=AValue) then exit;
  FtotalBytesBilled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics2.SettotalBytesProcessed(AIndex : Integer; const AValue : String); 

begin
  If (FtotalBytesProcessed=AValue) then exit;
  FtotalBytesProcessed:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobStatistics2.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'queryplan' : SetLength(FqueryPlan,ALength);
  'referencedtables' : SetLength(FreferencedTables,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobStatistics3
  --------------------------------------------------------------------}


Procedure TJobStatistics3.SetinputFileBytes(AIndex : Integer; const AValue : String); 

begin
  If (FinputFileBytes=AValue) then exit;
  FinputFileBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics3.SetinputFiles(AIndex : Integer; const AValue : String); 

begin
  If (FinputFiles=AValue) then exit;
  FinputFiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics3.SetoutputBytes(AIndex : Integer; const AValue : String); 

begin
  If (FoutputBytes=AValue) then exit;
  FoutputBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatistics3.SetoutputRows(AIndex : Integer; const AValue : String); 

begin
  If (FoutputRows=AValue) then exit;
  FoutputRows:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobStatistics4
  --------------------------------------------------------------------}


Procedure TJobStatistics4.SetdestinationUriFileCounts(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdestinationUriFileCounts=AValue) then exit;
  FdestinationUriFileCounts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobStatistics4.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'destinationurifilecounts' : SetLength(FdestinationUriFileCounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobStatus
  --------------------------------------------------------------------}


Procedure TJobStatus.SeterrorResult(AIndex : Integer; const AValue : TErrorProto); 

begin
  If (FerrorResult=AValue) then exit;
  FerrorResult:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatus.Seterrors(AIndex : Integer; const AValue : TJobStatusTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatus.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJsonObject
  --------------------------------------------------------------------}


Class Function TJsonObject.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TProjectListTypeprojectsItem
  --------------------------------------------------------------------}


Procedure TProjectListTypeprojectsItem.SetfriendlyName(AIndex : Integer; const AValue : String); 

begin
  If (FfriendlyName=AValue) then exit;
  FfriendlyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectListTypeprojectsItem.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectListTypeprojectsItem.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectListTypeprojectsItem.SetnumericId(AIndex : Integer; const AValue : String); 

begin
  If (FnumericId=AValue) then exit;
  FnumericId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectListTypeprojectsItem.SetprojectReference(AIndex : Integer; const AValue : TProjectReference); 

begin
  If (FprojectReference=AValue) then exit;
  FprojectReference:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectList
  --------------------------------------------------------------------}


Procedure TProjectList.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectList.Setprojects(AIndex : Integer; const AValue : TProjectListTypeprojectsArray); 

begin
  If (Fprojects=AValue) then exit;
  Fprojects:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectList.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TProjectList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'projects' : SetLength(Fprojects,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProjectReference
  --------------------------------------------------------------------}


Procedure TProjectReference.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQueryRequest
  --------------------------------------------------------------------}


Procedure TQueryRequest.SetdefaultDataset(AIndex : Integer; const AValue : TDatasetReference); 

begin
  If (FdefaultDataset=AValue) then exit;
  FdefaultDataset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRequest.SetdryRun(AIndex : Integer; const AValue : boolean); 

begin
  If (FdryRun=AValue) then exit;
  FdryRun:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRequest.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRequest.SetmaxResults(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxResults=AValue) then exit;
  FmaxResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRequest.SetpreserveNulls(AIndex : Integer; const AValue : boolean); 

begin
  If (FpreserveNulls=AValue) then exit;
  FpreserveNulls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRequest.Setquery(AIndex : Integer; const AValue : String); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRequest.SettimeoutMs(AIndex : Integer; const AValue : integer); 

begin
  If (FtimeoutMs=AValue) then exit;
  FtimeoutMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRequest.SetuseLegacySql(AIndex : Integer; const AValue : boolean); 

begin
  If (FuseLegacySql=AValue) then exit;
  FuseLegacySql:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRequest.SetuseQueryCache(AIndex : Integer; const AValue : boolean); 

begin
  If (FuseQueryCache=AValue) then exit;
  FuseQueryCache:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQueryResponse
  --------------------------------------------------------------------}


Procedure TQueryResponse.SetcacheHit(AIndex : Integer; const AValue : boolean); 

begin
  If (FcacheHit=AValue) then exit;
  FcacheHit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResponse.Seterrors(AIndex : Integer; const AValue : TQueryResponseTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResponse.SetjobComplete(AIndex : Integer; const AValue : boolean); 

begin
  If (FjobComplete=AValue) then exit;
  FjobComplete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResponse.SetjobReference(AIndex : Integer; const AValue : TJobReference); 

begin
  If (FjobReference=AValue) then exit;
  FjobReference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResponse.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResponse.Setrows(AIndex : Integer; const AValue : TQueryResponseTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResponse.Setschema(AIndex : Integer; const AValue : TTableSchema); 

begin
  If (Fschema=AValue) then exit;
  Fschema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResponse.SettotalBytesProcessed(AIndex : Integer; const AValue : String); 

begin
  If (FtotalBytesProcessed=AValue) then exit;
  FtotalBytesProcessed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResponse.SettotalRows(AIndex : Integer; const AValue : String); 

begin
  If (FtotalRows=AValue) then exit;
  FtotalRows:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TQueryResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStreamingbuffer
  --------------------------------------------------------------------}


Procedure TStreamingbuffer.SetestimatedBytes(AIndex : Integer; const AValue : String); 

begin
  If (FestimatedBytes=AValue) then exit;
  FestimatedBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingbuffer.SetestimatedRows(AIndex : Integer; const AValue : String); 

begin
  If (FestimatedRows=AValue) then exit;
  FestimatedRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingbuffer.SetoldestEntryTime(AIndex : Integer; const AValue : String); 

begin
  If (FoldestEntryTime=AValue) then exit;
  FoldestEntryTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTable
  --------------------------------------------------------------------}


Procedure TTable.SetcreationTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetexpirationTime(AIndex : Integer; const AValue : String); 

begin
  If (FexpirationTime=AValue) then exit;
  FexpirationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetexternalDataConfiguration(AIndex : Integer; const AValue : TExternalDataConfiguration); 

begin
  If (FexternalDataConfiguration=AValue) then exit;
  FexternalDataConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetfriendlyName(AIndex : Integer; const AValue : String); 

begin
  If (FfriendlyName=AValue) then exit;
  FfriendlyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetlastModifiedTime(AIndex : Integer; const AValue : String); 

begin
  If (FlastModifiedTime=AValue) then exit;
  FlastModifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setlocation(AIndex : Integer; const AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetnumBytes(AIndex : Integer; const AValue : String); 

begin
  If (FnumBytes=AValue) then exit;
  FnumBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetnumLongTermBytes(AIndex : Integer; const AValue : String); 

begin
  If (FnumLongTermBytes=AValue) then exit;
  FnumLongTermBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetnumRows(AIndex : Integer; const AValue : String); 

begin
  If (FnumRows=AValue) then exit;
  FnumRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setschema(AIndex : Integer; const AValue : TTableSchema); 

begin
  If (Fschema=AValue) then exit;
  Fschema:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SetstreamingBuffer(AIndex : Integer; const AValue : TStreamingbuffer); 

begin
  If (FstreamingBuffer=AValue) then exit;
  FstreamingBuffer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SettableReference(AIndex : Integer; const AValue : TTableReference); 

begin
  If (FtableReference=AValue) then exit;
  FtableReference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.SettimePartitioning(AIndex : Integer; const AValue : TTimePartitioning); 

begin
  If (FtimePartitioning=AValue) then exit;
  FtimePartitioning:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTable.Setview(AIndex : Integer; const AValue : TViewDefinition); 

begin
  If (Fview=AValue) then exit;
  Fview:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTable.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TTableCell
  --------------------------------------------------------------------}


Procedure TTableCell.Setv(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (Fv=AValue) then exit;
  Fv:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTableDataInsertAllRequestTyperowsItem
  --------------------------------------------------------------------}


Procedure TTableDataInsertAllRequestTyperowsItem.SetinsertId(AIndex : Integer; const AValue : String); 

begin
  If (FinsertId=AValue) then exit;
  FinsertId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataInsertAllRequestTyperowsItem.Setjson(AIndex : Integer; const AValue : TJsonObject); 

begin
  If (Fjson=AValue) then exit;
  Fjson:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTableDataInsertAllRequest
  --------------------------------------------------------------------}


Procedure TTableDataInsertAllRequest.SetignoreUnknownValues(AIndex : Integer; const AValue : boolean); 

begin
  If (FignoreUnknownValues=AValue) then exit;
  FignoreUnknownValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataInsertAllRequest.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataInsertAllRequest.Setrows(AIndex : Integer; const AValue : TTableDataInsertAllRequestTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataInsertAllRequest.SetskipInvalidRows(AIndex : Integer; const AValue : boolean); 

begin
  If (FskipInvalidRows=AValue) then exit;
  FskipInvalidRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataInsertAllRequest.SettemplateSuffix(AIndex : Integer; const AValue : String); 

begin
  If (FtemplateSuffix=AValue) then exit;
  FtemplateSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTableDataInsertAllRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTableDataInsertAllResponseTypeinsertErrorsItem
  --------------------------------------------------------------------}


Procedure TTableDataInsertAllResponseTypeinsertErrorsItem.Seterrors(AIndex : Integer; const AValue : TTableDataInsertAllResponseTypeinsertErrorsItemTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataInsertAllResponseTypeinsertErrorsItem.Setindex(AIndex : Integer; const AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTableDataInsertAllResponseTypeinsertErrorsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTableDataInsertAllResponse
  --------------------------------------------------------------------}


Procedure TTableDataInsertAllResponse.SetinsertErrors(AIndex : Integer; const AValue : TTableDataInsertAllResponseTypeinsertErrorsArray); 

begin
  If (FinsertErrors=AValue) then exit;
  FinsertErrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataInsertAllResponse.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTableDataInsertAllResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'inserterrors' : SetLength(FinsertErrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTableDataList
  --------------------------------------------------------------------}


Procedure TTableDataList.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataList.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataList.Setrows(AIndex : Integer; const AValue : TTableDataListTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableDataList.SettotalRows(AIndex : Integer; const AValue : String); 

begin
  If (FtotalRows=AValue) then exit;
  FtotalRows:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTableDataList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTableFieldSchema
  --------------------------------------------------------------------}


Procedure TTableFieldSchema.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableFieldSchema.Setfields(AIndex : Integer; const AValue : TTableFieldSchemaTypefieldsArray); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableFieldSchema.Setmode(AIndex : Integer; const AValue : String); 

begin
  If (Fmode=AValue) then exit;
  Fmode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableFieldSchema.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableFieldSchema.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTableFieldSchema.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTableFieldSchema.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'fields' : SetLength(Ffields,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTableListTypetablesItem
  --------------------------------------------------------------------}


Procedure TTableListTypetablesItem.SetfriendlyName(AIndex : Integer; const AValue : String); 

begin
  If (FfriendlyName=AValue) then exit;
  FfriendlyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableListTypetablesItem.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableListTypetablesItem.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableListTypetablesItem.SettableReference(AIndex : Integer; const AValue : TTableReference); 

begin
  If (FtableReference=AValue) then exit;
  FtableReference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableListTypetablesItem.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTableListTypetablesItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TTableList
  --------------------------------------------------------------------}


Procedure TTableList.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableList.Settables(AIndex : Integer; const AValue : TTableListTypetablesArray); 

begin
  If (Ftables=AValue) then exit;
  Ftables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableList.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTableList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'tables' : SetLength(Ftables,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTableReference
  --------------------------------------------------------------------}


Procedure TTableReference.SetdatasetId(AIndex : Integer; const AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableReference.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTableReference.SettableId(AIndex : Integer; const AValue : String); 

begin
  If (FtableId=AValue) then exit;
  FtableId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTableRow
  --------------------------------------------------------------------}


Procedure TTableRow.Setf(AIndex : Integer; const AValue : TTableRowTypefArray); 

begin
  If (Ff=AValue) then exit;
  Ff:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTableRow.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'f' : SetLength(Ff,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTableSchema
  --------------------------------------------------------------------}


Procedure TTableSchema.Setfields(AIndex : Integer; const AValue : TTableSchemaTypefieldsArray); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTableSchema.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'fields' : SetLength(Ffields,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTimePartitioning
  --------------------------------------------------------------------}


Procedure TTimePartitioning.SetexpirationMs(AIndex : Integer; const AValue : String); 

begin
  If (FexpirationMs=AValue) then exit;
  FexpirationMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimePartitioning.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTimePartitioning.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TUserDefinedFunctionResource
  --------------------------------------------------------------------}


Procedure TUserDefinedFunctionResource.SetinlineCode(AIndex : Integer; const AValue : String); 

begin
  If (FinlineCode=AValue) then exit;
  FinlineCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserDefinedFunctionResource.SetresourceUri(AIndex : Integer; const AValue : String); 

begin
  If (FresourceUri=AValue) then exit;
  FresourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TViewDefinition
  --------------------------------------------------------------------}


Procedure TViewDefinition.Setquery(AIndex : Integer; const AValue : String); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TViewDefinition.SetuserDefinedFunctionResources(AIndex : Integer; const AValue : TViewDefinitionTypeuserDefinedFunctionResourcesArray); 

begin
  If (FuserDefinedFunctionResources=AValue) then exit;
  FuserDefinedFunctionResources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TViewDefinition.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'userdefinedfunctionresources' : SetLength(FuserDefinedFunctionResources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDatasetsResource
  --------------------------------------------------------------------}


Class Function TDatasetsResource.ResourceName : String;

begin
  Result:='datasets';
end;

Class Function TDatasetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbigqueryAPI;
end;

Procedure TDatasetsResource.Delete(datasetId: string; projectId: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'projects/{projectId}/datasets/{datasetId}';
  _Methodid   = 'bigquery.datasets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TDatasetsResource.Delete(datasetId: string; projectId: string; AQuery : TDatasetsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'deleteContents',AQuery.deleteContents);
  Delete(datasetId,projectId,_Q);
end;

Function TDatasetsResource.Get(datasetId: string; projectId: string) : TDataset;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{projectId}/datasets/{datasetId}';
  _Methodid   = 'bigquery.datasets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDataset) as TDataset;
end;

Function TDatasetsResource.Insert(projectId: string; aDataset : TDataset) : TDataset;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{projectId}/datasets';
  _Methodid   = 'bigquery.datasets.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDataset,TDataset) as TDataset;
end;

Function TDatasetsResource.List(projectId: string; AQuery : string = '') : TDatasetList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{projectId}/datasets';
  _Methodid   = 'bigquery.datasets.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDatasetList) as TDatasetList;
end;


Function TDatasetsResource.List(projectId: string; AQuery : TDatasetslistOptions) : TDatasetList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'all',AQuery.all);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(projectId,_Q);
end;

Function TDatasetsResource.Patch(datasetId: string; projectId: string; aDataset : TDataset) : TDataset;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'projects/{projectId}/datasets/{datasetId}';
  _Methodid   = 'bigquery.datasets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDataset,TDataset) as TDataset;
end;

Function TDatasetsResource.Update(datasetId: string; projectId: string; aDataset : TDataset) : TDataset;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'projects/{projectId}/datasets/{datasetId}';
  _Methodid   = 'bigquery.datasets.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDataset,TDataset) as TDataset;
end;



{ --------------------------------------------------------------------
  TJobsResource
  --------------------------------------------------------------------}


Class Function TJobsResource.ResourceName : String;

begin
  Result:='jobs';
end;

Class Function TJobsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbigqueryAPI;
end;

Function TJobsResource.Cancel(jobId: string; projectId: string) : TJobCancelResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'project/{projectId}/jobs/{jobId}/cancel';
  _Methodid   = 'bigquery.jobs.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TJobCancelResponse) as TJobCancelResponse;
end;

Function TJobsResource.Get(jobId: string; projectId: string) : TJob;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{projectId}/jobs/{jobId}';
  _Methodid   = 'bigquery.jobs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TJob) as TJob;
end;

Function TJobsResource.GetQueryResults(jobId: string; projectId: string; AQuery : string = '') : TGetQueryResultsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{projectId}/queries/{jobId}';
  _Methodid   = 'bigquery.jobs.getQueryResults';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TGetQueryResultsResponse) as TGetQueryResultsResponse;
end;


Function TJobsResource.GetQueryResults(jobId: string; projectId: string; AQuery : TJobsgetQueryResultsOptions) : TGetQueryResultsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  AddToQuery(_Q,'timeoutMs',AQuery.timeoutMs);
  Result:=GetQueryResults(jobId,projectId,_Q);
end;

Function TJobsResource.Insert(projectId: string; aJob : TJob) : TJob;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{projectId}/jobs';
  _Methodid   = 'bigquery.jobs.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aJob,TJob) as TJob;
end;

Function TJobsResource.List(projectId: string; AQuery : string = '') : TJobList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{projectId}/jobs';
  _Methodid   = 'bigquery.jobs.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TJobList) as TJobList;
end;


Function TJobsResource.List(projectId: string; AQuery : TJobslistOptions) : TJobList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'allUsers',AQuery.allUsers);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'stateFilter',AQuery.stateFilter);
  Result:=List(projectId,_Q);
end;

Function TJobsResource.Query(projectId: string; aQueryRequest : TQueryRequest) : TQueryResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{projectId}/queries';
  _Methodid   = 'bigquery.jobs.query';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aQueryRequest,TQueryResponse) as TQueryResponse;
end;



{ --------------------------------------------------------------------
  TProjectsResource
  --------------------------------------------------------------------}


Class Function TProjectsResource.ResourceName : String;

begin
  Result:='projects';
end;

Class Function TProjectsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbigqueryAPI;
end;

Function TProjectsResource.List(AQuery : string = '') : TProjectList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects';
  _Methodid   = 'bigquery.projects.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TProjectList) as TProjectList;
end;


Function TProjectsResource.List(AQuery : TProjectslistOptions) : TProjectList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TTabledataResource
  --------------------------------------------------------------------}


Class Function TTabledataResource.ResourceName : String;

begin
  Result:='tabledata';
end;

Class Function TTabledataResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbigqueryAPI;
end;

Function TTabledataResource.InsertAll(datasetId: string; projectId: string; tableId: string; aTableDataInsertAllRequest : TTableDataInsertAllRequest) : TTableDataInsertAllResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{projectId}/datasets/{datasetId}/tables/{tableId}/insertAll';
  _Methodid   = 'bigquery.tabledata.insertAll';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTableDataInsertAllRequest,TTableDataInsertAllResponse) as TTableDataInsertAllResponse;
end;

Function TTabledataResource.List(datasetId: string; projectId: string; tableId: string; AQuery : string = '') : TTableDataList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{projectId}/datasets/{datasetId}/tables/{tableId}/data';
  _Methodid   = 'bigquery.tabledata.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTableDataList) as TTableDataList;
end;


Function TTabledataResource.List(datasetId: string; projectId: string; tableId: string; AQuery : TTabledatalistOptions) : TTableDataList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  Result:=List(datasetId,projectId,tableId,_Q);
end;



{ --------------------------------------------------------------------
  TTablesResource
  --------------------------------------------------------------------}


Class Function TTablesResource.ResourceName : String;

begin
  Result:='tables';
end;

Class Function TTablesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbigqueryAPI;
end;

Procedure TTablesResource.Delete(datasetId: string; projectId: string; tableId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'projects/{projectId}/datasets/{datasetId}/tables/{tableId}';
  _Methodid   = 'bigquery.tables.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId,'tableId',tableId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTablesResource.Get(datasetId: string; projectId: string; tableId: string) : TTable;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{projectId}/datasets/{datasetId}/tables/{tableId}';
  _Methodid   = 'bigquery.tables.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTable) as TTable;
end;

Function TTablesResource.Insert(datasetId: string; projectId: string; aTable : TTable) : TTable;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{projectId}/datasets/{datasetId}/tables';
  _Methodid   = 'bigquery.tables.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTable,TTable) as TTable;
end;

Function TTablesResource.List(datasetId: string; projectId: string; AQuery : string = '') : TTableList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{projectId}/datasets/{datasetId}/tables';
  _Methodid   = 'bigquery.tables.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTableList) as TTableList;
end;


Function TTablesResource.List(datasetId: string; projectId: string; AQuery : TTableslistOptions) : TTableList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(datasetId,projectId,_Q);
end;

Function TTablesResource.Patch(datasetId: string; projectId: string; tableId: string; aTable : TTable) : TTable;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'projects/{projectId}/datasets/{datasetId}/tables/{tableId}';
  _Methodid   = 'bigquery.tables.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTable,TTable) as TTable;
end;

Function TTablesResource.Update(datasetId: string; projectId: string; tableId: string; aTable : TTable) : TTable;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'projects/{projectId}/datasets/{datasetId}/tables/{tableId}';
  _Methodid   = 'bigquery.tables.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId,'projectId',projectId,'tableId',tableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTable,TTable) as TTable;
end;



{ --------------------------------------------------------------------
  TBigqueryAPI
  --------------------------------------------------------------------}

Class Function TBigqueryAPI.APIName : String;

begin
  Result:='bigquery';
end;

Class Function TBigqueryAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TBigqueryAPI.APIRevision : String;

begin
  Result:='20160511';
end;

Class Function TBigqueryAPI.APIID : String;

begin
  Result:='bigquery:v2';
end;

Class Function TBigqueryAPI.APITitle : String;

begin
  Result:='BigQuery API';
end;

Class Function TBigqueryAPI.APIDescription : String;

begin
  Result:='A data platform for customers to create, manage, share and query data.';
end;

Class Function TBigqueryAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TBigqueryAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TBigqueryAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TBigqueryAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TBigqueryAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/bigquery/';
end;

Class Function TBigqueryAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TBigqueryAPI.APIbasePath : string;

begin
  Result:='/bigquery/v2/';
end;

Class Function TBigqueryAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/bigquery/v2/';
end;

Class Function TBigqueryAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TBigqueryAPI.APIservicePath : string;

begin
  Result:='bigquery/v2/';
end;

Class Function TBigqueryAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TBigqueryAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,7);
  Result[0].Name:='https://www.googleapis.com/auth/bigquery';
  Result[0].Description:='View and manage your data in Google BigQuery';
  Result[1].Name:='https://www.googleapis.com/auth/bigquery.insertdata';
  Result[1].Description:='Insert data into Google BigQuery';
  Result[2].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[2].Description:='View and manage your data across Google Cloud Platform services';
  Result[3].Name:='https://www.googleapis.com/auth/cloud-platform.read-only';
  Result[3].Description:='View your data across Google Cloud Platform services';
  Result[4].Name:='https://www.googleapis.com/auth/devstorage.full_control';
  Result[4].Description:='Manage your data and permissions in Google Cloud Storage';
  Result[5].Name:='https://www.googleapis.com/auth/devstorage.read_only';
  Result[5].Description:='View your data in Google Cloud Storage';
  Result[6].Name:='https://www.googleapis.com/auth/devstorage.read_write';
  Result[6].Description:='Manage your data in Google Cloud Storage';
  
end;

Class Function TBigqueryAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TBigqueryAPI.RegisterAPIResources;

begin
  TBigtableColumn.RegisterObject;
  TBigtableColumnFamily.RegisterObject;
  TBigtableOptions.RegisterObject;
  TCsvOptions.RegisterObject;
  TDatasetTypeaccessItem.RegisterObject;
  TDataset.RegisterObject;
  TDatasetListTypedatasetsItem.RegisterObject;
  TDatasetList.RegisterObject;
  TDatasetReference.RegisterObject;
  TErrorProto.RegisterObject;
  TExplainQueryStage.RegisterObject;
  TExplainQueryStep.RegisterObject;
  TExternalDataConfiguration.RegisterObject;
  TGetQueryResultsResponse.RegisterObject;
  TGoogleSheetsOptions.RegisterObject;
  TJob.RegisterObject;
  TJobCancelResponse.RegisterObject;
  TJobConfiguration.RegisterObject;
  TJobConfigurationExtract.RegisterObject;
  TJobConfigurationLoad.RegisterObject;
  TJobConfigurationQueryTypetableDefinitions.RegisterObject;
  TJobConfigurationQuery.RegisterObject;
  TJobConfigurationTableCopy.RegisterObject;
  TJobListTypejobsItem.RegisterObject;
  TJobList.RegisterObject;
  TJobReference.RegisterObject;
  TJobStatistics.RegisterObject;
  TJobStatistics2.RegisterObject;
  TJobStatistics3.RegisterObject;
  TJobStatistics4.RegisterObject;
  TJobStatus.RegisterObject;
  TJsonObject.RegisterObject;
  TProjectListTypeprojectsItem.RegisterObject;
  TProjectList.RegisterObject;
  TProjectReference.RegisterObject;
  TQueryRequest.RegisterObject;
  TQueryResponse.RegisterObject;
  TStreamingbuffer.RegisterObject;
  TTable.RegisterObject;
  TTableCell.RegisterObject;
  TTableDataInsertAllRequestTyperowsItem.RegisterObject;
  TTableDataInsertAllRequest.RegisterObject;
  TTableDataInsertAllResponseTypeinsertErrorsItem.RegisterObject;
  TTableDataInsertAllResponse.RegisterObject;
  TTableDataList.RegisterObject;
  TTableFieldSchema.RegisterObject;
  TTableListTypetablesItem.RegisterObject;
  TTableList.RegisterObject;
  TTableReference.RegisterObject;
  TTableRow.RegisterObject;
  TTableSchema.RegisterObject;
  TTimePartitioning.RegisterObject;
  TUserDefinedFunctionResource.RegisterObject;
  TViewDefinition.RegisterObject;
end;


Function TBigqueryAPI.GetDatasetsInstance : TDatasetsResource;

begin
  if (FDatasetsInstance=Nil) then
    FDatasetsInstance:=CreateDatasetsResource;
  Result:=FDatasetsInstance;
end;

Function TBigqueryAPI.CreateDatasetsResource : TDatasetsResource;

begin
  Result:=CreateDatasetsResource(Self);
end;


Function TBigqueryAPI.CreateDatasetsResource(AOwner : TComponent) : TDatasetsResource;

begin
  Result:=TDatasetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBigqueryAPI.GetJobsInstance : TJobsResource;

begin
  if (FJobsInstance=Nil) then
    FJobsInstance:=CreateJobsResource;
  Result:=FJobsInstance;
end;

Function TBigqueryAPI.CreateJobsResource : TJobsResource;

begin
  Result:=CreateJobsResource(Self);
end;


Function TBigqueryAPI.CreateJobsResource(AOwner : TComponent) : TJobsResource;

begin
  Result:=TJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBigqueryAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TBigqueryAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TBigqueryAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBigqueryAPI.GetTabledataInstance : TTabledataResource;

begin
  if (FTabledataInstance=Nil) then
    FTabledataInstance:=CreateTabledataResource;
  Result:=FTabledataInstance;
end;

Function TBigqueryAPI.CreateTabledataResource : TTabledataResource;

begin
  Result:=CreateTabledataResource(Self);
end;


Function TBigqueryAPI.CreateTabledataResource(AOwner : TComponent) : TTabledataResource;

begin
  Result:=TTabledataResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBigqueryAPI.GetTablesInstance : TTablesResource;

begin
  if (FTablesInstance=Nil) then
    FTablesInstance:=CreateTablesResource;
  Result:=FTablesInstance;
end;

Function TBigqueryAPI.CreateTablesResource : TTablesResource;

begin
  Result:=CreateTablesResource(Self);
end;


Function TBigqueryAPI.CreateTablesResource(AOwner : TComponent) : TTablesResource;

begin
  Result:=TTablesResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TBigqueryAPI.RegisterAPI;
end.
