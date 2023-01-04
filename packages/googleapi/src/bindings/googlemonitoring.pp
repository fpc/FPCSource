unit googlemonitoring;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TCreateCollectdTimeSeriesRequest = Class;
  TMonitoredResource = Class;
  TCollectdPayload = Class;
  TCollectdValue = Class;
  TTypedValue = Class;
  TDistribution = Class;
  TRange = Class;
  TBucketOptions = Class;
  TLinear = Class;
  TExponential = Class;
  TExplicit = Class;
  TEmpty = Class;
  TListGroupsResponse = Class;
  TGroup = Class;
  TListGroupMembersResponse = Class;
  TListMonitoredResourceDescriptorsResponse = Class;
  TMonitoredResourceDescriptor = Class;
  TLabelDescriptor = Class;
  TListMetricDescriptorsResponse = Class;
  TMetricDescriptor = Class;
  TListTimeSeriesResponse = Class;
  TTimeSeries = Class;
  TMetric = Class;
  TPoint = Class;
  TTimeInterval = Class;
  TCreateTimeSeriesRequest = Class;
  TType = Class;
  TField = Class;
  TOption = Class;
  TSourceContext = Class;
  TCreateCollectdTimeSeriesRequestArray = Array of TCreateCollectdTimeSeriesRequest;
  TMonitoredResourceArray = Array of TMonitoredResource;
  TCollectdPayloadArray = Array of TCollectdPayload;
  TCollectdValueArray = Array of TCollectdValue;
  TTypedValueArray = Array of TTypedValue;
  TDistributionArray = Array of TDistribution;
  TRangeArray = Array of TRange;
  TBucketOptionsArray = Array of TBucketOptions;
  TLinearArray = Array of TLinear;
  TExponentialArray = Array of TExponential;
  TExplicitArray = Array of TExplicit;
  TEmptyArray = Array of TEmpty;
  TListGroupsResponseArray = Array of TListGroupsResponse;
  TGroupArray = Array of TGroup;
  TListGroupMembersResponseArray = Array of TListGroupMembersResponse;
  TListMonitoredResourceDescriptorsResponseArray = Array of TListMonitoredResourceDescriptorsResponse;
  TMonitoredResourceDescriptorArray = Array of TMonitoredResourceDescriptor;
  TLabelDescriptorArray = Array of TLabelDescriptor;
  TListMetricDescriptorsResponseArray = Array of TListMetricDescriptorsResponse;
  TMetricDescriptorArray = Array of TMetricDescriptor;
  TListTimeSeriesResponseArray = Array of TListTimeSeriesResponse;
  TTimeSeriesArray = Array of TTimeSeries;
  TMetricArray = Array of TMetric;
  TPointArray = Array of TPoint;
  TTimeIntervalArray = Array of TTimeInterval;
  TCreateTimeSeriesRequestArray = Array of TCreateTimeSeriesRequest;
  TTypeArray = Array of TType;
  TFieldArray = Array of TField;
  TOptionArray = Array of TOption;
  TSourceContextArray = Array of TSourceContext;
  //Anonymous types, using auto-generated names
  TMonitoredResourceTypelabels = Class;
  TCollectdPayloadTypemetadata = Class;
  TMetricTypelabels = Class;
  TOptionTypevalue = Class;
  TCreateCollectdTimeSeriesRequestTypecollectdPayloadsArray = Array of TCollectdPayload;
  TCollectdPayloadTypevaluesArray = Array of TCollectdValue;
  TListGroupsResponseTypegroupArray = Array of TGroup;
  TListGroupMembersResponseTypemembersArray = Array of TMonitoredResource;
  TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray = Array of TMonitoredResourceDescriptor;
  TMonitoredResourceDescriptorTypelabelsArray = Array of TLabelDescriptor;
  TListMetricDescriptorsResponseTypemetricDescriptorsArray = Array of TMetricDescriptor;
  TMetricDescriptorTypelabelsArray = Array of TLabelDescriptor;
  TListTimeSeriesResponseTypetimeSeriesArray = Array of TTimeSeries;
  TTimeSeriesTypepointsArray = Array of TPoint;
  TCreateTimeSeriesRequestTypetimeSeriesArray = Array of TTimeSeries;
  TTypeTypefieldsArray = Array of TField;
  TTypeTypeoptionsArray = Array of TOption;
  TFieldTypeoptionsArray = Array of TOption;
  
  { --------------------------------------------------------------------
    TCreateCollectdTimeSeriesRequest
    --------------------------------------------------------------------}
  
  TCreateCollectdTimeSeriesRequest = Class(TGoogleBaseObject)
  Private
    Fresource : TMonitoredResource;
    FcollectdVersion : String;
    FcollectdPayloads : TCreateCollectdTimeSeriesRequestTypecollectdPayloadsArray;
  Protected
    //Property setters
    Procedure Setresource(AIndex : Integer; const AValue : TMonitoredResource); virtual;
    Procedure SetcollectdVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcollectdPayloads(AIndex : Integer; const AValue : TCreateCollectdTimeSeriesRequestTypecollectdPayloadsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property resource : TMonitoredResource Index 0 Read Fresource Write Setresource;
    Property collectdVersion : String Index 8 Read FcollectdVersion Write SetcollectdVersion;
    Property collectdPayloads : TCreateCollectdTimeSeriesRequestTypecollectdPayloadsArray Index 16 Read FcollectdPayloads Write SetcollectdPayloads;
  end;
  TCreateCollectdTimeSeriesRequestClass = Class of TCreateCollectdTimeSeriesRequest;
  
  { --------------------------------------------------------------------
    TMonitoredResourceTypelabels
    --------------------------------------------------------------------}
  
  TMonitoredResourceTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMonitoredResourceTypelabelsClass = Class of TMonitoredResourceTypelabels;
  
  { --------------------------------------------------------------------
    TMonitoredResource
    --------------------------------------------------------------------}
  
  TMonitoredResource = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Flabels : TMonitoredResourceTypelabels;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TMonitoredResourceTypelabels); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property labels : TMonitoredResourceTypelabels Index 8 Read Flabels Write Setlabels;
  end;
  TMonitoredResourceClass = Class of TMonitoredResource;
  
  { --------------------------------------------------------------------
    TCollectdPayloadTypemetadata
    --------------------------------------------------------------------}
  
  TCollectdPayloadTypemetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TCollectdPayloadTypemetadataClass = Class of TCollectdPayloadTypemetadata;
  
  { --------------------------------------------------------------------
    TCollectdPayload
    --------------------------------------------------------------------}
  
  TCollectdPayload = Class(TGoogleBaseObject)
  Private
    Fvalues : TCollectdPayloadTypevaluesArray;
    FstartTime : String;
    FendTime : String;
    Fplugin : String;
    FpluginInstance : String;
    F_type : String;
    FtypeInstance : String;
    Fmetadata : TCollectdPayloadTypemetadata;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setvalues(AIndex : Integer; const AValue : TCollectdPayloadTypevaluesArray); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setplugin(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpluginInstance(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SettypeInstance(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TCollectdPayloadTypemetadata); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property values : TCollectdPayloadTypevaluesArray Index 0 Read Fvalues Write Setvalues;
    Property startTime : String Index 8 Read FstartTime Write SetstartTime;
    Property endTime : String Index 16 Read FendTime Write SetendTime;
    Property plugin : String Index 24 Read Fplugin Write Setplugin;
    Property pluginInstance : String Index 32 Read FpluginInstance Write SetpluginInstance;
    Property _type : String Index 40 Read F_type Write Set_type;
    Property typeInstance : String Index 48 Read FtypeInstance Write SettypeInstance;
    Property metadata : TCollectdPayloadTypemetadata Index 56 Read Fmetadata Write Setmetadata;
  end;
  TCollectdPayloadClass = Class of TCollectdPayload;
  
  { --------------------------------------------------------------------
    TCollectdValue
    --------------------------------------------------------------------}
  
  TCollectdValue = Class(TGoogleBaseObject)
  Private
    FdataSourceName : String;
    FdataSourceType : String;
    Fvalue : TTypedValue;
  Protected
    //Property setters
    Procedure SetdataSourceName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataSourceType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : TTypedValue); virtual;
  Public
  Published
    Property dataSourceName : String Index 0 Read FdataSourceName Write SetdataSourceName;
    Property dataSourceType : String Index 8 Read FdataSourceType Write SetdataSourceType;
    Property value : TTypedValue Index 16 Read Fvalue Write Setvalue;
  end;
  TCollectdValueClass = Class of TCollectdValue;
  
  { --------------------------------------------------------------------
    TTypedValue
    --------------------------------------------------------------------}
  
  TTypedValue = Class(TGoogleBaseObject)
  Private
    FboolValue : boolean;
    Fint64Value : String;
    FdoubleValue : double;
    FstringValue : String;
    FdistributionValue : TDistribution;
  Protected
    //Property setters
    Procedure SetboolValue(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setint64Value(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdoubleValue(AIndex : Integer; const AValue : double); virtual;
    Procedure SetstringValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdistributionValue(AIndex : Integer; const AValue : TDistribution); virtual;
  Public
  Published
    Property boolValue : boolean Index 0 Read FboolValue Write SetboolValue;
    Property int64Value : String Index 8 Read Fint64Value Write Setint64Value;
    Property doubleValue : double Index 16 Read FdoubleValue Write SetdoubleValue;
    Property stringValue : String Index 24 Read FstringValue Write SetstringValue;
    Property distributionValue : TDistribution Index 32 Read FdistributionValue Write SetdistributionValue;
  end;
  TTypedValueClass = Class of TTypedValue;
  
  { --------------------------------------------------------------------
    TDistribution
    --------------------------------------------------------------------}
  
  TDistribution = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    Fmean : double;
    FsumOfSquaredDeviation : double;
    Frange : TRange;
    FbucketOptions : TBucketOptions;
    FbucketCounts : TStringArray;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmean(AIndex : Integer; const AValue : double); virtual;
    Procedure SetsumOfSquaredDeviation(AIndex : Integer; const AValue : double); virtual;
    Procedure Setrange(AIndex : Integer; const AValue : TRange); virtual;
    Procedure SetbucketOptions(AIndex : Integer; const AValue : TBucketOptions); virtual;
    Procedure SetbucketCounts(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property mean : double Index 8 Read Fmean Write Setmean;
    Property sumOfSquaredDeviation : double Index 16 Read FsumOfSquaredDeviation Write SetsumOfSquaredDeviation;
    Property range : TRange Index 24 Read Frange Write Setrange;
    Property bucketOptions : TBucketOptions Index 32 Read FbucketOptions Write SetbucketOptions;
    Property bucketCounts : TStringArray Index 40 Read FbucketCounts Write SetbucketCounts;
  end;
  TDistributionClass = Class of TDistribution;
  
  { --------------------------------------------------------------------
    TRange
    --------------------------------------------------------------------}
  
  TRange = Class(TGoogleBaseObject)
  Private
    Fmin : double;
    Fmax : double;
  Protected
    //Property setters
    Procedure Setmin(AIndex : Integer; const AValue : double); virtual;
    Procedure Setmax(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property min : double Index 0 Read Fmin Write Setmin;
    Property max : double Index 8 Read Fmax Write Setmax;
  end;
  TRangeClass = Class of TRange;
  
  { --------------------------------------------------------------------
    TBucketOptions
    --------------------------------------------------------------------}
  
  TBucketOptions = Class(TGoogleBaseObject)
  Private
    FlinearBuckets : TLinear;
    FexponentialBuckets : TExponential;
    FexplicitBuckets : TExplicit;
  Protected
    //Property setters
    Procedure SetlinearBuckets(AIndex : Integer; const AValue : TLinear); virtual;
    Procedure SetexponentialBuckets(AIndex : Integer; const AValue : TExponential); virtual;
    Procedure SetexplicitBuckets(AIndex : Integer; const AValue : TExplicit); virtual;
  Public
  Published
    Property linearBuckets : TLinear Index 0 Read FlinearBuckets Write SetlinearBuckets;
    Property exponentialBuckets : TExponential Index 8 Read FexponentialBuckets Write SetexponentialBuckets;
    Property explicitBuckets : TExplicit Index 16 Read FexplicitBuckets Write SetexplicitBuckets;
  end;
  TBucketOptionsClass = Class of TBucketOptions;
  
  { --------------------------------------------------------------------
    TLinear
    --------------------------------------------------------------------}
  
  TLinear = Class(TGoogleBaseObject)
  Private
    FnumFiniteBuckets : integer;
    Fwidth : double;
    Foffset : double;
  Protected
    //Property setters
    Procedure SetnumFiniteBuckets(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : double); virtual;
    Procedure Setoffset(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property numFiniteBuckets : integer Index 0 Read FnumFiniteBuckets Write SetnumFiniteBuckets;
    Property width : double Index 8 Read Fwidth Write Setwidth;
    Property offset : double Index 16 Read Foffset Write Setoffset;
  end;
  TLinearClass = Class of TLinear;
  
  { --------------------------------------------------------------------
    TExponential
    --------------------------------------------------------------------}
  
  TExponential = Class(TGoogleBaseObject)
  Private
    FnumFiniteBuckets : integer;
    FgrowthFactor : double;
    Fscale : double;
  Protected
    //Property setters
    Procedure SetnumFiniteBuckets(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetgrowthFactor(AIndex : Integer; const AValue : double); virtual;
    Procedure Setscale(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property numFiniteBuckets : integer Index 0 Read FnumFiniteBuckets Write SetnumFiniteBuckets;
    Property growthFactor : double Index 8 Read FgrowthFactor Write SetgrowthFactor;
    Property scale : double Index 16 Read Fscale Write Setscale;
  end;
  TExponentialClass = Class of TExponential;
  
  { --------------------------------------------------------------------
    TExplicit
    --------------------------------------------------------------------}
  
  TExplicit = Class(TGoogleBaseObject)
  Private
    Fbounds : TdoubleArray;
  Protected
    //Property setters
    Procedure Setbounds(AIndex : Integer; const AValue : TdoubleArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bounds : TdoubleArray Index 0 Read Fbounds Write Setbounds;
  end;
  TExplicitClass = Class of TExplicit;
  
  { --------------------------------------------------------------------
    TEmpty
    --------------------------------------------------------------------}
  
  TEmpty = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEmptyClass = Class of TEmpty;
  
  { --------------------------------------------------------------------
    TListGroupsResponse
    --------------------------------------------------------------------}
  
  TListGroupsResponse = Class(TGoogleBaseObject)
  Private
    Fgroup : TListGroupsResponseTypegroupArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setgroup(AIndex : Integer; const AValue : TListGroupsResponseTypegroupArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property group : TListGroupsResponseTypegroupArray Index 0 Read Fgroup Write Setgroup;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListGroupsResponseClass = Class of TListGroupsResponse;
  
  { --------------------------------------------------------------------
    TGroup
    --------------------------------------------------------------------}
  
  TGroup = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FdisplayName : String;
    FparentName : String;
    Ffilter : String;
    FisCluster : boolean;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetparentName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfilter(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisCluster(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property parentName : String Index 16 Read FparentName Write SetparentName;
    Property filter : String Index 24 Read Ffilter Write Setfilter;
    Property isCluster : boolean Index 32 Read FisCluster Write SetisCluster;
  end;
  TGroupClass = Class of TGroup;
  
  { --------------------------------------------------------------------
    TListGroupMembersResponse
    --------------------------------------------------------------------}
  
  TListGroupMembersResponse = Class(TGoogleBaseObject)
  Private
    Fmembers : TListGroupMembersResponseTypemembersArray;
    FnextPageToken : String;
    FtotalSize : integer;
  Protected
    //Property setters
    Procedure Setmembers(AIndex : Integer; const AValue : TListGroupMembersResponseTypemembersArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property members : TListGroupMembersResponseTypemembersArray Index 0 Read Fmembers Write Setmembers;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property totalSize : integer Index 16 Read FtotalSize Write SettotalSize;
  end;
  TListGroupMembersResponseClass = Class of TListGroupMembersResponse;
  
  { --------------------------------------------------------------------
    TListMonitoredResourceDescriptorsResponse
    --------------------------------------------------------------------}
  
  TListMonitoredResourceDescriptorsResponse = Class(TGoogleBaseObject)
  Private
    FresourceDescriptors : TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetresourceDescriptors(AIndex : Integer; const AValue : TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property resourceDescriptors : TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray Index 0 Read FresourceDescriptors Write SetresourceDescriptors;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListMonitoredResourceDescriptorsResponseClass = Class of TListMonitoredResourceDescriptorsResponse;
  
  { --------------------------------------------------------------------
    TMonitoredResourceDescriptor
    --------------------------------------------------------------------}
  
  TMonitoredResourceDescriptor = Class(TGoogleBaseObject)
  Private
    Fname : String;
    F_type : String;
    FdisplayName : String;
    Fdescription : String;
    Flabels : TMonitoredResourceDescriptorTypelabelsArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TMonitoredResourceDescriptorTypelabelsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property displayName : String Index 16 Read FdisplayName Write SetdisplayName;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property labels : TMonitoredResourceDescriptorTypelabelsArray Index 32 Read Flabels Write Setlabels;
  end;
  TMonitoredResourceDescriptorClass = Class of TMonitoredResourceDescriptor;
  
  { --------------------------------------------------------------------
    TLabelDescriptor
    --------------------------------------------------------------------}
  
  TLabelDescriptor = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    FvalueType : String;
    Fdescription : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvalueType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property valueType : String Index 8 Read FvalueType Write SetvalueType;
    Property description : String Index 16 Read Fdescription Write Setdescription;
  end;
  TLabelDescriptorClass = Class of TLabelDescriptor;
  
  { --------------------------------------------------------------------
    TListMetricDescriptorsResponse
    --------------------------------------------------------------------}
  
  TListMetricDescriptorsResponse = Class(TGoogleBaseObject)
  Private
    FmetricDescriptors : TListMetricDescriptorsResponseTypemetricDescriptorsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetmetricDescriptors(AIndex : Integer; const AValue : TListMetricDescriptorsResponseTypemetricDescriptorsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property metricDescriptors : TListMetricDescriptorsResponseTypemetricDescriptorsArray Index 0 Read FmetricDescriptors Write SetmetricDescriptors;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListMetricDescriptorsResponseClass = Class of TListMetricDescriptorsResponse;
  
  { --------------------------------------------------------------------
    TMetricDescriptor
    --------------------------------------------------------------------}
  
  TMetricDescriptor = Class(TGoogleBaseObject)
  Private
    Fname : String;
    F_type : String;
    Flabels : TMetricDescriptorTypelabelsArray;
    FmetricKind : String;
    FvalueType : String;
    F_unit : String;
    Fdescription : String;
    FdisplayName : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TMetricDescriptorTypelabelsArray); virtual;
    Procedure SetmetricKind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvalueType(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_unit(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property labels : TMetricDescriptorTypelabelsArray Index 16 Read Flabels Write Setlabels;
    Property metricKind : String Index 24 Read FmetricKind Write SetmetricKind;
    Property valueType : String Index 32 Read FvalueType Write SetvalueType;
    Property _unit : String Index 40 Read F_unit Write Set_unit;
    Property description : String Index 48 Read Fdescription Write Setdescription;
    Property displayName : String Index 56 Read FdisplayName Write SetdisplayName;
  end;
  TMetricDescriptorClass = Class of TMetricDescriptor;
  
  { --------------------------------------------------------------------
    TListTimeSeriesResponse
    --------------------------------------------------------------------}
  
  TListTimeSeriesResponse = Class(TGoogleBaseObject)
  Private
    FtimeSeries : TListTimeSeriesResponseTypetimeSeriesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SettimeSeries(AIndex : Integer; const AValue : TListTimeSeriesResponseTypetimeSeriesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property timeSeries : TListTimeSeriesResponseTypetimeSeriesArray Index 0 Read FtimeSeries Write SettimeSeries;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListTimeSeriesResponseClass = Class of TListTimeSeriesResponse;
  
  { --------------------------------------------------------------------
    TTimeSeries
    --------------------------------------------------------------------}
  
  TTimeSeries = Class(TGoogleBaseObject)
  Private
    Fmetric : TMetric;
    Fresource : TMonitoredResource;
    FmetricKind : String;
    FvalueType : String;
    Fpoints : TTimeSeriesTypepointsArray;
  Protected
    //Property setters
    Procedure Setmetric(AIndex : Integer; const AValue : TMetric); virtual;
    Procedure Setresource(AIndex : Integer; const AValue : TMonitoredResource); virtual;
    Procedure SetmetricKind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvalueType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpoints(AIndex : Integer; const AValue : TTimeSeriesTypepointsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property metric : TMetric Index 0 Read Fmetric Write Setmetric;
    Property resource : TMonitoredResource Index 8 Read Fresource Write Setresource;
    Property metricKind : String Index 16 Read FmetricKind Write SetmetricKind;
    Property valueType : String Index 24 Read FvalueType Write SetvalueType;
    Property points : TTimeSeriesTypepointsArray Index 32 Read Fpoints Write Setpoints;
  end;
  TTimeSeriesClass = Class of TTimeSeries;
  
  { --------------------------------------------------------------------
    TMetricTypelabels
    --------------------------------------------------------------------}
  
  TMetricTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMetricTypelabelsClass = Class of TMetricTypelabels;
  
  { --------------------------------------------------------------------
    TMetric
    --------------------------------------------------------------------}
  
  TMetric = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Flabels : TMetricTypelabels;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TMetricTypelabels); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property labels : TMetricTypelabels Index 8 Read Flabels Write Setlabels;
  end;
  TMetricClass = Class of TMetric;
  
  { --------------------------------------------------------------------
    TPoint
    --------------------------------------------------------------------}
  
  TPoint = Class(TGoogleBaseObject)
  Private
    Finterval : TTimeInterval;
    Fvalue : TTypedValue;
  Protected
    //Property setters
    Procedure Setinterval(AIndex : Integer; const AValue : TTimeInterval); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : TTypedValue); virtual;
  Public
  Published
    Property interval : TTimeInterval Index 0 Read Finterval Write Setinterval;
    Property value : TTypedValue Index 8 Read Fvalue Write Setvalue;
  end;
  TPointClass = Class of TPoint;
  
  { --------------------------------------------------------------------
    TTimeInterval
    --------------------------------------------------------------------}
  
  TTimeInterval = Class(TGoogleBaseObject)
  Private
    FendTime : String;
    FstartTime : String;
  Protected
    //Property setters
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property endTime : String Index 0 Read FendTime Write SetendTime;
    Property startTime : String Index 8 Read FstartTime Write SetstartTime;
  end;
  TTimeIntervalClass = Class of TTimeInterval;
  
  { --------------------------------------------------------------------
    TCreateTimeSeriesRequest
    --------------------------------------------------------------------}
  
  TCreateTimeSeriesRequest = Class(TGoogleBaseObject)
  Private
    FtimeSeries : TCreateTimeSeriesRequestTypetimeSeriesArray;
  Protected
    //Property setters
    Procedure SettimeSeries(AIndex : Integer; const AValue : TCreateTimeSeriesRequestTypetimeSeriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property timeSeries : TCreateTimeSeriesRequestTypetimeSeriesArray Index 0 Read FtimeSeries Write SettimeSeries;
  end;
  TCreateTimeSeriesRequestClass = Class of TCreateTimeSeriesRequest;
  
  { --------------------------------------------------------------------
    TType
    --------------------------------------------------------------------}
  
  TType = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Ffields : TTypeTypefieldsArray;
    Foneofs : TStringArray;
    Foptions : TTypeTypeoptionsArray;
    FsourceContext : TSourceContext;
    Fsyntax : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfields(AIndex : Integer; const AValue : TTypeTypefieldsArray); virtual;
    Procedure Setoneofs(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setoptions(AIndex : Integer; const AValue : TTypeTypeoptionsArray); virtual;
    Procedure SetsourceContext(AIndex : Integer; const AValue : TSourceContext); virtual;
    Procedure Setsyntax(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property fields : TTypeTypefieldsArray Index 8 Read Ffields Write Setfields;
    Property oneofs : TStringArray Index 16 Read Foneofs Write Setoneofs;
    Property options : TTypeTypeoptionsArray Index 24 Read Foptions Write Setoptions;
    Property sourceContext : TSourceContext Index 32 Read FsourceContext Write SetsourceContext;
    Property syntax : String Index 40 Read Fsyntax Write Setsyntax;
  end;
  TTypeClass = Class of TType;
  
  { --------------------------------------------------------------------
    TField
    --------------------------------------------------------------------}
  
  TField = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fcardinality : String;
    Fnumber : integer;
    Fname : String;
    FtypeUrl : String;
    FoneofIndex : integer;
    F_packed : boolean;
    Foptions : TFieldTypeoptionsArray;
    FjsonName : String;
    FdefaultValue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcardinality(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnumber(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SettypeUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoneofIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure Set_packed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setoptions(AIndex : Integer; const AValue : TFieldTypeoptionsArray); virtual;
    Procedure SetjsonName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdefaultValue(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property cardinality : String Index 8 Read Fcardinality Write Setcardinality;
    Property number : integer Index 16 Read Fnumber Write Setnumber;
    Property name : String Index 24 Read Fname Write Setname;
    Property typeUrl : String Index 32 Read FtypeUrl Write SettypeUrl;
    Property oneofIndex : integer Index 40 Read FoneofIndex Write SetoneofIndex;
    Property _packed : boolean Index 48 Read F_packed Write Set_packed;
    Property options : TFieldTypeoptionsArray Index 56 Read Foptions Write Setoptions;
    Property jsonName : String Index 64 Read FjsonName Write SetjsonName;
    Property defaultValue : String Index 72 Read FdefaultValue Write SetdefaultValue;
  end;
  TFieldClass = Class of TField;
  
  { --------------------------------------------------------------------
    TOptionTypevalue
    --------------------------------------------------------------------}
  
  TOptionTypevalue = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOptionTypevalueClass = Class of TOptionTypevalue;
  
  { --------------------------------------------------------------------
    TOption
    --------------------------------------------------------------------}
  
  TOption = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fvalue : TOptionTypevalue;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : TOptionTypevalue); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property value : TOptionTypevalue Index 8 Read Fvalue Write Setvalue;
  end;
  TOptionClass = Class of TOption;
  
  { --------------------------------------------------------------------
    TSourceContext
    --------------------------------------------------------------------}
  
  TSourceContext = Class(TGoogleBaseObject)
  Private
    FfileName : String;
  Protected
    //Property setters
    Procedure SetfileName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property fileName : String Index 0 Read FfileName Write SetfileName;
  end;
  TSourceContextClass = Class of TSourceContext;
  
  { --------------------------------------------------------------------
    TProjectsCollectdTimeSeriesResource
    --------------------------------------------------------------------}
  
  TProjectsCollectdTimeSeriesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(_name: string; aCreateCollectdTimeSeriesRequest : TCreateCollectdTimeSeriesRequest) : TEmpty;overload;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsGroupsMembersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsGroupsMembersResource, method List
  
  TProjectsGroupsMembersListOptions = Record
    pageSize : integer;
    pageToken : String;
    filter : String;
    intervalendTime : String;
    intervalstartTime : String;
  end;
  
  TProjectsGroupsMembersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(_name: string; AQuery : string  = '') : TListGroupMembersResponse;
    Function List(_name: string; AQuery : TProjectsGroupsMemberslistOptions) : TListGroupMembersResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsGroupsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsGroupsResource, method List
  
  TProjectsGroupsListOptions = Record
    childrenOfGroup : String;
    ancestorsOfGroup : String;
    descendantsOfGroup : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TProjectsGroupsResource, method Create
  
  TProjectsGroupsCreateOptions = Record
    validateOnly : boolean;
  end;
  
  
  //Optional query Options for TProjectsGroupsResource, method Update
  
  TProjectsGroupsUpdateOptions = Record
    validateOnly : boolean;
  end;
  
  TProjectsGroupsResource = Class(TGoogleResource)
  Private
    FMembersInstance : TProjectsGroupsMembersResource;
    Function GetMembersInstance : TProjectsGroupsMembersResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(_name: string; AQuery : string  = '') : TListGroupsResponse;
    Function List(_name: string; AQuery : TProjectsGroupslistOptions) : TListGroupsResponse;
    Function Get(_name: string) : TGroup;
    Function Create(_name: string; aGroup : TGroup; AQuery : string  = '') : TGroup;overload;
    Function Create(_name: string; aGroup : TGroup; AQuery : TProjectsGroupscreateOptions) : TGroup;overload;
    Function Update(_name: string; aGroup : TGroup; AQuery : string  = '') : TGroup;
    Function Update(_name: string; aGroup : TGroup; AQuery : TProjectsGroupsupdateOptions) : TGroup;
    Function Delete(_name: string) : TEmpty;
    Function CreateMembersResource(AOwner : TComponent) : TProjectsGroupsMembersResource;virtual;overload;
    Function CreateMembersResource : TProjectsGroupsMembersResource;virtual;overload;
    Property MembersResource : TProjectsGroupsMembersResource Read GetMembersInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsMonitoredResourceDescriptorsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsMonitoredResourceDescriptorsResource, method List
  
  TProjectsMonitoredResourceDescriptorsListOptions = Record
    filter : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsMonitoredResourceDescriptorsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(_name: string; AQuery : string  = '') : TListMonitoredResourceDescriptorsResponse;
    Function List(_name: string; AQuery : TProjectsMonitoredResourceDescriptorslistOptions) : TListMonitoredResourceDescriptorsResponse;
    Function Get(_name: string) : TMonitoredResourceDescriptor;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsMetricDescriptorsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsMetricDescriptorsResource, method List
  
  TProjectsMetricDescriptorsListOptions = Record
    filter : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsMetricDescriptorsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(_name: string; AQuery : string  = '') : TListMetricDescriptorsResponse;
    Function List(_name: string; AQuery : TProjectsMetricDescriptorslistOptions) : TListMetricDescriptorsResponse;
    Function Get(_name: string) : TMetricDescriptor;
    Function Create(_name: string; aMetricDescriptor : TMetricDescriptor) : TMetricDescriptor;overload;
    Function Delete(_name: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsTimeSeriesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsTimeSeriesResource, method List
  
  TProjectsTimeSeriesListOptions = Record
    filter : String;
    intervalendTime : String;
    intervalstartTime : String;
    aggregationalignmentPeriod : String;
    aggregationperSeriesAligner : String;
    aggregationcrossSeriesReducer : String;
    aggregationgroupByFields : String;
    orderBy : String;
    view : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsTimeSeriesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(_name: string; AQuery : string  = '') : TListTimeSeriesResponse;
    Function List(_name: string; AQuery : TProjectsTimeSerieslistOptions) : TListTimeSeriesResponse;
    Function Create(_name: string; aCreateTimeSeriesRequest : TCreateTimeSeriesRequest) : TEmpty;overload;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FCollectdTimeSeriesInstance : TProjectsCollectdTimeSeriesResource;
    FGroupsMembersInstance : TProjectsGroupsMembersResource;
    FGroupsInstance : TProjectsGroupsResource;
    FMonitoredResourceDescriptorsInstance : TProjectsMonitoredResourceDescriptorsResource;
    FMetricDescriptorsInstance : TProjectsMetricDescriptorsResource;
    FTimeSeriesInstance : TProjectsTimeSeriesResource;
    Function GetCollectdTimeSeriesInstance : TProjectsCollectdTimeSeriesResource;virtual;
    Function GetGroupsMembersInstance : TProjectsGroupsMembersResource;virtual;
    Function GetGroupsInstance : TProjectsGroupsResource;virtual;
    Function GetMonitoredResourceDescriptorsInstance : TProjectsMonitoredResourceDescriptorsResource;virtual;
    Function GetMetricDescriptorsInstance : TProjectsMetricDescriptorsResource;virtual;
    Function GetTimeSeriesInstance : TProjectsTimeSeriesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateCollectdTimeSeriesResource(AOwner : TComponent) : TProjectsCollectdTimeSeriesResource;virtual;overload;
    Function CreateCollectdTimeSeriesResource : TProjectsCollectdTimeSeriesResource;virtual;overload;
    Function CreateGroupsMembersResource(AOwner : TComponent) : TProjectsGroupsMembersResource;virtual;overload;
    Function CreateGroupsMembersResource : TProjectsGroupsMembersResource;virtual;overload;
    Function CreateGroupsResource(AOwner : TComponent) : TProjectsGroupsResource;virtual;overload;
    Function CreateGroupsResource : TProjectsGroupsResource;virtual;overload;
    Function CreateMonitoredResourceDescriptorsResource(AOwner : TComponent) : TProjectsMonitoredResourceDescriptorsResource;virtual;overload;
    Function CreateMonitoredResourceDescriptorsResource : TProjectsMonitoredResourceDescriptorsResource;virtual;overload;
    Function CreateMetricDescriptorsResource(AOwner : TComponent) : TProjectsMetricDescriptorsResource;virtual;overload;
    Function CreateMetricDescriptorsResource : TProjectsMetricDescriptorsResource;virtual;overload;
    Function CreateTimeSeriesResource(AOwner : TComponent) : TProjectsTimeSeriesResource;virtual;overload;
    Function CreateTimeSeriesResource : TProjectsTimeSeriesResource;virtual;overload;
    Property CollectdTimeSeriesResource : TProjectsCollectdTimeSeriesResource Read GetCollectdTimeSeriesInstance;
    Property GroupsMembersResource : TProjectsGroupsMembersResource Read GetGroupsMembersInstance;
    Property GroupsResource : TProjectsGroupsResource Read GetGroupsInstance;
    Property MonitoredResourceDescriptorsResource : TProjectsMonitoredResourceDescriptorsResource Read GetMonitoredResourceDescriptorsInstance;
    Property MetricDescriptorsResource : TProjectsMetricDescriptorsResource Read GetMetricDescriptorsInstance;
    Property TimeSeriesResource : TProjectsTimeSeriesResource Read GetTimeSeriesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TMonitoringAPI
    --------------------------------------------------------------------}
  
  TMonitoringAPI = Class(TGoogleAPI)
  Private
    FProjectsCollectdTimeSeriesInstance : TProjectsCollectdTimeSeriesResource;
    FProjectsGroupsMembersInstance : TProjectsGroupsMembersResource;
    FProjectsGroupsInstance : TProjectsGroupsResource;
    FProjectsMonitoredResourceDescriptorsInstance : TProjectsMonitoredResourceDescriptorsResource;
    FProjectsMetricDescriptorsInstance : TProjectsMetricDescriptorsResource;
    FProjectsTimeSeriesInstance : TProjectsTimeSeriesResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsCollectdTimeSeriesInstance : TProjectsCollectdTimeSeriesResource;virtual;
    Function GetProjectsGroupsMembersInstance : TProjectsGroupsMembersResource;virtual;
    Function GetProjectsGroupsInstance : TProjectsGroupsResource;virtual;
    Function GetProjectsMonitoredResourceDescriptorsInstance : TProjectsMonitoredResourceDescriptorsResource;virtual;
    Function GetProjectsMetricDescriptorsInstance : TProjectsMetricDescriptorsResource;virtual;
    Function GetProjectsTimeSeriesInstance : TProjectsTimeSeriesResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
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
    Function CreateProjectsCollectdTimeSeriesResource(AOwner : TComponent) : TProjectsCollectdTimeSeriesResource;virtual;overload;
    Function CreateProjectsCollectdTimeSeriesResource : TProjectsCollectdTimeSeriesResource;virtual;overload;
    Function CreateProjectsGroupsMembersResource(AOwner : TComponent) : TProjectsGroupsMembersResource;virtual;overload;
    Function CreateProjectsGroupsMembersResource : TProjectsGroupsMembersResource;virtual;overload;
    Function CreateProjectsGroupsResource(AOwner : TComponent) : TProjectsGroupsResource;virtual;overload;
    Function CreateProjectsGroupsResource : TProjectsGroupsResource;virtual;overload;
    Function CreateProjectsMonitoredResourceDescriptorsResource(AOwner : TComponent) : TProjectsMonitoredResourceDescriptorsResource;virtual;overload;
    Function CreateProjectsMonitoredResourceDescriptorsResource : TProjectsMonitoredResourceDescriptorsResource;virtual;overload;
    Function CreateProjectsMetricDescriptorsResource(AOwner : TComponent) : TProjectsMetricDescriptorsResource;virtual;overload;
    Function CreateProjectsMetricDescriptorsResource : TProjectsMetricDescriptorsResource;virtual;overload;
    Function CreateProjectsTimeSeriesResource(AOwner : TComponent) : TProjectsTimeSeriesResource;virtual;overload;
    Function CreateProjectsTimeSeriesResource : TProjectsTimeSeriesResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsCollectdTimeSeriesResource : TProjectsCollectdTimeSeriesResource Read GetProjectsCollectdTimeSeriesInstance;
    Property ProjectsGroupsMembersResource : TProjectsGroupsMembersResource Read GetProjectsGroupsMembersInstance;
    Property ProjectsGroupsResource : TProjectsGroupsResource Read GetProjectsGroupsInstance;
    Property ProjectsMonitoredResourceDescriptorsResource : TProjectsMonitoredResourceDescriptorsResource Read GetProjectsMonitoredResourceDescriptorsInstance;
    Property ProjectsMetricDescriptorsResource : TProjectsMetricDescriptorsResource Read GetProjectsMetricDescriptorsInstance;
    Property ProjectsTimeSeriesResource : TProjectsTimeSeriesResource Read GetProjectsTimeSeriesInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TCreateCollectdTimeSeriesRequest
  --------------------------------------------------------------------}


Procedure TCreateCollectdTimeSeriesRequest.Setresource(AIndex : Integer; const AValue : TMonitoredResource); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateCollectdTimeSeriesRequest.SetcollectdVersion(AIndex : Integer; const AValue : String); 

begin
  If (FcollectdVersion=AValue) then exit;
  FcollectdVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateCollectdTimeSeriesRequest.SetcollectdPayloads(AIndex : Integer; const AValue : TCreateCollectdTimeSeriesRequestTypecollectdPayloadsArray); 

begin
  If (FcollectdPayloads=AValue) then exit;
  FcollectdPayloads:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreateCollectdTimeSeriesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'collectdpayloads' : SetLength(FcollectdPayloads,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMonitoredResourceTypelabels
  --------------------------------------------------------------------}


Class Function TMonitoredResourceTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMonitoredResource
  --------------------------------------------------------------------}


Procedure TMonitoredResource.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResource.Setlabels(AIndex : Integer; const AValue : TMonitoredResourceTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMonitoredResource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCollectdPayloadTypemetadata
  --------------------------------------------------------------------}


Class Function TCollectdPayloadTypemetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TCollectdPayload
  --------------------------------------------------------------------}


Procedure TCollectdPayload.Setvalues(AIndex : Integer; const AValue : TCollectdPayloadTypevaluesArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectdPayload.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectdPayload.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectdPayload.Setplugin(AIndex : Integer; const AValue : String); 

begin
  If (Fplugin=AValue) then exit;
  Fplugin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectdPayload.SetpluginInstance(AIndex : Integer; const AValue : String); 

begin
  If (FpluginInstance=AValue) then exit;
  FpluginInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectdPayload.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectdPayload.SettypeInstance(AIndex : Integer; const AValue : String); 

begin
  If (FtypeInstance=AValue) then exit;
  FtypeInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectdPayload.Setmetadata(AIndex : Integer; const AValue : TCollectdPayloadTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCollectdPayload.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCollectdPayload.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCollectdValue
  --------------------------------------------------------------------}


Procedure TCollectdValue.SetdataSourceName(AIndex : Integer; const AValue : String); 

begin
  If (FdataSourceName=AValue) then exit;
  FdataSourceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectdValue.SetdataSourceType(AIndex : Integer; const AValue : String); 

begin
  If (FdataSourceType=AValue) then exit;
  FdataSourceType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectdValue.Setvalue(AIndex : Integer; const AValue : TTypedValue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTypedValue
  --------------------------------------------------------------------}


Procedure TTypedValue.SetboolValue(AIndex : Integer; const AValue : boolean); 

begin
  If (FboolValue=AValue) then exit;
  FboolValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTypedValue.Setint64Value(AIndex : Integer; const AValue : String); 

begin
  If (Fint64Value=AValue) then exit;
  Fint64Value:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTypedValue.SetdoubleValue(AIndex : Integer; const AValue : double); 

begin
  If (FdoubleValue=AValue) then exit;
  FdoubleValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTypedValue.SetstringValue(AIndex : Integer; const AValue : String); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTypedValue.SetdistributionValue(AIndex : Integer; const AValue : TDistribution); 

begin
  If (FdistributionValue=AValue) then exit;
  FdistributionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDistribution
  --------------------------------------------------------------------}


Procedure TDistribution.Setcount(AIndex : Integer; const AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDistribution.Setmean(AIndex : Integer; const AValue : double); 

begin
  If (Fmean=AValue) then exit;
  Fmean:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDistribution.SetsumOfSquaredDeviation(AIndex : Integer; const AValue : double); 

begin
  If (FsumOfSquaredDeviation=AValue) then exit;
  FsumOfSquaredDeviation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDistribution.Setrange(AIndex : Integer; const AValue : TRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDistribution.SetbucketOptions(AIndex : Integer; const AValue : TBucketOptions); 

begin
  If (FbucketOptions=AValue) then exit;
  FbucketOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDistribution.SetbucketCounts(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FbucketCounts=AValue) then exit;
  FbucketCounts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDistribution.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bucketcounts' : SetLength(FbucketCounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRange
  --------------------------------------------------------------------}


Procedure TRange.Setmin(AIndex : Integer; const AValue : double); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRange.Setmax(AIndex : Integer; const AValue : double); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketOptions
  --------------------------------------------------------------------}


Procedure TBucketOptions.SetlinearBuckets(AIndex : Integer; const AValue : TLinear); 

begin
  If (FlinearBuckets=AValue) then exit;
  FlinearBuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketOptions.SetexponentialBuckets(AIndex : Integer; const AValue : TExponential); 

begin
  If (FexponentialBuckets=AValue) then exit;
  FexponentialBuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketOptions.SetexplicitBuckets(AIndex : Integer; const AValue : TExplicit); 

begin
  If (FexplicitBuckets=AValue) then exit;
  FexplicitBuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinear
  --------------------------------------------------------------------}


Procedure TLinear.SetnumFiniteBuckets(AIndex : Integer; const AValue : integer); 

begin
  If (FnumFiniteBuckets=AValue) then exit;
  FnumFiniteBuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinear.Setwidth(AIndex : Integer; const AValue : double); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinear.Setoffset(AIndex : Integer; const AValue : double); 

begin
  If (Foffset=AValue) then exit;
  Foffset:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExponential
  --------------------------------------------------------------------}


Procedure TExponential.SetnumFiniteBuckets(AIndex : Integer; const AValue : integer); 

begin
  If (FnumFiniteBuckets=AValue) then exit;
  FnumFiniteBuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExponential.SetgrowthFactor(AIndex : Integer; const AValue : double); 

begin
  If (FgrowthFactor=AValue) then exit;
  FgrowthFactor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExponential.Setscale(AIndex : Integer; const AValue : double); 

begin
  If (Fscale=AValue) then exit;
  Fscale:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExplicit
  --------------------------------------------------------------------}


Procedure TExplicit.Setbounds(AIndex : Integer; const AValue : TdoubleArray); 

begin
  If (Fbounds=AValue) then exit;
  Fbounds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExplicit.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bounds' : SetLength(Fbounds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListGroupsResponse
  --------------------------------------------------------------------}


Procedure TListGroupsResponse.Setgroup(AIndex : Integer; const AValue : TListGroupsResponseTypegroupArray); 

begin
  If (Fgroup=AValue) then exit;
  Fgroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListGroupsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListGroupsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'group' : SetLength(Fgroup,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGroup
  --------------------------------------------------------------------}


Procedure TGroup.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.SetparentName(AIndex : Integer; const AValue : String); 

begin
  If (FparentName=AValue) then exit;
  FparentName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setfilter(AIndex : Integer; const AValue : String); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.SetisCluster(AIndex : Integer; const AValue : boolean); 

begin
  If (FisCluster=AValue) then exit;
  FisCluster:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListGroupMembersResponse
  --------------------------------------------------------------------}


Procedure TListGroupMembersResponse.Setmembers(AIndex : Integer; const AValue : TListGroupMembersResponseTypemembersArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListGroupMembersResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListGroupMembersResponse.SettotalSize(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalSize=AValue) then exit;
  FtotalSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListGroupMembersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'members' : SetLength(Fmembers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListMonitoredResourceDescriptorsResponse
  --------------------------------------------------------------------}


Procedure TListMonitoredResourceDescriptorsResponse.SetresourceDescriptors(AIndex : Integer; const AValue : TListMonitoredResourceDescriptorsResponseTyperesourceDescriptorsArray); 

begin
  If (FresourceDescriptors=AValue) then exit;
  FresourceDescriptors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMonitoredResourceDescriptorsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListMonitoredResourceDescriptorsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resourcedescriptors' : SetLength(FresourceDescriptors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMonitoredResourceDescriptor
  --------------------------------------------------------------------}


Procedure TMonitoredResourceDescriptor.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResourceDescriptor.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResourceDescriptor.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResourceDescriptor.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMonitoredResourceDescriptor.Setlabels(AIndex : Integer; const AValue : TMonitoredResourceDescriptorTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMonitoredResourceDescriptor.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMonitoredResourceDescriptor.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labels' : SetLength(Flabels,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLabelDescriptor
  --------------------------------------------------------------------}


Procedure TLabelDescriptor.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelDescriptor.SetvalueType(AIndex : Integer; const AValue : String); 

begin
  If (FvalueType=AValue) then exit;
  FvalueType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabelDescriptor.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListMetricDescriptorsResponse
  --------------------------------------------------------------------}


Procedure TListMetricDescriptorsResponse.SetmetricDescriptors(AIndex : Integer; const AValue : TListMetricDescriptorsResponseTypemetricDescriptorsArray); 

begin
  If (FmetricDescriptors=AValue) then exit;
  FmetricDescriptors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMetricDescriptorsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListMetricDescriptorsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'metricdescriptors' : SetLength(FmetricDescriptors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetricDescriptor
  --------------------------------------------------------------------}


Procedure TMetricDescriptor.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Setlabels(AIndex : Integer; const AValue : TMetricDescriptorTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.SetmetricKind(AIndex : Integer; const AValue : String); 

begin
  If (FmetricKind=AValue) then exit;
  FmetricKind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.SetvalueType(AIndex : Integer; const AValue : String); 

begin
  If (FvalueType=AValue) then exit;
  FvalueType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Set_unit(AIndex : Integer; const AValue : String); 

begin
  If (F_unit=AValue) then exit;
  F_unit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMetricDescriptor.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  '_unit' : Result:='unit';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMetricDescriptor.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labels' : SetLength(Flabels,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListTimeSeriesResponse
  --------------------------------------------------------------------}


Procedure TListTimeSeriesResponse.SettimeSeries(AIndex : Integer; const AValue : TListTimeSeriesResponseTypetimeSeriesArray); 

begin
  If (FtimeSeries=AValue) then exit;
  FtimeSeries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeSeriesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListTimeSeriesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'timeseries' : SetLength(FtimeSeries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTimeSeries
  --------------------------------------------------------------------}


Procedure TTimeSeries.Setmetric(AIndex : Integer; const AValue : TMetric); 

begin
  If (Fmetric=AValue) then exit;
  Fmetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeSeries.Setresource(AIndex : Integer; const AValue : TMonitoredResource); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeSeries.SetmetricKind(AIndex : Integer; const AValue : String); 

begin
  If (FmetricKind=AValue) then exit;
  FmetricKind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeSeries.SetvalueType(AIndex : Integer; const AValue : String); 

begin
  If (FvalueType=AValue) then exit;
  FvalueType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeSeries.Setpoints(AIndex : Integer; const AValue : TTimeSeriesTypepointsArray); 

begin
  If (Fpoints=AValue) then exit;
  Fpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTimeSeries.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'points' : SetLength(Fpoints,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetricTypelabels
  --------------------------------------------------------------------}


Class Function TMetricTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMetric
  --------------------------------------------------------------------}


Procedure TMetric.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetric.Setlabels(AIndex : Integer; const AValue : TMetricTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMetric.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPoint
  --------------------------------------------------------------------}


Procedure TPoint.Setinterval(AIndex : Integer; const AValue : TTimeInterval); 

begin
  If (Finterval=AValue) then exit;
  Finterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPoint.Setvalue(AIndex : Integer; const AValue : TTypedValue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeInterval
  --------------------------------------------------------------------}


Procedure TTimeInterval.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeInterval.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateTimeSeriesRequest
  --------------------------------------------------------------------}


Procedure TCreateTimeSeriesRequest.SettimeSeries(AIndex : Integer; const AValue : TCreateTimeSeriesRequestTypetimeSeriesArray); 

begin
  If (FtimeSeries=AValue) then exit;
  FtimeSeries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCreateTimeSeriesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'timeseries' : SetLength(FtimeSeries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TType
  --------------------------------------------------------------------}


Procedure TType.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TType.Setfields(AIndex : Integer; const AValue : TTypeTypefieldsArray); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TType.Setoneofs(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Foneofs=AValue) then exit;
  Foneofs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TType.Setoptions(AIndex : Integer; const AValue : TTypeTypeoptionsArray); 

begin
  If (Foptions=AValue) then exit;
  Foptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TType.SetsourceContext(AIndex : Integer; const AValue : TSourceContext); 

begin
  If (FsourceContext=AValue) then exit;
  FsourceContext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TType.Setsyntax(AIndex : Integer; const AValue : String); 

begin
  If (Fsyntax=AValue) then exit;
  Fsyntax:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TType.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'fields' : SetLength(Ffields,ALength);
  'oneofs' : SetLength(Foneofs,ALength);
  'options' : SetLength(Foptions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TField
  --------------------------------------------------------------------}


Procedure TField.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TField.Setcardinality(AIndex : Integer; const AValue : String); 

begin
  If (Fcardinality=AValue) then exit;
  Fcardinality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TField.Setnumber(AIndex : Integer; const AValue : integer); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TField.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TField.SettypeUrl(AIndex : Integer; const AValue : String); 

begin
  If (FtypeUrl=AValue) then exit;
  FtypeUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TField.SetoneofIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FoneofIndex=AValue) then exit;
  FoneofIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TField.Set_packed(AIndex : Integer; const AValue : boolean); 

begin
  If (F_packed=AValue) then exit;
  F_packed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TField.Setoptions(AIndex : Integer; const AValue : TFieldTypeoptionsArray); 

begin
  If (Foptions=AValue) then exit;
  Foptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TField.SetjsonName(AIndex : Integer; const AValue : String); 

begin
  If (FjsonName=AValue) then exit;
  FjsonName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TField.SetdefaultValue(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultValue=AValue) then exit;
  FdefaultValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TField.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_packed' : Result:='packed';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TField.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'options' : SetLength(Foptions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOptionTypevalue
  --------------------------------------------------------------------}


Class Function TOptionTypevalue.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOption
  --------------------------------------------------------------------}


Procedure TOption.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOption.Setvalue(AIndex : Integer; const AValue : TOptionTypevalue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceContext
  --------------------------------------------------------------------}


Procedure TSourceContext.SetfileName(AIndex : Integer; const AValue : String); 

begin
  If (FfileName=AValue) then exit;
  FfileName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsCollectdTimeSeriesResource
  --------------------------------------------------------------------}


Class Function TProjectsCollectdTimeSeriesResource.ResourceName : String;

begin
  Result:='collectdTimeSeries';
end;

Class Function TProjectsCollectdTimeSeriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmonitoringAPI;
end;

Function TProjectsCollectdTimeSeriesResource.Create(_name: string; aCreateCollectdTimeSeriesRequest : TCreateCollectdTimeSeriesRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v3/{+name}/collectdTimeSeries';
  _Methodid   = 'monitoring.projects.collectdTimeSeries.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreateCollectdTimeSeriesRequest,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsGroupsMembersResource
  --------------------------------------------------------------------}


Class Function TProjectsGroupsMembersResource.ResourceName : String;

begin
  Result:='members';
end;

Class Function TProjectsGroupsMembersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmonitoringAPI;
end;

Function TProjectsGroupsMembersResource.List(_name: string; AQuery : string = '') : TListGroupMembersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v3/{+name}/members';
  _Methodid   = 'monitoring.projects.groups.members.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListGroupMembersResponse) as TListGroupMembersResponse;
end;


Function TProjectsGroupsMembersResource.List(_name: string; AQuery : TProjectsGroupsMemberslistOptions) : TListGroupMembersResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'interval.endTime',AQuery.intervalendTime);
  AddToQuery(_Q,'interval.startTime',AQuery.intervalstartTime);
  Result:=List(_name,_Q);
end;



{ --------------------------------------------------------------------
  TProjectsGroupsResource
  --------------------------------------------------------------------}


Class Function TProjectsGroupsResource.ResourceName : String;

begin
  Result:='groups';
end;

Class Function TProjectsGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmonitoringAPI;
end;

Function TProjectsGroupsResource.List(_name: string; AQuery : string = '') : TListGroupsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v3/{+name}/groups';
  _Methodid   = 'monitoring.projects.groups.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListGroupsResponse) as TListGroupsResponse;
end;


Function TProjectsGroupsResource.List(_name: string; AQuery : TProjectsGroupslistOptions) : TListGroupsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'childrenOfGroup',AQuery.childrenOfGroup);
  AddToQuery(_Q,'ancestorsOfGroup',AQuery.ancestorsOfGroup);
  AddToQuery(_Q,'descendantsOfGroup',AQuery.descendantsOfGroup);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TProjectsGroupsResource.Get(_name: string) : TGroup;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v3/{+name}';
  _Methodid   = 'monitoring.projects.groups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGroup) as TGroup;
end;

Function TProjectsGroupsResource.Create(_name: string; aGroup : TGroup; AQuery : string = '') : TGroup;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v3/{+name}/groups';
  _Methodid   = 'monitoring.projects.groups.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aGroup,TGroup) as TGroup;
end;


Function TProjectsGroupsResource.Create(_name: string; aGroup : TGroup; AQuery : TProjectsGroupscreateOptions) : TGroup;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'validateOnly',AQuery.validateOnly);
  Result:=Create(_name,aGroup,_Q);
end;

Function TProjectsGroupsResource.Update(_name: string; aGroup : TGroup; AQuery : string = '') : TGroup;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v3/{+name}';
  _Methodid   = 'monitoring.projects.groups.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aGroup,TGroup) as TGroup;
end;


Function TProjectsGroupsResource.Update(_name: string; aGroup : TGroup; AQuery : TProjectsGroupsupdateOptions) : TGroup;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'validateOnly',AQuery.validateOnly);
  Result:=Update(_name,aGroup,_Q);
end;

Function TProjectsGroupsResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v3/{+name}';
  _Methodid   = 'monitoring.projects.groups.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



Function TProjectsGroupsResource.GetMembersInstance : TProjectsGroupsMembersResource;

begin
  if (FMembersInstance=Nil) then
    FMembersInstance:=CreateMembersResource;
  Result:=FMembersInstance;
end;

Function TProjectsGroupsResource.CreateMembersResource : TProjectsGroupsMembersResource;

begin
  Result:=CreateMembersResource(Self);
end;


Function TProjectsGroupsResource.CreateMembersResource(AOwner : TComponent) : TProjectsGroupsMembersResource;

begin
  Result:=TProjectsGroupsMembersResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TProjectsMonitoredResourceDescriptorsResource
  --------------------------------------------------------------------}


Class Function TProjectsMonitoredResourceDescriptorsResource.ResourceName : String;

begin
  Result:='monitoredResourceDescriptors';
end;

Class Function TProjectsMonitoredResourceDescriptorsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmonitoringAPI;
end;

Function TProjectsMonitoredResourceDescriptorsResource.List(_name: string; AQuery : string = '') : TListMonitoredResourceDescriptorsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v3/{+name}/monitoredResourceDescriptors';
  _Methodid   = 'monitoring.projects.monitoredResourceDescriptors.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListMonitoredResourceDescriptorsResponse) as TListMonitoredResourceDescriptorsResponse;
end;


Function TProjectsMonitoredResourceDescriptorsResource.List(_name: string; AQuery : TProjectsMonitoredResourceDescriptorslistOptions) : TListMonitoredResourceDescriptorsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TProjectsMonitoredResourceDescriptorsResource.Get(_name: string) : TMonitoredResourceDescriptor;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v3/{+name}';
  _Methodid   = 'monitoring.projects.monitoredResourceDescriptors.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMonitoredResourceDescriptor) as TMonitoredResourceDescriptor;
end;



{ --------------------------------------------------------------------
  TProjectsMetricDescriptorsResource
  --------------------------------------------------------------------}


Class Function TProjectsMetricDescriptorsResource.ResourceName : String;

begin
  Result:='metricDescriptors';
end;

Class Function TProjectsMetricDescriptorsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmonitoringAPI;
end;

Function TProjectsMetricDescriptorsResource.List(_name: string; AQuery : string = '') : TListMetricDescriptorsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v3/{+name}/metricDescriptors';
  _Methodid   = 'monitoring.projects.metricDescriptors.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListMetricDescriptorsResponse) as TListMetricDescriptorsResponse;
end;


Function TProjectsMetricDescriptorsResource.List(_name: string; AQuery : TProjectsMetricDescriptorslistOptions) : TListMetricDescriptorsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TProjectsMetricDescriptorsResource.Get(_name: string) : TMetricDescriptor;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v3/{+name}';
  _Methodid   = 'monitoring.projects.metricDescriptors.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMetricDescriptor) as TMetricDescriptor;
end;

Function TProjectsMetricDescriptorsResource.Create(_name: string; aMetricDescriptor : TMetricDescriptor) : TMetricDescriptor;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v3/{+name}/metricDescriptors';
  _Methodid   = 'monitoring.projects.metricDescriptors.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aMetricDescriptor,TMetricDescriptor) as TMetricDescriptor;
end;

Function TProjectsMetricDescriptorsResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v3/{+name}';
  _Methodid   = 'monitoring.projects.metricDescriptors.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsTimeSeriesResource
  --------------------------------------------------------------------}


Class Function TProjectsTimeSeriesResource.ResourceName : String;

begin
  Result:='timeSeries';
end;

Class Function TProjectsTimeSeriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmonitoringAPI;
end;

Function TProjectsTimeSeriesResource.List(_name: string; AQuery : string = '') : TListTimeSeriesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v3/{+name}/timeSeries';
  _Methodid   = 'monitoring.projects.timeSeries.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListTimeSeriesResponse) as TListTimeSeriesResponse;
end;


Function TProjectsTimeSeriesResource.List(_name: string; AQuery : TProjectsTimeSerieslistOptions) : TListTimeSeriesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'interval.endTime',AQuery.intervalendTime);
  AddToQuery(_Q,'interval.startTime',AQuery.intervalstartTime);
  AddToQuery(_Q,'aggregation.alignmentPeriod',AQuery.aggregationalignmentPeriod);
  AddToQuery(_Q,'aggregation.perSeriesAligner',AQuery.aggregationperSeriesAligner);
  AddToQuery(_Q,'aggregation.crossSeriesReducer',AQuery.aggregationcrossSeriesReducer);
  AddToQuery(_Q,'aggregation.groupByFields',AQuery.aggregationgroupByFields);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'view',AQuery.view);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TProjectsTimeSeriesResource.Create(_name: string; aCreateTimeSeriesRequest : TCreateTimeSeriesRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v3/{+name}/timeSeries';
  _Methodid   = 'monitoring.projects.timeSeries.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreateTimeSeriesRequest,TEmpty) as TEmpty;
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
  Result:=TmonitoringAPI;
end;



Function TProjectsResource.GetCollectdTimeSeriesInstance : TProjectsCollectdTimeSeriesResource;

begin
  if (FCollectdTimeSeriesInstance=Nil) then
    FCollectdTimeSeriesInstance:=CreateCollectdTimeSeriesResource;
  Result:=FCollectdTimeSeriesInstance;
end;

Function TProjectsResource.CreateCollectdTimeSeriesResource : TProjectsCollectdTimeSeriesResource;

begin
  Result:=CreateCollectdTimeSeriesResource(Self);
end;


Function TProjectsResource.CreateCollectdTimeSeriesResource(AOwner : TComponent) : TProjectsCollectdTimeSeriesResource;

begin
  Result:=TProjectsCollectdTimeSeriesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetGroupsMembersInstance : TProjectsGroupsMembersResource;

begin
  if (FGroupsMembersInstance=Nil) then
    FGroupsMembersInstance:=CreateGroupsMembersResource;
  Result:=FGroupsMembersInstance;
end;

Function TProjectsResource.CreateGroupsMembersResource : TProjectsGroupsMembersResource;

begin
  Result:=CreateGroupsMembersResource(Self);
end;


Function TProjectsResource.CreateGroupsMembersResource(AOwner : TComponent) : TProjectsGroupsMembersResource;

begin
  Result:=TProjectsGroupsMembersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetGroupsInstance : TProjectsGroupsResource;

begin
  if (FGroupsInstance=Nil) then
    FGroupsInstance:=CreateGroupsResource;
  Result:=FGroupsInstance;
end;

Function TProjectsResource.CreateGroupsResource : TProjectsGroupsResource;

begin
  Result:=CreateGroupsResource(Self);
end;


Function TProjectsResource.CreateGroupsResource(AOwner : TComponent) : TProjectsGroupsResource;

begin
  Result:=TProjectsGroupsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetMonitoredResourceDescriptorsInstance : TProjectsMonitoredResourceDescriptorsResource;

begin
  if (FMonitoredResourceDescriptorsInstance=Nil) then
    FMonitoredResourceDescriptorsInstance:=CreateMonitoredResourceDescriptorsResource;
  Result:=FMonitoredResourceDescriptorsInstance;
end;

Function TProjectsResource.CreateMonitoredResourceDescriptorsResource : TProjectsMonitoredResourceDescriptorsResource;

begin
  Result:=CreateMonitoredResourceDescriptorsResource(Self);
end;


Function TProjectsResource.CreateMonitoredResourceDescriptorsResource(AOwner : TComponent) : TProjectsMonitoredResourceDescriptorsResource;

begin
  Result:=TProjectsMonitoredResourceDescriptorsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetMetricDescriptorsInstance : TProjectsMetricDescriptorsResource;

begin
  if (FMetricDescriptorsInstance=Nil) then
    FMetricDescriptorsInstance:=CreateMetricDescriptorsResource;
  Result:=FMetricDescriptorsInstance;
end;

Function TProjectsResource.CreateMetricDescriptorsResource : TProjectsMetricDescriptorsResource;

begin
  Result:=CreateMetricDescriptorsResource(Self);
end;


Function TProjectsResource.CreateMetricDescriptorsResource(AOwner : TComponent) : TProjectsMetricDescriptorsResource;

begin
  Result:=TProjectsMetricDescriptorsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetTimeSeriesInstance : TProjectsTimeSeriesResource;

begin
  if (FTimeSeriesInstance=Nil) then
    FTimeSeriesInstance:=CreateTimeSeriesResource;
  Result:=FTimeSeriesInstance;
end;

Function TProjectsResource.CreateTimeSeriesResource : TProjectsTimeSeriesResource;

begin
  Result:=CreateTimeSeriesResource(Self);
end;


Function TProjectsResource.CreateTimeSeriesResource(AOwner : TComponent) : TProjectsTimeSeriesResource;

begin
  Result:=TProjectsTimeSeriesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TMonitoringAPI
  --------------------------------------------------------------------}

Class Function TMonitoringAPI.APIName : String;

begin
  Result:='monitoring';
end;

Class Function TMonitoringAPI.APIVersion : String;

begin
  Result:='v3';
end;

Class Function TMonitoringAPI.APIRevision : String;

begin
  Result:='20160425';
end;

Class Function TMonitoringAPI.APIID : String;

begin
  Result:='monitoring:v3';
end;

Class Function TMonitoringAPI.APITitle : String;

begin
  Result:='Google Monitoring API';
end;

Class Function TMonitoringAPI.APIDescription : String;

begin
  Result:='Manages your Stackdriver monitoring data and configurations. Projects must be associated with a Stackdriver account, except for the following methods: [monitoredResourceDescriptors.list](v3/projects.monitoredResourceDescriptors/list), [monitoredResourceDescriptors.get](v3/projects.monitoredResourceDescriptors/get), [metricDescriptors.list](v3/projects.metricDescriptors/list), [metricDescriptors.get](v3/projects.metricDescriptors/get), and [timeSeries.list](v3/projects.timeSeries/list).';
end;

Class Function TMonitoringAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TMonitoringAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TMonitoringAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TMonitoringAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TMonitoringAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/monitoring/api/';
end;

Class Function TMonitoringAPI.APIrootUrl : string;

begin
  Result:='https://monitoring.googleapis.com/';
end;

Class Function TMonitoringAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TMonitoringAPI.APIbaseURL : String;

begin
  Result:='https://monitoring.googleapis.com/';
end;

Class Function TMonitoringAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TMonitoringAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TMonitoringAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TMonitoringAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/monitoring';
  Result[1].Description:='View and write monitoring data for all of your Google and third-party Cloud and API projects';
  Result[2].Name:='https://www.googleapis.com/auth/monitoring.read';
  Result[2].Description:='View monitoring data for all of your Google Cloud and third-party projects';
  Result[3].Name:='https://www.googleapis.com/auth/monitoring.write';
  Result[3].Description:='Publish metric data to your Google Cloud projects';
  
end;

Class Function TMonitoringAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TMonitoringAPI.RegisterAPIResources;

begin
  TCreateCollectdTimeSeriesRequest.RegisterObject;
  TMonitoredResourceTypelabels.RegisterObject;
  TMonitoredResource.RegisterObject;
  TCollectdPayloadTypemetadata.RegisterObject;
  TCollectdPayload.RegisterObject;
  TCollectdValue.RegisterObject;
  TTypedValue.RegisterObject;
  TDistribution.RegisterObject;
  TRange.RegisterObject;
  TBucketOptions.RegisterObject;
  TLinear.RegisterObject;
  TExponential.RegisterObject;
  TExplicit.RegisterObject;
  TEmpty.RegisterObject;
  TListGroupsResponse.RegisterObject;
  TGroup.RegisterObject;
  TListGroupMembersResponse.RegisterObject;
  TListMonitoredResourceDescriptorsResponse.RegisterObject;
  TMonitoredResourceDescriptor.RegisterObject;
  TLabelDescriptor.RegisterObject;
  TListMetricDescriptorsResponse.RegisterObject;
  TMetricDescriptor.RegisterObject;
  TListTimeSeriesResponse.RegisterObject;
  TTimeSeries.RegisterObject;
  TMetricTypelabels.RegisterObject;
  TMetric.RegisterObject;
  TPoint.RegisterObject;
  TTimeInterval.RegisterObject;
  TCreateTimeSeriesRequest.RegisterObject;
  TType.RegisterObject;
  TField.RegisterObject;
  TOptionTypevalue.RegisterObject;
  TOption.RegisterObject;
  TSourceContext.RegisterObject;
end;


Function TMonitoringAPI.GetProjectsCollectdTimeSeriesInstance : TProjectsCollectdTimeSeriesResource;

begin
  if (FProjectsCollectdTimeSeriesInstance=Nil) then
    FProjectsCollectdTimeSeriesInstance:=CreateProjectsCollectdTimeSeriesResource;
  Result:=FProjectsCollectdTimeSeriesInstance;
end;

Function TMonitoringAPI.CreateProjectsCollectdTimeSeriesResource : TProjectsCollectdTimeSeriesResource;

begin
  Result:=CreateProjectsCollectdTimeSeriesResource(Self);
end;


Function TMonitoringAPI.CreateProjectsCollectdTimeSeriesResource(AOwner : TComponent) : TProjectsCollectdTimeSeriesResource;

begin
  Result:=TProjectsCollectdTimeSeriesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMonitoringAPI.GetProjectsGroupsMembersInstance : TProjectsGroupsMembersResource;

begin
  if (FProjectsGroupsMembersInstance=Nil) then
    FProjectsGroupsMembersInstance:=CreateProjectsGroupsMembersResource;
  Result:=FProjectsGroupsMembersInstance;
end;

Function TMonitoringAPI.CreateProjectsGroupsMembersResource : TProjectsGroupsMembersResource;

begin
  Result:=CreateProjectsGroupsMembersResource(Self);
end;


Function TMonitoringAPI.CreateProjectsGroupsMembersResource(AOwner : TComponent) : TProjectsGroupsMembersResource;

begin
  Result:=TProjectsGroupsMembersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMonitoringAPI.GetProjectsGroupsInstance : TProjectsGroupsResource;

begin
  if (FProjectsGroupsInstance=Nil) then
    FProjectsGroupsInstance:=CreateProjectsGroupsResource;
  Result:=FProjectsGroupsInstance;
end;

Function TMonitoringAPI.CreateProjectsGroupsResource : TProjectsGroupsResource;

begin
  Result:=CreateProjectsGroupsResource(Self);
end;


Function TMonitoringAPI.CreateProjectsGroupsResource(AOwner : TComponent) : TProjectsGroupsResource;

begin
  Result:=TProjectsGroupsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMonitoringAPI.GetProjectsMonitoredResourceDescriptorsInstance : TProjectsMonitoredResourceDescriptorsResource;

begin
  if (FProjectsMonitoredResourceDescriptorsInstance=Nil) then
    FProjectsMonitoredResourceDescriptorsInstance:=CreateProjectsMonitoredResourceDescriptorsResource;
  Result:=FProjectsMonitoredResourceDescriptorsInstance;
end;

Function TMonitoringAPI.CreateProjectsMonitoredResourceDescriptorsResource : TProjectsMonitoredResourceDescriptorsResource;

begin
  Result:=CreateProjectsMonitoredResourceDescriptorsResource(Self);
end;


Function TMonitoringAPI.CreateProjectsMonitoredResourceDescriptorsResource(AOwner : TComponent) : TProjectsMonitoredResourceDescriptorsResource;

begin
  Result:=TProjectsMonitoredResourceDescriptorsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMonitoringAPI.GetProjectsMetricDescriptorsInstance : TProjectsMetricDescriptorsResource;

begin
  if (FProjectsMetricDescriptorsInstance=Nil) then
    FProjectsMetricDescriptorsInstance:=CreateProjectsMetricDescriptorsResource;
  Result:=FProjectsMetricDescriptorsInstance;
end;

Function TMonitoringAPI.CreateProjectsMetricDescriptorsResource : TProjectsMetricDescriptorsResource;

begin
  Result:=CreateProjectsMetricDescriptorsResource(Self);
end;


Function TMonitoringAPI.CreateProjectsMetricDescriptorsResource(AOwner : TComponent) : TProjectsMetricDescriptorsResource;

begin
  Result:=TProjectsMetricDescriptorsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMonitoringAPI.GetProjectsTimeSeriesInstance : TProjectsTimeSeriesResource;

begin
  if (FProjectsTimeSeriesInstance=Nil) then
    FProjectsTimeSeriesInstance:=CreateProjectsTimeSeriesResource;
  Result:=FProjectsTimeSeriesInstance;
end;

Function TMonitoringAPI.CreateProjectsTimeSeriesResource : TProjectsTimeSeriesResource;

begin
  Result:=CreateProjectsTimeSeriesResource(Self);
end;


Function TMonitoringAPI.CreateProjectsTimeSeriesResource(AOwner : TComponent) : TProjectsTimeSeriesResource;

begin
  Result:=TProjectsTimeSeriesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMonitoringAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TMonitoringAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TMonitoringAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TMonitoringAPI.RegisterAPI;
end.
