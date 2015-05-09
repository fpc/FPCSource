unit googlecloudmonitoring;
{
   **********************************************************************
      This file is part of the Free Component Library (FCL)
      Copyright (c) 2015 The free pascal team.
  
      See the file COPYING.FPC, included in this distribution,
      for details about the copyright.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
   **********************************************************************
}
//Generated on: 9-5-15 13:22:50
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TDeleteMetricDescriptorResponse = class;
  TListMetricDescriptorsRequest = class;
  TListMetricDescriptorsResponse = class;
  TListTimeseriesDescriptorsRequest = class;
  TListTimeseriesDescriptorsResponse = class;
  TListTimeseriesRequest = class;
  TListTimeseriesResponse = class;
  TMetricDescriptor = class;
  TMetricDescriptorLabelDescriptor = class;
  TMetricDescriptorTypeDescriptor = class;
  TPoint = class;
  TPointDistribution = class;
  TPointDistributionBucket = class;
  TPointDistributionOverflowBucket = class;
  TPointDistributionUnderflowBucket = class;
  TTimeseries = class;
  TTimeseriesDescriptor = class;
  TTimeseriesDescriptorLabel = class;
  TTimeseriesPoint = class;
  TWriteTimeseriesRequest = class;
  TWriteTimeseriesResponse = class;
  TDeleteMetricDescriptorResponseArray = Array of TDeleteMetricDescriptorResponse;
  TListMetricDescriptorsRequestArray = Array of TListMetricDescriptorsRequest;
  TListMetricDescriptorsResponseArray = Array of TListMetricDescriptorsResponse;
  TListTimeseriesDescriptorsRequestArray = Array of TListTimeseriesDescriptorsRequest;
  TListTimeseriesDescriptorsResponseArray = Array of TListTimeseriesDescriptorsResponse;
  TListTimeseriesRequestArray = Array of TListTimeseriesRequest;
  TListTimeseriesResponseArray = Array of TListTimeseriesResponse;
  TMetricDescriptorArray = Array of TMetricDescriptor;
  TMetricDescriptorLabelDescriptorArray = Array of TMetricDescriptorLabelDescriptor;
  TMetricDescriptorTypeDescriptorArray = Array of TMetricDescriptorTypeDescriptor;
  TPointArray = Array of TPoint;
  TPointDistributionArray = Array of TPointDistribution;
  TPointDistributionBucketArray = Array of TPointDistributionBucket;
  TPointDistributionOverflowBucketArray = Array of TPointDistributionOverflowBucket;
  TPointDistributionUnderflowBucketArray = Array of TPointDistributionUnderflowBucket;
  TTimeseriesArray = Array of TTimeseries;
  TTimeseriesDescriptorArray = Array of TTimeseriesDescriptor;
  TTimeseriesDescriptorLabelArray = Array of TTimeseriesDescriptorLabel;
  TTimeseriesPointArray = Array of TTimeseriesPoint;
  TWriteTimeseriesRequestArray = Array of TWriteTimeseriesRequest;
  TWriteTimeseriesResponseArray = Array of TWriteTimeseriesResponse;
  //Anonymous types, using auto-generated names
  TTimeseriesDescriptorTypelabels = class;
  TWriteTimeseriesRequestTypecommonLabels = class;
  TListMetricDescriptorsResponseTypemetricsArray = Array of TMetricDescriptor;
  TListTimeseriesDescriptorsResponseTypetimeseriesArray = Array of TTimeseriesDescriptor;
  TListTimeseriesResponseTypetimeseriesArray = Array of TTimeseries;
  TMetricDescriptorTypelabelsArray = Array of TMetricDescriptorLabelDescriptor;
  TPointDistributionTypebucketsArray = Array of TPointDistributionBucket;
  TTimeseriesTypepointsArray = Array of TPoint;
  TWriteTimeseriesRequestTypetimeseriesArray = Array of TTimeseriesPoint;
  
  { --------------------------------------------------------------------
    TDeleteMetricDescriptorResponse
    --------------------------------------------------------------------}
  
  TDeleteMetricDescriptorResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
  end;
  TDeleteMetricDescriptorResponseClass = Class of TDeleteMetricDescriptorResponse;
  
  { --------------------------------------------------------------------
    TListMetricDescriptorsRequest
    --------------------------------------------------------------------}
  
  TListMetricDescriptorsRequest = Class(TGoogleBaseObject)
  Private
    Fkind : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
  end;
  TListMetricDescriptorsRequestClass = Class of TListMetricDescriptorsRequest;
  
  { --------------------------------------------------------------------
    TListMetricDescriptorsResponse
    --------------------------------------------------------------------}
  
  TListMetricDescriptorsResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fmetrics : TListMetricDescriptorsResponseTypemetricsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TListMetricDescriptorsResponseTypemetricsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property metrics : TListMetricDescriptorsResponseTypemetricsArray Index 8 Read Fmetrics Write Setmetrics;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TListMetricDescriptorsResponseClass = Class of TListMetricDescriptorsResponse;
  
  { --------------------------------------------------------------------
    TListTimeseriesDescriptorsRequest
    --------------------------------------------------------------------}
  
  TListTimeseriesDescriptorsRequest = Class(TGoogleBaseObject)
  Private
    Fkind : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
  end;
  TListTimeseriesDescriptorsRequestClass = Class of TListTimeseriesDescriptorsRequest;
  
  { --------------------------------------------------------------------
    TListTimeseriesDescriptorsResponse
    --------------------------------------------------------------------}
  
  TListTimeseriesDescriptorsResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Foldest : TDatetime;
    Ftimeseries : TListTimeseriesDescriptorsResponseTypetimeseriesArray;
    Fyoungest : TDatetime;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setoldest(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settimeseries(AIndex : Integer; AValue : TListTimeseriesDescriptorsResponseTypetimeseriesArray); virtual;
    Procedure Setyoungest(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property oldest : TDatetime Index 16 Read Foldest Write Setoldest;
    Property timeseries : TListTimeseriesDescriptorsResponseTypetimeseriesArray Index 24 Read Ftimeseries Write Settimeseries;
    Property youngest : TDatetime Index 32 Read Fyoungest Write Setyoungest;
  end;
  TListTimeseriesDescriptorsResponseClass = Class of TListTimeseriesDescriptorsResponse;
  
  { --------------------------------------------------------------------
    TListTimeseriesRequest
    --------------------------------------------------------------------}
  
  TListTimeseriesRequest = Class(TGoogleBaseObject)
  Private
    Fkind : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
  end;
  TListTimeseriesRequestClass = Class of TListTimeseriesRequest;
  
  { --------------------------------------------------------------------
    TListTimeseriesResponse
    --------------------------------------------------------------------}
  
  TListTimeseriesResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Foldest : TDatetime;
    Ftimeseries : TListTimeseriesResponseTypetimeseriesArray;
    Fyoungest : TDatetime;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setoldest(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settimeseries(AIndex : Integer; AValue : TListTimeseriesResponseTypetimeseriesArray); virtual;
    Procedure Setyoungest(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property oldest : TDatetime Index 16 Read Foldest Write Setoldest;
    Property timeseries : TListTimeseriesResponseTypetimeseriesArray Index 24 Read Ftimeseries Write Settimeseries;
    Property youngest : TDatetime Index 32 Read Fyoungest Write Setyoungest;
  end;
  TListTimeseriesResponseClass = Class of TListTimeseriesResponse;
  
  { --------------------------------------------------------------------
    TMetricDescriptor
    --------------------------------------------------------------------}
  
  TMetricDescriptor = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Flabels : TMetricDescriptorTypelabelsArray;
    Fname : String;
    Fproject : String;
    FtypeDescriptor : TMetricDescriptorTypeDescriptor;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TMetricDescriptorTypelabelsArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setproject(AIndex : Integer; AValue : String); virtual;
    Procedure SettypeDescriptor(AIndex : Integer; AValue : TMetricDescriptorTypeDescriptor); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property labels : TMetricDescriptorTypelabelsArray Index 8 Read Flabels Write Setlabels;
    Property name : String Index 16 Read Fname Write Setname;
    Property project : String Index 24 Read Fproject Write Setproject;
    Property typeDescriptor : TMetricDescriptorTypeDescriptor Index 32 Read FtypeDescriptor Write SettypeDescriptor;
  end;
  TMetricDescriptorClass = Class of TMetricDescriptor;
  
  { --------------------------------------------------------------------
    TMetricDescriptorLabelDescriptor
    --------------------------------------------------------------------}
  
  TMetricDescriptorLabelDescriptor = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Fkey : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property key : String Index 8 Read Fkey Write Setkey;
  end;
  TMetricDescriptorLabelDescriptorClass = Class of TMetricDescriptorLabelDescriptor;
  
  { --------------------------------------------------------------------
    TMetricDescriptorTypeDescriptor
    --------------------------------------------------------------------}
  
  TMetricDescriptorTypeDescriptor = Class(TGoogleBaseObject)
  Private
    FmetricType : String;
    FvalueType : String;
  Protected
    //Property setters
    Procedure SetmetricType(AIndex : Integer; AValue : String); virtual;
    Procedure SetvalueType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property metricType : String Index 0 Read FmetricType Write SetmetricType;
    Property valueType : String Index 8 Read FvalueType Write SetvalueType;
  end;
  TMetricDescriptorTypeDescriptorClass = Class of TMetricDescriptorTypeDescriptor;
  
  { --------------------------------------------------------------------
    TPoint
    --------------------------------------------------------------------}
  
  TPoint = Class(TGoogleBaseObject)
  Private
    FboolValue : boolean;
    FdistributionValue : TPointDistribution;
    FdoubleValue : double;
    F_end : TDatetime;
    Fint64Value : String;
    Fstart : TDatetime;
    FstringValue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetboolValue(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdistributionValue(AIndex : Integer; AValue : TPointDistribution); virtual;
    Procedure SetdoubleValue(AIndex : Integer; AValue : double); virtual;
    Procedure Set_end(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setint64Value(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetstringValue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property boolValue : boolean Index 0 Read FboolValue Write SetboolValue;
    Property distributionValue : TPointDistribution Index 8 Read FdistributionValue Write SetdistributionValue;
    Property doubleValue : double Index 16 Read FdoubleValue Write SetdoubleValue;
    Property _end : TDatetime Index 24 Read F_end Write Set_end;
    Property int64Value : String Index 32 Read Fint64Value Write Setint64Value;
    Property start : TDatetime Index 40 Read Fstart Write Setstart;
    Property stringValue : String Index 48 Read FstringValue Write SetstringValue;
  end;
  TPointClass = Class of TPoint;
  
  { --------------------------------------------------------------------
    TPointDistribution
    --------------------------------------------------------------------}
  
  TPointDistribution = Class(TGoogleBaseObject)
  Private
    Fbuckets : TPointDistributionTypebucketsArray;
    FoverflowBucket : TPointDistributionOverflowBucket;
    FunderflowBucket : TPointDistributionUnderflowBucket;
  Protected
    //Property setters
    Procedure Setbuckets(AIndex : Integer; AValue : TPointDistributionTypebucketsArray); virtual;
    Procedure SetoverflowBucket(AIndex : Integer; AValue : TPointDistributionOverflowBucket); virtual;
    Procedure SetunderflowBucket(AIndex : Integer; AValue : TPointDistributionUnderflowBucket); virtual;
  Public
  Published
    Property buckets : TPointDistributionTypebucketsArray Index 0 Read Fbuckets Write Setbuckets;
    Property overflowBucket : TPointDistributionOverflowBucket Index 8 Read FoverflowBucket Write SetoverflowBucket;
    Property underflowBucket : TPointDistributionUnderflowBucket Index 16 Read FunderflowBucket Write SetunderflowBucket;
  end;
  TPointDistributionClass = Class of TPointDistribution;
  
  { --------------------------------------------------------------------
    TPointDistributionBucket
    --------------------------------------------------------------------}
  
  TPointDistributionBucket = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    FlowerBound : double;
    FupperBound : double;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure SetlowerBound(AIndex : Integer; AValue : double); virtual;
    Procedure SetupperBound(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property lowerBound : double Index 8 Read FlowerBound Write SetlowerBound;
    Property upperBound : double Index 16 Read FupperBound Write SetupperBound;
  end;
  TPointDistributionBucketClass = Class of TPointDistributionBucket;
  
  { --------------------------------------------------------------------
    TPointDistributionOverflowBucket
    --------------------------------------------------------------------}
  
  TPointDistributionOverflowBucket = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    FlowerBound : double;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure SetlowerBound(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property lowerBound : double Index 8 Read FlowerBound Write SetlowerBound;
  end;
  TPointDistributionOverflowBucketClass = Class of TPointDistributionOverflowBucket;
  
  { --------------------------------------------------------------------
    TPointDistributionUnderflowBucket
    --------------------------------------------------------------------}
  
  TPointDistributionUnderflowBucket = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    FupperBound : double;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure SetupperBound(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property upperBound : double Index 8 Read FupperBound Write SetupperBound;
  end;
  TPointDistributionUnderflowBucketClass = Class of TPointDistributionUnderflowBucket;
  
  { --------------------------------------------------------------------
    TTimeseries
    --------------------------------------------------------------------}
  
  TTimeseries = Class(TGoogleBaseObject)
  Private
    Fpoints : TTimeseriesTypepointsArray;
    FtimeseriesDesc : TTimeseriesDescriptor;
  Protected
    //Property setters
    Procedure Setpoints(AIndex : Integer; AValue : TTimeseriesTypepointsArray); virtual;
    Procedure SettimeseriesDesc(AIndex : Integer; AValue : TTimeseriesDescriptor); virtual;
  Public
  Published
    Property points : TTimeseriesTypepointsArray Index 0 Read Fpoints Write Setpoints;
    Property timeseriesDesc : TTimeseriesDescriptor Index 8 Read FtimeseriesDesc Write SettimeseriesDesc;
  end;
  TTimeseriesClass = Class of TTimeseries;
  
  { --------------------------------------------------------------------
    TTimeseriesDescriptorTypelabels
    --------------------------------------------------------------------}
  
  TTimeseriesDescriptorTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTimeseriesDescriptorTypelabelsClass = Class of TTimeseriesDescriptorTypelabels;
  
  { --------------------------------------------------------------------
    TTimeseriesDescriptor
    --------------------------------------------------------------------}
  
  TTimeseriesDescriptor = Class(TGoogleBaseObject)
  Private
    Flabels : TTimeseriesDescriptorTypelabels;
    Fmetric : String;
    Fproject : String;
  Protected
    //Property setters
    Procedure Setlabels(AIndex : Integer; AValue : TTimeseriesDescriptorTypelabels); virtual;
    Procedure Setmetric(AIndex : Integer; AValue : String); virtual;
    Procedure Setproject(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property labels : TTimeseriesDescriptorTypelabels Index 0 Read Flabels Write Setlabels;
    Property metric : String Index 8 Read Fmetric Write Setmetric;
    Property project : String Index 16 Read Fproject Write Setproject;
  end;
  TTimeseriesDescriptorClass = Class of TTimeseriesDescriptor;
  
  { --------------------------------------------------------------------
    TTimeseriesDescriptorLabel
    --------------------------------------------------------------------}
  
  TTimeseriesDescriptorLabel = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TTimeseriesDescriptorLabelClass = Class of TTimeseriesDescriptorLabel;
  
  { --------------------------------------------------------------------
    TTimeseriesPoint
    --------------------------------------------------------------------}
  
  TTimeseriesPoint = Class(TGoogleBaseObject)
  Private
    Fpoint : TPoint;
    FtimeseriesDesc : TTimeseriesDescriptor;
  Protected
    //Property setters
    Procedure Setpoint(AIndex : Integer; AValue : TPoint); virtual;
    Procedure SettimeseriesDesc(AIndex : Integer; AValue : TTimeseriesDescriptor); virtual;
  Public
  Published
    Property point : TPoint Index 0 Read Fpoint Write Setpoint;
    Property timeseriesDesc : TTimeseriesDescriptor Index 8 Read FtimeseriesDesc Write SettimeseriesDesc;
  end;
  TTimeseriesPointClass = Class of TTimeseriesPoint;
  
  { --------------------------------------------------------------------
    TWriteTimeseriesRequestTypecommonLabels
    --------------------------------------------------------------------}
  
  TWriteTimeseriesRequestTypecommonLabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWriteTimeseriesRequestTypecommonLabelsClass = Class of TWriteTimeseriesRequestTypecommonLabels;
  
  { --------------------------------------------------------------------
    TWriteTimeseriesRequest
    --------------------------------------------------------------------}
  
  TWriteTimeseriesRequest = Class(TGoogleBaseObject)
  Private
    FcommonLabels : TWriteTimeseriesRequestTypecommonLabels;
    Ftimeseries : TWriteTimeseriesRequestTypetimeseriesArray;
  Protected
    //Property setters
    Procedure SetcommonLabels(AIndex : Integer; AValue : TWriteTimeseriesRequestTypecommonLabels); virtual;
    Procedure Settimeseries(AIndex : Integer; AValue : TWriteTimeseriesRequestTypetimeseriesArray); virtual;
  Public
  Published
    Property commonLabels : TWriteTimeseriesRequestTypecommonLabels Index 0 Read FcommonLabels Write SetcommonLabels;
    Property timeseries : TWriteTimeseriesRequestTypetimeseriesArray Index 8 Read Ftimeseries Write Settimeseries;
  end;
  TWriteTimeseriesRequestClass = Class of TWriteTimeseriesRequest;
  
  { --------------------------------------------------------------------
    TWriteTimeseriesResponse
    --------------------------------------------------------------------}
  
  TWriteTimeseriesResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
  end;
  TWriteTimeseriesResponseClass = Class of TWriteTimeseriesResponse;
  
  { --------------------------------------------------------------------
    TMetricDescriptorsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMetricDescriptorsResource, method List
  
  TMetricDescriptorsListOptions = Record
    count : integer;
    pageToken : String;
    query : String;
  end;
  
  TMetricDescriptorsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(project: string; aMetricDescriptor : TMetricDescriptor) : TMetricDescriptor;overload;
    Function Delete(metric: string; project: string) : TDeleteMetricDescriptorResponse;
    Function List(project: string; aListMetricDescriptorsRequest : TListMetricDescriptorsRequest; AQuery : string  = '') : TListMetricDescriptorsResponse;
    Function List(project: string; aListMetricDescriptorsRequest : TListMetricDescriptorsRequest; AQuery : TMetricDescriptorslistOptions) : TListMetricDescriptorsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTimeseriesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTimeseriesResource, method List
  
  TTimeseriesListOptions = Record
    aggregator : String;
    count : integer;
    labels : String;
    oldest : String;
    pageToken : String;
    timespan : String;
    window : String;
    youngest : String;
  end;
  
  TTimeseriesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(metric: string; project: string; aListTimeseriesRequest : TListTimeseriesRequest; AQuery : string  = '') : TListTimeseriesResponse;
    Function List(metric: string; project: string; aListTimeseriesRequest : TListTimeseriesRequest; AQuery : TTimeserieslistOptions) : TListTimeseriesResponse;
    Function Write(project: string; aWriteTimeseriesRequest : TWriteTimeseriesRequest) : TWriteTimeseriesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTimeseriesDescriptorsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTimeseriesDescriptorsResource, method List
  
  TTimeseriesDescriptorsListOptions = Record
    aggregator : String;
    count : integer;
    labels : String;
    oldest : String;
    pageToken : String;
    timespan : String;
    window : String;
    youngest : String;
  end;
  
  TTimeseriesDescriptorsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(metric: string; project: string; aListTimeseriesDescriptorsRequest : TListTimeseriesDescriptorsRequest; AQuery : string  = '') : TListTimeseriesDescriptorsResponse;
    Function List(metric: string; project: string; aListTimeseriesDescriptorsRequest : TListTimeseriesDescriptorsRequest; AQuery : TTimeseriesDescriptorslistOptions) : TListTimeseriesDescriptorsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCloudmonitoringAPI
    --------------------------------------------------------------------}
  
  TCloudmonitoringAPI = Class(TGoogleAPI)
  Private
    FMetricDescriptorsInstance : TMetricDescriptorsResource;
    FTimeseriesInstance : TTimeseriesResource;
    FTimeseriesDescriptorsInstance : TTimeseriesDescriptorsResource;
    Function GetMetricDescriptorsInstance : TMetricDescriptorsResource;virtual;
    Function GetTimeseriesInstance : TTimeseriesResource;virtual;
    Function GetTimeseriesDescriptorsInstance : TTimeseriesDescriptorsResource;virtual;
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
    Function CreateMetricDescriptorsResource(AOwner : TComponent) : TMetricDescriptorsResource;virtual;overload;
    Function CreateMetricDescriptorsResource : TMetricDescriptorsResource;virtual;overload;
    Function CreateTimeseriesResource(AOwner : TComponent) : TTimeseriesResource;virtual;overload;
    Function CreateTimeseriesResource : TTimeseriesResource;virtual;overload;
    Function CreateTimeseriesDescriptorsResource(AOwner : TComponent) : TTimeseriesDescriptorsResource;virtual;overload;
    Function CreateTimeseriesDescriptorsResource : TTimeseriesDescriptorsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property MetricDescriptorsResource : TMetricDescriptorsResource Read GetMetricDescriptorsInstance;
    Property TimeseriesResource : TTimeseriesResource Read GetTimeseriesInstance;
    Property TimeseriesDescriptorsResource : TTimeseriesDescriptorsResource Read GetTimeseriesDescriptorsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TDeleteMetricDescriptorResponse
  --------------------------------------------------------------------}


Procedure TDeleteMetricDescriptorResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListMetricDescriptorsRequest
  --------------------------------------------------------------------}


Procedure TListMetricDescriptorsRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListMetricDescriptorsResponse
  --------------------------------------------------------------------}


Procedure TListMetricDescriptorsResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMetricDescriptorsResponse.Setmetrics(AIndex : Integer; AValue : TListMetricDescriptorsResponseTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMetricDescriptorsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTimeseriesDescriptorsRequest
  --------------------------------------------------------------------}


Procedure TListTimeseriesDescriptorsRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTimeseriesDescriptorsResponse
  --------------------------------------------------------------------}


Procedure TListTimeseriesDescriptorsResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesDescriptorsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesDescriptorsResponse.Setoldest(AIndex : Integer; AValue : TDatetime); 

begin
  If (Foldest=AValue) then exit;
  Foldest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesDescriptorsResponse.Settimeseries(AIndex : Integer; AValue : TListTimeseriesDescriptorsResponseTypetimeseriesArray); 

begin
  If (Ftimeseries=AValue) then exit;
  Ftimeseries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesDescriptorsResponse.Setyoungest(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fyoungest=AValue) then exit;
  Fyoungest:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTimeseriesRequest
  --------------------------------------------------------------------}


Procedure TListTimeseriesRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTimeseriesResponse
  --------------------------------------------------------------------}


Procedure TListTimeseriesResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesResponse.Setoldest(AIndex : Integer; AValue : TDatetime); 

begin
  If (Foldest=AValue) then exit;
  Foldest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesResponse.Settimeseries(AIndex : Integer; AValue : TListTimeseriesResponseTypetimeseriesArray); 

begin
  If (Ftimeseries=AValue) then exit;
  Ftimeseries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesResponse.Setyoungest(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fyoungest=AValue) then exit;
  Fyoungest:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetricDescriptor
  --------------------------------------------------------------------}


Procedure TMetricDescriptor.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Setlabels(AIndex : Integer; AValue : TMetricDescriptorTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Setproject(AIndex : Integer; AValue : String); 

begin
  If (Fproject=AValue) then exit;
  Fproject:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.SettypeDescriptor(AIndex : Integer; AValue : TMetricDescriptorTypeDescriptor); 

begin
  If (FtypeDescriptor=AValue) then exit;
  FtypeDescriptor:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetricDescriptorLabelDescriptor
  --------------------------------------------------------------------}


Procedure TMetricDescriptorLabelDescriptor.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptorLabelDescriptor.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetricDescriptorTypeDescriptor
  --------------------------------------------------------------------}


Procedure TMetricDescriptorTypeDescriptor.SetmetricType(AIndex : Integer; AValue : String); 

begin
  If (FmetricType=AValue) then exit;
  FmetricType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptorTypeDescriptor.SetvalueType(AIndex : Integer; AValue : String); 

begin
  If (FvalueType=AValue) then exit;
  FvalueType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPoint
  --------------------------------------------------------------------}


Procedure TPoint.SetboolValue(AIndex : Integer; AValue : boolean); 

begin
  If (FboolValue=AValue) then exit;
  FboolValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPoint.SetdistributionValue(AIndex : Integer; AValue : TPointDistribution); 

begin
  If (FdistributionValue=AValue) then exit;
  FdistributionValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPoint.SetdoubleValue(AIndex : Integer; AValue : double); 

begin
  If (FdoubleValue=AValue) then exit;
  FdoubleValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPoint.Set_end(AIndex : Integer; AValue : TDatetime); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPoint.Setint64Value(AIndex : Integer; AValue : String); 

begin
  If (Fint64Value=AValue) then exit;
  Fint64Value:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPoint.Setstart(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPoint.SetstringValue(AIndex : Integer; AValue : String); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPoint.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPointDistribution
  --------------------------------------------------------------------}


Procedure TPointDistribution.Setbuckets(AIndex : Integer; AValue : TPointDistributionTypebucketsArray); 

begin
  If (Fbuckets=AValue) then exit;
  Fbuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPointDistribution.SetoverflowBucket(AIndex : Integer; AValue : TPointDistributionOverflowBucket); 

begin
  If (FoverflowBucket=AValue) then exit;
  FoverflowBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPointDistribution.SetunderflowBucket(AIndex : Integer; AValue : TPointDistributionUnderflowBucket); 

begin
  If (FunderflowBucket=AValue) then exit;
  FunderflowBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPointDistributionBucket
  --------------------------------------------------------------------}


Procedure TPointDistributionBucket.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPointDistributionBucket.SetlowerBound(AIndex : Integer; AValue : double); 

begin
  If (FlowerBound=AValue) then exit;
  FlowerBound:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPointDistributionBucket.SetupperBound(AIndex : Integer; AValue : double); 

begin
  If (FupperBound=AValue) then exit;
  FupperBound:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPointDistributionOverflowBucket
  --------------------------------------------------------------------}


Procedure TPointDistributionOverflowBucket.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPointDistributionOverflowBucket.SetlowerBound(AIndex : Integer; AValue : double); 

begin
  If (FlowerBound=AValue) then exit;
  FlowerBound:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPointDistributionUnderflowBucket
  --------------------------------------------------------------------}


Procedure TPointDistributionUnderflowBucket.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPointDistributionUnderflowBucket.SetupperBound(AIndex : Integer; AValue : double); 

begin
  If (FupperBound=AValue) then exit;
  FupperBound:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeseries
  --------------------------------------------------------------------}


Procedure TTimeseries.Setpoints(AIndex : Integer; AValue : TTimeseriesTypepointsArray); 

begin
  If (Fpoints=AValue) then exit;
  Fpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeseries.SettimeseriesDesc(AIndex : Integer; AValue : TTimeseriesDescriptor); 

begin
  If (FtimeseriesDesc=AValue) then exit;
  FtimeseriesDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeseriesDescriptorTypelabels
  --------------------------------------------------------------------}


Class Function TTimeseriesDescriptorTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTimeseriesDescriptor
  --------------------------------------------------------------------}


Procedure TTimeseriesDescriptor.Setlabels(AIndex : Integer; AValue : TTimeseriesDescriptorTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeseriesDescriptor.Setmetric(AIndex : Integer; AValue : String); 

begin
  If (Fmetric=AValue) then exit;
  Fmetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeseriesDescriptor.Setproject(AIndex : Integer; AValue : String); 

begin
  If (Fproject=AValue) then exit;
  Fproject:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeseriesDescriptorLabel
  --------------------------------------------------------------------}


Procedure TTimeseriesDescriptorLabel.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeseriesDescriptorLabel.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeseriesPoint
  --------------------------------------------------------------------}


Procedure TTimeseriesPoint.Setpoint(AIndex : Integer; AValue : TPoint); 

begin
  If (Fpoint=AValue) then exit;
  Fpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeseriesPoint.SettimeseriesDesc(AIndex : Integer; AValue : TTimeseriesDescriptor); 

begin
  If (FtimeseriesDesc=AValue) then exit;
  FtimeseriesDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWriteTimeseriesRequestTypecommonLabels
  --------------------------------------------------------------------}


Class Function TWriteTimeseriesRequestTypecommonLabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWriteTimeseriesRequest
  --------------------------------------------------------------------}


Procedure TWriteTimeseriesRequest.SetcommonLabels(AIndex : Integer; AValue : TWriteTimeseriesRequestTypecommonLabels); 

begin
  If (FcommonLabels=AValue) then exit;
  FcommonLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteTimeseriesRequest.Settimeseries(AIndex : Integer; AValue : TWriteTimeseriesRequestTypetimeseriesArray); 

begin
  If (Ftimeseries=AValue) then exit;
  Ftimeseries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWriteTimeseriesResponse
  --------------------------------------------------------------------}


Procedure TWriteTimeseriesResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetricDescriptorsResource
  --------------------------------------------------------------------}


Class Function TMetricDescriptorsResource.ResourceName : String;

begin
  Result:='metricDescriptors';
end;

Class Function TMetricDescriptorsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudmonitoringAPI;
end;

Function TMetricDescriptorsResource.Create(project: string; aMetricDescriptor : TMetricDescriptor) : TMetricDescriptor;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/metricDescriptors';
  _Methodid   = 'cloudmonitoring.metricDescriptors.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aMetricDescriptor,TMetricDescriptor) as TMetricDescriptor;
end;

Function TMetricDescriptorsResource.Delete(metric: string; project: string) : TDeleteMetricDescriptorResponse;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/metricDescriptors/{metric}';
  _Methodid   = 'cloudmonitoring.metricDescriptors.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['metric',metric,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDeleteMetricDescriptorResponse) as TDeleteMetricDescriptorResponse;
end;

Function TMetricDescriptorsResource.List(project: string; aListMetricDescriptorsRequest : TListMetricDescriptorsRequest; AQuery : string = '') : TListMetricDescriptorsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/metricDescriptors';
  _Methodid   = 'cloudmonitoring.metricDescriptors.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aListMetricDescriptorsRequest,TListMetricDescriptorsResponse) as TListMetricDescriptorsResponse;
end;


Function TMetricDescriptorsResource.List(project: string; aListMetricDescriptorsRequest : TListMetricDescriptorsRequest; AQuery : TMetricDescriptorslistOptions) : TListMetricDescriptorsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'count',AQuery.count);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'query',AQuery.query);
  Result:=List(project,aListMetricDescriptorsRequest,_Q);
end;



{ --------------------------------------------------------------------
  TTimeseriesResource
  --------------------------------------------------------------------}


Class Function TTimeseriesResource.ResourceName : String;

begin
  Result:='timeseries';
end;

Class Function TTimeseriesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudmonitoringAPI;
end;

Function TTimeseriesResource.List(metric: string; project: string; aListTimeseriesRequest : TListTimeseriesRequest; AQuery : string = '') : TListTimeseriesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/timeseries/{metric}';
  _Methodid   = 'cloudmonitoring.timeseries.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['metric',metric,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aListTimeseriesRequest,TListTimeseriesResponse) as TListTimeseriesResponse;
end;


Function TTimeseriesResource.List(metric: string; project: string; aListTimeseriesRequest : TListTimeseriesRequest; AQuery : TTimeserieslistOptions) : TListTimeseriesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'aggregator',AQuery.aggregator);
  AddToQuery(_Q,'count',AQuery.count);
  AddToQuery(_Q,'labels',AQuery.labels);
  AddToQuery(_Q,'oldest',AQuery.oldest);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'timespan',AQuery.timespan);
  AddToQuery(_Q,'window',AQuery.window);
  AddToQuery(_Q,'youngest',AQuery.youngest);
  Result:=List(metric,project,aListTimeseriesRequest,_Q);
end;

Function TTimeseriesResource.Write(project: string; aWriteTimeseriesRequest : TWriteTimeseriesRequest) : TWriteTimeseriesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/timeseries:write';
  _Methodid   = 'cloudmonitoring.timeseries.write';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aWriteTimeseriesRequest,TWriteTimeseriesResponse) as TWriteTimeseriesResponse;
end;



{ --------------------------------------------------------------------
  TTimeseriesDescriptorsResource
  --------------------------------------------------------------------}


Class Function TTimeseriesDescriptorsResource.ResourceName : String;

begin
  Result:='timeseriesDescriptors';
end;

Class Function TTimeseriesDescriptorsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudmonitoringAPI;
end;

Function TTimeseriesDescriptorsResource.List(metric: string; project: string; aListTimeseriesDescriptorsRequest : TListTimeseriesDescriptorsRequest; AQuery : string = '') : TListTimeseriesDescriptorsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/timeseriesDescriptors/{metric}';
  _Methodid   = 'cloudmonitoring.timeseriesDescriptors.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['metric',metric,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aListTimeseriesDescriptorsRequest,TListTimeseriesDescriptorsResponse) as TListTimeseriesDescriptorsResponse;
end;


Function TTimeseriesDescriptorsResource.List(metric: string; project: string; aListTimeseriesDescriptorsRequest : TListTimeseriesDescriptorsRequest; AQuery : TTimeseriesDescriptorslistOptions) : TListTimeseriesDescriptorsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'aggregator',AQuery.aggregator);
  AddToQuery(_Q,'count',AQuery.count);
  AddToQuery(_Q,'labels',AQuery.labels);
  AddToQuery(_Q,'oldest',AQuery.oldest);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'timespan',AQuery.timespan);
  AddToQuery(_Q,'window',AQuery.window);
  AddToQuery(_Q,'youngest',AQuery.youngest);
  Result:=List(metric,project,aListTimeseriesDescriptorsRequest,_Q);
end;



{ --------------------------------------------------------------------
  TCloudmonitoringAPI
  --------------------------------------------------------------------}

Class Function TCloudmonitoringAPI.APIName : String;

begin
  Result:='cloudmonitoring';
end;

Class Function TCloudmonitoringAPI.APIVersion : String;

begin
  Result:='v2beta2';
end;

Class Function TCloudmonitoringAPI.APIRevision : String;

begin
  Result:='20150311';
end;

Class Function TCloudmonitoringAPI.APIID : String;

begin
  Result:='cloudmonitoring:v2beta2';
end;

Class Function TCloudmonitoringAPI.APITitle : String;

begin
  Result:='Cloud Monitoring API';
end;

Class Function TCloudmonitoringAPI.APIDescription : String;

begin
  Result:='API for accessing Google Cloud and API monitoring data.';
end;

Class Function TCloudmonitoringAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCloudmonitoringAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCloudmonitoringAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCloudmonitoringAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCloudmonitoringAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/monitoring/v2beta2/';
end;

Class Function TCloudmonitoringAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TCloudmonitoringAPI.APIbasePath : string;

begin
  Result:='/cloudmonitoring/v2beta2/projects/';
end;

Class Function TCloudmonitoringAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/cloudmonitoring/v2beta2/projects/';
end;

Class Function TCloudmonitoringAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCloudmonitoringAPI.APIservicePath : string;

begin
  Result:='cloudmonitoring/v2beta2/projects/';
end;

Class Function TCloudmonitoringAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCloudmonitoringAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/monitoring';
  Result[0].Description:='View and write monitoring data for all of your Google and third-party Cloud and API projects';
  
end;

Class Function TCloudmonitoringAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TCloudmonitoringAPI.RegisterAPIResources;

begin
  TDeleteMetricDescriptorResponse.RegisterObject;
  TListMetricDescriptorsRequest.RegisterObject;
  TListMetricDescriptorsResponse.RegisterObject;
  TListTimeseriesDescriptorsRequest.RegisterObject;
  TListTimeseriesDescriptorsResponse.RegisterObject;
  TListTimeseriesRequest.RegisterObject;
  TListTimeseriesResponse.RegisterObject;
  TMetricDescriptor.RegisterObject;
  TMetricDescriptorLabelDescriptor.RegisterObject;
  TMetricDescriptorTypeDescriptor.RegisterObject;
  TPoint.RegisterObject;
  TPointDistribution.RegisterObject;
  TPointDistributionBucket.RegisterObject;
  TPointDistributionOverflowBucket.RegisterObject;
  TPointDistributionUnderflowBucket.RegisterObject;
  TTimeseries.RegisterObject;
  TTimeseriesDescriptorTypelabels.RegisterObject;
  TTimeseriesDescriptor.RegisterObject;
  TTimeseriesDescriptorLabel.RegisterObject;
  TTimeseriesPoint.RegisterObject;
  TWriteTimeseriesRequestTypecommonLabels.RegisterObject;
  TWriteTimeseriesRequest.RegisterObject;
  TWriteTimeseriesResponse.RegisterObject;
end;


Function TCloudmonitoringAPI.GetMetricDescriptorsInstance : TMetricDescriptorsResource;

begin
  if (FMetricDescriptorsInstance=Nil) then
    FMetricDescriptorsInstance:=CreateMetricDescriptorsResource;
  Result:=FMetricDescriptorsInstance;
end;

Function TCloudmonitoringAPI.CreateMetricDescriptorsResource : TMetricDescriptorsResource;

begin
  Result:=CreateMetricDescriptorsResource(Self);
end;


Function TCloudmonitoringAPI.CreateMetricDescriptorsResource(AOwner : TComponent) : TMetricDescriptorsResource;

begin
  Result:=TMetricDescriptorsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCloudmonitoringAPI.GetTimeseriesInstance : TTimeseriesResource;

begin
  if (FTimeseriesInstance=Nil) then
    FTimeseriesInstance:=CreateTimeseriesResource;
  Result:=FTimeseriesInstance;
end;

Function TCloudmonitoringAPI.CreateTimeseriesResource : TTimeseriesResource;

begin
  Result:=CreateTimeseriesResource(Self);
end;


Function TCloudmonitoringAPI.CreateTimeseriesResource(AOwner : TComponent) : TTimeseriesResource;

begin
  Result:=TTimeseriesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCloudmonitoringAPI.GetTimeseriesDescriptorsInstance : TTimeseriesDescriptorsResource;

begin
  if (FTimeseriesDescriptorsInstance=Nil) then
    FTimeseriesDescriptorsInstance:=CreateTimeseriesDescriptorsResource;
  Result:=FTimeseriesDescriptorsInstance;
end;

Function TCloudmonitoringAPI.CreateTimeseriesDescriptorsResource : TTimeseriesDescriptorsResource;

begin
  Result:=CreateTimeseriesDescriptorsResource(Self);
end;


Function TCloudmonitoringAPI.CreateTimeseriesDescriptorsResource(AOwner : TComponent) : TTimeseriesDescriptorsResource;

begin
  Result:=TTimeseriesDescriptorsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TCloudmonitoringAPI.RegisterAPI;
end.
