unit googlecloudmonitoring;
{
  This is the file COPYING.FPC, it applies to the Free Pascal Run-Time Library 
  (RTL) and packages (packages) distributed by members of the Free Pascal 
  Development Team.
  
  The source code of the Free Pascal Runtime Libraries and packages are 
  distributed under the Library GNU General Public License 
  (see the file COPYING) with the following modification:
  
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,
  and to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the library, but you are
  not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.
  
  If you didn't receive a copy of the file COPYING, contact:
        Free Software Foundation
        675 Mass Ave
        Cambridge, MA  02139
        USA
  
}
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  //
  TDeleteMetricDescriptorResponse = class;
  TDeleteMetricDescriptorResponseArray = Array of TDeleteMetricDescriptorResponse;
  TListMetricDescriptorsRequest = class;
  TListMetricDescriptorsRequestArray = Array of TListMetricDescriptorsRequest;
  TListMetricDescriptorsResponse = class;
  TListMetricDescriptorsResponseArray = Array of TListMetricDescriptorsResponse;
  TListMetricDescriptorsResponsemetrics = class;
  TListMetricDescriptorsResponsemetricsArray = Array of TListMetricDescriptorsResponsemetrics;
  TListTimeseriesDescriptorsRequest = class;
  TListTimeseriesDescriptorsRequestArray = Array of TListTimeseriesDescriptorsRequest;
  TListTimeseriesDescriptorsResponse = class;
  TListTimeseriesDescriptorsResponseArray = Array of TListTimeseriesDescriptorsResponse;
  TListTimeseriesDescriptorsResponsetimeseries = class;
  TListTimeseriesDescriptorsResponsetimeseriesArray = Array of TListTimeseriesDescriptorsResponsetimeseries;
  TListTimeseriesRequest = class;
  TListTimeseriesRequestArray = Array of TListTimeseriesRequest;
  TListTimeseriesResponse = class;
  TListTimeseriesResponseArray = Array of TListTimeseriesResponse;
  TListTimeseriesResponsetimeseries = class;
  TListTimeseriesResponsetimeseriesArray = Array of TListTimeseriesResponsetimeseries;
  TMetricDescriptor = class;
  TMetricDescriptorArray = Array of TMetricDescriptor;
  TMetricDescriptorlabels = class;
  TMetricDescriptorlabelsArray = Array of TMetricDescriptorlabels;
  TMetricDescriptorLabelDescriptor = class;
  TMetricDescriptorLabelDescriptorArray = Array of TMetricDescriptorLabelDescriptor;
  TMetricDescriptorTypeDescriptor = class;
  TMetricDescriptorTypeDescriptorArray = Array of TMetricDescriptorTypeDescriptor;
  TPoint = class;
  TPointArray = Array of TPoint;
  TPointDistribution = class;
  TPointDistributionArray = Array of TPointDistribution;
  TPointDistributionbuckets = class;
  TPointDistributionbucketsArray = Array of TPointDistributionbuckets;
  TPointDistributionBucket = class;
  TPointDistributionBucketArray = Array of TPointDistributionBucket;
  TPointDistributionOverflowBucket = class;
  TPointDistributionOverflowBucketArray = Array of TPointDistributionOverflowBucket;
  TPointDistributionUnderflowBucket = class;
  TPointDistributionUnderflowBucketArray = Array of TPointDistributionUnderflowBucket;
  TTimeseries = class;
  TTimeseriesArray = Array of TTimeseries;
  TTimeseriespoints = class;
  TTimeseriespointsArray = Array of TTimeseriespoints;
  TTimeseriesDescriptor = class;
  TTimeseriesDescriptorArray = Array of TTimeseriesDescriptor;
  TTimeseriesDescriptorlabels = class;
  TTimeseriesDescriptorlabelsArray = Array of TTimeseriesDescriptorlabels;
  TTimeseriesDescriptorLabel = class;
  TTimeseriesDescriptorLabelArray = Array of TTimeseriesDescriptorLabel;
  TTimeseriesPoint = class;
  TTimeseriesPointArray = Array of TTimeseriesPoint;
  TWriteTimeseriesRequest = class;
  TWriteTimeseriesRequestArray = Array of TWriteTimeseriesRequest;
  TWriteTimeseriesRequestcommonLabels = class;
  TWriteTimeseriesRequestcommonLabelsArray = Array of TWriteTimeseriesRequestcommonLabels;
  TWriteTimeseriesRequesttimeseries = class;
  TWriteTimeseriesRequesttimeseriesArray = Array of TWriteTimeseriesRequesttimeseries;
  TWriteTimeseriesResponse = class;
  TWriteTimeseriesResponseArray = Array of TWriteTimeseriesResponse;
  
  { --------------------------------------------------------------------
    TDeleteMetricDescriptorResponse
    --------------------------------------------------------------------}
  
  TDeleteMetricDescriptorResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
  end;
  TDeleteMetricDescriptorResponseClass = Class of TDeleteMetricDescriptorResponse;
  
  { --------------------------------------------------------------------
    TListMetricDescriptorsRequest
    --------------------------------------------------------------------}
  
  TListMetricDescriptorsRequest = Class(TGoogleBaseObject)
  Private
    Fkind : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
  end;
  TListMetricDescriptorsRequestClass = Class of TListMetricDescriptorsRequest;
  
  { --------------------------------------------------------------------
    TListMetricDescriptorsResponse
    --------------------------------------------------------------------}
  
  TListMetricDescriptorsResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fmetrics : TListMetricDescriptorsResponsemetrics;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TListMetricDescriptorsResponsemetrics); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property metrics : TListMetricDescriptorsResponsemetrics Index 8 Read Fmetrics Write Setmetrics;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TListMetricDescriptorsResponseClass = Class of TListMetricDescriptorsResponse;
  
  { --------------------------------------------------------------------
    TListMetricDescriptorsResponsemetrics
    --------------------------------------------------------------------}
  
  TListMetricDescriptorsResponsemetrics = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListMetricDescriptorsResponsemetricsClass = Class of TListMetricDescriptorsResponsemetrics;
  
  { --------------------------------------------------------------------
    TListTimeseriesDescriptorsRequest
    --------------------------------------------------------------------}
  
  TListTimeseriesDescriptorsRequest = Class(TGoogleBaseObject)
  Private
    Fkind : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
  end;
  TListTimeseriesDescriptorsRequestClass = Class of TListTimeseriesDescriptorsRequest;
  
  { --------------------------------------------------------------------
    TListTimeseriesDescriptorsResponse
    --------------------------------------------------------------------}
  
  TListTimeseriesDescriptorsResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Foldest : TDatetime;
    Ftimeseries : TListTimeseriesDescriptorsResponsetimeseries;
    Fyoungest : TDatetime;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setoldest(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settimeseries(AIndex : Integer; AValue : TListTimeseriesDescriptorsResponsetimeseries); virtual;
    Procedure Setyoungest(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property oldest : TDatetime Index 16 Read Foldest Write Setoldest;
    Property timeseries : TListTimeseriesDescriptorsResponsetimeseries Index 24 Read Ftimeseries Write Settimeseries;
    Property youngest : TDatetime Index 32 Read Fyoungest Write Setyoungest;
  end;
  TListTimeseriesDescriptorsResponseClass = Class of TListTimeseriesDescriptorsResponse;
  
  { --------------------------------------------------------------------
    TListTimeseriesDescriptorsResponsetimeseries
    --------------------------------------------------------------------}
  
  TListTimeseriesDescriptorsResponsetimeseries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListTimeseriesDescriptorsResponsetimeseriesClass = Class of TListTimeseriesDescriptorsResponsetimeseries;
  
  { --------------------------------------------------------------------
    TListTimeseriesRequest
    --------------------------------------------------------------------}
  
  TListTimeseriesRequest = Class(TGoogleBaseObject)
  Private
    Fkind : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
  end;
  TListTimeseriesRequestClass = Class of TListTimeseriesRequest;
  
  { --------------------------------------------------------------------
    TListTimeseriesResponse
    --------------------------------------------------------------------}
  
  TListTimeseriesResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FnextPageToken : string;
    Foldest : TDatetime;
    Ftimeseries : TListTimeseriesResponsetimeseries;
    Fyoungest : TDatetime;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setoldest(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settimeseries(AIndex : Integer; AValue : TListTimeseriesResponsetimeseries); virtual;
    Procedure Setyoungest(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property oldest : TDatetime Index 16 Read Foldest Write Setoldest;
    Property timeseries : TListTimeseriesResponsetimeseries Index 24 Read Ftimeseries Write Settimeseries;
    Property youngest : TDatetime Index 32 Read Fyoungest Write Setyoungest;
  end;
  TListTimeseriesResponseClass = Class of TListTimeseriesResponse;
  
  { --------------------------------------------------------------------
    TListTimeseriesResponsetimeseries
    --------------------------------------------------------------------}
  
  TListTimeseriesResponsetimeseries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListTimeseriesResponsetimeseriesClass = Class of TListTimeseriesResponsetimeseries;
  
  { --------------------------------------------------------------------
    TMetricDescriptor
    --------------------------------------------------------------------}
  
  TMetricDescriptor = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Flabels : TMetricDescriptorlabels;
    Fname : string;
    Fproject : string;
    FtypeDescriptor : TMetricDescriptorTypeDescriptor;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TMetricDescriptorlabels); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setproject(AIndex : Integer; AValue : string); virtual;
    Procedure SettypeDescriptor(AIndex : Integer; AValue : TMetricDescriptorTypeDescriptor); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property labels : TMetricDescriptorlabels Index 8 Read Flabels Write Setlabels;
    Property name : string Index 16 Read Fname Write Setname;
    Property project : string Index 24 Read Fproject Write Setproject;
    Property typeDescriptor : TMetricDescriptorTypeDescriptor Index 32 Read FtypeDescriptor Write SettypeDescriptor;
  end;
  TMetricDescriptorClass = Class of TMetricDescriptor;
  
  { --------------------------------------------------------------------
    TMetricDescriptorlabels
    --------------------------------------------------------------------}
  
  TMetricDescriptorlabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMetricDescriptorlabelsClass = Class of TMetricDescriptorlabels;
  
  { --------------------------------------------------------------------
    TMetricDescriptorLabelDescriptor
    --------------------------------------------------------------------}
  
  TMetricDescriptorLabelDescriptor = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fkey : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property key : string Index 8 Read Fkey Write Setkey;
  end;
  TMetricDescriptorLabelDescriptorClass = Class of TMetricDescriptorLabelDescriptor;
  
  { --------------------------------------------------------------------
    TMetricDescriptorTypeDescriptor
    --------------------------------------------------------------------}
  
  TMetricDescriptorTypeDescriptor = Class(TGoogleBaseObject)
  Private
    FmetricType : string;
    FvalueType : string;
  Protected
    //Property setters
    Procedure SetmetricType(AIndex : Integer; AValue : string); virtual;
    Procedure SetvalueType(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property metricType : string Index 0 Read FmetricType Write SetmetricType;
    Property valueType : string Index 8 Read FvalueType Write SetvalueType;
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
    Fint64Value : string;
    Fstart : TDatetime;
    FstringValue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetboolValue(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdistributionValue(AIndex : Integer; AValue : TPointDistribution); virtual;
    Procedure SetdoubleValue(AIndex : Integer; AValue : double); virtual;
    Procedure Set_end(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setint64Value(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetstringValue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property boolValue : boolean Index 0 Read FboolValue Write SetboolValue;
    Property distributionValue : TPointDistribution Index 8 Read FdistributionValue Write SetdistributionValue;
    Property doubleValue : double Index 16 Read FdoubleValue Write SetdoubleValue;
    Property _end : TDatetime Index 24 Read F_end Write Set_end;
    Property int64Value : string Index 32 Read Fint64Value Write Setint64Value;
    Property start : TDatetime Index 40 Read Fstart Write Setstart;
    Property stringValue : string Index 48 Read FstringValue Write SetstringValue;
  end;
  TPointClass = Class of TPoint;
  
  { --------------------------------------------------------------------
    TPointDistribution
    --------------------------------------------------------------------}
  
  TPointDistribution = Class(TGoogleBaseObject)
  Private
    Fbuckets : TPointDistributionbuckets;
    FoverflowBucket : TPointDistributionOverflowBucket;
    FunderflowBucket : TPointDistributionUnderflowBucket;
  Protected
    //Property setters
    Procedure Setbuckets(AIndex : Integer; AValue : TPointDistributionbuckets); virtual;
    Procedure SetoverflowBucket(AIndex : Integer; AValue : TPointDistributionOverflowBucket); virtual;
    Procedure SetunderflowBucket(AIndex : Integer; AValue : TPointDistributionUnderflowBucket); virtual;
  Public
  Published
    Property buckets : TPointDistributionbuckets Index 0 Read Fbuckets Write Setbuckets;
    Property overflowBucket : TPointDistributionOverflowBucket Index 8 Read FoverflowBucket Write SetoverflowBucket;
    Property underflowBucket : TPointDistributionUnderflowBucket Index 16 Read FunderflowBucket Write SetunderflowBucket;
  end;
  TPointDistributionClass = Class of TPointDistribution;
  
  { --------------------------------------------------------------------
    TPointDistributionbuckets
    --------------------------------------------------------------------}
  
  TPointDistributionbuckets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPointDistributionbucketsClass = Class of TPointDistributionbuckets;
  
  { --------------------------------------------------------------------
    TPointDistributionBucket
    --------------------------------------------------------------------}
  
  TPointDistributionBucket = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    FlowerBound : double;
    FupperBound : double;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure SetlowerBound(AIndex : Integer; AValue : double); virtual;
    Procedure SetupperBound(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property lowerBound : double Index 8 Read FlowerBound Write SetlowerBound;
    Property upperBound : double Index 16 Read FupperBound Write SetupperBound;
  end;
  TPointDistributionBucketClass = Class of TPointDistributionBucket;
  
  { --------------------------------------------------------------------
    TPointDistributionOverflowBucket
    --------------------------------------------------------------------}
  
  TPointDistributionOverflowBucket = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    FlowerBound : double;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure SetlowerBound(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property lowerBound : double Index 8 Read FlowerBound Write SetlowerBound;
  end;
  TPointDistributionOverflowBucketClass = Class of TPointDistributionOverflowBucket;
  
  { --------------------------------------------------------------------
    TPointDistributionUnderflowBucket
    --------------------------------------------------------------------}
  
  TPointDistributionUnderflowBucket = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    FupperBound : double;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure SetupperBound(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property upperBound : double Index 8 Read FupperBound Write SetupperBound;
  end;
  TPointDistributionUnderflowBucketClass = Class of TPointDistributionUnderflowBucket;
  
  { --------------------------------------------------------------------
    TTimeseries
    --------------------------------------------------------------------}
  
  TTimeseries = Class(TGoogleBaseObject)
  Private
    Fpoints : TTimeseriespoints;
    FtimeseriesDesc : TTimeseriesDescriptor;
  Protected
    //Property setters
    Procedure Setpoints(AIndex : Integer; AValue : TTimeseriespoints); virtual;
    Procedure SettimeseriesDesc(AIndex : Integer; AValue : TTimeseriesDescriptor); virtual;
  Public
  Published
    Property points : TTimeseriespoints Index 0 Read Fpoints Write Setpoints;
    Property timeseriesDesc : TTimeseriesDescriptor Index 8 Read FtimeseriesDesc Write SettimeseriesDesc;
  end;
  TTimeseriesClass = Class of TTimeseries;
  
  { --------------------------------------------------------------------
    TTimeseriespoints
    --------------------------------------------------------------------}
  
  TTimeseriespoints = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTimeseriespointsClass = Class of TTimeseriespoints;
  
  { --------------------------------------------------------------------
    TTimeseriesDescriptor
    --------------------------------------------------------------------}
  
  TTimeseriesDescriptor = Class(TGoogleBaseObject)
  Private
    Flabels : TTimeseriesDescriptorlabels;
    Fmetric : string;
    Fproject : string;
  Protected
    //Property setters
    Procedure Setlabels(AIndex : Integer; AValue : TTimeseriesDescriptorlabels); virtual;
    Procedure Setmetric(AIndex : Integer; AValue : string); virtual;
    Procedure Setproject(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property labels : TTimeseriesDescriptorlabels Index 0 Read Flabels Write Setlabels;
    Property metric : string Index 8 Read Fmetric Write Setmetric;
    Property project : string Index 16 Read Fproject Write Setproject;
  end;
  TTimeseriesDescriptorClass = Class of TTimeseriesDescriptor;
  
  { --------------------------------------------------------------------
    TTimeseriesDescriptorlabels
    --------------------------------------------------------------------}
  
  TTimeseriesDescriptorlabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTimeseriesDescriptorlabelsClass = Class of TTimeseriesDescriptorlabels;
  
  { --------------------------------------------------------------------
    TTimeseriesDescriptorLabel
    --------------------------------------------------------------------}
  
  TTimeseriesDescriptorLabel = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
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
    TWriteTimeseriesRequest
    --------------------------------------------------------------------}
  
  TWriteTimeseriesRequest = Class(TGoogleBaseObject)
  Private
    FcommonLabels : TWriteTimeseriesRequestcommonLabels;
    Ftimeseries : TWriteTimeseriesRequesttimeseries;
  Protected
    //Property setters
    Procedure SetcommonLabels(AIndex : Integer; AValue : TWriteTimeseriesRequestcommonLabels); virtual;
    Procedure Settimeseries(AIndex : Integer; AValue : TWriteTimeseriesRequesttimeseries); virtual;
  Public
  Published
    Property commonLabels : TWriteTimeseriesRequestcommonLabels Index 0 Read FcommonLabels Write SetcommonLabels;
    Property timeseries : TWriteTimeseriesRequesttimeseries Index 8 Read Ftimeseries Write Settimeseries;
  end;
  TWriteTimeseriesRequestClass = Class of TWriteTimeseriesRequest;
  
  { --------------------------------------------------------------------
    TWriteTimeseriesRequestcommonLabels
    --------------------------------------------------------------------}
  
  TWriteTimeseriesRequestcommonLabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWriteTimeseriesRequestcommonLabelsClass = Class of TWriteTimeseriesRequestcommonLabels;
  
  { --------------------------------------------------------------------
    TWriteTimeseriesRequesttimeseries
    --------------------------------------------------------------------}
  
  TWriteTimeseriesRequesttimeseries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWriteTimeseriesRequesttimeseriesClass = Class of TWriteTimeseriesRequesttimeseries;
  
  { --------------------------------------------------------------------
    TWriteTimeseriesResponse
    --------------------------------------------------------------------}
  
  TWriteTimeseriesResponse = Class(TGoogleBaseObject)
  Private
    Fkind : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
  end;
  TWriteTimeseriesResponseClass = Class of TWriteTimeseriesResponse;
  
  { --------------------------------------------------------------------
    TMetricDescriptorsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMetricDescriptorsResource, method List
  
  TMetricDescriptorsListOptions = Record
    count : integer;
    pageToken : string;
    query : string;
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
    aggregator : string;
    count : integer;
    labels : string;
    oldest : string;
    pageToken : string;
    timespan : string;
    window : string;
    youngest : string;
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
    aggregator : string;
    count : integer;
    labels : string;
    oldest : string;
    pageToken : string;
    timespan : string;
    window : string;
    youngest : string;
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


Procedure TDeleteMetricDescriptorResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListMetricDescriptorsRequest
  --------------------------------------------------------------------}


Procedure TListMetricDescriptorsRequest.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListMetricDescriptorsResponse
  --------------------------------------------------------------------}


Procedure TListMetricDescriptorsResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMetricDescriptorsResponse.Setmetrics(AIndex : Integer; AValue : TListMetricDescriptorsResponsemetrics); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMetricDescriptorsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListMetricDescriptorsResponsemetrics
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListTimeseriesDescriptorsRequest
  --------------------------------------------------------------------}


Procedure TListTimeseriesDescriptorsRequest.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTimeseriesDescriptorsResponse
  --------------------------------------------------------------------}


Procedure TListTimeseriesDescriptorsResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesDescriptorsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

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



Procedure TListTimeseriesDescriptorsResponse.Settimeseries(AIndex : Integer; AValue : TListTimeseriesDescriptorsResponsetimeseries); 

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
  TListTimeseriesDescriptorsResponsetimeseries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListTimeseriesRequest
  --------------------------------------------------------------------}


Procedure TListTimeseriesRequest.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTimeseriesResponse
  --------------------------------------------------------------------}


Procedure TListTimeseriesResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTimeseriesResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

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



Procedure TListTimeseriesResponse.Settimeseries(AIndex : Integer; AValue : TListTimeseriesResponsetimeseries); 

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
  TListTimeseriesResponsetimeseries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMetricDescriptor
  --------------------------------------------------------------------}


Procedure TMetricDescriptor.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Setlabels(AIndex : Integer; AValue : TMetricDescriptorlabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptor.Setproject(AIndex : Integer; AValue : string); 

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
  TMetricDescriptorlabels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMetricDescriptorLabelDescriptor
  --------------------------------------------------------------------}


Procedure TMetricDescriptorLabelDescriptor.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptorLabelDescriptor.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetricDescriptorTypeDescriptor
  --------------------------------------------------------------------}


Procedure TMetricDescriptorTypeDescriptor.SetmetricType(AIndex : Integer; AValue : string); 

begin
  If (FmetricType=AValue) then exit;
  FmetricType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricDescriptorTypeDescriptor.SetvalueType(AIndex : Integer; AValue : string); 

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



Procedure TPoint.Setint64Value(AIndex : Integer; AValue : string); 

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



Procedure TPoint.SetstringValue(AIndex : Integer; AValue : string); 

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


Procedure TPointDistribution.Setbuckets(AIndex : Integer; AValue : TPointDistributionbuckets); 

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
  TPointDistributionbuckets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPointDistributionBucket
  --------------------------------------------------------------------}


Procedure TPointDistributionBucket.Setcount(AIndex : Integer; AValue : string); 

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


Procedure TPointDistributionOverflowBucket.Setcount(AIndex : Integer; AValue : string); 

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


Procedure TPointDistributionUnderflowBucket.Setcount(AIndex : Integer; AValue : string); 

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


Procedure TTimeseries.Setpoints(AIndex : Integer; AValue : TTimeseriespoints); 

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
  TTimeseriespoints
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTimeseriesDescriptor
  --------------------------------------------------------------------}


Procedure TTimeseriesDescriptor.Setlabels(AIndex : Integer; AValue : TTimeseriesDescriptorlabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeseriesDescriptor.Setmetric(AIndex : Integer; AValue : string); 

begin
  If (Fmetric=AValue) then exit;
  Fmetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeseriesDescriptor.Setproject(AIndex : Integer; AValue : string); 

begin
  If (Fproject=AValue) then exit;
  Fproject:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeseriesDescriptorlabels
  --------------------------------------------------------------------}


Class Function TTimeseriesDescriptorlabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTimeseriesDescriptorLabel
  --------------------------------------------------------------------}


Procedure TTimeseriesDescriptorLabel.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeseriesDescriptorLabel.Setvalue(AIndex : Integer; AValue : string); 

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
  TWriteTimeseriesRequest
  --------------------------------------------------------------------}


Procedure TWriteTimeseriesRequest.SetcommonLabels(AIndex : Integer; AValue : TWriteTimeseriesRequestcommonLabels); 

begin
  If (FcommonLabels=AValue) then exit;
  FcommonLabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteTimeseriesRequest.Settimeseries(AIndex : Integer; AValue : TWriteTimeseriesRequesttimeseries); 

begin
  If (Ftimeseries=AValue) then exit;
  Ftimeseries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWriteTimeseriesRequestcommonLabels
  --------------------------------------------------------------------}


Class Function TWriteTimeseriesRequestcommonLabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWriteTimeseriesRequesttimeseries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWriteTimeseriesResponse
  --------------------------------------------------------------------}


Procedure TWriteTimeseriesResponse.Setkind(AIndex : Integer; AValue : string); 

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
  Result:='20150305';
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
  TListMetricDescriptorsResponsemetrics.RegisterObject;
  TListTimeseriesDescriptorsRequest.RegisterObject;
  TListTimeseriesDescriptorsResponse.RegisterObject;
  TListTimeseriesDescriptorsResponsetimeseries.RegisterObject;
  TListTimeseriesRequest.RegisterObject;
  TListTimeseriesResponse.RegisterObject;
  TListTimeseriesResponsetimeseries.RegisterObject;
  TMetricDescriptor.RegisterObject;
  TMetricDescriptorlabels.RegisterObject;
  TMetricDescriptorLabelDescriptor.RegisterObject;
  TMetricDescriptorTypeDescriptor.RegisterObject;
  TPoint.RegisterObject;
  TPointDistribution.RegisterObject;
  TPointDistributionbuckets.RegisterObject;
  TPointDistributionBucket.RegisterObject;
  TPointDistributionOverflowBucket.RegisterObject;
  TPointDistributionUnderflowBucket.RegisterObject;
  TTimeseries.RegisterObject;
  TTimeseriespoints.RegisterObject;
  TTimeseriesDescriptor.RegisterObject;
  TTimeseriesDescriptorlabels.RegisterObject;
  TTimeseriesDescriptorLabel.RegisterObject;
  TTimeseriesPoint.RegisterObject;
  TWriteTimeseriesRequest.RegisterObject;
  TWriteTimeseriesRequestcommonLabels.RegisterObject;
  TWriteTimeseriesRequesttimeseries.RegisterObject;
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
