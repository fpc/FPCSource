unit googlefitness;
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
  TApplication = class;
  TApplicationArray = Array of TApplication;
  TDataPoint = class;
  TDataPointArray = Array of TDataPoint;
  TDataPointvalue = class;
  TDataPointvalueArray = Array of TDataPointvalue;
  TDataSource = class;
  TDataSourceArray = Array of TDataSource;
  TDataType = class;
  TDataTypeArray = Array of TDataType;
  TDataTypefield = class;
  TDataTypefieldArray = Array of TDataTypefield;
  TDataset = class;
  TDatasetArray = Array of TDataset;
  TDatasetpoint = class;
  TDatasetpointArray = Array of TDatasetpoint;
  TDevice = class;
  TDeviceArray = Array of TDevice;
  TListDataSourcesResponse = class;
  TListDataSourcesResponseArray = Array of TListDataSourcesResponse;
  TListDataSourcesResponsedataSource = class;
  TListDataSourcesResponsedataSourceArray = Array of TListDataSourcesResponsedataSource;
  TListSessionsResponse = class;
  TListSessionsResponseArray = Array of TListSessionsResponse;
  TListSessionsResponsedeletedSession = class;
  TListSessionsResponsedeletedSessionArray = Array of TListSessionsResponsedeletedSession;
  TListSessionsResponsesession = class;
  TListSessionsResponsesessionArray = Array of TListSessionsResponsesession;
  TSession = class;
  TSessionArray = Array of TSession;
  TValue = class;
  TValueArray = Array of TValue;
  
  { --------------------------------------------------------------------
    TApplication
    --------------------------------------------------------------------}
  
  TApplication = Class(TGoogleBaseObject)
  Private
    FdetailsUrl : string;
    Fname : string;
    FpackageName : string;
    Fversion : string;
  Protected
    //Property setters
    Procedure SetdetailsUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpackageName(AIndex : Integer; AValue : string); virtual;
    Procedure Setversion(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property detailsUrl : string Index 0 Read FdetailsUrl Write SetdetailsUrl;
    Property name : string Index 8 Read Fname Write Setname;
    Property packageName : string Index 16 Read FpackageName Write SetpackageName;
    Property version : string Index 24 Read Fversion Write Setversion;
  end;
  TApplicationClass = Class of TApplication;
  
  { --------------------------------------------------------------------
    TDataPoint
    --------------------------------------------------------------------}
  
  TDataPoint = Class(TGoogleBaseObject)
  Private
    FcomputationTimeMillis : string;
    FdataTypeName : string;
    FendTimeNanos : string;
    FmodifiedTimeMillis : string;
    ForiginDataSourceId : string;
    FrawTimestampNanos : string;
    FstartTimeNanos : string;
    Fvalue : TDataPointvalue;
  Protected
    //Property setters
    Procedure SetcomputationTimeMillis(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataTypeName(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTimeNanos(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodifiedTimeMillis(AIndex : Integer; AValue : string); virtual;
    Procedure SetoriginDataSourceId(AIndex : Integer; AValue : string); virtual;
    Procedure SetrawTimestampNanos(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTimeNanos(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : TDataPointvalue); virtual;
  Public
  Published
    Property computationTimeMillis : string Index 0 Read FcomputationTimeMillis Write SetcomputationTimeMillis;
    Property dataTypeName : string Index 8 Read FdataTypeName Write SetdataTypeName;
    Property endTimeNanos : string Index 16 Read FendTimeNanos Write SetendTimeNanos;
    Property modifiedTimeMillis : string Index 24 Read FmodifiedTimeMillis Write SetmodifiedTimeMillis;
    Property originDataSourceId : string Index 32 Read ForiginDataSourceId Write SetoriginDataSourceId;
    Property rawTimestampNanos : string Index 40 Read FrawTimestampNanos Write SetrawTimestampNanos;
    Property startTimeNanos : string Index 48 Read FstartTimeNanos Write SetstartTimeNanos;
    Property value : TDataPointvalue Index 56 Read Fvalue Write Setvalue;
  end;
  TDataPointClass = Class of TDataPoint;
  
  { --------------------------------------------------------------------
    TDataPointvalue
    --------------------------------------------------------------------}
  
  TDataPointvalue = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDataPointvalueClass = Class of TDataPointvalue;
  
  { --------------------------------------------------------------------
    TDataSource
    --------------------------------------------------------------------}
  
  TDataSource = Class(TGoogleBaseObject)
  Private
    Fapplication : TApplication;
    FdataStreamId : string;
    FdataStreamName : string;
    FdataType : TDataType;
    Fdevice : TDevice;
    Fname : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setapplication(AIndex : Integer; AValue : TApplication); virtual;
    Procedure SetdataStreamId(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataStreamName(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataType(AIndex : Integer; AValue : TDataType); virtual;
    Procedure Setdevice(AIndex : Integer; AValue : TDevice); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property application : TApplication Index 0 Read Fapplication Write Setapplication;
    Property dataStreamId : string Index 8 Read FdataStreamId Write SetdataStreamId;
    Property dataStreamName : string Index 16 Read FdataStreamName Write SetdataStreamName;
    Property dataType : TDataType Index 24 Read FdataType Write SetdataType;
    Property device : TDevice Index 32 Read Fdevice Write Setdevice;
    Property name : string Index 40 Read Fname Write Setname;
    Property _type : string Index 48 Read F_type Write Set_type;
  end;
  TDataSourceClass = Class of TDataSource;
  
  { --------------------------------------------------------------------
    TDataType
    --------------------------------------------------------------------}
  
  TDataType = Class(TGoogleBaseObject)
  Private
    Ffield : TDataTypefield;
    Fname : string;
  Protected
    //Property setters
    Procedure Setfield(AIndex : Integer; AValue : TDataTypefield); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property field : TDataTypefield Index 0 Read Ffield Write Setfield;
    Property name : string Index 8 Read Fname Write Setname;
  end;
  TDataTypeClass = Class of TDataType;
  
  { --------------------------------------------------------------------
    TDataTypefield
    --------------------------------------------------------------------}
  
  TDataTypefield = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDataTypefieldClass = Class of TDataTypefield;
  
  { --------------------------------------------------------------------
    TDataset
    --------------------------------------------------------------------}
  
  TDataset = Class(TGoogleBaseObject)
  Private
    FdataSourceId : string;
    FmaxEndTimeNs : string;
    FminStartTimeNs : string;
    FnextPageToken : string;
    Fpoint : TDatasetpoint;
  Protected
    //Property setters
    Procedure SetdataSourceId(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxEndTimeNs(AIndex : Integer; AValue : string); virtual;
    Procedure SetminStartTimeNs(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setpoint(AIndex : Integer; AValue : TDatasetpoint); virtual;
  Public
  Published
    Property dataSourceId : string Index 0 Read FdataSourceId Write SetdataSourceId;
    Property maxEndTimeNs : string Index 8 Read FmaxEndTimeNs Write SetmaxEndTimeNs;
    Property minStartTimeNs : string Index 16 Read FminStartTimeNs Write SetminStartTimeNs;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property point : TDatasetpoint Index 32 Read Fpoint Write Setpoint;
  end;
  TDatasetClass = Class of TDataset;
  
  { --------------------------------------------------------------------
    TDatasetpoint
    --------------------------------------------------------------------}
  
  TDatasetpoint = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatasetpointClass = Class of TDatasetpoint;
  
  { --------------------------------------------------------------------
    TDevice
    --------------------------------------------------------------------}
  
  TDevice = Class(TGoogleBaseObject)
  Private
    Fmanufacturer : string;
    Fmodel : string;
    F_type : string;
    Fuid : string;
    Fversion : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmanufacturer(AIndex : Integer; AValue : string); virtual;
    Procedure Setmodel(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setuid(AIndex : Integer; AValue : string); virtual;
    Procedure Setversion(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property manufacturer : string Index 0 Read Fmanufacturer Write Setmanufacturer;
    Property model : string Index 8 Read Fmodel Write Setmodel;
    Property _type : string Index 16 Read F_type Write Set_type;
    Property uid : string Index 24 Read Fuid Write Setuid;
    Property version : string Index 32 Read Fversion Write Setversion;
  end;
  TDeviceClass = Class of TDevice;
  
  { --------------------------------------------------------------------
    TListDataSourcesResponse
    --------------------------------------------------------------------}
  
  TListDataSourcesResponse = Class(TGoogleBaseObject)
  Private
    FdataSource : TListDataSourcesResponsedataSource;
  Protected
    //Property setters
    Procedure SetdataSource(AIndex : Integer; AValue : TListDataSourcesResponsedataSource); virtual;
  Public
  Published
    Property dataSource : TListDataSourcesResponsedataSource Index 0 Read FdataSource Write SetdataSource;
  end;
  TListDataSourcesResponseClass = Class of TListDataSourcesResponse;
  
  { --------------------------------------------------------------------
    TListDataSourcesResponsedataSource
    --------------------------------------------------------------------}
  
  TListDataSourcesResponsedataSource = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListDataSourcesResponsedataSourceClass = Class of TListDataSourcesResponsedataSource;
  
  { --------------------------------------------------------------------
    TListSessionsResponse
    --------------------------------------------------------------------}
  
  TListSessionsResponse = Class(TGoogleBaseObject)
  Private
    FdeletedSession : TListSessionsResponsedeletedSession;
    FnextPageToken : string;
    Fsession : TListSessionsResponsesession;
  Protected
    //Property setters
    Procedure SetdeletedSession(AIndex : Integer; AValue : TListSessionsResponsedeletedSession); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setsession(AIndex : Integer; AValue : TListSessionsResponsesession); virtual;
  Public
  Published
    Property deletedSession : TListSessionsResponsedeletedSession Index 0 Read FdeletedSession Write SetdeletedSession;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property session : TListSessionsResponsesession Index 16 Read Fsession Write Setsession;
  end;
  TListSessionsResponseClass = Class of TListSessionsResponse;
  
  { --------------------------------------------------------------------
    TListSessionsResponsedeletedSession
    --------------------------------------------------------------------}
  
  TListSessionsResponsedeletedSession = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListSessionsResponsedeletedSessionClass = Class of TListSessionsResponsedeletedSession;
  
  { --------------------------------------------------------------------
    TListSessionsResponsesession
    --------------------------------------------------------------------}
  
  TListSessionsResponsesession = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListSessionsResponsesessionClass = Class of TListSessionsResponsesession;
  
  { --------------------------------------------------------------------
    TSession
    --------------------------------------------------------------------}
  
  TSession = Class(TGoogleBaseObject)
  Private
    FactiveTimeMillis : string;
    FactivityType : integer;
    Fapplication : TApplication;
    Fdescription : string;
    FendTimeMillis : string;
    Fid : string;
    FmodifiedTimeMillis : string;
    Fname : string;
    FstartTimeMillis : string;
  Protected
    //Property setters
    Procedure SetactiveTimeMillis(AIndex : Integer; AValue : string); virtual;
    Procedure SetactivityType(AIndex : Integer; AValue : integer); virtual;
    Procedure Setapplication(AIndex : Integer; AValue : TApplication); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTimeMillis(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodifiedTimeMillis(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTimeMillis(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property activeTimeMillis : string Index 0 Read FactiveTimeMillis Write SetactiveTimeMillis;
    Property activityType : integer Index 8 Read FactivityType Write SetactivityType;
    Property application : TApplication Index 16 Read Fapplication Write Setapplication;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property endTimeMillis : string Index 32 Read FendTimeMillis Write SetendTimeMillis;
    Property id : string Index 40 Read Fid Write Setid;
    Property modifiedTimeMillis : string Index 48 Read FmodifiedTimeMillis Write SetmodifiedTimeMillis;
    Property name : string Index 56 Read Fname Write Setname;
    Property startTimeMillis : string Index 64 Read FstartTimeMillis Write SetstartTimeMillis;
  end;
  TSessionClass = Class of TSession;
  
  { --------------------------------------------------------------------
    TValue
    --------------------------------------------------------------------}
  
  TValue = Class(TGoogleBaseObject)
  Private
    FfpVal : double;
    FintVal : integer;
  Protected
    //Property setters
    Procedure SetfpVal(AIndex : Integer; AValue : double); virtual;
    Procedure SetintVal(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property fpVal : double Index 0 Read FfpVal Write SetfpVal;
    Property intVal : integer Index 8 Read FintVal Write SetintVal;
  end;
  TValueClass = Class of TValue;
  
  { --------------------------------------------------------------------
    TUsersResource
    --------------------------------------------------------------------}
  
  TUsersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
  end;
  
  
  { --------------------------------------------------------------------
    TFitnessAPI
    --------------------------------------------------------------------}
  
  TFitnessAPI = Class(TGoogleAPI)
  Private
    FUsersInstance : TUsersResource;
    Function GetUsersInstance : TUsersResource;virtual;
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
    Function CreateUsersResource(AOwner : TComponent) : TUsersResource;virtual;overload;
    Function CreateUsersResource : TUsersResource;virtual;overload;
    //Add default on-demand instances for resources
    Property UsersResource : TUsersResource Read GetUsersInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TApplication
  --------------------------------------------------------------------}


Procedure TApplication.SetdetailsUrl(AIndex : Integer; AValue : string); 

begin
  If (FdetailsUrl=AValue) then exit;
  FdetailsUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetpackageName(AIndex : Integer; AValue : string); 

begin
  If (FpackageName=AValue) then exit;
  FpackageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setversion(AIndex : Integer; AValue : string); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataPoint
  --------------------------------------------------------------------}


Procedure TDataPoint.SetcomputationTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FcomputationTimeMillis=AValue) then exit;
  FcomputationTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetdataTypeName(AIndex : Integer; AValue : string); 

begin
  If (FdataTypeName=AValue) then exit;
  FdataTypeName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetendTimeNanos(AIndex : Integer; AValue : string); 

begin
  If (FendTimeNanos=AValue) then exit;
  FendTimeNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetmodifiedTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FmodifiedTimeMillis=AValue) then exit;
  FmodifiedTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetoriginDataSourceId(AIndex : Integer; AValue : string); 

begin
  If (ForiginDataSourceId=AValue) then exit;
  ForiginDataSourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetrawTimestampNanos(AIndex : Integer; AValue : string); 

begin
  If (FrawTimestampNanos=AValue) then exit;
  FrawTimestampNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.SetstartTimeNanos(AIndex : Integer; AValue : string); 

begin
  If (FstartTimeNanos=AValue) then exit;
  FstartTimeNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataPoint.Setvalue(AIndex : Integer; AValue : TDataPointvalue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataPointvalue
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDataSource
  --------------------------------------------------------------------}


Procedure TDataSource.Setapplication(AIndex : Integer; AValue : TApplication); 

begin
  If (Fapplication=AValue) then exit;
  Fapplication:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.SetdataStreamId(AIndex : Integer; AValue : string); 

begin
  If (FdataStreamId=AValue) then exit;
  FdataStreamId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.SetdataStreamName(AIndex : Integer; AValue : string); 

begin
  If (FdataStreamName=AValue) then exit;
  FdataStreamName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.SetdataType(AIndex : Integer; AValue : TDataType); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.Setdevice(AIndex : Integer; AValue : TDevice); 

begin
  If (Fdevice=AValue) then exit;
  Fdevice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataSource.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDataSource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDataType
  --------------------------------------------------------------------}


Procedure TDataType.Setfield(AIndex : Integer; AValue : TDataTypefield); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataType.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataTypefield
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDataset
  --------------------------------------------------------------------}


Procedure TDataset.SetdataSourceId(AIndex : Integer; AValue : string); 

begin
  If (FdataSourceId=AValue) then exit;
  FdataSourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetmaxEndTimeNs(AIndex : Integer; AValue : string); 

begin
  If (FmaxEndTimeNs=AValue) then exit;
  FmaxEndTimeNs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetminStartTimeNs(AIndex : Integer; AValue : string); 

begin
  If (FminStartTimeNs=AValue) then exit;
  FminStartTimeNs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.Setpoint(AIndex : Integer; AValue : TDatasetpoint); 

begin
  If (Fpoint=AValue) then exit;
  Fpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatasetpoint
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDevice
  --------------------------------------------------------------------}


Procedure TDevice.Setmanufacturer(AIndex : Integer; AValue : string); 

begin
  If (Fmanufacturer=AValue) then exit;
  Fmanufacturer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.Setmodel(AIndex : Integer; AValue : string); 

begin
  If (Fmodel=AValue) then exit;
  Fmodel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.Setuid(AIndex : Integer; AValue : string); 

begin
  If (Fuid=AValue) then exit;
  Fuid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.Setversion(AIndex : Integer; AValue : string); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDevice.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TListDataSourcesResponse
  --------------------------------------------------------------------}


Procedure TListDataSourcesResponse.SetdataSource(AIndex : Integer; AValue : TListDataSourcesResponsedataSource); 

begin
  If (FdataSource=AValue) then exit;
  FdataSource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListDataSourcesResponsedataSource
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListSessionsResponse
  --------------------------------------------------------------------}


Procedure TListSessionsResponse.SetdeletedSession(AIndex : Integer; AValue : TListSessionsResponsedeletedSession); 

begin
  If (FdeletedSession=AValue) then exit;
  FdeletedSession:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListSessionsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListSessionsResponse.Setsession(AIndex : Integer; AValue : TListSessionsResponsesession); 

begin
  If (Fsession=AValue) then exit;
  Fsession:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListSessionsResponsedeletedSession
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListSessionsResponsesession
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSession
  --------------------------------------------------------------------}


Procedure TSession.SetactiveTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FactiveTimeMillis=AValue) then exit;
  FactiveTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.SetactivityType(AIndex : Integer; AValue : integer); 

begin
  If (FactivityType=AValue) then exit;
  FactivityType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.Setapplication(AIndex : Integer; AValue : TApplication); 

begin
  If (Fapplication=AValue) then exit;
  Fapplication:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.SetendTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FendTimeMillis=AValue) then exit;
  FendTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.SetmodifiedTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FmodifiedTimeMillis=AValue) then exit;
  FmodifiedTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSession.SetstartTimeMillis(AIndex : Integer; AValue : string); 

begin
  If (FstartTimeMillis=AValue) then exit;
  FstartTimeMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TValue
  --------------------------------------------------------------------}


Procedure TValue.SetfpVal(AIndex : Integer; AValue : double); 

begin
  If (FfpVal=AValue) then exit;
  FfpVal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetintVal(AIndex : Integer; AValue : integer); 

begin
  If (FintVal=AValue) then exit;
  FintVal:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersResource
  --------------------------------------------------------------------}


Class Function TUsersResource.ResourceName : String;

begin
  Result:='users';
end;

Class Function TUsersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfitnessAPI;
end;



{ --------------------------------------------------------------------
  TFitnessAPI
  --------------------------------------------------------------------}

Class Function TFitnessAPI.APIName : String;

begin
  Result:='fitness';
end;

Class Function TFitnessAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TFitnessAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TFitnessAPI.APIID : String;

begin
  Result:='fitness:v1';
end;

Class Function TFitnessAPI.APITitle : String;

begin
  Result:='Fitness';
end;

Class Function TFitnessAPI.APIDescription : String;

begin
  Result:='Google Fit API';
end;

Class Function TFitnessAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TFitnessAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TFitnessAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TFitnessAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TFitnessAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/fit/rest/';
end;

Class Function TFitnessAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TFitnessAPI.APIbasePath : string;

begin
  Result:='/fitness/v1/users/';
end;

Class Function TFitnessAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/fitness/v1/users/';
end;

Class Function TFitnessAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TFitnessAPI.APIservicePath : string;

begin
  Result:='fitness/v1/users/';
end;

Class Function TFitnessAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TFitnessAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,6);
  Result[0].Name:='https://www.googleapis.com/auth/fitness.activity.read';
  Result[0].Description:='View your activity information in Google Fit';
  Result[1].Name:='https://www.googleapis.com/auth/fitness.activity.write';
  Result[1].Description:='View and store your activity information in Google Fit';
  Result[2].Name:='https://www.googleapis.com/auth/fitness.body.read';
  Result[2].Description:='View body sensor information in Google Fit';
  Result[3].Name:='https://www.googleapis.com/auth/fitness.body.write';
  Result[3].Description:='View and store body sensor data in Google Fit';
  Result[4].Name:='https://www.googleapis.com/auth/fitness.location.read';
  Result[4].Description:='View your stored location data in Google Fit';
  Result[5].Name:='https://www.googleapis.com/auth/fitness.location.write';
  Result[5].Description:='View and store your location data in Google Fit';
  
end;

Class Function TFitnessAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TFitnessAPI.RegisterAPIResources;

begin
  TApplication.RegisterObject;
  TDataPoint.RegisterObject;
  TDataPointvalue.RegisterObject;
  TDataSource.RegisterObject;
  TDataType.RegisterObject;
  TDataTypefield.RegisterObject;
  TDataset.RegisterObject;
  TDatasetpoint.RegisterObject;
  TDevice.RegisterObject;
  TListDataSourcesResponse.RegisterObject;
  TListDataSourcesResponsedataSource.RegisterObject;
  TListSessionsResponse.RegisterObject;
  TListSessionsResponsedeletedSession.RegisterObject;
  TListSessionsResponsesession.RegisterObject;
  TSession.RegisterObject;
  TValue.RegisterObject;
end;


Function TFitnessAPI.GetUsersInstance : TUsersResource;

begin
  if (FUsersInstance=Nil) then
    FUsersInstance:=CreateUsersResource;
  Result:=FUsersInstance;
end;

Function TFitnessAPI.CreateUsersResource : TUsersResource;

begin
  Result:=CreateUsersResource(Self);
end;


Function TFitnessAPI.CreateUsersResource(AOwner : TComponent) : TUsersResource;

begin
  Result:=TUsersResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TFitnessAPI.RegisterAPI;
end.
