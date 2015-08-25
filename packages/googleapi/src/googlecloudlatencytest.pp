unit googlecloudlatencytest;
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
//Generated on: 16-5-15 08:53:00
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAggregatedStats = Class;
  TAggregatedStatsReply = Class;
  TDoubleValue = Class;
  TIntValue = Class;
  TStats = Class;
  TStatsReply = Class;
  TStringValue = Class;
  TAggregatedStatsArray = Array of TAggregatedStats;
  TAggregatedStatsReplyArray = Array of TAggregatedStatsReply;
  TDoubleValueArray = Array of TDoubleValue;
  TIntValueArray = Array of TIntValue;
  TStatsArray = Array of TStats;
  TStatsReplyArray = Array of TStatsReply;
  TStringValueArray = Array of TStringValue;
  //Anonymous types, using auto-generated names
  TAggregatedStatsTypestatsArray = Array of TStats;
  TStatsTypedoubleValuesArray = Array of TDoubleValue;
  TStatsTypeintValuesArray = Array of TIntValue;
  TStatsTypestringValuesArray = Array of TStringValue;
  
  { --------------------------------------------------------------------
    TAggregatedStats
    --------------------------------------------------------------------}
  
  TAggregatedStats = Class(TGoogleBaseObject)
  Private
    Fstats : TAggregatedStatsTypestatsArray;
  Protected
    //Property setters
    Procedure Setstats(AIndex : Integer; AValue : TAggregatedStatsTypestatsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property stats : TAggregatedStatsTypestatsArray Index 0 Read Fstats Write Setstats;
  end;
  TAggregatedStatsClass = Class of TAggregatedStats;
  
  { --------------------------------------------------------------------
    TAggregatedStatsReply
    --------------------------------------------------------------------}
  
  TAggregatedStatsReply = Class(TGoogleBaseObject)
  Private
    FtestValue : String;
  Protected
    //Property setters
    Procedure SettestValue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property testValue : String Index 0 Read FtestValue Write SettestValue;
  end;
  TAggregatedStatsReplyClass = Class of TAggregatedStatsReply;
  
  { --------------------------------------------------------------------
    TDoubleValue
    --------------------------------------------------------------------}
  
  TDoubleValue = Class(TGoogleBaseObject)
  Private
    F_label : String;
    Fvalue : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_label(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property _label : String Index 0 Read F_label Write Set_label;
    Property value : integer Index 8 Read Fvalue Write Setvalue;
  end;
  TDoubleValueClass = Class of TDoubleValue;
  
  { --------------------------------------------------------------------
    TIntValue
    --------------------------------------------------------------------}
  
  TIntValue = Class(TGoogleBaseObject)
  Private
    F_label : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_label(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _label : String Index 0 Read F_label Write Set_label;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TIntValueClass = Class of TIntValue;
  
  { --------------------------------------------------------------------
    TStats
    --------------------------------------------------------------------}
  
  TStats = Class(TGoogleBaseObject)
  Private
    FdoubleValues : TStatsTypedoubleValuesArray;
    FintValues : TStatsTypeintValuesArray;
    FstringValues : TStatsTypestringValuesArray;
    Ftime : double;
  Protected
    //Property setters
    Procedure SetdoubleValues(AIndex : Integer; AValue : TStatsTypedoubleValuesArray); virtual;
    Procedure SetintValues(AIndex : Integer; AValue : TStatsTypeintValuesArray); virtual;
    Procedure SetstringValues(AIndex : Integer; AValue : TStatsTypestringValuesArray); virtual;
    Procedure Settime(AIndex : Integer; AValue : double); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property doubleValues : TStatsTypedoubleValuesArray Index 0 Read FdoubleValues Write SetdoubleValues;
    Property intValues : TStatsTypeintValuesArray Index 8 Read FintValues Write SetintValues;
    Property stringValues : TStatsTypestringValuesArray Index 16 Read FstringValues Write SetstringValues;
    Property time : double Index 24 Read Ftime Write Settime;
  end;
  TStatsClass = Class of TStats;
  
  { --------------------------------------------------------------------
    TStatsReply
    --------------------------------------------------------------------}
  
  TStatsReply = Class(TGoogleBaseObject)
  Private
    FtestValue : String;
  Protected
    //Property setters
    Procedure SettestValue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property testValue : String Index 0 Read FtestValue Write SettestValue;
  end;
  TStatsReplyClass = Class of TStatsReply;
  
  { --------------------------------------------------------------------
    TStringValue
    --------------------------------------------------------------------}
  
  TStringValue = Class(TGoogleBaseObject)
  Private
    F_label : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_label(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _label : String Index 0 Read F_label Write Set_label;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TStringValueClass = Class of TStringValue;
  
  { --------------------------------------------------------------------
    TStatscollectionResource
    --------------------------------------------------------------------}
  
  TStatscollectionResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Updateaggregatedstats(aAggregatedStats : TAggregatedStats) : TAggregatedStatsReply;
    Function Updatestats(aStats : TStats) : TStatsReply;
  end;
  
  
  { --------------------------------------------------------------------
    TCloudlatencytestAPI
    --------------------------------------------------------------------}
  
  TCloudlatencytestAPI = Class(TGoogleAPI)
  Private
    FStatscollectionInstance : TStatscollectionResource;
    Function GetStatscollectionInstance : TStatscollectionResource;virtual;
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
    Function CreateStatscollectionResource(AOwner : TComponent) : TStatscollectionResource;virtual;overload;
    Function CreateStatscollectionResource : TStatscollectionResource;virtual;overload;
    //Add default on-demand instances for resources
    Property StatscollectionResource : TStatscollectionResource Read GetStatscollectionInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAggregatedStats
  --------------------------------------------------------------------}


Procedure TAggregatedStats.Setstats(AIndex : Integer; AValue : TAggregatedStatsTypestatsArray); 

begin
  If (Fstats=AValue) then exit;
  Fstats:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAggregatedStats.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'stats' : SetLength(Fstats,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAggregatedStatsReply
  --------------------------------------------------------------------}


Procedure TAggregatedStatsReply.SettestValue(AIndex : Integer; AValue : String); 

begin
  If (FtestValue=AValue) then exit;
  FtestValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDoubleValue
  --------------------------------------------------------------------}


Procedure TDoubleValue.Set_label(AIndex : Integer; AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDoubleValue.Setvalue(AIndex : Integer; AValue : integer); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDoubleValue.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TIntValue
  --------------------------------------------------------------------}


Procedure TIntValue.Set_label(AIndex : Integer; AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIntValue.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TIntValue.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TStats
  --------------------------------------------------------------------}


Procedure TStats.SetdoubleValues(AIndex : Integer; AValue : TStatsTypedoubleValuesArray); 

begin
  If (FdoubleValues=AValue) then exit;
  FdoubleValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStats.SetintValues(AIndex : Integer; AValue : TStatsTypeintValuesArray); 

begin
  If (FintValues=AValue) then exit;
  FintValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStats.SetstringValues(AIndex : Integer; AValue : TStatsTypestringValuesArray); 

begin
  If (FstringValues=AValue) then exit;
  FstringValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStats.Settime(AIndex : Integer; AValue : double); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStats.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'doublevalues' : SetLength(FdoubleValues,ALength);
  'intvalues' : SetLength(FintValues,ALength);
  'stringvalues' : SetLength(FstringValues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStatsReply
  --------------------------------------------------------------------}


Procedure TStatsReply.SettestValue(AIndex : Integer; AValue : String); 

begin
  If (FtestValue=AValue) then exit;
  FtestValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStringValue
  --------------------------------------------------------------------}


Procedure TStringValue.Set_label(AIndex : Integer; AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStringValue.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TStringValue.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TStatscollectionResource
  --------------------------------------------------------------------}


Class Function TStatscollectionResource.ResourceName : String;

begin
  Result:='statscollection';
end;

Class Function TStatscollectionResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudlatencytestAPI;
end;

Function TStatscollectionResource.Updateaggregatedstats(aAggregatedStats : TAggregatedStats) : TAggregatedStatsReply;

Const
  _HTTPMethod = 'POST';
  _Path       = 'updateaggregatedstats';
  _Methodid   = 'cloudlatencytest.statscollection.updateaggregatedstats';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAggregatedStats,TAggregatedStatsReply) as TAggregatedStatsReply;
end;

Function TStatscollectionResource.Updatestats(aStats : TStats) : TStatsReply;

Const
  _HTTPMethod = 'POST';
  _Path       = 'updatestats';
  _Methodid   = 'cloudlatencytest.statscollection.updatestats';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aStats,TStatsReply) as TStatsReply;
end;



{ --------------------------------------------------------------------
  TCloudlatencytestAPI
  --------------------------------------------------------------------}

Class Function TCloudlatencytestAPI.APIName : String;

begin
  Result:='cloudlatencytest';
end;

Class Function TCloudlatencytestAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TCloudlatencytestAPI.APIRevision : String;

begin
  Result:='20150508';
end;

Class Function TCloudlatencytestAPI.APIID : String;

begin
  Result:='cloudlatencytest:v2';
end;

Class Function TCloudlatencytestAPI.APITitle : String;

begin
  Result:='Google Cloud Network Performance Monitoring API';
end;

Class Function TCloudlatencytestAPI.APIDescription : String;

begin
  Result:='A Test API to report latency data.';
end;

Class Function TCloudlatencytestAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCloudlatencytestAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCloudlatencytestAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCloudlatencytestAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCloudlatencytestAPI.APIdocumentationLink : String;

begin
  Result:='';
end;

Class Function TCloudlatencytestAPI.APIrootUrl : string;

begin
  Result:='https://cloudlatencytest-pa.googleapis.com/';
end;

Class Function TCloudlatencytestAPI.APIbasePath : string;

begin
  Result:='/v2/statscollection/';
end;

Class Function TCloudlatencytestAPI.APIbaseURL : String;

begin
  Result:='https://cloudlatencytest-pa.googleapis.com/v2/statscollection/';
end;

Class Function TCloudlatencytestAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCloudlatencytestAPI.APIservicePath : string;

begin
  Result:='v2/statscollection/';
end;

Class Function TCloudlatencytestAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCloudlatencytestAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/monitoring.readonly';
  Result[0].Description:='View monitoring data for all of your Google Cloud and API projects';
  
end;

Class Function TCloudlatencytestAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TCloudlatencytestAPI.RegisterAPIResources;

begin
  TAggregatedStats.RegisterObject;
  TAggregatedStatsReply.RegisterObject;
  TDoubleValue.RegisterObject;
  TIntValue.RegisterObject;
  TStats.RegisterObject;
  TStatsReply.RegisterObject;
  TStringValue.RegisterObject;
end;


Function TCloudlatencytestAPI.GetStatscollectionInstance : TStatscollectionResource;

begin
  if (FStatscollectionInstance=Nil) then
    FStatscollectionInstance:=CreateStatscollectionResource;
  Result:=FStatscollectionInstance;
end;

Function TCloudlatencytestAPI.CreateStatscollectionResource : TStatscollectionResource;

begin
  Result:=CreateStatscollectionResource(Self);
end;


Function TCloudlatencytestAPI.CreateStatscollectionResource(AOwner : TComponent) : TStatscollectionResource;

begin
  Result:=TStatscollectionResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TCloudlatencytestAPI.RegisterAPI;
end.
