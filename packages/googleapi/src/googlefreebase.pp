unit googlefreebase;
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
//Generated on: 16-5-15 08:53:03
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TReconcileCandidate = Class;
  TReconcileGet = Class;
  TReconcileCandidateArray = Array of TReconcileCandidate;
  TReconcileGetArray = Array of TReconcileGet;
  //Anonymous types, using auto-generated names
  TReconcileCandidateTypenotable = Class;
  TReconcileGetTypecosts = Class;
  TReconcileGetTypewarningItem = Class;
  TReconcileGetTypecandidateArray = Array of TReconcileCandidate;
  TReconcileGetTypewarningArray = Array of TReconcileGetTypewarningItem;
  
  { --------------------------------------------------------------------
    TReconcileCandidateTypenotable
    --------------------------------------------------------------------}
  
  TReconcileCandidateTypenotable = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TReconcileCandidateTypenotableClass = Class of TReconcileCandidateTypenotable;
  
  { --------------------------------------------------------------------
    TReconcileCandidate
    --------------------------------------------------------------------}
  
  TReconcileCandidate = Class(TGoogleBaseObject)
  Private
    Fconfidence : integer;
    Flang : String;
    Fmid : String;
    Fname : String;
    Fnotable : TReconcileCandidateTypenotable;
  Protected
    //Property setters
    Procedure Setconfidence(AIndex : Integer; AValue : integer); virtual;
    Procedure Setlang(AIndex : Integer; AValue : String); virtual;
    Procedure Setmid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotable(AIndex : Integer; AValue : TReconcileCandidateTypenotable); virtual;
  Public
  Published
    Property confidence : integer Index 0 Read Fconfidence Write Setconfidence;
    Property lang : String Index 8 Read Flang Write Setlang;
    Property mid : String Index 16 Read Fmid Write Setmid;
    Property name : String Index 24 Read Fname Write Setname;
    Property notable : TReconcileCandidateTypenotable Index 32 Read Fnotable Write Setnotable;
  end;
  TReconcileCandidateClass = Class of TReconcileCandidate;
  
  { --------------------------------------------------------------------
    TReconcileGetTypecosts
    --------------------------------------------------------------------}
  
  TReconcileGetTypecosts = Class(TGoogleBaseObject)
  Private
    Fhits : integer;
    Fms : integer;
  Protected
    //Property setters
    Procedure Sethits(AIndex : Integer; AValue : integer); virtual;
    Procedure Setms(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property hits : integer Index 0 Read Fhits Write Sethits;
    Property ms : integer Index 8 Read Fms Write Setms;
  end;
  TReconcileGetTypecostsClass = Class of TReconcileGetTypecosts;
  
  { --------------------------------------------------------------------
    TReconcileGetTypewarningItem
    --------------------------------------------------------------------}
  
  TReconcileGetTypewarningItem = Class(TGoogleBaseObject)
  Private
    Flocation : String;
    Fmessage : String;
    Freason : String;
  Protected
    //Property setters
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setreason(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property location : String Index 0 Read Flocation Write Setlocation;
    Property message : String Index 8 Read Fmessage Write Setmessage;
    Property reason : String Index 16 Read Freason Write Setreason;
  end;
  TReconcileGetTypewarningItemClass = Class of TReconcileGetTypewarningItem;
  
  { --------------------------------------------------------------------
    TReconcileGet
    --------------------------------------------------------------------}
  
  TReconcileGet = Class(TGoogleBaseObject)
  Private
    Fcandidate : TReconcileGetTypecandidateArray;
    Fcosts : TReconcileGetTypecosts;
    Fmatch : TReconcileCandidate;
    Fwarning : TReconcileGetTypewarningArray;
  Protected
    //Property setters
    Procedure Setcandidate(AIndex : Integer; AValue : TReconcileGetTypecandidateArray); virtual;
    Procedure Setcosts(AIndex : Integer; AValue : TReconcileGetTypecosts); virtual;
    Procedure Setmatch(AIndex : Integer; AValue : TReconcileCandidate); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TReconcileGetTypewarningArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property candidate : TReconcileGetTypecandidateArray Index 0 Read Fcandidate Write Setcandidate;
    Property costs : TReconcileGetTypecosts Index 8 Read Fcosts Write Setcosts;
    Property match : TReconcileCandidate Index 16 Read Fmatch Write Setmatch;
    Property warning : TReconcileGetTypewarningArray Index 24 Read Fwarning Write Setwarning;
  end;
  TReconcileGetClass = Class of TReconcileGet;
  
  { --------------------------------------------------------------------
    TFreebaseAPI
    --------------------------------------------------------------------}
  
  TFreebaseAPI = Class(TGoogleAPI)
  Private
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
    //Add default on-demand instances for resources
  end;

implementation


{ --------------------------------------------------------------------
  TReconcileCandidateTypenotable
  --------------------------------------------------------------------}


Procedure TReconcileCandidateTypenotable.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidateTypenotable.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReconcileCandidate
  --------------------------------------------------------------------}


Procedure TReconcileCandidate.Setconfidence(AIndex : Integer; AValue : integer); 

begin
  If (Fconfidence=AValue) then exit;
  Fconfidence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidate.Setlang(AIndex : Integer; AValue : String); 

begin
  If (Flang=AValue) then exit;
  Flang:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidate.Setmid(AIndex : Integer; AValue : String); 

begin
  If (Fmid=AValue) then exit;
  Fmid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidate.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidate.Setnotable(AIndex : Integer; AValue : TReconcileCandidateTypenotable); 

begin
  If (Fnotable=AValue) then exit;
  Fnotable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReconcileGetTypecosts
  --------------------------------------------------------------------}


Procedure TReconcileGetTypecosts.Sethits(AIndex : Integer; AValue : integer); 

begin
  If (Fhits=AValue) then exit;
  Fhits:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGetTypecosts.Setms(AIndex : Integer; AValue : integer); 

begin
  If (Fms=AValue) then exit;
  Fms:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReconcileGetTypewarningItem
  --------------------------------------------------------------------}


Procedure TReconcileGetTypewarningItem.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGetTypewarningItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGetTypewarningItem.Setreason(AIndex : Integer; AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReconcileGet
  --------------------------------------------------------------------}


Procedure TReconcileGet.Setcandidate(AIndex : Integer; AValue : TReconcileGetTypecandidateArray); 

begin
  If (Fcandidate=AValue) then exit;
  Fcandidate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGet.Setcosts(AIndex : Integer; AValue : TReconcileGetTypecosts); 

begin
  If (Fcosts=AValue) then exit;
  Fcosts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGet.Setmatch(AIndex : Integer; AValue : TReconcileCandidate); 

begin
  If (Fmatch=AValue) then exit;
  Fmatch:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGet.Setwarning(AIndex : Integer; AValue : TReconcileGetTypewarningArray); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReconcileGet.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'candidate' : SetLength(Fcandidate,ALength);
  'warning' : SetLength(Fwarning,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFreebaseAPI
  --------------------------------------------------------------------}

Class Function TFreebaseAPI.APIName : String;

begin
  Result:='freebase';
end;

Class Function TFreebaseAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TFreebaseAPI.APIRevision : String;

begin
  Result:='20150330';
end;

Class Function TFreebaseAPI.APIID : String;

begin
  Result:='freebase:v1';
end;

Class Function TFreebaseAPI.APITitle : String;

begin
  Result:='Freebase Search';
end;

Class Function TFreebaseAPI.APIDescription : String;

begin
  Result:='Find Freebase entities using textual queries and other constraints.';
end;

Class Function TFreebaseAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TFreebaseAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TFreebaseAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/freebase-16.png';
end;

Class Function TFreebaseAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/freebase-32.png';
end;

Class Function TFreebaseAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/freebase/';
end;

Class Function TFreebaseAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TFreebaseAPI.APIbasePath : string;

begin
  Result:='/freebase/v1/';
end;

Class Function TFreebaseAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/freebase/v1/';
end;

Class Function TFreebaseAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TFreebaseAPI.APIservicePath : string;

begin
  Result:='freebase/v1/';
end;

Class Function TFreebaseAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TFreebaseAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TFreebaseAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TFreebaseAPI.RegisterAPIResources;

begin
  TReconcileCandidateTypenotable.RegisterObject;
  TReconcileCandidate.RegisterObject;
  TReconcileGetTypecosts.RegisterObject;
  TReconcileGetTypewarningItem.RegisterObject;
  TReconcileGet.RegisterObject;
end;


initialization
  TFreebaseAPI.RegisterAPI;
end.
