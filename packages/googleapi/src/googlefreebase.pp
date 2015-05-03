unit googlefreebase;
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
  TReconcileCandidate = class;
  TReconcileCandidateArray = Array of TReconcileCandidate;
  TReconcileCandidatenotable = class;
  TReconcileCandidatenotableArray = Array of TReconcileCandidatenotable;
  TReconcileGet = class;
  TReconcileGetArray = Array of TReconcileGet;
  TReconcileGetcandidate = class;
  TReconcileGetcandidateArray = Array of TReconcileGetcandidate;
  TReconcileGetcosts = class;
  TReconcileGetcostsArray = Array of TReconcileGetcosts;
  TReconcileGetwarning = class;
  TReconcileGetwarningArray = Array of TReconcileGetwarning;
  
  { --------------------------------------------------------------------
    TReconcileCandidate
    --------------------------------------------------------------------}
  
  TReconcileCandidate = Class(TGoogleBaseObject)
  Private
    Fconfidence : integer;
    Flang : string;
    Fmid : string;
    Fname : string;
    Fnotable : TReconcileCandidatenotable;
  Protected
    //Property setters
    Procedure Setconfidence(AIndex : Integer; AValue : integer); virtual;
    Procedure Setlang(AIndex : Integer; AValue : string); virtual;
    Procedure Setmid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotable(AIndex : Integer; AValue : TReconcileCandidatenotable); virtual;
  Public
  Published
    Property confidence : integer Index 0 Read Fconfidence Write Setconfidence;
    Property lang : string Index 8 Read Flang Write Setlang;
    Property mid : string Index 16 Read Fmid Write Setmid;
    Property name : string Index 24 Read Fname Write Setname;
    Property notable : TReconcileCandidatenotable Index 32 Read Fnotable Write Setnotable;
  end;
  TReconcileCandidateClass = Class of TReconcileCandidate;
  
  { --------------------------------------------------------------------
    TReconcileCandidatenotable
    --------------------------------------------------------------------}
  
  TReconcileCandidatenotable = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property name : string Index 8 Read Fname Write Setname;
  end;
  TReconcileCandidatenotableClass = Class of TReconcileCandidatenotable;
  
  { --------------------------------------------------------------------
    TReconcileGet
    --------------------------------------------------------------------}
  
  TReconcileGet = Class(TGoogleBaseObject)
  Private
    Fcandidate : TReconcileGetcandidate;
    Fcosts : TReconcileGetcosts;
    Fmatch : TReconcileCandidate;
    Fwarning : TReconcileGetwarning;
  Protected
    //Property setters
    Procedure Setcandidate(AIndex : Integer; AValue : TReconcileGetcandidate); virtual;
    Procedure Setcosts(AIndex : Integer; AValue : TReconcileGetcosts); virtual;
    Procedure Setmatch(AIndex : Integer; AValue : TReconcileCandidate); virtual;
    Procedure Setwarning(AIndex : Integer; AValue : TReconcileGetwarning); virtual;
  Public
  Published
    Property candidate : TReconcileGetcandidate Index 0 Read Fcandidate Write Setcandidate;
    Property costs : TReconcileGetcosts Index 8 Read Fcosts Write Setcosts;
    Property match : TReconcileCandidate Index 16 Read Fmatch Write Setmatch;
    Property warning : TReconcileGetwarning Index 24 Read Fwarning Write Setwarning;
  end;
  TReconcileGetClass = Class of TReconcileGet;
  
  { --------------------------------------------------------------------
    TReconcileGetcandidate
    --------------------------------------------------------------------}
  
  TReconcileGetcandidate = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReconcileGetcandidateClass = Class of TReconcileGetcandidate;
  
  { --------------------------------------------------------------------
    TReconcileGetcosts
    --------------------------------------------------------------------}
  
  TReconcileGetcosts = Class(TGoogleBaseObject)
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
  TReconcileGetcostsClass = Class of TReconcileGetcosts;
  
  { --------------------------------------------------------------------
    TReconcileGetwarning
    --------------------------------------------------------------------}
  
  TReconcileGetwarning = Class(TGoogleBaseObject)
  Private
    Flocation : string;
    Fmessage : string;
    Freason : string;
  Protected
    //Property setters
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setreason(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property location : string Index 0 Read Flocation Write Setlocation;
    Property message : string Index 8 Read Fmessage Write Setmessage;
    Property reason : string Index 16 Read Freason Write Setreason;
  end;
  TReconcileGetwarningClass = Class of TReconcileGetwarning;
  
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
  TReconcileCandidate
  --------------------------------------------------------------------}


Procedure TReconcileCandidate.Setconfidence(AIndex : Integer; AValue : integer); 

begin
  If (Fconfidence=AValue) then exit;
  Fconfidence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidate.Setlang(AIndex : Integer; AValue : string); 

begin
  If (Flang=AValue) then exit;
  Flang:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidate.Setmid(AIndex : Integer; AValue : string); 

begin
  If (Fmid=AValue) then exit;
  Fmid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidate.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidate.Setnotable(AIndex : Integer; AValue : TReconcileCandidatenotable); 

begin
  If (Fnotable=AValue) then exit;
  Fnotable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReconcileCandidatenotable
  --------------------------------------------------------------------}


Procedure TReconcileCandidatenotable.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileCandidatenotable.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReconcileGet
  --------------------------------------------------------------------}


Procedure TReconcileGet.Setcandidate(AIndex : Integer; AValue : TReconcileGetcandidate); 

begin
  If (Fcandidate=AValue) then exit;
  Fcandidate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGet.Setcosts(AIndex : Integer; AValue : TReconcileGetcosts); 

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



Procedure TReconcileGet.Setwarning(AIndex : Integer; AValue : TReconcileGetwarning); 

begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReconcileGetcandidate
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReconcileGetcosts
  --------------------------------------------------------------------}


Procedure TReconcileGetcosts.Sethits(AIndex : Integer; AValue : integer); 

begin
  If (Fhits=AValue) then exit;
  Fhits:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGetcosts.Setms(AIndex : Integer; AValue : integer); 

begin
  If (Fms=AValue) then exit;
  Fms:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReconcileGetwarning
  --------------------------------------------------------------------}


Procedure TReconcileGetwarning.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGetwarning.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReconcileGetwarning.Setreason(AIndex : Integer; AValue : string); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





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
  Result:='https://www.googleapis.com/';
end;

Class Function TFreebaseAPI.APIbasePath : string;

begin
  Result:='/freebase/v1/';
end;

Class Function TFreebaseAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/freebase/v1/';
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
  TReconcileCandidate.RegisterObject;
  TReconcileCandidatenotable.RegisterObject;
  TReconcileGet.RegisterObject;
  TReconcileGetcandidate.RegisterObject;
  TReconcileGetcosts.RegisterObject;
  TReconcileGetwarning.RegisterObject;
end;


initialization
  TFreebaseAPI.RegisterAPI;
end.
