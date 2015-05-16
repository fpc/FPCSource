unit googletaskqueue;
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
//Generated on: 16-5-15 08:53:08
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TTask = Class;
  TTaskQueue = Class;
  TTasks = Class;
  TTasks2 = Class;
  TTaskArray = Array of TTask;
  TTaskQueueArray = Array of TTaskQueue;
  TTasksArray = Array of TTasks;
  TTasks2Array = Array of TTasks2;
  //Anonymous types, using auto-generated names
  TTaskQueueTypeacl = Class;
  TTaskQueueTypestats = Class;
  TTasksTypeitemsArray = Array of TTask;
  TTasks2TypeitemsArray = Array of TTask;
  
  { --------------------------------------------------------------------
    TTask
    --------------------------------------------------------------------}
  
  TTask = Class(TGoogleBaseObject)
  Private
    FenqueueTimestamp : String;
    Fid : String;
    Fkind : String;
    FleaseTimestamp : String;
    FpayloadBase64 : String;
    FqueueName : String;
    Fretry_count : integer;
    Ftag : String;
  Protected
    //Property setters
    Procedure SetenqueueTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetleaseTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetpayloadBase64(AIndex : Integer; AValue : String); virtual;
    Procedure SetqueueName(AIndex : Integer; AValue : String); virtual;
    Procedure Setretry_count(AIndex : Integer; AValue : integer); virtual;
    Procedure Settag(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property enqueueTimestamp : String Index 0 Read FenqueueTimestamp Write SetenqueueTimestamp;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property leaseTimestamp : String Index 24 Read FleaseTimestamp Write SetleaseTimestamp;
    Property payloadBase64 : String Index 32 Read FpayloadBase64 Write SetpayloadBase64;
    Property queueName : String Index 40 Read FqueueName Write SetqueueName;
    Property retry_count : integer Index 48 Read Fretry_count Write Setretry_count;
    Property tag : String Index 56 Read Ftag Write Settag;
  end;
  TTaskClass = Class of TTask;
  
  { --------------------------------------------------------------------
    TTaskQueueTypeacl
    --------------------------------------------------------------------}
  
  TTaskQueueTypeacl = Class(TGoogleBaseObject)
  Private
    FadminEmails : TStringArray;
    FconsumerEmails : TStringArray;
    FproducerEmails : TStringArray;
  Protected
    //Property setters
    Procedure SetadminEmails(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetconsumerEmails(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetproducerEmails(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property adminEmails : TStringArray Index 0 Read FadminEmails Write SetadminEmails;
    Property consumerEmails : TStringArray Index 8 Read FconsumerEmails Write SetconsumerEmails;
    Property producerEmails : TStringArray Index 16 Read FproducerEmails Write SetproducerEmails;
  end;
  TTaskQueueTypeaclClass = Class of TTaskQueueTypeacl;
  
  { --------------------------------------------------------------------
    TTaskQueueTypestats
    --------------------------------------------------------------------}
  
  TTaskQueueTypestats = Class(TGoogleBaseObject)
  Private
    FleasedLastHour : String;
    FleasedLastMinute : String;
    FoldestTask : String;
    FtotalTasks : integer;
  Protected
    //Property setters
    Procedure SetleasedLastHour(AIndex : Integer; AValue : String); virtual;
    Procedure SetleasedLastMinute(AIndex : Integer; AValue : String); virtual;
    Procedure SetoldestTask(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalTasks(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property leasedLastHour : String Index 0 Read FleasedLastHour Write SetleasedLastHour;
    Property leasedLastMinute : String Index 8 Read FleasedLastMinute Write SetleasedLastMinute;
    Property oldestTask : String Index 16 Read FoldestTask Write SetoldestTask;
    Property totalTasks : integer Index 24 Read FtotalTasks Write SettotalTasks;
  end;
  TTaskQueueTypestatsClass = Class of TTaskQueueTypestats;
  
  { --------------------------------------------------------------------
    TTaskQueue
    --------------------------------------------------------------------}
  
  TTaskQueue = Class(TGoogleBaseObject)
  Private
    Facl : TTaskQueueTypeacl;
    Fid : String;
    Fkind : String;
    FmaxLeases : integer;
    Fstats : TTaskQueueTypestats;
  Protected
    //Property setters
    Procedure Setacl(AIndex : Integer; AValue : TTaskQueueTypeacl); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxLeases(AIndex : Integer; AValue : integer); virtual;
    Procedure Setstats(AIndex : Integer; AValue : TTaskQueueTypestats); virtual;
  Public
  Published
    Property acl : TTaskQueueTypeacl Index 0 Read Facl Write Setacl;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property maxLeases : integer Index 24 Read FmaxLeases Write SetmaxLeases;
    Property stats : TTaskQueueTypestats Index 32 Read Fstats Write Setstats;
  end;
  TTaskQueueClass = Class of TTaskQueue;
  
  { --------------------------------------------------------------------
    TTasks
    --------------------------------------------------------------------}
  
  TTasks = Class(TGoogleBaseObject)
  Private
    Fitems : TTasksTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTasksTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TTasksTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TTasksClass = Class of TTasks;
  
  { --------------------------------------------------------------------
    TTasks2
    --------------------------------------------------------------------}
  
  TTasks2 = Class(TGoogleBaseObject)
  Private
    Fitems : TTasks2TypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTasks2TypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TTasks2TypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TTasks2Class = Class of TTasks2;
  
  { --------------------------------------------------------------------
    TTaskqueuesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTaskqueuesResource, method Get
  
  TTaskqueuesGetOptions = Record
    getStats : boolean;
  end;
  
  TTaskqueuesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(project: string; taskqueue: string; AQuery : string  = '') : TTaskQueue;
    Function Get(project: string; taskqueue: string; AQuery : TTaskqueuesgetOptions) : TTaskQueue;
  end;
  
  
  { --------------------------------------------------------------------
    TTasksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTasksResource, method Lease
  
  TTasksLeaseOptions = Record
    groupByTag : boolean;
    leaseSecs : integer;
    numTasks : integer;
    tag : String;
  end;
  
  
  //Optional query Options for TTasksResource, method Patch
  
  TTasksPatchOptions = Record
    newLeaseSeconds : integer;
  end;
  
  
  //Optional query Options for TTasksResource, method Update
  
  TTasksUpdateOptions = Record
    newLeaseSeconds : integer;
  end;
  
  TTasksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(project: string; task: string; taskqueue: string);
    Function Get(project: string; task: string; taskqueue: string) : TTask;
    Function Insert(project: string; taskqueue: string; aTask : TTask) : TTask;
    Function Lease(project: string; taskqueue: string; AQuery : string  = '') : TTasks;
    Function Lease(project: string; taskqueue: string; AQuery : TTasksleaseOptions) : TTasks;
    Function List(project: string; taskqueue: string) : TTasks2;
    Function Patch(project: string; task: string; taskqueue: string; aTask : TTask; AQuery : string  = '') : TTask;
    Function Patch(project: string; task: string; taskqueue: string; aTask : TTask; AQuery : TTaskspatchOptions) : TTask;
    Function Update(project: string; task: string; taskqueue: string; aTask : TTask; AQuery : string  = '') : TTask;
    Function Update(project: string; task: string; taskqueue: string; aTask : TTask; AQuery : TTasksupdateOptions) : TTask;
  end;
  
  
  { --------------------------------------------------------------------
    TTaskqueueAPI
    --------------------------------------------------------------------}
  
  TTaskqueueAPI = Class(TGoogleAPI)
  Private
    FTaskqueuesInstance : TTaskqueuesResource;
    FTasksInstance : TTasksResource;
    Function GetTaskqueuesInstance : TTaskqueuesResource;virtual;
    Function GetTasksInstance : TTasksResource;virtual;
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
    Function CreateTaskqueuesResource(AOwner : TComponent) : TTaskqueuesResource;virtual;overload;
    Function CreateTaskqueuesResource : TTaskqueuesResource;virtual;overload;
    Function CreateTasksResource(AOwner : TComponent) : TTasksResource;virtual;overload;
    Function CreateTasksResource : TTasksResource;virtual;overload;
    //Add default on-demand instances for resources
    Property TaskqueuesResource : TTaskqueuesResource Read GetTaskqueuesInstance;
    Property TasksResource : TTasksResource Read GetTasksInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TTask
  --------------------------------------------------------------------}


Procedure TTask.SetenqueueTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FenqueueTimestamp=AValue) then exit;
  FenqueueTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.SetleaseTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FleaseTimestamp=AValue) then exit;
  FleaseTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.SetpayloadBase64(AIndex : Integer; AValue : String); 

begin
  If (FpayloadBase64=AValue) then exit;
  FpayloadBase64:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.SetqueueName(AIndex : Integer; AValue : String); 

begin
  If (FqueueName=AValue) then exit;
  FqueueName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setretry_count(AIndex : Integer; AValue : integer); 

begin
  If (Fretry_count=AValue) then exit;
  Fretry_count:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Settag(AIndex : Integer; AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskQueueTypeacl
  --------------------------------------------------------------------}


Procedure TTaskQueueTypeacl.SetadminEmails(AIndex : Integer; AValue : TStringArray); 

begin
  If (FadminEmails=AValue) then exit;
  FadminEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueueTypeacl.SetconsumerEmails(AIndex : Integer; AValue : TStringArray); 

begin
  If (FconsumerEmails=AValue) then exit;
  FconsumerEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueueTypeacl.SetproducerEmails(AIndex : Integer; AValue : TStringArray); 

begin
  If (FproducerEmails=AValue) then exit;
  FproducerEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTaskQueueTypeacl.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'adminemails' : SetLength(FadminEmails,ALength);
  'consumeremails' : SetLength(FconsumerEmails,ALength);
  'produceremails' : SetLength(FproducerEmails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTaskQueueTypestats
  --------------------------------------------------------------------}


Procedure TTaskQueueTypestats.SetleasedLastHour(AIndex : Integer; AValue : String); 

begin
  If (FleasedLastHour=AValue) then exit;
  FleasedLastHour:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueueTypestats.SetleasedLastMinute(AIndex : Integer; AValue : String); 

begin
  If (FleasedLastMinute=AValue) then exit;
  FleasedLastMinute:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueueTypestats.SetoldestTask(AIndex : Integer; AValue : String); 

begin
  If (FoldestTask=AValue) then exit;
  FoldestTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueueTypestats.SettotalTasks(AIndex : Integer; AValue : integer); 

begin
  If (FtotalTasks=AValue) then exit;
  FtotalTasks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskQueue
  --------------------------------------------------------------------}


Procedure TTaskQueue.Setacl(AIndex : Integer; AValue : TTaskQueueTypeacl); 

begin
  If (Facl=AValue) then exit;
  Facl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueue.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueue.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueue.SetmaxLeases(AIndex : Integer; AValue : integer); 

begin
  If (FmaxLeases=AValue) then exit;
  FmaxLeases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueue.Setstats(AIndex : Integer; AValue : TTaskQueueTypestats); 

begin
  If (Fstats=AValue) then exit;
  Fstats:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTasks
  --------------------------------------------------------------------}


Procedure TTasks.Setitems(AIndex : Integer; AValue : TTasksTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTasks.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTasks.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTasks2
  --------------------------------------------------------------------}


Procedure TTasks2.Setitems(AIndex : Integer; AValue : TTasks2TypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTasks2.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTasks2.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTaskqueuesResource
  --------------------------------------------------------------------}


Class Function TTaskqueuesResource.ResourceName : String;

begin
  Result:='taskqueues';
end;

Class Function TTaskqueuesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtaskqueueAPI;
end;

Function TTaskqueuesResource.Get(project: string; taskqueue: string; AQuery : string = '') : TTaskQueue;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/taskqueues/{taskqueue}';
  _Methodid   = 'taskqueue.taskqueues.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'taskqueue',taskqueue]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTaskQueue) as TTaskQueue;
end;


Function TTaskqueuesResource.Get(project: string; taskqueue: string; AQuery : TTaskqueuesgetOptions) : TTaskQueue;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'getStats',AQuery.getStats);
  Result:=Get(project,taskqueue,_Q);
end;



{ --------------------------------------------------------------------
  TTasksResource
  --------------------------------------------------------------------}


Class Function TTasksResource.ResourceName : String;

begin
  Result:='tasks';
end;

Class Function TTasksResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtaskqueueAPI;
end;

Procedure TTasksResource.Delete(project: string; task: string; taskqueue: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/taskqueues/{taskqueue}/tasks/{task}';
  _Methodid   = 'taskqueue.tasks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'task',task,'taskqueue',taskqueue]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTasksResource.Get(project: string; task: string; taskqueue: string) : TTask;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/taskqueues/{taskqueue}/tasks/{task}';
  _Methodid   = 'taskqueue.tasks.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'task',task,'taskqueue',taskqueue]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTask) as TTask;
end;

Function TTasksResource.Insert(project: string; taskqueue: string; aTask : TTask) : TTask;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/taskqueues/{taskqueue}/tasks';
  _Methodid   = 'taskqueue.tasks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'taskqueue',taskqueue]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTask,TTask) as TTask;
end;

Function TTasksResource.Lease(project: string; taskqueue: string; AQuery : string = '') : TTasks;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/taskqueues/{taskqueue}/tasks/lease';
  _Methodid   = 'taskqueue.tasks.lease';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'taskqueue',taskqueue]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTasks) as TTasks;
end;


Function TTasksResource.Lease(project: string; taskqueue: string; AQuery : TTasksleaseOptions) : TTasks;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'groupByTag',AQuery.groupByTag);
  AddToQuery(_Q,'leaseSecs',AQuery.leaseSecs);
  AddToQuery(_Q,'numTasks',AQuery.numTasks);
  AddToQuery(_Q,'tag',AQuery.tag);
  Result:=Lease(project,taskqueue,_Q);
end;

Function TTasksResource.List(project: string; taskqueue: string) : TTasks2;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/taskqueues/{taskqueue}/tasks';
  _Methodid   = 'taskqueue.tasks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'taskqueue',taskqueue]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTasks2) as TTasks2;
end;

Function TTasksResource.Patch(project: string; task: string; taskqueue: string; aTask : TTask; AQuery : string = '') : TTask;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{project}/taskqueues/{taskqueue}/tasks/{task}';
  _Methodid   = 'taskqueue.tasks.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'task',task,'taskqueue',taskqueue]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTask,TTask) as TTask;
end;


Function TTasksResource.Patch(project: string; task: string; taskqueue: string; aTask : TTask; AQuery : TTaskspatchOptions) : TTask;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'newLeaseSeconds',AQuery.newLeaseSeconds);
  Result:=Patch(project,task,taskqueue,aTask,_Q);
end;

Function TTasksResource.Update(project: string; task: string; taskqueue: string; aTask : TTask; AQuery : string = '') : TTask;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/taskqueues/{taskqueue}/tasks/{task}';
  _Methodid   = 'taskqueue.tasks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'task',task,'taskqueue',taskqueue]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTask,TTask) as TTask;
end;


Function TTasksResource.Update(project: string; task: string; taskqueue: string; aTask : TTask; AQuery : TTasksupdateOptions) : TTask;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'newLeaseSeconds',AQuery.newLeaseSeconds);
  Result:=Update(project,task,taskqueue,aTask,_Q);
end;



{ --------------------------------------------------------------------
  TTaskqueueAPI
  --------------------------------------------------------------------}

Class Function TTaskqueueAPI.APIName : String;

begin
  Result:='taskqueue';
end;

Class Function TTaskqueueAPI.APIVersion : String;

begin
  Result:='v1beta2';
end;

Class Function TTaskqueueAPI.APIRevision : String;

begin
  Result:='20141111';
end;

Class Function TTaskqueueAPI.APIID : String;

begin
  Result:='taskqueue:v1beta2';
end;

Class Function TTaskqueueAPI.APITitle : String;

begin
  Result:='TaskQueue API';
end;

Class Function TTaskqueueAPI.APIDescription : String;

begin
  Result:='Lets you access a Google App Engine Pull Task Queue over REST.';
end;

Class Function TTaskqueueAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TTaskqueueAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TTaskqueueAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/app_engine-16.png';
end;

Class Function TTaskqueueAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/app_engine-32.png';
end;

Class Function TTaskqueueAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/appengine/docs/python/taskqueue/rest';
end;

Class Function TTaskqueueAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TTaskqueueAPI.APIbasePath : string;

begin
  Result:='/taskqueue/v1beta2/projects/';
end;

Class Function TTaskqueueAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/taskqueue/v1beta2/projects/';
end;

Class Function TTaskqueueAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TTaskqueueAPI.APIservicePath : string;

begin
  Result:='taskqueue/v1beta2/projects/';
end;

Class Function TTaskqueueAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TTaskqueueAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/taskqueue';
  Result[0].Description:='Manage your Tasks and Taskqueues';
  Result[1].Name:='https://www.googleapis.com/auth/taskqueue.consumer';
  Result[1].Description:='Consume Tasks from your Taskqueues';
  
end;

Class Function TTaskqueueAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TTaskqueueAPI.RegisterAPIResources;

begin
  TTask.RegisterObject;
  TTaskQueueTypeacl.RegisterObject;
  TTaskQueueTypestats.RegisterObject;
  TTaskQueue.RegisterObject;
  TTasks.RegisterObject;
  TTasks2.RegisterObject;
end;


Function TTaskqueueAPI.GetTaskqueuesInstance : TTaskqueuesResource;

begin
  if (FTaskqueuesInstance=Nil) then
    FTaskqueuesInstance:=CreateTaskqueuesResource;
  Result:=FTaskqueuesInstance;
end;

Function TTaskqueueAPI.CreateTaskqueuesResource : TTaskqueuesResource;

begin
  Result:=CreateTaskqueuesResource(Self);
end;


Function TTaskqueueAPI.CreateTaskqueuesResource(AOwner : TComponent) : TTaskqueuesResource;

begin
  Result:=TTaskqueuesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTaskqueueAPI.GetTasksInstance : TTasksResource;

begin
  if (FTasksInstance=Nil) then
    FTasksInstance:=CreateTasksResource;
  Result:=FTasksInstance;
end;

Function TTaskqueueAPI.CreateTasksResource : TTasksResource;

begin
  Result:=CreateTasksResource(Self);
end;


Function TTaskqueueAPI.CreateTasksResource(AOwner : TComponent) : TTasksResource;

begin
  Result:=TTasksResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TTaskqueueAPI.RegisterAPI;
end.
