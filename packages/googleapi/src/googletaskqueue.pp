unit googletaskqueue;
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
  TTask = class;
  TTaskArray = Array of TTask;
  TTaskQueue = class;
  TTaskQueueArray = Array of TTaskQueue;
  TTaskQueueacl = class;
  TTaskQueueaclArray = Array of TTaskQueueacl;
  TTaskQueueacladminEmails = class;
  TTaskQueueacladminEmailsArray = Array of TTaskQueueacladminEmails;
  TTaskQueueaclconsumerEmails = class;
  TTaskQueueaclconsumerEmailsArray = Array of TTaskQueueaclconsumerEmails;
  TTaskQueueaclproducerEmails = class;
  TTaskQueueaclproducerEmailsArray = Array of TTaskQueueaclproducerEmails;
  TTaskQueuestats = class;
  TTaskQueuestatsArray = Array of TTaskQueuestats;
  TTasks = class;
  TTasksArray = Array of TTasks;
  TTasksitems = class;
  TTasksitemsArray = Array of TTasksitems;
  TTasks2 = class;
  TTasks2Array = Array of TTasks2;
  TTasks2items = class;
  TTasks2itemsArray = Array of TTasks2items;
  
  { --------------------------------------------------------------------
    TTask
    --------------------------------------------------------------------}
  
  TTask = Class(TGoogleBaseObject)
  Private
    FenqueueTimestamp : string;
    Fid : string;
    Fkind : string;
    FleaseTimestamp : string;
    FpayloadBase64 : string;
    FqueueName : string;
    Fretry_count : integer;
    Ftag : string;
  Protected
    //Property setters
    Procedure SetenqueueTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetleaseTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetpayloadBase64(AIndex : Integer; AValue : string); virtual;
    Procedure SetqueueName(AIndex : Integer; AValue : string); virtual;
    Procedure Setretry_count(AIndex : Integer; AValue : integer); virtual;
    Procedure Settag(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property enqueueTimestamp : string Index 0 Read FenqueueTimestamp Write SetenqueueTimestamp;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property leaseTimestamp : string Index 24 Read FleaseTimestamp Write SetleaseTimestamp;
    Property payloadBase64 : string Index 32 Read FpayloadBase64 Write SetpayloadBase64;
    Property queueName : string Index 40 Read FqueueName Write SetqueueName;
    Property retry_count : integer Index 48 Read Fretry_count Write Setretry_count;
    Property tag : string Index 56 Read Ftag Write Settag;
  end;
  TTaskClass = Class of TTask;
  
  { --------------------------------------------------------------------
    TTaskQueue
    --------------------------------------------------------------------}
  
  TTaskQueue = Class(TGoogleBaseObject)
  Private
    Facl : TTaskQueueacl;
    Fid : string;
    Fkind : string;
    FmaxLeases : integer;
    Fstats : TTaskQueuestats;
  Protected
    //Property setters
    Procedure Setacl(AIndex : Integer; AValue : TTaskQueueacl); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxLeases(AIndex : Integer; AValue : integer); virtual;
    Procedure Setstats(AIndex : Integer; AValue : TTaskQueuestats); virtual;
  Public
  Published
    Property acl : TTaskQueueacl Index 0 Read Facl Write Setacl;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property maxLeases : integer Index 24 Read FmaxLeases Write SetmaxLeases;
    Property stats : TTaskQueuestats Index 32 Read Fstats Write Setstats;
  end;
  TTaskQueueClass = Class of TTaskQueue;
  
  { --------------------------------------------------------------------
    TTaskQueueacl
    --------------------------------------------------------------------}
  
  TTaskQueueacl = Class(TGoogleBaseObject)
  Private
    FadminEmails : TTaskQueueacladminEmails;
    FconsumerEmails : TTaskQueueaclconsumerEmails;
    FproducerEmails : TTaskQueueaclproducerEmails;
  Protected
    //Property setters
    Procedure SetadminEmails(AIndex : Integer; AValue : TTaskQueueacladminEmails); virtual;
    Procedure SetconsumerEmails(AIndex : Integer; AValue : TTaskQueueaclconsumerEmails); virtual;
    Procedure SetproducerEmails(AIndex : Integer; AValue : TTaskQueueaclproducerEmails); virtual;
  Public
  Published
    Property adminEmails : TTaskQueueacladminEmails Index 0 Read FadminEmails Write SetadminEmails;
    Property consumerEmails : TTaskQueueaclconsumerEmails Index 8 Read FconsumerEmails Write SetconsumerEmails;
    Property producerEmails : TTaskQueueaclproducerEmails Index 16 Read FproducerEmails Write SetproducerEmails;
  end;
  TTaskQueueaclClass = Class of TTaskQueueacl;
  
  { --------------------------------------------------------------------
    TTaskQueueacladminEmails
    --------------------------------------------------------------------}
  
  TTaskQueueacladminEmails = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTaskQueueacladminEmailsClass = Class of TTaskQueueacladminEmails;
  
  { --------------------------------------------------------------------
    TTaskQueueaclconsumerEmails
    --------------------------------------------------------------------}
  
  TTaskQueueaclconsumerEmails = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTaskQueueaclconsumerEmailsClass = Class of TTaskQueueaclconsumerEmails;
  
  { --------------------------------------------------------------------
    TTaskQueueaclproducerEmails
    --------------------------------------------------------------------}
  
  TTaskQueueaclproducerEmails = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTaskQueueaclproducerEmailsClass = Class of TTaskQueueaclproducerEmails;
  
  { --------------------------------------------------------------------
    TTaskQueuestats
    --------------------------------------------------------------------}
  
  TTaskQueuestats = Class(TGoogleBaseObject)
  Private
    FleasedLastHour : string;
    FleasedLastMinute : string;
    FoldestTask : string;
    FtotalTasks : integer;
  Protected
    //Property setters
    Procedure SetleasedLastHour(AIndex : Integer; AValue : string); virtual;
    Procedure SetleasedLastMinute(AIndex : Integer; AValue : string); virtual;
    Procedure SetoldestTask(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalTasks(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property leasedLastHour : string Index 0 Read FleasedLastHour Write SetleasedLastHour;
    Property leasedLastMinute : string Index 8 Read FleasedLastMinute Write SetleasedLastMinute;
    Property oldestTask : string Index 16 Read FoldestTask Write SetoldestTask;
    Property totalTasks : integer Index 24 Read FtotalTasks Write SettotalTasks;
  end;
  TTaskQueuestatsClass = Class of TTaskQueuestats;
  
  { --------------------------------------------------------------------
    TTasks
    --------------------------------------------------------------------}
  
  TTasks = Class(TGoogleBaseObject)
  Private
    Fitems : TTasksitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTasksitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TTasksitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TTasksClass = Class of TTasks;
  
  { --------------------------------------------------------------------
    TTasksitems
    --------------------------------------------------------------------}
  
  TTasksitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTasksitemsClass = Class of TTasksitems;
  
  { --------------------------------------------------------------------
    TTasks2
    --------------------------------------------------------------------}
  
  TTasks2 = Class(TGoogleBaseObject)
  Private
    Fitems : TTasks2items;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTasks2items); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TTasks2items Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TTasks2Class = Class of TTasks2;
  
  { --------------------------------------------------------------------
    TTasks2items
    --------------------------------------------------------------------}
  
  TTasks2items = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTasks2itemsClass = Class of TTasks2items;
  
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
    tag : string;
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


Procedure TTask.SetenqueueTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FenqueueTimestamp=AValue) then exit;
  FenqueueTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.SetleaseTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FleaseTimestamp=AValue) then exit;
  FleaseTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.SetpayloadBase64(AIndex : Integer; AValue : string); 

begin
  If (FpayloadBase64=AValue) then exit;
  FpayloadBase64:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.SetqueueName(AIndex : Integer; AValue : string); 

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



Procedure TTask.Settag(AIndex : Integer; AValue : string); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskQueue
  --------------------------------------------------------------------}


Procedure TTaskQueue.Setacl(AIndex : Integer; AValue : TTaskQueueacl); 

begin
  If (Facl=AValue) then exit;
  Facl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueue.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueue.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TTaskQueue.Setstats(AIndex : Integer; AValue : TTaskQueuestats); 

begin
  If (Fstats=AValue) then exit;
  Fstats:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskQueueacl
  --------------------------------------------------------------------}


Procedure TTaskQueueacl.SetadminEmails(AIndex : Integer; AValue : TTaskQueueacladminEmails); 

begin
  If (FadminEmails=AValue) then exit;
  FadminEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueueacl.SetconsumerEmails(AIndex : Integer; AValue : TTaskQueueaclconsumerEmails); 

begin
  If (FconsumerEmails=AValue) then exit;
  FconsumerEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueueacl.SetproducerEmails(AIndex : Integer; AValue : TTaskQueueaclproducerEmails); 

begin
  If (FproducerEmails=AValue) then exit;
  FproducerEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskQueueacladminEmails
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTaskQueueaclconsumerEmails
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTaskQueueaclproducerEmails
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTaskQueuestats
  --------------------------------------------------------------------}


Procedure TTaskQueuestats.SetleasedLastHour(AIndex : Integer; AValue : string); 

begin
  If (FleasedLastHour=AValue) then exit;
  FleasedLastHour:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueuestats.SetleasedLastMinute(AIndex : Integer; AValue : string); 

begin
  If (FleasedLastMinute=AValue) then exit;
  FleasedLastMinute:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueuestats.SetoldestTask(AIndex : Integer; AValue : string); 

begin
  If (FoldestTask=AValue) then exit;
  FoldestTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskQueuestats.SettotalTasks(AIndex : Integer; AValue : integer); 

begin
  If (FtotalTasks=AValue) then exit;
  FtotalTasks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTasks
  --------------------------------------------------------------------}


Procedure TTasks.Setitems(AIndex : Integer; AValue : TTasksitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTasks.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTasksitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTasks2
  --------------------------------------------------------------------}


Procedure TTasks2.Setitems(AIndex : Integer; AValue : TTasks2items); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTasks2.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTasks2items
  --------------------------------------------------------------------}




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
  Result:='https://www.googleapis.com/';
end;

Class Function TTaskqueueAPI.APIbasePath : string;

begin
  Result:='/taskqueue/v1beta2/projects/';
end;

Class Function TTaskqueueAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/taskqueue/v1beta2/projects/';
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
  TTaskQueue.RegisterObject;
  TTaskQueueacl.RegisterObject;
  TTaskQueueacladminEmails.RegisterObject;
  TTaskQueueaclconsumerEmails.RegisterObject;
  TTaskQueueaclproducerEmails.RegisterObject;
  TTaskQueuestats.RegisterObject;
  TTasks.RegisterObject;
  TTasksitems.RegisterObject;
  TTasks2.RegisterObject;
  TTasks2items.RegisterObject;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TTaskqueueAPI.RegisterAPI;
end.
