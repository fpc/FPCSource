unit googletasks;
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
//Generated on: 9-5-15 13:22:59
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TTask = class;
  TTaskList = class;
  TTaskLists = class;
  TTasks = class;
  TTaskArray = Array of TTask;
  TTaskListArray = Array of TTaskList;
  TTaskListsArray = Array of TTaskLists;
  TTasksArray = Array of TTasks;
  //Anonymous types, using auto-generated names
  TTaskTypelinksItem = class;
  TTaskTypelinksArray = Array of TTaskTypelinksItem;
  TTaskListsTypeitemsArray = Array of TTaskList;
  TTasksTypeitemsArray = Array of TTask;
  
  { --------------------------------------------------------------------
    TTaskTypelinksItem
    --------------------------------------------------------------------}
  
  TTaskTypelinksItem = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Flink : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setlink(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property link : String Index 8 Read Flink Write Setlink;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TTaskTypelinksItemClass = Class of TTaskTypelinksItem;
  
  { --------------------------------------------------------------------
    TTask
    --------------------------------------------------------------------}
  
  TTask = Class(TGoogleBaseObject)
  Private
    Fcompleted : TDatetime;
    Fdeleted : boolean;
    Fdue : TDatetime;
    Fetag : String;
    Fhidden : boolean;
    Fid : String;
    Fkind : String;
    Flinks : TTaskTypelinksArray;
    Fnotes : String;
    Fparent : String;
    Fposition : String;
    FselfLink : String;
    Fstatus : String;
    Ftitle : String;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setcompleted(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setdue(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Sethidden(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlinks(AIndex : Integer; AValue : TTaskTypelinksArray); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure Setparent(AIndex : Integer; AValue : String); virtual;
    Procedure Setposition(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property completed : TDatetime Index 0 Read Fcompleted Write Setcompleted;
    Property deleted : boolean Index 8 Read Fdeleted Write Setdeleted;
    Property due : TDatetime Index 16 Read Fdue Write Setdue;
    Property etag : String Index 24 Read Fetag Write Setetag;
    Property hidden : boolean Index 32 Read Fhidden Write Sethidden;
    Property id : String Index 40 Read Fid Write Setid;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property links : TTaskTypelinksArray Index 56 Read Flinks Write Setlinks;
    Property notes : String Index 64 Read Fnotes Write Setnotes;
    Property parent : String Index 72 Read Fparent Write Setparent;
    Property position : String Index 80 Read Fposition Write Setposition;
    Property selfLink : String Index 88 Read FselfLink Write SetselfLink;
    Property status : String Index 96 Read Fstatus Write Setstatus;
    Property title : String Index 104 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 112 Read Fupdated Write Setupdated;
  end;
  TTaskClass = Class of TTask;
  
  { --------------------------------------------------------------------
    TTaskList
    --------------------------------------------------------------------}
  
  TTaskList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fkind : String;
    FselfLink : String;
    Ftitle : String;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property selfLink : String Index 24 Read FselfLink Write SetselfLink;
    Property title : String Index 32 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 40 Read Fupdated Write Setupdated;
  end;
  TTaskListClass = Class of TTaskList;
  
  { --------------------------------------------------------------------
    TTaskLists
    --------------------------------------------------------------------}
  
  TTaskLists = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TTaskListsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTaskListsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TTaskListsTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TTaskListsClass = Class of TTaskLists;
  
  { --------------------------------------------------------------------
    TTasks
    --------------------------------------------------------------------}
  
  TTasks = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TTasksTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTasksTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TTasksTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TTasksClass = Class of TTasks;
  
  { --------------------------------------------------------------------
    TTasklistsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTasklistsResource, method List
  
  TTasklistsListOptions = Record
    maxResults : int64;
    pageToken : String;
  end;
  
  TTasklistsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(tasklist: string);
    Function Get(tasklist: string) : TTaskList;
    Function Insert(aTaskList : TTaskList) : TTaskList;
    Function List(AQuery : string  = '') : TTaskLists;
    Function List(AQuery : TTasklistslistOptions) : TTaskLists;
    Function Patch(tasklist: string; aTaskList : TTaskList) : TTaskList;
    Function Update(tasklist: string; aTaskList : TTaskList) : TTaskList;
  end;
  
  
  { --------------------------------------------------------------------
    TTasksResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTasksResource, method Insert
  
  TTasksInsertOptions = Record
    parent : String;
    previous : String;
  end;
  
  
  //Optional query Options for TTasksResource, method List
  
  TTasksListOptions = Record
    completedMax : String;
    completedMin : String;
    dueMax : String;
    dueMin : String;
    maxResults : int64;
    pageToken : String;
    showCompleted : boolean;
    showDeleted : boolean;
    showHidden : boolean;
    updatedMin : String;
  end;
  
  
  //Optional query Options for TTasksResource, method Move
  
  TTasksMoveOptions = Record
    parent : String;
    previous : String;
  end;
  
  TTasksResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Clear(tasklist: string);
    Procedure Delete(task: string; tasklist: string);
    Function Get(task: string; tasklist: string) : TTask;
    Function Insert(tasklist: string; aTask : TTask; AQuery : string  = '') : TTask;
    Function Insert(tasklist: string; aTask : TTask; AQuery : TTasksinsertOptions) : TTask;
    Function List(tasklist: string; AQuery : string  = '') : TTasks;
    Function List(tasklist: string; AQuery : TTaskslistOptions) : TTasks;
    Function Move(task: string; tasklist: string; AQuery : string  = '') : TTask;
    Function Move(task: string; tasklist: string; AQuery : TTasksmoveOptions) : TTask;
    Function Patch(task: string; tasklist: string; aTask : TTask) : TTask;
    Function Update(task: string; tasklist: string; aTask : TTask) : TTask;
  end;
  
  
  { --------------------------------------------------------------------
    TTasksAPI
    --------------------------------------------------------------------}
  
  TTasksAPI = Class(TGoogleAPI)
  Private
    FTasklistsInstance : TTasklistsResource;
    FTasksInstance : TTasksResource;
    Function GetTasklistsInstance : TTasklistsResource;virtual;
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
    Function CreateTasklistsResource(AOwner : TComponent) : TTasklistsResource;virtual;overload;
    Function CreateTasklistsResource : TTasklistsResource;virtual;overload;
    Function CreateTasksResource(AOwner : TComponent) : TTasksResource;virtual;overload;
    Function CreateTasksResource : TTasksResource;virtual;overload;
    //Add default on-demand instances for resources
    Property TasklistsResource : TTasklistsResource Read GetTasklistsInstance;
    Property TasksResource : TTasksResource Read GetTasksInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TTaskTypelinksItem
  --------------------------------------------------------------------}


Procedure TTaskTypelinksItem.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskTypelinksItem.Setlink(AIndex : Integer; AValue : String); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskTypelinksItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTaskTypelinksItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TTask
  --------------------------------------------------------------------}


Procedure TTask.Setcompleted(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcompleted=AValue) then exit;
  Fcompleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setdeleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setdue(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fdue=AValue) then exit;
  Fdue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Sethidden(AIndex : Integer; AValue : boolean); 

begin
  If (Fhidden=AValue) then exit;
  Fhidden:=AValue;
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



Procedure TTask.Setlinks(AIndex : Integer; AValue : TTaskTypelinksArray); 

begin
  If (Flinks=AValue) then exit;
  Flinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setparent(AIndex : Integer; AValue : String); 

begin
  If (Fparent=AValue) then exit;
  Fparent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setposition(AIndex : Integer; AValue : String); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskList
  --------------------------------------------------------------------}


Procedure TTaskList.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskLists
  --------------------------------------------------------------------}


Procedure TTaskLists.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskLists.Setitems(AIndex : Integer; AValue : TTaskListsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskLists.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskLists.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTasks
  --------------------------------------------------------------------}


Procedure TTasks.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



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



Procedure TTasks.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTasklistsResource
  --------------------------------------------------------------------}


Class Function TTasklistsResource.ResourceName : String;

begin
  Result:='tasklists';
end;

Class Function TTasklistsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtasksAPI;
end;

Procedure TTasklistsResource.Delete(tasklist: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'users/@me/lists/{tasklist}';
  _Methodid   = 'tasks.tasklists.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tasklist',tasklist]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTasklistsResource.Get(tasklist: string) : TTaskList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/@me/lists/{tasklist}';
  _Methodid   = 'tasks.tasklists.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tasklist',tasklist]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTaskList) as TTaskList;
end;

Function TTasklistsResource.Insert(aTaskList : TTaskList) : TTaskList;

Const
  _HTTPMethod = 'POST';
  _Path       = 'users/@me/lists';
  _Methodid   = 'tasks.tasklists.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aTaskList,TTaskList) as TTaskList;
end;

Function TTasklistsResource.List(AQuery : string = '') : TTaskLists;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/@me/lists';
  _Methodid   = 'tasks.tasklists.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TTaskLists) as TTaskLists;
end;


Function TTasklistsResource.List(AQuery : TTasklistslistOptions) : TTaskLists;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;

Function TTasklistsResource.Patch(tasklist: string; aTaskList : TTaskList) : TTaskList;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'users/@me/lists/{tasklist}';
  _Methodid   = 'tasks.tasklists.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tasklist',tasklist]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTaskList,TTaskList) as TTaskList;
end;

Function TTasklistsResource.Update(tasklist: string; aTaskList : TTaskList) : TTaskList;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'users/@me/lists/{tasklist}';
  _Methodid   = 'tasks.tasklists.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tasklist',tasklist]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTaskList,TTaskList) as TTaskList;
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
  Result:=TtasksAPI;
end;

Procedure TTasksResource.Clear(tasklist: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'lists/{tasklist}/clear';
  _Methodid   = 'tasks.tasks.clear';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tasklist',tasklist]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TTasksResource.Delete(task: string; tasklist: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'lists/{tasklist}/tasks/{task}';
  _Methodid   = 'tasks.tasks.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['task',task,'tasklist',tasklist]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTasksResource.Get(task: string; tasklist: string) : TTask;

Const
  _HTTPMethod = 'GET';
  _Path       = 'lists/{tasklist}/tasks/{task}';
  _Methodid   = 'tasks.tasks.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['task',task,'tasklist',tasklist]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTask) as TTask;
end;

Function TTasksResource.Insert(tasklist: string; aTask : TTask; AQuery : string = '') : TTask;

Const
  _HTTPMethod = 'POST';
  _Path       = 'lists/{tasklist}/tasks';
  _Methodid   = 'tasks.tasks.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tasklist',tasklist]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTask,TTask) as TTask;
end;


Function TTasksResource.Insert(tasklist: string; aTask : TTask; AQuery : TTasksinsertOptions) : TTask;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'parent',AQuery.parent);
  AddToQuery(_Q,'previous',AQuery.previous);
  Result:=Insert(tasklist,aTask,_Q);
end;

Function TTasksResource.List(tasklist: string; AQuery : string = '') : TTasks;

Const
  _HTTPMethod = 'GET';
  _Path       = 'lists/{tasklist}/tasks';
  _Methodid   = 'tasks.tasks.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['tasklist',tasklist]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTasks) as TTasks;
end;


Function TTasksResource.List(tasklist: string; AQuery : TTaskslistOptions) : TTasks;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'completedMax',AQuery.completedMax);
  AddToQuery(_Q,'completedMin',AQuery.completedMin);
  AddToQuery(_Q,'dueMax',AQuery.dueMax);
  AddToQuery(_Q,'dueMin',AQuery.dueMin);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'showCompleted',AQuery.showCompleted);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'showHidden',AQuery.showHidden);
  AddToQuery(_Q,'updatedMin',AQuery.updatedMin);
  Result:=List(tasklist,_Q);
end;

Function TTasksResource.Move(task: string; tasklist: string; AQuery : string = '') : TTask;

Const
  _HTTPMethod = 'POST';
  _Path       = 'lists/{tasklist}/tasks/{task}/move';
  _Methodid   = 'tasks.tasks.move';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['task',task,'tasklist',tasklist]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTask) as TTask;
end;


Function TTasksResource.Move(task: string; tasklist: string; AQuery : TTasksmoveOptions) : TTask;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'parent',AQuery.parent);
  AddToQuery(_Q,'previous',AQuery.previous);
  Result:=Move(task,tasklist,_Q);
end;

Function TTasksResource.Patch(task: string; tasklist: string; aTask : TTask) : TTask;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'lists/{tasklist}/tasks/{task}';
  _Methodid   = 'tasks.tasks.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['task',task,'tasklist',tasklist]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTask,TTask) as TTask;
end;

Function TTasksResource.Update(task: string; tasklist: string; aTask : TTask) : TTask;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'lists/{tasklist}/tasks/{task}';
  _Methodid   = 'tasks.tasks.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['task',task,'tasklist',tasklist]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTask,TTask) as TTask;
end;



{ --------------------------------------------------------------------
  TTasksAPI
  --------------------------------------------------------------------}

Class Function TTasksAPI.APIName : String;

begin
  Result:='tasks';
end;

Class Function TTasksAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TTasksAPI.APIRevision : String;

begin
  Result:='20141121';
end;

Class Function TTasksAPI.APIID : String;

begin
  Result:='tasks:v1';
end;

Class Function TTasksAPI.APITitle : String;

begin
  Result:='Tasks API';
end;

Class Function TTasksAPI.APIDescription : String;

begin
  Result:='Lets you manage your tasks and task lists.';
end;

Class Function TTasksAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TTasksAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TTasksAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/tasks-16.png';
end;

Class Function TTasksAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/tasks-32.png';
end;

Class Function TTasksAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/google-apps/tasks/firstapp';
end;

Class Function TTasksAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TTasksAPI.APIbasePath : string;

begin
  Result:='/tasks/v1/';
end;

Class Function TTasksAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/tasks/v1/';
end;

Class Function TTasksAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TTasksAPI.APIservicePath : string;

begin
  Result:='tasks/v1/';
end;

Class Function TTasksAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TTasksAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/tasks';
  Result[0].Description:='Manage your tasks';
  Result[1].Name:='https://www.googleapis.com/auth/tasks.readonly';
  Result[1].Description:='View your tasks';
  
end;

Class Function TTasksAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TTasksAPI.RegisterAPIResources;

begin
  TTaskTypelinksItem.RegisterObject;
  TTask.RegisterObject;
  TTaskList.RegisterObject;
  TTaskLists.RegisterObject;
  TTasks.RegisterObject;
end;


Function TTasksAPI.GetTasklistsInstance : TTasklistsResource;

begin
  if (FTasklistsInstance=Nil) then
    FTasklistsInstance:=CreateTasklistsResource;
  Result:=FTasklistsInstance;
end;

Function TTasksAPI.CreateTasklistsResource : TTasklistsResource;

begin
  Result:=CreateTasklistsResource(Self);
end;


Function TTasksAPI.CreateTasklistsResource(AOwner : TComponent) : TTasklistsResource;

begin
  Result:=TTasklistsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TTasksAPI.GetTasksInstance : TTasksResource;

begin
  if (FTasksInstance=Nil) then
    FTasksInstance:=CreateTasksResource;
  Result:=FTasksInstance;
end;

Function TTasksAPI.CreateTasksResource : TTasksResource;

begin
  Result:=CreateTasksResource(Self);
end;


Function TTasksAPI.CreateTasksResource(AOwner : TComponent) : TTasksResource;

begin
  Result:=TTasksResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TTasksAPI.RegisterAPI;
end.
