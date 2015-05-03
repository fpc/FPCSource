unit googletasks;
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
  TTasklinks = class;
  TTasklinksArray = Array of TTasklinks;
  TTaskList = class;
  TTaskListArray = Array of TTaskList;
  TTaskLists = class;
  TTaskListsArray = Array of TTaskLists;
  TTaskListsitems = class;
  TTaskListsitemsArray = Array of TTaskListsitems;
  TTasks = class;
  TTasksArray = Array of TTasks;
  TTasksitems = class;
  TTasksitemsArray = Array of TTasksitems;
  
  { --------------------------------------------------------------------
    TTask
    --------------------------------------------------------------------}
  
  TTask = Class(TGoogleBaseObject)
  Private
    Fcompleted : TDatetime;
    Fdeleted : boolean;
    Fdue : TDatetime;
    Fetag : string;
    Fhidden : boolean;
    Fid : string;
    Fkind : string;
    Flinks : TTasklinks;
    Fnotes : string;
    Fparent : string;
    Fposition : string;
    FselfLink : string;
    Fstatus : string;
    Ftitle : string;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setcompleted(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setdue(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Sethidden(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlinks(AIndex : Integer; AValue : TTasklinks); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : string); virtual;
    Procedure Setparent(AIndex : Integer; AValue : string); virtual;
    Procedure Setposition(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property completed : TDatetime Index 0 Read Fcompleted Write Setcompleted;
    Property deleted : boolean Index 8 Read Fdeleted Write Setdeleted;
    Property due : TDatetime Index 16 Read Fdue Write Setdue;
    Property etag : string Index 24 Read Fetag Write Setetag;
    Property hidden : boolean Index 32 Read Fhidden Write Sethidden;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property links : TTasklinks Index 56 Read Flinks Write Setlinks;
    Property notes : string Index 64 Read Fnotes Write Setnotes;
    Property parent : string Index 72 Read Fparent Write Setparent;
    Property position : string Index 80 Read Fposition Write Setposition;
    Property selfLink : string Index 88 Read FselfLink Write SetselfLink;
    Property status : string Index 96 Read Fstatus Write Setstatus;
    Property title : string Index 104 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 112 Read Fupdated Write Setupdated;
  end;
  TTaskClass = Class of TTask;
  
  { --------------------------------------------------------------------
    TTasklinks
    --------------------------------------------------------------------}
  
  TTasklinks = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Flink : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setlink(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property link : string Index 8 Read Flink Write Setlink;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TTasklinksClass = Class of TTasklinks;
  
  { --------------------------------------------------------------------
    TTaskList
    --------------------------------------------------------------------}
  
  TTaskList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fid : string;
    Fkind : string;
    FselfLink : string;
    Ftitle : string;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
    Property title : string Index 32 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 40 Read Fupdated Write Setupdated;
  end;
  TTaskListClass = Class of TTaskList;
  
  { --------------------------------------------------------------------
    TTaskLists
    --------------------------------------------------------------------}
  
  TTaskLists = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TTaskListsitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTaskListsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TTaskListsitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TTaskListsClass = Class of TTaskLists;
  
  { --------------------------------------------------------------------
    TTaskListsitems
    --------------------------------------------------------------------}
  
  TTaskListsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTaskListsitemsClass = Class of TTaskListsitems;
  
  { --------------------------------------------------------------------
    TTasks
    --------------------------------------------------------------------}
  
  TTasks = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TTasksitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTasksitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TTasksitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
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
    TTasklistsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTasklistsResource, method List
  
  TTasklistsListOptions = Record
    maxResults : int64;
    pageToken : string;
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
    parent : string;
    previous : string;
  end;
  
  
  //Optional query Options for TTasksResource, method List
  
  TTasksListOptions = Record
    completedMax : string;
    completedMin : string;
    dueMax : string;
    dueMin : string;
    maxResults : int64;
    pageToken : string;
    showCompleted : boolean;
    showDeleted : boolean;
    showHidden : boolean;
    updatedMin : string;
  end;
  
  
  //Optional query Options for TTasksResource, method Move
  
  TTasksMoveOptions = Record
    parent : string;
    previous : string;
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



Procedure TTask.Setetag(AIndex : Integer; AValue : string); 

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



Procedure TTask.Setlinks(AIndex : Integer; AValue : TTasklinks); 

begin
  If (Flinks=AValue) then exit;
  Flinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setnotes(AIndex : Integer; AValue : string); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setparent(AIndex : Integer; AValue : string); 

begin
  If (Fparent=AValue) then exit;
  Fparent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setposition(AIndex : Integer; AValue : string); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTask.Settitle(AIndex : Integer; AValue : string); 

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
  TTasklinks
  --------------------------------------------------------------------}


Procedure TTasklinks.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTasklinks.Setlink(AIndex : Integer; AValue : string); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTasklinks.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTasklinks.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TTaskList
  --------------------------------------------------------------------}


Procedure TTaskList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskList.Settitle(AIndex : Integer; AValue : string); 

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


Procedure TTaskLists.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskLists.Setitems(AIndex : Integer; AValue : TTaskListsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskLists.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskLists.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskListsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTasks
  --------------------------------------------------------------------}


Procedure TTasks.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



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



Procedure TTasks.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTasksitems
  --------------------------------------------------------------------}




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
  TTask.RegisterObject;
  TTasklinks.RegisterObject;
  TTaskList.RegisterObject;
  TTaskLists.RegisterObject;
  TTaskListsitems.RegisterObject;
  TTasks.RegisterObject;
  TTasksitems.RegisterObject;
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
