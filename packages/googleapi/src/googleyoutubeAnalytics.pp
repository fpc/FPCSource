unit googleyoutubeAnalytics;
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
//Generated on: 9-5-15 13:23:00
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TBatchReport = class;
  TBatchReportDefinition = class;
  TBatchReportDefinitionList = class;
  TBatchReportList = class;
  TGroup = class;
  TGroupItem = class;
  TGroupItemListResponse = class;
  TGroupListResponse = class;
  TResultTable = class;
  TBatchReportArray = Array of TBatchReport;
  TBatchReportDefinitionArray = Array of TBatchReportDefinition;
  TBatchReportDefinitionListArray = Array of TBatchReportDefinitionList;
  TBatchReportListArray = Array of TBatchReportList;
  TGroupArray = Array of TGroup;
  TGroupItemArray = Array of TGroupItem;
  TGroupItemListResponseArray = Array of TGroupItemListResponse;
  TGroupListResponseArray = Array of TGroupListResponse;
  TResultTableArray = Array of TResultTable;
  //Anonymous types, using auto-generated names
  TBatchReportTypeoutputsItem = class;
  TBatchReportTypetimeSpan = class;
  TGroupTypecontentDetails = class;
  TGroupTypesnippet = class;
  TGroupItemTyperesource = class;
  TResultTableTypecolumnHeadersItem = class;
  TBatchReportTypeoutputsArray = Array of TBatchReportTypeoutputsItem;
  TBatchReportDefinitionListTypeitemsArray = Array of TBatchReportDefinition;
  TBatchReportListTypeitemsArray = Array of TBatchReport;
  TGroupItemListResponseTypeitemsArray = Array of TGroupItem;
  TGroupListResponseTypeitemsArray = Array of TGroup;
  TResultTableTypecolumnHeadersArray = Array of TResultTableTypecolumnHeadersItem;
  TResultTableTyperowsArray = Array of TTJSONSchemaArray;
  
  { --------------------------------------------------------------------
    TBatchReportTypeoutputsItem
    --------------------------------------------------------------------}
  
  TBatchReportTypeoutputsItem = Class(TGoogleBaseObject)
  Private
    FdownloadUrl : String;
    Fformat : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdownloadUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property downloadUrl : String Index 0 Read FdownloadUrl Write SetdownloadUrl;
    Property format : String Index 8 Read Fformat Write Setformat;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TBatchReportTypeoutputsItemClass = Class of TBatchReportTypeoutputsItem;
  
  { --------------------------------------------------------------------
    TBatchReportTypetimeSpan
    --------------------------------------------------------------------}
  
  TBatchReportTypetimeSpan = Class(TGoogleBaseObject)
  Private
    FendTime : TDatetime;
    FstartTime : TDatetime;
  Protected
    //Property setters
    Procedure SetendTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property endTime : TDatetime Index 0 Read FendTime Write SetendTime;
    Property startTime : TDatetime Index 8 Read FstartTime Write SetstartTime;
  end;
  TBatchReportTypetimeSpanClass = Class of TBatchReportTypetimeSpan;
  
  { --------------------------------------------------------------------
    TBatchReport
    --------------------------------------------------------------------}
  
  TBatchReport = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Foutputs : TBatchReportTypeoutputsArray;
    FreportId : String;
    FtimeSpan : TBatchReportTypetimeSpan;
    FtimeUpdated : TDatetime;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setoutputs(AIndex : Integer; AValue : TBatchReportTypeoutputsArray); virtual;
    Procedure SetreportId(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeSpan(AIndex : Integer; AValue : TBatchReportTypetimeSpan); virtual;
    Procedure SettimeUpdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property outputs : TBatchReportTypeoutputsArray Index 16 Read Foutputs Write Setoutputs;
    Property reportId : String Index 24 Read FreportId Write SetreportId;
    Property timeSpan : TBatchReportTypetimeSpan Index 32 Read FtimeSpan Write SettimeSpan;
    Property timeUpdated : TDatetime Index 40 Read FtimeUpdated Write SettimeUpdated;
  end;
  TBatchReportClass = Class of TBatchReport;
  
  { --------------------------------------------------------------------
    TBatchReportDefinition
    --------------------------------------------------------------------}
  
  TBatchReportDefinition = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fname : String;
    Fstatus : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
    Property status : String Index 24 Read Fstatus Write Setstatus;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TBatchReportDefinitionClass = Class of TBatchReportDefinition;
  
  { --------------------------------------------------------------------
    TBatchReportDefinitionList
    --------------------------------------------------------------------}
  
  TBatchReportDefinitionList = Class(TGoogleBaseObject)
  Private
    Fitems : TBatchReportDefinitionListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBatchReportDefinitionListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TBatchReportDefinitionListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TBatchReportDefinitionListClass = Class of TBatchReportDefinitionList;
  
  { --------------------------------------------------------------------
    TBatchReportList
    --------------------------------------------------------------------}
  
  TBatchReportList = Class(TGoogleBaseObject)
  Private
    Fitems : TBatchReportListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBatchReportListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TBatchReportListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TBatchReportListClass = Class of TBatchReportList;
  
  { --------------------------------------------------------------------
    TGroupTypecontentDetails
    --------------------------------------------------------------------}
  
  TGroupTypecontentDetails = Class(TGoogleBaseObject)
  Private
    FitemCount : String;
    FitemType : String;
  Protected
    //Property setters
    Procedure SetitemCount(AIndex : Integer; AValue : String); virtual;
    Procedure SetitemType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property itemCount : String Index 0 Read FitemCount Write SetitemCount;
    Property itemType : String Index 8 Read FitemType Write SetitemType;
  end;
  TGroupTypecontentDetailsClass = Class of TGroupTypecontentDetails;
  
  { --------------------------------------------------------------------
    TGroupTypesnippet
    --------------------------------------------------------------------}
  
  TGroupTypesnippet = Class(TGoogleBaseObject)
  Private
    FpublishedAt : TDatetime;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property publishedAt : TDatetime Index 0 Read FpublishedAt Write SetpublishedAt;
    Property title : String Index 8 Read Ftitle Write Settitle;
  end;
  TGroupTypesnippetClass = Class of TGroupTypesnippet;
  
  { --------------------------------------------------------------------
    TGroup
    --------------------------------------------------------------------}
  
  TGroup = Class(TGoogleBaseObject)
  Private
    FcontentDetails : TGroupTypecontentDetails;
    Fetag : String;
    Fid : String;
    Fkind : String;
    Fsnippet : TGroupTypesnippet;
  Protected
    //Property setters
    Procedure SetcontentDetails(AIndex : Integer; AValue : TGroupTypecontentDetails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TGroupTypesnippet); virtual;
  Public
  Published
    Property contentDetails : TGroupTypecontentDetails Index 0 Read FcontentDetails Write SetcontentDetails;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property snippet : TGroupTypesnippet Index 32 Read Fsnippet Write Setsnippet;
  end;
  TGroupClass = Class of TGroup;
  
  { --------------------------------------------------------------------
    TGroupItemTyperesource
    --------------------------------------------------------------------}
  
  TGroupItemTyperesource = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TGroupItemTyperesourceClass = Class of TGroupItemTyperesource;
  
  { --------------------------------------------------------------------
    TGroupItem
    --------------------------------------------------------------------}
  
  TGroupItem = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    FgroupId : String;
    Fid : String;
    Fkind : String;
    Fresource : TGroupItemTyperesource;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure SetgroupId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setresource(AIndex : Integer; AValue : TGroupItemTyperesource); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property groupId : String Index 8 Read FgroupId Write SetgroupId;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property resource : TGroupItemTyperesource Index 32 Read Fresource Write Setresource;
  end;
  TGroupItemClass = Class of TGroupItem;
  
  { --------------------------------------------------------------------
    TGroupItemListResponse
    --------------------------------------------------------------------}
  
  TGroupItemListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TGroupItemListResponseTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TGroupItemListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TGroupItemListResponseTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TGroupItemListResponseClass = Class of TGroupItemListResponse;
  
  { --------------------------------------------------------------------
    TGroupListResponse
    --------------------------------------------------------------------}
  
  TGroupListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TGroupListResponseTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TGroupListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TGroupListResponseTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TGroupListResponseClass = Class of TGroupListResponse;
  
  { --------------------------------------------------------------------
    TResultTableTypecolumnHeadersItem
    --------------------------------------------------------------------}
  
  TResultTableTypecolumnHeadersItem = Class(TGoogleBaseObject)
  Private
    FcolumnType : String;
    FdataType : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetcolumnType(AIndex : Integer; AValue : String); virtual;
    Procedure SetdataType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property columnType : String Index 0 Read FcolumnType Write SetcolumnType;
    Property dataType : String Index 8 Read FdataType Write SetdataType;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TResultTableTypecolumnHeadersItemClass = Class of TResultTableTypecolumnHeadersItem;
  
  { --------------------------------------------------------------------
    TResultTable
    --------------------------------------------------------------------}
  
  TResultTable = Class(TGoogleBaseObject)
  Private
    FcolumnHeaders : TResultTableTypecolumnHeadersArray;
    Fkind : String;
    Frows : TResultTableTyperowsArray;
  Protected
    //Property setters
    Procedure SetcolumnHeaders(AIndex : Integer; AValue : TResultTableTypecolumnHeadersArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TResultTableTyperowsArray); virtual;
  Public
  Published
    Property columnHeaders : TResultTableTypecolumnHeadersArray Index 0 Read FcolumnHeaders Write SetcolumnHeaders;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property rows : TResultTableTyperowsArray Index 16 Read Frows Write Setrows;
  end;
  TResultTableClass = Class of TResultTable;
  
  { --------------------------------------------------------------------
    TBatchReportDefinitionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBatchReportDefinitionsResource, method List
  
  TBatchReportDefinitionsListOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  TBatchReportDefinitionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TBatchReportDefinitionList;
    Function List(AQuery : TBatchReportDefinitionslistOptions) : TBatchReportDefinitionList;
  end;
  
  
  { --------------------------------------------------------------------
    TBatchReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBatchReportsResource, method List
  
  TBatchReportsListOptions = Record
    batchReportDefinitionId : String;
    onBehalfOfContentOwner : String;
  end;
  
  TBatchReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TBatchReportList;
    Function List(AQuery : TBatchReportslistOptions) : TBatchReportList;
  end;
  
  
  { --------------------------------------------------------------------
    TGroupItemsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TGroupItemsResource, method Delete
  
  TGroupItemsDeleteOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TGroupItemsResource, method Insert
  
  TGroupItemsInsertOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TGroupItemsResource, method List
  
  TGroupItemsListOptions = Record
    groupId : String;
    onBehalfOfContentOwner : String;
  end;
  
  TGroupItemsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TGroupItemsdeleteOptions);
    Function Insert(aGroupItem : TGroupItem; AQuery : string  = '') : TGroupItem;
    Function Insert(aGroupItem : TGroupItem; AQuery : TGroupItemsinsertOptions) : TGroupItem;
    Function List(AQuery : string  = '') : TGroupItemListResponse;
    Function List(AQuery : TGroupItemslistOptions) : TGroupItemListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TGroupsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TGroupsResource, method Delete
  
  TGroupsDeleteOptions = Record
    id : String;
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TGroupsResource, method Insert
  
  TGroupsInsertOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TGroupsResource, method List
  
  TGroupsListOptions = Record
    id : String;
    mine : boolean;
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TGroupsResource, method Update
  
  TGroupsUpdateOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  TGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(AQuery : string  = '');
    Procedure Delete(AQuery : TGroupsdeleteOptions);
    Function Insert(aGroup : TGroup; AQuery : string  = '') : TGroup;
    Function Insert(aGroup : TGroup; AQuery : TGroupsinsertOptions) : TGroup;
    Function List(AQuery : string  = '') : TGroupListResponse;
    Function List(AQuery : TGroupslistOptions) : TGroupListResponse;
    Function Update(aGroup : TGroup; AQuery : string  = '') : TGroup;
    Function Update(aGroup : TGroup; AQuery : TGroupsupdateOptions) : TGroup;
  end;
  
  
  { --------------------------------------------------------------------
    TReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReportsResource, method Query
  
  TReportsQueryOptions = Record
    currency : String;
    dimensions : String;
    enddate : String;
    filters : String;
    ids : String;
    maxresults : integer;
    metrics : String;
    sort : String;
    startdate : String;
    startindex : integer;
  end;
  
  TReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Query(AQuery : string  = '') : TResultTable;
    Function Query(AQuery : TReportsqueryOptions) : TResultTable;
  end;
  
  
  { --------------------------------------------------------------------
    TYoutubeAnalyticsAPI
    --------------------------------------------------------------------}
  
  TYoutubeAnalyticsAPI = Class(TGoogleAPI)
  Private
    FBatchReportDefinitionsInstance : TBatchReportDefinitionsResource;
    FBatchReportsInstance : TBatchReportsResource;
    FGroupItemsInstance : TGroupItemsResource;
    FGroupsInstance : TGroupsResource;
    FReportsInstance : TReportsResource;
    Function GetBatchReportDefinitionsInstance : TBatchReportDefinitionsResource;virtual;
    Function GetBatchReportsInstance : TBatchReportsResource;virtual;
    Function GetGroupItemsInstance : TGroupItemsResource;virtual;
    Function GetGroupsInstance : TGroupsResource;virtual;
    Function GetReportsInstance : TReportsResource;virtual;
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
    Function CreateBatchReportDefinitionsResource(AOwner : TComponent) : TBatchReportDefinitionsResource;virtual;overload;
    Function CreateBatchReportDefinitionsResource : TBatchReportDefinitionsResource;virtual;overload;
    Function CreateBatchReportsResource(AOwner : TComponent) : TBatchReportsResource;virtual;overload;
    Function CreateBatchReportsResource : TBatchReportsResource;virtual;overload;
    Function CreateGroupItemsResource(AOwner : TComponent) : TGroupItemsResource;virtual;overload;
    Function CreateGroupItemsResource : TGroupItemsResource;virtual;overload;
    Function CreateGroupsResource(AOwner : TComponent) : TGroupsResource;virtual;overload;
    Function CreateGroupsResource : TGroupsResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TReportsResource;virtual;overload;
    Function CreateReportsResource : TReportsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property BatchReportDefinitionsResource : TBatchReportDefinitionsResource Read GetBatchReportDefinitionsInstance;
    Property BatchReportsResource : TBatchReportsResource Read GetBatchReportsInstance;
    Property GroupItemsResource : TGroupItemsResource Read GetGroupItemsInstance;
    Property GroupsResource : TGroupsResource Read GetGroupsInstance;
    Property ReportsResource : TReportsResource Read GetReportsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TBatchReportTypeoutputsItem
  --------------------------------------------------------------------}


Procedure TBatchReportTypeoutputsItem.SetdownloadUrl(AIndex : Integer; AValue : String); 

begin
  If (FdownloadUrl=AValue) then exit;
  FdownloadUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportTypeoutputsItem.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportTypeoutputsItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBatchReportTypeoutputsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TBatchReportTypetimeSpan
  --------------------------------------------------------------------}


Procedure TBatchReportTypetimeSpan.SetendTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportTypetimeSpan.SetstartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchReport
  --------------------------------------------------------------------}


Procedure TBatchReport.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReport.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReport.Setoutputs(AIndex : Integer; AValue : TBatchReportTypeoutputsArray); 

begin
  If (Foutputs=AValue) then exit;
  Foutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReport.SetreportId(AIndex : Integer; AValue : String); 

begin
  If (FreportId=AValue) then exit;
  FreportId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReport.SettimeSpan(AIndex : Integer; AValue : TBatchReportTypetimeSpan); 

begin
  If (FtimeSpan=AValue) then exit;
  FtimeSpan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReport.SettimeUpdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtimeUpdated=AValue) then exit;
  FtimeUpdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchReportDefinition
  --------------------------------------------------------------------}


Procedure TBatchReportDefinition.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinition.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinition.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinition.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinition.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBatchReportDefinition.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TBatchReportDefinitionList
  --------------------------------------------------------------------}


Procedure TBatchReportDefinitionList.Setitems(AIndex : Integer; AValue : TBatchReportDefinitionListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinitionList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchReportList
  --------------------------------------------------------------------}


Procedure TBatchReportList.Setitems(AIndex : Integer; AValue : TBatchReportListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupTypecontentDetails
  --------------------------------------------------------------------}


Procedure TGroupTypecontentDetails.SetitemCount(AIndex : Integer; AValue : String); 

begin
  If (FitemCount=AValue) then exit;
  FitemCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupTypecontentDetails.SetitemType(AIndex : Integer; AValue : String); 

begin
  If (FitemType=AValue) then exit;
  FitemType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupTypesnippet
  --------------------------------------------------------------------}


Procedure TGroupTypesnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupTypesnippet.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroup
  --------------------------------------------------------------------}


Procedure TGroup.SetcontentDetails(AIndex : Integer; AValue : TGroupTypecontentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setsnippet(AIndex : Integer; AValue : TGroupTypesnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupItemTyperesource
  --------------------------------------------------------------------}


Procedure TGroupItemTyperesource.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItemTyperesource.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupItem
  --------------------------------------------------------------------}


Procedure TGroupItem.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItem.SetgroupId(AIndex : Integer; AValue : String); 

begin
  If (FgroupId=AValue) then exit;
  FgroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItem.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItem.Setresource(AIndex : Integer; AValue : TGroupItemTyperesource); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupItemListResponse
  --------------------------------------------------------------------}


Procedure TGroupItemListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItemListResponse.Setitems(AIndex : Integer; AValue : TGroupItemListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItemListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupListResponse
  --------------------------------------------------------------------}


Procedure TGroupListResponse.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupListResponse.Setitems(AIndex : Integer; AValue : TGroupListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultTableTypecolumnHeadersItem
  --------------------------------------------------------------------}


Procedure TResultTableTypecolumnHeadersItem.SetcolumnType(AIndex : Integer; AValue : String); 

begin
  If (FcolumnType=AValue) then exit;
  FcolumnType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTableTypecolumnHeadersItem.SetdataType(AIndex : Integer; AValue : String); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTableTypecolumnHeadersItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultTable
  --------------------------------------------------------------------}


Procedure TResultTable.SetcolumnHeaders(AIndex : Integer; AValue : TResultTableTypecolumnHeadersArray); 

begin
  If (FcolumnHeaders=AValue) then exit;
  FcolumnHeaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTable.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTable.Setrows(AIndex : Integer; AValue : TResultTableTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchReportDefinitionsResource
  --------------------------------------------------------------------}


Class Function TBatchReportDefinitionsResource.ResourceName : String;

begin
  Result:='batchReportDefinitions';
end;

Class Function TBatchReportDefinitionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAnalyticsAPI;
end;

Function TBatchReportDefinitionsResource.List(AQuery : string = '') : TBatchReportDefinitionList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'batchReportDefinitions';
  _Methodid   = 'youtubeAnalytics.batchReportDefinitions.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TBatchReportDefinitionList) as TBatchReportDefinitionList;
end;


Function TBatchReportDefinitionsResource.List(AQuery : TBatchReportDefinitionslistOptions) : TBatchReportDefinitionList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TBatchReportsResource
  --------------------------------------------------------------------}


Class Function TBatchReportsResource.ResourceName : String;

begin
  Result:='batchReports';
end;

Class Function TBatchReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAnalyticsAPI;
end;

Function TBatchReportsResource.List(AQuery : string = '') : TBatchReportList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'batchReports';
  _Methodid   = 'youtubeAnalytics.batchReports.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TBatchReportList) as TBatchReportList;
end;


Function TBatchReportsResource.List(AQuery : TBatchReportslistOptions) : TBatchReportList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'batchReportDefinitionId',AQuery.batchReportDefinitionId);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TGroupItemsResource
  --------------------------------------------------------------------}


Class Function TGroupItemsResource.ResourceName : String;

begin
  Result:='groupItems';
end;

Class Function TGroupItemsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAnalyticsAPI;
end;

Procedure TGroupItemsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'groupItems';
  _Methodid   = 'youtubeAnalytics.groupItems.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TGroupItemsResource.Delete(AQuery : TGroupItemsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Delete(_Q);
end;

Function TGroupItemsResource.Insert(aGroupItem : TGroupItem; AQuery : string = '') : TGroupItem;

Const
  _HTTPMethod = 'POST';
  _Path       = 'groupItems';
  _Methodid   = 'youtubeAnalytics.groupItems.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aGroupItem,TGroupItem) as TGroupItem;
end;


Function TGroupItemsResource.Insert(aGroupItem : TGroupItem; AQuery : TGroupItemsinsertOptions) : TGroupItem;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=Insert(aGroupItem,_Q);
end;

Function TGroupItemsResource.List(AQuery : string = '') : TGroupItemListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'groupItems';
  _Methodid   = 'youtubeAnalytics.groupItems.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TGroupItemListResponse) as TGroupItemListResponse;
end;


Function TGroupItemsResource.List(AQuery : TGroupItemslistOptions) : TGroupItemListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'groupId',AQuery.groupId);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TGroupsResource
  --------------------------------------------------------------------}


Class Function TGroupsResource.ResourceName : String;

begin
  Result:='groups';
end;

Class Function TGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAnalyticsAPI;
end;

Procedure TGroupsResource.Delete(AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'groups';
  _Methodid   = 'youtubeAnalytics.groups.delete';

begin
  ServiceCall(_HTTPMethod,_Path,AQuery,Nil,Nil);
end;


Procedure TGroupsResource.Delete(AQuery : TGroupsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Delete(_Q);
end;

Function TGroupsResource.Insert(aGroup : TGroup; AQuery : string = '') : TGroup;

Const
  _HTTPMethod = 'POST';
  _Path       = 'groups';
  _Methodid   = 'youtubeAnalytics.groups.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aGroup,TGroup) as TGroup;
end;


Function TGroupsResource.Insert(aGroup : TGroup; AQuery : TGroupsinsertOptions) : TGroup;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=Insert(aGroup,_Q);
end;

Function TGroupsResource.List(AQuery : string = '') : TGroupListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'groups';
  _Methodid   = 'youtubeAnalytics.groups.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TGroupListResponse) as TGroupListResponse;
end;


Function TGroupsResource.List(AQuery : TGroupslistOptions) : TGroupListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'id',AQuery.id);
  AddToQuery(_Q,'mine',AQuery.mine);
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=List(_Q);
end;

Function TGroupsResource.Update(aGroup : TGroup; AQuery : string = '') : TGroup;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'groups';
  _Methodid   = 'youtubeAnalytics.groups.update';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aGroup,TGroup) as TGroup;
end;


Function TGroupsResource.Update(aGroup : TGroup; AQuery : TGroupsupdateOptions) : TGroup;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=Update(aGroup,_Q);
end;



{ --------------------------------------------------------------------
  TReportsResource
  --------------------------------------------------------------------}


Class Function TReportsResource.ResourceName : String;

begin
  Result:='reports';
end;

Class Function TReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubeAnalyticsAPI;
end;

Function TReportsResource.Query(AQuery : string = '') : TResultTable;

Const
  _HTTPMethod = 'GET';
  _Path       = 'reports';
  _Methodid   = 'youtubeAnalytics.reports.query';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TResultTable) as TResultTable;
end;


Function TReportsResource.Query(AQuery : TReportsqueryOptions) : TResultTable;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'currency',AQuery.currency);
  AddToQuery(_Q,'dimensions',AQuery.dimensions);
  AddToQuery(_Q,'end-date',AQuery.enddate);
  AddToQuery(_Q,'filters',AQuery.filters);
  AddToQuery(_Q,'ids',AQuery.ids);
  AddToQuery(_Q,'max-results',AQuery.maxresults);
  AddToQuery(_Q,'metrics',AQuery.metrics);
  AddToQuery(_Q,'sort',AQuery.sort);
  AddToQuery(_Q,'start-date',AQuery.startdate);
  AddToQuery(_Q,'start-index',AQuery.startindex);
  Result:=Query(_Q);
end;



{ --------------------------------------------------------------------
  TYoutubeAnalyticsAPI
  --------------------------------------------------------------------}

Class Function TYoutubeAnalyticsAPI.APIName : String;

begin
  Result:='youtubeAnalytics';
end;

Class Function TYoutubeAnalyticsAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TYoutubeAnalyticsAPI.APIRevision : String;

begin
  Result:='20150304';
end;

Class Function TYoutubeAnalyticsAPI.APIID : String;

begin
  Result:='youtubeAnalytics:v1';
end;

Class Function TYoutubeAnalyticsAPI.APITitle : String;

begin
  Result:='YouTube Analytics API';
end;

Class Function TYoutubeAnalyticsAPI.APIDescription : String;

begin
  Result:='Retrieve your YouTube Analytics reports.';
end;

Class Function TYoutubeAnalyticsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TYoutubeAnalyticsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TYoutubeAnalyticsAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/youtube-16.png';
end;

Class Function TYoutubeAnalyticsAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/youtube-32.png';
end;

Class Function TYoutubeAnalyticsAPI.APIdocumentationLink : String;

begin
  Result:='http://developers.google.com/youtube/analytics/';
end;

Class Function TYoutubeAnalyticsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TYoutubeAnalyticsAPI.APIbasePath : string;

begin
  Result:='/youtube/analytics/v1/';
end;

Class Function TYoutubeAnalyticsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/youtube/analytics/v1/';
end;

Class Function TYoutubeAnalyticsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TYoutubeAnalyticsAPI.APIservicePath : string;

begin
  Result:='youtube/analytics/v1/';
end;

Class Function TYoutubeAnalyticsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TYoutubeAnalyticsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,5);
  Result[0].Name:='https://www.googleapis.com/auth/youtube';
  Result[0].Description:='Manage your YouTube account';
  Result[1].Name:='https://www.googleapis.com/auth/youtube.readonly';
  Result[1].Description:='View your YouTube account';
  Result[2].Name:='https://www.googleapis.com/auth/youtubepartner';
  Result[2].Description:='View and manage your assets and associated content on YouTube';
  Result[3].Name:='https://www.googleapis.com/auth/yt-analytics-monetary.readonly';
  Result[3].Description:='View YouTube Analytics monetary reports for your YouTube content';
  Result[4].Name:='https://www.googleapis.com/auth/yt-analytics.readonly';
  Result[4].Description:='View YouTube Analytics reports for your YouTube content';
  
end;

Class Function TYoutubeAnalyticsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TYoutubeAnalyticsAPI.RegisterAPIResources;

begin
  TBatchReportTypeoutputsItem.RegisterObject;
  TBatchReportTypetimeSpan.RegisterObject;
  TBatchReport.RegisterObject;
  TBatchReportDefinition.RegisterObject;
  TBatchReportDefinitionList.RegisterObject;
  TBatchReportList.RegisterObject;
  TGroupTypecontentDetails.RegisterObject;
  TGroupTypesnippet.RegisterObject;
  TGroup.RegisterObject;
  TGroupItemTyperesource.RegisterObject;
  TGroupItem.RegisterObject;
  TGroupItemListResponse.RegisterObject;
  TGroupListResponse.RegisterObject;
  TResultTableTypecolumnHeadersItem.RegisterObject;
  TResultTable.RegisterObject;
end;


Function TYoutubeAnalyticsAPI.GetBatchReportDefinitionsInstance : TBatchReportDefinitionsResource;

begin
  if (FBatchReportDefinitionsInstance=Nil) then
    FBatchReportDefinitionsInstance:=CreateBatchReportDefinitionsResource;
  Result:=FBatchReportDefinitionsInstance;
end;

Function TYoutubeAnalyticsAPI.CreateBatchReportDefinitionsResource : TBatchReportDefinitionsResource;

begin
  Result:=CreateBatchReportDefinitionsResource(Self);
end;


Function TYoutubeAnalyticsAPI.CreateBatchReportDefinitionsResource(AOwner : TComponent) : TBatchReportDefinitionsResource;

begin
  Result:=TBatchReportDefinitionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAnalyticsAPI.GetBatchReportsInstance : TBatchReportsResource;

begin
  if (FBatchReportsInstance=Nil) then
    FBatchReportsInstance:=CreateBatchReportsResource;
  Result:=FBatchReportsInstance;
end;

Function TYoutubeAnalyticsAPI.CreateBatchReportsResource : TBatchReportsResource;

begin
  Result:=CreateBatchReportsResource(Self);
end;


Function TYoutubeAnalyticsAPI.CreateBatchReportsResource(AOwner : TComponent) : TBatchReportsResource;

begin
  Result:=TBatchReportsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAnalyticsAPI.GetGroupItemsInstance : TGroupItemsResource;

begin
  if (FGroupItemsInstance=Nil) then
    FGroupItemsInstance:=CreateGroupItemsResource;
  Result:=FGroupItemsInstance;
end;

Function TYoutubeAnalyticsAPI.CreateGroupItemsResource : TGroupItemsResource;

begin
  Result:=CreateGroupItemsResource(Self);
end;


Function TYoutubeAnalyticsAPI.CreateGroupItemsResource(AOwner : TComponent) : TGroupItemsResource;

begin
  Result:=TGroupItemsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAnalyticsAPI.GetGroupsInstance : TGroupsResource;

begin
  if (FGroupsInstance=Nil) then
    FGroupsInstance:=CreateGroupsResource;
  Result:=FGroupsInstance;
end;

Function TYoutubeAnalyticsAPI.CreateGroupsResource : TGroupsResource;

begin
  Result:=CreateGroupsResource(Self);
end;


Function TYoutubeAnalyticsAPI.CreateGroupsResource(AOwner : TComponent) : TGroupsResource;

begin
  Result:=TGroupsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TYoutubeAnalyticsAPI.GetReportsInstance : TReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TYoutubeAnalyticsAPI.CreateReportsResource : TReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TYoutubeAnalyticsAPI.CreateReportsResource(AOwner : TComponent) : TReportsResource;

begin
  Result:=TReportsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TYoutubeAnalyticsAPI.RegisterAPI;
end.
