unit googleyoutubeAnalytics;
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
  TBatchReport = class;
  TBatchReportArray = Array of TBatchReport;
  TBatchReportoutputs = class;
  TBatchReportoutputsArray = Array of TBatchReportoutputs;
  TBatchReporttimeSpan = class;
  TBatchReporttimeSpanArray = Array of TBatchReporttimeSpan;
  TBatchReportDefinition = class;
  TBatchReportDefinitionArray = Array of TBatchReportDefinition;
  TBatchReportDefinitionList = class;
  TBatchReportDefinitionListArray = Array of TBatchReportDefinitionList;
  TBatchReportDefinitionListitems = class;
  TBatchReportDefinitionListitemsArray = Array of TBatchReportDefinitionListitems;
  TBatchReportList = class;
  TBatchReportListArray = Array of TBatchReportList;
  TBatchReportListitems = class;
  TBatchReportListitemsArray = Array of TBatchReportListitems;
  TGroup = class;
  TGroupArray = Array of TGroup;
  TGroupcontentDetails = class;
  TGroupcontentDetailsArray = Array of TGroupcontentDetails;
  TGroupsnippet = class;
  TGroupsnippetArray = Array of TGroupsnippet;
  TGroupItem = class;
  TGroupItemArray = Array of TGroupItem;
  TGroupItemresource = class;
  TGroupItemresourceArray = Array of TGroupItemresource;
  TGroupItemListResponse = class;
  TGroupItemListResponseArray = Array of TGroupItemListResponse;
  TGroupItemListResponseitems = class;
  TGroupItemListResponseitemsArray = Array of TGroupItemListResponseitems;
  TGroupListResponse = class;
  TGroupListResponseArray = Array of TGroupListResponse;
  TGroupListResponseitems = class;
  TGroupListResponseitemsArray = Array of TGroupListResponseitems;
  TResultTable = class;
  TResultTableArray = Array of TResultTable;
  TResultTablecolumnHeaders = class;
  TResultTablecolumnHeadersArray = Array of TResultTablecolumnHeaders;
  TResultTablerows = class;
  TResultTablerowsArray = Array of TResultTablerows;
  
  { --------------------------------------------------------------------
    TBatchReport
    --------------------------------------------------------------------}
  
  TBatchReport = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Foutputs : TBatchReportoutputs;
    FreportId : string;
    FtimeSpan : TBatchReporttimeSpan;
    FtimeUpdated : TDatetime;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setoutputs(AIndex : Integer; AValue : TBatchReportoutputs); virtual;
    Procedure SetreportId(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeSpan(AIndex : Integer; AValue : TBatchReporttimeSpan); virtual;
    Procedure SettimeUpdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property outputs : TBatchReportoutputs Index 16 Read Foutputs Write Setoutputs;
    Property reportId : string Index 24 Read FreportId Write SetreportId;
    Property timeSpan : TBatchReporttimeSpan Index 32 Read FtimeSpan Write SettimeSpan;
    Property timeUpdated : TDatetime Index 40 Read FtimeUpdated Write SettimeUpdated;
  end;
  TBatchReportClass = Class of TBatchReport;
  
  { --------------------------------------------------------------------
    TBatchReportoutputs
    --------------------------------------------------------------------}
  
  TBatchReportoutputs = Class(TGoogleBaseObject)
  Private
    FdownloadUrl : string;
    Fformat : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdownloadUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setformat(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property downloadUrl : string Index 0 Read FdownloadUrl Write SetdownloadUrl;
    Property format : string Index 8 Read Fformat Write Setformat;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TBatchReportoutputsClass = Class of TBatchReportoutputs;
  
  { --------------------------------------------------------------------
    TBatchReporttimeSpan
    --------------------------------------------------------------------}
  
  TBatchReporttimeSpan = Class(TGoogleBaseObject)
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
  TBatchReporttimeSpanClass = Class of TBatchReporttimeSpan;
  
  { --------------------------------------------------------------------
    TBatchReportDefinition
    --------------------------------------------------------------------}
  
  TBatchReportDefinition = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fname : string;
    Fstatus : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
    Property status : string Index 24 Read Fstatus Write Setstatus;
    Property _type : string Index 32 Read F_type Write Set_type;
  end;
  TBatchReportDefinitionClass = Class of TBatchReportDefinition;
  
  { --------------------------------------------------------------------
    TBatchReportDefinitionList
    --------------------------------------------------------------------}
  
  TBatchReportDefinitionList = Class(TGoogleBaseObject)
  Private
    Fitems : TBatchReportDefinitionListitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBatchReportDefinitionListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TBatchReportDefinitionListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TBatchReportDefinitionListClass = Class of TBatchReportDefinitionList;
  
  { --------------------------------------------------------------------
    TBatchReportDefinitionListitems
    --------------------------------------------------------------------}
  
  TBatchReportDefinitionListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBatchReportDefinitionListitemsClass = Class of TBatchReportDefinitionListitems;
  
  { --------------------------------------------------------------------
    TBatchReportList
    --------------------------------------------------------------------}
  
  TBatchReportList = Class(TGoogleBaseObject)
  Private
    Fitems : TBatchReportListitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBatchReportListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TBatchReportListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TBatchReportListClass = Class of TBatchReportList;
  
  { --------------------------------------------------------------------
    TBatchReportListitems
    --------------------------------------------------------------------}
  
  TBatchReportListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBatchReportListitemsClass = Class of TBatchReportListitems;
  
  { --------------------------------------------------------------------
    TGroup
    --------------------------------------------------------------------}
  
  TGroup = Class(TGoogleBaseObject)
  Private
    FcontentDetails : TGroupcontentDetails;
    Fetag : string;
    Fid : string;
    Fkind : string;
    Fsnippet : TGroupsnippet;
  Protected
    //Property setters
    Procedure SetcontentDetails(AIndex : Integer; AValue : TGroupcontentDetails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : TGroupsnippet); virtual;
  Public
  Published
    Property contentDetails : TGroupcontentDetails Index 0 Read FcontentDetails Write SetcontentDetails;
    Property etag : string Index 8 Read Fetag Write Setetag;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property snippet : TGroupsnippet Index 32 Read Fsnippet Write Setsnippet;
  end;
  TGroupClass = Class of TGroup;
  
  { --------------------------------------------------------------------
    TGroupcontentDetails
    --------------------------------------------------------------------}
  
  TGroupcontentDetails = Class(TGoogleBaseObject)
  Private
    FitemCount : string;
    FitemType : string;
  Protected
    //Property setters
    Procedure SetitemCount(AIndex : Integer; AValue : string); virtual;
    Procedure SetitemType(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property itemCount : string Index 0 Read FitemCount Write SetitemCount;
    Property itemType : string Index 8 Read FitemType Write SetitemType;
  end;
  TGroupcontentDetailsClass = Class of TGroupcontentDetails;
  
  { --------------------------------------------------------------------
    TGroupsnippet
    --------------------------------------------------------------------}
  
  TGroupsnippet = Class(TGoogleBaseObject)
  Private
    FpublishedAt : TDatetime;
    Ftitle : string;
  Protected
    //Property setters
    Procedure SetpublishedAt(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property publishedAt : TDatetime Index 0 Read FpublishedAt Write SetpublishedAt;
    Property title : string Index 8 Read Ftitle Write Settitle;
  end;
  TGroupsnippetClass = Class of TGroupsnippet;
  
  { --------------------------------------------------------------------
    TGroupItem
    --------------------------------------------------------------------}
  
  TGroupItem = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    FgroupId : string;
    Fid : string;
    Fkind : string;
    Fresource : TGroupItemresource;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure SetgroupId(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setresource(AIndex : Integer; AValue : TGroupItemresource); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property groupId : string Index 8 Read FgroupId Write SetgroupId;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property resource : TGroupItemresource Index 32 Read Fresource Write Setresource;
  end;
  TGroupItemClass = Class of TGroupItem;
  
  { --------------------------------------------------------------------
    TGroupItemresource
    --------------------------------------------------------------------}
  
  TGroupItemresource = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TGroupItemresourceClass = Class of TGroupItemresource;
  
  { --------------------------------------------------------------------
    TGroupItemListResponse
    --------------------------------------------------------------------}
  
  TGroupItemListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TGroupItemListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TGroupItemListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TGroupItemListResponseitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TGroupItemListResponseClass = Class of TGroupItemListResponse;
  
  { --------------------------------------------------------------------
    TGroupItemListResponseitems
    --------------------------------------------------------------------}
  
  TGroupItemListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGroupItemListResponseitemsClass = Class of TGroupItemListResponseitems;
  
  { --------------------------------------------------------------------
    TGroupListResponse
    --------------------------------------------------------------------}
  
  TGroupListResponse = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TGroupListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TGroupListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TGroupListResponseitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TGroupListResponseClass = Class of TGroupListResponse;
  
  { --------------------------------------------------------------------
    TGroupListResponseitems
    --------------------------------------------------------------------}
  
  TGroupListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGroupListResponseitemsClass = Class of TGroupListResponseitems;
  
  { --------------------------------------------------------------------
    TResultTable
    --------------------------------------------------------------------}
  
  TResultTable = Class(TGoogleBaseObject)
  Private
    FcolumnHeaders : TResultTablecolumnHeaders;
    Fkind : string;
    Frows : TResultTablerows;
  Protected
    //Property setters
    Procedure SetcolumnHeaders(AIndex : Integer; AValue : TResultTablecolumnHeaders); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TResultTablerows); virtual;
  Public
  Published
    Property columnHeaders : TResultTablecolumnHeaders Index 0 Read FcolumnHeaders Write SetcolumnHeaders;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property rows : TResultTablerows Index 16 Read Frows Write Setrows;
  end;
  TResultTableClass = Class of TResultTable;
  
  { --------------------------------------------------------------------
    TResultTablecolumnHeaders
    --------------------------------------------------------------------}
  
  TResultTablecolumnHeaders = Class(TGoogleBaseObject)
  Private
    FcolumnType : string;
    FdataType : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetcolumnType(AIndex : Integer; AValue : string); virtual;
    Procedure SetdataType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property columnType : string Index 0 Read FcolumnType Write SetcolumnType;
    Property dataType : string Index 8 Read FdataType Write SetdataType;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TResultTablecolumnHeadersClass = Class of TResultTablecolumnHeaders;
  
  { --------------------------------------------------------------------
    TResultTablerows
    --------------------------------------------------------------------}
  
  TResultTablerows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TResultTablerowsClass = Class of TResultTablerows;
  
  { --------------------------------------------------------------------
    TBatchReportDefinitionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBatchReportDefinitionsResource, method List
  
  TBatchReportDefinitionsListOptions = Record
    onBehalfOfContentOwner : string;
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
    batchReportDefinitionId : string;
    onBehalfOfContentOwner : string;
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
    id : string;
    onBehalfOfContentOwner : string;
  end;
  
  
  //Optional query Options for TGroupItemsResource, method Insert
  
  TGroupItemsInsertOptions = Record
    onBehalfOfContentOwner : string;
  end;
  
  
  //Optional query Options for TGroupItemsResource, method List
  
  TGroupItemsListOptions = Record
    groupId : string;
    onBehalfOfContentOwner : string;
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
    id : string;
    onBehalfOfContentOwner : string;
  end;
  
  
  //Optional query Options for TGroupsResource, method Insert
  
  TGroupsInsertOptions = Record
    onBehalfOfContentOwner : string;
  end;
  
  
  //Optional query Options for TGroupsResource, method List
  
  TGroupsListOptions = Record
    id : string;
    mine : boolean;
    onBehalfOfContentOwner : string;
  end;
  
  
  //Optional query Options for TGroupsResource, method Update
  
  TGroupsUpdateOptions = Record
    onBehalfOfContentOwner : string;
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
    currency : string;
    dimensions : string;
    enddate : string;
    filters : string;
    ids : string;
    maxresults : integer;
    metrics : string;
    sort : string;
    startdate : string;
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
  TBatchReport
  --------------------------------------------------------------------}


Procedure TBatchReport.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReport.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReport.Setoutputs(AIndex : Integer; AValue : TBatchReportoutputs); 

begin
  If (Foutputs=AValue) then exit;
  Foutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReport.SetreportId(AIndex : Integer; AValue : string); 

begin
  If (FreportId=AValue) then exit;
  FreportId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReport.SettimeSpan(AIndex : Integer; AValue : TBatchReporttimeSpan); 

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
  TBatchReportoutputs
  --------------------------------------------------------------------}


Procedure TBatchReportoutputs.SetdownloadUrl(AIndex : Integer; AValue : string); 

begin
  If (FdownloadUrl=AValue) then exit;
  FdownloadUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportoutputs.Setformat(AIndex : Integer; AValue : string); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportoutputs.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBatchReportoutputs.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TBatchReporttimeSpan
  --------------------------------------------------------------------}


Procedure TBatchReporttimeSpan.SetendTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReporttimeSpan.SetstartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchReportDefinition
  --------------------------------------------------------------------}


Procedure TBatchReportDefinition.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinition.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinition.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinition.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinition.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TBatchReportDefinitionList.Setitems(AIndex : Integer; AValue : TBatchReportDefinitionListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportDefinitionList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchReportDefinitionListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBatchReportList
  --------------------------------------------------------------------}


Procedure TBatchReportList.Setitems(AIndex : Integer; AValue : TBatchReportListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchReportList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchReportListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGroup
  --------------------------------------------------------------------}


Procedure TGroup.SetcontentDetails(AIndex : Integer; AValue : TGroupcontentDetails); 

begin
  If (FcontentDetails=AValue) then exit;
  FcontentDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setsnippet(AIndex : Integer; AValue : TGroupsnippet); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupcontentDetails
  --------------------------------------------------------------------}


Procedure TGroupcontentDetails.SetitemCount(AIndex : Integer; AValue : string); 

begin
  If (FitemCount=AValue) then exit;
  FitemCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupcontentDetails.SetitemType(AIndex : Integer; AValue : string); 

begin
  If (FitemType=AValue) then exit;
  FitemType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupsnippet
  --------------------------------------------------------------------}


Procedure TGroupsnippet.SetpublishedAt(AIndex : Integer; AValue : TDatetime); 

begin
  If (FpublishedAt=AValue) then exit;
  FpublishedAt:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupsnippet.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupItem
  --------------------------------------------------------------------}


Procedure TGroupItem.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItem.SetgroupId(AIndex : Integer; AValue : string); 

begin
  If (FgroupId=AValue) then exit;
  FgroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItem.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItem.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItem.Setresource(AIndex : Integer; AValue : TGroupItemresource); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupItemresource
  --------------------------------------------------------------------}


Procedure TGroupItemresource.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItemresource.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupItemListResponse
  --------------------------------------------------------------------}


Procedure TGroupItemListResponse.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItemListResponse.Setitems(AIndex : Integer; AValue : TGroupItemListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupItemListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupItemListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGroupListResponse
  --------------------------------------------------------------------}


Procedure TGroupListResponse.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupListResponse.Setitems(AIndex : Integer; AValue : TGroupListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TResultTable
  --------------------------------------------------------------------}


Procedure TResultTable.SetcolumnHeaders(AIndex : Integer; AValue : TResultTablecolumnHeaders); 

begin
  If (FcolumnHeaders=AValue) then exit;
  FcolumnHeaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTable.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTable.Setrows(AIndex : Integer; AValue : TResultTablerows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultTablecolumnHeaders
  --------------------------------------------------------------------}


Procedure TResultTablecolumnHeaders.SetcolumnType(AIndex : Integer; AValue : string); 

begin
  If (FcolumnType=AValue) then exit;
  FcolumnType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTablecolumnHeaders.SetdataType(AIndex : Integer; AValue : string); 

begin
  If (FdataType=AValue) then exit;
  FdataType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTablecolumnHeaders.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultTablerows
  --------------------------------------------------------------------}




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
  TBatchReport.RegisterObject;
  TBatchReportoutputs.RegisterObject;
  TBatchReporttimeSpan.RegisterObject;
  TBatchReportDefinition.RegisterObject;
  TBatchReportDefinitionList.RegisterObject;
  TBatchReportDefinitionListitems.RegisterObject;
  TBatchReportList.RegisterObject;
  TBatchReportListitems.RegisterObject;
  TGroup.RegisterObject;
  TGroupcontentDetails.RegisterObject;
  TGroupsnippet.RegisterObject;
  TGroupItem.RegisterObject;
  TGroupItemresource.RegisterObject;
  TGroupItemListResponse.RegisterObject;
  TGroupItemListResponseitems.RegisterObject;
  TGroupListResponse.RegisterObject;
  TGroupListResponseitems.RegisterObject;
  TResultTable.RegisterObject;
  TResultTablecolumnHeaders.RegisterObject;
  TResultTablerows.RegisterObject;
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
