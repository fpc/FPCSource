unit googlepagespeedonline;
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
  TPagespeedApiFormatStringV2 = class;
  TPagespeedApiFormatStringV2Array = Array of TPagespeedApiFormatStringV2;
  TPagespeedApiFormatStringV2args = class;
  TPagespeedApiFormatStringV2argsArray = Array of TPagespeedApiFormatStringV2args;
  TPagespeedApiFormatStringV2argsrects = class;
  TPagespeedApiFormatStringV2argsrectsArray = Array of TPagespeedApiFormatStringV2argsrects;
  TPagespeedApiFormatStringV2argssecondary_rects = class;
  TPagespeedApiFormatStringV2argssecondary_rectsArray = Array of TPagespeedApiFormatStringV2argssecondary_rects;
  TPagespeedApiImageV2 = class;
  TPagespeedApiImageV2Array = Array of TPagespeedApiImageV2;
  TPagespeedApiImageV2page_rect = class;
  TPagespeedApiImageV2page_rectArray = Array of TPagespeedApiImageV2page_rect;
  TResult = class;
  TResultArray = Array of TResult;
  TResultformattedResults = class;
  TResultformattedResultsArray = Array of TResultformattedResults;
  TResultformattedResultsruleResults = class;
  TResultformattedResultsruleResultsArray = Array of TResultformattedResultsruleResults;
  TResultinvalidRules = class;
  TResultinvalidRulesArray = Array of TResultinvalidRules;
  TResultpageStats = class;
  TResultpageStatsArray = Array of TResultpageStats;
  TResultruleGroups = class;
  TResultruleGroupsArray = Array of TResultruleGroups;
  TResultversion = class;
  TResultversionArray = Array of TResultversion;
  
  { --------------------------------------------------------------------
    TPagespeedApiFormatStringV2
    --------------------------------------------------------------------}
  
  TPagespeedApiFormatStringV2 = Class(TGoogleBaseObject)
  Private
    Fargs : TPagespeedApiFormatStringV2args;
    Fformat : string;
  Protected
    //Property setters
    Procedure Setargs(AIndex : Integer; AValue : TPagespeedApiFormatStringV2args); virtual;
    Procedure Setformat(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property args : TPagespeedApiFormatStringV2args Index 0 Read Fargs Write Setargs;
    Property format : string Index 8 Read Fformat Write Setformat;
  end;
  TPagespeedApiFormatStringV2Class = Class of TPagespeedApiFormatStringV2;
  
  { --------------------------------------------------------------------
    TPagespeedApiFormatStringV2args
    --------------------------------------------------------------------}
  
  TPagespeedApiFormatStringV2args = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Frects : TPagespeedApiFormatStringV2argsrects;
    Fsecondary_rects : TPagespeedApiFormatStringV2argssecondary_rects;
    F_type : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setrects(AIndex : Integer; AValue : TPagespeedApiFormatStringV2argsrects); virtual;
    Procedure Setsecondary_rects(AIndex : Integer; AValue : TPagespeedApiFormatStringV2argssecondary_rects); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property rects : TPagespeedApiFormatStringV2argsrects Index 8 Read Frects Write Setrects;
    Property secondary_rects : TPagespeedApiFormatStringV2argssecondary_rects Index 16 Read Fsecondary_rects Write Setsecondary_rects;
    Property _type : string Index 24 Read F_type Write Set_type;
    Property value : string Index 32 Read Fvalue Write Setvalue;
  end;
  TPagespeedApiFormatStringV2argsClass = Class of TPagespeedApiFormatStringV2args;
  
  { --------------------------------------------------------------------
    TPagespeedApiFormatStringV2argsrects
    --------------------------------------------------------------------}
  
  TPagespeedApiFormatStringV2argsrects = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Fleft : integer;
    Ftop : integer;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setleft(AIndex : Integer; AValue : integer); virtual;
    Procedure Settop(AIndex : Integer; AValue : integer); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property left : integer Index 8 Read Fleft Write Setleft;
    Property top : integer Index 16 Read Ftop Write Settop;
    Property width : integer Index 24 Read Fwidth Write Setwidth;
  end;
  TPagespeedApiFormatStringV2argsrectsClass = Class of TPagespeedApiFormatStringV2argsrects;
  
  { --------------------------------------------------------------------
    TPagespeedApiFormatStringV2argssecondary_rects
    --------------------------------------------------------------------}
  
  TPagespeedApiFormatStringV2argssecondary_rects = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Fleft : integer;
    Ftop : integer;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setleft(AIndex : Integer; AValue : integer); virtual;
    Procedure Settop(AIndex : Integer; AValue : integer); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property left : integer Index 8 Read Fleft Write Setleft;
    Property top : integer Index 16 Read Ftop Write Settop;
    Property width : integer Index 24 Read Fwidth Write Setwidth;
  end;
  TPagespeedApiFormatStringV2argssecondary_rectsClass = Class of TPagespeedApiFormatStringV2argssecondary_rects;
  
  { --------------------------------------------------------------------
    TPagespeedApiImageV2
    --------------------------------------------------------------------}
  
  TPagespeedApiImageV2 = Class(TGoogleBaseObject)
  Private
    Fdata : string;
    Fheight : integer;
    Fkey : string;
    Fmime_type : string;
    Fpage_rect : TPagespeedApiImageV2page_rect;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setdata(AIndex : Integer; AValue : string); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setmime_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setpage_rect(AIndex : Integer; AValue : TPagespeedApiImageV2page_rect); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property data : string Index 0 Read Fdata Write Setdata;
    Property height : integer Index 8 Read Fheight Write Setheight;
    Property key : string Index 16 Read Fkey Write Setkey;
    Property mime_type : string Index 24 Read Fmime_type Write Setmime_type;
    Property page_rect : TPagespeedApiImageV2page_rect Index 32 Read Fpage_rect Write Setpage_rect;
    Property width : integer Index 40 Read Fwidth Write Setwidth;
  end;
  TPagespeedApiImageV2Class = Class of TPagespeedApiImageV2;
  
  { --------------------------------------------------------------------
    TPagespeedApiImageV2page_rect
    --------------------------------------------------------------------}
  
  TPagespeedApiImageV2page_rect = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Fleft : integer;
    Ftop : integer;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setleft(AIndex : Integer; AValue : integer); virtual;
    Procedure Settop(AIndex : Integer; AValue : integer); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property left : integer Index 8 Read Fleft Write Setleft;
    Property top : integer Index 16 Read Ftop Write Settop;
    Property width : integer Index 24 Read Fwidth Write Setwidth;
  end;
  TPagespeedApiImageV2page_rectClass = Class of TPagespeedApiImageV2page_rect;
  
  { --------------------------------------------------------------------
    TResult
    --------------------------------------------------------------------}
  
  TResult = Class(TGoogleBaseObject)
  Private
    FformattedResults : TResultformattedResults;
    Fid : string;
    FinvalidRules : TResultinvalidRules;
    Fkind : string;
    FpageStats : TResultpageStats;
    FresponseCode : integer;
    FruleGroups : TResultruleGroups;
    Fscreenshot : TPagespeedApiImageV2;
    Ftitle : string;
    Fversion : TResultversion;
  Protected
    //Property setters
    Procedure SetformattedResults(AIndex : Integer; AValue : TResultformattedResults); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinvalidRules(AIndex : Integer; AValue : TResultinvalidRules); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetpageStats(AIndex : Integer; AValue : TResultpageStats); virtual;
    Procedure SetresponseCode(AIndex : Integer; AValue : integer); virtual;
    Procedure SetruleGroups(AIndex : Integer; AValue : TResultruleGroups); virtual;
    Procedure Setscreenshot(AIndex : Integer; AValue : TPagespeedApiImageV2); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setversion(AIndex : Integer; AValue : TResultversion); virtual;
  Public
  Published
    Property formattedResults : TResultformattedResults Index 0 Read FformattedResults Write SetformattedResults;
    Property id : string Index 8 Read Fid Write Setid;
    Property invalidRules : TResultinvalidRules Index 16 Read FinvalidRules Write SetinvalidRules;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property pageStats : TResultpageStats Index 32 Read FpageStats Write SetpageStats;
    Property responseCode : integer Index 40 Read FresponseCode Write SetresponseCode;
    Property ruleGroups : TResultruleGroups Index 48 Read FruleGroups Write SetruleGroups;
    Property screenshot : TPagespeedApiImageV2 Index 56 Read Fscreenshot Write Setscreenshot;
    Property title : string Index 64 Read Ftitle Write Settitle;
    Property version : TResultversion Index 72 Read Fversion Write Setversion;
  end;
  TResultClass = Class of TResult;
  
  { --------------------------------------------------------------------
    TResultformattedResults
    --------------------------------------------------------------------}
  
  TResultformattedResults = Class(TGoogleBaseObject)
  Private
    Flocale : string;
    FruleResults : TResultformattedResultsruleResults;
  Protected
    //Property setters
    Procedure Setlocale(AIndex : Integer; AValue : string); virtual;
    Procedure SetruleResults(AIndex : Integer; AValue : TResultformattedResultsruleResults); virtual;
  Public
  Published
    Property locale : string Index 0 Read Flocale Write Setlocale;
    Property ruleResults : TResultformattedResultsruleResults Index 8 Read FruleResults Write SetruleResults;
  end;
  TResultformattedResultsClass = Class of TResultformattedResults;
  
  { --------------------------------------------------------------------
    TResultformattedResultsruleResults
    --------------------------------------------------------------------}
  
  TResultformattedResultsruleResults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TResultformattedResultsruleResultsClass = Class of TResultformattedResultsruleResults;
  
  { --------------------------------------------------------------------
    TResultinvalidRules
    --------------------------------------------------------------------}
  
  TResultinvalidRules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TResultinvalidRulesClass = Class of TResultinvalidRules;
  
  { --------------------------------------------------------------------
    TResultpageStats
    --------------------------------------------------------------------}
  
  TResultpageStats = Class(TGoogleBaseObject)
  Private
    FcssResponseBytes : string;
    FflashResponseBytes : string;
    FhtmlResponseBytes : string;
    FimageResponseBytes : string;
    FjavascriptResponseBytes : string;
    FnumberCssResources : integer;
    FnumberHosts : integer;
    FnumberJsResources : integer;
    FnumberResources : integer;
    FnumberStaticResources : integer;
    FotherResponseBytes : string;
    FtextResponseBytes : string;
    FtotalRequestBytes : string;
  Protected
    //Property setters
    Procedure SetcssResponseBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SetflashResponseBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SethtmlResponseBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SetimageResponseBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SetjavascriptResponseBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumberCssResources(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberHosts(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberJsResources(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberResources(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberStaticResources(AIndex : Integer; AValue : integer); virtual;
    Procedure SetotherResponseBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SettextResponseBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalRequestBytes(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property cssResponseBytes : string Index 0 Read FcssResponseBytes Write SetcssResponseBytes;
    Property flashResponseBytes : string Index 8 Read FflashResponseBytes Write SetflashResponseBytes;
    Property htmlResponseBytes : string Index 16 Read FhtmlResponseBytes Write SethtmlResponseBytes;
    Property imageResponseBytes : string Index 24 Read FimageResponseBytes Write SetimageResponseBytes;
    Property javascriptResponseBytes : string Index 32 Read FjavascriptResponseBytes Write SetjavascriptResponseBytes;
    Property numberCssResources : integer Index 40 Read FnumberCssResources Write SetnumberCssResources;
    Property numberHosts : integer Index 48 Read FnumberHosts Write SetnumberHosts;
    Property numberJsResources : integer Index 56 Read FnumberJsResources Write SetnumberJsResources;
    Property numberResources : integer Index 64 Read FnumberResources Write SetnumberResources;
    Property numberStaticResources : integer Index 72 Read FnumberStaticResources Write SetnumberStaticResources;
    Property otherResponseBytes : string Index 80 Read FotherResponseBytes Write SetotherResponseBytes;
    Property textResponseBytes : string Index 88 Read FtextResponseBytes Write SettextResponseBytes;
    Property totalRequestBytes : string Index 96 Read FtotalRequestBytes Write SettotalRequestBytes;
  end;
  TResultpageStatsClass = Class of TResultpageStats;
  
  { --------------------------------------------------------------------
    TResultruleGroups
    --------------------------------------------------------------------}
  
  TResultruleGroups = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TResultruleGroupsClass = Class of TResultruleGroups;
  
  { --------------------------------------------------------------------
    TResultversion
    --------------------------------------------------------------------}
  
  TResultversion = Class(TGoogleBaseObject)
  Private
    Fmajor : integer;
    Fminor : integer;
  Protected
    //Property setters
    Procedure Setmajor(AIndex : Integer; AValue : integer); virtual;
    Procedure Setminor(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property major : integer Index 0 Read Fmajor Write Setmajor;
    Property minor : integer Index 8 Read Fminor Write Setminor;
  end;
  TResultversionClass = Class of TResultversion;
  
  { --------------------------------------------------------------------
    TPagespeedapiResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPagespeedapiResource, method Runpagespeed
  
  TPagespeedapiRunpagespeedOptions = Record
    filter_third_party_resources : boolean;
    locale : string;
    rule : string;
    screenshot : boolean;
    strategy : string;
    url : string;
  end;
  
  TPagespeedapiResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Runpagespeed(AQuery : string  = '') : TResult;
    Function Runpagespeed(AQuery : TPagespeedapirunpagespeedOptions) : TResult;
  end;
  
  
  { --------------------------------------------------------------------
    TPagespeedonlineAPI
    --------------------------------------------------------------------}
  
  TPagespeedonlineAPI = Class(TGoogleAPI)
  Private
    FPagespeedapiInstance : TPagespeedapiResource;
    Function GetPagespeedapiInstance : TPagespeedapiResource;virtual;
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
    Function CreatePagespeedapiResource(AOwner : TComponent) : TPagespeedapiResource;virtual;overload;
    Function CreatePagespeedapiResource : TPagespeedapiResource;virtual;overload;
    //Add default on-demand instances for resources
    Property PagespeedapiResource : TPagespeedapiResource Read GetPagespeedapiInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TPagespeedApiFormatStringV2
  --------------------------------------------------------------------}


Procedure TPagespeedApiFormatStringV2.Setargs(AIndex : Integer; AValue : TPagespeedApiFormatStringV2args); 

begin
  If (Fargs=AValue) then exit;
  Fargs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2.Setformat(AIndex : Integer; AValue : string); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPagespeedApiFormatStringV2args
  --------------------------------------------------------------------}


Procedure TPagespeedApiFormatStringV2args.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2args.Setrects(AIndex : Integer; AValue : TPagespeedApiFormatStringV2argsrects); 

begin
  If (Frects=AValue) then exit;
  Frects:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2args.Setsecondary_rects(AIndex : Integer; AValue : TPagespeedApiFormatStringV2argssecondary_rects); 

begin
  If (Fsecondary_rects=AValue) then exit;
  Fsecondary_rects:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2args.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2args.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPagespeedApiFormatStringV2args.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPagespeedApiFormatStringV2argsrects
  --------------------------------------------------------------------}


Procedure TPagespeedApiFormatStringV2argsrects.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2argsrects.Setleft(AIndex : Integer; AValue : integer); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2argsrects.Settop(AIndex : Integer; AValue : integer); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2argsrects.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPagespeedApiFormatStringV2argssecondary_rects
  --------------------------------------------------------------------}


Procedure TPagespeedApiFormatStringV2argssecondary_rects.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2argssecondary_rects.Setleft(AIndex : Integer; AValue : integer); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2argssecondary_rects.Settop(AIndex : Integer; AValue : integer); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2argssecondary_rects.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPagespeedApiImageV2
  --------------------------------------------------------------------}


Procedure TPagespeedApiImageV2.Setdata(AIndex : Integer; AValue : string); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2.Setmime_type(AIndex : Integer; AValue : string); 

begin
  If (Fmime_type=AValue) then exit;
  Fmime_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2.Setpage_rect(AIndex : Integer; AValue : TPagespeedApiImageV2page_rect); 

begin
  If (Fpage_rect=AValue) then exit;
  Fpage_rect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPagespeedApiImageV2page_rect
  --------------------------------------------------------------------}


Procedure TPagespeedApiImageV2page_rect.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2page_rect.Setleft(AIndex : Integer; AValue : integer); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2page_rect.Settop(AIndex : Integer; AValue : integer); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2page_rect.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResult
  --------------------------------------------------------------------}


Procedure TResult.SetformattedResults(AIndex : Integer; AValue : TResultformattedResults); 

begin
  If (FformattedResults=AValue) then exit;
  FformattedResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetinvalidRules(AIndex : Integer; AValue : TResultinvalidRules); 

begin
  If (FinvalidRules=AValue) then exit;
  FinvalidRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetpageStats(AIndex : Integer; AValue : TResultpageStats); 

begin
  If (FpageStats=AValue) then exit;
  FpageStats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetresponseCode(AIndex : Integer; AValue : integer); 

begin
  If (FresponseCode=AValue) then exit;
  FresponseCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetruleGroups(AIndex : Integer; AValue : TResultruleGroups); 

begin
  If (FruleGroups=AValue) then exit;
  FruleGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setscreenshot(AIndex : Integer; AValue : TPagespeedApiImageV2); 

begin
  If (Fscreenshot=AValue) then exit;
  Fscreenshot:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setversion(AIndex : Integer; AValue : TResultversion); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultformattedResults
  --------------------------------------------------------------------}


Procedure TResultformattedResults.Setlocale(AIndex : Integer; AValue : string); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultformattedResults.SetruleResults(AIndex : Integer; AValue : TResultformattedResultsruleResults); 

begin
  If (FruleResults=AValue) then exit;
  FruleResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultformattedResultsruleResults
  --------------------------------------------------------------------}


Class Function TResultformattedResultsruleResults.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TResultinvalidRules
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TResultpageStats
  --------------------------------------------------------------------}


Procedure TResultpageStats.SetcssResponseBytes(AIndex : Integer; AValue : string); 

begin
  If (FcssResponseBytes=AValue) then exit;
  FcssResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SetflashResponseBytes(AIndex : Integer; AValue : string); 

begin
  If (FflashResponseBytes=AValue) then exit;
  FflashResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SethtmlResponseBytes(AIndex : Integer; AValue : string); 

begin
  If (FhtmlResponseBytes=AValue) then exit;
  FhtmlResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SetimageResponseBytes(AIndex : Integer; AValue : string); 

begin
  If (FimageResponseBytes=AValue) then exit;
  FimageResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SetjavascriptResponseBytes(AIndex : Integer; AValue : string); 

begin
  If (FjavascriptResponseBytes=AValue) then exit;
  FjavascriptResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SetnumberCssResources(AIndex : Integer; AValue : integer); 

begin
  If (FnumberCssResources=AValue) then exit;
  FnumberCssResources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SetnumberHosts(AIndex : Integer; AValue : integer); 

begin
  If (FnumberHosts=AValue) then exit;
  FnumberHosts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SetnumberJsResources(AIndex : Integer; AValue : integer); 

begin
  If (FnumberJsResources=AValue) then exit;
  FnumberJsResources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SetnumberResources(AIndex : Integer; AValue : integer); 

begin
  If (FnumberResources=AValue) then exit;
  FnumberResources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SetnumberStaticResources(AIndex : Integer; AValue : integer); 

begin
  If (FnumberStaticResources=AValue) then exit;
  FnumberStaticResources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SetotherResponseBytes(AIndex : Integer; AValue : string); 

begin
  If (FotherResponseBytes=AValue) then exit;
  FotherResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SettextResponseBytes(AIndex : Integer; AValue : string); 

begin
  If (FtextResponseBytes=AValue) then exit;
  FtextResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultpageStats.SettotalRequestBytes(AIndex : Integer; AValue : string); 

begin
  If (FtotalRequestBytes=AValue) then exit;
  FtotalRequestBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultruleGroups
  --------------------------------------------------------------------}


Class Function TResultruleGroups.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TResultversion
  --------------------------------------------------------------------}


Procedure TResultversion.Setmajor(AIndex : Integer; AValue : integer); 

begin
  If (Fmajor=AValue) then exit;
  Fmajor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultversion.Setminor(AIndex : Integer; AValue : integer); 

begin
  If (Fminor=AValue) then exit;
  Fminor:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPagespeedapiResource
  --------------------------------------------------------------------}


Class Function TPagespeedapiResource.ResourceName : String;

begin
  Result:='pagespeedapi';
end;

Class Function TPagespeedapiResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpagespeedonlineAPI;
end;

Function TPagespeedapiResource.Runpagespeed(AQuery : string = '') : TResult;

Const
  _HTTPMethod = 'GET';
  _Path       = 'runPagespeed';
  _Methodid   = 'pagespeedonline.pagespeedapi.runpagespeed';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TResult) as TResult;
end;


Function TPagespeedapiResource.Runpagespeed(AQuery : TPagespeedapirunpagespeedOptions) : TResult;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter_third_party_resources',AQuery.filter_third_party_resources);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'rule',AQuery.rule);
  AddToQuery(_Q,'screenshot',AQuery.screenshot);
  AddToQuery(_Q,'strategy',AQuery.strategy);
  AddToQuery(_Q,'url',AQuery.url);
  Result:=Runpagespeed(_Q);
end;



{ --------------------------------------------------------------------
  TPagespeedonlineAPI
  --------------------------------------------------------------------}

Class Function TPagespeedonlineAPI.APIName : String;

begin
  Result:='pagespeedonline';
end;

Class Function TPagespeedonlineAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TPagespeedonlineAPI.APIRevision : String;

begin
  Result:='20150317';
end;

Class Function TPagespeedonlineAPI.APIID : String;

begin
  Result:='pagespeedonline:v2';
end;

Class Function TPagespeedonlineAPI.APITitle : String;

begin
  Result:='PageSpeed Insights API';
end;

Class Function TPagespeedonlineAPI.APIDescription : String;

begin
  Result:='Lets you analyze the performance of a web page and get tailored suggestions to make that page faster.';
end;

Class Function TPagespeedonlineAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TPagespeedonlineAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TPagespeedonlineAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/pagespeed-16.png';
end;

Class Function TPagespeedonlineAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/pagespeed-32.png';
end;

Class Function TPagespeedonlineAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/speed/docs/insights/v2/getting-started';
end;

Class Function TPagespeedonlineAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TPagespeedonlineAPI.APIbasePath : string;

begin
  Result:='/pagespeedonline/v2/';
end;

Class Function TPagespeedonlineAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/pagespeedonline/v2/';
end;

Class Function TPagespeedonlineAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TPagespeedonlineAPI.APIservicePath : string;

begin
  Result:='pagespeedonline/v2/';
end;

Class Function TPagespeedonlineAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TPagespeedonlineAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TPagespeedonlineAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TPagespeedonlineAPI.RegisterAPIResources;

begin
  TPagespeedApiFormatStringV2.RegisterObject;
  TPagespeedApiFormatStringV2args.RegisterObject;
  TPagespeedApiFormatStringV2argsrects.RegisterObject;
  TPagespeedApiFormatStringV2argssecondary_rects.RegisterObject;
  TPagespeedApiImageV2.RegisterObject;
  TPagespeedApiImageV2page_rect.RegisterObject;
  TResult.RegisterObject;
  TResultformattedResults.RegisterObject;
  TResultformattedResultsruleResults.RegisterObject;
  TResultinvalidRules.RegisterObject;
  TResultpageStats.RegisterObject;
  TResultruleGroups.RegisterObject;
  TResultversion.RegisterObject;
end;


Function TPagespeedonlineAPI.GetPagespeedapiInstance : TPagespeedapiResource;

begin
  if (FPagespeedapiInstance=Nil) then
    FPagespeedapiInstance:=CreatePagespeedapiResource;
  Result:=FPagespeedapiInstance;
end;

Function TPagespeedonlineAPI.CreatePagespeedapiResource : TPagespeedapiResource;

begin
  Result:=CreatePagespeedapiResource(Self);
end;


Function TPagespeedonlineAPI.CreatePagespeedapiResource(AOwner : TComponent) : TPagespeedapiResource;

begin
  Result:=TPagespeedapiResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TPagespeedonlineAPI.RegisterAPI;
end.
