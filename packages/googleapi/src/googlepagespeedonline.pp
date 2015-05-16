unit googlepagespeedonline;
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
//Generated on: 16-5-15 08:53:06
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TPagespeedApiFormatStringV2 = Class;
  TPagespeedApiImageV2 = Class;
  TResult = Class;
  TPagespeedApiFormatStringV2Array = Array of TPagespeedApiFormatStringV2;
  TPagespeedApiImageV2Array = Array of TPagespeedApiImageV2;
  TResultArray = Array of TResult;
  //Anonymous types, using auto-generated names
  TPagespeedApiFormatStringV2TypeargsItemTyperectsItem = Class;
  TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem = Class;
  TPagespeedApiFormatStringV2TypeargsItem = Class;
  TPagespeedApiImageV2Typepage_rect = Class;
  TResultTypeformattedResultsTyperuleResults = Class;
  TResultTypeformattedResults = Class;
  TResultTypepageStats = Class;
  TResultTyperuleGroups = Class;
  TResultTypeversion = Class;
  TPagespeedApiFormatStringV2TypeargsItemTyperectsArray = Array of TPagespeedApiFormatStringV2TypeargsItemTyperectsItem;
  TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsArray = Array of TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem;
  TPagespeedApiFormatStringV2TypeargsArray = Array of TPagespeedApiFormatStringV2TypeargsItem;
  
  { --------------------------------------------------------------------
    TPagespeedApiFormatStringV2TypeargsItemTyperectsItem
    --------------------------------------------------------------------}
  
  TPagespeedApiFormatStringV2TypeargsItemTyperectsItem = Class(TGoogleBaseObject)
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
  TPagespeedApiFormatStringV2TypeargsItemTyperectsItemClass = Class of TPagespeedApiFormatStringV2TypeargsItemTyperectsItem;
  
  { --------------------------------------------------------------------
    TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem
    --------------------------------------------------------------------}
  
  TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem = Class(TGoogleBaseObject)
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
  TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItemClass = Class of TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem;
  
  { --------------------------------------------------------------------
    TPagespeedApiFormatStringV2TypeargsItem
    --------------------------------------------------------------------}
  
  TPagespeedApiFormatStringV2TypeargsItem = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Frects : TPagespeedApiFormatStringV2TypeargsItemTyperectsArray;
    Fsecondary_rects : TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsArray;
    F_type : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setrects(AIndex : Integer; AValue : TPagespeedApiFormatStringV2TypeargsItemTyperectsArray); virtual;
    Procedure Setsecondary_rects(AIndex : Integer; AValue : TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property rects : TPagespeedApiFormatStringV2TypeargsItemTyperectsArray Index 8 Read Frects Write Setrects;
    Property secondary_rects : TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsArray Index 16 Read Fsecondary_rects Write Setsecondary_rects;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property value : String Index 32 Read Fvalue Write Setvalue;
  end;
  TPagespeedApiFormatStringV2TypeargsItemClass = Class of TPagespeedApiFormatStringV2TypeargsItem;
  
  { --------------------------------------------------------------------
    TPagespeedApiFormatStringV2
    --------------------------------------------------------------------}
  
  TPagespeedApiFormatStringV2 = Class(TGoogleBaseObject)
  Private
    Fargs : TPagespeedApiFormatStringV2TypeargsArray;
    Fformat : String;
  Protected
    //Property setters
    Procedure Setargs(AIndex : Integer; AValue : TPagespeedApiFormatStringV2TypeargsArray); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property args : TPagespeedApiFormatStringV2TypeargsArray Index 0 Read Fargs Write Setargs;
    Property format : String Index 8 Read Fformat Write Setformat;
  end;
  TPagespeedApiFormatStringV2Class = Class of TPagespeedApiFormatStringV2;
  
  { --------------------------------------------------------------------
    TPagespeedApiImageV2Typepage_rect
    --------------------------------------------------------------------}
  
  TPagespeedApiImageV2Typepage_rect = Class(TGoogleBaseObject)
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
  TPagespeedApiImageV2Typepage_rectClass = Class of TPagespeedApiImageV2Typepage_rect;
  
  { --------------------------------------------------------------------
    TPagespeedApiImageV2
    --------------------------------------------------------------------}
  
  TPagespeedApiImageV2 = Class(TGoogleBaseObject)
  Private
    Fdata : String;
    Fheight : integer;
    Fkey : String;
    Fmime_type : String;
    Fpage_rect : TPagespeedApiImageV2Typepage_rect;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setdata(AIndex : Integer; AValue : String); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setmime_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setpage_rect(AIndex : Integer; AValue : TPagespeedApiImageV2Typepage_rect); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property data : String Index 0 Read Fdata Write Setdata;
    Property height : integer Index 8 Read Fheight Write Setheight;
    Property key : String Index 16 Read Fkey Write Setkey;
    Property mime_type : String Index 24 Read Fmime_type Write Setmime_type;
    Property page_rect : TPagespeedApiImageV2Typepage_rect Index 32 Read Fpage_rect Write Setpage_rect;
    Property width : integer Index 40 Read Fwidth Write Setwidth;
  end;
  TPagespeedApiImageV2Class = Class of TPagespeedApiImageV2;
  
  { --------------------------------------------------------------------
    TResultTypeformattedResultsTyperuleResults
    --------------------------------------------------------------------}
  
  TResultTypeformattedResultsTyperuleResults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TResultTypeformattedResultsTyperuleResultsClass = Class of TResultTypeformattedResultsTyperuleResults;
  
  { --------------------------------------------------------------------
    TResultTypeformattedResults
    --------------------------------------------------------------------}
  
  TResultTypeformattedResults = Class(TGoogleBaseObject)
  Private
    Flocale : String;
    FruleResults : TResultTypeformattedResultsTyperuleResults;
  Protected
    //Property setters
    Procedure Setlocale(AIndex : Integer; AValue : String); virtual;
    Procedure SetruleResults(AIndex : Integer; AValue : TResultTypeformattedResultsTyperuleResults); virtual;
  Public
  Published
    Property locale : String Index 0 Read Flocale Write Setlocale;
    Property ruleResults : TResultTypeformattedResultsTyperuleResults Index 8 Read FruleResults Write SetruleResults;
  end;
  TResultTypeformattedResultsClass = Class of TResultTypeformattedResults;
  
  { --------------------------------------------------------------------
    TResultTypepageStats
    --------------------------------------------------------------------}
  
  TResultTypepageStats = Class(TGoogleBaseObject)
  Private
    FcssResponseBytes : String;
    FflashResponseBytes : String;
    FhtmlResponseBytes : String;
    FimageResponseBytes : String;
    FjavascriptResponseBytes : String;
    FnumberCssResources : integer;
    FnumberHosts : integer;
    FnumberJsResources : integer;
    FnumberResources : integer;
    FnumberStaticResources : integer;
    FotherResponseBytes : String;
    FtextResponseBytes : String;
    FtotalRequestBytes : String;
  Protected
    //Property setters
    Procedure SetcssResponseBytes(AIndex : Integer; AValue : String); virtual;
    Procedure SetflashResponseBytes(AIndex : Integer; AValue : String); virtual;
    Procedure SethtmlResponseBytes(AIndex : Integer; AValue : String); virtual;
    Procedure SetimageResponseBytes(AIndex : Integer; AValue : String); virtual;
    Procedure SetjavascriptResponseBytes(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumberCssResources(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberHosts(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberJsResources(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberResources(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberStaticResources(AIndex : Integer; AValue : integer); virtual;
    Procedure SetotherResponseBytes(AIndex : Integer; AValue : String); virtual;
    Procedure SettextResponseBytes(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalRequestBytes(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property cssResponseBytes : String Index 0 Read FcssResponseBytes Write SetcssResponseBytes;
    Property flashResponseBytes : String Index 8 Read FflashResponseBytes Write SetflashResponseBytes;
    Property htmlResponseBytes : String Index 16 Read FhtmlResponseBytes Write SethtmlResponseBytes;
    Property imageResponseBytes : String Index 24 Read FimageResponseBytes Write SetimageResponseBytes;
    Property javascriptResponseBytes : String Index 32 Read FjavascriptResponseBytes Write SetjavascriptResponseBytes;
    Property numberCssResources : integer Index 40 Read FnumberCssResources Write SetnumberCssResources;
    Property numberHosts : integer Index 48 Read FnumberHosts Write SetnumberHosts;
    Property numberJsResources : integer Index 56 Read FnumberJsResources Write SetnumberJsResources;
    Property numberResources : integer Index 64 Read FnumberResources Write SetnumberResources;
    Property numberStaticResources : integer Index 72 Read FnumberStaticResources Write SetnumberStaticResources;
    Property otherResponseBytes : String Index 80 Read FotherResponseBytes Write SetotherResponseBytes;
    Property textResponseBytes : String Index 88 Read FtextResponseBytes Write SettextResponseBytes;
    Property totalRequestBytes : String Index 96 Read FtotalRequestBytes Write SettotalRequestBytes;
  end;
  TResultTypepageStatsClass = Class of TResultTypepageStats;
  
  { --------------------------------------------------------------------
    TResultTyperuleGroups
    --------------------------------------------------------------------}
  
  TResultTyperuleGroups = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TResultTyperuleGroupsClass = Class of TResultTyperuleGroups;
  
  { --------------------------------------------------------------------
    TResultTypeversion
    --------------------------------------------------------------------}
  
  TResultTypeversion = Class(TGoogleBaseObject)
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
  TResultTypeversionClass = Class of TResultTypeversion;
  
  { --------------------------------------------------------------------
    TResult
    --------------------------------------------------------------------}
  
  TResult = Class(TGoogleBaseObject)
  Private
    FformattedResults : TResultTypeformattedResults;
    Fid : String;
    FinvalidRules : TStringArray;
    Fkind : String;
    FpageStats : TResultTypepageStats;
    FresponseCode : integer;
    FruleGroups : TResultTyperuleGroups;
    Fscreenshot : TPagespeedApiImageV2;
    Ftitle : String;
    Fversion : TResultTypeversion;
  Protected
    //Property setters
    Procedure SetformattedResults(AIndex : Integer; AValue : TResultTypeformattedResults); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinvalidRules(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageStats(AIndex : Integer; AValue : TResultTypepageStats); virtual;
    Procedure SetresponseCode(AIndex : Integer; AValue : integer); virtual;
    Procedure SetruleGroups(AIndex : Integer; AValue : TResultTyperuleGroups); virtual;
    Procedure Setscreenshot(AIndex : Integer; AValue : TPagespeedApiImageV2); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : TResultTypeversion); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property formattedResults : TResultTypeformattedResults Index 0 Read FformattedResults Write SetformattedResults;
    Property id : String Index 8 Read Fid Write Setid;
    Property invalidRules : TStringArray Index 16 Read FinvalidRules Write SetinvalidRules;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property pageStats : TResultTypepageStats Index 32 Read FpageStats Write SetpageStats;
    Property responseCode : integer Index 40 Read FresponseCode Write SetresponseCode;
    Property ruleGroups : TResultTyperuleGroups Index 48 Read FruleGroups Write SetruleGroups;
    Property screenshot : TPagespeedApiImageV2 Index 56 Read Fscreenshot Write Setscreenshot;
    Property title : String Index 64 Read Ftitle Write Settitle;
    Property version : TResultTypeversion Index 72 Read Fversion Write Setversion;
  end;
  TResultClass = Class of TResult;
  
  { --------------------------------------------------------------------
    TPagespeedapiResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPagespeedapiResource, method Runpagespeed
  
  TPagespeedapiRunpagespeedOptions = Record
    filter_third_party_resources : boolean;
    locale : String;
    rule : String;
    screenshot : boolean;
    strategy : String;
    url : String;
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
  TPagespeedApiFormatStringV2TypeargsItemTyperectsItem
  --------------------------------------------------------------------}


Procedure TPagespeedApiFormatStringV2TypeargsItemTyperectsItem.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItemTyperectsItem.Setleft(AIndex : Integer; AValue : integer); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItemTyperectsItem.Settop(AIndex : Integer; AValue : integer); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItemTyperectsItem.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem
  --------------------------------------------------------------------}


Procedure TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem.Setleft(AIndex : Integer; AValue : integer); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem.Settop(AIndex : Integer; AValue : integer); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPagespeedApiFormatStringV2TypeargsItem
  --------------------------------------------------------------------}


Procedure TPagespeedApiFormatStringV2TypeargsItem.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItem.Setrects(AIndex : Integer; AValue : TPagespeedApiFormatStringV2TypeargsItemTyperectsArray); 

begin
  If (Frects=AValue) then exit;
  Frects:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItem.Setsecondary_rects(AIndex : Integer; AValue : TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsArray); 

begin
  If (Fsecondary_rects=AValue) then exit;
  Fsecondary_rects:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2TypeargsItem.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPagespeedApiFormatStringV2TypeargsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPagespeedApiFormatStringV2TypeargsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rects' : SetLength(Frects,ALength);
  'secondary_rects' : SetLength(Fsecondary_rects,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPagespeedApiFormatStringV2
  --------------------------------------------------------------------}


Procedure TPagespeedApiFormatStringV2.Setargs(AIndex : Integer; AValue : TPagespeedApiFormatStringV2TypeargsArray); 

begin
  If (Fargs=AValue) then exit;
  Fargs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiFormatStringV2.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPagespeedApiFormatStringV2.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'args' : SetLength(Fargs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPagespeedApiImageV2Typepage_rect
  --------------------------------------------------------------------}


Procedure TPagespeedApiImageV2Typepage_rect.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2Typepage_rect.Setleft(AIndex : Integer; AValue : integer); 

begin
  If (Fleft=AValue) then exit;
  Fleft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2Typepage_rect.Settop(AIndex : Integer; AValue : integer); 

begin
  If (Ftop=AValue) then exit;
  Ftop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2Typepage_rect.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPagespeedApiImageV2
  --------------------------------------------------------------------}


Procedure TPagespeedApiImageV2.Setdata(AIndex : Integer; AValue : String); 

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



Procedure TPagespeedApiImageV2.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2.Setmime_type(AIndex : Integer; AValue : String); 

begin
  If (Fmime_type=AValue) then exit;
  Fmime_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPagespeedApiImageV2.Setpage_rect(AIndex : Integer; AValue : TPagespeedApiImageV2Typepage_rect); 

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
  TResultTypeformattedResultsTyperuleResults
  --------------------------------------------------------------------}


Class Function TResultTypeformattedResultsTyperuleResults.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TResultTypeformattedResults
  --------------------------------------------------------------------}


Procedure TResultTypeformattedResults.Setlocale(AIndex : Integer; AValue : String); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypeformattedResults.SetruleResults(AIndex : Integer; AValue : TResultTypeformattedResultsTyperuleResults); 

begin
  If (FruleResults=AValue) then exit;
  FruleResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultTypepageStats
  --------------------------------------------------------------------}


Procedure TResultTypepageStats.SetcssResponseBytes(AIndex : Integer; AValue : String); 

begin
  If (FcssResponseBytes=AValue) then exit;
  FcssResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SetflashResponseBytes(AIndex : Integer; AValue : String); 

begin
  If (FflashResponseBytes=AValue) then exit;
  FflashResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SethtmlResponseBytes(AIndex : Integer; AValue : String); 

begin
  If (FhtmlResponseBytes=AValue) then exit;
  FhtmlResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SetimageResponseBytes(AIndex : Integer; AValue : String); 

begin
  If (FimageResponseBytes=AValue) then exit;
  FimageResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SetjavascriptResponseBytes(AIndex : Integer; AValue : String); 

begin
  If (FjavascriptResponseBytes=AValue) then exit;
  FjavascriptResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SetnumberCssResources(AIndex : Integer; AValue : integer); 

begin
  If (FnumberCssResources=AValue) then exit;
  FnumberCssResources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SetnumberHosts(AIndex : Integer; AValue : integer); 

begin
  If (FnumberHosts=AValue) then exit;
  FnumberHosts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SetnumberJsResources(AIndex : Integer; AValue : integer); 

begin
  If (FnumberJsResources=AValue) then exit;
  FnumberJsResources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SetnumberResources(AIndex : Integer; AValue : integer); 

begin
  If (FnumberResources=AValue) then exit;
  FnumberResources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SetnumberStaticResources(AIndex : Integer; AValue : integer); 

begin
  If (FnumberStaticResources=AValue) then exit;
  FnumberStaticResources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SetotherResponseBytes(AIndex : Integer; AValue : String); 

begin
  If (FotherResponseBytes=AValue) then exit;
  FotherResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SettextResponseBytes(AIndex : Integer; AValue : String); 

begin
  If (FtextResponseBytes=AValue) then exit;
  FtextResponseBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypepageStats.SettotalRequestBytes(AIndex : Integer; AValue : String); 

begin
  If (FtotalRequestBytes=AValue) then exit;
  FtotalRequestBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultTyperuleGroups
  --------------------------------------------------------------------}


Class Function TResultTyperuleGroups.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TResultTypeversion
  --------------------------------------------------------------------}


Procedure TResultTypeversion.Setmajor(AIndex : Integer; AValue : integer); 

begin
  If (Fmajor=AValue) then exit;
  Fmajor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypeversion.Setminor(AIndex : Integer; AValue : integer); 

begin
  If (Fminor=AValue) then exit;
  Fminor:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResult
  --------------------------------------------------------------------}


Procedure TResult.SetformattedResults(AIndex : Integer; AValue : TResultTypeformattedResults); 

begin
  If (FformattedResults=AValue) then exit;
  FformattedResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetinvalidRules(AIndex : Integer; AValue : TStringArray); 

begin
  If (FinvalidRules=AValue) then exit;
  FinvalidRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetpageStats(AIndex : Integer; AValue : TResultTypepageStats); 

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



Procedure TResult.SetruleGroups(AIndex : Integer; AValue : TResultTyperuleGroups); 

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



Procedure TResult.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setversion(AIndex : Integer; AValue : TResultTypeversion); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TResult.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'invalidrules' : SetLength(FinvalidRules,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TPagespeedonlineAPI.APIbasePath : string;

begin
  Result:='/pagespeedonline/v2/';
end;

Class Function TPagespeedonlineAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/pagespeedonline/v2/';
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
  TPagespeedApiFormatStringV2TypeargsItemTyperectsItem.RegisterObject;
  TPagespeedApiFormatStringV2TypeargsItemTypesecondary_rectsItem.RegisterObject;
  TPagespeedApiFormatStringV2TypeargsItem.RegisterObject;
  TPagespeedApiFormatStringV2.RegisterObject;
  TPagespeedApiImageV2Typepage_rect.RegisterObject;
  TPagespeedApiImageV2.RegisterObject;
  TResultTypeformattedResultsTyperuleResults.RegisterObject;
  TResultTypeformattedResults.RegisterObject;
  TResultTypepageStats.RegisterObject;
  TResultTyperuleGroups.RegisterObject;
  TResultTypeversion.RegisterObject;
  TResult.RegisterObject;
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
  Result.API:=Self.API;
end;



initialization
  TPagespeedonlineAPI.RegisterAPI;
end.
