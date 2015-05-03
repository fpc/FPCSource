unit googlewebfonts;
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
  TWebfont = class;
  TWebfontArray = Array of TWebfont;
  TWebfontfiles = class;
  TWebfontfilesArray = Array of TWebfontfiles;
  TWebfontsubsets = class;
  TWebfontsubsetsArray = Array of TWebfontsubsets;
  TWebfontvariants = class;
  TWebfontvariantsArray = Array of TWebfontvariants;
  TWebfontList = class;
  TWebfontListArray = Array of TWebfontList;
  TWebfontListitems = class;
  TWebfontListitemsArray = Array of TWebfontListitems;
  
  { --------------------------------------------------------------------
    TWebfont
    --------------------------------------------------------------------}
  
  TWebfont = Class(TGoogleBaseObject)
  Private
    Fcategory : string;
    Ffamily : string;
    Ffiles : TWebfontfiles;
    Fkind : string;
    FlastModified : TDate;
    Fsubsets : TWebfontsubsets;
    Fvariants : TWebfontvariants;
    Fversion : string;
  Protected
    //Property setters
    Procedure Setcategory(AIndex : Integer; AValue : string); virtual;
    Procedure Setfamily(AIndex : Integer; AValue : string); virtual;
    Procedure Setfiles(AIndex : Integer; AValue : TWebfontfiles); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastModified(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setsubsets(AIndex : Integer; AValue : TWebfontsubsets); virtual;
    Procedure Setvariants(AIndex : Integer; AValue : TWebfontvariants); virtual;
    Procedure Setversion(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property category : string Index 0 Read Fcategory Write Setcategory;
    Property family : string Index 8 Read Ffamily Write Setfamily;
    Property files : TWebfontfiles Index 16 Read Ffiles Write Setfiles;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property lastModified : TDate Index 32 Read FlastModified Write SetlastModified;
    Property subsets : TWebfontsubsets Index 40 Read Fsubsets Write Setsubsets;
    Property variants : TWebfontvariants Index 48 Read Fvariants Write Setvariants;
    Property version : string Index 56 Read Fversion Write Setversion;
  end;
  TWebfontClass = Class of TWebfont;
  
  { --------------------------------------------------------------------
    TWebfontfiles
    --------------------------------------------------------------------}
  
  TWebfontfiles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWebfontfilesClass = Class of TWebfontfiles;
  
  { --------------------------------------------------------------------
    TWebfontsubsets
    --------------------------------------------------------------------}
  
  TWebfontsubsets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWebfontsubsetsClass = Class of TWebfontsubsets;
  
  { --------------------------------------------------------------------
    TWebfontvariants
    --------------------------------------------------------------------}
  
  TWebfontvariants = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWebfontvariantsClass = Class of TWebfontvariants;
  
  { --------------------------------------------------------------------
    TWebfontList
    --------------------------------------------------------------------}
  
  TWebfontList = Class(TGoogleBaseObject)
  Private
    Fitems : TWebfontListitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TWebfontListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TWebfontListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TWebfontListClass = Class of TWebfontList;
  
  { --------------------------------------------------------------------
    TWebfontListitems
    --------------------------------------------------------------------}
  
  TWebfontListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWebfontListitemsClass = Class of TWebfontListitems;
  
  { --------------------------------------------------------------------
    TWebfontsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TWebfontsResource, method List
  
  TWebfontsListOptions = Record
    sort : string;
  end;
  
  TWebfontsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TWebfontList;
    Function List(AQuery : TWebfontslistOptions) : TWebfontList;
  end;
  
  
  { --------------------------------------------------------------------
    TWebfontsAPI
    --------------------------------------------------------------------}
  
  TWebfontsAPI = Class(TGoogleAPI)
  Private
    FWebfontsInstance : TWebfontsResource;
    Function GetWebfontsInstance : TWebfontsResource;virtual;
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
    Function CreateWebfontsResource(AOwner : TComponent) : TWebfontsResource;virtual;overload;
    Function CreateWebfontsResource : TWebfontsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property WebfontsResource : TWebfontsResource Read GetWebfontsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TWebfont
  --------------------------------------------------------------------}


Procedure TWebfont.Setcategory(AIndex : Integer; AValue : string); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setfamily(AIndex : Integer; AValue : string); 

begin
  If (Ffamily=AValue) then exit;
  Ffamily:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setfiles(AIndex : Integer; AValue : TWebfontfiles); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.SetlastModified(AIndex : Integer; AValue : TDate); 

begin
  If (FlastModified=AValue) then exit;
  FlastModified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setsubsets(AIndex : Integer; AValue : TWebfontsubsets); 

begin
  If (Fsubsets=AValue) then exit;
  Fsubsets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setvariants(AIndex : Integer; AValue : TWebfontvariants); 

begin
  If (Fvariants=AValue) then exit;
  Fvariants:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setversion(AIndex : Integer; AValue : string); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebfontfiles
  --------------------------------------------------------------------}


Class Function TWebfontfiles.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWebfontsubsets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWebfontvariants
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWebfontList
  --------------------------------------------------------------------}


Procedure TWebfontList.Setitems(AIndex : Integer; AValue : TWebfontListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfontList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWebfontListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWebfontsResource
  --------------------------------------------------------------------}


Class Function TWebfontsResource.ResourceName : String;

begin
  Result:='webfonts';
end;

Class Function TWebfontsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TwebfontsAPI;
end;

Function TWebfontsResource.List(AQuery : string = '') : TWebfontList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'webfonts';
  _Methodid   = 'webfonts.webfonts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TWebfontList) as TWebfontList;
end;


Function TWebfontsResource.List(AQuery : TWebfontslistOptions) : TWebfontList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'sort',AQuery.sort);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TWebfontsAPI
  --------------------------------------------------------------------}

Class Function TWebfontsAPI.APIName : String;

begin
  Result:='webfonts';
end;

Class Function TWebfontsAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TWebfontsAPI.APIRevision : String;

begin
  Result:='20140210';
end;

Class Function TWebfontsAPI.APIID : String;

begin
  Result:='webfonts:v1';
end;

Class Function TWebfontsAPI.APITitle : String;

begin
  Result:='Google Fonts Developer API';
end;

Class Function TWebfontsAPI.APIDescription : String;

begin
  Result:='The Google Fonts Developer API.';
end;

Class Function TWebfontsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TWebfontsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TWebfontsAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/feature/font_api-16.png';
end;

Class Function TWebfontsAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/feature/font_api-32.gif';
end;

Class Function TWebfontsAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/fonts/docs/developer_api';
end;

Class Function TWebfontsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TWebfontsAPI.APIbasePath : string;

begin
  Result:='/webfonts/v1/';
end;

Class Function TWebfontsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/webfonts/v1/';
end;

Class Function TWebfontsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TWebfontsAPI.APIservicePath : string;

begin
  Result:='webfonts/v1/';
end;

Class Function TWebfontsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TWebfontsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TWebfontsAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TWebfontsAPI.RegisterAPIResources;

begin
  TWebfont.RegisterObject;
  TWebfontfiles.RegisterObject;
  TWebfontsubsets.RegisterObject;
  TWebfontvariants.RegisterObject;
  TWebfontList.RegisterObject;
  TWebfontListitems.RegisterObject;
end;


Function TWebfontsAPI.GetWebfontsInstance : TWebfontsResource;

begin
  if (FWebfontsInstance=Nil) then
    FWebfontsInstance:=CreateWebfontsResource;
  Result:=FWebfontsInstance;
end;

Function TWebfontsAPI.CreateWebfontsResource : TWebfontsResource;

begin
  Result:=CreateWebfontsResource(Self);
end;


Function TWebfontsAPI.CreateWebfontsResource(AOwner : TComponent) : TWebfontsResource;

begin
  Result:=TWebfontsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TWebfontsAPI.RegisterAPI;
end.
