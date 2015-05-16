{ Base Google API code generator

  Copyright (C) 2015 Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit googlediscoverytopas;

{$mode objfpc}{$H+}

interface

uses
   typinfo, Classes, SysUtils, fpjson, restcodegen, restbase, googlebase;

Type
  TTypeDef = Class;
  TSchema = Class;
  TSchemas = Array of TSchema;
  TPropertyDef = Class;
  TProperties = TSchemas;
  TRestMethod = Class;
  TRestMethods = Array of TRestMethod;
  TArrayPropertyDef = Class;

  { TIcons }

  TGoogleIcons = Class(TGoogleBaseObject)
  private
    Fx16: String;
    Fx32: String;
  Published
    Property x16 : String Read Fx16 Write fx16;
    Property x32 : String Read Fx32 Write fx32;
  end;

  { TGoogleAuth2 }

  TGoogleAuth2 = Class(TGoogleBaseObject)
  private
    FScopes: TSchemas;
  Protected
{$ifdef ver2_6}
    Procedure SetArrayLength(const AName : String; ALength : Longint); override;
{$endif}
  Published
    Property Scopes : TSchemas Read Fscopes Write Fscopes;
  end;

  { TGoogleAuth }

  TGoogleAuth = Class(TGoogleBaseObject)
  private
    Foauth2: TGoogleAuth2;
  Published
    Property oauth2 : TGoogleAuth2 Read Foauth2 Write Foauth2;
  end;

  { TDiscoveryJSONToPas }

  { TArrayPropertyDef }



  { TAnnotations }

  TAnnotations = Class(TGoogleBaseObject)
  private
    FRequired: TStringArray;
  Protected
{$ifdef ver2_6}
    Procedure SetArrayLength(const AName : String; ALength : Longint); override;
{$endif}
  Published
    Property required : TStringArray Read FRequired Write Frequired;
  end;


  { TSchema }

  TSchema = Class(TGoogleBaseObject)
  private
    FadditionalProperties: TSchema;
    FAnnotations: TAnnotations;
    FDefault: String;
    Fdescription: string;
    fdivisibleby: integer;
    FEnum: TJSONSchema;
    FenumDescriptions: TStringArray;
    fexclusivemaximum: boolean;
    fexclusiveminimum: boolean;
    Fextends: string;
    FFormat: string;
    fitems: TSchema;
    fmaximum: integer;
    fmaxItems: integer;
    fmaxlength: integer;
    Fmethods: TRestMethods;
    fminimum: integer;
    fminItems: integer;
    fminlength: integer;
    FName: String;
    FPattern: string;
    FProperties: TProperties;
    FPropertyName: String;
    FreadOnly: Boolean;
    Fref: string;
    FRefSchema: TSchema;
    frequired: Boolean;
    FResources: TSchemas;
    Fschema: String;
    FType: string;
    FID: String;
    FTypeDef: TTypeDef;
    FTypeName: String;
    funiqueItems: boolean;
    FVariant: TJSONSchema;
    function DoGetTypeName(PrependT: Boolean): String;
    function GetTypeName: String;
  Public
    Class function BaseType(ATypeName: String): Boolean;
    Class function GetBaseTypeName(AType,AFormat : String) : string;
{$ifdef ver2_6}
    Procedure SetArrayLength(const AName : String; ALength : Longint); override;
{$endif}
    Function DebugName : String;
    function GetBaseTypeName : string;
    Function BaseType : Boolean;
    Function ClassProperties : TProperties;
    Property PropertyName : String Read FPropertyName Write FPropertyName;
    Property TypeName : String Read GetTypeName Write FTypeName;
    Property Refschema : TSchema Read FRefSchema Write FRefSchema;
    Property TypeDef : TTypeDef Read FTypeDef write FTypeDef;
  Published
    Property id : String Read FID Write FID;
    Property description : string read Fdescription Write Fdescription;
    Property _type : String Read FType Write FType;
    Property ref : String Read FRef Write FRef;
    Property schema: String Read Fschema Write Fschema;
    Property name : String Read FName Write FName;
    Property enum : TJSONSchema Read FEnum Write FEnum;
    Property enumDescriptions : TStringArray Read FenumDescriptions Write FenumDescriptions;
    Property properties : TProperties Read FProperties Write FProperties;
    Property items : TSchema Read fitems write fitems;
    Property default : String Read FDefault Write FDefault;
    property required : Boolean read frequired write frequired;
    Property annotations : TAnnotations Read FAnnotations Write FAnnotations;
    Property format : string Read FFormat Write FFormat;
    Property additionalProperties : TSchema Read FadditionalProperties Write FadditionalProperties;
    Property minLength : integer read fminlength write fminlength;
    Property maxLength : integer read fmaxlength write fmaxlength;
    Property minItems : integer read fminItems write fminItems;
    Property maxItems : integer read fmaxItems write fmaxItems;
    Property minimum : integer read fminimum write fminimum;
    Property pattern : string read FPattern write fPattern;
    Property exclusiveMaximum : boolean read fexclusivemaximum write fexclusivemaximum;
    Property exclusiveMinimum : boolean read fexclusiveminimum write fexclusiveminimum;
    Property uniqueItems : boolean read funiqueItems write funiqueItems;
    Property maximum : integer read fmaximum write fmaximum;
    Property divisibleBy : integer read fdivisibleby write fdivisibleby;
    Property extends : string Read Fextends Write Fextends;
    Property methods : TRestMethods Read Fmethods Write Fmethods;
    property readOnly : Boolean Read FreadOnly Write fReadOnly;
    Property resources : TSchemas Read FResources write FResources;
    Property variant : TJSONSchema Read FVariant Write FVariant;
  end;
  TArrayPropertyDef = Class(TSchema);
  TPropertyDef = Class(TSchema);

  TMediaUploadprotocolssimple = Class(TGoogleBaseObject)
  Private
    Fmultipart : boolean;
    Fpath : string;
  Published
    Property multipart : boolean Read Fmultipart Write Fmultipart;
    Property path : string  Read Fpath Write Fpath;
  end;
  TMediaUploadProtocolsSimpleArray = Array of TMediaUploadProtocolsSimple;

  TMediaUploadProtocolsResumable = Class(TGoogleBaseObject)
   Private
     Fmultipart : boolean;
     Fpath : string;
   Published
     Property multipart : boolean Read Fmultipart Write Fmultipart;
     Property path : string Read Fpath Write Fpath;
   end;
  TMediaUploadProtocolsResumableArray = Array of TMediaUploadProtocolsResumable;

  { TMediaUploadProtocols }

  TMediaUploadProtocols = Class(TGoogleBaseObject)
  private
    Fresumable: TMediaUploadprotocolsresumable;
    Fsimple: TMediaUploadprotocolssimple;
  Published
    Property resumable : TMediaUploadprotocolsresumable Read Fresumable Write Fresumable;
    Property simple : TMediaUploadprotocolssimple Read Fsimple Write Fsimple;
  end;
  TMediaUploadProtocolsArray = Array of TMediaUploadProtocols;

  TMediaUpload = Class(TGoogleBaseObject)
  private
    FAccept: TStringArray;
    FMaxSize: String;
    Fprotocols: TMediaUploadProtocols;
  protected
{$ifdef ver2_6}
    Procedure SetArrayLength(const AName : String; ALength : Longint); override;
{$endif}
  Published
    Property Accept : TStringArray Read FAccept Write FAccept;
    property MaxSize : String Read FMaxSize Write FMaxSize;
    Property protocols : TMediaUploadProtocols Read Fprotocols Write Fprotocols;
  end;


  { TRequest }

  TRequest = Class(TGoogleBaseObject)
  private
    fparameterName: string;
    fref: string;
  Published
    Property ref : string read fref write fref;
    Property parameterName : string read fparameterName write fparameterName;
  end;

  { TResponse }

  TResponse = Class(TGoogleBaseObject)
  private
    fref: string;
  Published
    Property ref : string read fref write fref;
  end;

  { TRestMethodParam }

  TRestMethodParam = Class(TSchema)
  private
    FLocation: string;
    FRepeated: boolean;
    FSourceName: String;
  Public
    Property SourceName : String Read FSourceName Write FSourceName;
  Published
    Property location : string Read FLocation Write FLocation;
    Property repeated : boolean Read FRepeated write FRepeated;
  end;
  TRestMethodParams = Array of TRestMethodParam;

  { TRestMethod }

  TRestMethod = Class(TGoogleBaseObject)
  private
    FDescription: String;
    FeTagrequired: Boolean;
    FhttpMethod: string;
    Fid: string;
    fMediaUpload: TMediaUpload;
    fname: string;
    FParameterOrder: TStringArray;
    fparameters: TRestMethodParams;
    fpath: string;
    frequest: trequest;
    fresponse: tresponse;
    Fscopes: TStringArray;
    FsupportsMediaDownload: Boolean;
    FsupportsMediaUpload: Boolean;
    FsupportsSubscription: Boolean;
  protected
{$ifdef ver2_6}
    Procedure SetArrayLength(const AName : String; ALength : Longint); override;
{$endif}
  Published
    Property name : string read fname Write fname;
    Property description : String Read FDescription Write FDescription;
    Property etagRequired: Boolean read FeTagrequired write FeTagrequired;
    Property httpMethod: string read FhttpMethod write FhttpMethod;
    Property id : string read Fid Write fID;
    Property MediaUpload : TMediaUpload Read fMediaUpload Write fmediaUpload;
    Property parameterOrder : TStringArray Read FParameterOrder Write FParameterOrder;
    Property parameters : TRestMethodParams read fparameters write fparameters;
    Property path : string read fpath Write fpath;
    Property request : trequest read frequest write frequest;
    Property response : tresponse read fresponse write fresponse;
    property scopes : TStringArray Read Fscopes write Fscopes;
    property supportsMediaDownload : Boolean Read FsupportsMediaDownload Write FsupportsMediaDownload;
    property supportsMediaUpload : Boolean Read FsupportsMediaUpload Write FsupportsMediaUpload;
    property supportsSubscription : Boolean Read FsupportsSubscription Write FsupportsSubscription;
  end;

  { TGoogleRestDescription }

  TGoogleRestDescription = Class(TGoogleBaseObject)
  private
    Fauth: TGoogleAuth;
    fbasePath: string;
    fbaseURL: string;
    fbatchPath: string;
    FCanonicalName: String;
    FDescription: string;
    FdiscoveryVersion: String;
    FdocumentationLink: string;
    FEtag: String;
    Ffeatures: TstringArray;
    Ficons: TGoogleIcons;
    fid: String;
    FKind: String;
    Flabels: TstringArray;
    Fmethods: TRestMethods;
    Fname: String;
    fOwnerDomain: String;
    fOwnerName: String;
    FpackagePath: String;
    Fparameters: TJSONSchema;
    Fprotocol: string;
    FResources: TSchemas;
    FRevision: string;
    frootURL: string;
    FSchemas: TSchemas;
    fservicePath: string;
    FTitle: string;
    Fversion: String;
  Protected
{$ifdef ver2_6}
    Procedure SetArrayLength(const AName : String; ALength : Longint); override;
{$endif}
  Public
  Published
    property Auth : TGoogleAuth Read Fauth Write Fauth;
    property basePath : string read fbasePath write FbasePath;
    property baseUrl : string read fbaseURL write FbaseURL;
    property batchPath : string read fbatchPath write FbatchPath;
    Property canonicalName : String Read FCanonicalName Write FCanonicalName;
    Property description : string Read FDescription Write FDescription;
    Property discoveryVersion : String Read FdiscoveryVersion Write FdiscoveryVersion;
    Property documentationLink : string read FdocumentationLink Write FdocumentationLink;
    Property etag : String Read FEtag Write FEtag;
    Property features : TstringArray Read Ffeatures Write Ffeatures;
    property icons : TGoogleIcons Read Ficons Write Ficons;
    Property id: String Read fid Write Fid;
    Property kind: String Read FKind Write FKind;
    Property labels : TstringArray Read Flabels Write Flabels;
    Property methods : TRestMethods Read Fmethods Write Fmethods;
    Property name : String Read Fname Write Fname;
    Property ownerDomain : String Read fOwnerDomain Write FOwnerDomain;
    Property ownerName : String Read fOwnerName Write FOwnerName;
    Property packagePath : String Read FpackagePath Write FpackagePath;
    Property parameters : TJSONSchema Read Fparameters Write FParameters;
    property protocol : string Read Fprotocol write FProtocol;
    Property resources : TSchemas Read FResources write FResources;
    Property revision : string Read FRevision Write FRevision;
    property rootUrl : string read frootURL write FrootURL;
    property schemas : TSchemas  read FSchemas write fschemas;
    property servicePath : string read fservicePath write FservicePath;
    Property title : string Read FTitle Write FTitle;
    Property version : String Read Fversion Write Fversion;
  end;

  TParamLocation = (plPath,plQuery);
  TParamLocations = Set of TParamLocation;

  TDataType = (dtalias,dtClass,dtarray);

  { TTypeDef }

  TTypeDef = Class(TCollectionItem)
  private
    FDataType: TDataType;
    FItemSchema: TSchema;
    FPascalName: String;
    FSchema: TSchema;
    FTopLevel: Boolean;
  Public
    Property PascalName : String Read FPascalName Write FPascalName;
    Property Schema : TSchema Read FSchema Write FSchema;
    Property DataType : TDataType Read FDataType Write FDataType;
    Property ItemSchema: TSchema Read FItemSchema Write FItemSchema;
    Property TopLevel : Boolean Read FTopLevel Write FTopLevel;
  end;

  { TTypeDefEnumerator }

  TTypeDefEnumerator = Class(TCollectionEnumerator)
  Public
    Function GetCurrent: TTypeDef;
    property Current: TTypeDef read GetCurrent;
  end;

  { TTypeDefs }

  TTypeDefs = Class(TCollection)
  private
    function GetD(AIndex : Integer): TTypeDef;
  Public
    Function GetEnumerator: TTypeDefEnumerator;
    Function IndexOf(ATypename : String) : Integer;
    Function Find(ATypename : String) : TTypeDef;
    Function AddTypeDef(ADataType : TDataType; APascalName : String; ASchema : TSchema) : TTypeDef;
    Property Defs[AIndex : Integer] : TTypeDef Read GetD; Default;
  end;

  TGoogleCodeOption = (gcoUseListForArray,gcoFlatResources);
  TGoogleCodeOptions = Set of TGoogleCodeOption;
  TDiscoveryJSONToPas = Class(TRestCodeGenerator)
  private
    FArrayItemSuffix: String;
    FCodeOptions: TGoogleCodeOptions;
    FDescription: TGoogleRestDescription;
    FPropertyTypeSuffix: String;
    FResourceSuffix: String;
    FTypes : TTypeDefs;
    Function AddType(ASchema: TSchema; NamePrefix: String; IsTopLevel : Boolean) : TTypeDef;
    procedure CollectType(S: TSchema; NamePrefix: String; IsTopLevel : Boolean);
    function GetFlat: Boolean;
    function GetSchemaDataType(S: TSchema): TDataType;
    function GetUseListForArray: Boolean;
    function ReservedMethod(ANAme: String): boolean;
  Protected
    procedure AssignParamNames(Res: TSchema; M: TRestMethod); virtual;
    Function BaseUnits : String; override;
    // Global functions
    Function GetPropertyType(AClassName: String; ASchema: TSchema): String;
    Procedure CollectTypes;
    Procedure CollectTypes(Schemas: TSchemas; NamePrefix : String);
    Procedure ResolveRefs;
    Procedure CreateInterface;
    Procedure CreateImplementation;
    // Schema Classes
    procedure CreateSetArrayLength(AClassName: String; ASchema, AItemSchema: TSchema);
    procedure CreateExportPropertyName(AClassName: String; ASchema,  AItemSchema: TSchema);
    Procedure CreateClassDeclaration(AClassName: String; ASchema: TSchema);
    Procedure CreateArrayClassDeclaration(AClassName: String; ASchema,AItemSchema: TSchema);
    Procedure CreateClassImplementation(AClassName: String; ASchema,AItemSchema: TSchema);
    Procedure CreateArrayClassImplementation(AClassName: String; ASchema,AItemSchema: TSchema);
    Procedure CreateArrayClassEnumeratorImplementation(AClassName: String; ASchema,AItemSchema: TSchema);
    // API class
    Function  GetAPIClassName: String;
    Procedure CreateAPIClassDeclaration;
    Procedure CreateAPIClassImplementation;
    // Resource classes
    Function ConstructResourcePrefix(Const APrefix: String; AResource: TSchema ): String;
    // Recursive calls
    Procedure CreateResourceClassImplementations(Const APrefix: String; Resources: TSchemas);
    Procedure CreateResourceAPIFunctionImplementations(Const AClassName: String; Const APrefix: String; Resources: TSchemas); virtual;
    Procedure CreateResourceInstanceFields(Const APrefix: String; Resources: TSchemas);
    Procedure CreateResourceInstanceGetters(Const APrefix: String; Resources: TSchemas);
    Procedure CreateResourceAPI(Const APrefix: String; Resources: TSchemas);
    Procedure CreateResourceProperties(Const APrefix: String; Resources: TSchemas);
    Function GetResourceClassName(Const APrefix : String; Res: TSchema): String;
    procedure CreateResourceDeclarations(Const APrefix: String; Resources: TSchemas);
    Procedure CreateResourceClassDeclaration(Const APrefix: String; Res: TSchema);
    Procedure CreateResourceClassImplementation(Const APrefix: String; Res: TSchema);
    Procedure CreateResourceClassMethodsImplementation(Res: TSchema; Const AClassName: String);
    Procedure CreateResourceMethodImplementation(AClassName: String; Res: TSchema; M: TRestMethod); // Query version
    Procedure CreateResourceMethodImplementationOptions(Const APrefix,
      AClassName: String; Res: TSchema; M: TRestMethod);// Options record version
    Function GetResourceMethodSignature(M: TRestmethod; Out IsFunction: Boolean; QueryOptionType : String; AddTypes : Boolean = True): String;
    Function DescribeMethodParams(M: TRestMethod): TParamLocations;
    Function HavePathParams(M: TRestMethod): Boolean;
    Function HaveQueryParams(M: TRestMethod): Boolean;
    Procedure CreateResourceMethodQueryParams(APrefix: String; Res: TSchema;
      M: TRestMethod);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure LoadFromStream(Const AStream : TStream); override;
    Procedure LoadFromJSON(Const JSON : TJSONObject); virtual;
    Procedure Execute; override;
    Class Procedure RegisterAllObjects;
    Property APIClassName: String Read GetAPIClassName;
    Property UseListForArray : Boolean Read GetUseListForArray;
    Property FlatResources : Boolean Read GetFlat;
  Published
    Property Description : TGoogleRestDescription Read FDescription;
    Property ResourceSuffix : String Read FResourceSuffix Write FResourceSuffix;
    Property ArrayItemSuffix : String Read FArrayItemSuffix Write FArrayItemSuffix;
    Property PropertyTypeSuffix : String Read FPropertyTypeSuffix Write FPropertyTypeSuffix;
    Property CodeOptions : TGoogleCodeOptions Read FCodeOptions Write FCodeOptions;
  end;


implementation

{ TGoogleRestDescription }

{$IFDEF VER_2_6}
Procedure TGoogleRestDescription.SetArrayLength(const AName: String;
  ALength: Longint);
begin
  case aname of
    'schemas' : setlength(FSchemas,ALength);
    'features' : setlength(FFeatures,ALength);
    'labels' : setlength(FLabels,ALength);
    'methods' : setlength(Fmethods,ALength);
    'resources' : setlength(FResources,ALength);
  else
    inherited SetArrayLength(AName, ALength);
  end;
end;
{$ENDIF}

{ TRestMethod }
{$ifdef ver2_6}
Procedure TRestMethod.SetArrayLength(const AName: String; ALength: Longint);
begin
  case AName of
   'parameterorder' : SetLength(FParameterOrder,ALength);
   'parameters' : SetLength(FParameters,ALength);
   'scopes' : SetLength(FScopes,ALength);
  else
    inherited SetArrayLength(AName, ALength);
  end;
end;

{ TMediaUpload }

Procedure TMediaUpload.SetArrayLength(const AName: String; ALength: Longint);
begin
  Case AName of
    'accept' : SetLength(FAccept,ALength);
  else
    inherited SetArrayLength(AName, ALength);
  end;
end;

{ TGoogleAuth2 }

Procedure TGoogleAuth2.SetArrayLength(const AName: String; ALength: Longint);
begin
  Case AName of
    'scopes' : SetLength(FScopes,ALength);
  else
    inherited SetArrayLength(AName, ALength);
  end;
end;

{ TAnnotations }

Procedure TAnnotations.SetArrayLength(const AName: String; ALength: Longint);
begin
  Case AName of
    'required' :SetLength(FRequired,ALength);
  else
    inherited SetArrayLength(AName, ALength);
  end;
end;
{$endif}

{ TTypeDefEnumerator }

function TTypeDefEnumerator.GetCurrent: TTypeDef;
begin
  Result:=(Inherited GetCurrent) as TTypeDef;
end;

{ TTypeDefs }

function TTypeDefs.GetD(AIndex : Integer): TTypeDef;
begin
  Result:=Items[AIndex] as TTypeDef;
end;

function TTypeDefs.GetEnumerator: TTypeDefEnumerator;
begin
  Result:=TTypeDefEnumerator.Create(Self);
end;

function TTypeDefs.IndexOf(ATypename: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(ATypeName,GetD(Result).PascalName)<>0) do
    Dec(Result);
end;

function TTypeDefs.Find(ATypename: String): TTypeDef;

Var
  I : Integer;

begin
  I:=IndexOf(ATypeName);
  if (I=-1) then
    Result:=Nil
  else
    Result:=GetD(I);
end;

function TTypeDefs.AddTypeDef(ADataType: TDataType; APascalName: String;
  ASchema: TSchema): TTypeDef;
begin
  Result:=Add as TTypeDef;
  Result.DataType:=ADataType;
  Result.PascalName:=APascalName;
  Result.Schema:=ASchema;
end;

{ TSchema }


function TSchema.DoGetTypeName(PrependT : Boolean) : String;

begin
  Result:=FTypeName;
  if (Result='') and Assigned(TypeDef) then
    Result:=TypeDef.PascalName;
  If Result='' then
    begin
    If BaseType then
      Result:=GetBaseTypeName
    else if (_type='array') and Assigned(Items)  then
      begin
      Result:=Items.DoGetTypeName(False);
      if (Result<>'') and not Assigned(Items.TypeDef)  then
        begin
        if PrependT and (items._type<>'object') and (items._type<>'array') then
          Result:='T'+Result;
        Result:=Result+'Array';
        end;
      end;
    end;
end;

function TSchema.GetTypeName: String;
begin
  Result:=DoGetTypeName(True);
end;

Function TSchema.ClassProperties: TProperties;
begin
  If Length(FProperties)>0 then
    Result:=FProperties
  //else if Assigned(Items) then
  //  Result:=Items.properties
  else
    Result:=Nil
end;


{ TGoogleRestDescription }

{ TDiscoveryJSONToPas }

Constructor TDiscoveryJSONToPas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDescription:=TGoogleRestDescription.Create;
  BaseClassName:='TGoogleBaseObject';
  BaseListClassName:='TGoogleBaseObjectList';
  FResourceSuffix:='Resource';
  ArrayItemSuffix:='Item';
  PropertyTypeSuffix:='Type';
//  UseListForArray:=True;
end;

Destructor TDiscoveryJSONToPas.Destroy;
begin
  FreeAndNil(FDescription);
  inherited Destroy;
end;


Procedure TDiscoveryJSONToPas.LoadFromStream(Const AStream: TStream);
Var
  D : TJSONData;
begin
  D:=GetJSON(AStream,True);
  try
    if Not (D is TJSONObject) then
      Raise EGoogleAPI.Create('Source does not contain a valid JSON object');
    LoadFromJSON(D as TJSONObject)
  finally
    D.Free;
  end;
end;

Procedure TDiscoveryJSONToPas.LoadFromJSON(Const JSON: TJSONObject);
begin
  Description.LoadFromJSON(JSON);
end;

function TDiscoveryJSONToPas.GetSchemaDataType(S: TSchema): TDataType;

begin
  if (S._type='array') then
    Result:=dtArray
  else if S._type='object' then
    Result:=dtClass
  else
    Result:=dtAlias;
end;

function TDiscoveryJSONToPas.GetUseListForArray: Boolean;
begin
  Result:=gcoUseListForArray in CodeOPtions;
end;

Function TDiscoveryJSONToPas.AddType(ASchema: TSchema; NamePrefix: String;
  IsTopLevel: Boolean): TTypeDef;

  Function DoAdd(DT : TDataType; TCN : String; S,ITS : TSchema) : TTypeDef;
  Var
    Msg : String;

  begin
    Result:=Nil;
    if (FTypes.IndexOf(TCN)<>-1) then
      Raise Exception.CreateFmt('Type already exists : "%s"',[TCN]);
    Result:=FTypes.AddTypeDef(dt,TCN,S);
    Result.ItemSchema:=ITS;
    Result.Toplevel:=isTopLevel;
    Str(dt,Msg);
    Msg:=Format('[%s] : Adding %s (%s) from ',[NamePrefix,TCN,Msg]);
    if Assigned(S) then
      Msg:=Msg+S.DebugName;
    if Assigned(ItS) then
      Msg:=Msg+Format(' (Array item: %s)',[its.DebugName]);
    DoLog(Msg);
  end;

Var
  CN,TCN : String;
  Dt : TDataType;

begin
  dt:=GetSchemaDataType(ASchema);
  CN:=NamePrefix+ASchema.Name;
  if (dt=dtArray) and (NamePrefix<>'') then
    CN:=CN+'Array';
  TCN:='T'+CN;
  Result:=doAdd(dt,TCN,ASchema,Aschema.Items);
  Aschema.TypeDef:=Result;
  if (dt=dtClass) and isToplevel then
    doAdd(dtArray,TCN+'Array',Nil,Aschema);
end;


procedure TDiscoveryJSONToPas.CollectType(S: TSchema; NamePrefix: String; IsTopLevel : Boolean);

Var
  AE : String;
  Dt : TDataType;
  BaseArrayElement : Boolean;

begin
  dt:=GetSchemaDataType(S);
  BaseArrayElement:=(dt=dtArray) and S.Items.BaseType;
  DoLog('[%s] Examining : %s (Ref : %s type: %s) Toplevel %s',[NamePrefix,S.Name,S.ref,S._type,BoolToStr(IsTopLevel,'True','False')]);
  case dt of
    dtArray : if Not BaseArrayElement and (S.Items.Ref='') then
                begin
                AE:=S.Name;
                if S.Items.Name='' then
                  AE:=AE+ArrayItemSuffix;
                DoLog('Array, adding array item type first (%s)',[AE]);
                CollectType(S.Items,NamePrefix+AE,isTopLevel);
                end;
    dtClass :
      if (S.Ref='') then
        begin
        DoLog('Class type, adding properties first');
        CollectTypes(S.Properties,NamePrefix+S.Name+PropertyTypeSuffix);
        end;
    end;
  if (NamePrefix='') then
    AddType(S,'',True)
  else if (Not S.BaseType) and (Not BaseArrayElement) and (S.Ref='') then
    AddType(S,NamePrefix,IsTopLevel);
end;

function TDiscoveryJSONToPas.GetFlat: Boolean;
begin
  Result:=gcoFlatResources in CodeOPtions;
end;

Procedure TDiscoveryJSONToPas.CollectTypes(Schemas: TSchemas; NamePrefix: String
  );

Var
  S : TSchema;

begin
  For S in Schemas do
    CollectType(S,NamePrefix,NamePrefix='');
end;

Procedure TDiscoveryJSONToPas.ResolveRefs;

Var
  Lookup : TStringList;

  Function DoFind (S : TSchema; C : String) : TSchema;
  Var
    idx : Integer;
  begin
    Idx:=Lookup.IndexOf(S.Ref);
    if idx<>-1 then
      Result:=TSchema(Lookup.Objects[idx])
    else
      Raise Exception.CreateFmt('Could not find reference %s (Context: %s)',[S.Name,C]);
  end;

  Procedure AddSchema(ASchema : TSchema);

  begin
    if Assigned(ASchema) then
      begin
      if (ASchema.Name<>'') then
        begin
        Lookup.AddObject(ASchema.Name,ASchema)
        end;
      end
  end;

  Procedure DoResolve(ASchema : TSchema);

  Var
    S2 : TSchema;
  begin
    if Assigned(ASchema) then
      begin
      if (ASchema.Ref<>'') and (ASchema.Refschema=Nil) then
        ASchema.Refschema:=DoFind(ASchema,ASchema.Name);
      if Assigned(ASchema.Items) then
        DoResolve(ASchema.Items);
      if Length(ASchema.Properties)<>0 then
        For S2 in ASchema.Properties do
          begin
          DoResolve(S2);
          DoResolve(S2.Items);
          end;
      end;
  end;

Var
  T : TTypeDef;
  S : TSchema;

begin
  Lookup:=TStringList.Create;
  try
     for S in Description.Schemas do
       AddSchema(S);
    Lookup.Sorted:=True;
    For T in FTypes do
      begin
      DoResolve(T.Schema);
      DoResolve(T.ItemSchema);
      end;
  finally
    Lookup.Free;
  end;
end;

Procedure TDiscoveryJSONToPas.CollectTypes;

Var
  S : TTypeDef;
  Msg : String;

begin
  CollectTypes(Description.Schemas,ClassPrefix);
  For S in FTypes do
    begin
    Str(S.DataType,Msg);
    DoLog('Created data type %s (%s)',[S.PascalName,Msg]);
    end;
  ResolveRefs;
end;

Function TDiscoveryJSONToPas.BaseUnits: String;
begin
  Result:='googleservice, restbase, googlebase'
end;

Class function TSchema.BaseType(ATypeName: String): Boolean;

begin
  Result:=(ATypeName='boolean') or (ATypeName='string') or (ATypeName='number') or (ATypeName='any') or (ATypeName='integer');
end;

Class function TSchema.GetBaseTypeName(AType, AFormat: String): string;

begin
  Result:=AType;
  if Result='any' then
    Result:='TJSONSchema'
  else if Result='integer' then
    begin
    if (AFormat='int64') then
       Result:='int64'
     else
       Result:='integer'
    end
  else if Result='number' then
    begin
    if (AFormat='double') then
      Result:='double'
    else
      Result:='integer'
    end
  else if Result='string' then
    if Aformat='date-time' then
      Result:='TDatetime'
    else if Aformat='date' then
      Result:='TDate'
    else
      Result:='String';
end;

{$IFDEF VER2_6}
Procedure TSchema.SetArrayLength(const AName: String; ALength: Longint);
begin
  Case AName of
   'enumdescriptions' : SetLength(FenumDescriptions,ALength);
   'properties' : SetLength(FProperties,ALength);
   'methods' : SetLength(FMethods,ALength);
   'resources' : SetLength(FResources,ALength);
  else
    inherited SetArrayLength(AName, ALength);
  end;
end;
{$ENDIF}

Function TSchema.DebugName: String;
begin
  Result:=sysutils.Format('(Name: %s, Pascal Type : %s, type : %s, Ref: %s)',[Name,TypeName,_type,Ref]);
end;

function TSchema.GetBaseTypeName: string;
begin
  Result:=GetBaseTypeName(_type,Format);
end;

Function TSchema.BaseType: Boolean;
begin
  Result:=BaseType(_type)
end;


Function TDiscoveryJSONToPas.GetPropertyType(AClassName: String;
  ASchema: TSchema): String;


begin
  DoLog('Checking property type "%s" property schema %s',[AClassName,ASchema.DebugName]);
  Result:=ASchema.TypeName;
  if (Result='') then
    begin
    Result:=ASchema.ref;
    if Result='' then
      begin
      Result:=ASchema._type;
      if TSchema.BaseType(Result) then
        Result:=ASchema.GetBaseTypeName;
      end
    else if Not Assigned(ASchema.Refschema) then
      Raise Exception.CreateFmt('%s : Unresolved property schema (%s) type reference : %s',[AClassName,ASchema.Name,ASchema.Ref])
    else
      Result:=ASchema.Refschema.TypeName;
    end;
  if Result='array' then
    begin
    if Aschema.Items.TypeDef<>Nil then
      Result:=Aschema.Items.TypeDef.PascalName
    else if (ASchema.Items.Ref<>'') then
      begin
      if not Assigned(ASchema.Items.Refschema) then
        Raise Exception.CreateFmt('%s : Unresolved property (%s) type reference : %s',[AClassName,ASchema.Name,ASchema.Ref]);
      Result:=Aschema.Items.RefSchema.TypeName+'Array';
      end
    else
      Result:=ASchema.Items.TypeName;
    end
  else if Result='object' then
    if (ASchema.ref<>'') then
      Result:='T'+ClassPrefix+ASchema.ref
    else
      Result:=AClassName+ASchema.Name;
end;

Procedure TDiscoveryJSONToPas.CreateClassDeclaration(AClassName: String;
  ASchema: TSchema);

Var
  S : TSchema;
  N : String;
  NeedSetArrayLength, NeedGetWritename : Boolean;
  TN : String;
  Idx,PropertyIndex,PropertyOptions : Integer;
  L : TStringList;

begin
  if ASchema=Nil then
    Raise Exception.Create(AClassName+' : no Schema');
  ClassHeader(AClassName);
  AddLn('%s = Class(%s)',[AClassName,BaseClassName]);
  AddLn('Private');
  NeedGetWriteName:=False;
  NeedSetArrayLength:=False;
  IncIndent;
  L:=TStringList.Create;
  try
    For S in ASchema.Properties do
      begin
      N:=TBaseObject.CleanPropertyName(S.Name);
      Repeat
        Idx:=L.IndexOf(N);
        if (idx=-1) then
          Idx:=L.IndexOf('F'+N);
        if (idx<>-1) then
          begin
          N:='_'+N;
          end;
      Until Idx=-1;
      NeedGetWritename:=NeedGetWritename or (CompareText(N,S.Name)<>0);
      NeedSetArrayLength:=NeedSetArrayLength or (S._type='array');
      S.PropertyName:=N;
      tn:=GetPropertyType(AClassName,S);
      AddLn('F%s : %s;',[N,tn]);
      L.Add(N);
      L.Add('F'+N);
      end;

  finally
    L.Free;
  end;
  DecIndent;
  AddLn('Protected');
  IncIndent;
  if NeedGetWriteName then
    AddLn('Class Function ExportPropertyName(Const AName : String) : string; override;');
  Comment('Property setters');
  For S in ASchema.ClassProperties do
    begin
    N:=S.PropertyName;
    tn:=GetPropertyType(AClassName,S);
    AddLn('Procedure Set%s(AIndex : Integer; AValue : %s); virtual;',[N,tn]);
    end;
  if NeedSetArrayLength and not UseListForArray then
    begin
    Comment('2.6.4. bug workaround');
    Addln('{$IFDEF VER2_6}');
    Addln('Procedure SetArrayLength(Const AName : String; ALength : Longint); override;');
    Addln('{$ENDIF VER2_6}');
    end;
  DecIndent;
  AddLn('Public');
  IncIndent;
  if Assigned(ASchema.additionalProperties) then
    AddLn('Class Function AllowAdditionalProperties : Boolean; override;');
  DecIndent;

  AddLn('Published');
  IncIndent;
  PropertyIndex:=0;
  For S in ASchema.ClassProperties do
    begin
    N:=S.PropertyName;
    Tn:=GetPropertyType(AClassName,S);
    PropertyOptions:=0;
    Idx:=(PropertyIndex shl IndexShift) or PropertyOptions;
    AddLn('Property %s : %s Index %d Read F%s Write Set%s;',[N,TN,Idx, N,N]);
    Inc(PropertyIndex);
    end;
  DecIndent;
  AddLn('end;');
  AddLn('%sClass = Class of %s;',[AClassName,AClassName]);
end;

Procedure TDiscoveryJSONToPas.CreateArrayClassDeclaration(AClassName: String;
  ASchema, AItemSchema: TSchema);
Var
  AEnumeratorName,AItemName : String;

begin
  if AItemSchema=Nil then
    Raise Exception.Create(AClassName+' : no item Schema');
  AItemName:=GetPropertyType('',AItemSchema);
  AEnumeratorName:=AClassName+'Enumerator';
  ClassHeader(AEnumeratorName);
  AddLn('%s = Class(%s)',[AEnumeratorName,'TBaseListEnumerator']);
  AddLn('Public');
  IncIndent;
  AddLn('Function GetCurrent  : %s; ',[AItemName]);
  AddLn('Property Current : %s Read GetCurrent;',[AItemName]);
  DecIndent;
  AddLn('end;');
  AddLn('');
  AddLn('');
  ClassHeader(AClassName);
  AddLn('%s = Class(%s)',[AClassName,BaseListClassName]);
  AddLn('Private');
  IncINdent;
  Addln('Function GetI (AIndex : Integer) : %s;',[AItemName]);
  Addln('Procedure SetI (AIndex : Integer; AValue: %s);',[AItemName]);
  DecIndent;
  AddLn('Protected');
  IncIndent;
  AddLn('Class Function ObjectClass : TBaseObjectClass; override;');
  DecIndent;
  AddLn('Public');
  IncIndent;
  AddLn('Function GetEnumerator : %s;',[AEnumeratorName]);
  AddLn('Property Items[AIndex : Integer] : %s Read GetI Write SetI; default;',[AItemName]);
  DecIndent;
  AddLn('end;');
  AddLn('%sClass = Class of %s;',[AClassName,AClassName]);
end;

procedure TDiscoveryJSONToPas.CreateSetArrayLength(AClassName: String; ASchema, AItemSchema: TSchema);

Var
  S : TSchema;
  N : String;

begin
  Comment('2.6.4. bug workaround');
  Addln('{$IFDEF VER2_6}');
  Addln('Procedure %s.SetArrayLength(Const AName : String; ALength : Longint); ',[AClassName]);
  Addln('');
  AddLn('begin');
  IncIndent;
  AddLn('Case AName of');
  For S in ASchema.ClassProperties do
    if (S._type='array') then
      begin
      N:=S.PropertyName;
      AddLn('''%s'' : SetLength(F%s,ALength);',[Lowercase(N),N]);
      end;
  AddLn('else');
  IncIndent;
  AddLn('Inherited SetArrayLength(AName,ALength);');
  DecIndent;
  AddLn('end;');
  DecIndent;
  AddLn('end;');
  Addln('{$ENDIF VER2_6}');
  Addln('');
end;

procedure TDiscoveryJSONToPas.CreateExportPropertyName(AClassName: String; ASchema, AItemSchema: TSchema);

Var
  S : TSchema;
  N : String;

begin
  begin
  Addln('');
  AddLn('Class Function %s.ExportPropertyName(Const AName : String) :String;',[AClassName]);
  Addln('');
  AddLn('begin');
  IncIndent;
  AddLn('Case AName of');
  For S in ASchema.ClassProperties do
    begin
    N:=S.PropertyName;
    if (CompareText(N,S.Name)<>0) then
      AddLn('''%s'' : Result:=''%s'';',[N,S.Name]);
    end;
  AddLn('else');
  IncIndent;
  AddLn('Result:=Inherited ExportPropertyName(AName);');
  DecIndent;
  AddLn('end;');
  DecIndent;
  AddLn('end;');
  Addln('');
  end;
end;

Procedure TDiscoveryJSONToPas.CreateClassImplementation(AClassName: String;
  ASchema, AItemSchema: TSchema);

Var
  S : TSchema;
  N : String;
  NeedSetArrayLength,NeedGetWritename : Boolean;
  TN : String;

begin
  NeedGetWriteName:=False;
  NeedSetArrayLength:=False;
  ClassHeader(AClassName);
  For S in ASchema.ClassProperties do
    begin
    N:=S.PropertyName;
    NeedSetArrayLength:=NeedSetArrayLength or (S._type='array');
    NeedGetWritename:=NeedGetWritename or (CompareText(N,S.Name)<>0);
    TN:=GetPropertyType(AClassName,S);
    Addln('');
    AddLn('Procedure %s.Set%s(AIndex : Integer; AValue : %s); ',[AClassName,N,tn]);
    SimpleMethodBody([Format('If (F%s=AValue) then exit;',[N]),
                      Format('F%s:=AValue;',[N]),
                      'MarkPropertyChanged(AIndex);']);
    Addln('');
    end;
  if NeedGetWriteName then
    CreateExportPropertyName(AClassName,ASchema, AItemSchema);
  if NeedSetArrayLength and not UseListForArray then
    CreateSetArrayLength(AClassName,ASchema, AItemSchema);
  Addln('');
  if Assigned(ASchema.additionalProperties) then
    begin
    AddLn('Class Function %s.AllowAdditionalProperties : Boolean;',[AClassName]);
    SimpleMethodBody(['Result:=True;']);
    end;
  Addln('');
end;

Procedure TDiscoveryJSONToPas.CreateArrayClassImplementation(
  AClassName: String; ASchema, AItemSchema: TSchema);

Var
  AEnumeratorName,AItemName : String;

begin
  if AItemSchema=Nil then
    Raise Exception.Create(AClassName+' : no Schema');
  CreateArrayClassEnumeratorImplementation(ACLassName,ASchema,AItemSchema);
  AItemName:=GetPropertyType('',AItemSchema);
  AEnumeratorName:=AClassName+'Enumerator';
  ClassHeader(AClassName);
  Addln('');
  Addln('Function %s.GetI (AIndex : Integer) : %s;',[AClassName,AItemName]);
  SimpleMethodBody([Format('Result:=%s(Objects[AIndex]);',[AItemName])]);
  Addln('');
  Addln('Procedure %s.SetI (AIndex : Integer; AValue: %s);',[AClassName,AItemName]);
  SimpleMethodBody(['Objects[AIndex]:=AValue;']);
  Addln('');
  AddLn('Class Function %s.ObjectClass : TBaseObjectClass;',[ACLassName]);
  SimpleMethodBody([Format('Result:=%s;',[AItemName])]);
  Addln('');
  AddLn('Function %S.GetEnumerator : %s;',[ACLassName,AEnumeratorName]);
  SimpleMethodBody([Format('Result:=%s(DoCreateEnumerator(%s));',[AEnumeratorName,AEnumeratorName])]);
  Addln('');
end;

Procedure TDiscoveryJSONToPas.CreateArrayClassEnumeratorImplementation(
  AClassName: String; ASchema, AItemSchema: TSchema);

Var
  AEnumeratorName,AItemName : String;

begin
  if AItemSchema=Nil then
    Raise Exception.Create(AClassName+' : no ItemSchema');
  AItemName:=GetPropertyType('',AItemSchema);
  AEnumeratorName:=AClassName+'Enumerator';
  ClassHeader(AEnumeratorName);
  AddLn('Function %s.GetCurrent  : %s;',[AEnumeratorName,AItemName]);
  SimpleMethodBody([Format('Result:=%s(Inherited GetCurrent);',[AItemName])]);
end;

Function TDiscoveryJSONToPas.GetAPIClassName: String;
begin
  Result:=Format('T%s%sAPI',[ClassPrefix,PrettyPrint(Description.Name)])
end;

Procedure TDiscoveryJSONToPas.CreateInterface;

  procedure AddTypeDecl(S : TTypeDef);

  begin
    Case S.DataType  of
      dtAlias : AddLn('%s = %s;',[S.PascalName,S.Schema.GetBaseTypeName]);
      dtArray :
        if UseListForArray then
          AddLn('%s = Class;',[S.PascalName])
        else
          AddLn('%s = Array of %s;',[S.PascalName,GetPropertyType('',S.ItemSchema)]);
      dtClass : AddLn('%s = Class;',[S.PascalName]);
    end;
  end;

  Procedure DoTypeLoops(DoTopLevel : Boolean);

  Var
    S : TTypeDef;
  begin
    For S in FTypes do
      if (S.DataType=dtAlias) and (S.TopLevel=DoTopLevel) then
        AddTypeDecl(S);
    For S in FTypes do
      if (S.DataType=dtClass) and (S.TopLevel=DoTopLevel) then
        AddTypeDecl(S);
    For S in FTypes do
      if (S.DataType=dtArray) and (S.TopLevel=DoTopLevel) then
        AddTypeDecl(S);
  end;

Var
  S : TTypeDef;

begin
  Addln('type');
  IncIndent;
  AddLn('');
  Comment('Top-level schema types');
  DoTypeLoops(True);
  Comment('Anonymous types, using auto-generated names');
  DoTypeLoops(False);
  For S in FTypes do
    if S.DataType=dtClass then
      CreateClassDeclaration(S.PascalName,S.Schema)
    else if (S.DataType=dtArray) and UseListForArray then
      CreateArrayClassDeclaration(S.PascalName,S.Schema,S.ItemSchema);
  CreateResourceDeclarations('',Description.Resources);
  CreateAPIClassDeclaration;
  DecIndent;
end;

procedure TDiscoveryJSONToPas.CreateResourceDeclarations(Const APrefix : String; Resources : TSchemas);

var
  R : TSchema;

begin
  For R in Resources do
    begin
    if Assigned(R.resources) then
      if FlatResources then
        CreateResourceDeclarations(APrefix,R.Resources)
      else
        CreateResourceDeclarations(APrefix+PrettyPrint(R.Name),R.Resources);
    R.TypeName:=GetResourceClassName(APrefix,R);
    CreateResourceClassDeclaration(APrefix,R);
    end;
end;

Procedure TDiscoveryJSONToPas.CreateImplementation;

Var
  S : TTypeDef;
begin
  For S in FTypes do
    Case S.DataType of
      dtClass: CreateClassImplementation(S.PascalName,S.Schema,S.ItemSchema);
      dtArray: if UseListForArray then
       CreateArrayClassImplementation(S.PascalName,S.Schema,S.ItemSchema);
    end;
  CreateResourceClassImplementations('',Description.Resources);
  CreateAPIClassImplementation;
end;

Procedure TDiscoveryJSONToPas.CreateResourceClassImplementations(Const APrefix : String; Resources : TSchemas);

var
  R : TSchema;

begin
  For R in Resources do
    begin
    if Assigned(R.Resources) then
      CreateResourceClassImplementations(ConstructResourcePrefix(APrefix,R),R.Resources);
    CreateResourceClassImplementation(APrefix,R);
    end;
end;

Function TDiscoveryJSONToPas.GetResourceMethodSignature(M: TRestmethod; Out
  IsFunction: Boolean; QueryOptionType: String; AddTypes: Boolean): String;

Const
  Seps : Array[Boolean] of String = (',','; ');

  Procedure AddSep(Var S : String);

  begin
    if (S<>'') then
      S:=S+Seps[AddTypes];
  end;

Var
  P : TRestMethodParam;
  S : String;
  Q : Boolean; // Was AQuery encountered in options

begin
  Q:=False;
  S:='';
  For P in M.parameters do
    if (P.Location='path') and P.Required then
      begin
      Q:=Q or (CompareText(p.SourceName,'aquery')=0);
      AddSep(S);
      S:=S+P.SourceName;
      If AddTypes then
        S:=S+': '+P._type;
      end;
  For P in M.parameters do
    if (P.Location='path') and not P.Required then
      begin
      Q:=Q or (CompareText(p.SourceName,'aquery')=0);
      AddSep(S);
      S:=S+P.SourceName;
      if AddTypes then
        begin
        S:=S+': '+P._type + ' = ';
        Case p._type of
          'string' : S:=S+'''''';
          'number' : S:=S+'0';
          'object' : S:=S+'nil';
        end;
        end;
      end;
  if M.Request<>Nil then
    begin
    AddSep(S);
    S:=S+'a'+M.request.ref;
    If AddTypes then
      S:=S+' : T'+ClassPrefix+M.request.ref;
    end;
  if (QueryOptionType<>'') then
    begin
    AddSep(S);
    if AddTypes then
      S:=S+'AQuery : '+QueryOptionType
    else
      S:=S+QueryOptionType; // Will be name instead of type;
    end;
  if (S<>'') then
    S:='('+S+')';
  S:=PrettyPrint(TBaseObject.CleanPropertyName(M.Name))+S;
  isFunction:=M.Response<>Nil;
  if isFunction and AddTypes then
    S:=S+' : T'+ClassPrefix+M.response.ref;
  Result:=S;
end;

Function TDiscoveryJSONToPas.DescribeMethodParams(M: TRestMethod
  ): TParamLocations;
Var
  P : TRestMethodParam;

begin
  Result:=[];
  For P in M.Parameters do
    Case P.location of
     'path' : Include(Result,plPath);
     'query' : Include(Result,plQuery);
    end;
end;


Function TDiscoveryJSONToPas.HavePathParams(M: TRestMethod): Boolean;

begin
  Result:=plPath in DescribeMethodParams(M);
end;


Procedure TDiscoveryJSONToPas.CreateResourceMethodQueryParams(APrefix : String; Res: TSchema;
  M: TRestMethod);

Var
  P : TRestMethodParam;
  RN,RCN,MN : String;

begin
  RN:=APrefix+PrettyPrint(Res.Name);
  RCN:=Res.TypeName;
  MN:=PrettyPrint(M.Name);
  Addln('');
  Comment(Format('Optional query Options for %s, method %s',[RCN,MN]));
  Addln('');
  Addln('T%s%sOptions = Record',[RN,MN]);
  IncIndent;
  For P in M.parameters do
    begin
    if (P.location='query') then
      if p.format='int64' then
        AddLn('%s : int64;',[P.Sourcename])
      else
        AddLn('%s : %s;',[P.sourcename,P.GetBaseTypeName]);
    end;
  DecIndent;
  Addln('end;');
  Addln('');
end;

Function TDiscoveryJSONToPas.HaveQueryParams(M: TRestMethod): Boolean;

begin
  Result:=plQuery in DescribeMethodParams(M);
end;


Function TDiscoveryJSONToPas.GetResourceClassName(Const APrefix: String;
  Res: TSchema): String;
Var
  Suffix : String;
begin
  if (Res.TypeName<>'') and (Res.TypeName<>Res.Name) then
    Result:=Res.TypeName
  else
    begin
    Suffix:='Resource';
    Repeat
      Result:=Format('T%s%s%s%s',[ClassPrefix,APrefix,PrettyPrint(Res.Name),Suffix]);
      Suffix:='_'+Suffix;
    Until FTypes.IndexOf(Result)=-1;
    end
end;

procedure TDiscoveryJSONToPas.AssignParamNames(Res: TSchema; M: TRestMethod);

// Google API has case sensitive names. We need to make sure the names are unique in a case insensitive manner.
// This is done by assigning the 'source name'

Var
  T : TStringList;
  P : TRestMethodParam;
  N : String;

begin
  T:=TStringList.Create;
  try
    // The request also has a parameter name
    If Assigned(M.request) then
      T.Add('a'+M.request.ref);
    // Some identifiers that are 'reserved' by the base classes.
    With T do
      begin
      Add('Name');
      Add('ResourceName');
      Add('DefaultAPI');
      Add('API');
      Add('Notification');
      end;
    // Actual paramters
    For P in M.parameters do
      begin
      N:=P.Name;
      While T.IndexOf(N)<>-1 do
        begin
        N:='_'+N;
        end;
      T.Add(N);
      P.SourceName:=TbaseObject.CleanPropertyName(N);
      end;
  finally
    T.Free;
  end;
end;

function TDiscoveryJSONToPas.ReservedMethod(ANAme: String): boolean;

begin
  AName:=';'+lowerCase(AName)+';';
  Result:=Pos(AName,';create;destroy;free;')<>0;
end;

Procedure TDiscoveryJSONToPas.CreateResourceClassDeclaration(
  Const APrefix: String; Res: TSchema);

Var
  M : TRestMethod;
  CN,S : String;
  HaveOpt : Boolean;
  IsFunc : Boolean;


begin
  CN:=Res.TypeName;
  ClassHeader(CN);
  For M in Res.methods do
    begin
    AssignParamNames(Res,M);
    if HaveQueryParams(M) then
      begin
      CreateResourceMethodQueryParams(APrefix,Res,M);
      end;
    end;
  Addln('%s = Class(TGoogleResource)',[CN]);
  if Assigned(Res.Resources) then
    begin
    Addln('Private');
    IncIndent;
    CreateResourceInstanceFields('',Res.Resources);
    CreateResourceInstanceGetters('',Res.Resources);
    DecIndent;
    end;
  Addln('Public');
  IncIndent;
  AddLn('Class Function ResourceName : String; override;');
  AddLn('Class Function DefaultAPI : TGoogleAPIClass; override;');
  For M in Res.methods do
    begin
    HaveOpt:=HaveQueryParams(M);
    if HaveOpt then
      S:=GetResourceMethodSignature(M,IsFunc,'string  = ''''')
    else
      S:=GetResourceMethodSignature(M,IsFunc,'');
    if IsFunc then
      S:='Function '+S
    else
      S:='Procedure '+S;
    if ReservedMethod(M.Name) then
      S:=S+';overload';
    AddLn(S+';');
    if HaveOpt then
      begin
      S:=GetResourceMethodSignature(M,IsFunc,Format('T%s%sOptions',[APrefix+PrettyPrint(Res.Name),M.Name]));
      if IsFunc then
        S:='Function '+S
      else
        S:='Procedure '+S;
      if ReservedMethod(M.Name) then
        S:=S+';overload';
      AddLn(S+';');
      end;
    end;
  if Assigned(Res.Resources) then
    begin
    CreateResourceAPI('',Res.resources);
    CreateResourceProperties('',Res.resources);
    end;
  DecIndent;
  Addln('end;',[Res.name]);
  AddLn('');
end;

Procedure TDiscoveryJSONToPas.CreateResourceClassMethodsImplementation(
  Res: TSchema; Const AClassName: String);

begin
  AddLn('');
  Addln('Class Function %s.ResourceName : String;',[AClassName]);
  AddLn('');
  AddLn('begin');
  IncIndent;
  AddLn('Result:=''%s'';',[Res.name]);
  DecIndent;
  AddLn('end;');
  AddLn('');
  Addln('Class Function %s.DefaultAPI : TGoogleAPIClass;',[AClassName]);
  AddLn('');
  AddLn('begin');
  IncIndent;
  AddLn('Result:=T%s%sAPI;',[ClassPrefix,Description.Name]);
  DecIndent;
  AddLn('end;');
  AddLn('');
end;

Procedure TDiscoveryJSONToPas.CreateResourceMethodImplementationOptions(Const APrefix,AClassName: String; Res: TSchema; M: TRestMethod);

Var
  P : TRestMethodParam;
  S: String;
  IsFunc : Boolean;
begin
  S:=GetResourceMethodSignature(M,IsFunc,Format('T%s%sOptions',[APrefix+PrettyPrint(Res.Name),M.Name]));
  S:=AClassName+'.'+S;
  if IsFunc then
    S:='Function '+S
  else
    S:='Procedure '+S;
  Addln('');
  AddLn(S+';');
  Addln('');
  AddLn('Var');
  IncIndent;
  Addln('_Q : String;');
  DecIndent;
  Addln('');
  AddLn('begin');
  IncIndent;
  AddLn('_Q:='''';');
  For P in M.parameters do
    begin
    if (P.location='query') then
      AddLn('AddToQuery(_Q,''%s'',AQuery.%s);',[P.name,P.SourceName]);
    end;
  S:=GetResourceMethodSignature(M,IsFunc,'_Q',False);
  if IsFunc then
    S:='Result:='+S;
  AddLn(S+';');
  DecIndent;
  AddLn('end;');
  Addln('');
end;

Procedure TDiscoveryJSONToPas.CreateResourceMethodImplementation(
  AClassName: String; Res: TSchema; M: TRestMethod);

Var
  P : TRestMethodParam;
  Q,RC,O,S,PA : String;
  IsFunc : Boolean;
  PL: TParamLocations;

begin
  PL:=DescribeMethodParams(M);
  if plQuery in PL then
    S:=GetResourceMethodSignature(M,IsFunc,'string = ''''')
  else
    S:=GetResourceMethodSignature(M,IsFunc,'');
  S:=AClassName+'.'+S;
  if IsFunc then
    S:='Function '+S
  else
    S:='Procedure '+S;
  AddLn(S+';');
  AddLn('');
  AddLn('Const');
  IncIndent;
  AddLn('_HTTPMethod = ''%s'';',[M.httpMethod]);
  AddLn('_Path       = ''%s'';',[M.path]);
  AddLn('_Methodid   = ''%s'';',[M.id]);
  DecIndent;
  AddLn('');
  if (plPath in PL) then
    begin
    AddLn('Var');
    IncIndent;
    Addln('_P : String;');
    DecIndent;
    Addln('');
    end;
  Addln('begin');
  IncIndent;
  S:='';
  PA:='_Path';
  if (plPath in PL) then
    begin
    for P in M.parameters do
      if P.location='path' then
        begin
        if (S<>'') then
          S:=S+',';
        S:=S+Format('''%s'',%s',[p.name,p.sourcename]);
        end;
    AddLn('_P:=SubstitutePath(_Path,[%s]);',[S]);
    PA:='_P';
    end;
  if M.request<>Nil then
    O:='a'+M.request.ref
  else
    O:='Nil';
  if (M.response<>Nil) then
    RC:='T'+ClassPrefix+M.response.ref
  else
    RC:='Nil';
  if (plQuery in PL) then
    Q:='AQuery'
  else
    Q:='''''';
  S:=Format('ServiceCall(_HTTPMethod,%s,%s,%s,%s)',[PA,Q,O,RC]);
  if isFunc then
    S:='Result:='+S+' as T'+ClassPrefix+M.response.ref;
  AddLn(S+';');
  DecIndent;
  Addln('end;');
  Addln('');
end;

Procedure TDiscoveryJSONToPas.CreateResourceClassImplementation(Const APrefix : String; Res: TSchema);

Var
  CN : String;
  M : TRestMethod;
  PL: TParamLocations;


begin
  CN:=Res.TypeName;
  ClassHeader(CN);
  CreateResourceClassMethodsImplementation(Res,CN);
  For M in Res.methods do
    begin
    PL:=DescribeMethodParams(M);
    CreateResourceMethodImplementation(CN,Res,M);
    if plQuery in PL then
      CreateResourceMethodImplementationOptions(APrefix,CN,Res,M);
    end;
  AddLn('');
  if Assigned(Res.resources) then
    CreateResourceAPIFunctionImplementations(CN,'',Res.resources);
end;

Procedure TDiscoveryJSONToPas.CreateResourceInstanceFields(Const APrefix : String;Resources : TSchemas);

Var
  R : TSchema;

begin
  For R in Resources do
    begin
    If Assigned(R.Resources) then
      CreateResourceInstanceFields(ConstructResourcePrefix(APrefix,R),R.Resources);
    AddLn('F%sInstance : %s;',[APrefix+PrettyPrint(R.Name),R.TypeName]);
    end;
end;

Function TDiscoveryJSONToPas.ConstructResourcePrefix(Const APrefix : String; AResource : TSchema) : String;

begin
  if FlatResources then
    Result:=APrefix
  else
    Result:=APrefix+PrettyPrint(AResource.Name);
end;

Procedure TDiscoveryJSONToPas.CreateResourceInstanceGetters(Const APrefix : String; Resources : TSchemas);

Var
  R : TSchema;

begin
  For R in Resources do
    begin
    If Assigned(R.Resources) then
      CreateResourceInstanceGetters(ConstructResourcePrefix(APrefix,R),R.Resources);
    AddLn('Function Get%sInstance : %s;virtual;',[APrefix+PrettyPrint(R.Name),R.TypeName]);
    end;
end;

Procedure TDiscoveryJSONToPas.CreateResourceProperties(Const APrefix : String; Resources : TSchemas);

Var
  R : TSchema;
begin
  For R in Resources do
    begin
    If Assigned(R.Resources) then
      CreateResourceProperties(ConstructResourcePrefix(APrefix,R),R.Resources);
    AddLn('Property %sResource : %s Read Get%sInstance;',[APrefix+PrettyPrint(R.Name),R.TypeName,APrefix+PrettyPrint(R.Name)]);
    end;
end;

Procedure TDiscoveryJSONToPas.CreateResourceAPI(Const APrefix : String; Resources : TSchemas);

Var
  R : TSchema;

begin
  For R in Resources do
    begin
    If Assigned(R.Resources) then
      CreateResourceAPI(ConstructResourcePrefix(APrefix,R),R.Resources);
    AddLn('Function Create%sResource(AOwner : TComponent) : %s;virtual;overload;',[APrefix+PrettyPrint(R.Name),R.TypeName]);
    AddLn('Function Create%sResource : %s;virtual;overload;',[APrefix+PrettyPrint(R.Name),R.TypeName]);
    end;
end;

Procedure TDiscoveryJSONToPas.CreateAPIClassDeclaration;

Var
  CN : String;

begin
  CN:=GetAPIClassName;
  Classheader(CN);
  AddLn('%s = Class(TGoogleAPI)',[CN]);
  AddLn('Private');
  IncIndent;
  CreateResourceInstanceFields('',Description.resources);
  CreateResourceInstanceGetters('',Description.resources);
  DecINdent;
  AddLn('Public');
  IncIndent;
  Comment('Override class functions with API info');
  AddLn('Class Function APIName : String; override;');
  AddLn('Class Function APIVersion : String; override;');
  AddLn('Class Function APIRevision : String; override;');
  AddLn('Class Function APIID : String; override;');
  AddLn('Class Function APITitle : String; override;');
  AddLn('Class Function APIDescription : String; override;');
  AddLn('Class Function APIOwnerDomain : String; override;');
  AddLn('Class Function APIOwnerName : String; override;');
  AddLn('Class Function APIIcon16 : String; override;');
  AddLn('Class Function APIIcon32 : String; override;');
  AddLn('Class Function APIdocumentationLink : String; override;');
  AddLn('Class Function APIrootUrl : string; override;');
  AddLn('Class Function APIbasePath : string;override;');
  AddLn('Class Function APIbaseURL : String;override;');
  AddLn('Class Function APIProtocol : string;override;');
  AddLn('Class Function APIservicePath : string;override;');
  AddLn('Class Function APIbatchPath : String;override;');
  AddLn('Class Function APIAuthScopes : TScopeInfoArray;override;');
  AddLn('Class Function APINeedsAuth : Boolean;override;');
  AddLn('Class Procedure RegisterAPIResources; override;');
  Comment('Add create function for resources');
  CreateResourceAPI('',Description.resources);
  Comment('Add default on-demand instances for resources');
  CreateResourceProperties('',Description.resources);
  DecIndent;
  AddLn('end;');
end;


Procedure TDiscoveryJSONToPas.CreateAPIClassImplementation;

  Procedure StringRes(AValue : String);

  begin
    SimpleMethodBody([Format('Result:=%s;',[MakePascalString(AValue,True)])]);
  end;

Var
  CN : String;
  S : TSchema;
  I : Integer;

begin
  CN:=GetAPIClassName;
  ClassHeader(CN);
  AddLn('Class Function %s.APIName : String;',[CN]);
  StringRes(Description.name);
  AddLn('Class Function %s.APIVersion : String;',[CN]);
  StringRes(Description.version);
  AddLn('Class Function %s.APIRevision : String;',[CN]);
  StringRes(Description.revision);
  AddLn('Class Function %s.APIID : String;',[CN]);
  StringRes(Description.id);
  AddLn('Class Function %s.APITitle : String;',[CN]);
  StringRes(Description.Title);
  AddLn('Class Function %s.APIDescription : String;',[CN]);
  StringRes(Description.Description);
  AddLn('Class Function %s.APIOwnerDomain : String;',[CN]);
  StringRes(Description.ownerDomain);
  AddLn('Class Function %s.APIOwnerName : String;',[CN]);
  StringRes(Description.ownerName);
  AddLn('Class Function %s.APIIcon16 : String;',[CN]);
  StringRes(Description.icons.x16);
  AddLn('Class Function %s.APIIcon32 : String;',[CN]);
  StringRes(Description.icons.x32);
  AddLn('Class Function %s.APIdocumentationLink : String;',[CN]);
  StringRes(Description.documentationLink);
  AddLn('Class Function %s.APIrootUrl : string;',[CN]);
  StringRes(Description.rootUrl);
  AddLn('Class Function %s.APIbasePath : string;',[CN]);
  StringRes(Description.BasePath);
  AddLn('Class Function %s.APIbaseURL : String;',[CN]);
  StringRes(Description.BaseURL);
  AddLn('Class Function %s.APIProtocol : string;',[CN]);
  StringRes(Description.protocol);
  AddLn('Class Function %s.APIservicePath : string;',[CN]);
  StringRes(Description.servicePath);
  AddLn('Class Function %s.APIbatchPath : String;',[CN]);
  StringRes(Description.batchPath);
  AddLn('Class Function %s.APIAuthScopes : TScopeInfoArray;',[CN]);
  Addln('');
  AddLn('begin');
  IncIndent;
  if not (Assigned(Description.Auth) and Assigned(Description.Auth.oauth2)) then
    AddLn('SetLength(Result,0);')
  else
    begin
    AddLn('SetLength(Result,%d);',[Length(Description.Auth.oauth2.Scopes)]);
    For I:=0 to Length(Description.Auth.oauth2.Scopes)-1 do
      begin
      S:=Description.Auth.oauth2.Scopes[i];
      AddLn('Result[%d].Name:=%s;',[I,MakePascalString(S.Name,True)]);
      AddLn('Result[%d].Description:=%s;',[I,MakePascalString(S.Description,True)]);
      end;
    end;
  Addln('');
  DecIndent;
  Addln('end;');
  Addln('');
  AddLn('Class Function %s.APINeedsAuth : Boolean;',[CN]);
  SimpleMethodBody([Format('Result:=%s;',[BoolToStr(Assigned(Description.Auth) and Assigned(Description.Auth.oauth2),'True','False')])]);
  AddLn('Class Procedure %s.RegisterAPIResources;',[CN]);
  Addln('');
  AddLn('begin');
  IncIndent;
  For I:=0 to FTypes.Count-1 do
    if FTypes[i].DataType=dtClass then
      AddLn('%s.RegisterObject;',[FTypes[i].PascalName]);
  DecIndent;
  Addln('end;');
  Addln('');
  CreateResourceAPIFunctionImplementations(GetAPIClassName,'',Description.resources);
end;

Procedure TDiscoveryJSONToPas.CreateResourceAPIFunctionImplementations(Const AClassName : String; Const APrefix : String; Resources : TSchemas);

Var
  RN,CN,RCN : String;
  R : TSchema;

begin
  CN:=AClassName;
  For R in Resources do
    begin
    if Assigned(R.Resources) then
      CreateResourceAPIFunctionImplementations(CN,ConstructResourcePrefix(APrefix,R),R.Resources);
    RN:=APrefix+PrettyPrint(R.Name);
    RCN:=R.TypeName;
    AddLn('');
    AddLn('Function %s.Get%sInstance : %s;',[CN,RN,RCN]);
    AddLn('');
    AddLn('begin');
    IncIndent;
    AddLn('if (F%sInstance=Nil) then',[RN]);
    IncIndent;
    AddLn('F%sInstance:=Create%sResource;',[RN,RN]);
    DecIndent;
    AddLn('Result:=F%sInstance;',[RN]);
    DecIndent;
    AddLn('end;');
    AddLn('');
    AddLn('Function %s.Create%sResource : %s;',[CN,RN,RCN]);
    SimpleMethodBody([Format('Result:=Create%sResource(Self);',[RN])]);
    AddLn('');
    AddLn('Function %s.Create%sResource(AOwner : TComponent) : %s;',[CN,RN,RCN]);
    SimpleMethodBody([Format('Result:=%s.Create(AOwner);',[RCN]),
                             'Result.API:=Self.API;']);
    AddLn('');
    end;
end;

Procedure TDiscoveryJSONToPas.Execute;

begin
  Source.Clear;
  Addln('unit '+outputunitname+';');
  CreateHeader;
  FTypes:=TTypeDefs.Create(TTypeDef);
  try
    CollectTypes;
    CreateInterface;
    AddLn('');
    AddLn('implementation');
    AddLn('');
    CreateImplementation;
    Addln('');
    AddLn('initialization');
    Addln('  %s.RegisterAPI;',[GetAPIClassName]);
  finally
    FTypes.Free;
  end;
  AddLn('end.');
end;

Class Procedure TDiscoveryJSONToPas.RegisterAllObjects;
begin
  TGoogleIcons.RegisterObject;
  TGoogleAuth2.RegisterObject;
  TGoogleAuth.RegisterObject;
  TArrayPropertyDef.RegisterObject;
  TPropertyDef.RegisterObject;
  TSchema.RegisterObject;
  TGoogleRestDescription.RegisterObject;
  TAnnotations.RegisterObject;
  TRestMethod.RegisterObject;
  TRestMethodParam.RegisterObject;
  TMediaUpload.RegisterObject;
  TMediaUploadProtocols.RegisterObject;
  TMediaUploadProtocolsSimple.RegisterObject;
  TMediaUploadProtocolsResumable.RegisterObject;
  TRequest.RegisterObject;
  TResponse.RegisterObject;
end;


end.

