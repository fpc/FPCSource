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
  TSchema = Class;
  TSchemas = Array of TSchema;
  TPropertyDef = Class;
  TProperties = Array of TPropertyDef;
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
    fitems: TArrayPropertyDef;
    fmaximum: integer;
    fmaxItems: integer;
    fmaxlength: integer;
    Fmethods: TRestMethods;
    fminimum: integer;
    fminItems: integer;
    fminlength: integer;
    FName: String;
    FPascalName: String;
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
    funiqueItems: boolean;
    FVariant: TJSONSchema;
    function GetPascalName: String;
  Public
    Function ClassProperties : TProperties;
    Property PropertyName : String Read FPropertyName Write FPropertyName;
    Property PascalName : String Read GetPascalName Write FPascalName;
    Property Refschema : TSchema Read FRefSchema Write FRefSchema;
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
    Property items : TArrayPropertyDef Read fitems write fitems;
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

  TDiscoveryJSONToPas = Class(TRestCodeGenerator)
  private
    FDescription: TGoogleRestDescription;
    FResourceSuffix: String;
    FClasses : TStrings;
    function BaseType(ATypeName: String): Boolean;
    function GetBaseTypeName(AType, AFormat: String): string;
    function GetBaseTypeName(ASchema: TPropertyDef): string;
    function ReservedMethod(ANAme: String): boolean;
  Protected
    procedure AssignParamNames(Res: TSchema; M: TRestMethod); virtual;
    Function BaseUnits : String; override;
    // Global functions
    Function GetPropertyType(AClassName: String; ASchema: TPropertyDef): String;
    Procedure CollectClasses;
    Procedure CollectClasses(Schemas: TSchemas; NamePrefix : String);
    Procedure ResolveRefs;
    Procedure CreateInterface(ClassList: TStrings);
    Procedure CreateImplementation(ClassList: TStrings);
    // Schema Classes
    Procedure CreateClassDeclaration(AClassName: String; ASchema: TSchema);
    Procedure CreateClassImplementation(AClassName: String; ASchema: TSchema);
    // API class
    Function  GetAPIClassName: String;
    Procedure CreateAPIClassDeclaration;
    Procedure CreateAPIClassImplementation;
    Procedure CreateAPIResourceFunctionImplementations; virtual;
    // Resource classes
    Function GetResourceClassName(Res: TSchema; AClasses : TStrings): String;
    Procedure CreateResourceClassDeclaration(Res: TSchema);
    Procedure CreateResourceClassImplementation(Res: TSchema);
    Procedure CreateResourceClassMethodsImplementation(Res: TSchema; Const AClassName: String);
    Procedure CreateResourceMethodImplementation(AClassName: String; Res: TSchema; M: TRestMethod); // Query version
    Procedure CreateResourceMethodImplementationOptions(AClassName: String; Res: TSchema; M: TRestMethod);// Options record version
    Function GetResourceMethodSignature(M: TRestmethod; Out IsFunction: Boolean; QueryOptionType : String; AddTypes : Boolean = True): String;
    Function DescribeMethodParams(M: TRestMethod): TParamLocations;
    Function HavePathParams(M: TRestMethod): Boolean;
    Function HaveQueryParams(M: TRestMethod): Boolean;
    Procedure CreateResourceMethodQueryParams(Res: TSchema; M: TRestMethod);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure LoadFromStream(Const AStream : TStream); override;
    Procedure LoadFromJSON(Const JSON : TJSONObject); virtual;
    Procedure Execute; override;
    Class Procedure RegisterAllObjects;
    Property APIClassName: String Read GetAPIClassName;
  Published
    Property Description : TGoogleRestDescription Read FDescription;
    Property ResourceSuffix : String Read FResourceSuffix Write FResourceSuffix;
  end;


implementation

{ TSchema }

function TSchema.GetPascalName: String;
begin
  Result:=FPascalName;
  If Result='' then
    Result:=Name;
end;

function TSchema.ClassProperties: TProperties;
begin
  If Length(FProperties)>0 then
    Result:=FProperties
  else if Assigned(Items) then
    Result:=Items.properties
  else
    Result:=Nil
end;


{ TGoogleRestDescription }

{ TDiscoveryJSONToPas }

constructor TDiscoveryJSONToPas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDescription:=TGoogleRestDescription.Create;
  BaseClassName:='TGoogleBaseObject';
  FResourceSuffix:='Resource';
end;

destructor TDiscoveryJSONToPas.Destroy;
begin
  FreeAndNil(FDescription);
  inherited Destroy;
end;


procedure TDiscoveryJSONToPas.LoadFromStream(const AStream: TStream);
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

procedure TDiscoveryJSONToPas.LoadFromJSON(const JSON: TJSONObject);
begin
  Description.LoadFromJSON(JSON);
end;


procedure TDiscoveryJSONToPas.CollectClasses(Schemas: TSchemas;
  NamePrefix: String);

Var
  S : TSchema;
  CN : String;
begin
  For S in Schemas do
    begin
    // Write('Examining : ',S.Name,' (NamePrefix: ',NamePrefix,' Ref : ',S.ref,', type: ',S._type,')');
    if (NamePrefix='') or ((s.ref='') and ((S._type='object') or (S._type='array'))) then
      begin
      CN:=NamePrefix+S.Name;
      S.PascalName:='T'+CN;
      if FClasses.IndexOf(S.PascalName)=-1 then
        FClasses.AddObject(S.PascalName,S);
      // Writeln(': Added as ',S.PascalName);
      CollectClasses(TSchemas(S.ClassProperties),CN);
      end
    else
      // Writeln
      ;
    end;
end;

procedure TDiscoveryJSONToPas.ResolveRefs;

Var
  I : Integer;
  Lookup : TStringList;
  S,S2 : TSchema;

  Function DoFind (Const N,C : String) : TSchema;
  Var
    idx : Integer;
  begin
    // Writeln('Resolving ',S.Ref);
    Idx:=Lookup.IndexOf(N);
    if idx<>-1 then
      Result:=TSchema(Lookup.Objects[idx])
    else
      Raise Exception.CreateFmt('Could not find reference %s (Context: %s)',[N,C]);
  end;

begin
  // Writeln('Resolving ');
  Lookup:=TStringList.Create;
  try
    For I:=0 to FClasses.Count-1 do
      begin
      S:=TSchema(FCLasses.Objects[i]);
      //  Writeln('Found ',FClasses[i],' : ',S.ID,' (original: ',S.Name,', pascal: ', S.PascalName,')');
      if (S.Name<>'') then
        Lookup.AddObject(S.Name,S);
      end;
    Lookup.Sorted:=True;
    For I:=0 to FClasses.Count-1 do
      begin
      S:=TSchema(FClasses.Objects[i]);
      if (S.Ref<>'') then
        S.Refschema:=DoFind(S.Ref,S.Name);
      if Length(S.Classproperties)<>0 then
        For S2 in S.Classproperties do
          if (S2.Ref<>'') then
            begin
            // Writeln('Resolving property ',S.Name, ' : ',S2.Ref);
            S2.Refschema:=DoFind(S2.Ref,'Property '+S.Name);
            end;
      end;
  finally
    Lookup.Free;
  end;
end;

procedure TDiscoveryJSONToPas.CollectClasses;

begin
  CollectClasses(Description.Schemas,ClassPrefix);
  ResolveRefs;
end;

function TDiscoveryJSONToPas.BaseUnits: String;
begin
  Result:='googleservice, restbase, googlebase'
end;

function TDiscoveryJSONToPas.BaseType(ATypeName: String) : Boolean;

begin
  Result:=(ATypeName='string') or (ATypeName='number') or (ATypeName='any');
end;

function TDiscoveryJSONToPas.GetBaseTypeName(AType,AFormat : String) : string;

begin
  Result:=AType;
  if Result='any' then
    Result:='TJSONSchema'
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
      Result:='TDate';
end;

function TDiscoveryJSONToPas.GetBaseTypeName(ASchema: TPropertyDef) : string;

begin
  Result:=GetBaseTypeName(ASchema._Type,ASchema.Format);
end;

function TDiscoveryJSONToPas.GetPropertyType(AClassName: String; ASchema: TPropertyDef): String;

Var
  B : Boolean;

begin
  if ASchema.PascalName<>ASchema.Name then
    Result:=ASchema.PascalName
  else
    begin
    Result:=ASchema.ref;
    if Result='' then
      begin
      Result:=ASchema._type;
      if BaseType(Result) then
        begin
        Result:=getBaseTypeName(ASchema);
        ASchema.PascalName:=Result;
        end;
      end
    else if Not Assigned(ASchema.Refschema) then
      Raise Exception.CreateFmt('%s : Unresolved property (%s) type reference : %s',[AClassName,ASchema.Name,ASchema.Ref])
    else
      Result:=ASchema.Refschema.PascalName;
    end;
  if Result='array' then
    begin
    if ASchema.Items.Ref<>'' then
      begin
      if BaseType(ASchema.Items.Ref) then
        Result:='T'+GetBaseTypeName(ASchema.Items.Ref,ASchema.items.Format)+'Array'
      else
        Result:='T'+ClassPrefix+ASchema.Items.Ref+'Array';
      // Writeln(Result, '(expected : ',ASchema.Items.Refschema.PascalName,') ',Result=ASchema.Items.Refschema.PascalName);
      end
    else if (ClassPrefix+ASchema.items._type='object') and (ASchema.Name<>'') then
      Result:=AClassName+ASchema.Name+'Array'
    else
      begin
      if BaseType(ASchema.Items._type) then
        Result:='T'+GetBaseTypeName(ASchema.items._type,ASchema.items.Format)+'Array'
      else
        Result:='T'+ClassPrefix+ASchema.items._type+'Array';
      end
    end
  else if Result='object' then
    if (ASchema.ref<>'') then
      Result:='T'+ClassPrefix+ASchema.ref
    else
      Result:=AClassName+ASchema.Name
end;

procedure TDiscoveryJSONToPas.CreateClassDeclaration(AClassName: String;
  ASchema: TSchema);

Var
  S : TPropertyDef;
  N : String;
  NeedGetWritename : Boolean;
  TN : String;
  Idx,PropertyIndex,PropertyOptions : Integer;
  L : TStringList;

begin
  ClassHeader(AClassName);
  AddLn('%s = Class(%s)',[AClassName,BaseClassName]);
  AddLn('Private');
  NeedGetWriteName:=False;
  IncIndent;
  L:=TStringList.Create;
  try
    For S in ASchema.ClassProperties do
      begin
      N:=TBaseObject.CleanPropertyName(S.Name);
      Repeat
        Idx:=L.IndexOf(N);
        if (idx=-1) then
          Idx:=L.IndexOf('F'+N);
        if (idx<>-1) then
          begin
          // Writeln('Need rename: ',N);
          N:='_'+N;
          end;
      Until Idx=-1;
      NeedGetWritename:=NeedGetWritename or (CompareText(N,S.Name)<>0);
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

procedure TDiscoveryJSONToPas.CreateClassImplementation(AClassName: String;
  ASchema: TSchema);

Var
  S : TPropertyDef;
  N : String;
  NeedGetWritename : Boolean;
  TN : String;

begin
  NeedGetWriteName:=False;
  ClassHeader(AClassName);
  For S in ASchema.ClassProperties do
    begin
    N:=S.PropertyName;
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
  Addln('');
  if Assigned(ASchema.additionalProperties) then
    begin
    AddLn('Class Function %s.AllowAdditionalProperties : Boolean;',[AClassName]);
    SimpleMethodBody(['Result:=True;']);
    end;
  Addln('');
end;

function TDiscoveryJSONToPas.GetAPIClassName: String;
begin
  Result:=Format('T%s%sAPI',[ClassPrefix,PrettyPrint(Description.Name)])
end;

procedure TDiscoveryJSONToPas.CreateInterface(ClassList: TStrings);

Var
  I : Integer;
  S : String;
  R : TSchema;

begin
  Addln('type');
  IncIndent;
  Comment('');
  For S in ClassList do
    begin
    AddLn('%s = class;',[S]);
    AddLn('%sArray = Array of %s;',[S,S]);
    end;
  For I:=0 to ClassList.Count-1 do
    CreateClassDeclaration(ClassList[i],TSchema(ClassList.Objects[I]));
  For R in Description.resources do
    begin
    R.PascalName:=GetResourceClassName(R,ClassList);
    CreateResourceClassDeclaration(R);
    end;
  CreateAPIClassDeclaration;
  DecIndent;
end;

procedure TDiscoveryJSONToPas.CreateImplementation(ClassList: TStrings);

Var
  I : Integer;
  R : TSchema;
begin
  For I:=0 to ClassList.Count-1 do
    CreateClassImplementation(ClassList[i],TSchema(ClassList.Objects[I]));
  For R in Description.resources do
    CreateResourceClassImplementation(R);
  CreateAPIClassImplementation;

end;

function TDiscoveryJSONToPas.GetResourceMethodSignature(M: TRestmethod; out
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

function TDiscoveryJSONToPas.DescribeMethodParams(M: TRestMethod
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


function TDiscoveryJSONToPas.HavePathParams(M: TRestMethod): Boolean;

begin
  Result:=plPath in DescribeMethodParams(M);
end;


procedure TDiscoveryJSONToPas.CreateResourceMethodQueryParams(Res: TSchema; M: TRestMethod);

Var
  P : TRestMethodParam;
  RN,RCN,MN : String;

begin
  RN:=PrettyPrint(Res.Name);
  RCN:=Res.PascalName;
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
        AddLn('%s : %s;',[P.sourcename,GetBaseTypeName(P._type,P.format)]);
    end;
  DecIndent;
  Addln('end;');
  Addln('');
end;

function TDiscoveryJSONToPas.HaveQueryParams(M: TRestMethod): Boolean;

begin
  Result:=plQuery in DescribeMethodParams(M);
end;


function TDiscoveryJSONToPas.GetResourceClassName(Res: TSchema; AClasses : TStrings): String;
Var
  Suffix : String;
begin
  if (Res.PascalName<>Res.Name) then
    Result:=Res.PascalName
  else
    begin
    Suffix:='Resource';
    Repeat
      Result:=Format('T%s%s%s',[ClassPrefix,PrettyPrint(Res.Name),Suffix]);
      Suffix:='_'+Suffix;
    Until AClasses.IndexOf(Result)=-1;
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
  // Writeln('Examining ',M.name,' ',Length(M.parameters),' params');
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
        // Writeln('Discovered double : ',N);
        N:='_'+N;
        end;
      T.Add(N);
      P.SourceName:=TbaseObject.CleanPropertyName(N);
      end;
  finally
    T.Free;
  end;
end;

Function TDiscoveryJSONToPas.ReservedMethod(ANAme : String) : boolean;

begin
  AName:=';'+lowerCase(AName)+';';
  Result:=Pos(AName,';create;destroy;free;')<>0;
end;

procedure TDiscoveryJSONToPas.CreateResourceClassDeclaration(Res: TSchema);

Var
  M : TRestMethod;
  CN,S : String;
  HaveOpt : Boolean;
  IsFunc : Boolean;


begin
  CN:=Res.PascalName;
  ClassHeader(CN);
  For M in Res.methods do
    begin
    AssignParamNames(Res,M);
    if HaveQueryParams(M) then
      begin
      CreateResourceMethodQueryParams(Res,M);
      end;
    end;
  Addln('%s = Class(TGoogleResource)',[CN]);
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
      S:=GetResourceMethodSignature(M,IsFunc,Format('T%s%sOptions',[PrettyPrint(Res.Name),M.Name]));
      if IsFunc then
        S:='Function '+S
      else
        S:='Procedure '+S;
      if ReservedMethod(M.Name) then
        S:=S+';overload';
      AddLn(S+';');
      end;
    end;
  DecIndent;
  Addln('end;',[Res.name]);
  AddLn('');
end;

procedure TDiscoveryJSONToPas.CreateResourceClassMethodsImplementation(
  Res: TSchema; const AClassName: String);

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

procedure TDiscoveryJSONToPas.CreateResourceMethodImplementationOptions(
  AClassName: String; Res: TSchema; M: TRestMethod);

Var
  P : TRestMethodParam;
  S: String;
  IsFunc : Boolean;
begin
  S:=GetResourceMethodSignature(M,IsFunc,Format('T%s%sOptions',[PrettyPrint(Res.Name),M.Name]));
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

procedure TDiscoveryJSONToPas.CreateResourceMethodImplementation(
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

procedure TDiscoveryJSONToPas.CreateResourceClassImplementation(Res: TSchema);

Var
  CN : String;
  M : TRestMethod;
  PL: TParamLocations;

begin
  CN:=Res.PascalName;
  ClassHeader(CN);
  CreateResourceClassMethodsImplementation(Res,CN);
  For M in Res.methods do
    begin
    PL:=DescribeMethodParams(M);
    CreateResourceMethodImplementation(CN,Res,M);
    if plQuery in PL then
      CreateResourceMethodImplementationOptions(CN,Res,M);
    end;
  AddLn('');
end;


procedure TDiscoveryJSONToPas.CreateAPIClassDeclaration;

Var
  CN : String;
  R : TSchema;

begin
  CN:=GetAPIClassName;
  Classheader(CN);
  AddLn('%s = Class(TGoogleAPI)',[CN]);
  AddLn('Private');
  IncIndent;
  For R in Description.resources do
    AddLn('F%sInstance : %s;',[PrettyPrint(R.Name),R.PascalName]);
  For R in Description.resources do
    AddLn('Function Get%sInstance : %s;virtual;',[PrettyPrint(R.Name),R.PascalName]);
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
  For R in Description.resources do
    begin
    AddLn('Function Create%sResource(AOwner : TComponent) : %s;virtual;overload;',[PrettyPrint(R.Name),R.PascalName]);
    AddLn('Function Create%sResource : %s;virtual;overload;',[PrettyPrint(R.Name),R.PascalName]);
    end;
  Comment('Add default on-demand instances for resources');
  For R in Description.resources do
    AddLn('Property %sResource : %s Read Get%sInstance;',[PrettyPrint(R.Name),R.PascalName,PrettyPrint(R.Name)]);
  DecIndent;
  AddLn('end;');
end;


procedure TDiscoveryJSONToPas.CreateAPIClassImplementation;

  Procedure StringRes(AValue : String);

  begin
    SimpleMethodBody([Format('Result:=%s;',[MakePascalString(AValue,True)])]);
  end;

Var
  CN,SCN : String;
  S : TSchema;
  I : Integer;
  L : TStrings;
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
  For SCN in FClasses do
    AddLn('%s.RegisterObject;',[SCN]);
  DecIndent;
  Addln('end;');
  Addln('');
  CreateAPIResourceFunctionImplementations;
end;

procedure TDiscoveryJSONToPas.CreateAPIResourceFunctionImplementations;

Var
  RN,CN,RCN : String;
  R : TSchema;

begin
  CN:=GetAPIClassName;
  For R in Description.resources do
    begin
    RN:=PrettyPrint(R.Name);
    RCN:=R.PascalName;
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
                             'Result.API:=Self;']);
    AddLn('');
    end;
end;

procedure TDiscoveryJSONToPas.Execute;

Var
  L : TStringList;
begin
  Source.Clear;
  Addln('unit '+outputunitname+';');
  CreateHeader;
  FClasses:=TStringList.Create;
  try
    CollectClasses;
    CreateInterface(FClasses);
    AddLn('');
    AddLn('implementation');
    AddLn('');
    CreateImplementation(FClasses);
    Addln('');
    AddLn('initialization');
    Addln('  %s.RegisterAPI;',[GetAPIClassName]);
  finally
    FClasses.Free;
  end;
  AddLn('end.');
end;

class procedure TDiscoveryJSONToPas.RegisterAllObjects;
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

