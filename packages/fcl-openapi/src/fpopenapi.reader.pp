{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt (michael@freepascal.org)

    Classes for reading a OpenAPI document.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpopenapi.reader;

{$mode ObjFPC}{$H+}

interface

uses 
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Contnrs, FpJson.Data, FpJson.Scanner, 
  {$ELSE}
  sysutils, classes, contnrs, fpjson, jsonscanner, 
  {$ENDIF}
  fpopenapi.types, fpopenapi.objects, fpjson.schema.schema;

Type
  EOpenAPIReader = Class(EOpenAPi);
  TJSONTokens = set of TJSONToken;

  { TOpenAPIReader }
  TCreateAndReadObjectFunc = Function(aParent : TBaseOpenAPIObject;aName : TJSONStringType) : TBaseOpenAPIObject of object;
  TReadObjectProc = Procedure(aObject : TBaseOpenAPIObject; aCheckBracket: Boolean=True) of object;
  TOpenAPIReader = Class(TComponent)
  Private
    FScanner : TJSONScanner;
    procedure InvalidToken(aToken: TJSONToken);
    function StringToJSONNumber(S: RawByteString): TJSONNumber;
    procedure ReadJSONArray(aArray: TJSONArray);
    function ReadParameterOrReferenceObject(aParent: TBaseOpenAPIObject; aName : TJSONStringType): TBaseOpenAPIObject;
    function ReadSecurityRequirementObject(aParent: TBaseOpenAPIObject; aName : TJSONStringType): TBaseOpenAPIObject;
    function ReadServerObject(aParent: TBaseOpenAPIObject; aName : TJSONStringType): TBaseOpenAPIObject;
    function ReadTagObject(aParent: TBaseOpenAPIObject; aName : TJSONStringType): TBaseOpenAPIObject;
    // Untyped callbacks
    procedure ReadServerVariableUntyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadPathItemOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadParameterOrReferenceUntyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadRequestBodyOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadMediaTypeUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadEncodingUntyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadResponseOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadExampleOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadLinkOrReferenceUntyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadHeaderOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadSecuritySchemeOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
    procedure ReadCallbackOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
  Protected
    function CurPosAsString: String;
    function TokensToString(aTokens: TJSONTokens): String;
    Function GetToken : TJSONToken;
    Procedure CheckToken(aExpected,aActual : TJSONToken);
    Procedure CheckToken(aExpected : TJSONTokens; aActual : TJSONToken);
    procedure CheckNextToken(aExpected : TJSONToken);
    function CheckNextToken(aExpected : TJSONTokens) : TJSONToken;
    Function GetTokenString : TJSONStringType;
    Function ReadString : TJSONStringType;
    Function ReadBoolean : Boolean;
    Function ReadJSONData : TJSONData;
    function ReadJSONData(aToken: TJSONToken): TJSONData;
    procedure ReadJSONObject(aObject: TJSONObject; aCheckOpening: Boolean=True);
    procedure HandleParamOrHeader(aObj: TParameterOrHeader; aName : String);
    procedure DoReadObject(aObj: TParameterOrHeader; aCheckBracket: Boolean);
    Function ReadObjectArray(aList : TFPObjectList; aGetObject : TCreateAndReadObjectFunc) : integer;overload;
    function ReadMapObject(aList: TNamedOpenAPIObjectList; aObjectReader : TReadObjectProc; aCheckBracket: Boolean=True): integer;
    Function ReadObject(aList : TStrings) : integer;
    // objects
    Procedure ReadOpenAPI(aObj : TOpenAPI; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadInfo(aObj : TInfo; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadContact(aObj : TContact; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadLicense(aObj : TLicense; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadServer(aObj : TServer; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadServerList(aObj : TServerList); virtual; overload;
    Procedure ReadPathsList(aObj : TPathsList; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadServerVariable(aObj : TServerVariable; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadServerVariableMap(aObj : TServerVariableMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadJSONSchemaMap(aObj : TJSONSchemaMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadJSONSchema(aObj : TJSONSchema; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadComponents(aObj : TComponents; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadPathItem(aObj : TPathItem; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadPathItemOrReference(aObj : TPathItemOrReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadPathItemOrReferenceMap(aObj : TPathItemOrReferenceMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadApiOperation(aObj : TApiOperation; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadExternalDocumentation(aObj : TExternalDocumentation; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadParameter(aObj : TParameter; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadParameterOrReference(aObj : TParameterOrReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadParameterOrReferenceList(aObj : TParameterOrReferenceList); virtual; overload;
    Procedure ReadParameterOrReferenceMap(aObj : TParameterOrReferenceMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadParameterStyle(aObj : TParameterStyle; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadRequestBody(aObj : TRequestBody; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadRequestBodyOrReference(aObj : TRequestBodyOrReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadRequestBodyOrReferenceMap(aObj : TRequestBodyOrReferenceMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadMediaType(aObj : TMediaType; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadMediaTypeMap(aObj : TMediaTypeMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadEncoding(aObj : TEncoding; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadEncodingMap(aObj : TEncodingMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadResponses(aObj : TResponses; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadResponse(aObj : TResponse; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadResponseOrReference(aObj : TResponseOrReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadResponseOrReferenceMap(aObj : TResponseOrReferenceMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadExample(aObj : TExample; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadExampleOrReference(aObj : TExampleOrReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadExampleOrReferenceMap(aObj : TExampleOrReferenceMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadLink(aObj : TLink; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadLinkOrReference(aObj : TLinkOrReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadLinkOrReferenceMap(aObj : TLinkOrReferenceMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadTag(aObj : TTag; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadReference(aObj : TReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadSchema(aObj : TSchema; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadDiscriminator(aObj : TDiscriminator; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadXML(aObj : TXML; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadSecurityScheme(aObj : TSecurityScheme; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadSecuritySchemeOrReference(aObj : TSecuritySchemeOrReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadSecuritySchemeOrReferenceMap(aObj : TSecuritySchemeOrReferenceMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadOAuthFlows(aObj : TOAuthFlows; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadOAuthFlow(aObj : TOauthFlow; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadHeader(aObj : THeader; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadHeaderOrReference(aObj : THeaderOrReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadHeaderOrReferenceMap(aObj : THeaderOrReferenceMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadCallbackOrReference(aObj : TCallbackOrReference; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadCallbackOrReferenceMap(aObj : TCallbackOrReferenceMap; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadSecurityRequirement(aObj : TSecurityRequirement; aCheckBracket : Boolean = True); virtual; overload;
    Procedure ReadSecurityRequirementList(aObj : TSecurityRequirementList); virtual; overload;
    Procedure ReadTagList(aObj : TTagList); virtual; overload;
 Public
   procedure ReadFromScanner(aOpenAPI: TOpenAPI; AScanner: TJSONScanner);
   procedure ReadFromFile(aOpenAPI: TOpenAPI; const AFilename: String);
   procedure ReadFromStream(aOpenAPI: TOpenAPI; AStream: TStream);
   procedure ReadFromString(aOpenAPI: TOpenAPI; const AString: TJSONStringType);
 end;

implementation

uses fpjson.schema.reader, fpjson.schema.consts;


function TOpenAPIReader.GetToken: TJSONToken;

const
  IgnoredTokens = [tkWhitespace,tkComment];

begin
  repeat
    Result:=FScanner.FetchToken;
  until Not (Result in IgnoredTokens);
end;

function TOpenAPIReader.CurPosAsString: String;

begin
  Result:=Format('%d:%d',[FScanner.CurRow,FScanner.CurColumn]);
end;

procedure TOpenAPIReader.InvalidToken(aToken: TJSONToken);

begin
  raise EOpenAPIReader.CreateFmt(SErrInvalidToken, [CurPosAsString, TokenInfos[aToken]]);
end;

procedure TOpenAPIReader.CheckToken(aExpected, aActual: TJSONToken);
begin
  if aExpected<>aActual then
    raise EOpenAPIReader.CreateFmt(SErrUnexpectedToken, [CurPosAsString, TokenInfos[aExpected], TokenInfos[aActual]]);
end;

procedure TOpenAPIReader.CheckToken(aExpected: TJSONTokens; aActual: TJSONToken);
begin
  if not (aActual in aExpected) then
    raise EOpenAPIReader.CreateFmt(SErrUnexpectedTokenNotInSet, [TokensToString(aExpected), TokenInfos[aActual]]);
end;

function TOpenAPIReader.TokensToString(aTokens : TJSONTokens) : String;

var
  T : TJSONToken;
  S : String;

begin
  S:='';
  For T in aTokens do
    begin
    if (S<>'') then
      S:=S+', ';
    S:=S+'"'+TokenInfos[T]+'"';
    end;
  Result:=S;
end;


procedure TOpenAPIReader.CheckNextToken(aExpected: TJSONToken);
begin
  CheckToken(aExpected,GetToken);
end;

function TOpenAPIReader.CheckNextToken(aExpected: TJSONTokens): TJSONToken;
begin
  Result:=GetToken;
  CheckToken(aExpected,Result);
end;

function TOpenAPIReader.GetTokenString: TJSONStringType;
begin
  Result:=FScanner.CurTokenString;
end;

function TOpenAPIReader.ReadString: TJSONStringType;
var
  aToken: TJSONToken;
begin
  aToken:=GetToken;
  CheckToken(tkString,aToken);
  Result:=GetTokenString;
end;

function TOpenAPIReader.ReadBoolean: Boolean;
var
  aToken : TJSONToken;

begin
  aToken:=GetToken;
  CheckToken([tkTrue,tkFalse],aToken);
  Result:=aToken=tkTrue;
end;

function TOpenAPIReader.ReadJSONData: TJSONData;

begin
  Result:=ReadJSONData(GetToken);
end;


function TOpenAPIReader.StringToJSONNumber(S : RawByteString): TJSONNumber;

var
  I : integer;
  I64 : Int64;
  Q : QWord;
  F : TJSONFloat;

begin
  if TryStrToInt(S,I) then
    Exit(TJSONIntegerNumber.Create(I));
  if TryStrToInt64(S,I64) then
    Exit(TJSONInt64Number.Create(I64));
  if TryStrToQWord(S,Q) then
    Exit(TJSONQWordNumber.Create(Q));
  Val(S,F,I);
  If (I<>0) then
    EConvertError.CreateFmt(SErrInvalidNumber,[S]);
  Result:=TJSONFloatNumber.Create(F);
end;


function TOpenAPIReader.ReadJSONData(aToken : TJSONToken): TJSONData;

begin
  Result:=nil;
  try
    case aToken of
      tkNull : Result:=TJSONNull.Create;
      tkNumber : Result:=StringToJSONNumber(GetTokenString);
      tkTrue,
      tkFalse : Result:=TJSONBoolean.Create(aToken=tkTrue);
      tkString : Result:=TJSONString.Create(GetTokenString);
      tkCurlyBraceOpen :
        begin
        Result:=TJSONObject.Create;
        ReadJSONObject(TJSONObject(Result),False);
        end;
      tkSquaredBraceOpen :
        begin
        Result:=TJSONArray.Create;
        ReadJSONArray(TJSONArray(Result));
        end
    else
      InvalidToken(aToken);
    end;
  except
    Result.Free;
    Raise;
  end;
end;

procedure TOpenAPIReader.ReadJSONArray(aArray: TJSONArray);
// On entry, we're on [

var
  aToken : TJSONToken;

begin
  Repeat
    aToken:=GetToken;
    if Not (aToken in [tkComma,tkSquaredBraceClose]) then
      aArray.Add(ReadJSONData(aToken));
  until (aToken in [tkSquaredBraceClose,tkEOF]);
  CheckToken(tkSquaredBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadJSONObject(aObject : TJSONObject; aCheckOpening: Boolean = True);

var
  aToken : TJSONToken;
  aName : TJSONStringtype;

begin
  if aCheckOpening then
    CheckNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aObject.Add(aNAme,ReadJSONData);
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
      aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.HandleParamOrHeader(aObj: TParameterOrHeader; aName: String);

var
  aKeyWord : TParameterKeyword;

begin
  aKeyword:=TParameterKeyword.FromString(aName);
  case aKeyword of
  pakName:
    if aObj is TParameter then
      TParameter(aObj).Name:=Readstring
    else
      aObj.Extensions.Add(aName,ReadJSONData);
  pakIn:
    if aObj is TParameter then
      TParameter(aObj).In_:=Readstring
    else
      aObj.Extensions.Add(aName,ReadJSONData);
  pakDescription:
    aObj.Description:=Readstring;
  pakRequired:
    aObj.Required:=ReadBoolean;
  pakDeprecated:
    aObj.Deprecated:=ReadBoolean;
  pakAllowEmptyValue:
    aObj.AllowEmptyValue:=ReadBoolean;
  pakStyle:
    aObj.Style:=Readstring;
  pakExplode:
    aObj.Explode:=ReadBoolean;
  pakAllowReserved:
    aObj.AllowReserved:=ReadBoolean;
  pakSchema:
    ReadJSONschema(aObj.Schema);
  pakExample:
    aObj.Example:=ReadJSONData;
  pakExamples:
    ReadExampleOrReferenceMap(aObj.Examples);
  pakContent:
    ReadMediaTypeMap(aObj.Content);
  else
    aObj.Extensions.Add(aName,ReadJSONData);
  end;
end;


function TOpenAPIReader.ReadMapObject(aList: TNamedOpenAPIObjectList; aObjectReader: TReadObjectProc; aCheckBracket: Boolean
  ): integer;
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  Itm : TObject;

begin
  Result:=0;
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    itm:=aList.CreateNew(aName);
//    aList.Add(aName,Itm);
    if not (Itm is TBaseOpenAPIObject) then
       Raise EOpenAPIReader.CreateFmt('List %s created an item that is not a TOPenAPIObject',[aList.ClassName]);
    aObjectReader(TBaseOpenAPIObject(Itm),True);
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
      aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

function TOpenAPIReader.ReadObject(aList: TStrings): integer;
var
  aToken: TJSONToken;
begin
  Result:=0;
  CheckNextToken(tkSquaredBraceOpen);
  aToken:=GetToken;
  while Not (aToken in [tkEOF,tkSquaredBraceClose]) do
    begin
    case aToken of
      tkComma:
        ;
      tkString:
        begin
        aList.Add(GetTokenString);
        inc(Result);
        end
    else
      InvalidToken(aToken);
    end;
    aToken:=GetToken;
    end;
end;

procedure TOpenAPIReader.ReadOpenAPI(aObj: TOpenAPI; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TOpenAPIKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TOpenAPIKeyword.FromString(aName);
    case aKeyword of
    oakOpenApi: 
      aObj.OpenApi:=Readstring;
    oakInfo: 
      ReadInfo(aObj.Info);
    oakJSONSchemaDialect: 
      aObj.JSONSchemaDialect:=Readstring;
    oakServers: 
      ReadServerList(aObj.Servers);
    oakPaths: 
      ReadPathsList(aObj.Paths);
    oakWebHooks: 
      ReadPathItemOrReferenceMap(aObj.WebHooks);
    oakComponents: 
      ReadComponents(aObj.Components);
    oakSecurity: 
      ReadSecurityRequirementList(aObj.Security);
    oakTags: 
      ReadTagList(aObj.Tags);
    oakExternalDocs: 
      ReadExternalDocumentation(aObj.ExternalDocs);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadInfo(aObj: TInfo; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TInfoKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TInfoKeyword.FromString(aName);
    case aKeyword of
    ikTitle: 
      aObj.Title:=Readstring;
    ikSummary: 
      aObj.Summary:=Readstring;
    ikDescription: 
      aObj.Description:=Readstring;
    ikTermsOfService: 
      aObj.TermsOfService:=Readstring;
    ikContact: 
      ReadContact(aObj.Contact);
    ikLicense: 
      ReadLicense(aObj.License);
    ikVersion: 
      aObj.Version:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadContact(aObj: TContact; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TContactKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TContactKeyword.FromString(aName);
    case aKeyword of
    cokName: 
      aObj.Name:=Readstring;
    cokUrl: 
      aObj.Url:=Readstring;
    cokEmail: 
      aObj.Email:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadLicense(aObj: TLicense; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TLicenseKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TLicenseKeyword.FromString(aName);
    case aKeyword of
    lkName: 
      aObj.Name:=Readstring;
    lkIdentifier: 
      aObj.Identifier:=Readstring;
    lkUrl: 
      aObj.Url:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadServer(aObj: TServer; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TServerKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TServerKeyword.FromString(aName);
    case aKeyword of
    skServerUrl: 
      aObj.Url:=Readstring;
    skDescription: 
      aObj.Description:=Readstring;
    skVariables: 
      ReadServerVariableMap(aObj.Variables);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


function TOpenAPIReader.ReadServerObject(aParent: TBaseOpenAPIObject; aName: TJSONStringType): TBaseOpenAPIObject;

begin
  Result:=TServer.Create(aParent,'server');
  try
    ReadServer(TServer(Result),False);
  except
    Result.Free;
    Raise;
  end;
end;
procedure TOpenAPIReader.ReadServerList(aObj: TServerList);

begin
  ReadObjectArray(aObj,@ReadServerObject);
end;


procedure TOpenAPIReader.ReadPathsList(aObj: TPathsList; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  Itm: TPathItem;

begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    Itm:=aObj.AddItem(aName);
    ReadPathItem(Itm);
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadServerVariable(aObj: TServerVariable; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TServerVariableKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TServerVariableKeyword.FromString(aName);
    case aKeyword of
    svkEnum: 
      ReadObject(aObj.Enum);
    svkDefault: 
      aObj.Default:=Readstring;
    svkDescription: 
      aObj.Description:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadServerVariableUntyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
begin
  ReadServerVariable(aObj as TServerVariable,aCheckBracket);
end;

procedure TOpenAPIReader.ReadServerVariableMap(aObj: TServerVariableMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadServerVariableUntyped,aCheckBracket);
end;

procedure TOpenAPIReader.ReadJSONSchemaMap(aObj: TJSONSchemaMap; aCheckBracket: Boolean);
var
  atoken : TJSONToken;
  aName : String;
  Schema : TJSONSchema;
  R : TJSONSchemaReader;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    Schema:=aObj.Add(aName);
    CheckNextToken(tkColon);
    R:=TJSONSchemaReader.Create(Nil);
    try
      R.ReadFromScanner(Schema,FScanner);
    finally
      R.Free;
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadJSONSchema(aObj: TJSONSchema; aCheckBracket: Boolean);

var
  R : TJSONSchemaReader;
begin
  R:=TJSONSchemaReader.Create(Nil);
  try
    R.ReadFromScanner(aObj,FScanner);
  finally
    R.Free;
  end;
end;


procedure TOpenAPIReader.ReadComponents(aObj: TComponents; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TComponentsKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TComponentsKeyword.FromString(aName);
    case aKeyword of
    ckSchemas: 
      ReadJSONSchemaMap(aObj.Schemas);
    ckResponses: 
      ReadResponseOrReferenceMap(aObj.Responses);
    ckParameters: 
      ReadParameterOrReferenceMap(aObj.Parameters);
    ckExamples: 
      ReadExampleOrReferenceMap(aObj.Examples);
    ckRequestBodies: 
      ReadRequestBodyOrReferenceMap(aObj.RequestBodies);
    ckHeaders: 
      ReadHeaderOrReferenceMap(aObj.Headers);
    ckSecuritySchemes: 
      ReadSecuritySchemeOrReferenceMap(aObj.SecuritySchemes);
    ckLinks: 
      ReadLinkOrReferenceMap(aObj.Links);
    ckCallbacks: 
      ReadLinkOrReferenceMap(aObj.Callbacks);
    ckPathItems: 
      ReadPathItemOrReferenceMap(aObj.PathItems);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadPathItem(aObj: TPathItem; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TPathItemKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TPathItemKeyword.FromString(aName);
    case aKeyword of
    pkRef: 
      aObj.Ref:=Readstring;
    pkSummary: 
      aObj.Summary:=Readstring;
    pkDescription: 
      aObj.Description:=Readstring;
    pkGet: 
      ReadApiOperation(aObj.Get);
    pkPut: 
      ReadApiOperation(aObj.Put);
    pkPost: 
      ReadApiOperation(aObj.Post);
    pkDelete: 
      ReadApiOperation(aObj.Delete);
    pkOptions: 
      ReadApiOperation(aObj.Options);
    pkHead: 
      ReadApiOperation(aObj.Head);
    pkPatch: 
      ReadApiOperation(aObj.Patch);
    pkTrace: 
      ReadApiOperation(aObj.Trace);
    pkServers: 
      ReadServerList(aObj.Servers);
    pkParameters: 
      ReadParameterOrReferenceList(aObj.Parameters);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadPathItemOrReference(aObj: TPathItemOrReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TPathItemKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TPathItemKeyword.FromString(aName);
    case aKeyword of
    pkRef: 
      aObj.Ref:=Readstring;
    pkSummary: 
      aObj.Summary:=Readstring;
    pkDescription: 
      aObj.Description:=Readstring;
    pkGet: 
      ReadApiOperation(aObj.Get);
    pkPut: 
      ReadApiOperation(aObj.Put);
    pkPost: 
      ReadApiOperation(aObj.Post);
    pkDelete: 
      ReadApiOperation(aObj.Delete);
    pkOptions: 
      ReadApiOperation(aObj.Options);
    pkHead: 
      ReadApiOperation(aObj.Head);
    pkPatch: 
      ReadApiOperation(aObj.Patch);
    pkTrace: 
      ReadApiOperation(aObj.Trace);
    pkServers: 
      ReadServerList(aObj.Servers);
    pkParameters: 
      ReadParameterOrReferenceList(aObj.Parameters);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadPathItemOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);

begin
  ReadPathItemOrReference(aObj as TPathItemOrReference,aCheckBracket);
end;

procedure TOpenAPIReader.ReadPathItemOrReferenceMap(aObj: TPathItemOrReferenceMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadPathItemOrReferenceUnTyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadApiOperation(aObj: TApiOperation; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TApiOperationKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TApiOperationKeyword.FromString(aName);
    case aKeyword of
    okTags: 
      ReadObject(aObj.Tags);
    okSummary: 
      aObj.Summary:=Readstring;
    okDescription: 
      aObj.Description:=Readstring;
    okExternalDocs: 
      ReadExternalDocumentation(aObj.ExternalDocs);
    okOperationId: 
      aObj.OperationId:=Readstring;
    okParameters: 
      ReadParameterOrReferenceList(aObj.Parameters);
    okRequestBody: 
      ReadRequestBody(aObj.RequestBody);
    okResponses: 
      ReadResponses(aObj.Responses);
    okCallbacks: 
      ReadCallBackOrReferenceMap(aObj.Callbacks);
    okDeprecated: 
      aObj.Deprecated:=ReadBoolean;
    okSecurity: 
      ReadSecurityRequirementList(aObj.Security);
    okServers: 
      ReadServerList(aObj.Servers);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadExternalDocumentation(aObj: TExternalDocumentation; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TExternalDocumentationKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TExternalDocumentationKeyword.FromString(aName);
    case aKeyword of
    edkDescription: 
      aObj.Description:=Readstring;
    edkUrl: 
      aObj.Url:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadParameter(aObj: TParameter; aCheckBracket: Boolean);

begin
  DoReadObject(aObj,aCheckBracket);
end;

procedure TOpenAPIReader.DoReadObject(aObj: TParameterOrHeader; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;

begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    HandleParamOrHeader(aObj,aName);
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


function TOpenAPIReader.ReadObjectArray(aList: TFPObjectList; aGetObject : TCreateAndReadObjectFunc): integer;
var
  Item: TBaseOpenAPIObject;
  aToken: TJSONToken;

begin
  Result:=0;
  CheckNextToken(tkSquaredBraceOpen);
  aToken:=GetToken;
  while not (aToken in [tkEOF,tkSquaredBraceClose]) do
    begin
    case aToken of
      tkComma:
        ;
      tkCurlyBraceOpen:
        begin
        Item:=aGetObject(Nil,IntToStr(Result));
        aList.Add(Item);
        Inc(Result);
        end;
    else
      InvalidToken(aToken);
    end;
    aToken:=GetToken;
    end;
  CheckToken(tkSquaredBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadParameterOrReference(aObj: TParameterOrReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TParameterKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TParameterKeyword.FromString(aName);
    case aKeyword of
    pakName: 
      aObj.Name:=Readstring;
    pakIn: 
      aObj.In_:=Readstring;
    pakDescription: 
      aObj.Description:=Readstring;
    pakRequired: 
      aObj.Required:=ReadBoolean;
    pakDeprecated: 
      aObj.Deprecated:=ReadBoolean;
    pakAllowEmptyValue: 
      aObj.AllowEmptyValue:=ReadBoolean;
    pakStyle: 
      aObj.Style:=Readstring;
    pakExplode: 
      aObj.Explode:=ReadBoolean;
    pakAllowReserved: 
      aObj.AllowReserved:=ReadBoolean;
    pakSchema: 
      ReadJSONSchema(aObj.Schema);
    pakExample: 
      aObj.Example:=ReadJSONData;
    pakExamples: 
      ReadExampleOrReferenceMap(aObj.Examples);
    pakContent: 
      ReadMediaTypeMap(aObj.Content);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

function TOpenAPIReader.ReadParameterOrReferenceObject(aParent: TBaseOpenAPIObject; aName: TJSONStringType): TBaseOpenAPIObject;

begin
  Result:=TParameterOrReference.Create(aParent,aName);
  try
    ReadParameterOrReference(TParameterOrReference(Result),False);
  except
    Result.Free;
    Raise;
  end;
end;

procedure TOpenAPIReader.ReadParameterOrReferenceList(aObj: TParameterOrReferenceList);

begin
  ReadObjectArray(aObj,@ReadParameterOrReferenceObject);
end;

procedure TOpenAPIReader.ReadParameterOrReferenceUntyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);

begin
  ReadParameterOrReference(aObj as TParameterOrReference,aCheckBracket);
end;

procedure TOpenAPIReader.ReadParameterOrReferenceMap(aObj: TParameterOrReferenceMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadParameterOrReferenceUnTyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadParameterStyle(aObj: TParameterStyle; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TParameterStyleKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TParameterStyleKeyword.FromString(aName);
    case aKeyword of
    pskMatrix: 
      aObj.Matrix:=Readstring;
    pskLabel: 
      aObj.Label_:=Readstring;
    pskForm: 
      aObj.Form:=Readstring;
    pskSimple: 
      aObj.Simple:=Readstring;
    pskSpaceDelimited: 
      aObj.SpaceDelimited:=Readstring;
    pskPipeDelimited: 
      aObj.PipeDelimited:=Readstring;
    pskDeepObject: 
      aObj.DeepObject:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadRequestBody(aObj: TRequestBody; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TRequestBodyKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TRequestBodyKeyword.FromString(aName);
    case aKeyword of
    rbkDescription: 
      aObj.Description:=Readstring;
    rbkContent: 
      ReadMediaTypeMap(aObj.Content);
    rbkRequired: 
      aObj.Required:=ReadBoolean;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadRequestBodyOrReference(aObj: TRequestBodyOrReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TRequestBodyKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TRequestBodyKeyword.FromString(aName);
    case aKeyword of
    rbkDescription: 
      aObj.Description:=Readstring;
    rbkContent: 
      ReadMediaTypeMap(aObj.Content);
    rbkRequired: 
      aObj.Required:=ReadBoolean;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadRequestBodyOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
begin
  ReadRequestBodyOrReference(aObj as TRequestBodyOrReference,aCheckBracket);
end;

procedure TOpenAPIReader.ReadRequestBodyOrReferenceMap(aObj: TRequestBodyOrReferenceMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadRequestBodyOrReferenceUnTyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadMediaType(aObj: TMediaType; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TMediaTypeKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TMediaTypeKeyword.FromString(aName);
    case aKeyword of
    mtkSchema: 
      ReadJSONSchema(aObj.Schema);
    mtkExample: 
      aObj.Example:=ReadJSONData;
    mtkExamples: 
      ReadExampleOrReferenceMap(aObj.Examples);
    mtkEncoding: 
      ReadEncodingMap(aObj.Encoding);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadMediaTypeUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
begin
  ReadMediaType(aObj as TMediaType,aCheckBracket);
end;

procedure TOpenAPIReader.ReadMediaTypeMap(aObj: TMediaTypeMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadMediaTypeUntyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadEncoding(aObj: TEncoding; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TEncodingKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TEncodingKeyword.FromString(aName);
    case aKeyword of
    eckContentType: 
      aObj.ContentType:=Readstring;
    eckHeaders: 
      ReadHeaderOrReferenceMap(aObj.Headers);
    eckStyle: 
      aObj.Style:=Readstring;
    eckExplode: 
      aObj.Explode:=ReadBoolean;
    eckAllowReserved: 
      aObj.AllowReserved:=ReadBoolean;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadEncodingUntyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
begin
  ReadEncoding(aObj as TEncoding,aCheckBracket);
end;

procedure TOpenAPIReader.ReadEncodingMap(aObj: TEncodingMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadEncodingUntyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadResponses(aObj: TResponses; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  Itm : TResponse;

begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    Itm:=aObj.AddItem(aName);
    ReadResponse(Itm);
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadResponse(aObj: TResponse; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TResponseKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TResponseKeyword.FromString(aName);
    case aKeyword of
    rkDescription: 
      aObj.Description:=Readstring;
    rkHeaders: 
      ReadHeaderOrReferenceMap(aObj.Headers);
    rkContent: 
      ReadMediaTypeMap(aObj.Content);
    rkLinks: 
      ReadLinkOrReferenceMap(aObj.Links);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadResponseOrReference(aObj: TResponseOrReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TResponseKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TResponseKeyword.FromString(aName);
    case aKeyword of
    rkDescription: 
      aObj.Description:=Readstring;
    rkHeaders: 
      ReadHeaderOrReferenceMap(aObj.Headers);
    rkContent: 
      ReadMediaTypeMap(aObj.Content);
    rkLinks: 
      ReadLinkOrReferenceMap(aObj.Links);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadResponseOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
begin
  ReadResponseOrReference(aObj as TResponseOrReference, aCheckBracket);
end;

procedure TOpenAPIReader.ReadResponseOrReferenceMap(aObj: TResponseOrReferenceMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadResponseOrReferenceUnTyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadExample(aObj: TExample; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TExampleKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TExampleKeyword.FromString(aName);
    case aKeyword of
    exkSummary: 
      aObj.Summary:=Readstring;
    exkDescription: 
      aObj.Description:=Readstring;
    exkValue: 
      aObj.Value:=ReadJSONData;
    exkExternalValue: 
      aObj.ExternalValue:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadExampleOrReference(aObj: TExampleOrReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TExampleKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TExampleKeyword.FromString(aName);
    case aKeyword of
    exkSummary: 
      aObj.Summary:=Readstring;
    exkDescription: 
      aObj.Description:=Readstring;
    exkValue: 
      aObj.Value:=ReadJSONData;
    exkExternalValue: 
      aObj.ExternalValue:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadExampleOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);

begin
  ReadExampleOrReference(aObj as TExampleOrReference,aCheckBracket);
end;

procedure TOpenAPIReader.ReadExampleOrReferenceMap(aObj: TExampleOrReferenceMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadExampleOrReferenceUnTyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadLink(aObj: TLink; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TLinkKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TLinkKeyword.FromString(aName);
    case aKeyword of
    likOperationRef: 
      aObj.OperationRef:=Readstring;
    likOperationId: 
      aObj.OperationId:=Readstring;
    likParameters: 
      ReadJSONObject(aObj.Parameters);
    likRequestBody: 
      aObj.RequestBody:=ReadJSONData;
    likDescription: 
      aObj.Description:=Readstring;
    likServer: 
      ReadServer(aObj.Server);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadLinkOrReference(aObj: TLinkOrReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TLinkKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TLinkKeyword.FromString(aName);
    case aKeyword of
    likOperationRef: 
      aObj.OperationRef:=Readstring;
    likOperationId: 
      aObj.OperationId:=Readstring;
    likParameters: 
      ReadJSONObject(aObj.Parameters);
    likRequestBody: 
      aObj.RequestBody:=ReadJSONData;
    likDescription: 
      aObj.Description:=Readstring;
    likServer: 
      ReadServer(aObj.Server);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadLinkOrReferenceUntyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);

begin
  ReadLinkOrReference(aObj as TLinkOrReference,aCheckbracket)
end;

procedure TOpenAPIReader.ReadLinkOrReferenceMap(aObj: TLinkOrReferenceMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadLinkOrReferenceUntyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadTag(aObj: TTag; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TTagKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TTagKeyword.FromString(aName);
    case aKeyword of
    tkName: 
      aObj.Name:=Readstring;
    tkDescription: 
      aObj.Description:=Readstring;
    tkExternalDocs: 
      ReadExternalDocumentation(aObj.ExternalDocs);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadReference(aObj: TReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TReferenceKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TReferenceKeyword.FromString(aName);
    case aKeyword of
    rfkRef: 
      aObj.Ref:=Readstring;
    rfkSummary: 
      aObj.Summary:=Readstring;
    rfkDescription: 
      aObj.Description:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadSchema(aObj: TSchema; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TSchemaKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TSchemaKeyword.FromString(aName);
    case aKeyword of
    sckDiscriminator: 
      ReadDiscriminator(aObj.Discriminator);
    sckXML: 
      ReadXML(aObj.XML);
    sckExternalDocs: 
      ReadExternalDocumentation(aObj.ExternalDocs);
    sckExample: 
      aObj.Example:=ReadJSONData;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadDiscriminator(aObj: TDiscriminator; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TDiscriminatorKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TDiscriminatorKeyword.FromString(aName);
    case aKeyword of
    dikPropertyName: 
      aObj.PropertyName:=Readstring;
    dikMapping: 
      ReadObject(aObj.Mapping);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadXML(aObj: TXML; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TXMLKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TXMLKeyword.FromString(aName);
    case aKeyword of
    xmkName: 
      aObj.Name:=Readstring;
    xmkNamespace: 
      aObj.Namespace:=Readstring;
    xmkPrefix: 
      aObj.Prefix:=Readstring;
    xmkAttribute: 
      aObj.Attribute:=ReadBoolean;
    xmkWrapped: 
      aObj.Wrapped:=ReadBoolean;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadSecurityScheme(aObj: TSecurityScheme; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TSecuritySchemeKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TSecuritySchemeKeyword.FromString(aName);
    case aKeyword of
    sskType: 
      aObj.Type_:=Readstring;
    sskDescription: 
      aObj.Description:=Readstring;
    sskName: 
      aObj.Name:=Readstring;
    sskIn: 
      aObj.In_:=Readstring;
    sskScheme: 
      aObj.Scheme:=Readstring;
    sskBearerFormat: 
      aObj.BearerFormat:=Readstring;
    sskFlows: 
      ReadOAuthFlows(aObj.Flows);
    sskOpenIdConnectUrl: 
      aObj.OpenIdConnectUrl:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadSecuritySchemeOrReference(aObj: TSecuritySchemeOrReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TSecuritySchemeKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TSecuritySchemeKeyword.FromString(aName);
    case aKeyword of
    sskType: 
      aObj.Type_:=Readstring;
    sskDescription: 
      aObj.Description:=Readstring;
    sskName: 
      aObj.Name:=Readstring;
    sskIn: 
      aObj.In_:=Readstring;
    sskScheme: 
      aObj.Scheme:=Readstring;
    sskBearerFormat: 
      aObj.BearerFormat:=Readstring;
    sskFlows: 
      ReadOAuthFlows(aObj.Flows);
    sskOpenIdConnectUrl: 
      aObj.OpenIdConnectUrl:=Readstring;
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadSecuritySchemeOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);

begin
  ReadSecuritySchemeOrReference(aObj as TSecuritySchemeOrReference,aCheckBracket);
end;

procedure TOpenAPIReader.ReadSecuritySchemeOrReferenceMap(aObj: TSecuritySchemeOrReferenceMap; aCheckBracket: Boolean);

begin
  ReadMapObject(aObj,@ReadSecuritySchemeOrReferenceUntyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadOAuthFlows(aObj: TOAuthFlows; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TOAuthFlowsKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TOAuthFlowsKeyword.FromString(aName);
    case aKeyword of
    ofskImplicit: 
      ReadOauthFlow(aObj.Implicit);
    ofskPassword: 
      ReadOauthFlow(aObj.Password);
    ofskClientCredentials: 
      ReadOauthFlow(aObj.ClientCredentials);
    ofskClientAuthorizationCode: 
      ReadOauthFlow(aObj.ClientAuthorizationCode);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadOAuthFlow(aObj: TOauthFlow; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  aKeyword : TOauthFlowKeyword;
begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aKeyword:=TOauthFlowKeyword.FromString(aName);
    case aKeyword of
    ofkAuthorizationUrl: 
      aObj.AuthorizationUrl:=Readstring;
    ofkTokenURL: 
      aObj.TokenURL:=Readstring;
    ofkRefreshURL: 
      aObj.RefreshURL:=Readstring;
    ofkScopes: 
      ReadObject(aObj.Scopes);
    else 
      aObj.Extensions.Add(aName,ReadJSONData);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadHeader(aObj: THeader; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  DoReadObject(aObj,aCheckBracket);
end;


procedure TOpenAPIReader.ReadHeaderOrReference(aObj: THeaderOrReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;

begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    if aName='ref' then
      ReadReference(aObj.Reference,True)
    else
      HandleParamOrHeader(aObj,aName);
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TOpenAPIReader.ReadHeaderOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);

begin
  ReadHeaderOrReference(aObj as THeaderOrReference,aCheckBracket)
end;

procedure TOpenAPIReader.ReadHeaderOrReferenceMap(aObj: THeaderOrReferenceMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadHeaderOrReferenceUnTyped,aCheckBracket);
end;



procedure TOpenAPIReader.ReadCallbackOrReference(aObj: TCallbackOrReference; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;
  itm : TPathItem;

begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    case aName of
      'ref' : ReadReference(aObj.Reference);
    else 
      Itm:=aObj.AddItem(Name);
      ReadPathItem(Itm);
    end;
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TOpenAPIReader.ReadCallbackOrReferenceUnTyped(aObj: TBaseOpenAPIObject; aCheckBracket: Boolean);
begin
  ReadCallbackOrReference(aObj as TCallbackOrReference,aCheckBracket);
end;

procedure TOpenAPIReader.ReadCallbackOrReferenceMap(aObj: TCallbackOrReferenceMap; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true

begin
  ReadMapObject(aObj,@ReadCallbackOrReferenceUnTyped,aCheckBracket);
end;


procedure TOpenAPIReader.ReadSecurityRequirement(aObj: TSecurityRequirement; aCheckBracket: Boolean);
// On entry, we are on { token if aCheckBracket is false, before if it is true
var
  atoken : TJSONToken;
  aName : String;

begin
  if aCheckBracket then
    checkNextToken(tkCurlyBraceOpen);
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    ReadObject(aObj.AddItem(aName));
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
       aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

function TOpenAPIReader.ReadSecurityRequirementObject(aParent: TBaseOpenAPIObject; aName: TJSONStringType): TBaseOpenAPIObject;

begin
  Result:=TSecurityRequirement.Create(aParent,aName);
  try
    ReadSecurityRequirement(TSecurityRequirement(Result),False);
  except
    Result.Free;
    Raise;
  end;
end;

procedure TOpenAPIReader.ReadSecurityRequirementList(aObj: TSecurityRequirementList);

begin
  ReadObjectArray(aObj,@ReadSecurityRequirementObject);
end;

function TOpenAPIReader.ReadTagObject(aParent: TBaseOpenAPIObject; aName: TJSONStringType): TBaseOpenAPIObject;

begin
  Result:=TTag.Create(aParent,aName);
  try
    ReadTag(TTag(Result),False);
  except
    Result.Free;
    Raise;
  end;
end;

procedure TOpenAPIReader.ReadTagList(aObj: TTagList);
begin
  ReadObjectArray(aObj,@ReadTagObject);
end;

procedure TOpenAPIReader.ReadFromScanner(aOpenAPI: TOpenAPI; AScanner: TJSONScanner);
begin
  FScanner:=aScanner;
  try
    ReadOpenApi(aOpenApi);
  finally
    FScanner:=Nil;
  end;
end;

procedure TOpenAPIReader.ReadFromFile(aOpenAPI: TOpenAPI; const AFilename: String);

var
  F : TFileStream;
begin
  F:=TFileStream.Create(aFilename,fmOpenRead or fmShareDenyWrite);
  try
    ReadFromStream(aOpenAPI,F);
  finally
    F.Free;
  end;
end;

procedure TOpenAPIReader.ReadFromStream(aOpenAPI: TOpenAPI; AStream: TStream);

var
  S : TJSONScanner;

begin
  S:=TJSONScanner.Create(aStream,[joUTF8]);
  try
    ReadFromScanner(aOpenAPI,S);
  finally
    S.Free;
  end;
end;

procedure TOpenAPIReader.ReadFromString(aOpenAPI: TOpenAPI; const AString: TJSONStringType);
var
  S: TStringStream;
begin
  S:=TStringStream.Create(AString);
  try
    ReadFromStream(aOpenAPI,S);
  finally
    S.Free;
  end;
end;

end.
