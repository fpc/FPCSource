{
    This file is part of the Free Component Library

    JSON Schema validator - validate JSON data
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FpJson.Schema.Validator;

{$mode ObjFPC}
{$H+}
{$modeswitch typehelpers}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, FpJson.Data, FpJson.Schema.Types, FpJson.Schema.Schema;
  {$ELSE}
  Classes, SysUtils, contnrs, fpjson, FpJson.Schema.Types, FpJson.Schema.Schema;
  {$ENDIF}

Type
  TValidationMessageType = (vmtInfo,vmtWarning,vmtError);
  TValidationMessageTypes = set of TValidationMessageType;

  { TValidationMessageTypeHelper }

  TValidationMessageTypeHelper = type helper for TValidationMessageType
  private
    Function ToString: String;
  public
    Property AsString : String Read ToString;
  end;

  { TValidationMessage }

  TValidationMessage = Class(TObject)
  private
    FKeyword: TJSONSchemaKeyword;
    FMessage: String;
    FPath: TJSONStringType;
    FSchema: TJSONSchema;
    FType: TValidationMessageType;
  Public
    Constructor Create(aSchema :TJSONSchema; aKeyword : TJSONSchemaKeyword; aType : TValidationMessageType; const aMessage : string; const aPath : TJSONStringType);
    procedure AsJSON(aJSON : TJSONObject); virtual;
    function AsJSON : TJSONObject;
    Property Schema : TJSONSchema Read FSchema Write FSchema;
    Property Keyword : TJSONSchemaKeyword Read FKeyword Write FKeyword;
    Property MessageType : TValidationMessageType Read FType Write FType;
    Property Message : String Read FMessage Write FMessage;
    Property Path : TJSONStringType Read FPath Write FPath;
  end;

  { TValidationMessageList }

  TValidationMessageList = Class sealed (TFPObjectList)
  private
    function GetMessage(aIndex : Integer): TValidationMessage;
  Public
    procedure AddMessages(aList:TValidationMessageList);
    Procedure Add(aMessage : TValidationMessage); reintroduce;
    procedure AsJSON(aJSON : TJSONArray); virtual;
    function AsJSON : TJSONArray;
    function AddMessage(aSchema : TJSONSchema; aKeyword : TJSONSchemaKeyword; aType: TValidationMessageType; const aMessage : String; const aPath : TJSONStringType) : TValidationMessage;
    Function ErrorCount(StartAt : Integer = 0) : Integer;
    Property Messages[aIndex : Integer] : TValidationMessage Read GetMessage; default;
  end;

  { TJSONSchemaValidator }
  TResolveSchemaEvent = Procedure (Sender : TObject; const aRef : String; Var aSchema : TJSONSchema) of object;
  TJSONSchemaValidator = class(TComponent)
  private
    FEpsilon: Double;
    FMessages: TValidationMessageList;
    FMessageTypes: TValidationMessageTypes;
    FOnResolveSchemaURI: TResolveSchemaEvent;
    FPath : Array of String;
    FPathCount : Integer;
    FLastArr : TJSONArray;
    FLastContains : TJSONSchema;
    FLastContainsCount : Integer;
    procedure CleanUpValidation;
    function GetCurrentPath: String;
  protected
    procedure PushPath(aPath : String);
    procedure PopPath;
    function CreateMessageList : TValidationMessageList;
    function SaveMessages : TValidationMessageList;
    procedure RestoreMessages(aList : TValidationMessageList);
    function AddMessage(aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword; aType: TValidationMessageType; const aMessage: String;
      const aPath: TJSONStringType): TValidationMessage;
    function AddMessage(aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword; aType: TValidationMessageType; const aMessage: String
      ): TValidationMessage;
    procedure Info(aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword; aValue: string);
    function EqualData(aData1, aData2: TJSONData): Boolean; virtual;
    // Other checks. To be overridden
    function CheckOtherKeyword(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONStringType) : Boolean; virtual;
    function CheckOtherKeyword(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword) : Boolean; virtual;
    // String value checks
    function CheckFormat(aSchema: TJSONSchema; aValue: TJSONStringType; aFormat: TStringFormatValidator) : Boolean; virtual;
    function CheckString(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword) : Boolean; virtual;
    // Numerical value checks
    function CheckNumerical(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean; virtual;
    // Array value checks
    function CheckArray(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean; virtual;
    function CheckArrayItems(Arr: TJSONArray; aStart: Integer; aItems: TJSONSchemaList): Integer; virtual;
    function CheckArrayPrefixItems(Arr: TJSONArray; aItems: TJSONSchemaList): Integer; virtual;
    function CheckArrayUniqueItems(Arr: TJSONArray): Integer; virtual;
    function GetArrayContainsCount(Arr: TJSONArray; aContains: TJSONSchema): Integer; virtual;
    // Object checks
    function CheckObject(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean; virtual;
    function CheckObjectPatternProperties(Obj: TJSONObject; aProperties: TJSONSchemaList): Boolean; virtual;
    function CheckObjectProperties(Obj: TJSONObject; aProperties: TJSONSchemaList): Boolean; virtual;
    function CheckObjectproperty(Obj: TJSONObject; aIndex: Integer; aSchema: TJSONSchema): Boolean; virtual;
    function CheckObjectAdditionalProperties(Obj: TJSONObject; aSchema: TJSONSchema): Boolean;
    function CheckObjectPropertyNames(Obj: TJSONObject; aSchema: TJSONSchema): Boolean;
    procedure CollectObjectProperties(Obj: TJSONObject; aProperties: TJSONSchemaList; aList: TStrings);
    procedure CollectPatternProperties(Obj: TJSONObject; aProperties: TJSONSchemaList; aList: TStrings);
    function GetMissingObjectProperties(Obj: TJSONObject; aList: TStrings): String; virtual;
    function CheckObjectDependentRequired(Obj: TJSONObject; aSchema: TJSONSchema): Boolean;
    function CheckObjectDependentSchemas(Obj: TJSONObject; aList: TJSONSchemaList): Boolean;
    // General checks
    function CheckConst(aJSON: TJSONData; aSchema: TJSONSchema): Boolean;
    function CheckEnum(aJSON: TJSONData; aSchema: TJSONSchema; aList : TJSONArray): Boolean;
    function CheckType(aJSON: TJSONData; aSchema: TJSONSchema) : Boolean; virtual;
    function CheckInList(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean;
    function CheckNot(aJSON: TJSONData; aNotSchema: TJSONSchema): Boolean;
    function CheckIf(aJSON: TJSONData; aSchema: TJSONSchema): Boolean;
    // Entry point for keywords
    function CheckKeyword(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword) : Boolean; virtual;
     //
    function ResolveSchema(aSchema: TJSONSchema): TJSONSchema; virtual;

    procedure DoValidateJSON(aJSON : TJSONData; aSchema : TJSONSchema); virtual;
    property CurrentPath : String Read GetCurrentPath;
  Public
    constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Reset;
    Function ValidateJSON(aJSON : TJSONData; aSchema : TJSONSchema) : Boolean;
    property Messages : TValidationMessageList Read FMessages;
    Property MessageTypes : TValidationMessageTypes Read FMessageTypes Write FMessageTypes;
    Property Epsilon : Double Read FEpsilon Write FEpsilon;
    Property OnResolveSchemaURI : TResolveSchemaEvent Read FOnResolveSchemaURI Write FOnResolveSchemaURI;
  end;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  FpJson.Schema.Consts, System.Regexpr, Fcl.UriParser
  {$ELSE}
  FpJson.Schema.Consts, RegExpr, URIParser
  {$ENDIF}
  ;


{ TValidationMessageTypeHelper }

function TValidationMessageTypeHelper.ToString: String;

Const
  Names : Array[TValidationMessageType] of string = ('Info','Warning','Error');

begin
  Result:=Names[Self];
end;

{ TValidationMessage }

constructor TValidationMessage.Create(aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword; aType: TValidationMessageType;
  const aMessage: string; const aPath: TJSONStringType);
begin
  FType:=aType;
  FSchema:=aSchema;
  FMessage:=aMessage;
  FPath:=aPath;
  FKeyword:=aKeyword;
end;

procedure TValidationMessage.AsJSON(aJSON: TJSONObject);
begin
  aJSON.Add('schema',Schema.Path);
  if Schema.HasKeywordData(jskID) then
    aJSON.Add('schemaID',Schema.ID);
  aJSON.Add('keyword',Keyword.AsString);
  aJSON.Add('type',MessageType.AsString);
  aJSON.Add('path',Path);
  aJSON.Add('message',Message);
end;

function TValidationMessage.AsJSON: TJSONObject;
begin
  Result:=TJSONObject.Create;
  try
    asJSON(Result);
  except
    Result.Free;
    Raise;
  end;
end;

{ TValidationMessageList }

function TValidationMessageList.GetMessage(aIndex : Integer): TValidationMessage;
begin
  Result:=Items[aIndex] as TValidationMessage;
end;

procedure TValidationMessageList.AddMessages(aList: TValidationMessageList);

var
  Msg : TValidationMessage;
  I : Integer;

begin
  For I:=0 to aList.Count-1 do
    begin
    Msg:=aList.Extract(aList[i]) as TValidationMessage;
    Add(Msg);
    end;
end;

procedure TValidationMessageList.Add(aMessage: TValidationMessage);
begin
  Inherited add(aMessage);
end;

procedure TValidationMessageList.AsJSON(aJSON: TJSONArray);

var
  I : integer;

begin
  For I:=0 to Count-1 do
    aJSON.Add(Messages[i].AsJSON);
end;

function TValidationMessageList.AsJSON: TJSONArray;
begin
  Result:=TJSONArray.Create;
  try
    AsJSON(Result);
  except
    Result.Free;
    Raise;
  end;
end;

function TValidationMessageList.AddMessage(aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword; aType: TValidationMessageType;
  const aMessage: String; const aPath: TJSONStringType): TValidationMessage;
begin
  Result:=TValidationMessage.Create(aSchema,aKeyword,aType,aMessage,aPath);
  Add(Result);
end;

function TValidationMessageList.ErrorCount(StartAt: Integer): Integer;

var
  i : Integer;

begin
  Result:=0;
  For I:=StartAt to Count-1 do
    if GetMessage(I).MessageType=vmtError then
      inc(Result);
end;

{ TJSONSchemaValidator }

function TJSONSchemaValidator.GetCurrentPath: String;

var
  i : Integer;

begin
  Result:='';
  if FPathCount=0 then Exit;
  Result:=FPath[0];
  For I:=1 to FPathCount-1 do
    Result:=Result+'.'+FPath[i];
end;

procedure TJSONSchemaValidator.PushPath(aPath: String);
begin
  if FPathCount=Length(FPath) then
    SetLength(FPath,FPathCount+10);
  FPath[FPathCount]:=aPath;
  inc(FPathCount);
end;

procedure TJSONSchemaValidator.PopPath;
begin
  if FPathCount>0 then
    Dec(FPathCount);
end;

function TJSONSchemaValidator.CreateMessageList: TValidationMessageList;
begin
  Result:=TValidationMessageList.Create(True);
end;

function TJSONSchemaValidator.SaveMessages: TValidationMessageList;
begin
  Result:=FMessages;
  FMessages:=CreateMessageList;
end;

procedure TJSONSchemaValidator.RestoreMessages(aList: TValidationMessageList);
begin
  FreeAndNil(FMessages);
  FMessages:=aList;
end;

function TJSONSchemaValidator.ResolveSchema(aSchema: TJSONSchema): TJSONSchema;

var
  aRef,OurID : String;
  S : TJSONSchema;

  URI : TURI;
  I : Integer;

begin
  if Not (aSchema.HasKeywordData(jskRef) or aSchema.HasKeywordData(jskDynamicRef)) then
    Exit(aSchema);
  aRef:=aSchema.Ref;
  if aRef='' then
    aRef:=aSchema.DynamicRef;
  if aRef='' then
    Exit(aSchema);
  Result:=Nil;
  if aSchema.HasKeywordData(jskId) then
    OurID:=aSchema.ID;
  if Pos(ourID,aRef)=1 then
    Delete(aRef,1,Length(OurID));

  URI:=ParseURI(aRef,'',0,True);
  if (Uri.Protocol='') then
    begin
    // Local ref
    if (Uri.Path='') and (Uri.Document='') then
      Result:=aSchema.Find('#'+Uri.BookMark)
    else
      begin
      // search defs
      I:=0;
      While (Result=Nil) and (I<aSchema.Defs.Count) do
        begin
        S:=aSchema.Defs[i];
        if S.HasKeywordData(jskAnchor) and (Uri.BookMark=S.Anchor) then
          Result:=S
        else if S.HasKeywordData(jskDynamicAnchor) and (Uri.BookMark=S.DynamicAnchor) then
          Result:=S
        else if (S.Name<>'') and (Uri.BookMark=S.Name) then
          Result:=S;
        Inc(I);
        end;
      end;
    end
  else
    Result:=Nil;
  if (Result=Nil) and Assigned(OnResolveSchemaURI) then
    OnResolveSchemaURI(Self,aRef,Result);
end;

function TJSONSchemaValidator.CheckOtherKeyword(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONStringType): Boolean;

begin
  // Do nothing

end;

function TJSONSchemaValidator.CheckOtherKeyword(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean;

begin
  CheckOtherKeyword(aJSON,aSchema,aKeyword.AsString);
end;

function TJSONSchemaValidator.CheckType(aJSON: TJSONData; aSchema: TJSONSchema): Boolean;

Const
  JSONTypes : Array[TSchemaSimpleType] of TJSONType = (
    jtUnknown, jtNull,jtBoolean,jtNumber,jtNumber,jtString,jtArray,jtObject,jtUnknown
  );

var
  St : TSchemaSimpleType;
  Found : Boolean;

begin
  if aSchema.Validations.Types=[] then
    exit;
  Found:=False;
  for St in aSchema.Validations.Types do
    if st<>sstNone then
      begin
      if st=sstAny then
        Found:=True
      else
        begin
        Found:=Found or (aJSON.JSONType=JSONTypes[st]);
        if Found and (ST=sstInteger) then
          Found:=TJSONNumber(aJSON).NumberType=ntInteger;
        end;
      end;
  if not Found then
    AddMessage(aSchema,jskType,vmtError,Format(SErrTypeMismatch,[JSONTypeName(aJSON.JSONType),aSchema.Validations.Types.AsString]));
  Result:=Found;
end;

procedure TJSONSchemaValidator.Info(aSchema : TJSONSchema; aKeyword : TJSONSchemaKeyword; aValue : string);

begin
  if vmtInfo in MessageTypes then
    AddMessage(aSchema,aKeyword,vmtInfo,Format(SSchemaInfo,[aKeyword.AsString,aValue]),CurrentPath);
end;

function TJSONSchemaValidator.CheckFormat(aSchema: TJSONSchema; aValue: TJSONStringType; aFormat: TStringFormatValidator): Boolean;

var
  OK : Boolean;

begin
  OK:=True;
  Result:=OK;
end;

function TJSONSchemaValidator.CheckString(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean;

var
  sCheck,sCond : TJSONStringType;
  OK : Boolean;
  Rex : TRegExpr;

begin
  if aJSON.JSONType<>jtString then
    begin
    AddMessage(aSchema, aKeyword, vmtWarning, Format(SNotStringData, [aJSON.AsJSON, aKeyword.AsString]), CurrentPath);
    exit(False);
    end;
  sCheck:=aJSON.AsString;
  case aKeyword of
    jskMinLength :
      begin
      Ok:=Length(sCheck)>=aSchema.Validations.MinLength;
      sCond:=IntToStr(aSchema.Validations.MinLength);
      end;
    jskMaxLength :
      begin
      Ok:=Length(sCheck)<=aSchema.Validations.MaxLength;
      sCond:=IntToStr(aSchema.Validations.MaxLength);
      end;
    jskPattern:
      begin
      sCond:=aSchema.Validations.Pattern;
      Rex:=TRegExpr.Create(sCond);
      try
        Ok:=Rex.Exec(sCheck);
      finally
        Rex.Free;
      end;
      end;
    jskFormat:
      begin
      OK:=CheckFormat(aSchema,sCheck,aSchema.Validations.FormatValidator);
      end;
    jskContentEncoding,
    jskContentMediaType,
    jskContentSchema:
       info(aSchema,aKeyword,Format(SNotImplementedInValidator,[ClassName]));
  end;
  if not OK then
    AddMessage(aSchema, aKeyword, vmtError, Format(SViolatesStringCondition, [sCheck, aKeyword.AsString, sCond]));
  Result:=OK;
end;

function TJSONSchemaValidator.CheckNumerical(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean;

var
  OK : Boolean;
  Tmp,TValue,CondValue : double;
begin
  if aJSON.JSONType<>jtNumber then
    begin
    AddMessage(aSchema, aKeyword, vmtWarning, Format(SNotNumericalData, [aJSON.AsJSON, aKeyword.AsString]), CurrentPath);
    exit(False);
    end;
  TValue:=aJSON.AsFloat;
  case aKeyword of
    jskMultipleOf :
       begin
       CondValue:=aschema.Validations.MultipleOf;
       Tmp:=TValue/CondValue;
       OK:=Frac(Tmp)<Epsilon;
       end;
    jskMaximum :
       begin
       CondValue:=aschema.Validations.Maximum;
       OK:=(TValue<=CondValue);
       end;
    jskExclusiveMaximum :
      begin
      CondValue:=aschema.Validations.ExclusiveMaximum;
      OK:=(CondValue-TValue)>FEpsilon;
      end;
    jskMinimum :
      begin
      CondValue:=aschema.Validations.Minimum;
      OK:=(TValue>=CondValue);
      end;
    jskExclusiveMinimum :
      begin
      CondValue:=aschema.Validations.ExclusiveMinimum;
      OK:=(TValue-CondValue)>FEpsilon;
      end;
  end;
  if not OK then
    AddMessage(aSchema, aKeyword, vmtError, Format(SViolatesNumericalCondition, [TValue, aKeyword.AsString, condValue]));
  Result:=OK;
end;

function TJSONSchemaValidator.CheckArrayUniqueItems(Arr : TJSONArray) : Integer;
// Return index of element that does not correspond.

var
  S : TJSONStringType;
  HT : TFPStringHashTable;
  I,aCount : Integer;

begin
  Result:=-1;
  aCount:=Arr.Count;
  if aCount<2 then
    exit;
  HT:=TFPStringHashTable.Create;
  try
    I:=0;
    While (Result=-1) and (I<aCount) do
      begin
      S:=Arr.Items[I].AsJSON;
      if HT.Items[S]='' then
        HT.Add(S,'Present')
      else
        Result:=I;
      Inc(I);
      end;
  finally
    HT.Free;
  end;
end;

function TJSONSchemaValidator.GetArrayContainsCount(Arr : TJSONArray; aContains : TJSONSchema) : Integer;

var
  M : TValidationMessageList;
  I : Integer;

begin
  if ((Arr=FLastArr) and (aContains=FLastContains)) then
    Exit(FLastContainsCount);
  FLastArr:=Arr;
  FLastContains:=aContains;
  FLastContainsCount:=0;
  M:=SaveMessages;
  try
    For I:=0 to Arr.Count-1 do
      begin
      DoValidateJSON(Arr[i],aContains);
      if (Messages.ErrorCount=0) then
        inc(FLastContainsCount);
      Messages.Clear;
      end;
  finally
    RestoreMessages(M);
  end;
  Result:=FLastContainsCount;
end;


function TJSONSchemaValidator.GetMissingObjectProperties(Obj : TJSONObject; aList : TStrings) : String;

var
  N : String;

begin
  Result:='';
  For N in aList do
    if Obj.IndexOfName(N)=-1 then
      begin
      if (Result<>'') then
        Result:=Result+',';
      Result:=Result+N;
      end;
end;

function TJSONSchemaValidator.CheckObjectproperty(Obj : TJSONObject; aIndex : Integer; aSchema :TJSONSchema) : Boolean;

var
  aCount : Integer;

begin
  aCount:=Messages.Count;
  PushPath(Obj.Names[aIndex]);
  ValidateJSON(Obj.Items[aIndex],aSchema);
  Result:=Messages.ErrorCount(aCount)=0;
  PopPath;
end;

function TJSONSchemaValidator.CheckObjectProperties(Obj : TJSONObject; aProperties: TJSONSchemaList) : Boolean;

var
  I,Idx : Integer;

begin
  Result:=True;
  for I:=0 to aProperties.Count-1 do
    begin
    Idx:=Obj.IndexOfName(aProperties[I].Name);
    if Idx<>-1 then
      Result:=CheckObjectproperty(Obj,Idx,aProperties[i]) and Result;
    end;
end;

procedure TJSONSchemaValidator.CollectObjectProperties(Obj : TJSONObject; aProperties: TJSONSchemaList; aList : TStrings);

var
  I,Idx : integer;

begin
  For I:=0 to aProperties.Count-1 do
    begin
    Idx:=Obj.IndexOfName(aProperties[I].Name);
    if Idx<>-1 then
      aList.Add(Obj.Names[Idx]);
    end;
end;

procedure TJSONSchemaValidator.CollectPatternProperties(Obj : TJSONObject; aProperties: TJSONSchemaList; aList : TStrings);

var
  I,Idx : integer;
  Rex : TRegExpr;

begin
  For I:=0 to aProperties.Count-1 do
    begin
    Rex:=TRegExpr.Create(aProperties[i].Name);
    try
      for Idx:=0 to Obj.Count-1 do
        begin
        if Rex.Exec(Obj.Names[Idx]) then
          aList.AddObject(Obj.Names[Idx],Obj.Items[Idx]);
        end;
    finally
      Rex.Free;
    end;
    end;
end;

function TJSONSchemaValidator.CheckObjectAdditionalProperties(Obj : TJSONObject; aSchema : TJSONSchema) : Boolean;

var
  List : TStringList;
  lSchema : TJSONSchema;
  I : Integer;

begin
  Result:=True;
  lSchema:=aSchema.AdditionalProperties;
  if (lSchema=Nil) or (lSchema.MatchType=smAny) then
    exit;
  List:=TstringList.Create;
  try
    CollectObjectProperties(Obj,aSchema.Properties,List);
    CollectPatternProperties(Obj,aSchema.PatternProperties,List);
    for I:=0 to Obj.Count-1 do
      if List.IndexOf(Obj.Names[I])<>-1 then
        Result:=ValidateJSON(Obj.Items[i],lSchema) and Result
  finally
    List.Free;
  end;
end;

function TJSONSchemaValidator.CheckObjectPatternProperties(Obj : TJSONObject; aProperties: TJSONSchemaList) : Boolean;

var
  I,Idx : Integer;
  Rex : TRegexpr;

begin
  Result:=True;
  For I:=0 to aProperties.Count-1 do
    begin
    Rex:=TRegExpr.Create(aProperties[i].Name);
    try
      for Idx:=0 to Obj.Count-1 do
        begin
          if Rex.Exec(Obj.Names[Idx]) then
            Result:=CheckObjectproperty(Obj,Idx,aProperties[i]) and Result;
          end;
      finally
        Rex.Free;
      end;
    end;
end;

function TJSONSchemaValidator.CheckObjectDependentRequired(Obj: TJSONObject; aSchema: TJSONSchema) : Boolean;

var
  I : Integer;
  D : TSchemaDependentRequired;
  Itm : TJSONData;
  N : String;


begin
  Result:=True;
  if (aSchema.Validations.DependentRequired.Count=0) then exit;
  For I:=0 to aSchema.Validations.DependentRequired.Count-1 do
    begin
    D:=aSchema.Validations.DependentRequired[I];
    Itm:=Obj.Find(D.Name);
    if Assigned(Itm) then
      begin
      PushPath(D.Name);
      For N in D.Required do
        if Obj.IndexOfName(N)=-1 then
          begin
          Result:=False;
          AddMessage(aSchema,jskDependentRequired, vmtError,Format(SErrMissingRequiredDependent,[N]));
          end;
      PopPath;
      end;
    end;
end;

function TJSONSchemaValidator.CheckObjectDependentSchemas(Obj: TJSONObject; aList: TJSONSchemaList): Boolean;

var
  I,Current : Integer;
  S : TJSONSchema;
  Itm : TJSONData;

begin
  Result:=True;
  if (aList.Count=0) then exit;
  For I:=0 to aList.Count-1 do
    begin
    S:=aList[I];
    if S.Name<>'' then
      begin
      Itm:=Obj.Find(S.Name);
      if Assigned(Itm) then
        begin
        PushPath(S.Name);
        Current:=Messages.Count;
        DoValidateJSON(Itm,S);
        if Messages.ErrorCount(Current)>0 then
          Result:=False;
        PopPath;
        end;
      end;
    end;
end;

function TJSONSchemaValidator.CheckObjectPropertyNames(Obj: TJSONObject; aSchema: TJSONSchema): Boolean;

var
  J : TJSONString;
  I : Integer;

begin
  Result:=True;
  if aSchema.MatchType=smAny then exit;
  J:=TJSONString.Create('');
  Try
    For I:=0 to Obj.Count-1 do
      begin
      J.AsString:=Obj.names[I];
      PushPath(Obj.names[I]);
      ValidateJSON(J,aSchema);
      PopPath;
      end;
  finally
    J.Free;
  end;
end;


function TJSONSchemaValidator.CheckObject(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean;

var
  ShowError,OK : Boolean;
  Obj : TJSONObject absolute aJSON;
  sValue,sCond : String;

begin
  if aJSON.JSONType<>jtObject then
    begin
    AddMessage(aSchema, aKeyword, vmtWarning, Format(SNotArrayData, [aJSON.AsJSON, aKeyword.AsString]), CurrentPath);
    exit(False);
    end;
  ShowError:=True;
  case aKeyword of
    jskMaxProperties:
      begin
      ok:=aJSON.Count<=aSchema.Validations.MaxProperties;
      if not OK then
        begin
        sValue:=IntToStr(aJSON.Count);
        sCond:=IntToStr(aSchema.Validations.MaxProperties);
        end;
      end;
    jskMinProperties:
      begin
      ok:=aJSON.Count>=aSchema.Validations.MinProperties;
      if not OK then
        begin
        sValue:=IntToStr(aJSON.Count);
        sCond:=IntToStr(aSchema.Validations.MinProperties);
        end;
      end;
    jskRequired:
      begin
      SValue:=GetMissingObjectProperties(Obj,aSchema.Validations.Required);
      OK:=sValue='';
      if not OK then
        sCond:=aSchema.Validations.Required.CommaText;
      end;
    jskAdditionalProperties:
      begin
      OK:=CheckObjectAdditionalProperties(Obj,aSchema);
      ShowError:=False;
      end;
    jskPropertyNames:
      begin
      OK:=CheckObjectPropertyNames(Obj,aSchema.PropertyNames);
      ShowError:=False;
      end;
    jskPatternProperties:
      begin
      OK:=CheckObjectPatternProperties(Obj,aSchema.PatternProperties);
      ShowError:=False;
      end;
    jskProperties:
       begin
       CheckObjectProperties(Obj,aSchema.Properties);
       ShowError:=False;
       end;
    jskDependentRequired :
      begin
      CheckObjectDependentRequired(Obj,aSchema);
      ShowError:=False;
      end;
    jskDependentSchemas:
      begin
      CheckObjectDependentSchemas(Obj,aSchema.DependentSchemas);
      ShowError:=False;
      end;
    jskUnevaluatedProperties:
      // Todo
      ;
  end;
  if ShowError and not OK then
    AddMessage(aSchema,aKeyword,vmtError,Format(SViolatesObjectCondition,[sValue,aKeyword.AsString,sCond]));
  Result:=OK;
end;

function TJSONSchemaValidator.CheckArrayItems(Arr : TJSONArray; aStart : Integer; aItems : TJSONSchemaList) : Integer;
// Return index of element that does not correspond to schema.
var
  M : TValidationMessageList;
  I,aCount : Integer;
  aSchema: TJSONSchema;

begin
  Result:=-1;
  if aItems.Count<1 then exit;
  aSchema:=aItems[0];
  aCount:=Arr.Count;
  M:=SaveMessages;
  try
    I:=aStart;
    While (Result=-1) and (I<aCount) do
      begin
      Messages.Clear;
      DoValidateJSON(Arr[i],aSchema);
      if (Messages.ErrorCount>0) then
        Result:=I;
      inc(I);
      end;
    M.AddMessages(Messages);
  finally
    RestoreMessages(M);
  end;
end;

function TJSONSchemaValidator.CheckArrayPrefixItems(Arr : TJSONArray; aItems : TJSONSchemaList) : Integer;
// Return index of first element that does not correspond to schema in aItems.
var
  M : TValidationMessageList;
  I,aCount : Integer;

begin
  // if there are less items than in prefix count, this is OK.
  aCount:=aItems.Count;
  if aCount>Arr.Count then
    aCount:=Arr.Count;
  Result:=-1;
  M:=SaveMessages;
  try
    I:=0;
    While (Result=-1) and (I<aCount) do
      begin
      Messages.Clear;
      PushPath(IntToStr(I));
      DoValidateJSON(Arr[i],aItems[I]);
      if (Messages.ErrorCount>0) then
        Result:=I;
      Inc(I);
      PopPath;
      end;
    M.AddMessages(Messages);
  finally
    RestoreMessages(M);
  end;
end;

function TJSONSchemaValidator.CheckArray(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean;

var
  OK : Boolean;
  Arr : TJSONArray absolute aJSON;
  sCond,sValue : String;
  aStartAt,aCount : Integer;

begin
  if aJSON.JSONType<>jtArray then
    begin
    AddMessage(aSchema, aKeyword, vmtWarning, Format(SNotArrayData, [aJSON.AsJSON, aKeyword.AsString]), CurrentPath);
    exit(False);
    end;
  OK:=true;
  case aKeyword of
  jskUniqueItems :
    begin
    if aSchema.Validations.UniqueItems then
      begin
      aCount:=CheckArrayUniqueItems(arr);
      OK:=(aCount=-1);
      if Not OK then
        begin
        sCond:='';
        sValue:=Format('%d',[aCount]);
        end;
      end;
    end;
  jskMaxItems :
    begin
    aCount:=aSchema.Validations.MaxItems;
    OK:=Arr.Count<=aCount;
    if not OK then
      begin
      sCond:=IntToStr(aCount);
      sValue:=IntToStr(Arr.Count);
      end;
    end;
  jskMinItems :
    begin
    aCount:=aSchema.Validations.MinItems;
    OK:=Arr.Count>=aCount;
    if not OK then
      begin
      sCond:=IntToStr(aCount);
      sValue:=IntToStr(Arr.Count);
      end;
    end;
  jskContains:
    begin
    aCount:=GetArrayContainsCount(Arr,aSchema.Contains);
    if (aCount=0) then
      begin
      OK:=(aSchema.HasKeywordData(jskMinContains) and
          (aSchema.Validations.MinContains=0));
      if not OK then
        begin
        sCond:='';
        sValue:='0';
        end;
      end;
    end;
  jskMaxContains :
    if aSchema.HasKeywordData(jskContains) then
      begin
      aCount:=GetArrayContainsCount(Arr,aSchema.Contains);
      OK:=aCount<=aSchema.Validations.MaxContains;
      if not Ok then
        begin
        sCond:=IntToStr(aSchema.Validations.MaxContains);
        sValue:=IntToStr(aCount);
        end;
      end;
  jskMinContains :
    if aSchema.HasKeywordData(jskContains) then
      begin
      aCount:=GetArrayContainsCount(Arr,aSchema.Contains);
      OK:=aCount>=aSchema.Validations.MinContains;
      if not Ok then
        begin
        sCond:=IntToStr(aSchema.Validations.MinContains);
        sValue:=IntToStr(aCount);
        end;
      end;
  jskItems:
     begin
     if aSchema.HasKeywordData(jskPrefixItems) then
       aStartAt:=aSchema.PrefixItems.Count
     else
       aStartAt:=0;
     aCount:=CheckArrayItems(Arr,aStartAt,aSchema.Items);
     OK:=aCount=-1;
     if Not OK then
       begin
       sCond:='<Items list>';
       sValue:=Format('Item %d',[aCount]);
       end;
     end;
  jskPrefixItems:
     begin
     acount:=CheckArrayPrefixItems(Arr,aSchema.PrefixItems);
     OK:=aCount=-1;
     if Not OK then
       begin
       sCond:='<Items list>';
       sValue:=Format('Item %d',[aCount]);
       end;
     end;
  jskUnevaluatedItems:
    // Todo
    ;
  end;
  if not OK then
    AddMessage(aSchema, aKeyword,vmtError, Format(SViolatesArrayCondition, [sValue, aKeyword.AsString,sCond]));
  Result:=OK;
end;

function TJSONSchemaValidator.CheckNot(aJSON : TJSONData; aNotSchema : TJSONSchema) : Boolean;

var
  M : TValidationMessageList;

begin
  M:=SaveMessages;
  try
    DoValidateJSON(aJSON,aNotSchema);
    Result:=Messages.ErrorCount>0;
  finally
    RestoreMessages(M);
  end;
  if not Result then
    AddMessage(aNotSchema,jskNot,vmtError,SErrSchemaMatchesNot)
end;

function TJSONSchemaValidator.CheckIf(aJSON: TJSONData; aSchema: TJSONSchema): Boolean;
var
  M : TValidationMessageList;
  CondSchema : TJSONSchema;
  IfOK : Boolean;


begin
  Result:=True;
  // Exit if no condition
  if not Assigned(aSchema.IfSchema) then
    exit;
  // Exit if no conditions
  if not (Assigned(aSchema.ThenSchema) or Assigned(aSchema.ThenSchema)) then
    exit;
  M:=SaveMessages;
  try
    DoValidateJSON(aJSON,aSchema.IfSchema);
    IfOK:=Messages.ErrorCount=0;
    Info(aSchema.IfSchema,jskIf,Format(SIfResult,[aSchema.IfSchema.Path,BoolToStr(IfOK,'True','False')]));
    Messages.Clear;
    if IfOK then
      CondSchema:=aSchema.ThenSchema
    else
      CondSchema:=aSchema.ElseSchema;
    if Assigned(CondSchema) then
      begin
      DoValidateJSON(aJSON,condSchema);
      Result:=Messages.ErrorCount=0;
      end;
    if not Result then
      M.AddMessages(Messages);
  finally
    RestoreMessages(M);
  end;
end;

function TJSONSchemaValidator.CheckInList(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean;

var
  aList : TJSONSchemaList;
  i,aCount : Integer;
  M : TValidationMessageList;

begin
  Result:=True;
  aList:=Nil;
  Case aKeyword of
    jskAnyOf : aList:=aSchema.AnyOf;
    jskAllOf : aList:=aSchema.AllOf;
    jskOneOf : aList:=aSchema.OneOf;
  end;
  if (Not Assigned(aList)) or (aList.Count=0) then
    Exit;
  M:=SaveMessages;
  try
    aCount:=0;
    for I:=0 to aList.Count-1 do
      begin
      Messages.Clear;
      DoValidateJSON(aJSON,AList[i]);
      if Messages.ErrorCount=0 then
        Inc(aCount);
      end;
    Case aKeyword of
      jskAnyOf : Result:=(aCount>0);
      jskAllOf : Result:=(aCount=aList.Count);
      jskOneOf : Result:=(aCount=1);
    end
  finally
    RestoreMessages(M);
  end;
  if not Result then
    AddMessage(aSchema,aKeyword,vmtError,Format(SErrListCountMismatch,[aKeyword.AsString,aCount,aList.Count]));
end;

function TJSONSchemaValidator.EqualData(aData1,aData2 : TJSONData) : Boolean;

begin
  Result:=aData1.asJSON=aData2.AsJSON;
end;

function TJSONSchemaValidator.CheckConst(aJSON: TJSONData; aSchema : TJSONSchema) : Boolean;

var
  aData : TJSONData;

begin
  Result:=True;
  aData:=aSchema.Validations.constValue;
  if not assigned(aData) then exit;
  Result:=EqualData(aJSON,aData);
  if not Result then
    AddMessage(aSchema,jskConst,vmtError,Format(SErrNotEqual,[aData.AsJSON]));
end;

function TJSONSchemaValidator.CheckEnum(aJSON: TJSONData; aSchema: TJSONSchema; aList: TJSONArray): Boolean;
var
  I,aCount : Integer;

begin
  aCount:=aList.Count;
  if aCount=0 then
    exit;
  Result:=False;
  I:=0;
  While (Not Result) and (I<aCount) do
    begin
    Result:=EqualData(aJSON,aList[i]);
    Inc(I);
    end;
  if not Result then
    AddMessage(aSchema,jskConst,vmtError,Format(SErrNotInList,[aList.AsJSON]));
end;

function TJSONSchemaValidator.CheckKeyword(aJSON: TJSONData; aSchema: TJSONSchema; aKeyword: TJSONSchemaKeyword): Boolean;
var
  OK : Boolean;

begin
  OK:=True;
  Case aKeyword of
    jskUnknown : ;
    jskId : Info(aSchema,jskID,aSchema.Id);
    jskAnchor : Info(aSchema,jskAnchor,aSchema.Anchor);
    jskIdDraft4 : Info(aSchema,jskID,aSchema.Id);
    jskSchema : Info(aSchema,jskID,aSchema.Schema);
    jskTitle : Info(aSchema,jskTitle,aSchema.Metadata.Title);
    jskDescription : Info(aSchema,jskDescription,aSchema.MetaData.Description);
    jskType : CheckType(aJSON,aSchema);
    jskRef: Info(aSchema,jskRef,aSchema.Ref);
    jskDynamicRef : Info(aSchema,jskRef,aSchema.DynamicRef);
    jskDynamicAnchor : Info(aSchema,jskRef,aSchema.DynamicAnchor);
    jskComment : Info(aSchema,jskComment,aSchema.Comment);
    jskVocabulary : Info(aSchema,jskComment,aSchema.Vocabulary.ToString);
    jskReadOnly : Info(aSchema,jskReadOnly,BoolToStr(aSchema.MetaData.ReadOnly,'True','False'));
    jskWriteOnly : Info(aSchema,jskWriteOnly,BoolToStr(aSchema.MetaData.WriteOnly,'True','False'));
    jskExamples : Info(aSchema,jskExamples,IntToStr(aSchema.Metadata.Examples.Count));
    jskDeprecated : Info(aSchema,jskDeprecated,BoolToStr(aSchema.MetaData.Deprecated,'True','False'));
    jskConst : CheckConst(aJSON,aSchema);
    jskDefinitions,
    jskDefs : Info(aSchema,jskDefs,IntToStr(aSchema.Defs.Count));
    jskDefault : Info(aSchema,jskDefault,'Yes');
    jskMultipleOf,
    jskMaximum,
    jskExclusiveMaximum,
    jskMinimum,
    jskExclusiveMinimum : Ok:=CheckNumerical(aJSON,aSchema,aKeyWord);
    jskMaxLength,
    jskMinLength,
    jskFormat,
    jskPattern,
    jskContentEncoding,
    jskContentMediaType,
    jskContentSchema : Ok:=CheckString(aJSON,aSchema,aKeyWord);
    jskContains,
    jskUniqueItems,
    jskMaxItems,
    jskMinItems,
    jskMaxContains,
    jskMinContains,
    jskItems,
    jskPrefixItems : Ok:=CheckArray(aJSON,aSchema,aKeyWord);
    jskDependentRequired,
    jskDependentSchemas,
    jskMaxProperties,
    jskMinProperties,
    jskRequired,
    jskAdditionalProperties,
    jskPropertyNames,
    jskPatternProperties,
    jskUnevaluatedProperties,
    jskProperties : Ok:=CheckObject(aJSON,aSchema,aKeyWord);
    jskEnum : OK:=CheckEnum(aJSON,aSchema,aSchema.Validations.Enum);
    jskAllOf,
    jskAnyOf,
    jskOneOf : OK:=CheckInList(aJSON,aSchema,aKeyword);
    jskNot : OK:=CheckNot(aJSON,aSchema.NotSchema);
    jskIf : OK:=CheckIf(aJSON,aSchema);
    jskThen,
    jskElse : ;

    jskAdditionalItems: //Old
      ;
  end;
  Result:=OK;
end;

function TJSONSchemaValidator.AddMessage(aSchema: TJSONSchema; aKeyword : TJSONSchemaKeyword; aType: TValidationMessageType; const aMessage: String; const aPath : TJSONStringType): TValidationMessage;

begin
  Result:=Messages.AddMessage(aSchema,aKeyword,aType,aMessage,aPath);
end;

function TJSONSchemaValidator.AddMessage(aSchema: TJSONSchema; aKeyword : TJSONSchemaKeyword; aType: TValidationMessageType; const aMessage: String): TValidationMessage;
begin
  Result:=AddMessage(aSchema,aKeyword, aType,aMessage,CurrentPath);
end;

procedure TJSONSchemaValidator.DoValidateJSON(aJSON: TJSONData; aSchema: TJSONSchema);

var
  aKeyword : TJSONSchemaKeyWord;
  lSchema : TJSONSchema;

begin
  // If no schema, any data matches
  if (aSchema=Nil) then
    exit;
  lSchema:=ResolveSchema(aSchema);
  // If no schema, cannot go further
  if (lSchema=Nil) then
    exit;
  case aSchema.MatchType of
    smAny : ;
    smNone : AddMessage(lSchema,jskUnknown,vmtError,SErrNoFalseMatch);
    smConstrained :
      begin
      for akeyword in TJSonSchemaKeyword do
        if lSchema.HasKeywordData(aKeyword) then
          CheckKeyword(aJSON,aSchema,aKeyword);
      CleanUpValidation;
      end;
 end;
end;

procedure TJSONSchemaValidator.CleanUpValidation;

begin
  FLastArr:=Nil;
  FLastContains:=Nil;
end;

constructor TJSONSchemaValidator.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMessages:=CreateMessageList;
  FEpsilon:=1E-14;
  FMessageTypes:=[Low(TValidationMessageType)..High(TValidationMessageType)];
end;

destructor TJSONSchemaValidator.Destroy;
begin
  FreeAndNil(FMessages);
  inherited Destroy;
end;

procedure TJSONSchemaValidator.Reset;
begin
  FPathCount:=0;
  SetLength(FPath,0);
  FMessages.Clear;
end;

function TJSONSchemaValidator.ValidateJSON(aJSON: TJSONData; aSchema: TJSONSchema): Boolean;
begin
  FMessages.Clear;
  DoValidateJSON(aJSON,aSchema);
  Result:=FMessages.ErrorCount=0;
end;

end.

