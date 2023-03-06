{
    This file is part of the Free Component Library

    WEBIDL source parser
    Copyright (c) 2021 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit webidlparser;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>=30301}
{$WARN 6060 off : }
{$ENDIF}
interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Contnrs, WebIdl.Scanner, WebIdl.Defs;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Contnrs, webidlscanner, webidldefs;
{$ENDIF FPC_DOTTEDUNITS}

Type
  EWebIDLParser = Class(Exception);

  { TWebIDLContext }

  TWebIDLVersion = {$IFDEF FPC_DOTTEDUNITS}WebIdl.Scanner{$ELSE}webidlscanner{$ENDIF}.TWebIDLVersion;

  TWebIDLContext = Class (TIDLBaseObject)
  private
    FAliases: TStrings;
    FDefinitions: TIDLDefinitionList;
    FHash : TFPObjectHashTable;
  Protected
    function FindDictionary(aName: UTF8String): TIDLDictionaryDefinition; virtual;
    function FindInterface(aName: UTF8String): TIDLInterfaceDefinition; virtual;
    procedure AppendDictionaryPartials; virtual;
    procedure AppendInterfacePartials; virtual;
    procedure AppendInterfaceIncludes; virtual;
    procedure ResolveParentTypes; virtual;
  Public
    Constructor Create(OwnsDefinitions : Boolean = True);
    Destructor Destroy; override;
    Procedure AppendPartials; virtual;
    Procedure AppendIncludes; virtual;
    Function GetInterfacesTopologically: TIDLDefinitionList; virtual;
    Procedure ResolveTypes; virtual;
    function GetDefPos(Def: TIDLBaseObject; WithoutFile: boolean = false): string; virtual;
    function IndexOfDefinition(const AName: String): Integer;
    Function FindDefinition(const AName : String) : TIDLDefinition;
    Function AsString(Full: Boolean): UTF8String; override;
    Function Add(aClass : TIDLDefinitionClass; const AName : UTF8String; const aFile: string; aLine, aCol: integer) : TIDLDefinition; override;
    Function Add(aParent : TIDLBaseObject; aClass : TIDLDefinitionClass; const AName : UTF8String; const aFile: string; aLine, aCol: integer) : TIDLDefinition; virtual;
    Property Definitions : TIDLDefinitionList Read FDefinitions;
    Property Aliases : TStrings Read FAliases Write FAliases;
  end;

  { TWebIDLParser }

  TWebIDLParser = Class
  private
    FContext: TWebIDLContext;
    FScanner: TWebIDLScanner;
    FOwnsScanner : Boolean;
    FVersion: TWebIDLVersion;
    procedure SetVersion(AValue: TWebIDLVersion);
  Protected
    function GetErrorPos: String; virtual;
    // Error mechanism
    Procedure Error(Msg : String);
    Procedure Error(Fmt : String; Args : Array of const);
    // Scanner access. Only use this, do not use scanner directly.
    function CurrentToken: TIDLToken; virtual;
    function GetToken: TIDLToken; virtual;
    function CurrentTokenString: UTF8String;
    function CurrentRow: integer; virtual;
    function CurrentColumn: integer; virtual;
    function CurrentFile: string; virtual;
    // Get next token, see if it is valid. Raise exception if not.
    procedure MaybeFree(Result: TIDLDefinition; aParent: TIDLBaseObject);
    Procedure CheckCurrentToken(aToken: TIDLToken);
    Procedure CheckCurrentTokens(aTokens: TIDLTokens);
    function ExpectToken(aToken: TIDLToken): TIDLToken;
    function ExpectTokens(aTokens: TIDLTokens): TIDLToken;
    // Extended Attributes
    function ParseExtAttributes: TExtAttributeList;
    procedure ParseExtAttributes(aList: TExtAttributeList; aTerminator: TIDLToken; ForSerializer: Boolean=False); virtual;
    // Definitions
    // Type is a type without name of the type
    function AddDefinition(aParent : TIDLBaseObject; aClass : TIDLDefinitionClass; const AName : UTF8String) : TIDLDefinition; virtual;
    function ParseAttribute(aParent: TIDLBaseObject): TIDLAttributeDefinition; virtual;
    function ParseArgument(aParent: TIDLBaseObject): TIDLArgumentDefinition; virtual;
    procedure ParseArguments(aParent: TIDLBaseObject);virtual;
    function ParseFunction(aParent: TIDLBaseObject): TIDLFunctionDefinition; virtual;
    function ParseType(aParent: TIDLBaseObject; FetchFirst: Boolean=True; AllowExtraTypes : Boolean = False): TIDLTypeDefDefinition; virtual;
    function ParseDictionaryMember(aParent: TIDLBaseObject): TIDLDictionaryMemberDefinition; virtual;
    function CompleteSimpleType(tk: TIDLToken; Var S: UTF8String; out IsNull: Boolean): TIDLToken; virtual;
    function ParseMapLikeMember(aParent: TIDLBaseObject): TIDLMaplikeDefinition; virtual;
    function ParseSetLikeMember(aParent: TIDLBaseObject): TIDLSetlikeDefinition; virtual;
    function ParseRecordTypeDef(aParent: TIDLBaseObject): TIDLRecordDefinition; virtual;
    function ParsePromiseTypeDef(aParent: TIDLBaseObject): TIDLPromiseTypeDefDefinition; virtual;
    function ParseSequenceTypeDef(aParent : TIDLBaseObject): TIDLSequenceTypeDefDefinition; virtual;
    function ParseUnionTypeDef(aParent : TIDLBaseObject): TIDLUnionTypeDefDefinition; virtual;
    function ParseConstValue(out aValue: UTF8String; aExtended: Boolean): TConstType; virtual;
    function ParseConst(aParent: TIDLBaseObject ): TIDLConstDefinition; virtual;
    function ParseCallBack(aParent : TIDLBaseObject): TIDLDefinition; virtual;
    function ParseStringifier(aParent : TIDLBaseObject): TIDLDefinition; virtual;
    function ParseOperation(aParent: TIDLBaseObject): TIDLFunctionDefinition; virtual;
    function ParseSerializer(aParent: TIDLBaseObject): TIDLSerializerDefinition; virtual;
    function ParseStatic(aParent: TIDLBaseObject): TIDLDefinition;virtual;
    function ParseIterable(aParent : TIDLBaseObject): TIDLIterableDefinition; virtual;
    function ParseInterface(aParent : TIDLBaseObject): TIDLInterfaceDefinition; virtual;
    function ParseDictionary(aParent : TIDLBaseObject; AllowInheritance : Boolean = True): TIDLDictionaryDefinition; virtual;
    function ParseEnum(aParent : TIDLBaseObject): TIDLEnumDefinition; virtual;
    function ParseTypeDef(aParent : TIDLBaseObject): TIDLTypeDefDefinition; virtual;
    function ParsePartial(aParent : TIDLBaseObject): TIDLStructuredDefinition; virtual;
    function ParseImplementsOrIncludes(aParent: TIDLBaseObject): TIDLImplementsOrIncludesDefinition; virtual;
    function ParseImplements(Const aName : UTF8String; aParent : TIDLBaseObject): TIDLImplementsDefinition; virtual;
    function ParseIncludes(Const aName : UTF8String; aParent : TIDLBaseObject): TIDLIncludesDefinition; virtual;
    function ParseDefinition(aParent : TIDLBaseObject): TIDLDefinition; virtual;
    procedure ParseDefinitions(aParent : TIDLBaseObject); virtual;
  Public
    Constructor Create(aContext : TWEBIDLContext; aScanner : TWebIDLScanner); overload;
    Constructor Create(aContext : TWEBIDLContext; aSource : UTF8String);overload;
    Destructor Destroy; override;
    Procedure Parse;
    Property Scanner : TWebIDLScanner Read FScanner;
    Property Context : TWebIDLContext Read FContext;
    Property Version : TWebIDLVersion Read FVersion Write SetVersion;
  end;

procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare); overload;
procedure MergeSort(List: TFPList; StartIndex, EndIndex: integer; const OnCompare: TListSortCompare); overload;

implementation

Resourcestring
  SErrInvalidToken = 'Invalid token: expected "%s", got: "%s"';
  SErrInvalidTokenList = 'Invalid token: expected one of "%s", got: "%s"';
  // SExpectedOther = 'Unexpected token in attribute list: "%s".';
  SErrUnExpectedToken = 'Unexpected token : "%s"';
  SErrTypeNotAllowed = 'Type "%s" not allowed in "%s" type.';
  SErrDictionaryNotFound = 'Dictionary %s not found';
  SErrInterfaceNotFound = 'Interface %s not found';
  SErrInterfaceNotFoundForAt = 'Included Interface %s not found for %s at %s';

procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare);
begin
  if List=nil then exit;
  MergeSort(List,0,List.Count-1,OnCompare);
end;

procedure MergeSort(List: TFPList; StartIndex, EndIndex: integer;
  const OnCompare: TListSortCompare);
// sort so that for each i is OnCompare(List[i],List[i+1])<=0
var
  MergeList: PPointer;

  procedure SmallSort(StartPos, EndPos: PtrInt);
  // use insertion sort for small lists
  var
    i: PtrInt;
    Best: PtrInt;
    j: PtrInt;
    Item: Pointer;
  begin
    for i:=StartPos to EndPos-1 do begin
      Best:=i;
      for j:=i+1 to EndPos do
        if OnCompare(List[Best],List[j])>0 then
          Best:=j;
      if Best>i then begin
        Item:=List[i];
        List[i]:=List[Best];
        List[Best]:=Item;
      end;
    end;
  end;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:PtrInt;
  begin
    while (Pos3>=Pos2) and (OnCompare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=OnCompare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      List[a]:=MergeList[a];
  end;

  procedure Sort(StartPos, EndPos: PtrInt);
  // sort an interval in List. Use MergeList as work space.
  var
    mid: integer;
  begin
    if EndPos-StartPos<6 then begin
      SmallSort(StartPos,EndPos);
    end else begin
      mid:=(StartPos+EndPos) shr 1;
      Sort(StartPos,mid);
      Sort(mid+1,EndPos);
      Merge(StartPos,mid+1,EndPos);
    end;
  end;

var
  Cnt: Integer;
begin
  if (List=nil) then exit;
  Cnt:=List.Count;
  if StartIndex<0 then StartIndex:=0;
  if EndIndex>=Cnt then EndIndex:=Cnt-1;
  if StartIndex>=EndIndex then exit;
  MergeList:=GetMem(List.Count*SizeOf(Pointer));
  Sort(StartIndex,EndIndex);
  Freemem(MergeList);
end;

{ TWebIDLParser }

constructor TWebIDLParser.Create(aContext: TWEBIDLContext; aScanner: TWebIDLScanner);
begin
  FScanner:=aScanner;
  FContext:=aContext;
end;

constructor TWebIDLParser.Create(aContext: TWEBIDLContext; aSource: UTF8String);
begin
  FOwnsScanner:=True;
  Create(aContext,TWebIDLScanner.Create(aSource));
end;

destructor TWebIDLParser.Destroy;
begin
  if FOwnsScanner then
    FreeAndNil(FScanner);
  inherited Destroy;
end;

function TWebIDLParser.CurrentToken: TIDLToken;

begin
  Result:=FScanner.CurToken;
end;

function TWebIDLParser.GetToken: TIDLToken;

begin
  Repeat
    Result:=FScanner.FetchToken;
  until Not (Result in [tkWhitespace,tkComment]);
end;

procedure TWebIDLParser.SetVersion(AValue: TWebIDLVersion);
begin
  if FVersion=AValue then Exit;
  FVersion:=AValue;
  if Assigned(Scanner) then
    FScanner.Version:=FVersion;
end;

function TWebIDLParser.GetErrorPos: String;

begin
  Result:='';
  If Assigned(FScanner) then
    Result:=Format('Error in IDL at line %d, pos %d: ',[FScanner.CurRow,FScanner.CurColumn]);
end;

procedure TWebIDLParser.Error(Msg: String);

begin
  Raise EWebIDLParser.Create(GetErrorPos+Msg);
end;

procedure TWebIDLParser.Error(Fmt: String; Args: array of const);
begin
  Raise EWebIDLParser.Create(GetErrorPos+Format(Fmt,Args));
end;

function TWebIDLParser.CurrentTokenString: UTF8String;

begin
  Result:=Fscanner.CurTokenString;
end;

function TWebIDLParser.CurrentRow: integer;
begin
  Result:=FScanner.CurRow;
end;

function TWebIDLParser.CurrentColumn: integer;
begin
  Result:=FScanner.CurColumn;
end;

function TWebIDLParser.CurrentFile: string;
begin
  Result:=FScanner.CurFile;
end;

procedure TWebIDLParser.CheckCurrentToken(aToken: TIDLToken);
begin
  if (aToken<>CurrentToken) then
    Error(SErrInvalidToken,[GetTokenName(aToken),CurrentTokenString]);
end;

procedure TWebIDLParser.CheckCurrentTokens(aTokens: TIDLTokens);
begin
  if Not (CurrentToken in aTokens) then
    Error('[20220725174524] '+SErrInvalidTokenList,[GetTokenNames(aTokens),CurrentTokenString]);
end;

function TWebIDLParser.ExpectToken(aToken: TIDLToken): TIDLToken;

begin
  Result:=GetToken;
  CheckCurrentToken(aToken);
end;

function TWebIDLParser.ExpectTokens(aTokens: TIDLTokens): TIDLToken;

begin
  Result:=GetToken;
  CheckCurrentTokens(aTokens);
end;

// We're at the [,{,( token when we enter here
// On exit, we're on the terminator token.
procedure TWebIDLParser.ParseExtAttributes(aList: TExtAttributeList; aTerminator: TIDLToken; ForSerializer : Boolean = False);

  Function AddSub(aTerm : TIDLToken) : String;

  Var
    L : TExtAttributeList;

  begin
    Result:=CurrentTokenString;
    L:=TExtAttributeList.Create;
    try
      ParseExtAttributes(L,aTerm,ForSerializer);
      Result:=Trim(Result+L.ToLine(',')+CurrentTokenString);
    finally
      L.Free;
    end;
  end;

  Procedure AddToCurrent(Var Current : UTF8String; Const aTerm : String);

  begin
    if (Current<>'') then
      Current:=Current+' ';
    Current:=Current+aTerm;
  end;

  Procedure AddToList(Var aTerm : UTF8String);

  begin
    ATerm:=Trim(ATerm);
    if (ATerm<>'') then
      begin
      AList.Add(aTerm);
      aTerm:='';
      end;
  end;

Const
  OtherTokens = [tkNumberInteger,tkNumberFloat,tkIdentifier,tkString, {tkOther, tkMinus,}tkNegInfinity,
                 tkDot,tkEllipsis,tkColon,tkSemicolon,tkLess,tkEqual,tkLarger,tkQuestionmark,tkStar,tkByteString,
                 tkDOMString,tkInfinity,tkNan,tkUSVString,tkAny,tkboolean,tkbyte,tkDouble,tkFalse,tkFloat,tkComma,
                 tkLong,tkNull,tkObject,tkOctet,tkOr,tkOptional,tkSequence,tkShort,tkTrue,tkUnsigned,tkVoid];

Var
  tk : TIDLToken;
  ValidTokens : TIDLTokens;
  S : UTF8String;
  WasSub : Boolean;

begin
  ValidTokens:=OtherTokens;
  if ForSerializer then
    ValidTokens:=ValidTokens + [tkInherit,tkGetter];
  tk:=GetToken;
  S:='';
  While Not (tk=aTerminator) do
    begin
    WasSub:=True;
    Case tk of
      tkEOF :
        CheckCurrentToken(aTerminator);
      tkSquaredBraceOpen:
        S:=S+AddSub(tkSquaredBraceClose);
      tkBracketOpen:
        S:=S+AddSub(tkBracketClose);
      tkCurlyBraceOpen :
        S:=S+AddSub(tkCurlyBraceClose);
      else
        WasSub:=False;
        // Check
        While (tk in ValidTokens) do
          begin
          AddToCurrent(S,CurrentTokenString);
          if tk=tkComma then
            AddToList(S);
          tk:=GetToken;
          end;
      end;
    if WasSub then
      tk:=GetToken;
    end;
  AddToList(S);
end;

function TWebIDLParser.AddDefinition(aParent: TIDLBaseObject;
  aClass: TIDLDefinitionClass; const AName: UTF8String): TIDLDefinition;
begin
  Result:=Context.Add(aParent,aClass,AName,CurrentFile,CurrentRow,CurrentColumn);
end;

function TWebIDLParser.ParseExtAttributes: TExtAttributeList;

var
  ok: Boolean;
begin
  Result:=TExtAttributeList.Create;
  ok:=false;
  try
    ParseExtAttributes(Result,tkSquaredBraceClose);
    ok:=true;
  finally
    if not ok then
      FreeandNil(Result);
  end;
end;

function TWebIDLParser.ParseArgument(aParent : TIDLBaseObject): TIDLArgumentDefinition;

(* On Entry, we're on the argument start
  on exit, on the token after the argument definition i.e. a comma or )  *)

var
  ok: Boolean;
begin
  Result:=TIDLArgumentDefinition(AddDefinition(aParent,TIDLArgumentDefinition,''));
  ok:=false;
  try
    if CurrentToken=tkOptional then
      begin
      Result.isOptional:=True;
      GetToken;
      end;
    if (CurrentToken=tkSquaredBraceOpen) then
      begin
      Result.Attributes:=ParseExtAttributes;
      GetToken;
      end;
    Result.ArgumentType:=ParseType(Result,False);
    if CurrentToken=tkEllipsis then
      begin
      Result.HasEllipsis:=True;
      GetToken;
      end;
    CheckCurrentTokens([tkIdentifier,tkOther,tkCallback,tkInterface]);
    Result.Name:=CurrentTokenString;
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseFunction(aParent : TIDLBaseObject): TIDLFunctionDefinition;

(* On Entry, we're on the function identifier, on exit, on the final ) *)


var
  ok: Boolean;
begin
  Result:=TIDLFunctionDefinition(AddDefinition(aParent,TIDLFunctionDefinition,CurrentTokenString));
  ok:=false;
  try
    ExpectToken(tkEqual);
    Result.ReturnType:=ParseType(Result,True,True);
    ParseArguments(Result.Arguments);
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseCallBack(aParent : TIDLBaseObject): TIDLDefinition;

var
  tk : TIDLToken;

begin
  tk:=GetToken;
  Case tk of
    tkInterface :
       begin
       Result:=ParseInterface(aParent);
       TIDLInterfaceDefinition(Result).IsCallBack:=True;
       end;
    tkIdentifier :
       begin
       Result:=ParseFunction(aParent);
       With TIDLFunctionDefinition(Result) do
         Options:=Options+[foCallBack];
       end;
  else
    Error('[20220725174529] '+SErrInvalidTokenList,[GetTokenNames([tkInterface,tkIdentifier]),CurrentTokenString]);
  end;
end;

procedure TWebIDLParser.ParseArguments(aParent: TIDLBaseObject);

Var
  A : TIDLArgumentDefinition;
  S : UTF8String;

begin
  CheckCurrentToken(tkBracketOpen);
  GetToken;
  While (CurrentToken<>tkBracketClose) do
    begin
    A:=ParseArgument(aParent);
    ExpectTokens([tkEqual,tkComma,tkBracketClose]);
    if (CurrentToken=tkEqual) then
      begin
      ParseConstValue(S,True);
      A.HasDefaultValue:=True;
      A.DefaultValue:=S;
      GetToken;
      end;
    if (CurrentToken=tkComma) then
      GetToken;
    end;
end;

function TWebIDLParser.ParseOperation(aParent: TIDLBaseObject): TIDLFunctionDefinition;
{ On entry, we're on the type definition or on one of getter,setter,deleter,legacycaller,
  on exit, we're on the final ) }

Const
  Specials = [tkGetter, tkSetter, tkDeleter, tkLegacyCaller, tkConstructor];
  OnlyGetter = [foGetter];
  OnlySetter = [foSetter];
  OnlyDeleter = [foDeleter];

Var
  Opts : TFunctionOptions;
  FO : TFunctionOption;
  ok: Boolean;

begin
  Opts:=[];
  While CurrentToken in Specials do
    begin
    Case CurrentToken of
      tkGetter : FO:=foGetter;
      tkSetter : FO:=foSetter;
      tkDeleter : FO:=foDeleter;
      tkLegacyCaller : FO:=foLegacyCaller;
      tkConstructor : fo:=foConstructor;
    end;
    Include(Opts,FO);
    GetToken;
    end;
  Result:=TIDLFunctionDefinition(AddDefinition(aParent,TIDLFunctionDefinition,''));
  ok:=false;
  try
    if (foConstructor in Opts) then
      Result.Name:='New'
    else
      begin
      Result.ReturnType:=ParseType(Result,False,True);
      case CurrentToken of
      tkIdentifier:
        begin
        Result.Name:=CurrentTokenString;
        GetToken;
        end;
      tkBracketOpen:
        if (Opts=OnlyGetter) or (Opts=OnlySetter) then
          // using default name getProperty/setProperty
        else if (Opts=OnlyDeleter) then
          // using default name
        else
          CheckCurrentToken(tkIdentifier);
      else
        CheckCurrentToken(tkIdentifier);
      end;
      end;
    ParseArguments(Result.Arguments);
    Result.Options:=Result.Options+Opts;
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseStringifier(aParent: TIDLBaseObject): TIDLDefinition;

(* On entry we're on stringifier, on exit, we're on the end of the definition, before ; *)

Var
  tk : TIDLToken;

begin
  tk:=GetToken;
  case tk of
  tkReadOnly,tkAttribute:
    begin
    Result:=ParseAttribute(aParent);
    With TIDLAttributeDefinition(Result) do
      Options:=Options+[aoStringifier];
    end;
  tkSemiColon:
    begin
    // stringifier;
    Result:=TIDLAttributeDefinition(AddDefinition(aParent,TIDLAttributeDefinition,''));
    With TIDLAttributeDefinition(Result) do
      Options:=Options+[aoStringifier];
    end;
  else
    begin
    Result:=ParseOperation(aParent);
    With TIDLFunctionDefinition(Result) do
      Options:=Options+[foStringifier];
    end;
  end;
end;

function TWebIDLParser.ParseIterable(aParent: TIDLBaseObject): TIDLIterableDefinition;

Var
  T1,T2 : TIDLTypeDefDefinition;
  ok: Boolean;

begin
  ExpectToken(tkLess);
  T1:=Nil;
  T2:=nil;
  Result:=TIDLIterableDefinition(AddDefinition(aParent,TIDLIterableDefinition,''));
  ok:=false;
  try
    T1:=ParseType(Result,True,True);
    if (CurrentToken=tkComma) then
      T2:=ParseType(Result,True,True);
    CheckCurrentToken(tkLarger);
    if T2=Nil then
      Result.ValueType:=T1
    else
      begin
      Result.ValueType:=T2;
      T2:=Nil;
      Result.KeyType:=T1;
      end;
    T1:=nil;
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.CompleteSimpleType(tk: TIDLToken; var S: UTF8String; out
  IsNull: Boolean): TIDLToken;

begin
  Result:=tk;
  IsNull:=false;
  S:='';
  if (Result=tkUnsigned) then
    begin
    S:=CurrentTokenString+' ';
    Result:=GetToken;
    end
  else if (Result=tkUnrestricted) then
    begin
    S:=CurrentTokenString+' ';
    Result:=GetToken;
    end;
  // long
  S:=S+CurrentTokenString;
  if (Result<>tkLong) then
    Result:=GetToken
  else
    begin
    Result:=GetToken;
    // Long long
    if Result=tkLong then
      begin
      S:=S+' '+CurrentTokenString;
      Result:=GetToken;
      end;
    end;
  if Result=tkQuestionmark then
    begin
    IsNull:=True;
    Result:=GetToken;
    end;
end;

function TWebIDLParser.ParseMapLikeMember(aParent: TIDLBaseObject): TIDLMaplikeDefinition;
var
  ok: Boolean;
begin
  Result:=TIDLMaplikeDefinition(AddDefinition(aParent,TIDLMaplikeDefinition,''));
  ok:=false;
  try
    Result.TypeName:='maplike';
    ExpectToken(tkLess);
    Result.KeyType:=ParseType(Result,True,true);
    CheckCurrentToken(tkComma);
    Result.ValueType:=ParseType(Result,True,true);
    CheckCurrentToken(tkLarger);
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseSetLikeMember(aParent: TIDLBaseObject): TIDLSetlikeDefinition;
(* On Entry we're on setlike. On exit, we're on the > token *)

var
  ok: Boolean;
begin
  Result:=TIDLSetlikeDefinition(AddDefinition(aParent,TIDLSetlikeDefinition,''));
  ok:=false;
  try
    ExpectToken(tkLess);
    Result.ElementType:=ParseType(Result);
    Result.ElementType.Parent:=Result;
    CheckCurrentToken(tkLarger);
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseRecordTypeDef(aParent: TIDLBaseObject): TIDLRecordDefinition;

var
  ok: Boolean;
begin
  Result:=TIDLRecordDefinition(AddDefinition(aParent,TIDLRecordDefinition,''));
  ok:=false;
  try
    Result.TypeName:='record';
    ExpectToken(tkLess);
    Result.KeyType:=ParseType(Result,True,true);
    CheckCurrentToken(tkComma);
    Result.ValueType:=ParseType(Result,True,true);
    CheckCurrentToken(tkLarger);
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseConstValue(out aValue: UTF8String;
  aExtended: Boolean): TConstType;

Const
  ValueTokens = [tkTrue,tkFalse,tkNumberFloat,tkNumberInteger,tkNull,tkInfinity,tkNegInfinity,tkNan];
  ExtendedTokens = [tkSquaredBraceOpen,tkString, tkCurlyBraceOpen];
  ExtendedValueTokens = ExtendedTokens + ValueTokens;
  AllowedTokens : Array[Boolean] of TIDLTokens = (ValueTokens,ExtendedValueTokens);

begin
  ExpectTokens(AllowedTokens[aExtended]);
  aValue:=CurrentTokenString;
  Case CurrentToken of
    tkTrue,tkFalse : Result:=ctBoolean;
    tkNumberFloat : Result:=ctFloat;
    tkNumberInteger : Result:=ctInteger;
    tkNull : Result:=ctNull;
    tkNan : Result:=ctNan;
    tkInfinity : Result:=ctInfinity;
    tkNegInfinity : Result:=ctNegInfinity;
    tkString :
      If aExtended then
        Result:=ctString
      else
        Error(SErrUnExpectedToken,[CurrentTokenString]);
    tkSquaredBraceOpen :
      If aExtended then
        begin
        ExpectToken(tkSquaredBraceClose);
        aValue:=AValue+CurrentTokenString;
        Result:=ctEmptyArray
        end
      else
        Error(SErrUnExpectedToken,[CurrentTokenString]);
    tkCurlyBraceOpen :
      If aExtended then
        begin
        ExpectToken(tkCurlyBraceClose);
        aValue:=AValue+CurrentTokenString;
        Result:=ctEmptyObject
        end
      else
        Error(SErrUnExpectedToken,[CurrentTokenString]);
  end;
end;

function TWebIDLParser.ParseConst(aParent : TIDLBaseObject): TIDLConstDefinition;

(*
  On Entry we're on const. On exit, we're before the ;
*)

Const
  PrefixTokens = [tkUnsigned,tkLong,tkUnrestricted];
  SingleTokens = [tkIdentifier,tkBoolean,tkByte,tkOctet,tkFloat,tkDouble,tkShort];
  TypeTokens = SingleTokens+PrefixTokens;

Var
  S : UTF8String;
  isNull , ok: Boolean;
  tk : TIDLToken;

begin
  Result:=Nil;
  isNull:=False;
  S:='';
  tk:=ExpectTokens(TypeTokens);
  // Unsigned
  Tk:=CompleteSimpleType(tk,S,IsNull);
  CheckCurrentToken(tkIdentifier);
  Result:=TIDLConstDefinition(AddDefinition(aParent,TIDLConstDefinition,CurrentTokenString));
  ok:=false;
  try
    Result.TypeName:=S;
    Result.AllowNull:=isNull;
    ExpectToken(tkEqual);
    Result.ConstType:=ParseConstValue(S,false);
    Result.Value:=S;
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

procedure TWebIDLParser.MaybeFree(Result: TIDLDefinition; aParent : TIDLBaseObject);

begin
  if (AParent=Nil)  then
    Result.Free
  else if (aParent is TIDLDefinitionList) and (Not TIDLDefinitionList(AParent).OwnsDefinitions) then
    Result.Free;
end;

function TWebIDLParser.ParseAttribute(aParent : TIDLBaseObject): TIDLAttributeDefinition;
(*
  On Entry we're on readonly, inherit or attribute.
  On Exit, we're on the last token of the attribute definition, the name
*)
Var
  Options : TAttributeOptions;
  ok: Boolean;

begin
  Options:=[];
  if CurrentToken=tkInherit then
    begin
    Include(Options,aoInherit);
    GetToken;
    end;
  if (CurrentToken=tkReadOnly) then
    begin
    Include(Options,aoReadOnly);
    GetToken;
    end;
  CheckCurrentToken(tkAttribute);
  Result:=TIDLAttributeDefinition(AddDefinition(aParent,TIDLAttributeDefinition,''));
  ok:=false;
  try
    Result.AttributeType:=ParseType(Result,True,True);
    CheckCurrentTokens([tkIdentifier,tkRequired]);
    Result.Name:=CurrentTokenString;
    Result.Options:=Options;
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseStatic(aParent : TIDLBaseObject): TIDLDefinition;

(* On Entry we're on static. On exit, we're on the end of the definition, before the ; *)

Var
  A : TIDLAttributeDefinition;
  F : TIDLFunctionDefinition;
  tk : TIDLToken;

begin
  tk:=GetToken;
  if (Tk in [tkReadonly,tkAttribute]) then
    begin
    A:=ParseAttribute(aParent);
    A.Options:=A.Options+[aoStatic];
    Result:=A;
    end
  else
    begin
    F:=ParseOperation(aParent);
    F.Options:=F.Options+[foStatic];
    Result:=F;
    end;
end;

function TWebIDLParser.ParseSerializer(aParent : TIDLBaseObject): TIDLSerializerDefinition;

Var
  tk : TIDLToken;
  ok: Boolean;

begin
  Result:=Nil;
  tk:=GetToken;
  if tk=tkSemiColon then
    exit;
  Result:=TIDLSerializerDefinition(AddDefinition(aParent,TIDLSerializerDefinition,''));
  ok:=false;
  try
    if tk<>tkEqual then
      begin
      Result.SerializerFunction:=ParseOperation(Result);
      Exit;
      end;
    ExpectTokens([tkSquaredBraceOpen,tkCurlyBraceOpen,tkIdentifier]);
    case CurrentToken of
      tkSquaredBraceOpen :
        begin
        ParseExtAttributes(Result.Identifiers,tkSquaredBraceClose,True);
        Result.Kind:=skArray;
        end;
      tkCurlyBraceOpen :
        begin
        ParseExtAttributes(Result.Identifiers,tkCurlyBraceClose,True);
        Result.Kind:=skObject;
        end;
      tkIdentifier :
        begin
        Result.Identifiers.Add(CurrentTokenString);
        Result.Kind:=skSingle;
        end;
      end;
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseInterface(aParent : TIDLBaseObject): TIDLInterfaceDefinition;
(*
  On Entry we're on interface. On exit, we're on the } character or the ; if it is an empty forward definition
*)

Var
  tk : TIDLToken;
  Attrs : TExtAttributeList;
  M : TIDLDefinition;
  isMixin,SemicolonSeen , ok: Boolean;

begin
  Attrs:=nil;
  ExpectTokens([tkMixin,tkIdentifier]);
  isMixin:=CurrentToken=tkMixin;
  if CurrentToken=tkMixin then
    ExpectToken(tkIdentifier);
  Result:=TIDLInterfaceDefinition(AddDefinition(aParent,TIDLInterfaceDefinition,CurrentTokenString));
  ok:=false;
  try
    Result.IsMixin:=IsMixin;
    tk:=GetToken;
    if tk=tkSemiColon then
      begin
      // empty interface
      Result.IsForward:=true;
      exit;
      end;
    if tk=tkColon then
      begin
      ExpectToken(tkIdentifier);
      Result.ParentName:=CurrentTokenString;
      tk:=GetToken;
      end;
    CheckCurrentToken(tkCurlyBraceOpen);
    tk:=GetToken;
    While (tk<>tkCurlyBraceClose) do
      begin
      SemicolonSeen:=False;
      Attrs:=nil;
      M:=Nil;
      if tk=tkSquaredBraceOpen then
        begin
        Attrs:=ParseExtAttributes;
        tk:=GetToken;
        end;
      Case tk of
        tkConst : M:=ParseConst(Result.Members);
        tkSetLike : M:=ParseSetLikeMember(Result.Members);
        tkMapLike : M:=ParseMapLikeMember(Result.Members);
        tkReadOnly :
          begin
          Case GetToken of
            tkAttribute,tkInherit:
              begin
              M:=ParseAttribute(Result.Members);
              With TIDLAttributeDefinition(M) do
                Options:=Options+[aoReadOnly];
              end;
            tkMapLike:
              begin
              M:=ParseMapLikeMember (Result.Members);
              TIDLMapLikeDefinition(M).IsReadonly:=True;
              end;
            tkSetLike:
              begin
              M:=ParseSetLikeMember (Result.Members);
              TIDLSetLikeDefinition(M).IsReadonly:=True;
              end
          else
            CheckCurrentTokens([tkAttribute,tkInherit,tkMapLike,tkSetLike]);
          end;
          end;
        tkInherit,
        tkAttribute : M:=ParseAttribute(Result.Members);
        tkStatic : M:=ParseStatic(Result.Members);
        tkSerializer :
          begin
          M:=ParseSerializer(Result.Members);
          Result.HasSerializer:=True;
          SemicolonSeen:=M=Nil;
          end;
        tkStringifier :
          begin
          M:=ParseStringifier(Result.Members);
          Result.HasStringifier:=true;
          if CurrentToken=tkSemiColon then
            SemicolonSeen:=true;
          end;
        tkIterable : ParseIterable(Result.Members);
      else
        {
        tkGetter, tkSetter, tkDeleter, tkLegacyCaller
        }
        M:=ParseOperation(Result.Members);
      end;
      IF Assigned(M) then
        begin
        M.Attributes:=Attrs;
        Attrs:=Nil; // So it does not get freed in except
        end;
      if not SemicolonSeen then
        GetToken;
      CheckCurrentToken(tkSemicolon);
      tk:=GetToken;
      end;
    ok:=true;
  finally
    if not ok then
      begin
      FreeAndNil(Attrs);
      MaybeFree(Result,aParent);
      end;
  end;
end;

function TWebIDLParser.ParsePartial(aParent : TIDLBaseObject): TIDLStructuredDefinition;

(* On entry, we're on Partial. On exit, we're on the } character *)

begin
  Case GetToken of
    tkInterface : Result:=ParseInterface(aParent);
    tkDictionary : Result:=ParseDictionary(aParent);
  else
    Error('[20220725174539] '+SErrInvalidTokenList,[GetTokenNames([tkInterface,tkDictionary]),CurrentTokenString]);
  end;
  Result.IsPartial:=True;
end;

function TWebIDLParser.ParseImplementsOrIncludes(aParent: TIDLBaseObject): TIDLImplementsOrIncludesDefinition;

Var
  aName : UTF8String;

begin
  aName:=CurrentTokenString;
  if version=v1 then
    begin
    ExpectToken(tkImplements);
    Result:=ParseImplements(aName,aParent)
    end
  else
    begin
    ExpectTokens([tkImplements,tkIncludes]);
    case CurrentToken of
     tkIncludes: Result:=ParseIncludes(aName,aParent);
     tkImplements: Result:=ParseImplements(aName,aParent);
    end;
    end;

end;

function TWebIDLParser.ParseEnum(aParent : TIDLBaseObject): TIDLEnumDefinition;
(* On entry, we're on enum. On exit, we're on the } character *)

Var
  tk : TIDLToken;

begin
  ExpectToken(tkIdentifier);
  Result:=TIDLEnumDefinition(AddDefinition(aParent,TIDLEnumDefinition,CurrentTokenString));
  ExpectToken(tkCurlyBraceOpen);
  Repeat
    tk:=ExpectTokens([tkCurlyBraceClose,tkString]);
    if tk=tkString then
      begin
      Result.AddValue(CurrentTokenString);
      tk:=ExpectTokens([tkCurlyBraceClose,tkComma]);
      end;
  Until (tk=tkCurlyBraceClose);
end;

function TWebIDLParser.ParseDictionaryMember(aParent : TIDLBaseObject): TIDLDictionaryMemberDefinition;

{ On Entry, we're at the start of the member. This may be required, attributes or the type.
  On Exit, we're on the ; }

Var
  Attrs : TExtAttributeList;
  tk : TIDLToken;
  isReq , ok: Boolean;
  S : UTF8String;

begin
  Attrs:=Nil;
  tk:=CurrentToken;
  isReq:=(tk=tkRequired);
  if IsReq then
    tk:=GetToken;
  if tk=tkSquaredBraceOpen then
    begin
    Attrs:=ParseExtAttributes;
    tk:=GetToken;
    isReq:=(tk=tkRequired);
    if IsReq then
      tk:=GetToken;
    end;
  Result:=TIDLDictionaryMemberDefinition(AddDefinition(aParent,TIDLDictionaryMemberDefinition,''));
  ok:=false;
  try
    Result.Attributes:=Attrs;
    Result.IsRequired:=isReq;
    Result.MemberType:=ParseType(Result,False,True);
    CheckCurrentToken(tkIdentifier);
    Result.Name:=CurrentTokenString;
    tk:=GetToken;
    if tk=tkEqual then
      begin
      Result.DefaultValue:=TIDLConstDefinition(AddDefinition(Result,TIDLConstDefinition,''));
      Result.DefaultValue.ConstType:=ParseConstValue(S,True);
      Result.DefaultValue.Value:=S;
      tk:=GetToken;
      end;
    CheckCurrentToken(tkSemicolon);
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseDictionary(aParent : TIDLBaseObject; AllowInheritance : Boolean = True): TIDLDictionaryDefinition;
(* On entry, we're on dictionary, on eexit, we're on { *)

Var
  Name,ParentName : UTF8String;
  tk : TIDLToken;

begin
  ExpectToken(tkIdentifier);
  Name:=CurrentTokenString;
  tk:=GetToken;
  if (tk=tkColon) then
    begin
    If Not AllowInheritance then
      Error(SErrUnExpectedToken,[CurrentTokenString]);
    ExpectToken(tkIdentifier);
    ParentName:=CurrentTokenString;
    tk:=GetToken;
    end;
  CheckCurrentToken(tkCurlyBraceOpen);
  Result:=TIDLDictionaryDefinition(AddDefinition(aParent,TIDLDictionaryDefinition,Name));
  Result.ParentName:=ParentName;
  GetToken;
  While (CurrentToken<>tkCurlyBraceClose) do
     begin
     ParseDictionaryMember(Result.Members);
     CheckCurrentTokens([tkSemicolon,tkCurlyBraceClose]);
     if (CurrentToken=tkSemicolon) then
       GetToken;
     end;
end;

function TWebIDLParser.ParseSequenceTypeDef(aParent : TIDLBaseObject): TIDLSequenceTypeDefDefinition;
(* On Entry we're on sequence. On exit, we're on the > token *)

var
  ok: Boolean;
begin
  Result:=TIDLSequenceTypeDefDefinition(AddDefinition(aParent,TIDLSequenceTypeDefDefinition,''));
  ok:=false;
  try
    Result.TypeName:='sequence';
    ExpectToken(tkLess);
    Result.ElementType:=ParseType(Result);
    Result.ElementType.Parent:=Result;
    CheckCurrentToken(tkLarger);
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseUnionTypeDef(aParent : TIDLBaseObject): TIDLUnionTypeDefDefinition;

(* On Entry we're on (. On exit, we're on the ) token *)

Var
  D : TIDLTypeDefDefinition;
  tk : TIDLToken;
  Attr : TExtAttributeList;
  ok: Boolean;

begin
  Attr:=Nil;
  Result:=TIDLUnionTypeDefDefinition(AddDefinition(aParent,TIDLUnionTypeDefDefinition,''));
  ok:=false;
  try
    Result.TypeName:='union';
    Repeat
      Attr:=Nil;
      tk:=GetToken;
      if Tk=tkSquaredBraceOpen then
        begin
        Attr:=ParseExtAttributes;
        tk:=GetToken;
        end;
      D:=ParseType(Result.Union,False);
      D.Attributes:=Attr;
      Attr:=Nil;
      if (D.TypeName='any') then
        Error(SErrTypeNotAllowed,['any','union']);
      CheckCurrentTokens([tkOr,tkBracketClose]);
      tk:=CurrentToken;
    until (tk=tkBracketClose);
    ok:=true;
  finally
    if not ok then
      begin
      FreeAndNil(Attr);
      MaybeFree(Result,aParent);
      end;
  end;
end;

function TWebIDLParser.ParsePromiseTypeDef(aParent: TIDLBaseObject): TIDLPromiseTypeDefDefinition;
(* On Entry we're on promise. On exit, we're on the > token *)

var
  ok: Boolean;
begin
  Result:=TIDLPromiseTypeDefDefinition(AddDefinition(aParent,TIDLPromiseTypeDefDefinition,''));
  ok:=false;
  try
    Result.TypeName:='Promise';
    ExpectToken(tkLess);
    Result.ReturnType:=ParseType(Result,True,true);
    CheckCurrentToken(tkLarger);
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseType(aParent : TIDLBaseObject; FetchFirst : Boolean = True; AllowExtraTypes : Boolean = False): TIDLTypeDefDefinition;

(* On Entry
   if FetchFirst = true we're on "typedef", "(", "or" or "<" tokens.
   if FetchFirst = true we're on the first actual token
   On exit, we're on the first token after the type

   *)

Const
  SimplePrefixTokens = [tkUnsigned,tkLong,tkUnrestricted];
  ComplexPrefixTokens = [tkSequence,tkPromise,tkBracketOpen,tkRecord,tkFrozenArray];
  PrefixTokens  = ComplexPrefixTokens+SimplePrefixTokens;
  PrimitiveTokens = [tkBoolean,tkByte,tkOctet,tkFloat,tkDouble,tkShort,tkAny,tkObject];
  IdentifierTokens = [tkIdentifier,tkByteString,tkUSVString,tkDOMString];
  SimpleTypeTokens = PrimitiveTokens+IdentifierTokens;
  TypeTokens = PrefixTokens+SimpleTypeTokens;
  ExtraTypeTokens = TypeTokens +[{tkStringToken,}tkVoid];
  EnforceRange = 'EnforceRange';
  LegacyDOMString = 'LegacyNullToEmptyString';

Var
  isNull , ok: Boolean;
  typeName: UTF8String;
  Allowed : TIDLTokens;
  tk : TIDLToken;

begin
  Result:=Nil;
  ok:=false;
  try
    isNull:=False;
    if FetchFirst then
      tk:=GetToken
    else
      tk:=CurrentToken;
    if tk=tkSquaredBraceOpen then
      begin
      ExpectToken(tkIdentifier);
      case CurrentTokenString of
      EnforceRange:
        begin
        // special: [EnforceRange] unsigned long
        ExpectToken(tkSquaredBraceClose);
        ExpectToken(tkunsigned);
        ExpectToken(tklong);
        Result:=TIDLTypeDefDefinition(AddDefinition(aParent,TIDLTypeDefDefinition,''));
        Result.TypeName:='unsigned long';
        Result.Attributes.Add(EnforceRange);
        end;
      LegacyDOMString:
        begin
        // special: [LegacyNullToEmptyString] DOMString
        ExpectToken(tkSquaredBraceClose);
        ExpectToken(tkDOMString);
        Result:=TIDLTypeDefDefinition(AddDefinition(aParent,TIDLTypeDefDefinition,''));
        Result.TypeName:='DOMString';
        Result.Attributes.Add(LegacyDOMString);
        end
      else
        Error(SErrInvalidToken,[LegacyDOMString,CurrentTokenString]);
      end;
      GetToken;
      ok:=true;
      exit;
      end;
    if AllowExtraTypes then
      Allowed:=ExtraTypeTokens
    else
      Allowed:=TypeTokens;
    CheckCurrentTokens(Allowed);
    TypeName:=CurrentTokenString;
    if (tk in SimplePrefixTokens) then
      begin
      tk:=CompleteSimpleType(tk,TypeName,isNull);
      Result:=TIDLTypeDefDefinition(AddDefinition(aParent,TIDLTypeDefDefinition,''));
      end
    else
      begin
      Case tk of
        tkRecord : Result:=ParseRecordTypeDef(aParent);
        tkFrozenArray,
        tkSequence : Result:=ParseSequenceTypeDef(aParent);
        tkPromise : Result:=ParsePromiseTypeDef(aParent);
        tkBracketOpen : Result:=ParseUnionTypeDef(aParent);
      else
        Result:=TIDLTypeDefDefinition(AddDefinition(aParent,TIDLTypeDefDefinition,''));
      end;
      tk:=GetToken;
      end;
    if Result.TypeName='' then
      Result.TypeName:=TypeName;
    // Null ?
    if tk=tkQuestionmark then
      begin
      tk:=GetToken;
      isNull:=True;
      end;
    Result.AllowNull:=isNull;
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseTypeDef(aParent : TIDLBaseObject): TIDLTypeDefDefinition;
(* On Entry we're on "typedef", "or" or "<" tokens. On exit, we're on the identifier *)


var
  ok: Boolean;
begin
  Result:=ParseType(aParent);
  ok:=false;
  try
    CheckCurrentToken(tkIdentifier);
    Result.Name:=CurrentTokenString;
    ok:=true;
  finally
    if not ok then
      MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseImplements(const aName: UTF8String;
  aParent: TIDLBaseObject): TIDLImplementsDefinition;
(* On entry, we're on the identifier for V1, we're. On Exit, we're on the last identifier *)

Var
  N : UTF8String;

begin
  if Version=V1 then
    begin
    N:=aName
    end
  else
    N:=aName;
  Result:=TIDLImplementsDefinition(AddDefinition(aParent,TIDLImplementsDefinition,N));
  try
    ExpectToken(tkIdentifier);
    Result.ImplementedInterface:=CurrentTokenString;
  except
    MaybeFree(Result,aParent);
  end;
end;

function TWebIDLParser.ParseIncludes(const aName: UTF8String;
  aParent: TIDLBaseObject): TIDLIncludesDefinition;

(* On entry, we're on the identifier. On Exit, we're on the last identifier *)

begin
  Result:=TIDLIncludesDefinition(AddDefinition(aParent,TIDLIncludesDefinition,aName));
  try
    ExpectToken(tkIdentifier);
    Result.IncludedInterface:=CurrentTokenString;
  except
    MaybeFree(Result,aParent);
  end;
end;


function TWebIDLParser.ParseDefinition(aParent : TIDLBaseObject): TIDLDefinition;

Var
  tk : TIDLToken;
  Attrs : TExtAttributeList;

begin
  Result:=Nil;
  Attrs:=Nil;
  tk:=GetToken;
  if tk=tkSquaredBraceOpen then
    begin
    Attrs:=ParseExtAttributes;
    tk:=GetToken;
    end;
  Try
    Case tk of
      tkCallback : Result:=ParseCallBack(aParent);
      tkInterface : Result:=ParseInterface(aParent);
      tkDictionary : Result:=ParseDictionary(aParent);
      tkPartial : Result:=ParsePartial(aParent);
      tkEnum : Result:=ParseEnum(aParent);
      tkTypeDef : Result:=ParseTypeDef(aParent);
      tkIdentifier :
        Result:=ParseImplementsOrIncludes(aParent);
      tkEOF : exit;
    else
      Error(SErrUnExpectedToken,[CurrentTokenString]);
    end;
    if Assigned(Result) then
      begin
      Result.Attributes:=Attrs;
      Attrs:=nil;
      end;
  finally
    FreeAndNil(Attrs);
  end;
  if CurrentToken=tkSemiColon then exit;
  ExpectToken(tkSemicolon);
end;

procedure TWebIDLParser.ParseDefinitions(aParent : TIDLBaseObject);

begin
  Repeat
    ParseDefinition(aParent);
  Until (CurrentToken=tkEOF)
end;

procedure TWebIDLParser.Parse;
begin
  ParseDefinitions(Context.Definitions);
end;

{ TWebIDLContext }

constructor TWebIDLContext.Create(OwnsDefinitions : Boolean = True);
begin
  FDefinitions:=TIDLDefinitionList.Create(Nil,OwnsDefinitions);
end;

destructor TWebIDLContext.Destroy;
begin
  FreeAndNil(FDefinitions);
  FreeAndNil(FHash);
  inherited Destroy;
end;

function TWebIDLContext.FindDictionary(aName: UTF8String
  ): TIDLDictionaryDefinition;

Var
  I : Integer;

begin
  I:=0;
  Result:=Nil;
  While (Result=Nil) and (I<FDefinitions.Count) do
    begin
    if (FDefinitions[i] is TIDLDictionaryDefinition) then
      begin
      Result:=TIDLDictionaryDefinition(FDefinitions[i]);
      if (Result.Name<>aName) or (Result.IsPartial) then
        Result:=nil;
      end;
    Inc(I);
    end;
end;
function TWebIDLContext.FindInterface(aName: UTF8String
  ): TIDLInterfaceDefinition;

Var
  I : Integer;

begin
  I:=0;
  Result:=Nil;
  While (Result=Nil) and (I<FDefinitions.Count) do
    begin
    if (FDefinitions[i] is TIDLInterfaceDefinition) then
      begin
      Result:=TIDLInterfaceDefinition(FDefinitions[i]);
      if (Result.Name<>aName) or (Result.IsPartial) then
        Result:=nil;
      end;
    Inc(I);
    end;
end;

procedure TWebIDLContext.AppendDictionaryPartials;

Var
  D : TIDLDefinition;
  DD : TIDLDictionaryDefinition absolute D;
  OD : TIDLDictionaryDefinition;

begin
  For D in FDefinitions do
    if (D is TIDLDictionaryDefinition) and (DD.IsPartial) then
      begin
      OD:=FindDictionary(DD.Name);
      If (OD=Nil) then
        Raise EWebIDLParser.CreateFmt(SErrDictionaryNotFound,[DD.Name]);
      OD.Partials.Add(DD);
      end;
end;

procedure TWebIDLContext.AppendInterfacePartials;

Var
  D : TIDLDefinition;
  ID : TIDLInterfaceDefinition absolute D;
  OD : TIDLInterfaceDefinition;

begin
  For D in FDefinitions do
    if (D is TIDLInterfaceDefinition) and (ID.IsPartial) then
      begin
      OD:=FindInterface(ID.Name);
      If (OD<>Nil) then
        OD.Partials.Add(ID);
      end;
end;

procedure TWebIDLContext.AppendInterfaceIncludes;
Var
  D : TIDLDefinition;
  ID : TIDLIncludesDefinition absolute D;
  II,OI : TIDLInterfaceDefinition; // Includes and original

begin
  For D in FDefinitions do
    if (D is TIDLIncludesDefinition)  then
      begin
      OI:=FindInterface(ID.Name);
      If (OI=Nil) then
        Raise EWebIDLParser.CreateFmt(SErrInterfaceNotFound,[ID.Name]);
      II:=FindInterface(ID.IncludedInterface);
      If (II=Nil) then
        begin
        if Assigned(Aliases) and (Aliases.IndexOfName(ID.IncludedInterface)<>-1) then
          OI.ParentName:=Aliases.Values[ID.IncludedInterface]
        else
          Raise EWebIDLParser.CreateFmt('[20220725182631] '+SErrInterfaceNotFoundForAt,[ID.IncludedInterface,ID.Name,GetDefPos(ID)]);
       end
      else
        begin
        II.IsInclude:=True;
        OI.Partials.Add(II);
        end
      end;
  // if there is a single include, no members and no parent, make it a descendent
  For D in FDefinitions do
    if (D is TIDLInterfaceDefinition)  then
      begin
      OI:=D as TIDLInterfaceDefinition;
      if (OI.ParentName='') and (OI.Partials.Count=1) then
        if (OI.Partial[0] is TIDLInterfaceDefinition) then
          begin
          II:=OI.Partial[0] as TIDLInterfaceDefinition;
          if II.IsInclude then
            begin
            // DoLog('Converting single include %s to parent class for %s',[II.Name,OI.Name]);
            OI.ParentName:=II.Name;
            OI.ParentInterface:=II;
            OI.Partials.Clear;
            end;
          end;
      end;
end;

procedure TWebIDLContext.AppendPartials;

begin
  AppendDictionaryPartials;
  AppendInterfacePartials;
end;

procedure TWebIDLContext.AppendIncludes;
begin
  AppendInterfaceIncludes;
end;

type
  TTopologicalIntf = class
    Intf: TIDLInterfaceDefinition;
    Parent: TIDLInterfaceDefinition;
    Level: integer;
    SrcPos: integer;
  end;

function CompareTopologicalIntfWithLevelAndSrcPos(Data1, Data2: Pointer): integer;
var
  A: TTopologicalIntf absolute Data1;
  B: TTopologicalIntf absolute Data2;
begin
  if A.Level<B.Level then
    Result:=-1
  else if A.Level>B.Level then
    Result:=1
  else if A.SrcPos<B.SrcPos then
    Result:=-1
  else if A.SrcPos>B.SrcPos then
    Result:=1
  else
    Result:=0;
end;

function TWebIDLContext.GetInterfacesTopologically: TIDLDefinitionList;
var
  List: TFPList; // list of TTopologicalIntf

  function FindIntf(Intf: TIDLInterfaceDefinition): TTopologicalIntf;
  var
    i: Integer;
  begin
    for i:=0 to List.Count-1 do
      if TTopologicalIntf(List[i]).Intf=Intf then
        exit(TTopologicalIntf(List[i]));
    Result:=nil;
  end;

  function FindParent(Top: TTopologicalIntf): TIDLInterfaceDefinition;
  var
    ParentIntf, IntfDef: TIDLInterfaceDefinition;
    Def: TIDLDefinition;
  begin
    IntfDef:=Top.Intf;
    if (Top.Parent=nil) and (IntfDef.ParentName<>'') then
      begin
      ParentIntf:=IntfDef.ParentInterface;
      if ParentIntf<>nil then
        Top.Parent:=ParentIntf
      else
        begin
        Def:=FindDefinition(IntfDef.ParentName);
        if Def is TIDLInterfaceDefinition then
          Top.Parent:=TIDLInterfaceDefinition(Def)
        else if Def=nil then
          begin
          raise EConvertError.Create('[20220725182112] interface "'+IntfDef.Name+'" at '+GetDefPos(IntfDef)+', parent "'+IntfDef.ParentName+'" not found');
          end
        else
          raise EConvertError.Create('[20220725182109] [TWebIDLContext.GetInterfacesTopologically] interface "'+IntfDef.Name+'" at '+GetDefPos(IntfDef)+', parent "'+IntfDef.ParentName+'" is not an interface at '+GetDefPos(Def));
        end;
      end;
    Result:=Top.Parent;
  end;

  function GetTopologicalLevel(Top: TTopologicalIntf): integer;
  var
    ParentTop: TTopologicalIntf;
    IntfDef: TIDLInterfaceDefinition;
  begin
    IntfDef:=Top.Intf;
    if Top.Level<0 then
      begin
      if Top.Parent=nil then
        Top.Level:=0
      else
        begin
        ParentTop:=FindIntf(Top.Parent);
        if ParentTop=nil then
          begin
          writeln('Warning: [20220725182101] [TWebIDLContext.GetInterfacesTopologically] interface "'+IntfDef.Name+'" at '+GetDefPos(IntfDef)+', parent "'+Top.Parent.Name+'" at '+GetDefPos(Top.Parent)+' not in definition list');
          Top.Level:=0;
          end
        else
          Top.Level:=GetTopologicalLevel(ParentTop)+1;
        end;
      end;
    Result:=Top.Level;
  end;

var
  D: TIDLDefinition;
  IntfDef: TIDLInterfaceDefinition;
  Top: TTopologicalIntf;
  i: Integer;
begin
  Result:=nil;
  List:=TFPList.Create;
  try
    // collect all interfaces
    for D in Definitions do
      if D is TIDLInterfaceDefinition then
        begin
        IntfDef:=TIDLInterfaceDefinition(D);
        if IntfDef.IsPartial then continue;
        Top:=TTopologicalIntf.Create;
        Top.Intf:=IntfDef;
        Top.Level:=-1;
        Top.SrcPos:=List.Count;
        List.Add(Top);
        end;
    // set parent interfaces
    for i:=0 to List.Count-1 do
      FindParent(TTopologicalIntf(List[i]));

    // sort topologically (keeping source order)
    for i:=0 to List.Count-1 do
      GetTopologicalLevel(TTopologicalIntf(List[i]));
    MergeSort(List,@CompareTopologicalIntfWithLevelAndSrcPos);

    Result:=TIDLDefinitionList.Create(nil,false);
    for i:=0 to List.Count-1 do
      begin
      Top:=TTopologicalIntf(List[i]);
      Result.Add(Top.Intf);
      end;
  finally
    List.Free;
  end;
end;

procedure TWebIDLContext.ResolveParentTypes;

Var
  D : TIDLDefinition;
  ID : TIDLInterfaceDefinition absolute D;
  DD : TIDLDictionaryDefinition absolute D;

begin
  For D in FDefinitions do
    if D is TIDLInterfaceDefinition then
      begin
      if (ID.ParentName<>'') then
        ID.ParentInterface:=FindInterface(ID.ParentName);
      end
    else if D is TIDLDictionaryDefinition then
      if (DD.ParentName<>'') then
        DD.ParentDictionary:=FindDictionary(DD.ParentName);
end;

procedure TWebIDLContext.ResolveTypes;
begin
  ResolveParentTypes;
end;

function TWebIDLContext.GetDefPos(Def: TIDLBaseObject; WithoutFile: boolean
  ): string;
begin
  Result:='('+IntToStr(Def.Line)+','+IntToStr(Def.Column)+')';
  if not WithoutFile then
    Result:=Def.SrcFile+Result;
end;

function TWebIDLContext.IndexOfDefinition(const AName: String): Integer;
begin
  Result:=Definitions.Count-1;
  While (Result>=0) and (Definitions[Result].Name<>AName) do
    Dec(Result);
end;


function TWebIDLContext.FindDefinition(const AName: String): TIDLDefinition;

Var
  D : TIDLDefinition;

begin
  if (FHash=Nil) then
    begin
    FHash:=TFPObjectHashTable.Create(False);
    For D in Definitions do
      if not D.IsExtension then
        FHash.Add(D.Name,D);
    end;
  Result:=TIDLDefinition(FHash.Items[AName]);
end;

function TWebIDLContext.AsString(Full: Boolean): UTF8String;
begin
  Result:=Definitions.AsString(';'+sLineBreak,'','','',Full,True);
end;

function TWebIDLContext.Add(aClass: TIDLDefinitionClass;
  const AName: UTF8String; const aFile: string; aLine, aCol: integer
  ): TIDLDefinition;
begin
  Result:=Add(FDefinitions,aClass,AName,aFile,aLine,aCol);
end;

function TWebIDLContext.Add(aParent: TIDLBaseObject;
  aClass: TIDLDefinitionClass; const AName: UTF8String; const aFile: string;
  aLine, aCol: integer): TIDLDefinition;
begin
  if Assigned(aParent) then
    Result:=aParent.Add(aClass,aName,aFile,aLine,aCol)
  else
    Result:=aClass.Create(Nil,aName,aFile,aLine,aCol);
end;

end.

