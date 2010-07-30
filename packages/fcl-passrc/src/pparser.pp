{
    This file is part of the Free Component Library

    Pascal source parser
    Copyright (c) 2000-2005 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

unit PParser;

interface

uses SysUtils, PasTree, PScanner;

resourcestring
  SErrNoSourceGiven = 'No source file specified';
  SErrMultipleSourceFiles = 'Please specify only one source file';
  SParserError = 'Error';
  SParserErrorAtToken = '%s at token "%s"';
  SParserUngetTokenError = 'Internal error: Cannot unget more tokens, history buffer is full';
  SParserExpectTokenError = 'Expected "%s"';
  SParserExpectedCommaRBracket = 'Expected "," or ")"';
  SParserExpectedCommaSemicolon = 'Expected "," or ";"';
  SParserExpectedCommaColon = 'Expected "," or ":"';
  SParserExpectedLBracketColon = 'Expected "(" or ":"';
  SParserExpectedLBracketSemicolon = 'Expected "(" or ";"';
  SParserExpectedColonSemicolon = 'Expected ":" or ";"';
  SParserExpectedSemiColonEnd = 'Expected ";" or "End"';
  SParserExpectedConstVarID = 'Expected "const", "var" or identifier';
  SParserExpectedColonID = 'Expected ":" or identifier';
  SParserSyntaxError = 'Syntax error';
  SParserTypeSyntaxError = 'Syntax error in type';
  SParserArrayTypeSyntaxError = 'Syntax error in array type';
  SParserInterfaceTokenError = 'Invalid token in interface section of unit';
  SParserImplementationTokenError = 'Invalid token in implementation section of unit';
  SParserInvalidTypeDef = 'Invalid type definition';
  SParserExpectedIdentifier = 'Identifier expected';

type
  TPasTreeContainer = class
  protected
    FPackage: TPasPackage;
    FInterfaceOnly : Boolean;
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; const ASourceFilename: String;
      ASourceLinenumber: Integer): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;overload;
      virtual; abstract;
    function CreateFunctionType(const AName, AResultName: String; AParent: TPasElement;
      UseParentAsResultParent: Boolean; const ASourceFilename: String;
      ASourceLinenumber: Integer): TPasFunctionType;
    function FindElement(const AName: String): TPasElement; virtual; abstract;
    function FindModule(const AName: String): TPasModule; virtual;
    property Package: TPasPackage read FPackage;
    property InterfaceOnly : Boolean Read FInterfaceOnly Write FInterFaceOnly;
  end;

  EParserError = class(Exception)
  private
    FFilename: String;
    FRow, FColumn: Integer;
  public
    constructor Create(const AReason, AFilename: String;
      ARow, AColumn: Integer);
    property Filename: String read FFilename;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
  end;

function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: String): TPasModule;


implementation

uses Classes;

var
  IsIdentStart: array[char] of boolean;

type

  TDeclType = (declNone, declConst, declResourcestring, declType, declVar, declThreadvar, declProperty);

  TProcType = (ptProcedure, ptFunction, ptOperator, ptConstructor, ptDestructor,
               ptClassProcedure, ptClassFunction);

  { TPasParser }

  TPasParser = class
  private
    FFileResolver: TFileResolver;
    FScanner: TPascalScanner;
    FEngine: TPasTreeContainer;
    FCurToken: TToken;
    FCurTokenString: String;
    // UngetToken support:
    FTokenBuffer: array[0..1] of TToken;
    FTokenStringBuffer: array[0..1] of String;
    FTokenBufferIndex: Integer; // current index in FTokenBuffer
    FTokenBufferSize: Integer; // maximum valid index in FTokenBuffer
    procedure ParseExc(const Msg: String);
  protected
    function OpLevel(t: TToken): Integer;
    Function TokenToExprOp (AToken : TToken) : TExprOpCode;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;overload;
    Function IsHint(Const S : String; var AHint : TPasMemberHint) : Boolean;
    Function CheckHint(Element : TPasElement; ExpectSemiColon : Boolean) : TPasMemberHints;

    function ParseParams(paramskind: TPasExprKind): TParamsExpr;
    function ParseExpIdent: TPasExpr;
  public
    Options : set of TPOptions;
    CurModule: TPasModule;
    constructor Create(AScanner: TPascalScanner; AFileResolver: TFileResolver;
      AEngine: TPasTreeContainer);
    function CurTokenName: String;
    function CurTokenText: String;
    procedure NextToken; // read next non whitespace, non space
    procedure UngetToken;
    procedure ExpectToken(tk: TToken);
    function ExpectIdentifier: String;

    function ParseType(Parent: TPasElement; Prefix : String): TPasType;overload;
    function ParseType(Parent: TPasElement): TPasType;overload;
    function ParseComplexType(Parent : TPasElement = Nil): TPasType;
    procedure ParseArrayType(Element: TPasArrayType);
    procedure ParseFileType(Element: TPasFileType);
    function DoParseExpression: TPasExpr;
    function ParseExpression: String;
    function ParseCommand: String; // single, not compound command like begin..end
    procedure AddProcOrFunction(Declarations: TPasDeclarations; AProc: TPasProcedure);
    function CheckIfOverloaded(AOwner: TPasClassType;
      const AName: String): TPasElement;

    procedure ParseMain(var Module: TPasModule);
    procedure ParseUnit(var Module: TPasModule);
    procedure ParseInterface;
    procedure ParseImplementation;
    procedure ParseInitialization;
    procedure ParseFinalization;
    procedure ParseDeclarations(Declarations: TPasDeclarations);
    procedure ParseUsesList(ASection: TPasSection);
    function ParseConstDecl(Parent: TPasElement): TPasConst;
    function ParseResourcestringDecl(Parent: TPasElement): TPasResString;
    function ParseTypeDecl(Parent: TPasElement): TPasType;
    procedure ParseInlineVarDecl(Parent: TPasElement; VarList: TList);overload;
    procedure ParseInlineVarDecl(Parent: TPasElement; VarList: TList;
      AVisibility : TPasMemberVisibility; ClosingBrace: Boolean);overload;
    procedure ParseVarDecl(Parent: TPasElement; List: TList);
    procedure ParseArgList(Parent: TPasElement; Args: TList; EndToken: TToken);
    procedure ParseProcedureOrFunctionHeader(Parent: TPasElement;
      Element: TPasProcedureType; ProcType: TProcType; OfObjectPossible: Boolean);
    procedure ParseProcedureBody(Parent: TPasElement);
    function ParseProcedureOrFunctionDecl(Parent: TPasElement;
      ProcType: TProcType): TPasProcedure;
    procedure ParseRecordDecl(Parent: TPasRecordType; IsNested: Boolean);   // !!!: Optimize this. We have 3x the same wrapper code around it.
    function ParseClassDecl(Parent: TPasElement; const AClassName: String;
      AObjKind: TPasObjKind): TPasType;
    procedure ParseProperty(Element:TPasElement);
    procedure ParseProcBeginBlock(Parent: TProcedureBody);
    procedure ParseStatement(Parent: TPasImplBlock;
                             out NewImplElement: TPasImplElement);

    property FileResolver: TFileResolver read FFileResolver;
    property Scanner: TPascalScanner read FScanner;
    property Engine: TPasTreeContainer read FEngine;

    property CurToken: TToken read FCurToken;
    property CurTokenString: String read FCurTokenString;
  end;

function TPasTreeContainer.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; const ASourceFilename: String;
  ASourceLinenumber: Integer): TPasElement;
begin
  Result := CreateElement(AClass, AName, AParent, visDefault, ASourceFilename,
    ASourceLinenumber);
end;

function TPasTreeContainer.CreateFunctionType(const AName, AResultName: String;
  AParent: TPasElement; UseParentAsResultParent: Boolean;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasFunctionType;
var
  ResultParent: TPasElement;
begin
  Result := TPasFunctionType(CreateElement(TPasFunctionType, AName, AParent,
    ASourceFilename, ASourceLinenumber));

  if UseParentAsResultParent then
    ResultParent := AParent
  else
    ResultParent := Result;

  TPasFunctionType(Result).ResultEl :=
    TPasResultElement(CreateElement(TPasResultElement, AResultName, ResultParent,
    ASourceFilename, ASourceLinenumber));
end;

function TPasTreeContainer.FindModule(const AName: String): TPasModule;
begin
  Result := nil;
end;

constructor EParserError.Create(const AReason, AFilename: String;
  ARow, AColumn: Integer);
begin
  inherited Create(AReason);
  FFilename := AFilename;
  FRow := ARow;
  FColumn := AColumn;
end;

procedure TPasParser.ParseExc(const Msg: String);
begin
  raise EParserError.Create(Format(SParserErrorAtToken, [Msg, CurTokenName]),
    Scanner.CurFilename, Scanner.CurRow, Scanner.CurColumn);
end;

constructor TPasParser.Create(AScanner: TPascalScanner;
  AFileResolver: TFileResolver; AEngine: TPasTreeContainer);
begin
  inherited Create;
  FScanner := AScanner;
  FFileResolver := AFileResolver;
  FEngine := AEngine;
end;

function TPasParser.CurTokenName: String;
begin
  if CurToken = tkIdentifier then
    Result := 'Identifier ' + FCurTokenString
  else
    Result := TokenInfos[CurToken];
end;

function TPasParser.CurTokenText: String;
begin
  case CurToken of
    tkIdentifier, tkString, tkNumber, tkChar:
      Result := FCurTokenString;
    else
      Result := TokenInfos[CurToken];
  end;
end;

procedure TPasParser.NextToken;
begin
  if FTokenBufferIndex < FTokenBufferSize then
  begin
    // Get token from buffer
    FCurToken := FTokenBuffer[FTokenBufferIndex];
    FCurTokenString := FTokenStringBuffer[FTokenBufferIndex];
    Inc(FTokenBufferIndex);
    //writeln('TPasParser.NextToken From Buf ',CurTokenText,' id=',FTokenBufferIndex);
  end else
  begin
    { We have to fetch a new token. But first check, wether there is space left
      in the token buffer.}
    if FTokenBufferSize = 2 then
    begin
      FTokenBuffer[0] := FTokenBuffer[1];
      FTokenStringBuffer[0] := FTokenStringBuffer[1];
      Dec(FTokenBufferSize);
      Dec(FTokenBufferIndex);
    end;
    // Fetch new token
    try
      repeat
        FCurToken := Scanner.FetchToken;
      until not (FCurToken in [tkWhitespace, tkComment]);
    except
      on e: EScannerError do
        raise EParserError.Create(e.Message,
          Scanner.CurFilename, Scanner.CurRow, Scanner.CurColumn);
    end;
    FCurTokenString := Scanner.CurTokenString;
    FTokenBuffer[FTokenBufferSize] := FCurToken;
    FTokenStringBuffer[FTokenBufferSize] := FCurTokenString;
    Inc(FTokenBufferSize);
    Inc(FTokenBufferIndex);
    //writeln('TPasParser.NextToken New ',CurTokenText,' id=',FTokenBufferIndex);
  end;
end;

procedure TPasParser.UngetToken;

begin
  if FTokenBufferIndex = 0 then
    ParseExc(SParserUngetTokenError)
  else begin
    Dec(FTokenBufferIndex);
    if FTokenBufferIndex>0 then
    begin
      FCurToken := FTokenBuffer[FTokenBufferIndex-1];
      FCurTokenString := FTokenStringBuffer[FTokenBufferIndex-1];
    end else begin
      FCurToken := tkWhitespace;
      FCurTokenString := '';
    end;
    //writeln('TPasParser.UngetToken ',CurTokenText,' id=',FTokenBufferIndex);
  end;
end;


procedure TPasParser.ExpectToken(tk: TToken);
begin
  NextToken;
  if CurToken <> tk then
    ParseExc(Format(SParserExpectTokenError, [TokenInfos[tk]]));
end;

function TPasParser.ExpectIdentifier: String;
begin
  ExpectToken(tkIdentifier);
  Result := CurTokenString;
end;

function TPasParser.ParseType(Parent: TPasElement): TPasType;

begin
  Result:=ParseType(Parent,'');
end;

Function TPasParser.IsHint(Const S : String; var AHint : TPasMemberHint) : Boolean;

Var
  T : string;

begin
  T:=LowerCase(S);
  Result:=(T='deprecated');
  If Result then
    Ahint:=hDeprecated
  else
    begin
    Result:=(T='library');
    if Result then
      Ahint:=hLibrary
    else
      begin
      Result:=(T='platform');
      If result then
        AHint:=hPlatform;
      end;
    end;  
end;

Function TPasParser.CheckHint(Element : TPasElement; ExpectSemiColon : Boolean) : TPasMemberHints;

Var
  Found : Boolean;
  h : TPasMemberHint;
  
begin
  Result:=[];
  Repeat
    NextToken;
    Found:=IsHint(CurTokenString,h);
    If Found then
      Include(Result,h)
  Until Not Found;
  UnGetToken;
  If Assigned(Element) then
    Element.Hints:=Result;
  if ExpectSemiColon then
    ExpectToken(tkSemiColon);
end;

function TPasParser.ParseType(Parent: TPasElement; Prefix : String): TPasType;

  procedure ParseRange;
  begin
    Result := TPasRangeType(CreateElement(TPasRangeType, '', Parent));
    try
      TPasRangeType(Result).RangeStart := ParseExpression;
      ExpectToken(tkDotDot);
      TPasRangeType(Result).RangeEnd := ParseExpression;
    except
      Result.Free;
      raise;
    end;
  end;

var
  Name, s: String;
  EnumValue: TPasEnumValue;
  Ref: TPasElement;
  HadPackedModifier : Boolean;           // 12/04/04 - Dave - Added
  IsBitPacked : Boolean;

begin
  Result := nil;         // !!!: Remove in the future
  HadPackedModifier := False;     { Assume not present }
  NextToken;
  if CurToken in [tkPacked,tkbitpacked] then     { If PACKED modifier }
     begin                        { Handle PACKED modifier for all situations }
     IsBitPacked:=(CurToken=tkBitPacked);
     NextToken;                   { Move to next token for rest of parse }
     if CurToken in [tkArray, tkRecord, tkObject, tkClass] then  { If allowed }
       HadPackedModifier := True  { rememeber for later }
     else                         { otherwise, syntax error }
       ParseExc(Format(SParserExpectTokenError,['ARRAY, RECORD, OBJECT or CLASS']))
     end;
  case CurToken of
    tkIdentifier:
      begin
        Name := CurTokenString;
        If (Prefix<>'') then
          Name:=Prefix+'.'+Name;
        NextToken;
        if CurToken = tkDot then
        begin
          ExpectIdentifier;
          Name := Name+'.'+CurTokenString;
        end else
          UngetToken;
        Ref := nil;
        s := UpperCase(Name);
        if s = 'BYTE' then Name := 'Byte'
        else if s = 'BOOLEAN' then Name := 'Boolean'
        else if s = 'CHAR' then Name := 'Char'
        else if s = 'INTEGER' then Name := 'Integer'
        else if s = 'INT64' then Name := 'Int64'
        else if s = 'LONGINT' then Name := 'LongInt'
        else if s = 'LONGWORD' then Name := 'LongWord'
        else if s = 'SHORTINT' then Name := 'ShortInt'
        else if s = 'SMALLINT' then Name := 'SmallInt'
        else if s = 'STRING' then Name := 'String'
        else if s = 'WORD' then Name := 'Word'
        else
          Ref := Engine.FindElement(Name);
        if Assigned(Ref) then
        begin
          {Result := TPasTypeRef(CreateElement(TPasTypeRef, Name, nil));
          TPasTypeRef(Result).RefType := Ref as TPasType;}
          Result := Ref as TPasType;
          Result.AddRef;
        end else
          Result := TPasUnresolvedTypeRef(CreateElement(TPasUnresolvedTypeRef, Name, nil));

        // !!!: Doesn't make sense for resolved types
        if Name = 'String' then
        begin
          NextToken;
          if CurToken = tkSquaredBraceOpen then
          begin
            // !!!: Parse the string length value and store it
            repeat
                  NextToken;
            until CurToken = tkSquaredBraceClose;
          end else
            UngetToken;
        end;
      end;
    tkCaret:
      begin
        Result := TPasPointerType(CreateElement(TPasPointerType, '', Parent));
        TPasPointerType(Result).DestType := ParseType(nil);
      end;
    tkFile:
      begin
        Result := TPasFileType(CreateElement(TPasFileType, '', Parent));
      end;
    tkArray:
      begin
        Result := TPasArrayType(CreateElement(TPasArrayType, '', Parent));
        TPasArrayType(Result).IsPacked := HadPackedModifier;
        ParseArrayType(TPasArrayType(Result));
      end;
    tkBraceOpen:
      begin
        Result := TPasEnumType(CreateElement(TPasEnumType, '', Parent));
        while True do
        begin
          NextToken;
          EnumValue := TPasEnumValue(CreateElement(TPasEnumValue,
            CurTokenString, Result));
          TPasEnumType(Result).Values.Add(EnumValue);
          NextToken;
          if CurToken = tkBraceClose then
            break
          else if CurToken in [tkEqual,tkAssign] then
            begin
            EnumValue.AssignedValue:=ParseExpression;
            NextToken;
            if CurToken = tkBraceClose then
              Break
            else if not (CurToken=tkComma) then
              ParseExc(SParserExpectedCommaRBracket);
            end
          else if not (CurToken=tkComma) then
            ParseExc(SParserExpectedCommaRBracket)
        end;
      end;
    tkSet:
      begin
        Result := TPasSetType(CreateElement(TPasSetType, '', Parent));
    try
          ExpectToken(tkOf);
          TPasSetType(Result).EnumType := ParseType(Result);
    except
      Result.Free;
      raise;
    end;
      end;
    tkRecord:
      begin
        Result := TPasRecordType(CreateElement(TPasRecordType, '', Parent));
        TPasRecordType(Result).IsPacked:=HadPackedModifier;
        If HadPackedModifier then
            TPasRecordType(Result).IsBitPacked:=IsBitPacked;
    try
          ParseRecordDecl(TPasRecordType(Result), False);
    except
      Result.Free;
      raise;
    end;
      end;
    tkProcedure:
      begin
        Result := TPasProcedureType(
          CreateElement(TPasProcedureType, '', Parent));
        try
          ParseProcedureOrFunctionHeader(Result,
            TPasProcedureType(Result), ptProcedure, True);
        except
          Result.Free;
          raise;
        end;
      end;
    tkFunction:
      begin
        Result := Engine.CreateFunctionType('', 'Result', Parent, False,
          Scanner.CurFilename, Scanner.CurRow);
        try
          ParseProcedureOrFunctionHeader(Result,
            TPasFunctionType(Result), ptFunction, True);
        except
          Result.Free;
          raise;
        end;
      end;
    else
    begin
      UngetToken;
      ParseRange;
    end;
//      ParseExc(SParserTypeSyntaxError);
  end;
end;

function TPasParser.ParseComplexType(Parent : TPasElement = Nil): TPasType;
begin
  NextToken;
  case CurToken of
    tkProcedure:
      begin
        Result := TPasProcedureType(CreateElement(TPasProcedureType, '', Parent));
        ParseProcedureOrFunctionHeader(Result,
          TPasProcedureType(Result), ptProcedure, True);
        UngetToken;        // Unget semicolon
      end;
    tkFunction:
      begin
        Result := Engine.CreateFunctionType('', 'Result', Parent, False,
      Scanner.CurFilename, Scanner.CurRow);
        ParseProcedureOrFunctionHeader(Result,
          TPasFunctionType(Result), ptFunction, True);
        UngetToken;        // Unget semicolon
      end;
    else
    begin
      UngetToken;
      Result := ParseType(Parent);
      exit;
    end;
  end;
end;

procedure TPasParser.ParseArrayType(Element: TPasArrayType);

Var
  S : String;

begin
  NextToken;
  S:='';
  case CurToken of
    tkSquaredBraceOpen:
      begin
        repeat
          NextToken;
          if CurToken<>tkSquaredBraceClose then
            S:=S+CurTokenText;
        until CurToken = tkSquaredBraceClose;
      Element.IndexRange:=S;
        ExpectToken(tkOf);
        Element.ElType := ParseType(nil);
      end;
    tkOf:
      begin
        NextToken;
        if CurToken = tkConst then
//          ArrayEl.AppendChild(Doc.CreateElement('const'))
        else
        begin
          UngetToken;
            Element.ElType := ParseType(nil);
        end
      end
    else
      ParseExc(SParserArrayTypeSyntaxError);
  end;
end;

procedure TPasParser.ParseFileType(Element: TPasFileType);


begin
  NextToken;
  If CurToken=tkOf then
    Element.ElType := ParseType(nil);
end;

const
  EndExprToken = [
    tkEOF, tkBraceClose, tkSquaredBraceClose, tkSemicolon, tkComma,
    tkdo, tkdownto, tkelse, tkend, tkof, tkthen, tkto
  ];


function TPasParser.ParseParams(paramskind: TPasExprKind): TParamsExpr;
var
  params  : TParamsExpr;
  p       : TPasExpr;
  PClose  : TToken;
begin
  Result:=nil;
  if paramskind in [pekArrayParams, pekSet] then begin
    if CurToken<>tkSquaredBraceOpen then Exit;
    PClose:=tkSquaredBraceClose;
  end else begin
    if CurToken<>tkBraceOpen then Exit;
    PClose:=tkBraceClose;
  end;

  params:=TParamsExpr.Create(paramskind);
  try
    NextToken;
    if not (CurToken in EndExprToken) then begin
      repeat
        p:=DoParseExpression;
        if not Assigned(p) then Exit; // bad param syntax
        params.AddParam(p);

        if not (CurToken in [tkComma, PClose]) then begin
          Exit;
        end;

        if CurToken = tkComma then begin
          NextToken;
          if CurToken = PClose then begin
            //ErrorExpected(parser, 'identifier');
            Exit;
          end;
        end;
      until CurToken=PClose;
    end;
    NextToken;
    Result:=params;
  finally
    if not Assigned(Result) then params.Free;
  end;
end;

Function TPasParser.TokenToExprOp (AToken : TToken) : TExprOpCode;

begin
  Case AToken of
    tkMul                   : Result:=eopMultiply;
    tkPlus                  : Result:=eopAdd;
    tkMinus                 : Result:=eopSubtract;
    tkDivision              : Result:=eopDivide;
    tkLessThan              : Result:=eopLessThan;
    tkEqual                 : Result:=eopEqual;
    tkGreaterThan           : Result:=eopGreaterThan;
    tkAt                    : Result:=eopAddress;
    tkNotEqual              : Result:=eopNotEqual;
    tkLessEqualThan         : Result:=eopLessthanEqual;
    tkGreaterEqualThan      : Result:=eopGreaterThanEqual;
    tkPower                 : Result:=eopPower;
    tkSymmetricalDifference : Result:=eopSymmetricalDifference;                                                                                              
    tkIs                    : Result:=eopIs;
    tkAs                    : Result:=eopAs;
    tkSHR                   : Result:=eopSHR;
    tkSHL                   : Result:=eopSHL;
    tkAnd                   : Result:=eopAnd;
    tkOr                    : Result:=eopOR;
    tkXor                   : Result:=eopXOR;
    tkMod                   : Result:=eopMod;
    tkDiv                   : Result:=eopDiv;
    tkNot                   : Result:=eopNot;
    tkIn                    : Result:=eopIn;
  else
    ParseExc(format('Not an operand: (%d : %s)',[AToken,TokenInfos[AToken]]));
  end;
end;
 
function TPasParser.ParseExpIdent:TPasExpr;
var
  x       : TPasExpr;
  prm     : TParamsExpr;
  u       : TUnaryExpr;
  b       : TBinaryExpr;
begin
  Result:=nil;
  case CurToken of
    tkString:           x:=TPrimitiveExpr.Create(pekString, CurTokenString);
    tkChar:             x:=TPrimitiveExpr.Create(pekString, CurTokenText);
    tkNumber:           x:=TPrimitiveExpr.Create(pekNumber, CurTokenString);
    tkIdentifier:       x:=TPrimitiveExpr.Create(pekIdent, CurTokenText);
    tkSquaredBraceOpen: x:=ParseParams(pekSet);
  else
    ParseExc(SParserExpectedIdentifier);
  end;

  if x.Kind<>pekSet then NextToken;

  try
    if x.Kind=pekIdent then begin
      while CurToken in [tkBraceOpen, tkSquaredBraceOpen, tkCaret] do
        case CurToken of
          tkBraceOpen: begin
            prm:=ParseParams(pekFuncParams);
            if not Assigned(prm) then Exit;
            prm.Value:=x;
            x:=prm;
          end;
          tkSquaredBraceOpen: begin
            prm:=ParseParams(pekArrayParams);
            if not Assigned(prm) then Exit;
            prm.Value:=x;
            x:=prm;
          end;
          tkCaret: begin
            u:=TUnaryExpr.Create(x, TokenToExprOp(CurToken));
            x:=u;
            NextToken;
          end;
        end;

      if CurToken in [tkDot, tkas] then begin
        NextToken;
        b:=TBinaryExpr.Create(x, ParseExpIdent, TokenToExprOp(CurToken));
        if not Assigned(b.right) then Exit; // error
        x:=b;
      end;
    end;

    if CurToken = tkDotDot then begin
      NextToken;
      b:=TBinaryExpr.CreateRange(x, DoParseExpression);
      if not Assigned(b.right) then Exit; // error
      x:=b;
    end;

    Result:=x;
  finally
    if not Assigned(Result) then x.Free;
  end;
end;

function TPasParser.OpLevel(t: TToken): Integer;
begin
  case t of
    tknot,tkAt:
      Result:=4;
    tkMul, tkDivision, tkdiv, tkmod, tkand, tkShl,tkShr, tkas, tkPower :
      Result:=3;
    tkPlus, tkMinus, tkor, tkxor:
      Result:=2;
    tkEqual, tkNotEqual, tkLessThan, tkLessEqualThan, tkGreaterThan, tkGreaterEqualThan, tkin, tkis:
      Result:=1;
  else
    Result:=0;
  end;
end;

function TPasParser.DoParseExpression: TPasExpr;
var
  expstack  : TList;
  opstack   : TList;
  pcount    : Integer;
  x         : TPasExpr;
  i         : Integer;
  tempop    : TToken;
  AllowEnd  : Boolean;
  
const
  PrefixSym = [tkPlus, tkMinus, tknot, tkAt]; // + - not @

  function PopExp: TPasExpr; inline;
  begin
    if expstack.Count>0 then begin
      Result:=TPasExpr(expstack[expstack.Count-1]);
      expstack.Delete(expstack.Count-1);
    end else
      Result:=nil;
  end;

  procedure PushOper(token: TToken); inline;
  begin
    opstack.Add( Pointer(PtrInt(token)) );
  end;

  function PeekOper: TToken; inline;
  begin
    if opstack.Count>0 then Result:=TToken(PtrUInt(opstack[ opstack.Count-1]))
    else Result:=tkEOF
  end;

  function PopOper: TToken; inline;
  begin
    Result:=PeekOper;
    if Result<>tkEOF then opstack.Delete(opstack.Count-1);
  end;

  procedure PopAndPushOperator;
  var
    t       : TToken;
    xright  : TPasExpr;
    xleft   : TPasExpr;
  begin
    t:=PopOper;
    xright:=PopExp;
    xleft:=PopExp;
    expstack.Add(TBinaryExpr.Create(xleft, xright, TokenToExprOp(t)));
  end;

begin
  Result:=nil;
  expstack := TList.Create;
  opstack := TList.Create;
  try
    repeat
      AllowEnd:=True;
      pcount:=0;
      while CurToken in PrefixSym do begin
        PushOper(CurToken);
        inc(pcount);
        NextToken;
      end;

      if CurToken = tkBraceOpen then begin
        NextToken;
        x:=DoParseExpression();
        if CurToken<>tkBraceClose then Exit;
        NextToken;
      end else begin
        x:=ParseExpIdent;
      end;

      if not Assigned(x) then Exit;
      expstack.Add(x);
      for i:=1 to pcount do
        begin
        tempop:=PopOper;
        expstack.Add( TUnaryExpr.Create( PopExp, TokenToExprOp(tempop) ));
        end;
      if not (CurToken in EndExprToken) then begin
        // Adjusting order of the operations
        AllowEnd:=False;
        tempop:=PeekOper;
        while (opstack.Count>0) and (OpLevel(tempop)>=OpLevel(CurToken)) do begin
          PopAndPushOperator;
          tempop:=PeekOper;
        end;
        PushOper(CurToken);
        NextToken;
      end;

    until AllowEnd and (CurToken in EndExprToken);

    while opstack.Count>0 do PopAndPushOperator;

    // only 1 expression should be on the stack, at the end of the correct expression
    if expstack.Count=1 then Result:=TPasExpr(expstack[0]);

  finally
    if not Assigned(Result) then begin
      // expression error!
      for i:=0 to expstack.Count-1 do
        TObject(expstack[i]).Free;
    end;
    opstack.Free;
    expstack.Free;
  end;
end;

function TPasParser.ParseExpression: String;
var
  BracketLevel: Integer;
  LastTokenWasWord: Boolean;
begin
  SetLength(Result, 0);
  BracketLevel := 0;
  LastTokenWasWord := false;
  while True do
  begin
    NextToken;
    { !!!: Does not detect when normal brackets and square brackets are mixed
      in a wrong way. }
    if CurToken in [tkBraceOpen, tkSquaredBraceOpen] then
      Inc(BracketLevel)
    else if CurToken in [tkBraceClose, tkSquaredBraceClose] then
    begin
      if BracketLevel = 0 then
        break;
      Dec(BracketLevel);
    end else if (BracketLevel = 0) and (CurToken in [tkComma, tkSemicolon,
      tkColon, tkDotDot, tkthen, tkend, tkelse, tkuntil, tkfinally, tkexcept,
      tkof, tkbegin, tkdo, tkto, tkdownto, tkinitialization, tkfinalization])
    then
      break;

    if (CurTokenString<>'') and IsIdentStart[CurTokenString[1]] then
      begin
      if LastTokenWasWord then
        Result := Result + ' ';
      LastTokenWasWord:=true;
      end
    else
      LastTokenWasWord:=false;
    if CurToken=tkString then
      begin
      If (Length(CurTokenText)>0) and (CurTokenText[1]=#0) then
        Raise Exception.Create('First char is null : "'+CurTokenText+'"');
      Result := Result + ''''+StringReplace(CurTokenText,'''','''''',[rfReplaceAll])+''''
      end
    else
      Result := Result + CurTokenText;
  end;
  if Result='' then
    ParseExc(SParserSyntaxError);
  UngetToken;
end;

function TPasParser.ParseCommand: String;
var
  BracketLevel: Integer;
  LastTokenWasWord: Boolean;
begin
  SetLength(Result, 0);
  BracketLevel := 0;
  LastTokenWasWord := false;
  while True do
  begin
    NextToken;
    { !!!: Does not detect when normal brackets and square brackets are mixed
      in a wrong way. }
    if CurToken in [tkBraceOpen, tkSquaredBraceOpen] then
      Inc(BracketLevel)
    else if CurToken in [tkBraceClose, tkSquaredBraceClose] then
    begin
      if BracketLevel = 0 then
        break;
      Dec(BracketLevel);
    end else if (BracketLevel = 0) and (CurToken in [tkComma, tkSemicolon,
      tkColon, tkthen, tkend, tkelse, tkuntil, tkfinally, tkexcept, tkof, tkdo,
      tkbegin, tkinitialization, tkfinalization]) then
      break;

    if (CurTokenString<>'') and IsIdentStart[CurTokenString[1]] then
      begin
      if LastTokenWasWord then
        Result := Result + ' ';
      LastTokenWasWord:=true;
      end
    else
      LastTokenWasWord:=false;
    if CurToken=tkString then
      begin
      If (Length(CurTokenText)>0) and (CurTokenText[1]=#0) then
        Raise Exception.Create('First char is null : "'+CurTokenText+'"');
      Result := Result + ''''+StringReplace(CurTokenText,'''','''''',[rfReplaceAll])+''''
      end
    else
      Result := Result + CurTokenText;
  end;
  UngetToken;
end;

procedure TPasParser.AddProcOrFunction(Declarations: TPasDeclarations;
  AProc: TPasProcedure);
var
  i: Integer;
  Member: TPasElement;
  OverloadedProc: TPasOverloadedProc;
begin
  for i := 0 to Declarations.Functions.Count - 1 do
  begin
    Member := TPasElement(Declarations.Functions[i]);
    if CompareText(Member.Name, AProc.Name) = 0 then
    begin
      if Member.ClassType = TPasOverloadedProc then
        TPasOverloadedProc(Member).Overloads.Add(AProc)
      else
      begin
        OverloadedProc := TPasOverloadedProc.Create(AProc.Name, Declarations);
        OverloadedProc.Overloads.Add(Member);
        OverloadedProc.Overloads.Add(AProc);
        Declarations.Functions[i] := OverloadedProc;
        Declarations.Declarations[Declarations.Declarations.IndexOf(Member)] :=
          OverloadedProc;
      end;
      exit;
    end;
  end;

  // Not overloaded, so just add the proc/function to the lists
  Declarations.Declarations.Add(AProc);
  Declarations.Functions.Add(AProc);
end;


// Returns the parent for an element which is to be created
function TPasParser.CheckIfOverloaded(AOwner: TPasClassType;
  const AName: String): TPasElement;
var
  i: Integer;
  Member: TPasElement;
begin
  for i := 0 to AOwner.Members.Count - 1 do
  begin
    Member := TPasElement(AOwner.Members[i]);
    if CompareText(Member.Name, AName) = 0 then
    begin
      if Member.ClassType = TPasOverloadedProc then
        Result := Member
      else
      begin
        Result := TPasOverloadedProc.Create(AName, AOwner);
        Result.Visibility := Member.Visibility;
        TPasOverloadedProc(Result).Overloads.Add(Member);
        AOwner.Members[i] := Result;
      end;
      exit;
    end;
  end;
  Result := AOwner;
end;


procedure TPasParser.ParseMain(var Module: TPasModule);
begin
  Module:=nil;
  NextToken;
  case CurToken of
    tkUnit: ParseUnit(Module);
    else
      ParseExc(Format(SParserExpectTokenError, ['unit']));
  end;
end;

// Starts after the "unit" token
procedure TPasParser.ParseUnit(var Module: TPasModule);
begin
  Module := nil;
  Module := TPasModule(CreateElement(TPasModule, ExpectIdentifier,
    Engine.Package));
  CurModule:=Module;
  try
    if Assigned(Engine.Package) then
    begin
      Module.PackageName := Engine.Package.Name;
      Engine.Package.Modules.Add(Module);
    end;
    ExpectToken(tkSemicolon);
    ExpectToken(tkInterface);
    ParseInterface;
  finally
    CurModule:=nil;
  end;
end;

// Starts after the "interface" token
procedure TPasParser.ParseInterface;
var
  Section: TInterfaceSection;
begin
  Section := TInterfaceSection(CreateElement(TInterfaceSection, '', CurModule));
  CurModule.InterfaceSection := Section;
  ParseDeclarations(Section);
end;

// Starts after the "implementation" token
procedure TPasParser.ParseImplementation;
var
  Section: TImplementationSection;
begin
  Section := TImplementationSection(CreateElement(TImplementationSection, '', CurModule));
  CurModule.ImplementationSection := Section;
  ParseDeclarations(Section);
end;

procedure TPasParser.ParseInitialization;
var
  Section: TInitializationSection;
  SubBlock: TPasImplElement;
begin
  Section := TInitializationSection(CreateElement(TInitializationSection, '', CurModule));
  CurModule.InitializationSection := Section;
  repeat
    NextToken;
    if (CurToken=tkend) then
    begin
      ExpectToken(tkDot);
      exit;
    end
    else if (CurToken=tkfinalization) then
    begin
      ParseFinalization;
      exit;
    end
    else if CurToken<>tkSemiColon then
    begin
      UngetToken;
      ParseStatement(Section,SubBlock);
      if SubBlock=nil then
        ExpectToken(tkend);
    end;
  until false;
end;

procedure TPasParser.ParseFinalization;
var
  Section: TFinalizationSection;
  SubBlock: TPasImplElement;
begin
  Section := TFinalizationSection(CreateElement(TFinalizationSection, '', CurModule));
  CurModule.FinalizationSection := Section;
  repeat
    NextToken;
    if (CurToken=tkend) then
    begin
      ExpectToken(tkDot);
      exit;
    end
    else if CurToken<>tkSemiColon then
    begin
      UngetToken;
      ParseStatement(Section,SubBlock);
      if SubBlock=nil then
        ExpectToken(tkend);
    end;
  until false;
  UngetToken;
end;

procedure TPasParser.ParseDeclarations(Declarations: TPasDeclarations);
var
  CurBlock: TDeclType;
  ConstEl: TPasConst;
  ResStrEl: TPasResString;
  TypeEl: TPasType;
  ClassEl: TPasClassType;
  List: TList;
  i,j: Integer;
  VarEl: TPasVariable;
  PropEl : TPasProperty;
begin
  CurBlock := declNone;
  while True do
  begin
    NextToken;
    //writeln('TPasParser.ParseSection Token=',Scanner.CurTokenString,' ',CurToken);
    case CurToken of
      tkend:
        begin
        ExpectToken(tkDot);
        break;
        end;
      tkimplementation:
        if (CurToken = tkImplementation) and (Declarations is TInterfaceSection) then
          begin
          If Not Engine.InterfaceOnly then
            ParseImplementation;
          break;
          end;
      tkinitialization:
        if (Declarations is TInterfaceSection)
        or (Declarations is TImplementationSection) then
          begin
          ParseInitialization;
          break;
          end;
      tkfinalization:
        if (Declarations is TInterfaceSection)
        or (Declarations is TImplementationSection) then
          begin
          ParseFinalization;
          break;
          end;
      tkUses:
        if Declarations is TPasSection then
          ParseUsesList(TPasSection(Declarations))
        else
          ParseExc(SParserSyntaxError);
      tkConst:
        CurBlock := declConst;
      tkResourcestring:
        CurBlock := declResourcestring;
      tkType:
        CurBlock := declType;
      tkVar:
        CurBlock := declVar;
      tkThreadVar:
        CurBlock := declThreadVar;
      tkProperty:
        CurBlock := declProperty;
      tkProcedure:
        begin
          AddProcOrFunction(Declarations,
                       ParseProcedureOrFunctionDecl(Declarations, ptProcedure));
          CurBlock := declNone;
        end;
      tkFunction:
        begin
          AddProcOrFunction(Declarations,
                        ParseProcedureOrFunctionDecl(Declarations, ptFunction));
          CurBlock := declNone;
        end;
      tkConstructor:
        begin
          AddProcOrFunction(Declarations,
                     ParseProcedureOrFunctionDecl(Declarations, ptConstructor));
          CurBlock := declNone;
        end;
      tkDestructor:
        begin
          AddProcOrFunction(Declarations,
                      ParseProcedureOrFunctionDecl(Declarations, ptDestructor));
          CurBlock := declNone;
        end;
      tkOperator:
        begin
          AddProcOrFunction(Declarations,
                        ParseProcedureOrFunctionDecl(Declarations, ptOperator));
          CurBlock := declNone;
        end;
      tkClass:
        begin
          NextToken;
          case CurToken of
            tkprocedure:
              begin
                AddProcOrFunction(Declarations,
                  ParseProcedureOrFunctionDecl(Declarations, ptClassProcedure));
                CurBlock := declNone;
              end;
            tkfunction:
              begin
                AddProcOrFunction(Declarations,
                  ParseProcedureOrFunctionDecl(Declarations, ptClassFunction));
                CurBlock := declNone;
              end;
          else
            ExpectToken(tkprocedure);
          end;
        end;
      tkIdentifier:
        begin
          case CurBlock of
            declConst:
              begin
                ConstEl := ParseConstDecl(Declarations);
                Declarations.Declarations.Add(ConstEl);
                Declarations.Consts.Add(ConstEl);
              end;
            declResourcestring:
              begin
                ResStrEl := ParseResourcestringDecl(Declarations);
                Declarations.Declarations.Add(ResStrEl);
                Declarations.ResStrings.Add(ResStrEl);
              end;
            declType:
              begin
                TypeEl := ParseTypeDecl(Declarations);
                if Assigned(TypeEl) then        // !!!
                begin
                  Declarations.Declarations.Add(TypeEl);
                  if TypeEl.ClassType = TPasClassType then
                  begin
                    // Remove previous forward declarations, if necessary
                    for i := 0 to Declarations.Classes.Count - 1 do
                    begin
                      ClassEl := TPasClassType(Declarations.Classes[i]);
                      if CompareText(ClassEl.Name, TypeEl.Name) = 0 then
                      begin
                        Declarations.Classes.Delete(i);
                        for j := 0 to Declarations.Declarations.Count - 1 do
                          if CompareText(TypeEl.Name,
                            TPasElement(Declarations.Declarations[j]).Name) = 0 then
                          begin
                            Declarations.Declarations.Delete(j);
                            break;
                          end;
                        ClassEl.Release;
                        break;
                      end;
                    end;
                    // Add the new class to the class list
                    Declarations.Classes.Add(TypeEl)
                  end else
                    Declarations.Types.Add(TypeEl);
                end;
              end;
            declVar, declThreadVar:
              begin
                List := TList.Create;
                try
                  try
                    ParseVarDecl(Declarations, List);
                  except
                    for i := 0 to List.Count - 1 do
                      TPasVariable(List[i]).Release;
                    raise;
                  end;
                  for i := 0 to List.Count - 1 do
                  begin
                    VarEl := TPasVariable(List[i]);
                    Declarations.Declarations.Add(VarEl);
                    Declarations.Variables.Add(VarEl);
                  end;
                finally
                  List.Free;
                end;
              end;
            declProperty:
              begin
              PropEl:=TPasProperty(CreateElement(TPasProperty, CurTokenString, Declarations));
              Try
                ParseProperty(PropEl)
              except
                Propel.Free;
                Raise;
              end;
              Declarations.Declarations.Add(PropEl);
              Declarations.properties.add(PropEl);
              end;
          else
            ParseExc(SParserSyntaxError);
          end;
        end;
      tkbegin:
        begin
        if Declarations is TProcedureBody then
          begin
          ParseProcBeginBlock(TProcedureBody(Declarations));
          break;
          end
        else if (Declarations is TInterfaceSection)
        or (Declarations is TImplementationSection) then
          begin
          ParseInitialization;
          break;
          end
        else
          ParseExc(SParserSyntaxError);
        end
    else
      ParseExc(SParserSyntaxError);
    end;
  end;
end;

// Starts after the "uses" token
procedure TPasParser.ParseUsesList(ASection: TPasSection);
var
  AUnitName: String;
  Element: TPasElement;
begin
  while True do
  begin
    AUnitName := ExpectIdentifier;

    Element := Engine.FindModule(AUnitName);
    if Assigned(Element) then
      Element.AddRef
    else
      Element := TPasType(CreateElement(TPasUnresolvedTypeRef, AUnitName,
        ASection));
    ASection.UsesList.Add(Element);

    NextToken;

    if CurToken = tkin then begin
      // todo: store unit's file name somewhere
      NextToken; // skip in
      ExpectToken(tkString); // skip unit's real file name
    end;

    if CurToken = tkSemicolon then
      break
    else if CurToken <> tkComma then
      ParseExc(SParserExpectedCommaSemicolon);
  end;
end;

// Starts after the variable name
function TPasParser.ParseConstDecl(Parent: TPasElement): TPasConst;
begin
  Result := TPasConst(CreateElement(TPasConst, CurTokenString, Parent));
  try
    NextToken;
    if CurToken = tkColon then
      Result.VarType := ParseType(nil)
    else
      UngetToken;

    ExpectToken(tkEqual);

    //skipping the expression as a value
    //Result.Value := ParseExpression;

    // using new expression parser!
    NextToken; // skip tkEqual
    Result.Expr:=DoParseExpression;

    // must unget for the check to be peformed fine!
    UngetToken;

    CheckHint(Result,True);
  except
    Result.Free;
    raise;
  end;
end;

// Starts after the variable name
function TPasParser.ParseResourcestringDecl(Parent: TPasElement): TPasResString;
begin
  Result := TPasResString(CreateElement(TPasResString, CurTokenString, Parent));
  try
    ExpectToken(tkEqual);
    Result.Value := ParseExpression;
    CheckHint(Result,True);
  except
    Result.Free;
    raise;
  end;
end;

// Starts after the type name
function TPasParser.ParseTypeDecl(Parent: TPasElement): TPasType;
var
  TypeName: String;

  procedure ParseRange;
  begin
    Result := TPasRangeType(CreateElement(TPasRangeType, TypeName, Parent));
    try
      TPasRangeType(Result).RangeStart := ParseExpression;
      ExpectToken(tkDotDot);
      TPasRangeType(Result).RangeEnd := ParseExpression;
      CheckHint(Result,True);
    except
      Result.Free;
      raise;
    end;
  end;

var
  EnumValue: TPasEnumValue;
  Prefix : String;
  HadPackedModifier : Boolean;           // 12/04/04 - Dave - Added
  IsBitPacked : Boolean;
  H : TPasMemberHint;
  
begin
  TypeName := CurTokenString;
  ExpectToken(tkEqual);
  NextToken;
  HadPackedModifier := False;     { Assume not present }
  if CurToken in [tkPacked,tkbitpacked] then     { If PACKED modifier }
     begin                        { Handle PACKED modifier for all situations }
     IsBitPacked:=CurToken=tkbitpacked;
     NextToken;                   { Move to next token for rest of parse }
     if CurToken in [tkArray, tkRecord, tkObject, tkClass] then  { If allowed }
       HadPackedModifier := True  { rememeber for later }
     else                         { otherwise, syntax error }
       ParseExc(Format(SParserExpectTokenError,['ARRAY, RECORD, OBJECT or CLASS']))
     end;
  case CurToken of
    tkRecord:
      begin
        Result := TPasRecordType(CreateElement(TPasRecordType, TypeName,
          Parent));
        try
          ParseRecordDecl(TPasRecordType(Result), False);
      CheckHint(Result,True);
          TPasRecordType(Result).IsPacked := HadPackedModifier;
          If HadPackedModifier then
            TPasRecordType(Result).IsBitPacked:=IsBitPacked;
        except
          Result.Free;
          raise;
        end;
      end;
    tkObject:
      begin
        Result := ParseClassDecl(Parent, TypeName, okObject);
        TPasClassType(Result).IsPacked := HadPackedModifier;
      end;
    tkClass:
      begin
        Result := ParseClassDecl(Parent, TypeName, okClass);
        { could be TPasClassOfType }
        if result is TPasClassType then
          TPasClassType(Result).IsPacked := HadPackedModifier;
      end;
    tkInterface:
      Result := ParseClassDecl(Parent, TypeName, okInterface);
    tkCaret:
      begin
        Result := TPasPointerType(CreateElement(TPasPointerType, TypeName,
          Parent));
        try
          TPasPointerType(Result).DestType := ParseType(nil);
          CheckHint(Result,True);
        except
          Result.Free;
          raise;
        end;
      end;
    tkIdentifier:
      begin
        Prefix:=CurTokenString;
        NextToken;
        if CurToken = tkDot then
          begin
          ExpectIdentifier;
          NextToken;
          end
        else
          Prefix:='';
        if (CurToken = tkSemicolon) or IsHint(CurtokenString,h)then
        begin
          UngetToken;
          UngetToken;
          Result := TPasAliasType(CreateElement(TPasAliasType, TypeName,
            Parent));
          try
            TPasAliasType(Result).DestType := ParseType(nil,Prefix);
            CheckHint(Result,True);
          except
            Result.Free;
            raise;
          end;
        end else if CurToken = tkSquaredBraceOpen then
        begin
          // !!!: Check for string type and store string length somewhere
          Result := TPasAliasType(CreateElement(TPasAliasType, TypeName,
            Parent));
          try
            TPasAliasType(Result).DestType :=
              TPasUnresolvedTypeRef.Create(CurTokenString, Parent);
            ParseExpression;
            ExpectToken(tkSquaredBraceClose);
            CheckHint(Result,True);
          except
            Result.Free;
            raise;
          end;
        end
        else  
        begin
          UngetToken;
          UngetToken;
          ParseRange;
        end;
      end;
    tkFile:
      begin
        Result := TPasFileType(CreateElement(TPasFileType, TypeName, Parent));
        Try
          ParseFileType(TPasFileType(Result));
          CheckHint(Result,True);
        Except
          Result.free;
          Raise;
        end;
      end;
    tkArray:
      begin
        Result := TPasArrayType(CreateElement(TPasArrayType, TypeName, Parent));
        try
          ParseArrayType(TPasArrayType(Result));
          TPasArrayType(Result).IsPacked := HadPackedModifier;
          CheckHint(Result,True);
        except
          Result.Free;
          raise;
        end;
      end;
    tkSet:
      begin
        Result := TPasSetType(CreateElement(TPasSetType, TypeName, Parent));
        try
          ExpectToken(tkOf);
          TPasSetType(Result).EnumType := ParseType(Result);
          CheckHint(Result,True);
        except
          Result.Free;
          raise;
        end;
      end;
    tkBraceOpen:
      begin
        Result := TPasEnumType(CreateElement(TPasEnumType, TypeName, Parent));
        try
          while True do
          begin
            NextToken;
            EnumValue := TPasEnumValue(CreateElement(TPasEnumValue,
              CurTokenString, Result));
            TPasEnumType(Result).Values.Add(EnumValue);
            NextToken;
            if CurToken = tkBraceClose then
              break
            else if CurToken in [tkEqual,tkAssign] then
              begin
              EnumValue.AssignedValue:=ParseExpression;
              NextToken;
              if CurToken = tkBraceClose then
                Break
              else if not (CurToken=tkComma) then
                ParseExc(SParserExpectedCommaRBracket);
              end
            else if not (CurToken=tkComma) then
              ParseExc(SParserExpectedCommaRBracket)
          end;
          CheckHint(Result,True);
        except
          Result.Free;
          raise;
        end;
      end;
    tkProcedure:
      begin
        Result := TPasProcedureType(CreateElement(TPasProcedureType, TypeName,
          Parent));
        try
          ParseProcedureOrFunctionHeader(Result,
            TPasProcedureType(Result), ptProcedure, True);
        except
          Result.Free;
          raise;
        end;
      end;
    tkFunction:
      begin
        Result := Engine.CreateFunctionType(TypeName, 'Result', Parent, False,
          Scanner.CurFilename, Scanner.CurRow);
        try
          ParseProcedureOrFunctionHeader(Result,
            TPasFunctionType(Result), ptFunction, True);
        except
          Result.Free;
          raise;
        end;
      end;
    tkType:
      begin
        Result := TPasTypeAliasType(CreateElement(TPasTypeAliasType, TypeName,
          Parent));
        try
          TPasTypeAliasType(Result).DestType := ParseType(nil);
          CheckHint(Result,True);
        except
          Result.Free;
          raise;
        end;
      end;
    else
    begin
      UngetToken;
      ParseRange;
    end;
  end;
end;

// Starts after the variable name

procedure TPasParser.ParseInlineVarDecl(Parent: TPasElement; VarList: TList);
begin
  ParseInlineVarDecl(Parent, VarList, visDefault, False);
end;

procedure TPasParser.ParseInlineVarDecl(Parent: TPasElement; VarList: TList;
  AVisibility: TPasMemberVisibility; ClosingBrace: Boolean);
var
  VarNames: TStringList;
  i: Integer;
  VarType: TPasType;
  VarEl: TPasVariable;
  H : TPasMemberHints;

begin
  VarNames := TStringList.Create;
  try
    while True do
    begin
      VarNames.Add(CurTokenString);
      NextToken;
      if CurToken = tkColon then
        break
      else if CurToken <> tkComma then
        ParseExc(SParserExpectedCommaColon);

      ExpectIdentifier;
    end;

    VarType := ParseComplexType(Parent);

    H:=CheckHint(Nil,False);
    NextToken;

    for i := 0 to VarNames.Count - 1 do
    begin
      VarEl := TPasVariable(CreateElement(TPasVariable, VarNames[i], Parent,
        AVisibility));
      VarEl.VarType := VarType;
      VarEl.Hints:=H;
      if i > 0 then
        VarType.AddRef;
      VarList.Add(VarEl);
    end;
    // Records may be terminated with end, no semicolon
    if (CurToken <> tkEnd) and (CurToken <> tkSemicolon) and not
      (ClosingBrace and (CurToken = tkBraceClose)) then
      ParseExc(SParserExpectedSemiColonEnd);
  finally
    VarNames.Free;
  end;
end;

// Starts after the variable name
procedure TPasParser.ParseVarDecl(Parent: TPasElement; List: TList);
var
  i: Integer;
  VarType: TPasType;
  Value, S: String;
  M: string;
  H : TPasMemberHints;
begin
  while True do
  begin
    List.Add(CreateElement(TPasVariable, CurTokenString, Parent));
    NextToken;
    if CurToken = tkColon then
      break
    else if CurToken <> tkComma then
      ParseExc(SParserExpectedCommaColon);
    ExpectIdentifier;
  end;
  VarType := ParseComplexType;
  for i := 0 to List.Count - 1 do
  begin
    TPasVariable(List[i]).VarType := VarType;
    if i > 0 then
      VarType.AddRef;
  end;
  //writeln('TPasParser.ParseVarDecl ',CurTokenText);
  NextToken;
  // Writeln(LastVar,': Parsed complex type: ',CurtokenText);
  // NextToken;
  // Writeln(LastVar,': Parsed complex type, next: ',CurtokenText);
  If CurToken=tkEqual then
    begin
    Value := ParseExpression;
    for i := 0 to List.Count - 1 do
      TPasVariable(List[i]).Value := Value;
    NextToken;
    end;

  if CurToken = tkAbsolute then
  begin
    ExpectIdentifier;
    S:=CurTokenText;
    NextToken;
    if CurToken=tkDot then
      begin
      ExpectIdentifier;
      S:=S+'.'+CurTokenText;
      end
    else
      UnGetToken;
    For I:=0 to List.Count-1 do
      TPasVariable(List[i]).AbsoluteLocation:=S;
  end else
    UngetToken;

  H:=CheckHint(Nil,True);
  If (H<>[]) then
    for i := 0 to List.Count - 1 do
      TPasVariable(List[i]).Hints:=H;
  M := '';
  while True do
  begin
    NextToken;
    if CurToken = tkIdentifier then
      begin
      s := UpperCase(CurTokenText);
      if s = 'CVAR' then
        begin
        M := M + '; cvar';
        ExpectToken(tkSemicolon);
        end
      else if (s = 'EXTERNAL') or (s = 'PUBLIC') or (s = 'EXPORT') then
        begin
        M := M + ';' + CurTokenText;
        if s = 'EXTERNAL' then
          begin
          NextToken;
          if ((CurToken = tkString) or (CurToken = tkIdentifier)) and (UpperCase(CurTokenText)<> 'NAME') then
            begin
            // !!!: Is this really correct for tkString?
            M := M + ' ' + CurTokenText;
            NextToken;
            end;
          end
        else
          NextToken;
        if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'NAME') then
          begin
          M := M + ' name ';
          NextToken;
          if (CurToken = tkString) or (CurToken = tkIdentifier) then
            // !!!: Is this really correct for tkString?
            M := M + CurTokenText
          else
            ParseExc(SParserSyntaxError);
          H:=CheckHint(Nil,True);
          If (H<>[]) then
            for i := 0 to List.Count - 1 do
              TPasVariable(List[i]).Hints:=H;

          // ExpectToken(tkSemicolon);
          end
        else if CurToken <> tkSemicolon then
          ParseExc(SParserSyntaxError);
      end else
      begin
        UngetToken;
        break;
      end
    end else
    begin
      UngetToken;
      break;
    end;
  end; // while

  if M <> '' then
    for i := 0 to List.Count - 1 do
      TPasVariable(List[i]).Modifiers := M;
end;

// Starts after the opening bracket token
procedure TPasParser.ParseArgList(Parent: TPasElement; Args: TList; EndToken: TToken);
var
  ArgNames: TStringList;
  IsUntyped: Boolean;
  Name, Value: String;
  i: Integer;
  Arg: TPasArgument;
  Access: TArgumentAccess;
  ArgType: TPasType;
begin
  while True do
  begin
    ArgNames := TStringList.Create;
    Access := argDefault;
    IsUntyped := False;
    ArgType := nil;
    while True do
    begin
      NextToken;
      if CurToken = tkConst then
      begin
        Access := argConst;
        Name := ExpectIdentifier;
      end else if CurToken = tkVar then
      begin
        Access := ArgVar;
        Name := ExpectIdentifier;
      end else if (CurToken = tkIdentifier) and (UpperCase(CurTokenString) = 'OUT') then
      begin
        Access := ArgOut;
        Name := ExpectIdentifier;
      end else if CurToken = tkIdentifier then
        Name := CurTokenString
      else
        ParseExc(SParserExpectedConstVarID);
      ArgNames.Add(Name);
      NextToken;
      if CurToken = tkColon then
        break
      else if ((CurToken = tkSemicolon) or (CurToken = tkBraceClose)) and
        (Access <> argDefault) then
      begin
        // found an untyped const or var argument
        UngetToken;
        IsUntyped := True;
        break
      end
      else if CurToken <> tkComma then
        ParseExc(SParserExpectedCommaColon);
    end;
    SetLength(Value, 0);
    if not IsUntyped then
    begin
      ArgType := ParseType(nil);
      NextToken;
      if CurToken = tkEqual then
      begin
        Value := ParseExpression;
      end else
        UngetToken;
    end;

    for i := 0 to ArgNames.Count - 1 do
    begin
      Arg := TPasArgument(CreateElement(TPasArgument, ArgNames[i], Parent));
      Arg.Access := Access;
      Arg.ArgType := ArgType;
      if (i > 0) and Assigned(ArgType) then
        ArgType.AddRef;
      Arg.Value := Value;
      Args.Add(Arg);
    end;

    ArgNames.Free;
    NextToken;
    if CurToken = EndToken then
      break;
  end;
end;

// Next token is expected to be a "(", ";" or for a function ":". The caller
// will get the token after the final ";" as next token.
procedure TPasParser.ParseProcedureOrFunctionHeader(Parent: TPasElement;
  Element: TPasProcedureType; ProcType: TProcType; OfObjectPossible: Boolean);
Var
  Tok : String;
  i: Integer;
  Proc: TPasProcedure;

begin
  NextToken;
  case ProcType of
    ptFunction,ptClassFunction:
      begin
        if CurToken = tkBraceOpen then
        begin
          NextToken;
          if (CurToken <> tkBraceClose) then
            begin
              UngetToken;
              ParseArgList(Parent, Element.Args, tkBraceClose);
            end;
          ExpectToken(tkColon);
        end else if CurToken <> tkColon then
          ParseExc(SParserExpectedLBracketColon);
        if Assigned(Element) then        // !!!
          TPasFunctionType(Element).ResultEl.ResultType := ParseType(Parent)
        else
          ParseType(nil);
      end;

    ptProcedure,ptConstructor,ptDestructor,ptClassProcedure:
      begin
        if CurToken = tkBraceOpen then
        begin
          NextToken;
          if (CurToken = tkBraceClose) then
          else
            begin
              UngetToken;
              ParseArgList(Element, Element.Args, tkBraceClose);
            end
        end else if (CurToken = tkSemicolon)
          or (OfObjectPossible and (CurToken in [tkOf,tkEqual]))
        then
          UngetToken
        else
          ParseExc(SParserExpectedLBracketSemicolon);
      end;
    ptOperator:
      begin
        ParseArgList(Element, Element.Args, tkBraceClose);
        NextToken;
        if (CurToken=tkIdentifier) then
        begin
          TPasFunctionType(Element).ResultEl.Name := CurTokenName;
          ExpectToken(tkColon);
        end
        else if (CurToken=tkColon) then
          TPasFunctionType(Element).ResultEl.Name := 'Result'
        else
          ParseExc(SParserExpectedColonID);
        if Assigned(Element) then        // !!!
          TPasFunctionType(Element).ResultEl.ResultType := ParseType(Parent)
        else
          ParseType(nil);
      end;
  end;

  NextToken;
  if OfObjectPossible and (CurToken = tkOf) then
  begin
    ExpectToken(tkObject);
    Element.IsOfObject := True;
  end else
    UngetToken;

  NextToken;
  if CurToken = tkEqual then
  begin
    // for example: const p: procedure = nil;
    UngetToken;
    exit;
  end else
    UngetToken;

  ExpectToken(tkSemicolon);

  while True do
    begin
    // CheckHint(Element,False);
    NextToken;
    if (CurToken = tkIdentifier) then
      begin
      Tok:=UpperCase(CurTokenString);
      If (Tok='CDECL') then
        begin
        TPasProcedure(Parent).CallingConvention:=ccCDecl;
        ExpectToken(tkSemicolon);
        end
      else If (Tok='EXPORT') then
        begin
        TPasProcedure(Parent).AddModifier(pmExported);
        ExpectToken(tkSemicolon);
        end
      else if (Tok='PASCAL') then
        begin
        TPasProcedure(Parent).CallingConvention:=ccPascal;
        ExpectToken(tkSemicolon);
        end
      else if (Tok='STDCALL') then
        begin
        TPasProcedure(Parent).CallingConvention:=ccStdCall;
        ExpectToken(tkSemicolon);
        end
      else if (Tok='OLDFPCCALL') then
        begin
        TPasProcedure(Parent).CallingConvention:=ccOldFPCCall;
        ExpectToken(tkSemicolon);
        end
      else if (Tok='EXTDECL') then
        begin
        // extdecl is a common macro for external functions
        TPasProcedure(Parent).AddModifier(pmExternal);
        ExpectToken(tkSemicolon);
        end
      else if (Tok='EXTERNAL') then
        begin
        TPasProcedure(Parent).AddModifier(pmExternal);
        NextToken;
        if CurToken in [tkString,tkIdentifier] then
        begin
          NextToken;
          Tok:=UpperCase(CurTokenString);
          if Tok='NAME' then
          begin
            NextToken;
            if not (CurToken in [tkString,tkIdentifier]) then
              ParseExc(Format(SParserExpectTokenError, [TokenInfos[tkString]]));
          end;
        end else
          UngetToken;
        ExpectToken(tkSemicolon);
        end
      else if (Tok='REGISTER') then
        begin
        TPasProcedure(Parent).CallingConvention:=ccRegister;
        ExpectToken(tkSemicolon);
        end
      else if (Tok='COMPILERPROC') then
        begin
        TPasProcedure(Parent).AddModifier(pmCompilerProc);
        ExpectToken(tkSemicolon);
        end
      else if (Tok='VARARGS') then
        begin
        TPasProcedure(Parent).AddModifier(pmVarArgs);
        ExpectToken(tkSemicolon);
        end
      else if (tok='DEPRECATED') then
        begin
        element.hints:=element.hints+[hDeprecated];
        ExpectToken(tkSemicolon);
        end
      else if (tok='PLATFORM') then
        begin
        element.hints:=element.hints+[hPlatform];
        ExpectToken(tkSemicolon);
        end
      else if (tok='LIBRARY') then
        begin
        element.hints:=element.hints+[hLibrary];
        ExpectToken(tkSemicolon);
        end
      else if (tok='OVERLOAD') then
        begin
        TPasProcedure(Parent).AddModifier(pmOverload);
        ExpectToken(tkSemicolon);
        end
      else if (tok='INLINE') then
        begin
        TPasProcedure(Parent).AddModifier(pmInline);
        ExpectToken(tkSemicolon);
        end
      else if (tok='ASSEMBLER') then
        begin
        TPasProcedure(Parent).AddModifier(pmAssembler);
        ExpectToken(tkSemicolon);
        end
      else if (tok = 'EXTERNAL') then
        repeat
          NextToken;
        until CurToken = tkSemicolon
      else if (tok = 'PUBLIC') then
        begin
          NextToken;
          { Should be token Name,
            if not we're in a class and the public section starts }
          If (Uppercase(CurTokenString)<>'NAME') then
            begin
            UngetToken;
            UngetToken;
            Break;
            end
          else
            begin
            NextToken;  // Should be export name string.
            ExpectToken(tkSemicolon);
            end;
        end
      else if (tok = 'FORWARD') then
        begin
          if (Parent.Parent is TInterfaceSection) then
            begin
            UngetToken;
            break;
            end;
          if Parent is TPasProcedure then
            TPasProcedure(Parent).AddModifier(pmForward);
          ExpectToken(tkSemicolon);
        end
      else
        begin
        UnGetToken;
        Break;
        end
      end
    else if (CurToken = tkInline) then
      begin
      if Parent is TPasProcedure then
        TPasProcedure(Parent).AddModifier(pmInline);
      ExpectToken(tkSemicolon);
      end
    else if (CurToken = tkSquaredBraceOpen) then
      begin
      repeat
        NextToken
      until CurToken = tkSquaredBraceClose;
      ExpectToken(tkSemicolon);
      end
    else
      begin
      UngetToken;
      break;
      end;
    end;
  if (ProcType = ptOperator) and (Parent is TPasProcedure) then
  begin
    Proc:=TPasProcedure(Parent);
    Proc.Name := Proc.Name + '(';
    for i := 0 to Proc.ProcType.Args.Count - 1 do
    begin
      if i > 0 then
        Proc.Name := Proc.Name + ', ';
      Proc.Name := Proc.Name +
        TPasArgument(Proc.ProcType.Args[i]).ArgType.Name;
    end;
    Proc.Name := Proc.Name + '): ' +
      TPasFunctionType(Proc.ProcType).ResultEl.ResultType.Name;
  end;

  if (Parent is TPasProcedure)
  and (not TPasProcedure(Parent).IsForward)
  and (not TPasProcedure(Parent).IsExternal)
  and ((Parent.Parent is TImplementationSection)
     or (Parent.Parent is TProcedureBody))
  then
    ParseProcedureBody(Parent);
end;

// starts after the semicolon
procedure TPasParser.ParseProcedureBody(Parent: TPasElement);
var
  Body: TProcedureBody;
begin
  Body := TProcedureBody(CreateElement(TProcedureBody, '', Parent));
  ParseDeclarations(Body);
end;


procedure TPasParser.ParseProperty(Element:TPasElement);

  procedure MaybeReadFullyQualifiedIdentifier(Var r : String);

  begin
    while True do
      begin
      NextToken;
      if CurToken = tkDot then
        begin
        ExpectIdentifier;
        R:=R + '.' + CurTokenString;
        end
      else
        break;
      end;
  end;

  function GetAccessorName: String;
  begin
    ExpectIdentifier;
    Result := CurTokenString;
    MaybeReadFullyQualifiedIdentifier(Result);
    if CurToken = tkSquaredBraceOpen then begin
      Result := Result + '[';
      NextToken;
      if CurToken in [tkIdentifier, tkNumber] then begin
    Result := Result + CurTokenString;
      end;
      ExpectToken(tkSquaredBraceClose);
      Result := Result + ']';
    end else
      UngetToken;
    //MaybeReadFullyQualifiedIdentifier(Result);
    //writeln(Result);
  end;

begin

  NextToken;
// if array prop then parse [ arg1:type1;... ]
  if CurToken = tkSquaredBraceOpen then begin
  // !!!: Parse array properties correctly
    ParseArgList(Element, TPasProperty(Element).Args, tkSquaredBraceClose);
    NextToken;
  end;

  if CurToken = tkColon then begin
// if ":prop_data_type" if supplied then read it
  // read property type
    TPasProperty(Element).VarType := ParseType(Element);
    NextToken;
  end;

  if CurToken <> tkSemicolon then begin
//  if indexed prop then read the index value
    if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'INDEX') then
//    read 'index' access modifier
      TPasProperty(Element).IndexValue := ParseExpression
    else
//    not indexed prop will be recheck for another token
      UngetToken;

    NextToken;
  end;

// if the accessors list is not finished
  if CurToken <> tkSemicolon then begin
    // read 'read' access modifier
    if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'READ') then
      TPasProperty(Element).ReadAccessorName := GetAccessorName
    else
//    not read accessor will be recheck for another token
      UngetToken;

    NextToken;
  end;

// if the accessors list is not finished
  if CurToken <> tkSemicolon then begin
    // read 'write' access modifier
    if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'WRITE') then
      TPasProperty(Element).WriteAccessorName := GetAccessorName
    else
    if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'IMPLEMENTS') then
      TPasProperty(Element).ImplementsName := GetAccessorName
    else
//    not write accessor will be recheck for another token
      UngetToken;

    NextToken;
  end;

// if the specifiers list is not finished
  if CurToken <> tkSemicolon then begin
    // read 'stored' access modifier
    if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'STORED') then begin
      NextToken;
      if CurToken = tkTrue then
        TPasProperty(Element).StoredAccessorName := 'True'
      else if CurToken = tkFalse then
        TPasProperty(Element).StoredAccessorName := 'False'
      else if CurToken = tkIdentifier then
        TPasProperty(Element).StoredAccessorName := CurTokenString
      else
        ParseExc(SParserSyntaxError);
    end else
//    not stored accessor will be recheck for another token
      UngetToken;

    NextToken;
  end;

// if the specifiers list is not finished
  if CurToken <> tkSemicolon then begin
    if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'DEFAULT') then
//    read 'default' value modifier -> ParseExpression(DEFAULT <value>)
      TPasProperty(Element).DefaultValue := ParseExpression
    else
//    not "default <value>" prop will be recheck for another token
      UngetToken;

    NextToken;
  end;

// if the specifiers list is not finished
  if CurToken <> tkSemicolon then begin
    if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'NODEFAULT') then begin
//    read 'nodefault' modifier
      TPasProperty(Element).IsNodefault:=true;
    end;
//  stop recheck for specifiers - start from next token
    NextToken;
  end;

// after NODEFAULT may be a ";"
  if CurToken = tkSemicolon then begin
    // read semicolon
    NextToken;
  end;

  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'DEFAULT') then begin
//  what is after DEFAULT token at the end
    NextToken;
    if CurToken = tkSemicolon then begin
//    ";" then DEFAULT=prop
      TPasProperty(Element).IsDefault := True;
      UngetToken;
    end else begin
//    "!;" then a step back to get phrase "DEFAULT <value>"
      UngetToken;
//    DefaultValue  -> ParseExpression(DEFAULT <value>)  and stay on the <value>
      TPasProperty(Element).DefaultValue := ParseExpression;
    end;

//!!  there may be DEPRECATED token
    CheckHint(Element,False);
    NextToken;

  end;

// after DEFAULT may be a ";"
  if CurToken = tkSemicolon then begin
    // read semicolon
    NextToken;
  end;

  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'DEPRECATED') then begin
//  nothing to do on DEPRECATED - just to accept
//    NextToken;
  end else
    UngetToken;;

//!!   else
//  not DEFAULT prop accessor will be recheck for another token
//!!    UngetToken;

{
  if CurToken = tkSemicolon then begin
    // read semicolon
    NextToken;
  end;
  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'DEPRECATED') then begin
//  nothing to do - just to process
    NextToken;
  end;
  if CurToken = tkSemicolon then begin
    // read semicolon
    NextToken;
  end;
}
end;

// Starts after the "begin" token
procedure TPasParser.ParseProcBeginBlock(Parent: TProcedureBody);
var
  BeginBlock: TPasImplBeginBlock;
  SubBlock: TPasImplElement;
begin
  //writeln('TPasParser.ParseProcBeginBlock ');

  BeginBlock := TPasImplBeginBlock(CreateElement(TPasImplBeginBlock, '', Parent));
  Parent.Body := BeginBlock;
  repeat
    NextToken;
    if CurToken=tkend then
      break
    else if CurToken<>tkSemiColon then
    begin
      UngetToken;
      ParseStatement(BeginBlock,SubBlock);
      if SubBlock=nil then
        ExpectToken(tkend);
    end;
  until false;
  ExpectToken(tkSemicolon);
end;

// Next token is start of (compound) statement
// After parsing CurToken is on last token of statement
procedure TPasParser.ParseStatement(Parent: TPasImplBlock;
  out NewImplElement: TPasImplElement);
var
  CurBlock: TPasImplBlock;

  {$IFDEF VerbosePasParser}
  function i: string;
  var
    c: TPasElement;
  begin
    Result:='ParseImplCompoundStatement ';
    c:=CurBlock;
    while c<>nil do begin
      Result:=Result+'  ';
      c:=c.Parent;
    end;
  end;
  {$ENDIF}

  function CloseBlock: boolean; // true if parent reached
  begin
    CurBlock:=CurBlock.Parent as TPasImplBlock;
    Result:=CurBlock=Parent;
  end;

  function CloseStatement(CloseIfs: boolean): boolean; // true if parent reached
  begin
    if CurBlock=Parent then exit(true);
    while CurBlock.CloseOnSemicolon
    or (CloseIfs and (CurBlock is TPasImplIfElse)) do
      if CloseBlock then exit(true);
    Result:=false;
  end;

  procedure CreateBlock(NewBlock: TPasImplBlock);
  begin
    CurBlock:=NewBlock;
    if NewImplElement=nil then NewImplElement:=CurBlock;
  end;

var
  Condition: String;
  Command: String;
  StartValue: String;
  VarName: String;
  EndValue: String;
  Expr: String;
  SubBlock: TPasImplElement;
  CmdElem: TPasImplCommand;
  TypeName: String;
  ForDownTo: Boolean;
begin
  NewImplElement:=nil;
  CurBlock := Parent;
  while True do
  begin
    NextToken;
    //WriteLn(i,'Token=',CurTokenText);
    case CurToken of
    tkbegin:
      CreateBlock(CurBlock.AddBeginBlock);
    tkrepeat:
      CreateBlock(CurBlock.AddRepeatUntil);
    tkIf:
      begin
        Condition:=ParseExpression;
        //WriteLn(i,'IF Condition="',Condition,'" Token=',CurTokenText);
        CreateBlock(CurBlock.AddIfElse(Condition));
        ExpectToken(tkthen);
      end;
    tkelse:
      if (CurBlock is TPasImplIfElse) then
      begin
        if TPasImplIfElse(CurBlock).IfBranch=nil then
        begin
          // empty then => add dummy command
          CurBlock.AddCommand('');
        end;
      end else if (CurBlock is TPasImplTryExcept) then
      begin
        CloseBlock;
        CurBlock:=TPasImplTry(CurBlock).AddExceptElse;
      end else
        ParseExc(SParserSyntaxError);
    tkwhile:
      begin
        // while Condition do
        Condition:=ParseExpression;
        //WriteLn(i,'WHILE Condition="',Condition,'" Token=',CurTokenText);
        CreateBlock(CurBlock.AddWhileDo(Condition));
        ExpectToken(tkdo);
      end;
    tkfor:
      begin
        // for VarName := StartValue to EndValue do
        ExpectIdentifier;
        VarName:=CurTokenString;
        ExpectToken(tkAssign);
        StartValue:=ParseExpression;
        //writeln(i,'FOR Start=',StartValue);
        NextToken;
        if CurToken=tkTo then
          ForDownTo:=false
        else if CurToken=tkdownto then
          ForDownTo:=true
        else
          ParseExc(Format(SParserExpectTokenError, [TokenInfos[tkTo]]));
        EndValue:=ParseExpression;
        CreateBlock(CurBlock.AddForLoop(VarName,StartValue,EndValue,ForDownTo));
        //WriteLn(i,'FOR "',VarName,'" := ',StartValue,' to ',EndValue,' Token=',CurTokenText);
        ExpectToken(tkdo);
      end;
    tkwith:
      begin
        // with Expr do
        // with Expr, Expr do
        Expr:=ParseExpression;
        //writeln(i,'WITH Expr="',Expr,'" Token=',CurTokenText);
        CreateBlock(CurBlock.AddWithDo(Expr));
        repeat
          NextToken;
          if CurToken=tkdo then break;
          if CurToken<>tkComma then
            ParseExc(Format(SParserExpectTokenError, [TokenInfos[tkdo]]));
          Expr:=ParseExpression;
          //writeln(i,'WITH ...,Expr="',Expr,'" Token=',CurTokenText);
          TPasImplWithDo(CurBlock).AddExpression(Expr);
        until false;
      end;
    tkcase:
      begin
        Expr:=ParseExpression;
        //writeln(i,'CASE OF Expr="',Expr,'" Token=',CurTokenText);
        ExpectToken(tkof);
        CreateBlock(CurBlock.AddCaseOf(Expr));
        repeat
          NextToken;
          //writeln(i,'CASE OF Token=',CurTokenText);
          case CurToken of
          tkend:
            break; // end without else
          tkelse:
            begin
              // create case-else block
              CurBlock:=TPasImplCaseOf(CurBlock).AddElse;
              break;
            end
          else
            UngetToken;
            // read case values
            repeat
              Expr:=ParseExpression;
              //writeln(i,'CASE value="',Expr,'" Token=',CurTokenText);
              if CurBlock is TPasImplCaseStatement then
                TPasImplCaseStatement(CurBlock).Expressions.Add(Expr)
              else
                CurBlock:=TPasImplCaseOf(CurBlock).AddCase(Expr);
              NextToken;
              if CurToken=tkDotDot then
              begin
                Expr:=Expr+'..'+ParseExpression;
                NextToken;
              end;
              //writeln(i,'CASE after value Token=',CurTokenText);
              if CurToken=tkColon then break;
              if CurToken<>tkComma then
                ParseExc(Format(SParserExpectTokenError, [TokenInfos[tkComma]]));
            until false;
            // read statement
            ParseStatement(CurBlock,SubBlock);
            CloseBlock;
            if CurToken<>tkSemicolon then
            begin
              NextToken;
              if not (CurToken in [tkSemicolon,tkelse,tkend]) then
                ParseExc(Format(SParserExpectTokenError, [TokenInfos[tkSemicolon]]));
              if CurToken<>tkSemicolon then
                UngetToken;
            end;
          end;
        until false;
        if CurToken=tkend then
        begin
          if CloseBlock then break;
          if CloseStatement(false) then break;
        end;
      end;
    tktry:
      CreateBlock(CurBlock.AddTry);
    tkfinally:
      begin
        if CloseStatement(true) then
        begin
          UngetToken;
          break;
        end;
        if CurBlock is TPasImplTry then
        begin
          CurBlock:=TPasImplTry(CurBlock).AddFinally;
        end else
          ParseExc(SParserSyntaxError);
      end;
    tkexcept:
      begin
        if CloseStatement(true) then
        begin
          UngetToken;
          break;
        end;
        if CurBlock is TPasImplTry then
        begin
          //writeln(i,'EXCEPT');
          CurBlock:=TPasImplTry(CurBlock).AddExcept;
        end else
          ParseExc(SParserSyntaxError);
      end;
    tkon:
      begin
        // in try except:
        // on E: Exception do
        // on Exception do
        if CurBlock is TPasImplTryExcept then
        begin
          VarName:='';
          TypeName:=ParseExpression;
          //writeln(i,'ON t=',TypeName,' Token=',CurTokenText);
          NextToken;
          if CurToken=tkColon then
          begin
            VarName:=TypeName;
            TypeName:=ParseExpression;
            //writeln(i,'ON v=',VarName,' t=',TypeName,' Token=',CurTokenText);
          end else
            UngetToken;
          CurBlock:=TPasImplTryExcept(CurBlock).AddExceptOn(VarName,TypeName);
          ExpectToken(tkDo);
        end else
          ParseExc(SParserSyntaxError);
      end;
    tkraise:
      CreateBlock(CurBlock.AddRaise);
    tkend:
      begin
        if CloseStatement(true) then
        begin
          UngetToken;
          break;
        end;
        if CurBlock is TPasImplBeginBlock then
        begin
          if CloseBlock then break; // close end
          if CloseStatement(false) then break;
        end else if CurBlock is TPasImplCaseElse then
        begin
          if CloseBlock then break; // close else
          if CloseBlock then break; // close caseof
          if CloseStatement(false) then break;
        end else if CurBlock is TPasImplTryHandler then
        begin
          if CloseBlock then break; // close finally/except
          if CloseBlock then break; // close try
          if CloseStatement(false) then break;
        end else
          ParseExc(SParserSyntaxError);
      end;
    tkSemiColon:
      if CloseStatement(true) then break;
    tkuntil:
      begin
        if CloseStatement(true) then
        begin
          UngetToken;
          break;
        end;
        if CurBlock is TPasImplRepeatUntil then
        begin
          Condition:=ParseExpression;
          TPasImplRepeatUntil(CurBlock).Condition:=Condition;
          //WriteLn(i,'UNTIL Condition="',Condition,'" Token=',CurTokenString);
          if CloseBlock then break;
        end else
          ParseExc(SParserSyntaxError);
      end;
    else
      UngetToken;
      Command:=ParseCommand;
      //WriteLn(i,'COMMAND="',Command,'" Token=',CurTokenString);
      if Command='' then
        ParseExc(SParserSyntaxError);
      CmdElem:=CurBlock.AddCommand(Command);
      if NewImplElement=nil then NewImplElement:=CmdElem;
      if CloseStatement(false) then break;
    end;
  end;
end;


// Starts after the "procedure" or "function" token
function TPasParser.ParseProcedureOrFunctionDecl(Parent: TPasElement;
  ProcType: TProcType): TPasProcedure;

  function ExpectProcName: string;
  begin
    Result:=ExpectIdentifier;
    //writeln('ExpectProcName ',Parent.Classname);
    if Parent is TImplementationSection then
    begin
      NextToken;
      if CurToken=tkDot then
      begin
        Result:=Result+'.'+ExpectIdentifier;
      end else
        UngetToken;
    end;
  end;

var
  Name: String;
begin
  case ProcType of
    ptFunction:
      begin
        Name := ExpectProcName;
        Result := TPasFunction(CreateElement(TPasFunction, Name, Parent));
        Result.ProcType := Engine.CreateFunctionType('', 'Result', Result, True,
                                           Scanner.CurFilename, Scanner.CurRow);
      end;
    ptClassFunction:
      begin
        Name := ExpectProcName;
        Result := TPasClassFunction(CreateElement(TPasClassFunction, Name, Parent));
        Result.ProcType := Engine.CreateFunctionType('', 'Result', Result, True,
                                           Scanner.CurFilename, Scanner.CurRow);
      end;
    ptProcedure:
      begin
        Name := ExpectProcName;
        Result := TPasProcedure(CreateElement(TPasProcedure, Name, Parent));
        Result.ProcType := TPasProcedureType(CreateElement(TPasProcedureType, '',
          Result));
      end;
    ptClassProcedure:
      begin
        Name := ExpectProcName;
        Result := TPasClassProcedure(CreateElement(TPasClassProcedure, Name, Parent));
        Result.ProcType := TPasProcedureType(CreateElement(TPasProcedureType, '',
          Result));
      end;
    ptOperator:
      begin
        NextToken;
        Name := 'operator ' + TokenInfos[CurToken];
        Result := TPasOperator(CreateElement(TPasOperator, Name, Parent));
        Result.ProcType := Engine.CreateFunctionType('', '__INVALID__', Result,
                                     True, Scanner.CurFilename, Scanner.CurRow);
      end;
    ptConstructor:
      begin
        Name := ExpectProcName;
        Result := TPasConstructor(CreateElement(TPasConstructor, Name, Parent));
        Result.ProcType := TPasProcedureType(CreateElement(TPasProcedureType, '',
          Result));
      end;
    ptDestructor:
      begin
        Name := ExpectProcName;
        Result := TPasDestructor(CreateElement(TPasDestructor, Name, Parent));
        Result.ProcType := TPasProcedureType(CreateElement(TPasProcedureType, '',
          Result));
      end;
  end;
  //writeln('TPasParser.ParseProcedureOrFunctionDecl Name="',Name,'" Token=',CurTokenText);

  ParseProcedureOrFunctionHeader(Result, Result.ProcType, ProcType, False);
end;


// Starts after the "record" token
procedure TPasParser.ParseRecordDecl(Parent: TPasRecordType; IsNested: Boolean);
var
  VariantName: String;
  Variant: TPasVariant;
begin
  while True do
  begin
    if IsNested then
    begin
      if CurToken = tkBraceClose then
        break;
      NextToken;
      if CurToken = tkBraceClose then
        break;
    end else
    begin
      if CurToken = tkEnd then
        break;
      NextToken;
      if CurToken = tkEnd then
        break;
    end;
    if CurToken = tkCase then
    begin
      ExpectToken(tkIdentifier);
      VariantName := CurTokenString;
      NextToken;
      if CurToken = tkColon then
        Parent.VariantName := VariantName
      else
      begin
        UngetToken;
    UngetToken;
      end;
      Parent.VariantType := ParseType(Parent);
      Parent.Variants := TList.Create;

      ExpectToken(tkOf);

      while True do
      begin
        Variant := TPasVariant(CreateElement(TPasVariant, '', Parent));
        Parent.Variants.Add(Variant);
        Variant.Values := TStringList.Create;
        while True do
        begin
      Variant.Values.Add(ParseExpression);
      NextToken;
      if CurToken = tkColon then
        break
      else if CurToken <> tkComma then
        ParseExc(SParserExpectedCommaColon);
    end;
        ExpectToken(tkBraceOpen);
    Variant.Members := TPasRecordType(CreateElement(TPasRecordType, '',
      Variant));
    try
          ParseRecordDecl(Variant.Members, True);
    except
      Variant.Members.Free;
      raise;
    end;
    NextToken;
    if CurToken = tkSemicolon then
      NextToken;
    if (CurToken = tkEnd) or (CurToken = tkBraceClose) then
      break
    else
      UngetToken;
      end
    end else
      ParseInlineVarDecl(Parent, Parent.Members, visDefault, IsNested);
  end;
end;

// Starts after the "class" token
function TPasParser.ParseClassDecl(Parent: TPasElement;
  const AClassName: String; AObjKind: TPasObjKind): TPasType;
var
  CurVisibility: TPasMemberVisibility;

  procedure ProcessMethod(const MethodTypeName: String; HasReturnValue: Boolean);
  var
    Owner: TPasElement;
    Proc: TPasProcedure;
    s: String;
    pt: TProcType;
  begin
    ExpectIdentifier;
    Owner := CheckIfOverloaded(TPasClassType(Result), CurTokenString);
    if HasReturnValue then
    begin
      Proc := TPasFunction(CreateElement(TPasFunction, CurTokenString, Owner,
        CurVisibility));
      Proc.ProcType := Engine.CreateFunctionType('', 'Result', Proc, True,
        Scanner.CurFilename, Scanner.CurRow);
    end else
    begin
      // !!!: The following is more than ugly
      if MethodTypeName = 'constructor' then
        Proc := TPasConstructor(CreateElement(TPasConstructor, CurTokenString,
          Owner, CurVisibility))
      else if MethodTypeName = 'destructor' then
        Proc := TPasDestructor(CreateElement(TPasDestructor, CurTokenString,
          Owner, CurVisibility))
      else
        Proc := TPasProcedure(CreateElement(TPasProcedure, CurTokenString,
          Owner, CurVisibility));
      Proc.ProcType := TPasProcedureType(CreateElement(TPasProcedureType, '',
        Proc, CurVisibility));
    end;
    if Owner.ClassType = TPasOverloadedProc then
      TPasOverloadedProc(Owner).Overloads.Add(Proc)
    else
      TPasClassType(Result).Members.Add(Proc);

    if HasReturnValue then
      pt := ptFunction
    else
      pt := ptProcedure;
    ParseProcedureOrFunctionHeader(Proc, Proc.ProcType, pt, False);

    while True do
    begin
      NextToken;
      if CurToken = tkIdentifier then
      begin
        s := UpperCase(CurTokenString);
        if s = 'VIRTUAL' then
          Proc.AddModifier(pmVirtual)
        else if s = 'DYNAMIC' then
          Proc.AddModifier(pmDynamic)
        else if s = 'ABSTRACT' then
          Proc.AddModifier(pmAbstract)
        else if s = 'OVERRIDE' then
          Proc.AddModifier(pmOverride)
        else if s = 'REINTRODUCE' then
          Proc.AddModifier(pmReintroduce)
        else if s = 'OVERLOAD' then
          Proc.AddModifier(pmOverload)
        else if s = 'STATIC' then
          Proc.AddModifier(pmStatic)
        else if s = 'MESSAGE' then begin
          Proc.AddModifier(pmMessage);
          repeat
            NextToken;
            If CurToken<>tkSemicolon then
              begin
              Proc.MessageName:=CurtokenString;
              If (CurToken=tkString) then
                Proc.Messagetype:=pmtString;
              end;
          until CurToken = tkSemicolon;
          UngetToken;
        end
    else if s = 'CDECL' then
      Proc.CallingConvention:=ccCDecl
    else if s = 'PASCAL' then
      Proc.CallingConvention:=ccPascal
        else if s = 'STDCALL' then
          Proc.CallingConvention:=ccStdCall
        else if s = 'OLDFPCCALL' then
          Proc.CallingConvention:=ccOldFPCCall
        else if s = 'EXTDECL' then
          Proc.AddModifier(pmExtdecl)
        else if s = 'DEPRECATED' then
         Proc.Hints:=Proc.Hints+[hDeprecated]
        else if s = 'EXPORT' then
          Proc.AddModifier(pmExported)
        else
        begin
          UngetToken;
          break;
        end;
        ExpectToken(tkSemicolon);
      end else
      begin
        UngetToken;
        break;
      end;
    end;
  end;

var
  s, SourceFilename: String;
  i, SourceLinenumber: Integer;
  VarList: TList;
  Element: TPasElement;
  isStrict: Boolean;
begin
  isStrict:=False;

  // Save current parsing position to get it correct in all cases
  SourceFilename := Scanner.CurFilename;
  SourceLinenumber := Scanner.CurRow;

  NextToken;

  if (AObjKind = okClass) and (CurToken = tkOf) then
  begin
    Result := TPasClassOfType(Engine.CreateElement(TPasClassOfType, AClassName,
      Parent, SourceFilename, SourceLinenumber));
    ExpectIdentifier;
    UngetToken;                // Only names are allowed as following type
    TPasClassOfType(Result).DestType := ParseType(Result);
    ExpectToken(tkSemicolon);
    exit;
  end;


  Result := TPasClassType(Engine.CreateElement(TPasClassType, AClassName,
    Parent, SourceFilename, SourceLinenumber));

  try
    TPasClassType(Result).ObjKind := AObjKind;

    // nettism/new delphi features
    if (CurToken = tkIdentifier) and (AObjKind = okClass) then begin
      s := LowerCase(CurTokenString);
      if (s = 'sealed') or (s = 'abstract') then begin
        TPasClassType(Result).Modifiers.Add(s);
        NextToken;
      end else
        ExpectToken(tkSemicolon);
    end;

    // Parse ancestor list
    if CurToken = tkBraceOpen then
    begin
      TPasClassType(Result).AncestorType := ParseType(nil);
      while True do
      begin
        NextToken;
        if CurToken = tkBraceClose then
          break;
        UngetToken;
        ExpectToken(tkComma);
        ExpectIdentifier;
        // !!!: Store interface name
      end;
      NextToken;
    end
    else
      TPasClassType(Result).isForward:=CurToken=tkSemicolon;

    if CurToken <> tkSemicolon then
    begin
      if ( AObjKind = okInterface ) and ( CurToken = tkSquaredBraceOpen ) then
      begin
        ExpectToken(tkString);
        TPasClassType(Result).InterfaceGUID := CurTokenString;
        ExpectToken(tkSquaredBraceClose);
      end;
      CurVisibility := visDefault;
      while CurToken <> tkEnd do
      begin
        case CurToken of
          tkIdentifier:
            begin
              s := LowerCase(CurTokenString);
              if s = 'strict' then
              begin
                isStrict:=True;
                NextToken;
                s := LowerCase(CurTokenString);
              end
              else
                isStrict:=False;

              if s = 'private' then
                CurVisibility := visPrivate
              else if s = 'protected' then
                CurVisibility := visProtected
              else if s = 'public' then
                CurVisibility := visPublic
              else if s = 'published' then
                CurVisibility := visPublished
              else if s = 'automated' then
                CurVisibility := visAutomated
              else
              begin
                VarList := TList.Create;
                try
                  ParseInlineVarDecl(Result, VarList, CurVisibility, False);
                  for i := 0 to VarList.Count - 1 do
                  begin
                    Element := TPasElement(VarList[i]);
                    Element.Visibility := CurVisibility;
                    TPasClassType(Result).Members.Add(Element);
                  end;
                finally
                  VarList.Free;
                end;
              end;
              if isStrict then
              begin
                case CurVisibility of
                  visPrivate   : CurVisibility:=visStrictPrivate;
                  visProtected : CurVisibility:=visStrictProtected;
                else
                  ParseExc('strange strict visiblity');
                end;
              end;

            end;
          tkProcedure:
            ProcessMethod('procedure', False);
          tkFunction:
            ProcessMethod('function', True);
          tkConstructor:
            ProcessMethod('constructor', False);
          tkDestructor:
            ProcessMethod('destructor', False);
          tkProperty:
            begin
              ExpectIdentifier;
              Element := CreateElement(TPasProperty, CurTokenString, Result, CurVisibility);
              TPasClassType(Result).Members.Add(Element);
              ParseProperty(Element);
            end;
          tkVar: // vars (nettism/new delphi features)
            if AObjKind<>okClass then ExpectToken(tkSemicolon);
          //todo: class vars
        end; // end case
        NextToken;
      end;
      // Eat semicolon after class...end
      ExpectToken(tkSemicolon);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent,
    Scanner.CurFilename, Scanner.CurRow);
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent, AVisibility,
    Scanner.CurFilename, Scanner.CurRow);
end;


function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: String): TPasModule;
var
  FileResolver: TFileResolver;
  Parser: TPasParser;
  Start, CurPos: PChar;
  Filename: String;
  Scanner: TPascalScanner;

  procedure ProcessCmdLinePart;
  var
    l: Integer;
    s: String;
  begin
    l := CurPos - Start;
    SetLength(s, l);
    if l > 0 then
      Move(Start^, s[1], l)
    else
      exit;
    if s[1] = '-' then
    begin
      case s[2] of
        'd': // -d define
          Scanner.Defines.Append(UpperCase(Copy(s, 3, Length(s))));
        'F': // -F
          if s[3] = 'i' then // -Fi include path
            FileResolver.AddIncludePath(Copy(s, 4, Length(s)));
        'I': // -I include path
          FileResolver.AddIncludePath(Copy(s, 3, Length(s)));
        'S': // -S mode
          if s[3]='d' then
            begin // -Sd mode delphi
              include(Scanner.Options,po_delphi);
              include(Parser.Options,po_delphi);
            end;
      end;
    end else
      if Filename <> '' then
        raise Exception.Create(SErrMultipleSourceFiles)
      else
        Filename := s;
  end;

var
  s: String;
begin
  Result := nil;
  FileResolver := nil;
  Scanner := nil;
  Parser := nil;
  try
    FileResolver := TFileResolver.Create;
    Scanner := TPascalScanner.Create(FileResolver);
    Scanner.Defines.Append('FPK');
    Scanner.Defines.Append('FPC');

    // TargetOS
    s := UpperCase(OSTarget);
    Scanner.Defines.Append(s);
    if s = 'LINUX' then
      Scanner.Defines.Append('UNIX')
    else if s = 'FREEBSD' then
    begin
      Scanner.Defines.Append('BSD');
      Scanner.Defines.Append('UNIX');
    end else if s = 'NETBSD' then
    begin
      Scanner.Defines.Append('BSD');
      Scanner.Defines.Append('UNIX');
    end else if s = 'SUNOS' then
    begin
      Scanner.Defines.Append('SOLARIS');
      Scanner.Defines.Append('UNIX');
    end else if s = 'GO32V2' then
      Scanner.Defines.Append('DPMI')
    else if s = 'BEOS' then
      Scanner.Defines.Append('UNIX')
    else if s = 'QNX' then
      Scanner.Defines.Append('UNIX');

    // TargetCPU
    s := UpperCase(CPUTarget);
    Scanner.Defines.Append('CPU'+s);
    if (s='x86_64') then
      Scanner.Defines.Append('CPU64')
    else
      Scanner.Defines.Append('CPU32');

    Parser := TPasParser.Create(Scanner, FileResolver, AEngine);
    Filename := '';

    if FPCCommandLine<>'' then
      begin
        Start := @FPCCommandLine[1];
        CurPos := Start;
        while CurPos[0] <> #0 do
        begin
          if CurPos[0] = ' ' then
          begin
            ProcessCmdLinePart;
            Start := CurPos + 1;
          end;
          Inc(CurPos);
        end;
        ProcessCmdLinePart;
      end;

    if Filename = '' then
      raise Exception.Create(SErrNoSourceGiven);

    Scanner.OpenFile(Filename);
    Parser.ParseMain(Result);
  finally
    Parser.Free;
    Scanner.Free;
    FileResolver.Free;
  end;
end;

procedure DoInit;
var
  c: Char;
begin
  for c:=low(char) to high(char) do
  begin
    IsIdentStart[c]:=c in ['a'..'z','A'..'Z','_'];
  end;
end;

initialization
  DoInit;

end.
