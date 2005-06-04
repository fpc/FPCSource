{
    This file is part of the Free Component Library

    Pascal source parser
    Copyright (c) 2000-2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit PParser;

interface

uses SysUtils, PasTree;

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
  SParserSyntaxError = 'Syntax error';
  SParserTypeSyntaxError = 'Syntax error in type';
  SParserArrayTypeSyntaxError = 'Syntax error in array type';
  SParserInterfaceTokenError = 'Invalid token in interface section of unit';
  SParserInvalidTypeDef = 'Invalid type definition';

type
  TPasTreeContainer = class
  protected
    FPackage: TPasPackage;
  public
    function CreateElement(AClass: TPTreeElement; const AName: string;
      AParent: TPasElement; const ASourceFilename: string;
      ASourceLinenumber: Integer): TPasElement;
    function CreateElement(AClass: TPTreeElement; const AName: string;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: string; ASourceLinenumber: Integer): TPasElement;
      virtual; abstract;
    function CreateFunctionType(const AName,resultname: string; AParent: TPasElement;
      UseParentAsResultParent: Boolean; const ASourceFilename: string;
      ASourceLinenumber: Integer): TPasFunctionType;
    function FindElement(const AName: string): TPasElement; virtual; abstract;
    function FindModule(const AName: string): TPasModule; virtual;
    property Package: TPasPackage read FPackage;
  end;

  EParserError = class(Exception)
  private
    FFilename: string;
    FRow, FColumn: Integer;
  public
    constructor Create(const AReason, AFilename: string;
      ARow, AColumn: Integer);
    property Filename: string read FFilename;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
  end;


function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: string): TPasModule;


implementation

uses Classes, PScanner;

type

  TDeclType = (declNone, declConst, declResourcestring, declType, declVar);

  Tproctype = (pt_procedure,pt_function,pt_operator);

  TPasParser = class
  private
    FFileResolver: TFileResolver;
    FScanner: TPascalScanner;
    FEngine: TPasTreeContainer;
    FCurToken: TToken;
    FCurTokenString: string;
    // UngetToken support:
    FTokenBuffer: array[0..1] of TToken;
    FTokenStringBuffer: array[0..1] of string;
    FTokenBufferIndex, FTokenBufferSize: Integer;

    procedure ParseExc(const Msg: string);
  protected
    function CreateElement(AClass: TPTreeElement; const AName: string;
      AParent: TPasElement): TPasElement;
    function CreateElement(AClass: TPTreeElement; const AName: string;
      AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;
  public
    constructor Create(AScanner: TPascalScanner; AFileResolver: TFileResolver;
      AEngine: TPasTreeContainer);
    function CurTokenName: string;
    function CurTokenText: string;
    procedure NextToken;
    procedure UngetToken;
    procedure ExpectToken(tk: TToken);
    function ExpectIdentifier: string;

    function ParseType(Parent: TPasElement; Prefix : string): TPasType;
    function ParseType(Parent: TPasElement): TPasType;
    function ParseComplexType: TPasType;
    procedure ParseArrayType(Element: TPasArrayType);
    function ParseExpression: string;
    procedure AddProcOrFunction(ASection: TPasSection; AProc: TPasProcedure);
    function CheckIfOverloaded(AOwner: TPasClassType;
      const AName: string): TPasElement;

    procedure ParseMain(var Module: TPasModule);
    procedure ParseUnit(var Module: TPasModule);
    procedure ParseUsesList(ASection: TPasSection);
    function ParseConstDecl(Parent: TPasElement): TPasConst;
    function ParseResourcestringDecl(Parent: TPasElement): TPasResString;
    function ParseTypeDecl(Parent: TPasElement): TPasType;
    procedure ParseInlineVarDecl(Parent: TPasElement; VarList: TList;
      AVisibility : TPasMemberVisibility);
    procedure ParseInlineVarDecl(Parent: TPasElement; VarList: TList);
    procedure ParseVarDecl(Parent: TPasElement; List: TList);
    procedure ParseArgList(Parent: TPasElement; Args: TList; EndToken: TToken);
    procedure ParseProcedureOrFunctionHeader(Parent: TPasElement;
      Element: TPasProcedureType; proctype:Tproctype; OfObjectPossible: Boolean);
    function ParseProcedureOrFunctionDecl(Parent: TPasElement;
      proctype:Tproctype): TPasProcedure;
    procedure ParseRecordDecl(Parent: TPasRecordType);
    function ParseClassDecl(Parent: TPasElement; const AClassName: string;
      AObjKind: TPasObjKind): TPasType;
    procedure ParseProperty(Element:TPasElement);

    property FileResolver: TFileResolver read FFileResolver;
    property Scanner: TPascalScanner read FScanner;
    property Engine: TPasTreeContainer read FEngine;

    property CurToken: TToken read FCurToken;
    property CurTokenString: string read FCurTokenString;
  end;


function TPasTreeContainer.CreateElement(AClass: TPTreeElement;
  const AName: string; AParent: TPasElement; const ASourceFilename: string;
  ASourceLinenumber: Integer): TPasElement;
begin
  Result := CreateElement(AClass, AName, AParent, visDefault, ASourceFilename,
    ASourceLinenumber);
end;

function TPasTreeContainer.CreateFunctionType(const AName,resultname: string;
  AParent: TPasElement; UseParentAsResultParent: Boolean;
  const ASourceFilename: string; ASourceLinenumber: Integer): TPasFunctionType;
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
    TPasResultElement(CreateElement(TPasResultElement, resultname, ResultParent,
    ASourceFilename, ASourceLinenumber));
end;

function TPasTreeContainer.FindModule(const AName: string): TPasModule;
begin
  Result := nil;
end;

constructor EParserError.Create(const AReason, AFilename: string;
  ARow, AColumn: Integer);
begin
  inherited Create(AReason);
  FFilename := AFilename;
  FRow := ARow;
  FColumn := AColumn;
end;

procedure TPasParser.ParseExc(const Msg: string);
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

function TPasParser.CurTokenName: string;
begin
  if CurToken = tkIdentifier then
    Result := 'Identifier ' + Scanner.CurTokenString
  else
    Result := TokenInfos[CurToken];
end;

function TPasParser.CurTokenText: string;
begin
  case CurToken of
    tkIdentifier, tkString, tkNumber, tkChar:
      Result := Scanner.CurTokenString;
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
  end;
end;

procedure TPasParser.UngetToken;

begin
  if FTokenBufferIndex = 0 then
    ParseExc(SParserUngetTokenError)
  else
    Dec(FTokenBufferIndex);
end;


procedure TPasParser.ExpectToken(tk: TToken);
begin
  NextToken;
  if CurToken <> tk then
    ParseExc(Format(SParserExpectTokenError, [TokenInfos[tk]]));
end;

function TPasParser.ExpectIdentifier: string;
begin
  ExpectToken(tkIdentifier);
  Result := CurTokenString;
end;

function TPasParser.ParseType(Parent: TPasElement): TPasType;

begin
  Result:=ParseType(Parent,'');
end;

function TPasParser.ParseType(Parent: TPasElement; Prefix : string): TPasType;

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
  Name, s: string;
  EnumValue: TPasEnumValue;
  Ref: TPasElement;
begin
  Result := nil;         // !!!: Remove in the future
  NextToken;
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
        else if s = 'string' then Name := 'string'
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
        if Name = 'string' then
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
    tkArray:
      begin
        Result := TPasArrayType(CreateElement(TPasArrayType, '', Parent));
        ParseArrayType(TPasArrayType(Result));
      end;
    tkBraceOpen:
      begin
        Result := TPasEnumType(CreateElement(TPasEnumType, '', Parent));
        while true do
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
        ExpectToken(tkOf);
        TPasSetType(Result).EnumType := ParseType(Result);
      end;
    tkRecord:
      begin
        Result := TPasRecordType(CreateElement(TPasRecordType, '', Parent));
        ParseRecordDecl(TPasRecordType(Result));
        UngetToken;
      end;
    tkProcedure:
      begin
        Result := TPasProcedureType(
          CreateElement(TPasProcedureType, '', Parent));
        try
          ParseProcedureOrFunctionHeader(Result,TPasProcedureType(Result),
                                         pt_procedure, true);
        except
          Result.Free;
          raise;
        end;
      end;
    tkFunction:
      begin
        Result := Engine.CreateFunctionType('','result', Parent, false,
          Scanner.CurFilename, Scanner.CurRow);
        try
          ParseProcedureOrFunctionHeader(Result,TPasFunctionType(Result),
                                         pt_function, true);
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

function TPasParser.ParseComplexType: TPasType;
begin
  NextToken;
  case CurToken of
    tkProcedure:
      begin
        Result := TPasProcedureType(CreateElement(TPasProcedureType, '', nil));
        ParseProcedureOrFunctionHeader(Result, TPasProcedureType(Result),
                                       pt_procedure, true);
        UngetToken;        // Unget semicolon
      end;
    tkFunction:
      begin
        Result := Engine.CreateFunctionType('','result', nil, false, Scanner.CurFilename,
          Scanner.CurRow);
        ParseProcedureOrFunctionHeader(Result, TPasFunctionType(Result),
                                       pt_function, true);
        UngetToken;        // Unget semicolon
      end;
    else
    begin
      UngetToken;
      Result := ParseType(nil);
      exit;
    end;
  end;
end;

procedure TPasParser.ParseArrayType(Element: TPasArrayType);

Var
  S : string;

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

function TPasParser.ParseExpression: string;
var
  BracketLevel: Integer;
  MayAppendSpace, AppendSpace, NextAppendSpace: Boolean;
begin
  SetLength(Result, 0);
  BracketLevel := 0;
  MayAppendSpace := false;
  AppendSpace := false;
  while true do
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
    end else if (CurToken in [tkComma, tkSemicolon, tkColon, tkSquaredBraceClose,
      tkDotDot]) and (BracketLevel = 0) then
      break;

    if MayAppendSpace then
    begin
      NextAppendSpace := false;
      case CurToken of
        tkBraceOpen, tkBraceClose, tkDivision, tkEqual, tkCaret, tkAnd, tkAs,
          tkDiv, tkIn, tkIs, tkMinus, tkMod, tkMul, tkNot, tkOf, tkOn,
          tkOr, tkPlus, tkSHL, tkSHR, tkXOR:
{        tkPlus.._ASSIGNMENT, _UNEQUAL, tkPlusASN.._XORASN, _AS, _AT, _IN, _IS,
          tkOf, _ON, _OR, _AND, _DIV, _MOD, _NOT, _SHL, _SHR, _XOR:}
          begin
            AppendSpace := true;
            NextAppendSpace := true;
          end;
      end;
      if AppendSpace then
        Result := Result + ' ';
      AppendSpace := NextAppendSpace;
    end else
      MayAppendSpace := true;
    if CurToken=tkString then
      begin
      If (Length(CurTokenText)>0) and (CurTokenText[1]=#0) then
        Writeln('First char is null : "',CurTokenText,'"');
      Result := Result + ''''+StringReplace(CurTokenText,'''','''''',[rfReplaceAll])+''''
      end
    else
      Result := Result + CurTokenText;
  end;
  UngetToken;
end;

procedure TPasParser.AddProcOrFunction(ASection: TPasSection;
  AProc: TPasProcedure);
var
  i: Integer;
  Member: TPasElement;
  OverloadedProc: TPasOverloadedProc;
begin
  for i := 0 to ASection.Functions.Count - 1 do
  begin
    Member := TPasElement(ASection.Functions[i]);
    if CompareText(Member.Name, AProc.Name) = 0 then
    begin
      if Member.ClassType = TPasOverloadedProc then
        TPasOverloadedProc(Member).Overloads.Add(AProc)
      else
      begin
        OverloadedProc := TPasOverloadedProc.Create(AProc.Name, ASection);
        OverloadedProc.Overloads.Add(Member);
        OverloadedProc.Overloads.Add(AProc);
        ASection.Functions[i] := OverloadedProc;
        ASection.Declarations[ASection.Declarations.IndexOf(Member)] :=
          OverloadedProc;
      end;
      exit;
    end;
  end;

  // Not overloaded, so just add the proc/function to the lists
  ASection.Declarations.Add(AProc);
  ASection.Functions.Add(AProc);
end;


// Returns the parent for an element which is to be created
function TPasParser.CheckIfOverloaded(AOwner: TPasClassType;
  const AName: string): TPasElement;
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
  NextToken;
  case CurToken of
    tkUnit: ParseUnit(Module);
    else
      ParseExc(Format(SParserExpectTokenError, ['unit']));
  end;
end;

// Starts after the "unit" token
procedure TPasParser.ParseUnit(var Module: TPasModule);
var
  CurBlock: TDeclType;
  Section: TPasSection;
  ConstEl: TPasConst;
  ResStrEl: TPasResString;
  TypeEl: TPasType;
  ClassEl: TPasClassType;
  List: TList;
  i,j: Integer;
  VarEl: TPasVariable;
begin
  Module := nil;
  Module := TPasModule(CreateElement(TPasModule, ExpectIdentifier,
    Engine.Package));
  if Assigned(Engine.Package) then
  begin
    Module.PackageName := Engine.Package.Name;
    Engine.Package.Modules.Add(Module);
  end;
  ExpectToken(tkSemicolon);
  ExpectToken(tkInterface);
  Section := TPasSection(CreateElement(TPasSection, '', Module));
  Module.InterfaceSection := Section;
  CurBlock := declNone;
  while true do
  begin
    NextToken;
    if CurToken = tkImplementation then
      break;
    case CurToken of
      tkUses:
        ParseUsesList(Section);
      tkConst:
        CurBlock := declConst;
      tkResourcestring:
        CurBlock := declResourcestring;
      tkType:
        CurBlock := declType;
      tkVar:
        CurBlock := declVar;
      tkProcedure:
        begin
          AddProcOrFunction(Section, ParseProcedureOrFunctionDecl(Section, pt_procedure));
          CurBlock := declNone;
        end;
      tkFunction:
        begin
          AddProcOrFunction(Section, ParseProcedureOrFunctionDecl(Section, pt_function));
          CurBlock := declNone;
        end;
      tkProperty:
        begin
          ExpectIdentifier;
          ParseProperty(CreateElement(TPasProperty, CurTokenString, Section));
        end;
      tkOperator:
        begin
          AddProcOrFunction(Section, ParseProcedureOrFunctionDecl(Section, pt_operator));
          CurBlock := declNone;
        end;
      tkIdentifier:
        begin
          case CurBlock of
            declConst:
              begin
                ConstEl := ParseConstDecl(Section);
                Section.Declarations.Add(ConstEl);
                Section.Consts.Add(ConstEl);
              end;
            declResourcestring:
              begin
                ResStrEl := ParseResourcestringDecl(Section);
                Section.Declarations.Add(ResStrEl);
                Section.ResStrings.Add(ResStrEl);
              end;
            declType:
              begin
                TypeEl := ParseTypeDecl(Section);
                if Assigned(TypeEl) then        // !!!
                begin
                  Section.Declarations.Add(TypeEl);
                  if TypeEl.ClassType = TPasClassType then
                  begin
                    // Remove previous forward declarations, if necessary
                    for i := 0 to Section.Classes.Count - 1 do
                    begin
                      ClassEl := TPasClassType(Section.Classes[i]);
                      if CompareText(ClassEl.Name, TypeEl.Name) = 0 then
                      begin
                        Section.Classes.Delete(i);
                        for j := 0 to Section.Declarations.Count - 1 do
                          if CompareText(TypeEl.Name,
                            TPasElement(Section.Declarations[j]).Name) = 0 then
                          begin
                            Section.Declarations.Delete(j);
                            break;
                          end;
                        ClassEl.Release;
                        break;
                      end;
                    end;
                    // Add the new class to the class list
                    Section.Classes.Add(TypeEl)
                  end else
                    Section.Types.Add(TypeEl);
                end;
              end;
            declVar:
              begin
                List := TList.Create;
                try
                  try
                    ParseVarDecl(Section, List);
                  except
                    for i := 0 to List.Count - 1 do
                      TPasVariable(List[i]).Release;
                    raise;
                  end;
                  for i := 0 to List.Count - 1 do
                  begin
                    VarEl := TPasVariable(List[i]);
                    Section.Declarations.Add(VarEl);
                    Section.Variables.Add(VarEl);
                  end;
                finally
                  List.Free;
                end;
              end;
          else
            ParseExc(SParserSyntaxError);
          end;
        end;
    else
      ParseExc(SParserInterfaceTokenError);
    end;
  end;
end;

// Starts after the "uses" token
procedure TPasParser.ParseUsesList(ASection: TPasSection);
var
  UnitName: string;
  Element: TPasElement;
begin
  while true do
  begin
    UnitName := ExpectIdentifier;

    Element := Engine.FindModule(UnitName);
    if Assigned(Element) then
      Element.AddRef
    else
      Element := TPasType(CreateElement(TPasUnresolvedTypeRef, UnitName,
        ASection));
    ASection.UsesList.Add(Element);

    NextToken;
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
    Result.Value := ParseExpression;
    ExpectToken(tkSemicolon);
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
    ExpectToken(tkSemicolon);
  except
    Result.Free;
    raise;
  end;
end;

// Starts after the type name
function TPasParser.ParseTypeDecl(Parent: TPasElement): TPasType;
var
  TypeName: string;

  procedure ParseRange;
  begin
    Result := TPasRangeType(CreateElement(TPasRangeType, TypeName, Parent));
    try
      TPasRangeType(Result).RangeStart := ParseExpression;
      ExpectToken(tkDotDot);
      TPasRangeType(Result).RangeEnd := ParseExpression;
      ExpectToken(tkSemicolon);
    except
      Result.Free;
      raise;
    end;
  end;

var
  EnumValue: TPasEnumValue;
  Prefix : string;
  HadPackedModifier : Boolean;           // 12/04/04 - Dave - Added

begin
  TypeName := CurTokenString;
  ExpectToken(tkEqual);
  NextToken;
  HadPackedModifier := false;     { Assume not present }
  if CurToken = tkPacked then     { If PACKED modifier }
     begin                        { Handle PACKED modifier for all situations }
     NextToken;                   { Move to next token for rest of parse }
     if CurToken in [tkArray, tkRecord, tkObject, tkClass] then  { If allowed }
       HadPackedModifier := true  { rememeber for later }
     else                         { otherwise, syntax error }
       ParseExc(Format(SParserExpectTokenError,['ARRAY, RECORD, OBJECT or CLASS']))
     end;
  case CurToken of
    tkRecord:
      begin
        Result := TPasRecordType(CreateElement(TPasRecordType, TypeName,
          Parent));
        try
          ParseRecordDecl(TPasRecordType(Result));
          TPasRecordType(Result).IsPacked := HadPackedModifier;
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
          ExpectToken(tkSemicolon);
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
        if CurToken = tkSemicolon then
        begin
          UngetToken;
          UngetToken;
          Result := TPasAliasType(CreateElement(TPasAliasType, TypeName,
            Parent));
          try
            TPasAliasType(Result).DestType := ParseType(nil,Prefix);
            ExpectToken(tkSemicolon);
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
            ExpectToken(tkSemicolon);
          except
            Result.Free;
            raise;
          end;
        end else
        begin
          UngetToken;
          UngetToken;
          ParseRange;
        end;
      end;
{    _string, _FILE:
      begin
        Result := TPasAliasType(CreateElement(TPasAliasType, TypeName, Parent));
        UngetToken;
        TPasAliasType(Result).DestType := ParseType(nil);
        ExpectToken(tkSemicolon);
      end;}
    tkArray:
      begin
        Result := TPasArrayType(CreateElement(TPasArrayType, TypeName, Parent));
        try
          ParseArrayType(TPasArrayType(Result));
          TPasArrayType(Result).IsPacked := HadPackedModifier;
          ExpectToken(tkSemicolon);
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
          ExpectToken(tkSemicolon);
        except
          Result.Free;
          raise;
        end;
      end;
    tkBraceOpen:
      begin
        Result := TPasEnumType(CreateElement(TPasEnumType, TypeName, Parent));
        try
          while true do
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
          ExpectToken(tkSemicolon);
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
          ParseProcedureOrFunctionHeader(Result,TPasProcedureType(Result),
                                         pt_procedure, true);
        except
          Result.Free;
          raise;
        end;
      end;
    tkFunction:
      begin
        Result := Engine.CreateFunctionType(TypeName, 'result', Parent, false,
          Scanner.CurFilename, Scanner.CurRow);
        try
          ParseProcedureOrFunctionHeader(Result, TPasFunctionType(Result),
                                         pt_function, true);
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
          ExpectToken(tkSemicolon);
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
  ParseInlineVarDecl(Parent, Varlist, visDefault);
end;

procedure TPasParser.ParseInlineVarDecl(Parent: TPasElement; VarList: TList;
  AVisibility: TPasMemberVisibility);
var
  VarNames: TStringList;
  i: Integer;
  VarType: TPasType;
  VarEl: TPasVariable;
begin
  VarNames := TStringList.Create;
  try
    while true do
    begin
      VarNames.Add(CurTokenString);
      NextToken;
      if CurToken = tkColon then
        break
      else if CurToken <> tkComma then
        ParseExc(SParserExpectedCommaColon);

      ExpectIdentifier;
    end;

    VarType := ParseComplexType;

    for i := 0 to VarNames.Count - 1 do
    begin
      VarEl := TPasVariable(CreateElement(TPasVariable, VarNames[i], Parent,
        AVisibility));
      VarEl.VarType := VarType;
      if i > 0 then
        VarType.AddRef;
      VarList.Add(VarEl);
    end;
    NextToken;
    // Records may be terminated with end, no semicolon
    If (CurToken<>tkEnd) and (CurToken<>tkSemicolon) then
      ParseExc(SParserExpectedSemiColonEnd)
  finally
    VarNames.Free;
  end;
end;

// Starts after the variable name
procedure TPasParser.ParseVarDecl(Parent: TPasElement; List: TList);
var
  i: Integer;
  VarType: TPasType;
  Value, S: string;
  M: string;
begin
  while true do
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
  NextToken;
  If CurToken=tkEqual then
    begin
    Value := ParseExpression;
    for i := 0 to List.Count - 1 do
      TPasVariable(List[i]).Value := Value;
    end
  else
    UngetToken;

  NextToken;
  if CurToken = tkAbsolute then
  begin
    // !!!: Store this information
    ExpectIdentifier;
  end else
    UngetToken;

  ExpectToken(tkSemicolon);
  M := '';
  while true do
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
          ExpectToken(tkSemicolon);
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
  Name, Value: string;
  i: Integer;
  Arg: TPasArgument;
  Access: TArgumentAccess;
  ArgType: TPasType;
begin
  while true do
  begin
    ArgNames := TStringList.Create;
    Access := argDefault;
    IsUntyped := false;
    ArgType := nil;
    while true do
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
        IsUntyped := true;
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
  Element: TPasProcedureType; proctype:Tproctype; OfObjectPossible: Boolean);

begin
  NextToken;
  case proctype of
    pt_function:
      begin
        if CurToken = tkBraceOpen then
        begin
          ParseArgList(Parent, Element.Args, tkBraceClose);
          ExpectToken(tkColon);
        end else if CurToken <> tkColon then
          ParseExc(SParserExpectedLBracketColon);
        if Assigned(Element) then        // !!!
          TPasFunctionType(Element).ResultEl.ResultType := ParseType(Parent)
        else
          ParseType(nil);
      end;
    pt_procedure:
      begin
        if CurToken = tkBraceOpen then
        begin
          ParseArgList(Element, Element.Args, tkBraceClose);
        end else if (CurToken = tkSemicolon) or (OfObjectPossible and (CurToken = tkOf)) then
          UngetToken
        else
          ParseExc(SParserExpectedLBracketSemicolon);
      end;
    pt_operator:
      begin
        ParseArgList(Parent, Element.Args, tkBraceClose);
        
        TPasFunctionType(Element).ResultEl.name := ExpectIdentifier;
            
        if CurToken <> tkColon then
          ParseExc(SParserExpectedLBracketColon);
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
    Element.IsOfObject := true;
  end else
    UngetToken;

  NextToken;
  if CurToken = tkEqual then begin
    // for example: const p: procedure = nil;
    UngetToken;
    exit;
  end else
    UngetToken;

  ExpectToken(tkSemicolon);

  while true do
  begin
    NextToken;
    if (CurToken = tkIdentifier) and (UpperCase(CurTokenString) = 'CDECL') then
    begin
{      El['calling-conv'] := 'cdecl';}
      ExpectToken(tkSemicolon);
    end else if (CurToken = tkIdentifier) and (UpperCase(CurTokenString) = 'STDCALL') then
    begin
{      El['calling-conv'] := 'stdcall';}
      ExpectToken(tkSemicolon);
    end else if (CurToken = tkIdentifier) and (UpperCase(CurTokenString) = 'DEPRECATED') then
    begin
{      El['calling-conv'] := 'cdecl';}
      ExpectToken(tkSemicolon);
    end else if (CurToken = tkIdentifier) and (UpperCase(CurTokenString) = 'EXTERNAL') then
    begin
      repeat
        NextToken
      until CurToken = tkSemicolon;
    end else if Parent.InheritsFrom(TPasProcedure) and
      (CurToken = tkIdentifier) and (UpperCase(CurTokenString) = 'OVERLOAD') then
    begin
      TPasProcedure(Parent).IsOverload := true;
      ExpectToken(tkSemicolon);
    end else
    begin
      UngetToken;
      break;
    end;
  end;
end;


procedure TPasParser.ParseProperty(Element:TPasElement);

  function GetAccessorName: string;
  begin
    ExpectIdentifier;
    Result := CurTokenString;
    while true do
    begin
      NextToken;
      if CurToken = tkDot then
      begin
        ExpectIdentifier;
        Result := Result + '.' + CurTokenString;
      end else
        break;
    end;
    UngetToken;
  end;

begin
  NextToken;
  // !!!: Parse array properties correctly
  if CurToken = tkSquaredBraceOpen then
  begin
  ParseArgList(Element, TPasProperty(Element).Args, tkSquaredBraceClose);
  NextToken;
  end;

  if CurToken = tkColon then
  begin
  // read property type
          TPasProperty(Element).VarType := ParseType(Element);
  NextToken;
  end;
  if CurToken <> tkSemicolon then
  begin
  // read 'index' access modifier
  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'INDEX') then
          TPasProperty(Element).IndexValue := ParseExpression
  else
          UngetToken;
  NextToken;
  end;
  if CurToken <> tkSemicolon then
  begin
  // read 'read' access modifier
  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'READ') then
          TPasProperty(Element).ReadAccessorName := GetAccessorName
  else
          UngetToken;
  NextToken;
  end;
  if CurToken <> tkSemicolon then
  begin
  // read 'write' access modifier
  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'WRITE') then
          TPasProperty(Element).WriteAccessorName := GetAccessorName
  else
          UngetToken;
  NextToken;
  end;
  if CurToken <> tkSemicolon then
  begin
  // read 'stored' access modifier
  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'STORED') then
  begin
          NextToken;
          if CurToken = tkTrue then
          TPasProperty(Element).StoredAccessorName := 'true'
          else if CurToken = tkFalse then
          TPasProperty(Element).StoredAccessorName := 'false'
          else if CurToken = tkIdentifier then
          TPasProperty(Element).StoredAccessorName := CurTokenString
          else
          ParseExc(SParserSyntaxError);
  end else
          UngetToken;
  NextToken;
  end;
  if CurToken <> tkSemicolon then
  begin
  // read 'default' value modifier
  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'DEFAULT') then
          TPasProperty(Element).DefaultValue := ParseExpression
  else
          UngetToken;
  NextToken;
  end;
  if CurToken <> tkSemicolon then
  begin
  // read 'nodefault' modifier
  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'NODEFAULT') then
  begin
          TPasProperty(Element).IsNodefault:=true;
  end;
  NextToken;
  end;
  if CurToken = tkSemicolon then
  begin
  // read semicolon
  NextToken;
  end;
  if (CurToken = tkIdentifier) and (UpperCase(CurTokenText) = 'DEFAULT') then
  begin
  NextToken;
  if CurToken = tkSemicolon then
  begin
  TPasProperty(Element).IsDefault := true;
  UngetToken;
  end else
  begin
          UngetToken;
          TPasProperty(Element).DefaultValue := ParseExpression;
  end;
  end else
  UngetToken;
end;


// Starts after the "procedure" or "function" token
function TPasParser.ParseProcedureOrFunctionDecl(Parent: TPasElement;
  proctype: Tproctype): TPasProcedure;
var
  Name: string;
begin
  case proctype of
    pt_function:
      begin
        Name := ExpectIdentifier;
        Result := TPasFunction(CreateElement(TPasFunction, Name, Parent));
        Result.ProcType := Engine.CreateFunctionType('', 'result', Result, true,
           Scanner.CurFilename, Scanner.CurRow);
      end;
    pt_procedure:
      begin
        Name := ExpectIdentifier;
        Result := TPasProcedure(CreateElement(TPasProcedure, Name, Parent));
        Result.ProcType := TPasProcedureType(CreateElement(TPasProcedureType, '',
           Result));
      end;
    pt_operator:
      begin
        name := tokeninfos[curtoken];
        Result := TPasOperator(CreateElement(TPasOperator, Name, Parent));
        Result.ProcType := Engine.CreateFunctionType('', '__INVALID__', Result, true,
           Scanner.CurFilename, Scanner.CurRow);
      end;
  end;

  ParseProcedureOrFunctionHeader(Result, Result.ProcType, proctype, false);
end;

// Starts after the "record" token

procedure TPasParser.ParseRecordDecl(Parent: TPasRecordType);

Var
  CCount : Integer;

begin
  while true do
  begin
    if CurToken = tkEnd then
      break;
    NextToken;
    if CurToken = tkEnd then
      break
    else if CurToken = tkCase then
      begin
      CCount:=0;
      Repeat
        NextToken;
        If CurToken=tkBraceOpen then
          inc(CCount)
        else If CurToken=tkBraceClose then
          Dec(CCount)
      until (CCount=0) and (CurToken=tkEnd);
      Break;
      end
    else
      ParseInlineVarDecl(Parent, Parent.Members);
    end;
  ExpectToken(tkSemicolon);
end;

// Starts after the "class" token
function TPasParser.ParseClassDecl(Parent: TPasElement;
  const AClassName: string; AObjKind: TPasObjKind): TPasType;
var
  CurVisibility: TPasMemberVisibility;
  pt: Tproctype;

  procedure ProcessMethod(const MethodTypeName: string; HasReturnValue: Boolean);
  var
    Owner: TPasElement;
    Proc: TPasProcedure;
    s: string;
  begin
    ExpectIdentifier;
    Owner := CheckIfOverloaded(TPasClassType(Result), CurTokenString);
    if HasReturnValue then
    begin
      Proc := TPasFunction(CreateElement(TPasFunction, CurTokenString, Owner,
        CurVisibility));
      Proc.ProcType := Engine.CreateFunctionType( '', 'result', Proc, true,
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

    if hasreturnvalue then
      pt:=pt_function
    else
      pt:=pt_procedure;

    ParseProcedureOrFunctionHeader(Proc, Proc.ProcType, pt, false);

    while true do
    begin
      NextToken;
      if CurToken = tkIdentifier then
      begin
        s := UpperCase(CurTokenString);
        if s = 'VIRTUAL' then
          Proc.IsVirtual := true
        else if s = 'DYNAMIC' then
          Proc.IsDynamic := true
        else if s = 'ABSTRACT' then
          Proc.IsAbstract := true
        else if s = 'OVERRIDE' then
          Proc.IsOverride := true
        else if s = 'OVERLOAD' then
          Proc.IsOverload := true
        else if s = 'MESSAGE' then
        begin
          Proc.IsMessage := true;
          repeat
            NextToken;
          until CurToken = tkSemicolon;
          UngetToken;
        end else if s = 'CDECL' then
{      El['calling-conv'] := 'cdecl';}
        else if s = 'STDCALL' then
{      El['calling-conv'] := 'stdcall';}
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
  s, SourceFilename: string;
  i, SourceLinenumber: Integer;
  VarList: TList;
  Element: TPasElement;
begin
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

    // Parse ancestor list
    if CurToken = tkBraceOpen then
    begin
      TPasClassType(Result).AncestorType := ParseType(nil);
      while true do
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
    end;

    if CurToken <> tkSemicolon then
    begin
      CurVisibility := visDefault;
      while CurToken <> tkEnd do
      begin
        case CurToken of
          tkIdentifier:
            begin
              s := LowerCase(CurTokenString);
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
                  ParseInlineVarDecl(Result, VarList, CurVisibility);
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
            end;
          tkProcedure:
            ProcessMethod('procedure', false);
          tkFunction:
            ProcessMethod('function', true);
          tkConstructor:
            ProcessMethod('constructor', false);
          tkDestructor:
            ProcessMethod('destructor', false);
          tkProperty:
            begin
              ExpectIdentifier;
              Element := CreateElement(TPasProperty, CurTokenString, Result, CurVisibility);
              TPasClassType(Result).Members.Add(Element);
              ParseProperty(Element);
            end;
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

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: string;
  AParent: TPasElement): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent,
    Scanner.CurFilename, Scanner.CurRow);
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: string;
  AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent, AVisibility,
    Scanner.CurFilename, Scanner.CurRow);
end;


function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: string): TPasModule;
var
  FileResolver: TFileResolver;
  Parser: TPasParser;
  Start, CurPos: PChar;
  Filename: string;
  Scanner: TPascalScanner;

  procedure ProcessCmdLinePart;
  var
    l: Integer;
    s: string;
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
        'd':
          Scanner.Defines.Append(UpperCase(Copy(s, 3, Length(s))));
        'F':
          if s[3] = 'i' then
            FileResolver.AddIncludePath(Copy(s, 4, Length(s)));
      end;
    end else
      if Filename <> '' then
        raise Exception.Create(SErrMultipleSourceFiles)
      else
        Filename := s;
  end;

var
  s: string;
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

end.
