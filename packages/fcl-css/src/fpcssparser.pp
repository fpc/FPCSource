{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022- by Michael Van Canneyt (michael@freepascal.org)

    This file contains a CSS parser

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpCSSParser;

{$mode ObjFPC}{$H+}

{ $DEFINE debugparser}

interface

uses
  TypInfo, Classes, SysUtils, fpcsstree, fpcssscanner;

Type
  ECSSParser = Class(Exception);
  { TCSSParser }

  TCSSParser = class(TObject)
  private
    FInput : TStream;
    FScanner: TCSSScanner;
    FPrevious : TCSSToken;
    FCurrent : TCSSToken;
    FCurrentTokenString : UTF8String;
    FPeekToken : TCSSToken;
    FPeekTokenString : UTF8String;
    FFreeScanner : Boolean;
    function CreateElement(aClass: TCSSElementClass): TCSSElement;
    class function GetAppendElement(aList: TCSSListElement): TCSSElement;
    function GetAtEOF: Boolean;
    function GetCurSource: UTF8String;
    Function GetCurLine : Integer;
    Function GetCurPos : Integer;
  protected
    Procedure DoError(Msg : UTF8String);
    Procedure DoError(Fmt : UTF8String; Args : Array of const);
    Procedure Consume(aToken : TCSSToken);
    function ParseComponentValueList(allowRules: Boolean=True): TCSSElement;
    function ParseComponentValue: TCSSElement;
    function ParseExpression: TCSSElement;
    function ParseRule(IsAt: Boolean=False): TCSSElement;
    function ParseAtRule: TCSSElement;
    function ParseRuleList(aStopOn : TCSStoken = ctkEOF): TCSSElement;
    function ParseSelector: TCSSElement;
    function ParseDeclaration(aIsAt : Boolean = false): TCSSDeclarationElement;
    function ParseCall(aName: UTF8String): TCSSElement;
    function ParseUnary: TCSSElement;
    function ParseUnit: TCSSUnits;
    function ParseIdentifier : TCSSElement;
    function ParseParenthesis: TCSSElement;
    function ParsePseudo: TCSSElement;
    Function ParseRuleBody(aRule: TCSSRuleElement; aIsAt : Boolean = False) : integer;
    function ParseInteger: TCSSElement;
    function ParseFloat: TCSSElement;
    function ParseString: TCSSElement;
    Function ParseUnicodeRange : TCSSElement;
    function ParseArray(aPrefix: TCSSElement): TCSSElement;
    function ParseURL: TCSSElement;
    Property CurrentSource : UTF8String Read GetCurSource;
    Property CurrentLine : Integer Read GetCurLine;
    Property CurrentPos : Integer Read GetCurPos;
  Public
    Constructor Create(AInput: TStream);
    Constructor Create(AScanner : TCSSScanner); virtual;
    Destructor Destroy; override;
    Function Parse : TCSSElement;
    Property CurrentToken : TCSSToken Read FCurrent;
    Property CurrentTokenString : UTF8String Read FCurrentTokenString;
    Function GetNextToken : TCSSToken;
    Function PeekNextToken : TCSSToken;
    Property Scanner : TCSSScanner Read FScanner;
    Property atEOF : Boolean Read GetAtEOF;
  end;

Function TokenToBinaryOperation(aToken : TCSSToken)  : TCSSBinaryOperation;
Function TokenToUnaryOperation(aToken : TCSSToken) : TCSSUnaryOperation;

implementation

Resourcestring
  SBinaryInvalidToken = 'Invalid token for binary operation: %s';
  SUnaryInvalidToken = 'Invalid token for unary operation: %s';
  SErrFileSource = 'Error: file "%s" line %d, pos %d: ';
  SErrSource = 'Error: line %d, pos %d: ';
  SErrUnexpectedToken = 'Unexpected token: Got %s (as UTF8String: "%s"), expected: %s ';
  SErrInvalidFloat = 'Invalid float: %s';
  SErrUnexpectedEndOfFile = 'Unexpected EOF while scanning function args: %s';

Function TokenToBinaryOperation(aToken : TCSSToken)  : TCSSBinaryOperation;

begin
  Case aToken of
    ctkEquals : Result:=boEquals;
    ctkPlus : Result:=boPlus;
    ctkMinus:  Result:=boMinus;
    ctkAnd : result:=boAnd;
    ctkLT : Result:=boLT;
    ctkDIV : Result:=boDIV;
    ctkStar : Result:=boSTAR;
    ctkTilde : Result:=boTilde;
    ctkColon : Result:=boCOLON;
    ctkDoubleColon : Result:=boDoubleColon;
    ctkSquared : Result:=boSquared;
    ctkGT : Result:=boGT;
  else
    Raise ECSSParser.CreateFmt(SBinaryInvalidToken,[GetEnumName(TypeInfo(aToken),Ord(aToken))]);
    // Result:=boEquals;
  end;
end;

Function TokenToUnaryOperation(aToken : TCSSToken) : TCSSUnaryOperation;

begin
  Case aToken of
    ctkDOUBLECOLON: Result:=uoDoubleColon;
    ctkMinus: Result:=uoMinus;
    ctkPlus: Result:=uoPlus;
    ctkDiv: Result:=uoDiv;
  else
    Raise ECSSParser.CreateFmt(SUnaryInvalidToken,[GetEnumName(TypeInfo(aToken),Ord(aToken))]);
  end;
end;

{ TCSSParser }

function TCSSParser.GetAtEOF: Boolean;
begin
  Result:=(CurrentToken=ctkEOF);
end;

procedure TCSSParser.DoError(Msg: UTF8String);
Var
  ErrAt : UTF8String;

begin
  If Assigned(FScanner) then
    If FScanner.CurFilename<>'' then
      ErrAt:=Format(SErrFileSource,[FScanner.CurFileName,FScanner.CurRow,FScanner.CurColumn])
    else
      ErrAt:=Format(SErrSource,[FScanner.Currow,FScanner.CurColumn]);
  Raise ECSSParser.Create(ErrAt+Msg)
end;

procedure TCSSParser.DoError(Fmt: UTF8String; Args: array of const);
begin
  DoError(Format(Fmt,Args));
end;

procedure TCSSParser.Consume(aToken: TCSSToken);
begin
  if CurrentToken<>aToken then
    DoError(SErrUnexpectedToken ,[
             GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
             CurrentTokenString,
             GetEnumName(TypeInfo(TCSSToken),Ord(aToken))
             ]);
  GetNextToken;
end;


function TCSSParser.GetCurSource: UTF8String;
begin
  If Assigned(FScanner) then
    Result:=FScanner.CurFileName
  else
    Result:='';
end;

function TCSSParser.GetCurLine: Integer;
begin
  if Assigned(FScanner) then
    Result:=FScanner.CurRow
  else
    Result:=0;
end;

function TCSSParser.GetCurPos: Integer;
begin
  if Assigned(FScanner) then
    Result:=FScanner.CurColumn
  else
    Result:=0;
end;

constructor TCSSParser.Create(AInput: TStream);
begin
  FInput:=AInput;
  Create(TCSSScanner.Create(FInput));
  FFreeScanner:=True;
end;

constructor TCSSParser.Create(AScanner: TCSSScanner);
begin
  FCurrent:=ctkUNKNOWN;
  FPeekToken:=ctkUNKNOWN;
  FPeekTokenString:='';
  FScanner:=aScanner;
end;

destructor TCSSParser.Destroy;
begin
  if FFreeScanner then
    FreeAndNil(FScanner);
  inherited Destroy;
end;

Class Function TCSSParser.GetAppendElement(aList : TCSSListElement) : TCSSElement;

begin
  Case aList.ChildCount of
    0 : Result:=Nil;
    1 : Result:=aList.ExtractElement(0);
  else
    Result:=aList;
  end;
  if Result<>aList then
    aList.Free;
end;

function TCSSParser.ParseAtRule: TCSSElement;

Var
  aRule : TCSSRuleElement;
  aSel : TCSSElement;
  Term : TCSSTokens;
  aLast : TCSSToken;
  aList : TCSSListElement;

begin
//  Writeln('Parse rule. IsAt:',IsAt);
  Term:=[ctkLBRACE,ctkEOF,ctkSEMICOLON];
  aRule:=TCSSAtRuleElement(CreateElement(TCSSAtRuleElement));
  TCSSAtRuleElement(aRule).AtKeyWord:=CurrentTokenString;
  Consume(ctkAtKeyWord);
  try
    aList:=TCSSListElement(CreateElement(TCSSListElement));
    While Not (CurrentToken in Term) do
      begin
      aSel:=ParseSelector;
      aList.AddChild(aSel);
      if CurrentToken=ctkCOMMA then
        begin
        Consume(ctkCOMMA);
        aRule.AddSelector(GetAppendElement(aList));
        aList:=TCSSListElement(CreateElement(TCSSListElement));
        end;
      end;
    aRule.AddSelector(GetAppendElement(aList));
    aLast:=CurrentToken;
    if (aLast<>ctkSEMICOLON) then
      begin
      Consume(ctkLBRACE);
      aRule.AddChild(ParseRuleList(ctkRBRACE));
      Consume(ctkRBRACE);
      end;
    Result:=aRule;
  except
    aRule.Free;
    Raise;
  end;

end;

function TCSSParser.ParseExpression: TCSSElement;

    Function AllowRules(aName : String) : Boolean;

    begin
      result:=sameText(aName,'@media') or sameText(aName,'@print');
    end;

Const
  RuleTokens =
       [ctkIDENTIFIER,ctkCLASSNAME,ctkHASH,ctkINTEGER,
        ctkDOUBLECOLON,ctkSTAR,ctkTILDE,ctkCOLON,ctkLBRACKET];

begin
  if CurrentToken in RuleTokens then
    Result:=ParseRule
  else if CurrentToken=ctkATKEYWORD then
    begin
    if AllowRules(CurrentTokenString) then
      Result:=ParseAtRule
    else
      Result:=ParseRule(True);
    end
  else
    Result:=ParseComponentValueList;
end;

function TCSSParser.ParseRuleList(aStopOn : TCSStoken = ctkEOF): TCSSElement;

Var
  aList : TCSSCompoundElement;
  aEl : TCSSElement;
  Terms : TCSSTokens;
begin
  Terms:=[ctkEOF];
  Include(Terms,aStopOn);
  aList:=TCSSCompoundElement(CreateElement(TCSSCompoundElement));
  Try
    While not (CurrentToken in Terms) do
      begin
      aEl:=ParseExpression;
      aList.AddChild(aEl);
      if CurrentToken=ctkSEMICOLON then
        Consume(ctkSEMICOLON);
      end;
    Result:=aList;
  except
    aList.Free;
    Raise;
  end;
end;

function TCSSParser.Parse: TCSSElement;
begin
  GetNextToken;
  if CurrentToken=ctkLBRACE then
    Result:=ParseRule
  else
    Result:=ParseRuleList;
end;


function TCSSParser.GetNextToken: TCSSToken;
begin
  FPrevious:=FCurrent;
  If (FPeekToken<>ctkUNKNOWN) then
     begin
     FCurrent:=FPeekToken;
     FCurrentTokenString:=FPeekTokenString;
     FPeekToken:=ctkUNKNOWN;
     FPeekTokenString:='';
     end
  else
    begin
    FCurrent:=FScanner.FetchToken;
    FCurrentTokenString:=FScanner.CurTokenString;
    end;
  Result:=FCurrent;
  {$ifdef debugparser}
     Writeln('GetNextToken returns ',
       GetEnumName(TypeInfo(TCSSToken),Ord(FCurrent)),
       '(String: "',FCurrentTokenString,'")',
       ' at (',FScanner.CurRow,',',FScanner.CurColumn,'): ',
       FSCanner.CurLine);
  {$endif debugparser}
end;

function TCSSParser.PeekNextToken: TCSSToken;
begin
  If (FPeekToken=ctkUNKNOWN) then
    begin
    FPeekToken:=FScanner.FetchToken;
    FPeekTokenString:=FScanner.CurTokenString;
    end;
  {$ifdef debugparser}Writeln('PeekNextToken : ',GetEnumName(TypeInfo(TCSSToken),Ord(FPeekToken)), ' As UTF8String: ',FPeekTokenString);{$endif debugparser}
  Result:=FPeekToken;
end;

function TCSSParser.ParseUnit : TCSSUnits;

begin
  Result:=cuNone;
  if (CurrentToken in [ctkIDENTIFIER,ctkPERCENTAGE]) then
    begin
    Case currentTokenString of
    '%'   : Result:=cuPERCENT;
    'px'  : Result:=cuPX;
    'rem' : Result:=cuREM;
    'em'  : Result:=cuEM;
    'fr'  : Result:=cuFR;
    'vw'  : Result:=cuVW;
    'vh'  : Result:=cuVH;
    'pt'  : Result:=cuPT;
    'deg' : Result:=cuDEG;
    else
      // Ignore. For instance margin: 0 auto
    end;
    if Result<>cuNone then
      Consume(CurrentToken);
    end;
end;

function TCSSParser.CreateElement(aClass : TCSSElementClass): TCSSElement;

begin
  Result:=aClass.Create(CurrentSource,CurrentLine,CurrentPos);
end;

function TCSSParser.ParseIdentifier: TCSSElement;

Var
  aValue : UTF8String;
  aId : TCSSIdentifierElement;

begin
  aValue:=CurrentTokenString;
  if CurrentToken=ctkCLASSNAME then
    aId:=TCSSClassNameElement(CreateElement(TCSSClassNameElement))
  else
    aId:=TCSSIdentifierElement(CreateElement(TCSSIdentifierElement));
  try
    Consume(CurrentToken);
    aId.Value:=aValue;
    Result:=aId;
  except
    aId.Free;
    Raise;
  end;
end;

function TCSSParser.ParseInteger: TCSSElement;

Var
  aValue : Integer;
  aInt : TCSSIntegerElement;

begin
  aValue:=StrToInt(CurrentTokenString);
  aInt:=TCSSIntegerElement(CreateElement(TCSSIntegerElement));
  try
    aInt.Value:=aValue;
    Consume(ctkINTEGER);
    aInt.Units:=ParseUnit;
    result:=aInt;
  except
    aInt.Free;
    Raise;
  end;
end;

function TCSSParser.ParseFloat: TCSSElement;
Var
  aCode : Integer;
  aValue : Double;
  aFloat : TCSSFloatElement;

begin
  Val(CurrentTokenString,aValue,aCode);
  if aCode<>0 then

    DoError(SErrInvalidFloat,[CurrentTokenString]);
  aFloat:=TCSSFloatElement(CreateElement(TCSSFloatElement));
  try
    Consume(ctkFloat);
    aFloat.Value:=aValue;
    aFloat.Units:=ParseUnit;
    result:=aFloat;
  except
    aFloat.Free;
    Raise;
  end;
end;


function TCSSParser.ParseParenthesis: TCSSElement;

begin
  Consume(ctkLPARENTHESIS);
  Result:=ParseComponentValueList;
  try
    Consume(ctkRPARENTHESIS);
  except
    Result.Free;
    Raise;
  end;
end;

function TCSSParser.ParseURL: TCSSElement;

Var
  aURL : TCSSURLElement;

begin
  aURL:=TCSSURLElement(CreateElement(TCSSURLElement));
  try
    aURL.Value:=CurrentTokenString;
    if CurrentToken=ctkURL then
      consume(ctkURL)
    else
      consume(ctkBADURL);
     Result:=aURL;
  except
    aURL.Free;
    Raise;
  end;
end;

function TCSSParser.ParsePseudo: TCSSElement;

Var
  aPseudo : TCSSIdentifierElement;
  aValue : string;

begin
  aValue:=CurrentTokenString;
  aPseudo:=TCSSIdentifierElement(CreateElement(TCSSIdentifierElement));
  try
    Consume(ctkPseudo);
    aPseudo.Value:=aValue;
    Result:=aPseudo;
  except
    aPseudo.Free;
    Raise;
  end;
end;

function TCSSParser.ParseRuleBody(aRule: TCSSRuleElement; aIsAt: Boolean = false): integer;

Var
  aDecl : TCSSDeclarationElement;

begin
  if not (CurrentToken in [ctkRBRACE,ctkSEMICOLON]) then
    begin
    aDecl:=ParseDeclaration(aIsAt) as TCSSDeclarationElement;
    if Assigned(aDecl) then
      aRule.AddChild(aDecl);
    end;
  While Not (CurrentToken in [ctkEOF,ctkRBRACE]) do
    begin
    if aDecl.Colon then
      While CurrentToken=ctkSEMICOLON do
        Consume(ctkSEMICOLON);
    if Not (CurrentToken in [ctkEOF,ctkRBRACE]) then
      begin
      aDecl:=ParseDeclaration(aIsAt);
      if Assigned(aDecl) then
        aRule.AddChild(aDecl);
      end;
    end;
  Result:=aRule.ChildCount;
end;

function TCSSParser.ParseRule(IsAt : Boolean = False): TCSSElement;

Var
  aRule : TCSSRuleElement;
  aSel : TCSSElement;
  Term : TCSSTokens;
  aLast : TCSSToken;
  aList: TCSSListElement;

begin
//  Writeln('Parse rule. IsAt:',IsAt);
  Term:=[ctkLBRACE,ctkEOF,ctkSEMICOLON];
  if IsAt then
    begin
    aRule:=TCSSAtRuleElement(CreateElement(TCSSAtRuleElement));
    TCSSAtRuleElement(aRule).AtKeyWord:=CurrentTokenString;
    Consume(ctkATKEYWORD);
    end
  else
    aRule:=TCSSRuleElement(CreateElement(TCSSRuleElement));
  try
    aList:=TCSSListElement(CreateElement(TCSSListElement));
    While Not (CurrentToken in Term) do
      begin
      aSel:=ParseSelector;
      aRule.AddSelector(aSel);
      if CurrentToken=ctkCOMMA then
        begin
        Consume(ctkCOMMA);
        aRule.AddSelector(GetAppendElement(aList));
        aList:=TCSSListElement(CreateElement(TCSSListElement));
        end;
      end;
    aRule.AddSelector(GetAppendElement(aList));
    aLast:=CurrentToken;
    if (aLast<>ctkSEMICOLON) then
      begin
      Consume(ctkLBrace);
      ParseRuleBody(aRule,IsAt);
      // Writeln('Parsed rule');
      Consume(ctkRBRACE);
      end;
    Result:=aRule;
  except
    aRule.Free;
    Raise;
  end;
end;

function TCSSParser.ParseUnary: TCSSElement;

var
  Un : TCSSUnaryElement;
  Op : TCSSUnaryOperation;

begin
  Result:=nil;
  if not (CurrentToken in [ctkDOUBLECOLON, ctkMinus, ctkPlus, ctkDiv]) then
    Raise ECSSParser.CreateFmt(SUnaryInvalidToken,[CurrentTokenString]);
  Un:=TCSSUnaryElement(CreateElement(TCSSUnaryElement));
  try
    op:=TokenToUnaryOperation(CurrentToken);
    Consume(CurrentToken);
    Un.Operation:=op;
    Un.Right:=ParseComponentValue;
    Result:=un;
  except
    Un.Free;
    Raise;
  end;
end;

function TCSSParser.ParseComponentValueList(allowRules : Boolean = True): TCSSElement;

Const
  TermSeps = [ctkEquals,ctkPlus,ctkMinus,ctkAnd,ctkLT,ctkDIV,
              ctkStar,ctkTilde,ctkColon, ctkDoubleColon,
              ctkSquared,ctkGT];


  function DoBinary(var aLeft : TCSSElement) : TCSSElement;
  var
    Bin : TCSSBinaryElement;
  begin
    Bin:=TCSSBinaryElement(CreateElement(TCSSBinaryElement));
    try
      Bin.Left:=ALeft;
      aLeft:=Nil;
      Bin.Operation:=TokenToBinaryOperation(CurrentToken);
      Consume(CurrentToken);
      Bin.Right:=ParseComponentValue;
      Result:=Bin;
    except
      Bin.Free;
      Raise;
    end;
  end;

begin
  Result:=Nil;
  if not AllowRules then
    Result:=ParseComponentValue
  else
    Case CurrentToken of
      ctkLBRACE : Result:=ParseRule();
      ctkATKEYWORD : Result:=ParseRule(True);
    else
      Result:=ParseComponentValue;
    end;
  If Not Assigned(Result) then
    exit;
  try
    While CurrentToken in TermSeps do
      Result:=DoBinary(Result);
  except
    Result.Free;
    Raise;
  end;
end;


function TCSSParser.ParseComponentValue: TCSSElement;

Const
  FinalTokens =
     [ctkLPARENTHESIS,ctkURL,ctkColon,ctkLBRACE, ctkLBRACKET,
      ctkDOUBLECOLON,ctkMinus,ctkPlus,ctkDiv,ctkSTAR,ctkTILDE];

var
  aToken : TCSSToken;

begin
  aToken:=CurrentToken;
  Case aToken of
    ctkLPARENTHESIS: Result:=ParseParenthesis;
    ctkURL: Result:=ParseURL;
    ctkPSEUDO: Result:=ParsePseudo;
    ctkLBRACE: Result:=ParseRule;
    ctkLBRACKET: Result:=ParseArray(Nil);
    ctkMinus,
    ctkPlus,
    ctkDiv: Result:=ParseUnary;
    ctkUnicodeRange: Result:=ParseUnicodeRange;
    ctkSTRING,
    ctkHASH : Result:=ParseString;
    ctkINTEGER: Result:=ParseInteger;
    ctkFloat : Result:=ParseFloat;
    ctkPSEUDOFUNCTION,
    ctkFUNCTION : Result:=ParseCall('');
    ctkSTAR,
    ctkTILDE,
    ctkIDENTIFIER,
    ctkCLASSNAME : Result:=ParseIdentifier;
  else
    Result:=nil;
//    Consume(aToken);// continue
  end;
  if aToken in FinalTokens then
    exit;
  if (CurrentToken=ctkLBRACKET) then
    Result:=ParseArray(Result);
end;

function TCSSParser.ParseSelector: TCSSElement;

begin
  Result:=ParseComponentValueList(false);
end;

function TCSSParser.ParseDeclaration(aIsAt: Boolean = false): TCSSDeclarationElement;

Var
  aDecl : TCSSDeclarationElement;
  aKey,aValue : TCSSElement;
  aPrevDisablePseudo : Boolean;
  aList : TCSSListElement;

begin
  aDecl:= TCSSDeclarationElement(CreateElement(TCSSDeclarationElement));
  try
    aPrevDisablePseudo:= Scanner.DisablePseudo;
    Scanner.DisablePseudo:=True;
    aKey:=ParseComponentValue;
    aDecl.AddKey(aKey);
    if aIsAt then
      begin
      While (CurrentToken=ctkCOMMA) do
        begin
        while (CurrentToken=ctkCOMMA) do
          consume(ctkCOMMA);
        aKey:=ParseComponentValue;
        aDecl.AddKey(aKey);
        end;
      end;
    if Not aIsAt then
      begin
      aDecl.Colon:=True;
      consume(ctkCOLON);
      end
    else
      begin
      aDecl.Colon:=CurrentToken=ctkColon;
      if aDecl.Colon then
        Consume(ctkColon)
      end;
    Scanner.DisablePseudo:=aPrevDisablePseudo;
    aValue:=ParseComponentValue;
    aList:=TCSSListElement(CreateElement(TCSSListElement));
    aList.AddChild(aValue);
    if aDecl.Colon then
      begin
      While not (CurrentToken in [ctkSemicolon,ctkRBRACE,ctkImportant]) do
        begin
        While CurrentToken=ctkCOMMA do
          begin
          Consume(ctkCOMMA);
          aDecl.AddChild(GetAppendElement(aList));
          aList:=TCSSListElement(CreateElement(TCSSListElement));
          end;
        aValue:=ParseComponentValue;
        aList.AddChild(aValue);
        end;
      if CurrentToken=ctkImportant then
        begin
        consume(ctkImportant);
        aDecl.IsImportant:=True;
        end;
      end;
    aDecl.AddChild(GetAppendElement(aList));
    Result:=aDecl;
  except
    Scanner.DisablePseudo:=False;
    aDecl.Free;
    Raise;
  end;
end;

function TCSSParser.ParseCall(aName : UTF8String): TCSSElement;

var
  aCall : TCSSCallElement;
  l : Integer;
begin
  aCall:=TCSSCallElement(CreateELement(TCSSCallElement));
  try
    if (aName='') then
      aName:=CurrentTokenString;
    L:=Length(aName);
    if (L>0) and (aName[L]='(') then
      aName:=Copy(aName,1,L-1);
    if CurrentToken=ctkPSEUDOFUNCTION then
      Consume(ctkPSEUDOFUNCTION)
    else
      Consume(ctkFUNCTION);
    While not (CurrentToken in [ctkRPARENTHESIS,ctkEOF]) do
      begin
      aCall.AddArg(ParseComponentValueList);
      if (CurrentToken=ctkCOMMA) then
        Consume(ctkCOMMA);
      end;
    if CurrentToken=ctkEOF then
      DoError(SErrUnexpectedEndOfFile,[aName]);
    Consume(ctkRPARENTHESIS);
    // Call argument list can be empty: mask()
    Result:=aCall;
  except
    aCall.Free;
    Raise;
  end;
end;


function TCSSParser.ParseString: TCSSElement;

Var
  aValue : UTF8String;
  aEl : TCSSElement;
  aStr : TCSSStringElement;

begin
  aValue:=CurrentTokenString;
  aStr:=TCSSStringElement(CreateElement(TCSSStringElement));
  try
    if CurrentToken=ctkSTRING then
      Consume(ctkSTRING)
    else
      Consume(ctkHASH);
    aStr.Value:=aValue;
    While (CurrentToken in [ctkIDENTIFIER,ctkSTRING,ctkINTEGER,ctkFLOAT,ctkHASH]) do
      begin
      aEl:=ParseComponentValue;
      aStr.Children.Add(aEl);
      end;
    Result:=aStr;
  except
    aStr.Free;
    Raise;
  end;
end;

function TCSSParser.ParseUnicodeRange: TCSSElement;
Var
  aValue : String;
  aRange : TCSSUnicodeRangeElement;

begin
  aValue:=CurrentTokenString;
  aRange:=TCSSUnicodeRangeElement(CreateElement(TCSSUnicodeRangeElement));
  try
    Consume(ctkUnicodeRange);
    aRange.Value:=aValue;
    result:=aRange;
  except
    aRange.Free;
    Raise;
  end;

end;

function TCSSParser.ParseArray(aPrefix: TCSSElement): TCSSElement;

Var
  aEl : TCSSElement;
  aArray : TCSSArrayElement;

begin
  Result:=Nil;
  aArray:=TCSSArrayElement(CreateElement(TCSSArrayElement));
  try
    aArray.Prefix:=aPrefix;
    Consume(ctkLBRACKET);
    While CurrentToken<>ctkRBRACKET do
      begin
      aEl:=ParseComponentValueList;
      aArray.AddChild(aEl);
      end;
    Consume(ctkRBRACKET);
    Result:=aArray;
  except
    aArray.Free;
    Raise;
  end;
end;




end.

