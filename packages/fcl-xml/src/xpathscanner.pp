{
    This file is part of the Free Component Library

    Lexical scanner for the XML Path Language (XPath), usable without DOM.
    Copyright (c) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

{$IFNDEF FPC_DOTTEDUNITS}
unit XPathScanner;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, Xml.Utils;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils, xmlutils;
{$ENDIF FPC_DOTTEDUNITS}

resourcestring
  SScannerUnclosedString = 'String literal was not closed';
  SScannerInvalidChar = 'Invalid character';
  SScannerMalformedQName = 'Expected "*" or local part after colon';
  SScannerExpectedVarName = 'Expected variable name after "$"';

type

  TXPathToken = (               // [28] - [38]
    tkInvalid,
    tkEndOfStream,
    tkIdentifier,
    tkNSNameTest,               // NCName:*
    tkString,
    tkNumber,
    tkVariable,                 // $QName
    tkLeftBracket,              // "("
    tkRightBracket,             // ")"
    tkAsterisk,                 // "*"
    tkPlus,                     // "+"
    tkComma,                    // ","
    tkMinus,                    // "-"
    tkDot,                      // "."
    tkDotDot,                   // ".."
    tkSlash,                    // "/"
    tkSlashSlash,               // "//"
    tkColonColon,               // "::"
    tkLess,                     // "<"
    tkLessEqual,                // "<="
    tkEqual,                    // "="
    tkNotEqual,                 // "!="
    tkGreater,                  // ">"
    tkGreaterEqual,             // ">="
    tkAt,                       // "@"
    tkLeftSquareBracket,        // "["
    tkRightSquareBracket,       // "]"
    tkPipe                      // "|"
  );

  TXPathKeyword = (
    // axis names
    xkNone, xkAncestor,  xkAncestorOrSelf,  xkAttribute,  xkChild,
    xkDescendant, xkDescendantOrSelf, xkFollowing, xkFollowingSibling,
    xkNamespace, xkParent, xkPreceding, xkPrecedingSibling, xkSelf,
    // node tests
    xkComment, xkText, xkProcessingInstruction, xkNode,
    // operators
    xkAnd, xkOr, xkDiv, xkMod,
    // standard functions
    xkLast, xkPosition, xkCount, xkId, xkLocalName, xkNamespaceUri,
    xkName, xkString, xkConcat, xkStartsWith, xkContains,
    xkSubstringBefore, xkSubstringAfter, xkSubstring,
    xkStringLength, xkNormalizeSpace, xkTranslate, xkBoolean,
    xkNot, xkTrue, xkFalse, xkLang, xkNumber, xkSum, xkFloor,
    xkCeiling, xkRound
  );

  // Base class of all XPath errors
  EXPathError = class(Exception);
  // Invalid character or malformed token in an expression
  EXPathLexerError = class(EXPathError);
  // Expression that does not follow the XPath grammar
  EXPathParserError = class(EXPathError);
  // Error while evaluating an expression
  EXPathEvaluationError = class(EXPathError);

  { TXPathLexer }

  TXPathLexer = class
  private
    FExpression: XMLString;
    FCurData: PXMLChar;
    FCurToken: TXPathToken;
    FCurTokenString: XMLString;
    FTokenStart: PXMLChar;
    FTokenLength: Integer;
    function GetToken: TXPathToken;
    function ScanQName: Boolean;
  protected
    FPrefixLength: Integer;
    FTokenId: TXPathKeyword;
    // Raise EXPathLexerError with aMsg
    procedure Error(const aMsg: String);
  public
    // Create a lexer for aExpressionString and read its first token
    constructor Create(const aExpressionString: XMLString);
    // Read the next token and make it the current token
    function NextToken: TXPathToken;
    // Return the next token without consuming it
    function PeekToken: TXPathToken;
    // Read the next token if the current token is aToken; True if it was
    function SkipToken(aToken: TXPathToken): Boolean;
    // The current token
    property CurToken: TXPathToken read FCurToken;
    // Text of the current identifier, name test prefix, number, string or variable name
    property CurTokenString: XMLString read FCurTokenString;
    // Keyword matching the current tkIdentifier token, xkNone otherwise
    property CurKeyword: TXPathKeyword read FTokenId;
    // Length of the prefix of the last scanned QName, 0 when it has none
    property PrefixLength: Integer read FPrefixLength;
  end;

// Keyword whose text is the aLen characters at p, xkNone if there is none
function LookupXPathKeyword(p: PWideChar; Len: Integer): TXPathKeyword;
// Keyword whose text is aName, xkNone if there is none
function LookupXPathKeyword(const aName: XMLString): TXPathKeyword;
// Text of aKeyword, empty for xkNone
function XPathKeywordName(aKeyword: TXPathKeyword): XMLString;

implementation

{$i xpathkw.inc}


function LookupXPathKeyword(const aName: XMLString): TXPathKeyword;

begin
  Result:=LookupXPathKeyword(PWideChar(aName),Length(aName));
end;


function XPathKeywordName(aKeyword: TXPathKeyword): XMLString;

var
  lText: PWideChar;

begin
  lText:=XPathKeywords[aKeyword];
  if lText^=#0 then
    Result:=''
  else
    SetString(Result,lText+1,Ord(lText^));
end;


constructor TXPathLexer.Create(const aExpressionString: XMLString);

begin
  inherited Create;
  FExpression:=aExpressionString;
  FCurData:=PXMLChar(FExpression);
  NextToken;
end;


function TXPathLexer.PeekToken: TXPathToken;

var
  lSave: PXMLChar;

begin
  lSave:=FCurData;
  Result:=GetToken;
  FCurData:=lSave;
end;


function TXPathLexer.NextToken: TXPathToken;

begin
  Result:=GetToken;
  FCurToken:=Result;
  if Result in [tkIdentifier, tkNSNameTest, tkNumber, tkString, tkVariable] then
    SetString(FCurTokenString,FTokenStart,FTokenLength);
  if Result=tkIdentifier then
    FTokenId:=LookupXPathKeyword(FTokenStart,FTokenLength)
  else
    FTokenId:=xkNone;
end;


function TXPathLexer.SkipToken(aToken: TXPathToken): Boolean;

begin
  Result:=(FCurToken=aToken);
  if Result then
    NextToken;
end;


function TXPathLexer.ScanQName: Boolean;

var
  lP: PXMLChar;

begin
  FPrefixLength:=0;
  lP:=FCurData;
  repeat
    if (Byte(lP^) in NamingBitmap[NamePages[hi(Word(lP^))]]) then
      Inc(lP)
    else
    begin
      // bad first character, or a colon not followed by a NameStartChar
      Result:=False;
      Break;
    end;

    while Byte(lP^) in NamingBitmap[NamePages[$100+hi(Word(lP^))]] do
      Inc(lP);

    Result:=True;
    if (lP^<>':') or (lP[1]=':') or (FPrefixLength>0) then
      Break;
    FPrefixLength:=lP-FTokenStart;
    Inc(lP);
  until False;
  FCurData:=lP;
  FTokenLength:=lP-FTokenStart;
end;


function TXPathLexer.GetToken: TXPathToken;

  procedure GetNumber(aHasDot: Boolean);

  begin
    FTokenLength:=1;
    while ((FCurData[1]>='0') and (FCurData[1]<='9')) or ((FCurData[1]='.') and not aHasDot) do
    begin
      Inc(FCurData);
      Inc(FTokenLength);
      if FCurData[0]='.' then
        aHasDot:=True;
    end;
    Result:=tkNumber;
  end;

var
  lDelim: WideChar;

begin
  while (FCurData[0]<#255) and (AnsiChar(ord(FCurData[0])) in [#9, #10, #13, ' ']) do
    Inc(FCurData);

  FTokenStart:=FCurData;
  FTokenLength:=0;
  Result:=tkInvalid;

  case FCurData[0] of
    #0:
      Result:=tkEndOfStream;
    '!':
      if FCurData[1]='=' then
      begin
        Inc(FCurData);
        Result:=tkNotEqual;
      end;
    '"', '''':
      begin
        lDelim:=FCurData^;
        Inc(FCurData);
        FTokenStart:=FCurData;
        while FCurData[0]<>lDelim do
        begin
          if FCurData[0]=#0 then
            Error(SScannerUnclosedString);
          Inc(FCurData);
        end;
        FTokenLength:=FCurData-FTokenStart;
        Result:=tkString;
      end;
    '$':
      begin
        Inc(FCurData);
        Inc(FTokenStart);
        if ScanQName then
          Result:=tkVariable
        else
          Error(SScannerExpectedVarName);
        Exit;
      end;
    '(':
      Result:=tkLeftBracket;
    ')':
      Result:=tkRightBracket;
    '*':
      Result:=tkAsterisk;
    '+':
      Result:=tkPlus;
    ',':
      Result:=tkComma;
    '-':
      Result:=tkMinus;
    '.':
      if FCurData[1]='.' then
      begin
        Inc(FCurData);
        Result:=tkDotDot;
      end else if (FCurData[1]>='0') and (FCurData[1]<='9') then
        GetNumber(True)
      else
        Result:=tkDot;
    '/':
      if FCurData[1]='/' then
      begin
        Inc(FCurData);
        Result:=tkSlashSlash;
      end else
        Result:=tkSlash;
    '0'..'9':
      GetNumber(False);
    ':':
      if FCurData[1]=':' then
      begin
        Inc(FCurData);
        Result:=tkColonColon;
      end;
    '<':
      if FCurData[1]='=' then
      begin
        Inc(FCurData);
        Result:=tkLessEqual;
      end else
        Result:=tkLess;
    '=':
      Result:=tkEqual;
    '>':
      if FCurData[1]='=' then
      begin
        Inc(FCurData);
        Result:=tkGreaterEqual;
      end else
        Result:=tkGreater;
    '@':
      Result:=tkAt;
    '[':
      Result:=tkLeftSquareBracket;
    ']':
      Result:=tkRightSquareBracket;
    '|':
      Result:=tkPipe;
  else
    if ScanQName then
    begin
      Result:=tkIdentifier;
      Exit;
    end
    else if FPrefixLength>0 then
    begin
      if FCurData^='*' then
      begin
        Inc(FCurData);
        Dec(FTokenLength);        // exclude ':'
        Result:=tkNSNameTest;
        Exit;
      end
      else
        Error(SScannerMalformedQName);
    end;
  end;

  if Result=tkInvalid then
    Error(SScannerInvalidChar);
  if Result>tkEndOfStream then
    Inc(FCurData);
end;


procedure TXPathLexer.Error(const aMsg: String);

begin
  raise EXPathLexerError.Create(aMsg) at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;


end.
