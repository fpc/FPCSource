{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt (michael@freepascal.org)

    EBNF grammar Parser

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ebnf.parser;

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.TypInfo,
  {$ELSE}
  sysutils, typinfo,
  {$ENDIF}
  ebnf.tree, ebnf.scanner;

type
  EEBNFParser = class(Exception);

  { TEBNFParser }

  TEBNFParser = class
  private
    FScanner: TEBNFScanner;
    FLastLine: integer;
    FCurrentToken: TToken;
    FSource: string;
  protected
    procedure Consume(aTokenType: TEBNFTokenType);
    function ParseGrammar: TEBNFGrammar;
    function ParseRule: TEBNFRule;
    function ParseExpression: TEBNFExpression;
    function ParseTerm: TEBNFTerm;
    function ParseFactor: TEBNFFactor;
    function Peek(aTokenType: TEBNFTokenType): Boolean;
    function Peek(aTokenTypes: TEBNFTokenTypes): Boolean;
    procedure Error(const aMessage: string);
  public
    constructor Create(const aEBNFSource: string);
    destructor Destroy; override;
    function Parse: TEBNFGrammar;
  end;

implementation

resourcestring
  SErrParser = 'Parsing Error at pos %s (Token: %s, Value: "%s"): %s';
  SExpectedFound = 'Expected %s, but found %s';
  SErrExpectedIdentifierForRule = 'Expected identifier for rule name';
  SerrExpectedStringLiteral = 'Expected string literal for special sequence value';
  SerrUnexpectedFactorToken = 'Unexpected token for factor: %s';

constructor TEBNFParser.Create(const aEBNFSource: string);
begin
  FSource := aEBNFSource;
  FScanner := TEBNFScanner.Create(FSource);
  FCurrentToken := FScanner.GetNextToken;
end;

destructor TEBNFParser.Destroy;
begin
  FScanner.Free;
  inherited;
end;

procedure TEBNFParser.Error(const aMessage: string);
begin
  raise EEBNFParser.CreateFmt(SErrParser,
    [FCurrentToken.Position.ToString, GetEnumName(TypeInfo(TEBNFTokenType), Ord(FCurrentToken.TokenType)), FCurrentToken.Value, aMessage]);
end;

procedure TEBNFParser.Consume(aTokenType: TEBNFTokenType);
begin
  if FCurrentToken.TokenType = aTokenType then
    begin
    FLastLine:=FCurrentToken.Position.Row;
    FCurrentToken := FScanner.GetNextToken;
    end
  else
    Error(Format(SExpectedFound,
      [GetEnumName(TypeInfo(TEBNFTokenType), Ord(aTokenType)),
       GetEnumName(TypeInfo(TEBNFTokenType), Ord(FCurrentToken.TokenType))]));
end;

function TEBNFParser.Peek(aTokenType: TEBNFTokenType): Boolean;
begin
  Result := FCurrentToken.TokenType = aTokenType;
end;

function TEBNFParser.Peek(aTokenTypes: TEBNFTokenTypes): Boolean;
begin
  Result := FCurrentToken.TokenType in aTokenTypes;
end;

function TEBNFParser.Parse: TEBNFGrammar;
begin
  Result := ParseGrammar;
end;

function TEBNFParser.ParseGrammar: TEBNFGrammar;
var
  Grammar: TEBNFGrammar;
  Rule: TEBNFRule;
begin
  Rule:=nil;
  Grammar := TEBNFGrammar.Create;
  try
    while FCurrentToken.TokenType <> ttEOF do
    begin
      Rule := ParseRule;
      Grammar.AddRule(Rule);
      Rule:=nil;
    end;
    Result := Grammar;
  except
    Rule.Free;
    Grammar.Free;
    raise;
  end;
end;

function TEBNFParser.ParseRule: TEBNFRule;
var
  Identifier: string;
  Expression: TEBNFExpression;
begin
  if FCurrentToken.TokenType <> ttIdentifier then
    Error(SErrExpectedIdentifierForRule);
  Identifier := FCurrentToken.Value;
  Consume(ttIdentifier);

  Consume(ttEquals);

  Expression := ParseExpression;
  try
    Consume(ttSemicolon);
    Result := TEBNFRule.Create(Identifier, Expression);
    Expression := Nil;
  except
    Expression.Free;
    Raise;
  end;
end;

function TEBNFParser.ParseExpression: TEBNFExpression;
var
  Expression: TEBNFExpression;
  Term: TEBNFTerm;
  NewLine : boolean;
begin
  Expression := TEBNFExpression.Create;
  try
    Term := ParseTerm;
    Expression.AddTerm(Term);
    NewLine:=FLastLine<>FCurrentToken.Position.Row;
    while Peek(ttPipe) do
    begin
      Consume(ttPipe);
      Term := ParseTerm;
      Term.newline:=NewLine;
      Expression.AddTerm(Term);
    end;
    Result := Expression;
  except
    Expression.Free;
    raise;
  end;
end;

function TEBNFParser.ParseTerm: TEBNFTerm;
var
  Term: TEBNFTerm;
  Factor: TEBNFFactor;
begin
  Term := TEBNFTerm.Create;
  try
    // A term is a sequence of factors.
    // Loop until we hit a delimiter for expression or rule end.
    while not Peek([ttPipe,ttSemicolon,ttCloseParen,ttCloseBracket,ttCloseBrace,ttEOF]) do
    begin
      Factor := ParseFactor;
      Term.AddFactor(Factor);
    end;
    Result := Term;
  except
    Term.Free;
    raise;
  end;
end;

function TEBNFParser.ParseFactor: TEBNFFactor;
var
  InnerExpression: TEBNFExpression;
  Value: string;
begin
  case FCurrentToken.TokenType of
    ttIdentifier:
    begin
      Result := TEBNFFactor.Create(etFactorIdentifier, FCurrentToken.Value);
      Consume(ttIdentifier);
    end;
    ttStringLiteral:
    begin
      Result := TEBNFFactor.Create(etFactorStringLiteral, FCurrentToken.Value);
      Consume(ttStringLiteral);
    end;
    ttOpenBracket: // Optional group [ expression ]
    begin
      Consume(ttOpenBracket);
      InnerExpression := ParseExpression;
      try
        Consume(ttCloseBracket);
        Result := TEBNFFactor.Create(etFactorOptional, InnerExpression);
        InnerExpression:=nil;
      except
        InnerExpression.Free;
        Raise;
      end;
    end;
    ttOpenBrace: // Repetition group { expression }
    begin
      Consume(ttOpenBrace);
      InnerExpression := ParseExpression;
      try
        Consume(ttCloseBrace);
        Result := TEBNFFactor.Create(etFactorRepetition, InnerExpression);
        InnerExpression:=nil;
      except
        InnerExpression.Free;
        Raise;
      end;
    end;
    ttOpenParen: // Group ( expression )
    begin
      Consume(ttOpenParen);
      InnerExpression := ParseExpression;
      try
        Consume(ttCloseParen);
        Result := TEBNFFactor.Create(etFactorGroup, InnerExpression);
        InnerExpression:=nil;
      except
        InnerExpression.Free;
        Raise;
      end;
    end;
    ttQuestion: // Special sequence ? value ?
    begin
      Consume(ttQuestion);
      if FCurrentToken.TokenType <> ttStringLiteral then
        Error(SerrExpectedStringLiteral);
      Value := FCurrentToken.Value;
      Consume(ttStringLiteral);
      Consume(ttQuestion);
      Result := TEBNFFactor.Create(etFactorSpecialSequence, Value);
    end;
    else
      Error(Format(SerrUnexpectedFactorToken, [GetEnumName(TypeInfo(TEBNFTokenType), Ord(FCurrentToken.TokenType))]));
  end;
end;

end.
