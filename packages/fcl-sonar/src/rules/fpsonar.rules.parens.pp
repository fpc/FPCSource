{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    CST-tier parenthesis analysis rules and their precedence foundation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Parens;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Ingest, FpSonar.Rules.Consts;

type
  // A list of token indices into the original TFpSonarTokenArray (the opening
  // '(' of a redundant pair, or a 'not' keyword) for the caller to report at.
  TParenIndexArray = array of integer;

{ The opening-'(' indices of every provably redundant grouping pair:
  a pair around a single atom / dotted chain, or the outer of a doubled pair
  '((expr))' }
function FindRedundantParens(const aTokens: TFpSonarTokenArray): TParenIndexArray;

{ The indices of every 'not' keyword whose operand is a NON-parenthesised
  atom/dotted chain immediately followed by a binary
  operator — the reader-ambiguous shape 'not a and b' }
function FindAmbiguousNots(const aTokens: TFpSonarTokenArray): TParenIndexArray;

{ The FPC operator-precedence rank of aToken:
  1 = unary (not/@/^),
  2 = multiplying (* / div mod and shl shr as),
  3 = adding (+ - or xor),
  4 = relational (= <> < > <= >= in is);
  0 when aToken is not an operator. }
function PrecedenceRank(const aToken: TFpSonarToken): integer;

type
  { Flags a grouping parenthesis pair that can be removed without changing meaning or precedence. }
  TRuleRemoveRedundantParentheses = class(TRuleBase)
  public
    // Emits one issue at the opening '(' of each provably-redundant grouping pair.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a 'not' whose operand grouping is ambiguous to a reader. }
  TRuleParenthesizeAmbiguousNot = class(TRuleBase)
  public
    // Emits one issue at each ambiguous 'not' keyword token.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

implementation

type
  TBoolArray = array of boolean;

{ The original-array indices of every significant token }
function BuildSignificant(const aTokens: TFpSonarTokenArray): TParenIndexArray;
var
  i, lCount: integer;
begin
  SetLength(Result, Length(aTokens));
  lCount := 0;
  for i := 0 to High(aTokens) do
    if (not aTokens[i].IsTrivia)
      and ((aTokens[i].Text <> '') or (aTokens[i].Punct <> '')) then
    begin
      Result[lCount] := i;
      Inc(lCount);
    end;
  SetLength(Result, lCount);
end;


{ For each '(' at significant index k, the significant index of its matching ')', or -1 (unmatched) }
function BuildMatches(const aTokens: TFpSonarTokenArray;
  const aSig: TParenIndexArray): TParenIndexArray;
var
  k, lTop: integer;
  lStack: TParenIndexArray;
  lSP: integer;
  lPunct: string;
begin
  SetLength(Result, Length(aSig));
  for k := 0 to High(Result) do
    Result[k] := -1;
  SetLength(lStack, Length(aSig));
  lSP := 0;
  for k := 0 to High(aSig) do
  begin
    lPunct := aTokens[aSig[k]].Punct;
    if lPunct = '(' then
    begin
      lStack[lSP] := k;
      Inc(lSP);
    end
    else if lPunct = ')' then
      if lSP > 0 then
      begin
        Dec(lSP);
        lTop := lStack[lSP];
        Result[lTop] := k;
      end;
  end;
end;


{ True iff the significant token at index k is an atom leaf: a number, a string/char literal, or an identifier }
function IsAtomSig(const aTokens: TFpSonarTokenArray;
  const aSig: TParenIndexArray; k: integer): boolean;
var
  lTok: TFpSonarToken;
begin
  lTok := aTokens[aSig[k]];
  if lTok.IsKeyword then
    Exit(False);
  Result := lTok.IsNumber or lTok.IsString
    or ((lTok.Punct = '') and (lTok.Text <> ''));
end;


{ True iff the significant range [aFirst..aLast] is a single atom or a dotted
  member chain a.b.c with no operators, calls or indices: the shape whose
  surrounding parentheses arm A can prove redundant. }
function IsSimpleOperandRange(const aTokens: TFpSonarTokenArray;
  const aSig: TParenIndexArray; aFirst, aLast: integer): boolean;
var
  k: integer;
begin
  if aFirst > aLast then
    Exit(False);
  if not IsAtomSig(aTokens, aSig, aFirst) then
    Exit(False);
  k := aFirst;
  while k < aLast do
  begin
    if aTokens[aSig[k + 1]].Punct <> '.' then
      Exit(False);
    if k + 2 > aLast then
      Exit(False);
    if not IsAtomSig(aTokens, aSig, k + 2) then
      Exit(False);
    Inc(k, 2);
  end;
  Result := True;
end;


{ True iff the '(' at significant index k opens an expression-grouping pair,
  as opposed to a routine call / typecast / index / declaration list,
  or an abstain context. }
function ParenIsGrouping(const aTokens: TFpSonarTokenArray;
  const aSig: TParenIndexArray; k: integer): boolean;
var
  lPrevPunct: string;
begin
  if k = 0 then
    Exit(False);
  // Postfix construct: a '(' bound to a preceding operand/postfix token.
  if IsAtomSig(aTokens, aSig, k - 1) then
    Exit(False);
  lPrevPunct := aTokens[aSig[k - 1]].Punct;
  if (lPrevPunct = ')') or (lPrevPunct = ']') or (lPrevPunct = '^') then
    Exit(False);
  if lPrevPunct = '@' then
    Exit(False);
  Result := (lPrevPunct = ':=') or (lPrevPunct = '+') or (lPrevPunct = '-')
    or (lPrevPunct = '*') or (lPrevPunct = '/') or (lPrevPunct = ',')
    or (lPrevPunct = '(');
end;


{ True iff the grouping pair wraps exactly one nested pair:
  the doubled '((expr))' expression . }
function IsDoubledPair(const aTokens: TFpSonarTokenArray;
  const aSig, aMatch: TParenIndexArray; k: integer): boolean;
var
  j: integer;
begin
  j := aMatch[k];
  Result := (k + 1 <= j - 1) and (aTokens[aSig[k + 1]].Punct = '(')
    and (aMatch[k + 1] = j - 1);
end;


function FindRedundantParens(const aTokens: TFpSonarTokenArray): TParenIndexArray;
var
  lSig, lMatch: TParenIndexArray;
  lConsumed: TBoolArray;
  k, lCount: integer;
begin
  lSig := BuildSignificant(aTokens);
  lMatch := BuildMatches(aTokens, lSig);
  SetLength(lConsumed, Length(lSig));

  { Mark the inner pair of every redundant doubled grouping pair so arm A does
    not also flag it — the outer arm-B finding stands for the whole nest. }
  for k := 0 to High(lSig) do
    if (aTokens[lSig[k]].Punct = '(') and (lMatch[k] >= 0)
      and ParenIsGrouping(aTokens, lSig, k)
      and IsDoubledPair(aTokens, lSig, lMatch, k) then
      lConsumed[k + 1] := True;

  SetLength(Result, Length(lSig));
  lCount := 0;
  for k := 0 to High(lSig) do
  begin
    if (aTokens[lSig[k]].Punct <> '(') or (lMatch[k] < 0) then
      Continue;
    if lConsumed[k] then
      Continue;
    if not ParenIsGrouping(aTokens, lSig, k) then
      Continue;
    if IsDoubledPair(aTokens, lSig, lMatch, k)
      or IsSimpleOperandRange(aTokens, lSig, k + 1, lMatch[k] - 1) then
    begin
      Result[lCount] := lSig[k];
      Inc(lCount);
    end;
  end;
  SetLength(Result, lCount);
end;


function PrecedenceRank(const aToken: TFpSonarToken): integer;
var
  lPunct, lWord: string;
begin
  Result := 0;
  if aToken.IsKeyword then
  begin
    lWord := LowerCase(aToken.Text);
    if lWord = 'not' then
      Result := 1
    else if (lWord = 'div') or (lWord = 'mod') or (lWord = 'and')
      or (lWord = 'shl') or (lWord = 'shr') or (lWord = 'as') then
      Result := 2
    else if (lWord = 'or') or (lWord = 'xor') then
      Result := 3
    else if (lWord = 'in') or (lWord = 'is') then
      Result := 4;
    Exit;
  end;
  lPunct := aToken.Punct;
  if (lPunct = '@') or (lPunct = '^') then
    Result := 1
  else if (lPunct = '*') or (lPunct = '/') or (lPunct = '<<')
    or (lPunct = '>>') then
    Result := 2
  else if (lPunct = '+') or (lPunct = '-') then
    Result := 3
  else if (lPunct = '=') or (lPunct = '<>') or (lPunct = '<') or (lPunct = '>')
    or (lPunct = '<=') or (lPunct = '>=') then
    Result := 4;
end;


// True iff the significant token at index k is a binary operator that may follow a 'not'-negated operand.
function IsBinaryOperatorSig(const aTokens: TFpSonarTokenArray;
  const aSig: TParenIndexArray; k: integer): boolean;
var
  lTok: TFpSonarToken;
  lWord, lPunct: string;
begin
  lTok := aTokens[aSig[k]];
  if lTok.IsKeyword then
  begin
    lWord := LowerCase(lTok.Text);
    Result := (lWord = 'and') or (lWord = 'or') or (lWord = 'xor')
      or (lWord = 'div') or (lWord = 'mod') or (lWord = 'shl')
      or (lWord = 'shr') or (lWord = 'as') or (lWord = 'is')
      or (lWord = 'in');
    Exit;
  end;
  lPunct := lTok.Punct;
  Result := (lPunct = '=') or (lPunct = '<>') or (lPunct = '<')
    or (lPunct = '>') or (lPunct = '<=') or (lPunct = '>=')
    or (lPunct = '+') or (lPunct = '-') or (lPunct = '*') or (lPunct = '/');
end;


function FindAmbiguousNots(const aTokens: TFpSonarTokenArray): TParenIndexArray;
var
  lSig: TParenIndexArray;
  k, lOperand, lAfter, lCount: integer;
begin
  lSig := BuildSignificant(aTokens);
  SetLength(Result, Length(lSig));
  lCount := 0;
  for k := 0 to High(lSig) do
  begin
    if not aTokens[lSig[k]].IsKeyword then
      Continue;
    if LowerCase(aTokens[lSig[k]].Text) <> 'not' then
      Continue;
    if k + 1 > High(lSig) then
      Continue;
    // A parenthesised operand 'not (...)' is already disambiguated.
    if aTokens[lSig[k + 1]].Punct = '(' then
      Continue;
    // The operand must be a non-call atom / dotted chain.
    if not IsAtomSig(aTokens, lSig, k + 1) then
      Continue;
    lOperand := k + 1;
    while (lOperand + 2 <= High(lSig))
      and (aTokens[lSig[lOperand + 1]].Punct = '.')
      and IsAtomSig(aTokens, lSig, lOperand + 2) do
      Inc(lOperand, 2);
    lAfter := lOperand + 1;
    if lAfter > High(lSig) then
      Continue;
    // A call/index right after the operand makes it complex — abstain.
    if (aTokens[lSig[lAfter]].Punct = '(')
      or (aTokens[lSig[lAfter]].Punct = '[') then
      Continue;
    if IsBinaryOperatorSig(aTokens, lSig, lAfter) then
    begin
      Result[lCount] := lSig[k];
      Inc(lCount);
    end;
  end;
  SetLength(Result, lCount);
end;

const
  cKeyRemoveRedundantParens = 'rule.RemoveRedundantParentheses.message';
  cKeyParenthesizeAmbiguousNot = 'rule.ParenthesizeAmbiguousNot.message';
  // RemoveRedundantParentheses's declared parameter: a comma-separated operator
  // list whose adjacency suppresses the deferred precedence arm.
  cKeepAroundOperatorsKey = 'keepAroundOperators';

  { TRuleRemoveRedundantParentheses }

procedure TRuleRemoveRedundantParentheses.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lOpens: TParenIndexArray;
  i: integer;
  lTok: TFpSonarToken;
begin
  lOpens := FindRedundantParens(aContext.Tokens);
  for i := 0 to High(lOpens) do
  begin
    lTok := aContext.Tokens[lOpens[i]];
    aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
      lTok.Row, lTok.Col, lTok.Row, lTok.Col, FMetadata.Severity,
      FMetadata.Category, FMetadata.DefaultConfidence, FMetadata.MessageKey,
      [], '(');
  end;
end;


{ TRuleParenthesizeAmbiguousNot }

procedure TRuleParenthesizeAmbiguousNot.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNots: TParenIndexArray;
  i: integer;
  lTok: TFpSonarToken;
begin
  lNots := FindAmbiguousNots(aContext.Tokens);
  for i := 0 to High(lNots) do
  begin
    lTok := aContext.Tokens[lNots[i]];
    aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
      lTok.Row, lTok.Col, lTok.Row, lTok.Col, FMetadata.Severity,
      FMetadata.Category, FMetadata.DefaultConfidence, FMetadata.MessageKey,
      [], 'not');
  end;
end;


var
  lMeta: TRuleMetadata;

initialization
  lMeta := TRuleMetadata.Make('RemoveRedundantParentheses', rtTok, rfTokenStream,
    sevMinor, itCodeSmell, cfHigh, True, cKeyRemoveRedundantParens);
  lMeta.AddParam(cKeepAroundOperatorsKey, rpkString);
  lMeta.Description :=
    'Flags redundant parentheses that can be removed.';
  RegisterRule(TRuleRemoveRedundantParentheses.Create(lMeta));
  RegisterMessage(cKeyRemoveRedundantParens, SRemoveRedundantParentheses);

  RegisterRule(TRuleParenthesizeAmbiguousNot.Create(TRuleMetadata.Make(
    'ParenthesizeAmbiguousNot', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyParenthesizeAmbiguousNot).WithDescription(
    'Flags an ambiguous not; parenthesize the negated operand.')));
  RegisterMessage(cKeyParenthesizeAmbiguousNot, SParenthesizeAmbiguousNot);

end.
