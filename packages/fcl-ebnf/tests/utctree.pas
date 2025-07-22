{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt (michael@freepascal.org)

    Test EBNF AST elements

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utctree;

interface

uses
  fpcunit, testregistry, SysUtils,
  ebnf.tree;

type
  TTestEBNFElementsToString = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestElementToString;
    procedure TestRuleToString;
    procedure TestExpressionToString;
    procedure TestTermToString;
    procedure TestFactorIdentifierToString;
    procedure TestFactorStringLiteralToString;
    procedure TestFactorOptionalToString;
    procedure TestFactorRepetitionToString;
    procedure TestFactorGroupToString;
    procedure TestFactorSpecialSequenceToString;
    procedure TestGrammarToString;
  end;

implementation

{ TTestEBNFElementsToString }

procedure TTestEBNFElementsToString.SetUp;
begin
  inherited SetUp;
end;

procedure TTestEBNFElementsToString.TearDown;
begin
  inherited TearDown;
end;

procedure TTestEBNFElementsToString.TestElementToString;
var
  Node: TEBNFElement;
begin
  Node := TEBNFElement.Create(etRule);
  try
    CheckEquals('Node Type: etRule', Node.ToString, 'Base AST Node ToString should show node type');
  finally
    Node.Free;
  end;
end;

procedure TTestEBNFElementsToString.TestRuleToString;
var
  Rule: TEBNFRule;
  Expression: TEBNFExpression;
  Term: TEBNFTerm;
  Factor: TEBNFFactor;
begin
  Factor := TEBNFFactor.Create(etFactorStringLiteral, 'keyword');
  Term := TEBNFTerm.Create;
  Term.AddFactor(Factor);
  Expression := TEBNFExpression.Create;
  Expression.AddTerm(Term);
  Rule := TEBNFRule.Create('myRule', Expression); // Rule takes ownership of Expression

  try
    CheckEquals('myRule = "keyword";', Rule.ToString, 'Rule ToString should be "identifier = expression;"');
  finally
    Rule.Free;
  end;
end;

procedure TTestEBNFElementsToString.TestExpressionToString;
var
  Expression: TEBNFExpression;
  Term1, Term2: TEBNFTerm;
  Factor1, Factor2: TEBNFFactor;
begin
  //  (id1) | (id2)
  Factor1 := TEBNFFactor.Create(etFactorIdentifier, 'id1');
  Term1 := TEBNFTerm.Create;
  Term1.AddFactor(Factor1);

  Factor2 := TEBNFFactor.Create(etFactorIdentifier, 'id2');
  Term2 := TEBNFTerm.Create;
  Term2.AddFactor(Factor2);

  Expression := TEBNFExpression.Create;
  Expression.AddTerm(Term1);
  Expression.AddTerm(Term2);

  try
    CheckEquals('id1 | id2', Expression.ToString, 'Expression ToString should concatenate terms with |');
  finally
    Expression.Free;
  end;
end;

procedure TTestEBNFElementsToString.TestTermToString;
var
  Term: TEBNFTerm;
  Factor1, Factor2: TEBNFFactor;
begin
  //  factorA "literalB"
  Factor1 := TEBNFFactor.Create(etFactorIdentifier, 'factorA');
  Factor2 := TEBNFFactor.Create(etFactorStringLiteral, 'literalB');

  Term := TEBNFTerm.Create;
  Term.AddFactor(Factor1);
  Term.AddFactor(Factor2);

  try
    CheckEquals('factorA "literalB"', Term.ToString, 'Term ToString should concatenate factors with space');
  finally
    Term.Free;
  end;
end;

procedure TTestEBNFElementsToString.TestFactorIdentifierToString;
var
  Factor: TEBNFFactor;
begin
  Factor := TEBNFFactor.Create(etFactorIdentifier, 'myIdentifier');
  try
    CheckEquals('myIdentifier', Factor.ToString, 'Identifier factor ToString should return its value');
  finally
    Factor.Free;
  end;
end;

procedure TTestEBNFElementsToString.TestFactorStringLiteralToString;
var
  Factor: TEBNFFactor;
begin
  Factor := TEBNFFactor.Create(etFactorStringLiteral, 'hello');
  try
    CheckEquals('"hello"', Factor.ToString, 'String literal factor ToString should return quoted value');
  finally
    Factor.Free;
  end;
end;

procedure TTestEBNFElementsToString.TestFactorOptionalToString;
var
  Factor: TEBNFFactor;
  InnerExpression: TEBNFExpression;
  InnerTerm: TEBNFTerm;
  InnerFactor: TEBNFFactor;
begin
  InnerFactor := TEBNFFactor.Create(etFactorIdentifier, 'optionalPart');
  InnerTerm := TEBNFTerm.Create;
  InnerTerm.AddFactor(InnerFactor);
  InnerExpression := TEBNFExpression.Create;
  InnerExpression.AddTerm(InnerTerm);

  Factor := TEBNFFactor.Create(etFactorOptional, InnerExpression); // Factor takes ownership of InnerExpression
  try
    CheckEquals('[optionalPart]', Factor.ToString, 'Optional factor ToString should be [expression]');
  finally
    Factor.Free;
  end;
end;

procedure TTestEBNFElementsToString.TestFactorRepetitionToString;
var
  Factor: TEBNFFactor;
  InnerExpression: TEBNFExpression;
  InnerTerm: TEBNFTerm;
  InnerFactor: TEBNFFactor;
begin
  InnerFactor := TEBNFFactor.Create(etFactorStringLiteral, 'repeated');
  InnerTerm := TEBNFTerm.Create;
  InnerTerm.AddFactor(InnerFactor);
  InnerExpression := TEBNFExpression.Create;
  InnerExpression.AddTerm(InnerTerm);

  Factor := TEBNFFactor.Create(etFactorRepetition, InnerExpression); // Factor takes ownership of InnerExpression
  try
    CheckEquals('{"repeated"}', Factor.ToString, 'Repetition factor ToString should be {expression}');
  finally
    Factor.Free;
  end;
end;

procedure TTestEBNFElementsToString.TestFactorGroupToString;
var
  Factor: TEBNFFactor;
  InnerExpression: TEBNFExpression;
  InnerTerm1, InnerTerm2: TEBNFTerm;
  InnerFactor1, InnerFactor2: TEBNFFactor;
begin
  InnerFactor1 := TEBNFFactor.Create(etFactorIdentifier, 'choiceA');
  InnerTerm1 := TEBNFTerm.Create;
  InnerTerm1.AddFactor(InnerFactor1);

  InnerFactor2 := TEBNFFactor.Create(etFactorIdentifier, 'choiceB');
  InnerTerm2 := TEBNFTerm.Create;
  InnerTerm2.AddFactor(InnerFactor2);

  InnerExpression := TEBNFExpression.Create;
  InnerExpression.AddTerm(InnerTerm1);
  InnerExpression.AddTerm(InnerTerm2);

  Factor := TEBNFFactor.Create(etFactorGroup, InnerExpression); // Factor takes ownership of InnerExpression
  try
    CheckEquals('(choiceA | choiceB)', Factor.ToString, 'Group factor ToString should be (expression)');
  finally
    Factor.Free; // This will free InnerExpression, InnerTerm1, InnerTerm2, InnerFactor1, InnerFactor2
  end;
end;

procedure TTestEBNFElementsToString.TestFactorSpecialSequenceToString;
var
  Factor: TEBNFFactor;
begin
  Factor := TEBNFFactor.Create(etFactorSpecialSequence, 'this is a comment');
  try
    CheckEquals('?this is a comment?', Factor.ToString, 'Special sequence factor ToString should be ?value?');
  finally
    Factor.Free;
  end;
end;

procedure TTestEBNFElementsToString.TestGrammarToString;
var
  Grammar: TEBNFGrammar;
  Rule1, Rule2: TEBNFRule;
  Expr1, Expr2: TEBNFExpression;
  Term1, Term2: TEBNFTerm;
  Factor1, Factor2: TEBNFFactor;
begin
  Grammar := TEBNFGrammar.Create;

  // Rule 1: ruleA = "litA" ;
  Factor1 := TEBNFFactor.Create(etFactorStringLiteral, 'litA');
  Term1 := TEBNFTerm.Create;
  Term1.AddFactor(Factor1);
  Expr1 := TEBNFExpression.Create;
  Expr1.AddTerm(Term1);
  Rule1 := TEBNFRule.Create('ruleA', Expr1);
  Grammar.AddRule(Rule1);

  // Rule 2: ruleB = idB ;
  Factor2 := TEBNFFactor.Create(etFactorIdentifier, 'idB');
  Term2 := TEBNFTerm.Create;
  Term2.AddFactor(Factor2);
  Expr2 := TEBNFExpression.Create;
  Expr2.AddTerm(Term2);
  Rule2 := TEBNFRule.Create('ruleB', Expr2);
  Grammar.AddRule(Rule2);
  try
    CheckEquals(
      'ruleA = "litA";' + sLineBreak +
      sLineBreak +
      'ruleB = idB;' + sLineBreak +
      sLineBreak,
      Grammar.ToString,
      'Grammar ToString should list all rules with line breaks'
    );
  finally
    Grammar.Free;
  end;
end;

initialization
  RegisterTest(TTestEBNFElementsToString);
end.

