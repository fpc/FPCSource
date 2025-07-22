{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt (michael@freepascal.org)

    Test EBNF Parser

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utcparser;

interface

uses
  SysUtils, classes, fpcunit, testregistry,
  ebnf.tree,
  ebnf.parser;

type

  { TTestEBNFParser }

  TTestEBNFParser = class(TTestCase)
  private
    FGrammar: TEBNFGrammar;
    FParser: TEBNFParser;
  protected

    procedure TearDown; override;

    procedure AssertEquals(const Msg : String; aExpected, aActual : TEBNFElementType); overload;
    procedure CheckEquals(aExpected, aActual : TEBNFElementType; const Msg : String = ''); overload;
    Property Parser : TEBNFParser Read FParser Write FParser;
    Property Grammar : TEBNFGrammar Read FGrammar Write FGrammar;
  published
    procedure TestOneRuleOneTermOneFactor;
    procedure TestOneRuleOneTermTwoFactors;
    procedure TestOneRuleTwoTermsOneFactorEach;
    procedure TestOneRuleTwoTermsTwoFactorsEach;
    procedure TestTwoRulesOneTermOneFactorEach;
    procedure TestRuleWithOptionalGroup;
    procedure TestRuleWithRepetitionGroup;
    procedure TestRuleWithParenthesizedGroup;
    procedure TestRuleWithSpecialSequence;
    procedure TestDuplicateRuleError;
    procedure TestMissingEqualsError;
    procedure TestMissingSemicolonError;
    procedure TestUnexpectedTokenInFactorError;
  end;

implementation

uses typinfo;

procedure TTestEBNFParser.AssertEquals(const Msg: String; aExpected, aActual: TEBNFElementType);
begin
  AssertEquals(Msg,GetEnumName(typeInfo(TEBNFElementType),ord(aExpected)),
                  GetEnumName(typeInfo(TEBNFElementType),ord(aActual)));
end;

procedure TTestEBNFParser.CheckEquals(aExpected, aActual: TEBNFElementType; const Msg: String);
begin
  AssertEquals(Msg,aExpected,aActual);
end;

procedure TTestEBNFParser.TearDown;
begin
  FreeAndNil(FParser);
  FreeAndNil(FGrammar);
  inherited TearDown;
end;

procedure TTestEBNFParser.TestOneRuleOneTermOneFactor;
var
  EBNFSource: string;
  Rule: TEBNFRule;
  Expression: TEBNFExpression;
  Term: TEBNFTerm;
  Factor: TEBNFFactor;
begin
  EBNFSource := 'rule1 = "literal" ;';
  Parser := TEBNFParser.Create(EBNFSource);
  Grammar := Parser.Parse;
  CheckEquals(1, Grammar.ChildCount, 'Expected 1 rule in grammar');
  Rule:=Grammar.Rules['rule1'];
  CheckNotNull(Rule, 'Rule object should not be nil');

  CheckEquals(etExpression, Rule.Expression.NodeType, 'Rule expression should be of type anExpression');
  Expression := TEBNFExpression(Rule.Expression);
  CheckEquals(1, Expression.ChildCount, 'Expected 1 term in expression');

  CheckEquals(etTerm, Expression.Terms[0].NodeType, 'Expression term should be of type anTerm');
  Term := TEBNFTerm(Expression.Terms[0]);
  CheckEquals(1, Term.ChildCount, 'Expected 1 factor in term');

  CheckEquals(etFactorStringLiteral, Term.Factors[0].NodeType, 'Term factor should be of type anFactorStringLiteral');
  Factor := TEBNFFactor(Term.Factors[0]);
  CheckEquals('literal', Factor.Value, 'Factor value should be "literal"');
end;

procedure TTestEBNFParser.TestOneRuleOneTermTwoFactors;
var
  EBNFSource: string;
  Rule: TEBNFRule;
  Expression: TEBNFExpression;
  Term: TEBNFTerm;
  Factor1, Factor2: TEBNFFactor;
begin
  EBNFSource := 'rule2 = identifier1 "literal2" ;';
  Parser := TEBNFParser.Create(EBNFSource);
  Grammar := Parser.Parse;
  CheckEquals(1, Grammar.ChildCount, 'Expected 1 rule in grammar');
  Rule:=Grammar.Rules['rule2'];
  CheckNotNull(Rule, 'Expected rule "rule2" to exist');

  Expression := TEBNFExpression(Rule.Expression);
  CheckEquals(1, Expression.ChildCount, 'Expected 1 term in expression');

  Term := TEBNFTerm(Expression.Terms[0]);
  CheckEquals(2, Term.ChildCount, 'Expected 2 factors in term');

  Factor1 := TEBNFFactor(Term.Factors[0]);
  CheckEquals(etFactorIdentifier, Factor1.NodeType, 'First factor should be identifier');
  CheckEquals('identifier1', Factor1.Value, 'First factor value should be "identifier1"');

  Factor2 := TEBNFFactor(Term.Factors[1]);
  CheckEquals(etFactorStringLiteral, Factor2.NodeType, 'Second factor should be string literal');
  CheckEquals('literal2', Factor2.Value, 'Second factor value should be "literal2"');
end;

procedure TTestEBNFParser.TestOneRuleTwoTermsOneFactorEach;
var
  EBNFSource: string;
  Rule: TEBNFRule;
  Expression: TEBNFExpression;
  Term1, Term2: TEBNFTerm;
  Factor1, Factor2: TEBNFFactor;
begin
  EBNFSource := 'rule3 = factorA | factorB ;';
  Parser := TEBNFParser.Create(EBNFSource);
  Grammar := Parser.Parse;
  CheckEquals(1, Grammar.ChildCount, 'Expected 1 rule in grammar');
  Rule:=Grammar.Rules['rule3'];
  ChecknotNull(Rule, 'Expected rule "rule3" to exist');

  Expression := TEBNFExpression(Rule.Expression);
  CheckEquals(2, Expression.ChildCount, 'Expected 2 terms in expression');

  // First term
  Term1 := TEBNFTerm(Expression.Terms[0]);
  CheckEquals(1, Term1.ChildCount, 'Expected 1 factor in first term');
  Factor1 := TEBNFFactor(Term1.Factors[0]);
  CheckEquals(etFactorIdentifier, Factor1.NodeType, 'First term factor should be identifier');
  CheckEquals('factorA', Factor1.Value, 'First term factor value should be "factorA"');

  // Second term
  Term2 := TEBNFTerm(Expression.Terms[1]);
  CheckEquals(1, Term2.ChildCount, 'Expected 1 factor in second term');
  Factor2 := TEBNFFactor(Term2.Factors[0]);
  CheckEquals(etFactorIdentifier, Factor2.NodeType, 'Second term factor should be identifier');
  CheckEquals('factorB', Factor2.Value, 'Second term factor value should be "factorB"');
end;

procedure TTestEBNFParser.TestOneRuleTwoTermsTwoFactorsEach;
var
  EBNFSource: string;
  Rule: TEBNFRule;
  Expression: TEBNFExpression;
  Term1, Term2: TEBNFTerm;
  Factor: TEBNFFactor;
begin
  EBNFSource := 'rule4 = (id1 "lit1") | (id2 "lit2") ;';
  Parser := TEBNFParser.Create(EBNFSource);
  Grammar := Parser.Parse;
  CheckEquals(1, Grammar.ChildCount, 'Expected 1 rule in grammar');
  Rule:=Grammar.Rules['rule4'];
  CheckNotNull(Rule, 'Expected rule "rule4" to exist');

  Expression := TEBNFExpression(Rule.Expression);
  CheckEquals(2, Expression.ChildCount, 'Expected 2 terms in expression');

  // First term (group)
  Term1 := TEBNFTerm(Expression.Terms[0]);
  CheckEquals(1, Term1.ChildCount, 'Expected 1 factor (group) in first term');
  Factor := TEBNFFactor(Term1.Factors[0]);
  CheckEquals(etFactorGroup, Factor.NodeType, 'Factor should be a group');
  CheckNotNull(Factor.InnerNode, 'Inner node of group should not be nil');

  // Check inner expression of first group
  CheckEquals(etExpression, Factor.InnerNode.NodeType, 'Inner node should be an expression');
  CheckEquals(1, TEBNFExpression(Factor.InnerNode).ChildCount, 'Inner expression should have 1 term');
  CheckEquals(2, TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).ChildCount, 'Inner term should have 2 factors');
  CheckEquals('id1', TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).Factors[0]).Value, 'Inner factor 1 value');
  CheckEquals('lit1', TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).Factors[1]).Value, 'Inner factor 2 value');

  // Second term (group)
  Term2 := TEBNFTerm(Expression.Terms[1]);
  CheckEquals(1, Term2.ChildCount, 'Expected 1 factor (group) in second term');
  Factor := TEBNFFactor(Term2.Factors[0]);
  CheckEquals(etFactorGroup, Factor.NodeType, 'Factor should be a group');
  CheckNotNull(Factor.InnerNode, 'Inner node of group should not be nil');

  // Check inner expression of second group
  CheckEquals(etExpression, Factor.InnerNode.NodeType, 'Inner node should be an expression');
  CheckEquals(1, TEBNFExpression(Factor.InnerNode).ChildCount, 'Inner expression should have 1 term');
  CheckEquals(2, TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).ChildCount, 'Inner term should have 2 factors');
  CheckEquals('id2', TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).Factors[0]).Value, 'Inner factor 1 value');
  CheckEquals('lit2', TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).Factors[1]).Value, 'Inner factor 2 value');

end;

procedure TTestEBNFParser.TestTwoRulesOneTermOneFactorEach;
var
  EBNFSource: string;
  Rule1, Rule2: TEBNFRule;
begin
  EBNFSource :=
    'ruleA = "first" ;' + sLineBreak +
    'ruleB = identifierB ;';
  Parser := TEBNFParser.Create(EBNFSource);
  Grammar := Parser.Parse;
  CheckEquals(2, Grammar.ChildCount, 'Expected 2 rules in grammar');

  // Check ruleA
  Rule1:=Grammar.Rules['ruleA'];
  CheckNotNull(Rule1, 'Expected rule "ruleA" to exist');
  CheckEquals(etExpression, Rule1.Expression.NodeType, 'RuleA expression type');
  CheckEquals(1, TEBNFExpression(Rule1.Expression).ChildCount, 'RuleA expression terms count');
  CheckEquals(1, TEBNFTerm(TEBNFExpression(Rule1.Expression).Terms[0]).ChildCount, 'RuleA term factors count');
  CheckEquals(etFactorStringLiteral, TEBNFFactor(TEBNFTerm(TEBNFExpression(Rule1.Expression).Terms[0]).Factors[0]).NodeType, 'RuleA factor type');
  CheckEquals('first', TEBNFFactor(TEBNFTerm(TEBNFExpression(Rule1.Expression).Terms[0]).Factors[0]).Value, 'RuleA factor value');

  // Check ruleB
  Rule2:=Grammar.Rules['ruleB'];
  CheckNotNull(Rule2, 'Expected rule "ruleB" to exist');
  CheckEquals(etExpression, Rule2.Expression.NodeType, 'RuleB expression type');
  CheckEquals(1, TEBNFExpression(Rule2.Expression).ChildCount, 'RuleB expression terms count');
  CheckEquals(1, TEBNFTerm(TEBNFExpression(Rule2.Expression).Terms[0]).ChildCount, 'RuleB term factors count');
  CheckEquals(etFactorIdentifier, TEBNFFactor(TEBNFTerm(TEBNFExpression(Rule2.Expression).Terms[0]).Factors[0]).NodeType, 'RuleB factor type');
  CheckEquals('identifierB', TEBNFFactor(TEBNFTerm(TEBNFExpression(Rule2.Expression).Terms[0]).Factors[0]).Value, 'RuleB factor value');
end;

procedure TTestEBNFParser.TestRuleWithOptionalGroup;
var
  EBNFSource: string;
  Rule: TEBNFRule;
  Expression: TEBNFExpression;
  Term: TEBNFTerm;
  Factor: TEBNFFactor;
begin
  EBNFSource := 'optional_rule = [ "optional_part" ] ;';
  Parser := TEBNFParser.Create(EBNFSource);
  Grammar := Parser.Parse;
  Rule:=Grammar.Rules['optional_rule'];
  CheckNotNull(Rule, 'Expected rule "optional_rule"');
  Expression := TEBNFExpression(Rule.Expression);
  Term := TEBNFTerm(Expression.Terms[0]);
  Factor := TEBNFFactor(Term.Factors[0]);

  CheckEquals(etFactorOptional, Factor.NodeType, 'Factor should be an optional group');
  CheckNotNull(Factor.InnerNode, 'Optional group should have an inner node');
  CheckEquals(etExpression, Factor.InnerNode.NodeType, 'Inner node should be an expression');
  CheckEquals(1, TEBNFExpression(Factor.InnerNode).ChildCount, 'Inner expression should have 1 term');
  CheckEquals(1, TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).ChildCount, 'Inner term should have 1 factor');
  CheckEquals(etFactorStringLiteral, TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).Factors[0]).NodeType, 'Inner factor type');
  CheckEquals('optional_part', TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).Factors[0]).Value, 'Inner factor value');
end;

procedure TTestEBNFParser.TestRuleWithRepetitionGroup;
var
  EBNFSource: string;
  Rule: TEBNFRule;
  Expression: TEBNFExpression;
  Term: TEBNFTerm;
  Factor: TEBNFFactor;
begin
  EBNFSource := 'repeat_rule = { identifier_to_repeat } ;';
  Parser := TEBNFParser.Create(EBNFSource);
  Grammar := Parser.Parse;
  Rule:=Grammar.Rules['repeat_rule'];
  CheckNotNull(Rule, 'Expected rule "repeat_rule"');
  Expression := TEBNFExpression(Rule.Expression);
  Term := TEBNFTerm(Expression.Terms[0]);
  Factor := TEBNFFactor(Term.Factors[0]);

  CheckEquals(etFactorRepetition, Factor.NodeType, 'Factor should be a repetition group');
  CheckNotNull(Factor.InnerNode, 'Repetition group should have an inner node');
  CheckEquals(etExpression, Factor.InnerNode.NodeType, 'Inner node should be an expression');
  CheckEquals(1, TEBNFExpression(Factor.InnerNode).ChildCount, 'Inner expression should have 1 term');
  CheckEquals(1, TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).ChildCount, 'Inner term should have 1 factor');
  CheckEquals(etFactorIdentifier, TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).Factors[0]).NodeType, 'Inner factor type');
  CheckEquals('identifier_to_repeat', TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).Factors[0]).Value, 'Inner factor value');
end;

procedure TTestEBNFParser.TestRuleWithParenthesizedGroup;
var
  EBNFSource: string;
  Rule: TEBNFRule;
  Expression: TEBNFExpression;
  Term: TEBNFTerm;
  Factor: TEBNFFactor;
begin
  EBNFSource := 'group_rule = ( "part_one" | "part_two" ) ;';
  Parser := TEBNFParser.Create(EBNFSource);
  Grammar := Parser.Parse;
  Rule:=Grammar.Rules['group_rule'];
  CheckNotNull(Rule, 'Expected rule "group_rule"');
  Expression := TEBNFExpression(Rule.Expression);
  Term := TEBNFTerm(Expression.Terms[0]);
  Factor := TEBNFFactor(Term.Factors[0]);

  CheckEquals(etFactorGroup, Factor.NodeType, 'Factor should be a parenthesized group');
  CheckNotNull(Factor.InnerNode, 'Group should have an inner node');
  CheckEquals(etExpression, Factor.InnerNode.NodeType, 'Inner node should be an expression');
  CheckEquals(2, TEBNFExpression(Factor.InnerNode).ChildCount, 'Inner expression should have 2 terms'); // Because of '|'
  CheckEquals(1, TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).ChildCount, 'First inner term should have 1 factor');
  CheckEquals('part_one', TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[0]).Factors[0]).Value, 'First inner factor value');
  CheckEquals(1, TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[1]).ChildCount, 'Second inner term should have 1 factor');
  CheckEquals('part_two', TEBNFFactor(TEBNFTerm(TEBNFExpression(Factor.InnerNode).Terms[1]).Factors[0]).Value, 'Second inner factor value');
end;

procedure TTestEBNFParser.TestRuleWithSpecialSequence;
var
  EBNFSource: string;
  Rule: TEBNFRule;
  Expression: TEBNFExpression;
  Term: TEBNFTerm;
  Factor: TEBNFFactor;
begin
  EBNFSource := 'special_rule = ? "this is a comment" ? ;';
  Parser := TEBNFParser.Create(EBNFSource);
  Grammar := Parser.Parse;
  Rule := Grammar.Rules['special_rule'];
  CheckNotNull(Rule, 'Expected rule "special_rule"');
  Expression := TEBNFExpression(Rule.Expression);
  Term := TEBNFTerm(Expression.Terms[0]);
  Factor := TEBNFFactor(Term.Factors[0]);

  CheckEquals(etFactorSpecialSequence, Factor.NodeType, 'Factor should be a special sequence');
  CheckEquals('this is a comment', Factor.Value, 'Special sequence value should match');
end;

procedure TTestEBNFParser.TestDuplicateRuleError;
var
  EBNFSource: string;

begin
  EBNFSource :=
    'ruleA = "first" ;' + sLineBreak +
    'ruleA = "second" ;'; // Duplicate rule definition
  Parser := TEBNFParser.Create(EBNFSource);
  try
    Parser.Parse;
    Fail('Expected an exception for duplicate rule');
  except
    on E: Exception do
      Check(Pos('Duplicate rule identifier: ruleA', E.Message) > 0, 'Expected "Duplicate rule identifier" error message');
  end;
end;

procedure TTestEBNFParser.TestMissingEqualsError;
var
  EBNFSource: string;
begin
  EBNFSource := 'rule_bad "literal" ;'; // Missing '='
  Parser := TEBNFParser.Create(EBNFSource);
  try
    Parser.Parse;
    Fail('Expected an exception for missing equals sign');
  except
    on E: Exception do
      Check(Pos('Expected ttEquals, but found ttStringLiteral', E.Message) > 0, 'Expected "Expected ttEquals" error message');
  end;
end;

procedure TTestEBNFParser.TestMissingSemicolonError;
var
  EBNFSource: string;
begin
  EBNFSource := 'rule_bad = "literal" '; // Missing ';'
  Parser := TEBNFParser.Create(EBNFSource);
  try
    Parser.Parse;
    Fail('Expected an exception for missing semicolon');
  except
    on E: Exception do
      Check(Pos('Expected ttSemicolon, but found ttEOF', E.Message) > 0, 'Expected "Expected ttSemicolon" error message');
  end;
end;

procedure TTestEBNFParser.TestUnexpectedTokenInFactorError;
var
  EBNFSource: string;
begin
  EBNFSource := 'rule_bad = = ;'; // '==' is not a valid factor
  Parser := TEBNFParser.Create(EBNFSource);
  try
    Parser.Parse;
    Fail('Expected an exception for unexpected token in factor');
  except
    on E: Exception do
      Check(Pos('Unexpected token for factor: ttEquals', E.Message) > 0, 'Expected "Unexpected token for factor" error message');
  end;
end;

initialization
  RegisterTest(TTestEBNFParser);

end.

