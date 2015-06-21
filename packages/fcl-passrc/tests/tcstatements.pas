unit tcstatements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pastree, pscanner, pparser,
  tcbaseparser, testregistry;

Type
  { TTestStatementParser }

  TTestStatementParser = Class(TTestParser)
  private
    FStatement: TPasImplBlock;
    FVariables : TStrings;
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    procedure AddStatements(ASource : Array of string);
    Procedure DeclareVar(Const AVarType : String; Const AVarName : String = 'A');
    function TestStatement(ASource : string) : TPasImplElement;
    function TestStatement(ASource : Array of string) : TPasImplElement;
    Procedure ExpectParserError(Const Msg : string);
    Procedure ExpectParserError(Const Msg : string; ASource : Array of string);
    Function AssertStatement(Msg : String; AClass : TClass;AIndex : Integer = 0) : TPasImplBlock;
    Property Statement: TPasImplBlock Read FStatement;
  Published
    Procedure TestEmpty;
    Procedure TestEmptyStatement;
    Procedure TestEmptyStatements;
    Procedure TestBlock;
    Procedure TestBlockComment;
    Procedure TestBlock2Comments;
    Procedure TestAssignment;
    Procedure TestAssignmentAdd;
    Procedure TestAssignmentMinus;
    Procedure TestAssignmentMul;
    Procedure TestAssignmentDivision;
    Procedure TestCall;
    Procedure TestCallComment;
    Procedure TestCallQualified;
    Procedure TestCallQualified2;
    Procedure TestCallNoArgs;
    Procedure TestCallOneArg;
    Procedure TestIf;
    Procedure TestIfBlock;
    Procedure TestIfAssignment;
    Procedure TestIfElse;
    Procedure TestIfElseBlock;
    Procedure TestIfSemiColonElseError;
    Procedure TestNestedIf;
    Procedure TestNestedIfElse;
    Procedure TestWhile;
    Procedure TestWhileBlock;
    Procedure TestWhileNested;
    Procedure TestRepeat;
    Procedure TestRepeatBlock;
    procedure TestRepeatBlockNosemicolon;
    Procedure TestRepeatNested;
    Procedure TestFor;
    Procedure TestForIn;
    Procedure TestForExpr;
    Procedure TestForBlock;
    procedure TestDowntoBlock;
    Procedure TestForNested;
    Procedure TestWith;
    Procedure TestWithMultiple;
    Procedure TestCaseEmpty;
    Procedure TestCaseOneInteger;
    Procedure TestCaseTwoIntegers;
    Procedure TestCaseRange;
    Procedure TestCaseRangeSeparate;
    Procedure TestCase2Cases;
    Procedure TestCaseBlock;
    Procedure TestCaseElseBlockEmpty;
    Procedure TestCaseElseBlockAssignment;
    Procedure TestCaseElseBlock2Assignments;
    Procedure TestCaseIfCaseElse;
    Procedure TestCaseIfElse;
    Procedure TestRaise;
    Procedure TestRaiseEmpty;
    Procedure TestRaiseAt;
    Procedure TestTryFinally;
    Procedure TestTryFinallyEmpty;
    Procedure TestTryFinallyNested;
    procedure TestTryExcept;
    procedure TestTryExceptNested;
    procedure TestTryExceptEmpty;
    Procedure TestTryExceptOn;
    Procedure TestTryExceptOn2;
    Procedure TestTryExceptOnElse;
    Procedure TestTryExceptOnIfElse;
    Procedure TestAsm;
  end;

implementation

{ TTestStatementParser }

procedure TTestStatementParser.SetUp;
begin
  inherited SetUp;
  FVariables:=TStringList.Create;
end;

procedure TTestStatementParser.TearDown;
begin
  FreeAndNil(FVariables);
  inherited TearDown;
end;

procedure TTestStatementParser.AddStatements(ASource: array of string);

Var
  I :Integer;
begin
  StartProgram('afile');
  if FVariables.Count>0 then
    begin
    Add('Var');
    For I:=0 to FVariables.Count-1 do
      Add('  '+Fvariables[I]);
    end;
  Add('begin');
  For I:=Low(ASource) to High(ASource) do
    Add('  '+ASource[i]);
end;

procedure TTestStatementParser.DeclareVar(const AVarType: String;
  const AVarName: String);
begin
  FVariables.Add(AVarName+' : '+AVarType+';');
end;

function TTestStatementParser.TestStatement(ASource: string): TPasImplElement;
begin
  Result:=TestStatement([ASource]);
end;

function TTestStatementParser.TestStatement(ASource: array of string
  ): TPasImplElement;


begin
  Result:=Nil;
  FStatement:=Nil;
  AddStatements(ASource);
  ParseModule;
  AssertEquals('Have program',TPasProgram,Module.ClassType);
  AssertNotNull('Have program section',PasProgram.ProgramSection);
  AssertNotNull('Have initialization section',PasProgram.InitializationSection);
  if (PasProgram.InitializationSection.Elements.Count>0) then
    if TObject(PasProgram.InitializationSection.Elements[0]) is TPasImplBlock then
      FStatement:=TPasImplBlock(PasProgram.InitializationSection.Elements[0]);
  Result:=FStatement;
end;

procedure TTestStatementParser.ExpectParserError(const Msg: string);
begin
  AssertException(Msg,EParserError,@ParseModule);
end;

procedure TTestStatementParser.ExpectParserError(const Msg: string;
  ASource: array of string);
begin
  AddStatements(ASource);
  ExpectParserError(Msg);
end;

function TTestStatementParser.AssertStatement(Msg: String; AClass: TClass;
  AIndex: Integer): TPasImplBlock;
begin
  if not (AIndex<PasProgram.InitializationSection.Elements.Count) then
    Fail(Msg+': No such statement : '+intTostr(AIndex));
  AssertNotNull(Msg+' Have statement',PasProgram.InitializationSection.Elements[AIndex]);
  AssertEquals(Msg+' statement class',AClass,TObject(PasProgram.InitializationSection.Elements[AIndex]).ClassType);
  Result:=TObject(PasProgram.InitializationSection.Elements[AIndex]) as TPasImplBlock;
end;

procedure TTestStatementParser.TestEmpty;
begin
  //TestStatement(';');
  TestStatement('');
  AssertEquals('No statements',0,PasProgram.InitializationSection.Elements.Count);
end;

procedure TTestStatementParser.TestEmptyStatement;
begin
  TestStatement(';');
  AssertEquals('0 statement',0,PasProgram.InitializationSection.Elements.Count);
end;

procedure TTestStatementParser.TestEmptyStatements;
begin
  TestStatement(';;');
  AssertEquals('0 statement',0,PasProgram.InitializationSection.Elements.Count);
end;

procedure TTestStatementParser.TestBlock;

Var
  B : TPasImplBeginBlock;

begin
  TestStatement(['begin','end']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertNotNull('Statement assigned',PasProgram.InitializationSection.Elements[0]);
  AssertEquals('Block statement',TPasImplBeginBlock,Statement.ClassType);
  B:= Statement as TPasImplBeginBlock;
  AssertEquals('Empty block',0,B.Elements.Count);
end;

procedure TTestStatementParser.TestBlockComment;
Var
  B : TPasImplBeginBlock;

begin
  Engine.NeedComments:=True;
  TestStatement(['{ This is a comment }','begin','end']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertNotNull('Statement assigned',PasProgram.InitializationSection.Elements[0]);
  AssertEquals('Block statement',TPasImplBeginBlock,Statement.ClassType);
  B:= Statement as TPasImplBeginBlock;
  AssertEquals('Empty block',0,B.Elements.Count);
  AssertEquals('No DocComment','',B.DocComment);
end;

procedure TTestStatementParser.TestBlock2Comments;
Var
  B : TPasImplBeginBlock;

begin
  Engine.NeedComments:=True;
  TestStatement(['{ This is a comment }','// Another comment','begin','end']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertNotNull('Statement assigned',PasProgram.InitializationSection.Elements[0]);
  AssertEquals('Block statement',TPasImplBeginBlock,Statement.ClassType);
  B:= Statement as TPasImplBeginBlock;
  AssertEquals('Empty block',0,B.Elements.Count);
  AssertEquals('No DocComment','',B.DocComment);
end;

procedure TTestStatementParser.TestAssignment;

Var
  A : TPasImplAssign;

begin
  DeclareVar('integer');
  TestStatement(['a:=1;']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Assignment statement',TPasImplAssign,Statement.ClassType);
  A:=Statement as TPasImplAssign;
  AssertEquals('Normal assignment',akDefault,A.Kind);
  AssertExpression('Right side is constant',A.Right,pekNumber,'1');
  AssertExpression('Left side is variable',A.Left,pekIdent,'a');
end;

procedure TTestStatementParser.TestAssignmentAdd;

Var
  A : TPasImplAssign;

begin
  Parser.Scanner.Options:=[po_cassignments];
  DeclareVar('integer');
  TestStatement(['a+=1;']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Assignment statement',TPasImplAssign,Statement.ClassType);
  A:=Statement as TPasImplAssign;
  AssertEquals('Add assignment',akAdd,A.Kind);
  AssertExpression('Right side is constant',A.Right,pekNumber,'1');
  AssertExpression('Left side is variable',A.Left,pekIdent,'a');
end;

procedure TTestStatementParser.TestAssignmentMinus;
Var
  A : TPasImplAssign;

begin
  Parser.Scanner.Options:=[po_cassignments];
  DeclareVar('integer');
  TestStatement(['a-=1;']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Assignment statement',TPasImplAssign,Statement.ClassType);
  A:=Statement as TPasImplAssign;
  AssertEquals('Minus assignment',akMinus,A.Kind);
  AssertExpression('Right side is constant',A.Right,pekNumber,'1');
  AssertExpression('Left side is variable',A.Left,pekIdent,'a');
end;

procedure TTestStatementParser.TestAssignmentMul;
Var
  A : TPasImplAssign;

begin
  Parser.Scanner.Options:=[po_cassignments];
  DeclareVar('integer');
  TestStatement(['a*=1;']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Assignment statement',TPasImplAssign,Statement.ClassType);
  A:=Statement as TPasImplAssign;
  AssertEquals('Mul assignment',akMul,A.Kind);
  AssertExpression('Right side is constant',A.Right,pekNumber,'1');
  AssertExpression('Left side is variable',A.Left,pekIdent,'a');
end;

procedure TTestStatementParser.TestAssignmentDivision;
Var
  A : TPasImplAssign;

begin
  Parser.Scanner.Options:=[po_cassignments];
  DeclareVar('integer');
  TestStatement(['a/=1;']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Assignment statement',TPasImplAssign,Statement.ClassType);
  A:=Statement as TPasImplAssign;
  AssertEquals('Division assignment',akDivision,A.Kind);
  AssertExpression('Right side is constant',A.Right,pekNumber,'1');
  AssertExpression('Left side is variable',A.Left,pekIdent,'a');
end;

procedure TTestStatementParser.TestCall;

Var
  S : TPasImplSimple;

begin
  TestStatement('Doit;');
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,Statement.ClassType);
  S:=Statement as TPasImplSimple;
  AssertExpression('Doit call',S.Expr,pekIdent,'Doit');
end;

procedure TTestStatementParser.TestCallComment;

Var
  S : TPasImplSimple;
begin
  Engine.NeedComments:=True;
  TestStatement(['//comment line','Doit;']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,Statement.ClassType);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  S:=Statement as TPasImplSimple;
  AssertExpression('Doit call',S.Expr,pekIdent,'Doit');
  AssertEquals('No DocComment','',S.DocComment);
end;

procedure TTestStatementParser.TestCallQualified;

Var
  S : TPasImplSimple;
  B : TBinaryExpr;

begin
  TestStatement('Unita.Doit;');
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,Statement.ClassType);
  S:=Statement as TPasImplSimple;
  AssertExpression('Doit call',S.Expr,pekBinary,TBinaryExpr);
  B:=S.Expr as TBinaryExpr;
  AssertExpression('Unit name',B.Left,pekIdent,'Unita');
  AssertExpression('Doit call',B.Right,pekIdent,'Doit');

end;

procedure TTestStatementParser.TestCallQualified2;
Var
  S : TPasImplSimple;
  B : TBinaryExpr;

begin
  TestStatement('Unita.ClassB.Doit;');
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,Statement.ClassType);
  S:=Statement as TPasImplSimple;
  AssertExpression('Doit call',S.Expr,pekBinary,TBinaryExpr);
  B:=S.Expr as TBinaryExpr;
  AssertExpression('Unit name',B.Left,pekIdent,'Unita');
  AssertExpression('Doit call',B.Right,pekBinary,TBinaryExpr);
  B:=B.Right  as TBinaryExpr;
  AssertExpression('Unit name',B.Left,pekIdent,'ClassB');
  AssertExpression('Doit call',B.Right,pekIdent,'Doit');
end;

procedure TTestStatementParser.TestCallNoArgs;

Var
  S : TPasImplSimple;
  P : TParamsExpr;

begin
  TestStatement('Doit();');
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,Statement.ClassType);
  S:=Statement as TPasImplSimple;
  AssertExpression('Doit call',S.Expr,pekFuncParams,TParamsExpr);
  P:=S.Expr as TParamsExpr;
  AssertExpression('Correct function call name',P.Value,pekIdent,'Doit');
  AssertEquals('No params',0,Length(P.Params));
end;

procedure TTestStatementParser.TestCallOneArg;
Var
  S : TPasImplSimple;
  P : TParamsExpr;

begin
  TestStatement('Doit(1);');
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,Statement.ClassType);
  S:=Statement as TPasImplSimple;
  AssertExpression('Doit call',S.Expr,pekFuncParams,TParamsExpr);
  P:=S.Expr as TParamsExpr;
  AssertExpression('Correct function call name',P.Value,pekIdent,'Doit');
  AssertEquals('One param',1,Length(P.Params));
  AssertExpression('Parameter is constant',P.Params[0],pekNumber,'1');
end;

procedure TTestStatementParser.TestIf;

Var
  I : TPasImplIfElse;

begin
  DeclareVar('boolean');
  TestStatement(['if a then',';']);
  I:=AssertStatement('If statement',TPasImplIfElse) as TPasImplIfElse;
  AssertExpression('IF condition',I.ConditionExpr,pekIdent,'a');
  AssertNull('No else',i.ElseBranch);
  AssertNull('No if branch',I.IfBranch);
end;

procedure TTestStatementParser.TestIfBlock;

Var
  I : TPasImplIfElse;

begin
  DeclareVar('boolean');
  TestStatement(['if a then','  begin','  end']);
  I:=AssertStatement('If statement',TPasImplIfElse) as TPasImplIfElse;
  AssertExpression('IF condition',I.ConditionExpr,pekIdent,'a');
  AssertNull('No else',i.ElseBranch);
  AssertNotNull('if branch',I.IfBranch);
  AssertEquals('begin end block',TPasImplBeginBlock,I.ifBranch.ClassType);
end;

procedure TTestStatementParser.TestIfAssignment;

Var
  I : TPasImplIfElse;

begin
  DeclareVar('boolean');
  TestStatement(['if a then','  a:=False;']);
  I:=AssertStatement('If statement',TPasImplIfElse) as TPasImplIfElse;
  AssertExpression('IF condition',I.ConditionExpr,pekIdent,'a');
  AssertNull('No else',i.ElseBranch);
  AssertNotNull('if branch',I.IfBranch);
  AssertEquals('assignment statement',TPasImplAssign,I.ifBranch.ClassType);
end;

procedure TTestStatementParser.TestIfElse;

Var
  I : TPasImplIfElse;

begin
  DeclareVar('boolean');
  TestStatement(['if a then','  begin','  end','else',';']);
  I:=AssertStatement('If statement',TPasImplIfElse) as TPasImplIfElse;
  AssertExpression('IF condition',I.ConditionExpr,pekIdent,'a');
  AssertNull('No else',i.ElseBranch);
  AssertNotNull('if branch',I.IfBranch);
  AssertEquals('begin end block',TPasImplBeginBlock,I.ifBranch.ClassType);
end;

procedure TTestStatementParser.TestIfElseBlock;
Var
  I : TPasImplIfElse;

begin
  DeclareVar('boolean');
  TestStatement(['if a then','  begin','  end','else','  begin','  end']);
  I:=AssertStatement('If statement',TPasImplIfElse) as TPasImplIfElse;
  AssertExpression('IF condition',I.ConditionExpr,pekIdent,'a');
  AssertNotNull('if branch',I.IfBranch);
  AssertEquals('begin end block',TPasImplBeginBlock,I.ifBranch.ClassType);
  AssertNotNull('Else branch',i.ElseBranch);
  AssertEquals('begin end block',TPasImplBeginBlock,I.ElseBranch.ClassType);
end;

procedure TTestStatementParser.TestIfSemiColonElseError;

begin
  DeclareVar('boolean');
  ExpectParserError('No semicolon before else',['if a then','  begin','  end;','else','  begin','  end']);
end;

procedure TTestStatementParser.TestNestedIf;
Var
  I : TPasImplIfElse;
begin
  DeclareVar('boolean');
  DeclareVar('boolean','b');
  TestStatement(['if a then','  if b then','    begin','    end','else','  begin','  end']);
  I:=AssertStatement('If statement',TPasImplIfElse) as TPasImplIfElse;
  AssertExpression('IF condition',I.ConditionExpr,pekIdent,'a');
  AssertNotNull('if branch',I.IfBranch);
  AssertNull('Else branch',i.ElseBranch);
  AssertEquals('if in if branch',TPasImplIfElse,I.ifBranch.ClassType);
  I:=I.Ifbranch as TPasImplIfElse;
  AssertEquals('begin end block',TPasImplBeginBlock,I.ElseBranch.ClassType);

end;

procedure TTestStatementParser.TestNestedIfElse;

Var
  I : TPasImplIfElse;

begin
  DeclareVar('boolean');
  TestStatement(['if a then','  if b then','    begin','    end','  else','    begin','    end','else','  begin','end']);
  I:=AssertStatement('If statement',TPasImplIfElse) as TPasImplIfElse;
  AssertExpression('IF condition',I.ConditionExpr,pekIdent,'a');
  AssertNotNull('if branch',I.IfBranch);
  AssertNotNull('Else branch',i.ElseBranch);
  AssertEquals('begin end block',TPasImplBeginBlock,I.ElseBranch.ClassType);
  AssertEquals('if in if branch',TPasImplIfElse,I.ifBranch.ClassType);
  I:=I.Ifbranch as TPasImplIfElse;
  AssertEquals('begin end block',TPasImplBeginBlock,I.ElseBranch.ClassType);
end;

procedure TTestStatementParser.TestWhile;

Var
  W : TPasImplWhileDo;

begin
  DeclareVar('boolean');
  TestStatement(['While a do ;']);
  W:=AssertStatement('While statement',TPasImplWhileDo) as TPasImplWhileDo;
  AssertExpression('While condition',W.ConditionExpr,pekIdent,'a');
  AssertNull('Empty body',W.Body);
end;

procedure TTestStatementParser.TestWhileBlock;
Var
  W : TPasImplWhileDo;

begin
  DeclareVar('boolean');
  TestStatement(['While a do','  begin','  end']);
  W:=AssertStatement('While statement',TPasImplWhileDo) as TPasImplWhileDo;
  AssertExpression('While condition',W.ConditionExpr,pekIdent,'a');
  AssertNotNull('Have while body',W.Body);
  AssertEquals('begin end block',TPasImplBeginBlock,W.Body.ClassType);
  AssertEquals('Empty block',0,TPasImplBeginBlock(W.Body).ELements.Count);
end;

procedure TTestStatementParser.TestWhileNested;

Var
  W : TPasImplWhileDo;

begin
  DeclareVar('boolean');
  DeclareVar('boolean','b');
  TestStatement(['While a do','  while b do','    begin','    end']);
  W:=AssertStatement('While statement',TPasImplWhileDo) as TPasImplWhileDo;
  AssertExpression('While condition',W.ConditionExpr,pekIdent,'a');
  AssertNotNull('Have while body',W.Body);
  AssertEquals('Nested while',TPasImplWhileDo,W.Body.ClassType);
  W:=W.Body as TPasImplWhileDo;
  AssertExpression('While condition',W.ConditionExpr,pekIdent,'b');
  AssertNotNull('Have nested while body',W.Body);
  AssertEquals('Nested begin end block',TPasImplBeginBlock,W.Body.ClassType);
  AssertEquals('Empty nested block',0,TPasImplBeginBlock(W.Body).ELements.Count);
end;

procedure TTestStatementParser.TestRepeat;

Var
  R : TPasImplRepeatUntil;

begin
  DeclareVar('boolean');
  TestStatement(['Repeat','Until a;']);
  R:=AssertStatement('Repeat statement',TPasImplRepeatUntil) as TPasImplRepeatUntil;
  AssertExpression('repeat condition',R.ConditionExpr,pekIdent,'a');
  AssertEquals('Empty body',0,R.Elements.Count);
end;

procedure TTestStatementParser.TestRepeatBlock;

Var
  R : TPasImplRepeatUntil;

begin
  DeclareVar('boolean');
  TestStatement(['Repeat','begin','end;','Until a;']);
  R:=AssertStatement('repeat statement',TPasImplRepeatUntil) as TPasImplRepeatUntil;
  AssertExpression('repeat condition',R.ConditionExpr,pekIdent,'a');
  AssertEquals('Have statement',1,R.Elements.Count);
  AssertEquals('begin end block',TPasImplBeginBlock,TObject(R.Elements[0]).ClassType);
  AssertEquals('Empty block',0,TPasImplBeginBlock(R.Elements[0]).ELements.Count);
end;

procedure TTestStatementParser.TestRepeatBlockNosemicolon;

Var
  R : TPasImplRepeatUntil;

begin
  DeclareVar('boolean');
  TestStatement(['Repeat','begin','end','Until a;']);
  R:=AssertStatement('repeat statement',TPasImplRepeatUntil) as TPasImplRepeatUntil;
  AssertExpression('repeat condition',R.ConditionExpr,pekIdent,'a');
  AssertEquals('Have statement',1,R.Elements.Count);
  AssertEquals('begin end block',TPasImplBeginBlock,TObject(R.Elements[0]).ClassType);
  AssertEquals('Empty block',0,TPasImplBeginBlock(R.Elements[0]).ELements.Count);
end;

procedure TTestStatementParser.TestRepeatNested;

Var
  R : TPasImplRepeatUntil;

begin
  DeclareVar('boolean');
  DeclareVar('boolean','b');
  TestStatement(['Repeat','repeat','begin','end','until b','Until a;']);
  R:=AssertStatement('repeat statement',TPasImplRepeatUntil) as TPasImplRepeatUntil;
  AssertExpression('repeat condition',R.ConditionExpr,pekIdent,'a');
  AssertEquals('Have statement',1,R.Elements.Count);
  AssertEquals('Nested repeat',TPasImplRepeatUntil,TObject(R.Elements[0]).ClassType);
  R:=TPasImplRepeatUntil(R.Elements[0]);
  AssertExpression('repeat condition',R.ConditionExpr,pekIdent,'b');
  AssertEquals('Have statement',1,R.Elements.Count);
  AssertEquals('begin end block',TPasImplBeginBlock,TObject(R.Elements[0]).ClassType);
  AssertEquals('Empty block',0,TPasImplBeginBlock(R.Elements[0]).ELements.Count);
end;

procedure TTestStatementParser.TestFor;

Var
  F : TPasImplForLoop;

begin
  DeclareVar('integer');
  TestStatement(['For a:=1 to 10 do',';']);
  F:=AssertStatement('For statement',TPasImplForLoop) as TPasImplForLoop;
  AssertEquals('Loop variable name','a',F.VariableName);
  AssertEquals('Loop type',ltNormal,F.Looptype);
  AssertEquals('Up loop',False,F.Down);
  AssertExpression('Start value',F.StartExpr,pekNumber,'1');
  AssertExpression('End value',F.EndExpr,pekNumber,'10');
  AssertNull('Empty body',F.Body);
end;

procedure TTestStatementParser.TestForIn;

Var
  F : TPasImplForLoop;

begin
  DeclareVar('integer');
  TestStatement(['For a in SomeSet Do',';']);
  F:=AssertStatement('For statement',TPasImplForLoop) as TPasImplForLoop;
  AssertEquals('Loop variable name','a',F.VariableName);
  AssertEquals('Loop type',ltIn,F.Looptype);
  AssertEquals('In loop',False,F.Down);
  AssertExpression('Start value',F.StartExpr,pekIdent,'SomeSet');
  AssertNull('Loop type',F.EndExpr);
  AssertNull('Empty body',F.Body);
end;

procedure TTestStatementParser.TestForExpr;
Var
  F : TPasImplForLoop;
  B : TBinaryExpr;

begin
  DeclareVar('integer');
  TestStatement(['For a:=1+1 to 5+5 do',';']);
  F:=AssertStatement('For statement',TPasImplForLoop) as TPasImplForLoop;
  AssertEquals('Loop variable name','a',F.VariableName);
  AssertEquals('Up loop',False,F.Down);
  AssertExpression('Start expression',F.StartExpr,pekBinary,TBinaryExpr);
  B:=F.StartExpr as TBinaryExpr;
  AssertExpression('Start value left',B.left,pekNumber,'1');
  AssertExpression('Start value right',B.right,pekNumber,'1');
  AssertExpression('Start expression',F.StartExpr,pekBinary,TBinaryExpr);
  B:=F.EndExpr as TBinaryExpr;
  AssertExpression('End value left',B.left,pekNumber,'5');
  AssertExpression('End value right',B.right,pekNumber,'5');
  AssertNull('Empty body',F.Body);
end;

procedure TTestStatementParser.TestForBlock;

Var
  F : TPasImplForLoop;

begin
  DeclareVar('integer');
  TestStatement(['For a:=1 to 10 do','begin','end']);
  F:=AssertStatement('For statement',TPasImplForLoop) as TPasImplForLoop;
  AssertEquals('Loop variable name','a',F.VariableName);
  AssertEquals('Up loop',False,F.Down);
  AssertExpression('Start value',F.StartExpr,pekNumber,'1');
  AssertExpression('End value',F.EndExpr,pekNumber,'10');
  AssertNotNull('Have for body',F.Body);
  AssertEquals('begin end block',TPasImplBeginBlock,F.Body.ClassType);
  AssertEquals('Empty block',0,TPasImplBeginBlock(F.Body).ELements.Count);
end;

procedure TTestStatementParser.TestDowntoBlock;

Var
  F : TPasImplForLoop;

begin
  DeclareVar('integer');
  TestStatement(['For a:=10 downto 1 do','begin','end']);
  F:=AssertStatement('For statement',TPasImplForLoop) as TPasImplForLoop;
  AssertEquals('Loop variable name','a',F.VariableName);
  AssertEquals('Down loop',True,F.Down);
  AssertExpression('Start value',F.StartExpr,pekNumber,'10');
  AssertExpression('End value',F.EndExpr,pekNumber,'1');
  AssertNotNull('Have for body',F.Body);
  AssertEquals('begin end block',TPasImplBeginBlock,F.Body.ClassType);
  AssertEquals('Empty block',0,TPasImplBeginBlock(F.Body).ELements.Count);
end;

procedure TTestStatementParser.TestForNested;
Var
  F : TPasImplForLoop;

begin
  DeclareVar('integer');
  DeclareVar('integer','b');
  TestStatement(['For a:=1 to 10 do','For b:=11 to 20 do','begin','end']);
  F:=AssertStatement('For statement',TPasImplForLoop) as TPasImplForLoop;
  AssertEquals('Loop variable name','a',F.VariableName);
  AssertEquals('Up loop',False,F.Down);
  AssertExpression('Start value',F.StartExpr,pekNumber,'1');
  AssertExpression('End value',F.EndExpr,pekNumber,'10');
  AssertNotNull('Have while body',F.Body);
  AssertEquals('begin end block',TPasImplForLoop,F.Body.ClassType);
  F:=F.Body as TPasImplForLoop;
  AssertEquals('Loop variable name','b',F.VariableName);
  AssertEquals('Up loop',False,F.Down);
  AssertExpression('Start value',F.StartExpr,pekNumber,'11');
  AssertExpression('End value',F.EndExpr,pekNumber,'20');
  AssertNotNull('Have for body',F.Body);
  AssertEquals('begin end block',TPasImplBeginBlock,F.Body.ClassType);
  AssertEquals('Empty block',0,TPasImplBeginBlock(F.Body).ELements.Count);
end;

procedure TTestStatementParser.TestWith;

Var
  W : TpasImplWithDo;

begin
  DeclareVar('record X,Y : Integer; end');
  TestStatement(['With a do','begin','end']);
  W:=AssertStatement('For statement',TpasImplWithDo) as TpasImplWithDo;
  AssertEquals('1 expression',1,W.Expressions.Count);
  AssertExpression('With identifier',TPasExpr(W.Expressions[0]),pekIdent,'a');
  AssertNotNull('Have with body',W.Body);
  AssertEquals('begin end block',TPasImplBeginBlock,W.Body.ClassType);
  AssertEquals('Empty block',0,TPasImplBeginBlock(W.Body).ELements.Count);
end;

procedure TTestStatementParser.TestWithMultiple;
Var
  W : TpasImplWithDo;

begin
  DeclareVar('record X,Y : Integer; end');
  DeclareVar('record W,Z : Integer; end','b');
  TestStatement(['With a,b do','begin','end']);
  W:=AssertStatement('For statement',TpasImplWithDo) as TpasImplWithDo;
  AssertEquals('2 expressions',2,W.Expressions.Count);
  AssertExpression('With identifier 1',TPasExpr(W.Expressions[0]),pekIdent,'a');
  AssertExpression('With identifier 2',TPasExpr(W.Expressions[1]),pekIdent,'b');
  AssertNotNull('Have with body',W.Body);
  AssertEquals('begin end block',TPasImplBeginBlock,W.Body.ClassType);
  AssertEquals('Empty block',0,TPasImplBeginBlock(W.Body).ELements.Count);
end;

procedure TTestStatementParser.TestCaseEmpty;
begin
  DeclareVar('integer');
  AddStatements(['case a of','end;']);
  ExpectParserError('Empty case not allowed');
end;

procedure TTestStatementParser.TestCaseOneInteger;

Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;

begin
  DeclareVar('integer');
  TestStatement(['case a of','1 : ;','end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertNull('No else branch',C.ElseBranch);
  AssertEquals('One case label',1,C.Elements.Count);
  AssertEquals('Correct case for case label',TPasImplCaseStatement,TPasElement(C.Elements[0]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('1 expression for case',1,S.Expressions.Count);
  AssertExpression('With identifier 1',TPasExpr(S.Expressions[0]),pekNumber,'1');
  AssertEquals('Empty case label statement',0,S.Elements.Count);
  AssertNull('Empty case label statement',S.Body);
end;

procedure TTestStatementParser.TestCaseTwoIntegers;

Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;

begin
  DeclareVar('integer');
  TestStatement(['case a of','1,2 : ;','end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertNull('No else branch',C.ElseBranch);
  AssertEquals('One case label',1,C.Elements.Count);
  AssertEquals('Correct case for case label',TPasImplCaseStatement,TPasElement(C.Elements[0]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('2 expressions for case',2,S.Expressions.Count);
  AssertExpression('With identifier 1',TPasExpr(S.Expressions[0]),pekNumber,'1');
  AssertExpression('With identifier 2',TPasExpr(S.Expressions[1]),pekNumber,'2');
  AssertEquals('Empty case label statement',0,S.Elements.Count);
  AssertNull('Empty case label statement',S.Body);
end;

procedure TTestStatementParser.TestCaseRange;
Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;

begin
  DeclareVar('integer');
  TestStatement(['case a of','1..3 : ;','end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertNull('No else branch',C.ElseBranch);
  AssertEquals('One case label',1,C.Elements.Count);
  AssertEquals('Correct case for case label',TPasImplCaseStatement,TPasElement(C.Elements[0]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('1 expression for case',1,S.Expressions.Count);
  AssertExpression('With identifier 1',TPasExpr(S.Expressions[0]),pekRange,TBinaryExpr);
  AssertEquals('Empty case label statement',0,S.Elements.Count);
  AssertNull('Empty case label statement',S.Body);
end;

procedure TTestStatementParser.TestCaseRangeSeparate;
Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;

begin
  DeclareVar('integer');
  TestStatement(['case a of','1..3,5 : ;','end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertNull('No else branch',C.ElseBranch);
  AssertEquals('One case label',1,C.Elements.Count);
  AssertEquals('Correct case for case label',TPasImplCaseStatement,TPasElement(C.Elements[0]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('2 expressions for case',2,S.Expressions.Count);
  AssertExpression('With identifier 1',TPasExpr(S.Expressions[0]),pekRange,TBinaryExpr);
  AssertExpression('With identifier 2',TPasExpr(S.Expressions[1]),pekNumber,'5');
  AssertEquals('Empty case label statement',0,S.Elements.Count);
  AssertNull('Empty case label statement',S.Body);
end;

procedure TTestStatementParser.TestCase2Cases;
Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;

begin
  DeclareVar('integer');
  TestStatement(['case a of','1 : ;','2 : ;','end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertNull('No else branch',C.ElseBranch);
  AssertEquals('Two case labels',2,C.Elements.Count);
  AssertEquals('Correct case for case label 1',TPasImplCaseStatement,TPasElement(C.Elements[0]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('2 expressions for case 1',1,S.Expressions.Count);
  AssertExpression('Case 1 With identifier 1',TPasExpr(S.Expressions[0]),pekNumber,'1');
  AssertEquals('Empty case label statement 1',0,S.Elements.Count);
  AssertNull('Empty case label statement 1',S.Body);
  // Two
  AssertEquals('Correct case for case label 2',TPasImplCaseStatement,TPasElement(C.Elements[1]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[1]);
  AssertEquals('2 expressions for case 2',1,S.Expressions.Count);
  AssertExpression('Case 2 With identifier 1',TPasExpr(S.Expressions[0]),pekNumber,'2');
  AssertEquals('Empty case label statement 2',0,S.Elements.Count);
  AssertNull('Empty case label statement 2',S.Body);
end;

procedure TTestStatementParser.TestCaseBlock;

Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;
  B : TPasImplbeginBlock;

begin
  DeclareVar('integer');
  TestStatement(['case a of','1 : begin end;','end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertNull('No else branch',C.ElseBranch);
  AssertEquals('Two case labels',1,C.Elements.Count);
  AssertEquals('Correct case for case label 1',TPasImplCaseStatement,TPasElement(C.Elements[0]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('2 expressions for case 1',1,S.Expressions.Count);
  AssertExpression('Case With identifier 1',TPasExpr(S.Expressions[0]),pekNumber,'1');
  AssertEquals('1 case label statement',1,S.Elements.Count);
  AssertEquals('Correct case for case label 1',TPasImplbeginBlock,TPasElement(S.Elements[0]).ClassType);
  B:=TPasImplbeginBlock(S.Elements[0]);
  AssertEquals('0 statements in block',0,B.Elements.Count);

end;

procedure TTestStatementParser.TestCaseElseBlockEmpty;

Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;
  B : TPasImplbeginBlock;

begin
  DeclareVar('integer');
  TestStatement(['case a of','1 : begin end;','else',' end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertEquals('Two case labels',2,C.Elements.Count);
  AssertEquals('Correct case for case label 1',TPasImplCaseStatement,TPasElement(C.Elements[0]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('2 expressions for case 1',1,S.Expressions.Count);
  AssertExpression('Case With identifier 1',TPasExpr(S.Expressions[0]),pekNumber,'1');
  AssertEquals('1 case label statement',1,S.Elements.Count);
  AssertEquals('Correct case for case label 1',TPasImplbeginBlock,TPasElement(S.Elements[0]).ClassType);
  B:=TPasImplbeginBlock(S.Elements[0]);
  AssertEquals('0 statements in block',0,B.Elements.Count);
  AssertNotNull('Have else branch',C.ElseBranch);
  AssertEquals('Correct else branch class',TPasImplCaseElse,C.ElseBranch.ClassType);
  AssertEquals('Zero statements ',0,TPasImplCaseElse(C.ElseBranch).Elements.Count);
end;

procedure TTestStatementParser.TestCaseElseBlockAssignment;
Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;
  B : TPasImplbeginBlock;

begin
  DeclareVar('integer');
  TestStatement(['case a of','1 : begin end;','else','a:=1',' end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertEquals('Two case labels',2,C.Elements.Count);
  AssertEquals('Correct case for case label 1',TPasImplCaseStatement,TPasElement(C.Elements[0]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('2 expressions for case 1',1,S.Expressions.Count);
  AssertExpression('Case With identifier 1',TPasExpr(S.Expressions[0]),pekNumber,'1');
  AssertEquals('1 case label statement',1,S.Elements.Count);
  AssertEquals('Correct case for case label 1',TPasImplbeginBlock,TPasElement(S.Elements[0]).ClassType);
  B:=TPasImplbeginBlock(S.Elements[0]);
  AssertEquals('0 statements in block',0,B.Elements.Count);
  AssertNotNull('Have else branch',C.ElseBranch);
  AssertEquals('Correct else branch class',TPasImplCaseElse,C.ElseBranch.ClassType);
  AssertEquals('1 statement in else branch ',1,TPasImplCaseElse(C.ElseBranch).Elements.Count);
end;

procedure TTestStatementParser.TestCaseElseBlock2Assignments;

Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;
  B : TPasImplbeginBlock;

begin
  DeclareVar('integer');
  TestStatement(['case a of','1 : begin end;','else','a:=1;','a:=32;',' end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertEquals('Two case labels',2,C.Elements.Count);
  AssertEquals('Correct case for case label 1',TPasImplCaseStatement,TPasElement(C.Elements[0]).ClassType);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('2 expressions for case 1',1,S.Expressions.Count);
  AssertExpression('Case With identifier 1',TPasExpr(S.Expressions[0]),pekNumber,'1');
  AssertEquals('1 case label statement',1,S.Elements.Count);
  AssertEquals('Correct case for case label 1',TPasImplbeginBlock,TPasElement(S.Elements[0]).ClassType);
  B:=TPasImplbeginBlock(S.Elements[0]);
  AssertEquals('0 statements in block',0,B.Elements.Count);
  AssertNotNull('Have else branch',C.ElseBranch);
  AssertEquals('Correct else branch class',TPasImplCaseElse,C.ElseBranch.ClassType);
  AssertEquals('2 statements in else branch ',2,TPasImplCaseElse(C.ElseBranch).Elements.Count);
end;

procedure TTestStatementParser.TestCaseIfCaseElse;

Var
  C : TPasImplCaseOf;

begin
  DeclareVar('integer');
  DeclareVar('boolean','b');
  TestStatement(['case a of','1 : if b then',' begin end;','else',' end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertEquals('Two case labels',2,C.Elements.Count);
  AssertNotNull('Have else branch',C.ElseBranch);
  AssertEquals('Correct else branch class',TPasImplCaseElse,C.ElseBranch.ClassType);
  AssertEquals('0 statement in else branch ',0,TPasImplCaseElse(C.ElseBranch).Elements.Count);
end;

procedure TTestStatementParser.TestCaseIfElse;
Var
  C : TPasImplCaseOf;
  S : TPasImplCaseStatement;

begin
  DeclareVar('integer');
  DeclareVar('boolean','b');
  TestStatement(['case a of','1 : if b then',' begin end','else','begin','end',' end;']);
  C:=AssertStatement('Case statement',TpasImplCaseOf) as TpasImplCaseOf;
  AssertNotNull('Have case expression',C.CaseExpr);
  AssertExpression('Case expression',C.CaseExpr,pekIdent,'a');
  AssertEquals('Two case labels',1,C.Elements.Count);
  AssertNull('Have no else branch',C.ElseBranch);
  S:=TPasImplCaseStatement(C.Elements[0]);
  AssertEquals('2 expressions for case 1',1,S.Expressions.Count);
  AssertExpression('Case With identifier 1',TPasExpr(S.Expressions[0]),pekNumber,'1');
  AssertEquals('1 case label statement',1,S.Elements.Count);
  AssertEquals('If statement in case label 1',TPasImplIfElse,TPasElement(S.Elements[0]).ClassType);
  AssertNotNull('If statement has else block',TPasImplIfElse(S.Elements[0]).ElseBranch);
end;

procedure TTestStatementParser.TestRaise;

Var
  R : TPasImplRaise;

begin
  DeclareVar('Exception');
  TestStatement('Raise A;');
  R:=AssertStatement('Raise statement',TPasImplRaise) as TPasImplRaise;
  AssertEquals(0,R.Elements.Count);
  AssertNotNull(R.ExceptObject);
  AssertNull(R.ExceptAddr);
  AssertExpression('Expression object',R.ExceptObject,pekIdent,'A');
end;

procedure TTestStatementParser.TestRaiseEmpty;
Var
  R : TPasImplRaise;

begin
  TestStatement('Raise;');
  R:=AssertStatement('Raise statement',TPasImplRaise) as TPasImplRaise;
  AssertEquals(0,R.Elements.Count);
  AssertNull(R.ExceptObject);
  AssertNull(R.ExceptAddr);
end;

procedure TTestStatementParser.TestRaiseAt;

Var
  R : TPasImplRaise;

begin
  DeclareVar('Exception');
  DeclareVar('Pointer','B');
  TestStatement('Raise A at B;');
  R:=AssertStatement('Raise statement',TPasImplRaise) as TPasImplRaise;
  AssertEquals(0,R.Elements.Count);
  AssertNotNull(R.ExceptObject);
  AssertNotNull(R.ExceptAddr);
  AssertExpression('Expression object',R.ExceptAddr,pekIdent,'B');
end;

procedure TTestStatementParser.TestTryFinally;

Var
  T : TPasImplTry;
  S : TPasImplSimple;
  F : TPasImplTryFinally;

begin
  TestStatement(['Try','  DoSomething;','finally','  DoSomethingElse','end']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(1,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomething');
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Finally statement',TPasImplTryFinally,T.FinallyExcept.ClassType);
  F:=TPasImplTryFinally(T.FinallyExcept);
  AssertEquals(1,F.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(F.Elements[0]).ClassType);
  S:=TPasImplSimple(F.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse');
end;

procedure TTestStatementParser.TestTryFinallyEmpty;
Var
  T : TPasImplTry;
  F : TPasImplTryFinally;

begin
  TestStatement(['Try','finally','end;']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(0,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertEquals('Finally statement',TPasImplTryFinally,T.FinallyExcept.ClassType);
  F:=TPasImplTryFinally(T.FinallyExcept);
  AssertEquals(0,F.Elements.Count);
end;

procedure TTestStatementParser.TestTryFinallyNested;
Var
  T : TPasImplTry;
  S : TPasImplSimple;
  F : TPasImplTryFinally;

begin
  TestStatement(['Try','  DoSomething1;','  Try','    DoSomething2;','  finally','    DoSomethingElse2','  end;','Finally','  DoSomethingElse1','end']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(2,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomething1');
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Finally statement',TPasImplTryFinally,T.FinallyExcept.ClassType);
  F:=TPasImplTryFinally(T.FinallyExcept);
  AssertEquals(1,F.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(F.Elements[0]).ClassType);
  S:=TPasImplSimple(F.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse1');
  // inner statement
  AssertNotNull(T.Elements[1]);
  AssertEquals('Nested try statement',TPasImplTry,TPasElement(T.Elements[1]).ClassType);
  T:=TPasImplTry(T.Elements[1]);
  AssertEquals(1,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomething2');
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Finally statement',TPasImplTryFinally,T.FinallyExcept.ClassType);
  F:=TPasImplTryFinally(T.FinallyExcept);
  AssertEquals(1,F.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(F.Elements[0]).ClassType);
  S:=TPasImplSimple(F.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse2');
end;

procedure TTestStatementParser.TestTryExcept;

Var
  T : TPasImplTry;
  S : TPasImplSimple;
  E : TPasImplTryExcept;

begin
  TestStatement(['Try','  DoSomething;','except','  DoSomethingElse','end']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(1,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomething');
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Except statement',TPasImplTryExcept,T.FinallyExcept.ClassType);
  E:=TPasImplTryExcept(T.FinallyExcept);
  AssertEquals(1,E.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(E.Elements[0]).ClassType);
  S:=TPasImplSimple(E.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse');
end;

procedure TTestStatementParser.TestTryExceptNested;
Var
  T : TPasImplTry;
  S : TPasImplSimple;
  E : TPasImplTryExcept;

begin
  TestStatement(['Try','  DoSomething1;','  try','    DoSomething2;','  except','    DoSomethingElse2','  end','except','  DoSomethingElse1','end']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(2,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomething1');
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Except statement',TPasImplTryExcept,T.FinallyExcept.ClassType);
  E:=TPasImplTryExcept(T.FinallyExcept);
  AssertEquals(1,E.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(E.Elements[0]).ClassType);
  S:=TPasImplSimple(E.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse1');
  AssertNotNull(T.Elements[1]);
  AssertEquals('Simple statement',TPasImplTry,TPasElement(T.Elements[1]).ClassType);
  T:=TPasImplTry(T.Elements[1]);
  AssertEquals(1,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement 2',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething2 call ',S.Expr,pekIdent,'DoSomething2');
  AssertEquals('Simple statement2',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Except statement2',TPasImplTryExcept,T.FinallyExcept.ClassType);
  E:=TPasImplTryExcept(T.FinallyExcept);
  AssertEquals(1,E.Elements.Count);
  AssertEquals('Simple statement2',TPasImplSimple,TPasElement(E.Elements[0]).ClassType);
  S:=TPasImplSimple(E.Elements[0]);
  AssertExpression('DoSomethingElse2 call',S.Expr,pekIdent,'DoSomethingElse2');
end;

procedure TTestStatementParser.TestTryExceptEmpty;

Var
  T : TPasImplTry;
  E : TPasImplTryExcept;

begin
  TestStatement(['Try','except','end;']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(0,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertEquals('Except statement',TPasImplTryExcept,T.FinallyExcept.ClassType);
  E:=TPasImplTryExcept(T.FinallyExcept);
  AssertEquals(0,E.Elements.Count);
end;

procedure TTestStatementParser.TestTryExceptOn;

Var
  T : TPasImplTry;
  S : TPasImplSimple;
  E : TPasImplTryExcept;
  O : TPasImplExceptOn;

begin
  TestStatement(['Try','  DoSomething;','except','On E : Exception do','DoSomethingElse;','end']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(1,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomething');
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Except statement',TPasImplTryExcept,T.FinallyExcept.ClassType);
  E:=TPasImplTryExcept(T.FinallyExcept);
  AssertEquals(1,E.Elements.Count);
  AssertEquals('Except on handler',TPasImplExceptOn,TPasElement(E.Elements[0]).ClassType);
  O:=TPasImplExceptOn(E.Elements[0]);
  AssertEquals(1,O.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(O.Elements[0]).ClassType);
  AssertExpression('Exception Variable name',O.VarExpr,pekIdent,'E');
  AssertExpression('Exception Type name',O.TypeExpr,pekIdent,'Exception');
  S:=TPasImplSimple(O.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse');
//  AssertEquals('Variable name',

end;

procedure TTestStatementParser.TestTryExceptOn2;

Var
  T : TPasImplTry;
  S : TPasImplSimple;
  E : TPasImplTryExcept;
  O : TPasImplExceptOn;

begin
  TestStatement(['Try','  DoSomething;','except',
                 'On E : Exception do','DoSomethingElse;',
                 'On Y : Exception2 do','DoSomethingElse2;',
                 'end']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(1,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomething');
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Except statement',TPasImplTryExcept,T.FinallyExcept.ClassType);
  E:=TPasImplTryExcept(T.FinallyExcept);
  AssertEquals(2,E.Elements.Count);
  // Exception handler 1
  AssertEquals('Except on handler',TPasImplExceptOn,TPasElement(E.Elements[0]).ClassType);
  O:=TPasImplExceptOn(E.Elements[0]);
  AssertEquals(1,O.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(O.Elements[0]).ClassType);
  AssertExpression('Exception Variable name',O.VarExpr,pekIdent,'E');
  AssertExpression('Exception Type name',O.TypeExpr,pekIdent,'Exception');
  S:=TPasImplSimple(O.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse');
  // Exception handler 2
  AssertEquals('Except on handler',TPasImplExceptOn,TPasElement(E.Elements[1]).ClassType);
  O:=TPasImplExceptOn(E.Elements[1]);
  AssertEquals(1,O.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(O.Elements[0]).ClassType);
  AssertExpression('Exception Variable name',O.VarExpr,pekIdent,'Y');
  AssertExpression('Exception Type name',O.TypeExpr,pekIdent,'Exception2');
  S:=TPasImplSimple(O.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse2');
end;

procedure TTestStatementParser.TestTryExceptOnElse;
Var
  T : TPasImplTry;
  S : TPasImplSimple;
  E : TPasImplTryExcept;
  O : TPasImplExceptOn;
  EE : TPasImplTryExceptElse;
  I : TPasImplIfElse;

begin
  DeclareVar('Boolean','b');
  // Check that Else belongs to Except, not to IF

  TestStatement(['Try','  DoSomething;','except','On E : Exception do','if b then','DoSomethingElse;','else','DoSomethingMore;','end']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(1,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNotNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomething');
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Except statement',TPasImplTryExcept,T.FinallyExcept.ClassType);
  E:=TPasImplTryExcept(T.FinallyExcept);
  AssertEquals(1,E.Elements.Count);
  AssertEquals('Except on handler',TPasImplExceptOn,TPasElement(E.Elements[0]).ClassType);
  O:=TPasImplExceptOn(E.Elements[0]);
  AssertExpression('Exception Variable name',O.VarExpr,pekIdent,'E');
  AssertExpression('Exception Type name',O.TypeExpr,pekIdent,'Exception');
  AssertEquals(1,O.Elements.Count);
  AssertEquals('Simple statement',TPasImplIfElse,TPasElement(O.Elements[0]).ClassType);
  I:=TPasImplIfElse(O.Elements[0]);
  AssertEquals(1,I.Elements.Count);
  AssertNull('No else barcnh for if',I.ElseBranch);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(I.Elements[0]).ClassType);
  S:=TPasImplSimple(I.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse');
  AssertEquals('Except Else statement',TPasImplTryExceptElse,T.ElseBranch.ClassType);
  EE:=TPasImplTryExceptElse(T.ElseBranch);
  AssertEquals(1,EE.Elements.Count);
  AssertNotNull(EE.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(EE.Elements[0]).ClassType);
  S:=TPasImplSimple(EE.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomethingMore');
end;

procedure TTestStatementParser.TestTryExceptOnIfElse;
Var
  T : TPasImplTry;
  S : TPasImplSimple;
  E : TPasImplTryExcept;
  O : TPasImplExceptOn;
  EE : TPasImplTryExceptElse;

begin
  TestStatement(['Try','  DoSomething;','except','On E : Exception do','DoSomethingElse;','else','DoSomethingMore;','end']);
  T:=AssertStatement('Try statement',TPasImplTry) as TPasImplTry;
  AssertEquals(1,T.Elements.Count);
  AssertNotNull(T.FinallyExcept);
  AssertNotNull(T.ElseBranch);
  AssertNotNull(T.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  S:=TPasImplSimple(T.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomething');
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(T.Elements[0]).ClassType);
  AssertEquals('Except statement',TPasImplTryExcept,T.FinallyExcept.ClassType);
  E:=TPasImplTryExcept(T.FinallyExcept);
  AssertEquals(1,E.Elements.Count);
  AssertEquals('Except on handler',TPasImplExceptOn,TPasElement(E.Elements[0]).ClassType);
  O:=TPasImplExceptOn(E.Elements[0]);
  AssertExpression('Exception Variable name',O.VarExpr,pekIdent,'E');
  AssertExpression('Exception Type name',O.TypeExpr,pekIdent,'Exception');
  AssertEquals(1,O.Elements.Count);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(O.Elements[0]).ClassType);
  S:=TPasImplSimple(O.Elements[0]);
  AssertExpression('DoSomethingElse call',S.Expr,pekIdent,'DoSomethingElse');
  AssertEquals('Except Else statement',TPasImplTryExceptElse,T.ElseBranch.ClassType);
  EE:=TPasImplTryExceptElse(T.ElseBranch);
  AssertEquals(1,EE.Elements.Count);
  AssertNotNull(EE.Elements[0]);
  AssertEquals('Simple statement',TPasImplSimple,TPasElement(EE.Elements[0]).ClassType);
  S:=TPasImplSimple(EE.Elements[0]);
  AssertExpression('DoSomething call',S.Expr,pekIdent,'DoSomethingMore');
end;

procedure TTestStatementParser.TestAsm;

Var
  T : TPasImplAsmStatement;

begin
  TestStatement(['asm','  mov eax,1','end;']);
  T:=AssertStatement('Asm statement',TPasImplAsmStatement) as TPasImplAsmStatement;
  AssertEquals('Asm tokens',4,T.Tokens.Count);
  AssertEquals('token 1 ','mov',T.Tokens[0]);
  AssertEquals('token 2 ','eax',T.Tokens[1]);
  AssertEquals('token 3 ',',',T.Tokens[2]);
  AssertEquals('token 4 ','1',T.Tokens[3]);
end;

initialization
  RegisterTests([TTestStatementParser]);

end.

