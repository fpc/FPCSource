unit tcstatements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, pastree, pscanner, pparser,
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
    Procedure TestAssignment;
    Procedure TestCall;
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
    Procedure TestForExpr;
    Procedure TestForBlock;
    procedure TestDowntoBlock;
    Procedure TestForNested;
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

function TTestStatementParser.TestStatement(ASource: array of string): TPasImplElement;

Var
  i : Integer;

begin
  FStatement:=Nil;
  AddStatements(ASource);
  ParseModule;
  AssertEquals('Have program',TPasProgram,Module.ClassType);
  AssertNotNull('Have program section',PasProgram.ProgramSection);
  AssertNotNull('Have program section',PasProgram.InitializationSection);
  if (PasProgram.InitializationSection.Elements.Count>0) then
    if TObject(PasProgram.InitializationSection.Elements[0]) is TPasImplBlock then
      FStatement:=TPasImplBlock(PasProgram.InitializationSection.Elements[0]);
end;

procedure TTestStatementParser.ExpectParserError(Const Msg : string);
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

procedure TTestStatementParser.TestAssignment;

Var
  A : TPasImplAssign;

begin
  DeclareVar('integer');
  TestStatement(['a:=1;']);
  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Assignment statement',TPasImplAssign,Statement.ClassType);
  A:=Statement as TPasImplAssign;
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

Var
  I : TPasImplIfElse;

begin
  DeclareVar('boolean');
  ExpectParserError('No semicolon before else',['if a then','  begin','  end;','else','  begin','  end']);
end;

procedure TTestStatementParser.TestNestedIf;
Var
  I,I2 : TPasImplIfElse;
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
  I,I2 : TPasImplIfElse;

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
  AssertEquals('Up loop',False,F.Down);
  AssertExpression('Start value',F.StartExpr,pekNumber,'1');
  AssertExpression('End value',F.EndExpr,pekNumber,'10');
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

initialization
  RegisterTests([TTestStatementParser]);

end.

