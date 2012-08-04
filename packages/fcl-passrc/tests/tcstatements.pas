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
  DeclareVar('boolean','b');
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

initialization
  RegisterTests([TTestStatementParser]);

end.

