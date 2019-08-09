unit tcresolvegenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, tcresolver, PasResolveEval, PParser;

type

  { TTestResolveGenerics }

  TTestResolveGenerics = Class(TCustomTestResolver)
  Published
    // generic types
    procedure TestGen_MissingTemplateFail;
    procedure TestGen_VarTypeWithoutSpecializeFail;
    procedure TestGen_GenTypeWithWrongParamCountFail;
    procedure TestGen_GenericNotFoundFail;
    procedure TestGen_SameNameSameParamCountFail;

    // constraints
    procedure TestGen_ConstraintStringFail;
    procedure TestGen_ConstraintMultiClassFail;
    procedure TestGen_ConstraintRecordExpectedFail;
    procedure TestGen_ConstraintClassRecordFail;
    procedure TestGen_ConstraintRecordClassFail;
    procedure TestGen_ConstraintArrayFail;
    // ToDo: constraint constructor
    // ToDo: constraint T:Unit2.TBird
    // ToDo: constraint T:Unit2.TGen<word>
    procedure TestGen_TemplNameEqTypeNameFail;

    // generic record
    procedure TestGen_RecordLocalNameDuplicateFail;
    procedure TestGen_Record;
    procedure TestGen_RecordDelphi;
    procedure TestGen_RecordNestedSpecialized;
    procedure TestGen_Record_SpecializeSelfInsideFail;
    // ToDo: enums within generic
    procedure TestGen_RecordAnoArray;
    // ToDo: procedure TestGen_SpecializeArg_ArrayOf;  type TBird = specialize<array of word>
    // ToDo: unitname.specialize TBird<word>.specialize

    // generic class
    procedure TestGen_Class;
    procedure TestGen_ClassDelphi;
    procedure TestGen_ClassForward;
    procedure TestGen_Class_Method;
    procedure TestGen_Class_SpecializeSelfInside;
    // ToDo: generic class forward (constraints must be repeated)
    // ToDo: generic class forward  constraints mismatch fail
    // ToDo: generic class overload <T> <S,T>
    procedure TestGen_Class_GenAncestor;
    procedure TestGen_Class_AncestorSelfFail;
    // ToDo: ancestor cycle: TBird<T> = class(TBird<word>) fail
    // ToDo: class-of
    // ToDo: UnitA.impl uses UnitB.intf uses UnitA.intf, UnitB has specialize of UnitA

    // ToDo: generic interface

    // ToDo: generic array

    // ToDo: generic procedure type

    // ToDo: pointer of generic

    // ToDo: helpers for generics

    // generic functions
    // ToDo: generic class method overload <T> <S,T>
    procedure TestGen_GenericFunction; // ToDo

    // generic statements
    procedure TestGen_LocalVar;
    procedure TestGen_Statements;
    procedure TestGen_ForLoop;
    // ToDo: for-in
    // ToDo: if
    // ToDo: case
    // ToDo: while, repeat
    // ToDo: try finally/except
    // ToDo: call
    // ToDo: dot
    // ToDo: is as
  end;

implementation

{ TTestResolveGenerics }

procedure TTestResolveGenerics.TestGen_MissingTemplateFail;
begin
  StartProgram(false);
  Add([
  'type generic g< > = array of word;',
  'begin',
  '']);
  CheckParserException('Expected "Identifier"',nParserExpectTokenError);
end;

procedure TTestResolveGenerics.TestGen_VarTypeWithoutSpecializeFail;
begin
  StartProgram(false);
  Add([
  'type generic TBird<T> = record end;',
  'var b: TBird;',
  'begin',
  '']);
  CheckResolverException('Generics without specialization cannot be used as a type for a variable',
    nGenericsWithoutSpecializationAsType);
end;

procedure TTestResolveGenerics.TestGen_GenTypeWithWrongParamCountFail;
begin
  StartProgram(false);
  Add([
  'type generic TBird<T> = record end;',
  'var b: TBird<word, byte>;',
  'begin',
  '']);
  CheckResolverException('identifier not found "TBird<,>"',
    nIdentifierNotFound);
end;

procedure TTestResolveGenerics.TestGen_GenericNotFoundFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TBird = specialize TAnimal<word>;',
  'begin',
  '']);
  CheckResolverException('identifier not found "TAnimal<>"',
    nIdentifierNotFound);
end;

procedure TTestResolveGenerics.TestGen_SameNameSameParamCountFail;
begin
  StartProgram(false);
  Add([
  '{$mode delphi}',
  'type',
  '  TBird<S,T> = record w: T; end;',
  '  TBird<X,Y> = record f: X; end;',
  'begin',
  '']);
  CheckResolverException('Duplicate identifier "TBird" at afile.pp(4,8)',
    nDuplicateIdentifier);
end;

procedure TTestResolveGenerics.TestGen_ConstraintStringFail;
begin
  StartProgram(false);
  Add([
  'generic function DoIt<T:string>(a: T): T;',
  'begin',
  '  Result:=a;',
  'end;',
  'begin',
  '']);
  CheckResolverException('"string" is not a valid constraint',
    nXIsNotAValidConstraint);
end;

procedure TTestResolveGenerics.TestGen_ConstraintMultiClassFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  TBird = class end;',
  '  TBear = class end;',
  'generic function DoIt<T: TBird, TBear>(a: T): T;',
  'begin',
  '  Result:=a;',
  'end;',
  'begin',
  '']);
  CheckResolverException('"TBird" constraint and "TBear" constraint cannot be specified together',
    nConstraintXAndConstraintYCannotBeTogether);
end;

procedure TTestResolveGenerics.TestGen_ConstraintRecordExpectedFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  generic TBird<T:record> = record v: T; end;',
  'var r: specialize TBird<word>;',
  'begin',
  '']);
  CheckResolverException('record type expected, but Word found',
    nXExpectedButYFound);
end;

procedure TTestResolveGenerics.TestGen_ConstraintClassRecordFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TRec = record end;',
  '  generic TBird<T:class> = record v: T; end;',
  'var r: specialize TBird<TRec>;',
  'begin',
  '']);
  CheckResolverException('class type expected, but TRec found',
    nXExpectedButYFound);
end;

procedure TTestResolveGenerics.TestGen_ConstraintRecordClassFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T:record> = record v: T; end;',
  'var r: specialize TBird<TObject>;',
  'begin',
  '']);
  CheckResolverException('record type expected, but TObject found',
    nXExpectedButYFound);
end;

procedure TTestResolveGenerics.TestGen_ConstraintArrayFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TArr = array of word;',
  '  generic TBird<T:TArr> = record v: T; end;',
  'begin',
  '']);
  CheckResolverException('"TArr" is not a valid constraint',
    nXIsNotAValidConstraint);
end;

procedure TTestResolveGenerics.TestGen_TemplNameEqTypeNameFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  generic TBird<TBird> = record v: T; end;',
  'var r: specialize TBird<word>;',
  'begin',
  '']);
  CheckResolverException('Duplicate identifier "TBird" at afile.pp(4,16)',
    nDuplicateIdentifier);
end;

procedure TTestResolveGenerics.TestGen_RecordLocalNameDuplicateFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  generic TBird<T> = record T: word; end;',
  'begin',
  '']);
  CheckResolverException('Duplicate identifier "T" at afile.pp(4,18)',
    nDuplicateIdentifier);
end;

procedure TTestResolveGenerics.TestGen_Record;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  {#Typ}T = word;',
  '  generic TRec<{#Templ}T> = record',
  '    {=Templ}v: T;',
  '  end;',
  'var',
  '  r: specialize TRec<word>;',
  '  {=Typ}w: T;',
  'begin',
  '  r.v:=w;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_RecordDelphi;
begin
  StartProgram(false);
  Add([
  '{$mode delphi}',
  'type',
  '  {#Typ}T = word;',
  '  TRec<{#Templ}T> = record',
  '    {=Templ}v: T;',
  '  end;',
  'var',
  '  r: TRec<word>;',
  '  {=Typ}w: T;',
  'begin',
  '  r.v:=w;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_RecordNestedSpecialized;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class v: T; end;',
  '  generic TFish<T:class> = record v: T; end;',
  'var f: specialize TFish<specialize TBird<word>>;',
  'begin',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Record_SpecializeSelfInsideFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  generic TBird<T> = record',
  '    v: specialize TBird<word>;',
  '  end;',
  'begin',
  '']);
  CheckResolverException('type "TBird" is not yet completely defined',
    nTypeXIsNotYetCompletelyDefined);
end;

procedure TTestResolveGenerics.TestGen_RecordAnoArray;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  generic TBird<T> = record v: T; end;',
  'var b: specialize TBird<array of word>;',
  'begin',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Class;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  {#Typ}T = word;',
  '  generic TBird<{#Templ}T> = class',
  '    {=Templ}v: T;',
  '  end;',
  'var',
  '  b: specialize TBird<word>;',
  '  {=Typ}w: T;',
  'begin',
  '  b.v:=w;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_ClassDelphi;
begin
  StartProgram(false);
  Add([
  '{$mode delphi}',
  'type',
  '  TObject = class end;',
  '  {#Typ}T = word;',
  '  TBird<{#Templ}T> = class',
  '    {=Templ}v: T;',
  '  end;',
  'var',
  '  b: TBird<word>;',
  '  {=Typ}w: T;',
  'begin',
  '  b.v:=w;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_ClassForward;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  {#Typ}T = word;',
  '  generic TBird<{#Templ_Forward}T> = class;',
  '  TRec = record',
  '    b: specialize TBird<T>;',
  '  end;',
  '  generic TBird<{#Templ}T> = class',
  '    {=Templ}v: T;',
  '    r: TRec;',
  '  end;',
  'var',
  '  s: specialize TRec;',
  '  {=Typ}w: T;',
  'begin',
  '  s.b.v:=w;',
  '  s.b.r:=s;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Class_Method;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  {#Typ}T = word;',
  '  generic TBird<{#Templ}T> = class',
  '    function Fly(p:T): T; virtual; abstract;',
  '    function Run(p:T): T;',
  '  end;',
  'function TBird.Run(p:T): T;',
  'begin',
  'end;',
  'var',
  '  b: specialize TBird<word>;',
  '  {=Typ}w: T;',
  'begin',
  '  w:=b.Fly(w);',
  '  w:=b.Run(w);',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Class_SpecializeSelfInside;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class',
  '    e: T;',
  '    v: TBird<boolean>;',
  '  end;',
  'var',
  '  b: specialize TBird<word>;',
  '  w: word;',
  'begin',
  '  b.e:=w;',
  '  if b.v.e then ;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Class_GenAncestor;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class',
  '    i: T;',
  '  end;',
  '  generic TEagle<T> = class(TBird<T>)',
  '    j: T;',
  '  end;',
  'var',
  '  e: specialize TEagle<word>;',
  'begin',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Class_AncestorSelfFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class(TBird<word>)',
  '    e: T;',
  '  end;',
  'var',
  '  b: specialize TBird<word>;',
  'begin',
  '']);
  CheckResolverException('type "TBird" is not yet completely defined',nTypeXIsNotYetCompletelyDefined);
end;

procedure TTestResolveGenerics.TestGen_GenericFunction;
begin
  StartProgram(false);
  Add([
  'generic function DoIt<T>(a: T): T;',
  'var i: T;',
  'begin',
  '  a:=i;',
  '  Result:=a;',
  'end;',
  'var w: word;',
  'begin',
  //'  w:=DoIt<word>(3);',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_LocalVar;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<{#Templ}T> = class',
  '    function Fly(p:T): T;',
  '  end;',
  'function TBird.Fly(p:T): T;',
  'var l: T;',
  'begin',
  '  l:=p;',
  '  p:=l;',
  '  Result:=p;',
  '  Result:=l;',
  '  l:=Result;',
  'end;',
  'var',
  '  b: specialize TBird<word>;',
  '  w: word;',
  'begin',
  '  w:=b.Fly(w);',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Statements;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<{#Templ}T> = class',
  '    function Fly(p:T): T;',
  '  end;',
  'function TBird.Fly(p:T): T;',
  'var',
  '  v1,v2,v3:T;',
  'begin',
  '  v1:=1;',
  '  v2:=v1+v1*v1+v1 div p;',
  '  v3:=-v1;',
  '  repeat',
  '    v1:=v1+1;',
  '  until v1>=5;',
  '  while v1>=0 do',
  '    v1:=v1-v2;',
  '  for v1:=v2 to v3 do v2:=v1;',
  '  if v1<v2 then v3:=v1 else v3:=v2;',
  '  if v1<v2 then else ;',
  '  case v1 of',
  '  1: v3:=3;',
  '  end;',
  'end;',
  'var',
  '  b: specialize TBird<word>;',
  'begin',
  '  b.Fly(2);',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_ForLoop;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<{#Templ}T> = class',
  '    function Fly(p:T): T;',
  '  end;',
  'function TBird.Fly(p:T): T;',
  'var i: T;',
  'begin',
  '  for i:=0 to 3 do Result:=i+p;',
  'end;',
  'var',
  '  b: specialize TBird<word>;',
  'begin',
  '  b.Fly(2);',
  '']);
  ParseProgram;
end;

initialization
  RegisterTests([TTestResolveGenerics]);

end.

