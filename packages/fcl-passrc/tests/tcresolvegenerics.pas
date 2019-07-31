unit tcresolvegenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, tcresolver, PasResolveEval, PParser;

type

  { TTestResolveGenerics }

  TTestResolveGenerics = Class(TCustomTestResolver)
  Published
    // generic functions
    procedure TestGen_GenericFunction; // ToDo

    // generic types
    procedure TestGen_MissingTemplateFail;
    procedure TestGen_VarTypeWithoutSpecializeFail;
    procedure TestGen_ConstraintStringFail;
    procedure TestGen_ConstraintMultiClassFail;
    procedure TestGen_ConstraintRecordExpectedFail;
    // ToDo: constraints mismatch: TAnt<T:record>; TBird<T:Class> = record v: TAnt<T> end   Fail
    // ToDo: constraint keyword record
    // ToDo: constraint keyword class, constructor, class+constructor
    // ToDo: constraint T:Unit2.TBird
    // ToDo: constraint T:Unit2.TGen<word>
    procedure TestGen_GenericNotFoundFail;
    procedure TestGen_RecordLocalNameDuplicateFail;
    procedure TestGen_Record;
    procedure TestGen_RecordDelphi;
    // ToDo: enums within generic
    // ToDo: procedure TestGen_SpecializeArg_ArrayOf;  type TBird = specialize<array of word>
    // ToDo: unitname.specialize TBird<word>.specialize
    procedure TestGen_Class;
    //procedure TestGen_ClassDelphi;
    // ToDo: generic class
    // ToDo: generic class forward (constraints must be repeated)
    // ToDo: generic class forward  constraints mismatch fail
    // ToDo: generic class overload
    // ToDo: ancestor cycle: TBird<T> = class(TBird<word>) fail
    // ToDo: class-of
    // ToDo: UnitA.impl uses UnitB.intf uses UnitA.intf, UnitB has specialize of UnitA
    // ToDo: generic interface
    // ToDo: generic array
    // ToDo: generic procedure type
    // ToDo: pointer of generic
    // ToDo: generic helpers
  end;

implementation

{ TTestResolveGenerics }

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
  CheckResolverException('''string'' is not a valid constraint',
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
  CheckResolverException('''TBird'' constraint and ''TBear'' constraint cannot be specified together',
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

procedure TTestResolveGenerics.TestGen_GenericNotFoundFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TBird = specialize TAnimal<word>;',
  'begin',
  '']);
  CheckResolverException('identifier not found "TAnimal"',
    nIdentifierNotFound);
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

initialization
  RegisterTests([TTestResolveGenerics]);

end.

