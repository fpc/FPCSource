unit tcresolvegenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, tcresolver, PasResolveEval;

type

  { TTestResolveGenerics }

  TTestResolveGenerics = Class(TCustomTestResolver)
  Published
    procedure TestGen_GenericFunction; // ToDo
    procedure TestGen_ConstraintStringFail;
    procedure TestGen_ConstraintMultiClassFail;
    // ToDo: constraint keyword record
    // ToDo: constraint keyword class, constructor, class+constructor
    // ToDo: constraint Unit2.TBird
    // ToDo: constraint Unit2.TGen<word>
    // ToDo: generic array
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

initialization
  RegisterTests([TTestResolveGenerics]);

end.

