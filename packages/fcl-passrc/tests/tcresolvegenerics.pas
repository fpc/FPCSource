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
    procedure TestGen_RecordAnoArray;
    // ToDo: unitname.specialize TBird<word>.specialize
    procedure TestGen_RecordNestedSpecialize;

    // generic class
    procedure TestGen_Class;
    procedure TestGen_ClassDelphi;
    procedure TestGen_ClassForward;
    procedure TestGen_ClassForwardConstraints;
    procedure TestGen_ClassForwardConstraintNameMismatchFail;
    procedure TestGen_ClassForwardConstraintKeywordMismatchFail;
    procedure TestGen_ClassForwardConstraintTypeMismatchFail;
    procedure TestGen_Class_Method;
    procedure TestGen_Class_SpecializeSelfInside;
    // ToDo: generic class overload <T> <S,T>
    procedure TestGen_Class_GenAncestor;
    procedure TestGen_Class_AncestorSelfFail;
    // ToDo: class-of
    // ToDo: UnitA.impl uses UnitB.intf uses UnitA.intf, UnitB has specialize of UnitA
    procedure TestGen_Class_NestedType;
    procedure TestGen_Class_NestedRecord;
    procedure TestGen_Class_NestedClass; // ToDo
    procedure TestGen_Class_Enums_NotPropagating;
    procedure TestGen_Class_List;

    // generic external class
    procedure TestGen_ExtClass_Array;

    // ToDo: generic interface

    // generic array
    procedure TestGen_Array;

    // generic procedure type
    procedure TestGen_ProcType;

    // ToDo: pointer of generic

    // ToDo: helpers for generics

    // generic functions
    // ToDo: generic class method overload <T> <S,T>
    procedure TestGen_GenericFunction; // ToDo

    // generic statements
    procedure TestGen_LocalVar;
    procedure TestGen_Statements;
    procedure TestGen_InlineSpecializeExpr;
    // ToDo: for-in
    procedure TestGen_TryExcept;
    // ToDo: call
    // ToDo: dot
    // ToDo: is as
    // ToDo: typecast
    // ToTo: nested proc
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
  CheckResolverException('"String" is not a valid constraint',
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
  CheckResolverException('"array of Word" is not a valid constraint',
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
  'var',
  '  a: specialize TBird<array of word>;',
  '  b: specialize TBird<array of word>;',
  'begin',
  '  a:=b;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_RecordNestedSpecialize;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  generic TBird<T> = record v: T; end;',
  'var',
  '  a: specialize TBird<specialize TBird<word>>;',
  'begin',
  '  a.v.v:=3;',
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
  '  s: TRec;',
  '  {=Typ}w: T;',
  'begin',
  '  s.b.v:=w;',
  '  s.b.r:=s;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_ClassForwardConstraints;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  TAnt = class end;',
  '  generic TBird<T: class; U; V: TAnt> = class;',
  '  TRec = record',
  '    b: specialize TBird<TAnt,word,TAnt>;',
  '  end;',
  '  generic TBird<T: class; U; V: TAnt> = class',
  '    i: U;',
  '    r: TRec;',
  '  end;',
  'var',
  '  s: TRec;',
  '  w: word;',
  'begin',
  '  s.b.i:=w;',
  '  s.b.r:=s;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_ClassForwardConstraintNameMismatchFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class;',
  '  generic TBird<U> = class',
  '    i: U;',
  '  end;',
  'begin',
  '']);
  CheckResolverException('Declaration of "U" differs from previous declaration at afile.pp(5,18)',
    nDeclOfXDiffersFromPrevAtY);
end;

procedure TTestResolveGenerics.
  TestGen_ClassForwardConstraintKeywordMismatchFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T: class, constructor> = class;',
  '  generic TBird<U: class> = class',
  '    i: U;',
  '  end;',
  'begin',
  '']);
  CheckResolverException('Declaration of "U" differs from previous declaration at afile.pp(5,18)',
    nDeclOfXDiffersFromPrevAtY);
end;

procedure TTestResolveGenerics.TestGen_ClassForwardConstraintTypeMismatchFail;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  TAnt = class end;',
  '  TFish = class end;',
  '  generic TBird<T: TAnt> = class;',
  '  generic TBird<T: TFish> = class',
  '    i: U;',
  '  end;',
  'begin',
  '']);
  CheckResolverException('Declaration of "T" differs from previous declaration at afile.pp(7,20)',
    nDeclOfXDiffersFromPrevAtY);
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
  '  e.i:=e.j;',
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

procedure TTestResolveGenerics.TestGen_Class_NestedType;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class',
  '  public type',
  '    TArrayEvent = reference to procedure(El: T);',
  '  public',
  '    p: TArrayEvent;',
  '  end;',
  '  TBirdWord = specialize TBird<word>;',
  'var',
  '  b: TBirdWord;',
  'begin',
  '  b.p:=procedure(El: word) begin end;']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Class_NestedRecord;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  '{$modeswitch advancedrecords}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class',
  '  public type TWing = record',
  '      s: T;',
  '      function GetIt: T;',
  '    end;',
  '  public',
  '    w: TWing;',
  '  end;',
  '  TBirdWord = specialize TBird<word>;',
  'function TBird.TWing.GetIt: T;',
  'begin',
  'end;',
  'var',
  '  b: TBirdWord;',
  '  i: word;',
  'begin',
  '  b.w.s:=i;',
  '  i:=b.w.GetIt;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Class_NestedClass;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class',
  '  public type TWing = class',
  '      s: T;',
  '      function GetIt: T;',
  '    end;',
  '  public',
  '    w: TWing;',
  '  end;',
  '  TBirdWord = specialize TBird<word>;',
  'function TBird.TWing.GetIt: T;',
  'begin',
  'end;',
  'var',
  '  b: TBirdWord;',
  '  i: word;',
  'begin',
  '  b.w.s:=3;',
  '  i:=b.w.GetIt;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Class_Enums_NotPropagating;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class',
  '  public type',
  '    TEnum = (red, blue);',
  '  const',
  '    e = blue;',
  '  end;',
  'const',
  '  r = red;',
  'begin']);
  CheckResolverException('identifier not found "red"',nIdentifierNotFound);
end;

procedure TTestResolveGenerics.TestGen_Class_List;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TList<T> = class',
  '  strict private',
  '    FItems: array of T;',
  '    function GetItems(Index: longint): T;',
  '    procedure SetItems(Index: longint; Value: T);',
  '  public',
  '    procedure Alter(w: T);',
  '    property Items[Index: longint]: T read GetItems write SetItems; default;',
  '  end;',
  '  TWordList = specialize TList<word>;',
  'function TList.GetItems(Index: longint): T;',
  'begin',
  '  Result:=FItems[Index];',
  'end;',
  'procedure TList.SetItems(Index: longint; Value: T);',
  'begin',
  '  FItems[Index]:=Value;',
  'end;',
  'procedure TList.Alter(w: T);',
  'begin',
  '  SetLength(FItems,length(FItems)+1);',
  '  Insert(w,FItems,2);',
  '  Delete(FItems,2,3);',
  'end;',
  'var l: TWordList;',
  '  w: word;',
  'begin',
  '  l[1]:=w;',
  '  w:=l[2];']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_ExtClass_Array;
begin
  StartProgram(false);
  Add([
  '{$mode delphi}',
  '{$ModeSwitch externalclass}',
  'type',
  '  NativeInt = longint;',
  '  TJSGenArray<T> = Class external name ''Array''',
  '  private',
  '    function GetElements(Index: NativeInt): T; external name ''[]'';',
  '    procedure SetElements(Index: NativeInt; const AValue: T); external name ''[]'';',
  '  public',
  '    type TSelfType = TJSGenArray<T>;',
  '    TArrayEvent = reference to function(El: T; Arr: TSelfType): Boolean;',
  '    TArrayCallback = TArrayEvent;',
  '  public',
  '    FLength : NativeInt; external name ''length'';',
  '    constructor new; overload;',
  '    constructor new(aLength : NativeInt); overload;',
  '    class function _of() : TSelfType; varargs; external name ''of'';',
  '    function every(const aCallback: TArrayCallBack): boolean; overload;',
  '    function fill(aValue : T) : TSelfType; overload;',
  '    function fill(aValue : T; aStartIndex : NativeInt) : TSelfType; overload;',
  '    function fill(aValue : T; aStartIndex,aEndIndex : NativeInt) : TSelfType; overload;',
  '    property Length : NativeInt Read FLength Write FLength;',
  '    property Elements[Index: NativeInt]: T read GetElements write SetElements; default;',
  '  end;',
  '  TJSWordArray = TJSGenArray<word>;',
  'var',
  '  wa: TJSWordArray;',
  '  w: word;',
  'begin',
  '  wa:=TJSWordArray.new;',
  '  wa:=TJSWordArray.new(3);',
  '  wa:=TJSWordArray._of(4,5);',
  '  wa:=wa.fill(7);',
  '  wa:=wa.fill(7,8,9);',
  '  w:=wa.length;',
  '  wa.length:=10;',
  '  wa[11]:=w;',
  '  w:=wa[12];',
  '  wa.every(function(El: word; Arr: TJSWordArray): Boolean',
  '           begin',
  '           end',
  '      );',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_Array;
begin
  StartProgram(false);
  Add([
  'type',
  '  generic TArray<T> = array of T;',
  '  TWordArray = specialize TArray<word>;',
  'var',
  '  a: specialize TArray<word>;',
  '  b: TWordArray;',
  '  w: word;',
  'begin',
  '  a[1]:=2;',
  '  b[2]:=a[3]+b[4];',
  '  a:=b;',
  '  b:=a;',
  '  SetLength(a,5);',
  '  SetLength(b,6);',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_ProcType;
begin
  StartProgram(false);
  Add([
  'type',
  '  generic TFunc<T> = function(v: T): T;',
  '  TWordFunc = specialize TFunc<word>;',
  'function GetIt(w: word): word;',
  'begin',
  'end;',
  'var',
  '  a: specialize TFunc<word>;',
  '  b: TWordFunc;',
  '  w: word;',
  'begin',
  '  a:=nil;',
  '  b:=nil;',
  '  a:=b;',
  '  b:=a;',
  '  w:=a(w);',
  '  w:=b(w);',
  '  a:=@GetIt;',
  '  b:=@GetIt;',
  '']);
  ParseProgram;
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

procedure TTestResolveGenerics.TestGen_InlineSpecializeExpr;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<T> = class',
  '    constructor Create;',
  '  end;',
  '  generic TAnt<U> = class',
  '    constructor Create;',
  '  end;',
  'constructor TBird.Create;',
  'var',
  '  a: TAnt<T>;',
  '  b: TAnt<word>;',
  'begin',
  '  a:=TAnt<T>.create;',
  '  b:=TAnt<word>.create;',
  'end;',
  'constructor TAnt.Create;',
  'var',
  '  i: TBird<U>;',
  '  j: TBird<word>;',
  '  k: TAnt<U>;',
  'begin',
  '  i:=TBird<U>.create;',
  '  j:=TBird<word>.create;',
  '  k:=TAnt<U>.create;',
  'end;',
  'var a: TAnt<word>;',
  'begin',
  '  a:=TAnt<word>.create;',
  '']);
  ParseProgram;
end;

procedure TTestResolveGenerics.TestGen_TryExcept;
begin
  StartProgram(false);
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class end;',
  '  generic TBird<{#Templ}T> = class',
  '    function Fly(p:T): T;',
  '  end;',
  '  Exception = class',
  '  end;',
  '  generic EMsg<T> = class',
  '    Msg: T;',
  '  end;',
  'function TBird.Fly(p:T): T;',
  'var',
  '  v1,v2,v3:T;',
  'begin',
  '  try',
  '  finally',
  '  end;',
  '  try',
  '    v1:=v2;',
  '  finally',
  '    v2:=v1;',
  '  end;',
  '  try',
  '  except',
  '    on Exception do ;',
  '    on E: Exception do ;',
  '    on E: EMsg<boolean> do E.Msg:=true;',
  '    on E: EMsg<T> do E.Msg:=1;',
  '  end;',
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

