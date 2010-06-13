{%OPT=-gh}
program tdel2;
{$ifdef fpc}{$mode objfpc}{$h+}{$endif}
{ A test for correct refcounting when using different methods of casting
  object to delegated COM interface. The requirement is no memleaks.
 }

uses
  //heaptrc,
  SysUtils;

const
  STestInterface = '{3FB19775-F5FA-464C-B10C-D8137D742088}';

type
  ITest = interface[STestInterface]
    function GetRefCount: Integer;
  end;

  TImpl = class(TInterfacedObject,ITest)
    function GetRefCount: Integer;
  end;

  TTest = class(TInterfacedObject)
  public
    constructor Create; virtual; abstract;
  end;

  TTestClass = class of TTest;

  TC1 = class(TTest,ITest)
  private
    FImpl: ITest;
  public
    constructor Create; override;
    property impl: ITest read FImpl implements ITest;
  end;

  TC2 = class(TTest,ITest)
  private
    FImpl: ITest;
    function GetImpl: ITest;
  public
    constructor Create; override;
    property impl: ITest read GetImpl implements ITest;
  end;

  TC3 = class(TTest,ITest)
  private
    FImpl: TImpl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property impl: TImpl read FImpl implements ITest;
  end;

function TImpl.GetRefCount: Integer;
begin
  Result := refcount;
end;

constructor TC1.Create;
begin
  FImpl := TImpl.Create;
end;

constructor TC2.Create;
begin
  FImpl := TImpl.Create;
end;

function TC2.GetImpl: ITest;
begin
  result:=FImpl;
end;

constructor TC3.Create;
begin
  FImpl := TImpl.Create;
  FImpl._AddRef;
end;

destructor TC3.Destroy;
begin
  FImpl._Release;
  inherited Destroy;
end;


type
  TTestCase = record
    c: TTestClass;
    by: String;
  end;


var
  tests: array[0..2] of TTestCase = (
    (c:TC1; by:'intf field'),
    (c:TC2; by:'intf function'),
    (c:TC3; by:'class field')
  );

  failed: Boolean = false;

procedure fail(const by: String);
begin
  writeln('  When delegating by ', by, ', failed');
  failed := true;
end;

procedure succ(const by: String);
begin
  writeln('  When delegating by ', by);
end;

procedure succ(const by: String; R: Integer);
begin
  writeln('  When delegating by ', by, ', refcount=', R);
end;

procedure succ(const by: String; const S: String);
begin
  writeln('  When delegating by ', by, ', Classname=', S);
end;

var
  T: Integer;
  C: TInterfacedObject;
  I: ITest;
  P: Pointer;
  O: TImpl;
begin

(*******************************************************************************
 * GetInterface function
 *******************************************************************************)

  writeln('Testing GetInteface()...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    if C.GetInterface(ITest, I) then
      succ(tests[T].by, I.GetRefCount)
    else
      fail(tests[T].by);
    I := nil;
    C.Free;
  end;


(*******************************************************************************
 * GetInterfaceByStr function
 *******************************************************************************)

  writeln('Testing GetInterfaceByStr()...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    if C.GetInterfaceByStr(STestInterface, I) then
      succ(tests[T].by, I.GetRefCount)
    else
      fail(tests[T].by);
    I := nil;
    C.Free;
  end;


(*******************************************************************************
 * GetInterfaceWeak function
 *******************************************************************************)

  writeln('Testing GetInterfaceWeak()...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    P := nil;
    if C.GetInterfaceWeak(ITest, P) then
      succ(tests[T].by, ITest(P).GetRefCount)
    else
      fail(tests[T].by);
    P := nil;
    C.Free;
  end;


(*******************************************************************************
 * Supports function
 *******************************************************************************)

  writeln('Testing ''supports'' function...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    if Supports(C, ITest, I) then
      succ(tests[T].by, I.GetRefCount)
    else
      fail(tests[T].by);
    I := nil;
    C.Free;
  end;


(*******************************************************************************
 * IS operator
 *******************************************************************************)

  writeln('Testing ''object is interface'' operator...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    if C is ITest then
      succ(tests[T].by)
    else
      fail(tests[T].by);
    C.Free;
  end;

  writeln('Testing ''interface is interface'' operator...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    P := nil;
    if C.GetInterfaceWeak(IUnknown, P) then
    begin
      if IUnknown(P) is ITest then
        succ(tests[T].by)
      else
        fail(tests[T].by);
    end else
      fail(tests[T].by);
    P := nil;
    C.Free;
  end;

  writeln('Testing ''interface is object'' operator...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    I := C as ITest;
    if I<>nil then
    begin
      if I is TImpl then
        succ(tests[T].by)
      else
        fail(tests[T].by);
    end else
      fail(tests[T].by);
    I := nil;
    C.Free;
  end;


(*******************************************************************************
 * AS operator
 *******************************************************************************)

  writeln('Testing ''object as interface'' operator...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    I := C as ITest;
    if I<>nil then
      succ(tests[T].by, I.GetRefCount)
    else
      fail(tests[T].by);
    I := nil;
    C.Free;
  end;

  writeln('Testing ''interface as interface'' operator...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    P := nil;
    if C.GetInterfaceWeak(IUnknown, P) then
    begin
      I := IUnknown(P) as ITest;
      if I<>nil then
        succ(tests[T].by, I.GetRefCount)
      else
        fail(tests[T].by);
      I := nil;
    end else
      fail(tests[T].by);
    P := nil;
    C.Free;
  end;

  writeln('Testing ''interface as object'' operator...');
  for T := 0 to High(tests) do
  begin
    C := tests[T].c.Create;
    I := C as ITest;
    if I<>nil then
    begin
      O := I as TImpl;
      if O<>nil then
        succ(tests[T].by, O.Classname)
      else
        fail(tests[T].by);
    end else
      fail(tests[T].by);
    I := nil;
    C.Free;
  end;



  if failed then
    Halt(1);
end.
