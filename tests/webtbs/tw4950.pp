// This code is provided bt Leandro Conde [leandor@gmail.com]
// to demonstrate a problem with function redirection in interfaces
// in the FPC compiler version 2.0.2
//
// In the real implementation is used to provide a set of data type
// conversion functions: From<Type> and To<Type>.
// In that scenario, the IBase contains functions specific por <Type>,
// and the Derived are the <From> and <To> parts.

{$mode delphi}

program FunctionRedirectionProblem;

var
  cnt : longint;

type
  // It really doesn't matters what this interface contains,
  // is just represet some functionality set that is provided
  // with two complementary sets of implementations.
  IBase = interface
    ['{6CEA50E0-1844-405F-9B6F-2E9D50BE6EFC}']
    function Base1(const Arg1: string; const Arg2: Double): Boolean;
    function Base2(const Arg1: TDateTime; const Arg2: Single): Boolean;
    function Base3(const Arg1: WideString; const Arg2: Integer): Boolean;
  end;

  // Derived 1 to provide an IBase with a specific funtionality when used from IDerived1
  IDerived1 = interface (IBase)
    ['{3DC49E1B-9A52-499E-8BCD-E1BA160329C9}']
    function Derived1Specific(Arg1: Integer): Integer;
  end;

  // Derived 2 to provide an IBase with a complementary funtionality when used from IDerived2
  IDerived2 = interface (IBase)
    ['{E44DF3BC-75C8-4284-928B-DE3162347C21}']
    function Derived2Specific(Arg1: Integer): Integer;
  end;

  // Just for runtime testing purposes
  ITestCase = interface
    ['{C608134F-2F9A-44A4-8932-F169EF40E177}']
    function Derived1: IDerived1;
    function Derived2: IDerived2;
  end;

type
  // Implementing all the above stuff
  TTestCase = class(TInterfacedObject, IDerived1, IDerived2, ITestCase )
  protected
    // methods from IBase in IDerived1 redirected to one function set:
    function IDerived1.Base1 = Derived1_Base1;
    function IDerived1.Base2 = Derived1_Base2;
    function IDerived1.Base3 = Derived1_Base3;
    // this is the function set for IBase in IDerived1
    function Derived1_Base1(const Arg1: string; const Arg2: Double): Boolean;
    function Derived1_Base2(const Arg1: TDateTime; const Arg2: Single): Boolean;
    function Derived1_Base3(const Arg1: WideString; const Arg2: Integer): Boolean;
    // specifics from IDerived1
    function Derived1Specific(Arg1: Integer): Integer;
  protected
    // methods from IBase in IDerived2 redirected to the other function set:
    function IDerived2.Base1 = Derived2_Base1;
    function IDerived2.Base2 = Derived2_Base2;
    function IDerived2.Base3 = Derived2_Base3;
    // this is the function set for IBase in IDerived2
    function Derived2_Base1(const Arg1: string; const Arg2: Double): Boolean;
    function Derived2_Base2(const Arg1: TDateTime; const Arg2: Single): Boolean;
    function Derived2_Base3(const Arg1: WideString; const Arg2: Integer): Boolean;
    // specifics from IDerived2
    function Derived2Specific(Arg1: Integer): Integer;
  protected // this is not essential, just for testing in runtime ITestCase
    function Derived1: IDerived1;
    function Derived2: IDerived2;
  end;

{ TTestCase }

function TTestCase.Derived1_Base1(const Arg1: string; const Arg2: Double): Boolean;
begin
  Result := True;
  writeln('1:',arg1,arg2);
  inc(cnt);
end;

function TTestCase.Derived1_Base2(const Arg1: TDateTime; const Arg2: Single): Boolean;
begin
  Result := True;
end;

function TTestCase.Derived1_Base3(const Arg1: WideString; const Arg2: Integer): Boolean;
begin
  Result := True;
end;

function TTestCase.Derived1Specific(Arg1: Integer): Integer;
begin
  Result := Arg1;
end;

function TTestCase.Derived2_Base1(const Arg1: string; const Arg2: Double): Boolean;
begin
  Result := True;
  writeln('2:',arg1,arg2);
  inc(cnt);
end;

function TTestCase.Derived2_Base2(const Arg1: TDateTime; const Arg2: Single): Boolean;
begin
  Result := True;
end;

function TTestCase.Derived2_Base3(const Arg1: WideString; const Arg2: Integer): Boolean;
begin
  Result := True;
end;

function TTestCase.Derived2Specific(Arg1: Integer): Integer;
begin
  Result := Arg1;
end;

function TTestCase.Derived1: IDerived1;
begin
  Result := Self;
end;

function TTestCase.Derived2: IDerived2;
begin
  Result := Self;
end;

var
  TestCase: ITestCase;
begin
  TestCase := TTestCase.Create;

  TestCase.Derived1.Base1('called from derived1', 3.14159);
  TestCase.Derived2.Base1('called from derived2', 2.7178);
  if cnt<>2 then
    halt(1);
end.
