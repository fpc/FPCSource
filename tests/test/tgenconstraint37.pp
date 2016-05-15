{ %NORUN }

program tgenconstraint37;

{$mode objfpc}

type
  generic TGenericTObject<T: TObject> = class
  end;

  generic TGenericClass<T: class> = class
  end;

  generic TGenericIInterface<T: IInterface> = class
  end;

  TTestObject = class;
  ITestInterface = interface;


  TGenericTObjectTTestObject = specialize TGenericTObject<TTestObject>;

  TGenericClassTTestObject = specialize TGenericClass<TTestObject>;

  TGenericIInterfaceITestInterface = specialize TGenericIInterface<ITestInterface>;


  TTestObject = class
  end;

  ITestInterface = interface
  end;

begin
end.
