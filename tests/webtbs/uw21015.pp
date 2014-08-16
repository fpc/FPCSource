unit uw21015;

{$mode delphi}

interface

type
  ITest<T> = interface
  end;

  TGenImpl<T> = class (TInterfacedObject,ITest<T>)
  end;

  IIntTest = Itest<Integer>;
implementation

end. 