{ %CPU=wasm32 }
// Test that methods of parent interfaces (without RTTI and hence no thunk class) are also added to thunk class.
// tthunkpc2 contains interface without rtti
unit tthunkpc1;

{$mode objfpc}

interface

uses tthunkpc2;

Type
  {$M+}
  IB = Interface(IAS)
    Procedure methodB;
  end;

implementation

end.     