{ %CPU=wasm32 }
// Test that methods of parent interfaces (without RTTI and hence no thunk class) are also added to thunk class.
// This unit contains a parent interface without RTTI (see tthunkpc1)
unit tthunkpc2;

{$mode objfpc}

interface

Type
  generic IA<T> = Interface  ['{9457c0d1-4ae6-40e3-94c0-486439b30e4c}']
    Procedure methodA(a : T);
  end;

  IAS = Interface(specialize IA<String>) ['{9457c0d1-4ae6-40e3-94c0-486439b30e4d}']
  end;

implementation

end. 