unit uw26922a;

{$mode objfpc}{$H+}

interface

uses
  unit2_test;

Type
  TTestAbstract = class

  end;

  TTest = class;

  TTestObject = class(specialize TTestObjectAbstract<TTest>);

  // Note: uncomment TTestAbstract when for recompilation
  TTest = class//(TTestAbstract)
    public
  end;


implementation

end.

