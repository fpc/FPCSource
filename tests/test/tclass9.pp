program tclass9;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}
{$APPTYPE CONSOLE}

type
  TSomeClass = class
  private
    type
      TSomeType = type integer;    // an internal type
    class var
      FSomeClassVar: TSomeType;    // class variable belongs to class, not an instance
    var
      FSomeIntanceVar: TSomeType;  // instance variable belongs to instance. it is a usual field
    class procedure SetSomeClassVar(const AValue: TSomeType); static;
  public
    class property SomeProperty: TSomeType read FSomeClassVar write SetSomeClassVar; // class property - belongs to class
    property SomeInstanceProp: TSomeType read FSomeIntanceVar;
  end;

{ TSomeClass }

class procedure TSomeClass.SetSomeClassVar(const AValue: TSomeType);
begin
   FSomeClassVar := AValue;
end;

var
  SomeClass: TSomeClass;
begin
  SomeClass.SomeProperty := 1;
  WriteLn(SomeClass.SomeProperty);
end.
