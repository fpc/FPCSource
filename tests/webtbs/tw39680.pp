{ %NORUN }

program tw39680;

{$mode objfpc}{$H+}
{$ModeSwitch implicitfunctionspecialization}

uses
  Generics.Collections;

generic procedure Foo<T>(lst: specialize TEnumerable<T>);
begin
end;

var
  lst: specialize TList<Integer>; // Inherits from TEnumerable
begin
  Foo(lst); // Error
  specialize Foo<Integer>(lst); // works
end.

