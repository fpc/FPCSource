{ %fail }

program project1;

{$mode objfpc}{$H+}

type

IExample = interface
  function add(a, b: single): integer; 
end;

{ TExample }

TExample = class (TInterfacedObject, IExample)
  function add(a, b: single): single;
end;

function texample.add(a, b: single): single;
begin
end;

begin
end.
