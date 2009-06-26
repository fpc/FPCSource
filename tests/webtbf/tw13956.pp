{ %fail }

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

type
  { TForm1 }
  TForm1 = class
  private
    function GetFoo(Index: Integer; Ask: Boolean = True): Integer;
  public
    property Foo[Index: Integer; Ask: Boolean]: Integer read GetFoo;
  end; 

function TForm1.GetFoo(Index: Integer; Ask: Boolean): Integer;
begin
  Result := Foo[Index];
end;

begin
end.
