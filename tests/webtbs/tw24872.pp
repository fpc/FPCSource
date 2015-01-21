{ %NORUN }

program tw24872;

{$mode delphi}

procedure Test;
begin
end;

type
  TRec<T> = record {for generic class is ok, and non generic record too}
    procedure Foo;
  end;

procedure TRec<T>.Foo;
begin
  Test
end; // Error: Global Generic template references static symtable

begin
end.

