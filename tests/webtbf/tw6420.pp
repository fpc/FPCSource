{ %fail }

{$mode delphi}

type
  IA = interface
    procedure B(out c: Integer);
  end;

  TA = class(TInterfacedObject, IA)
    procedure B(var c: Integer);
  end;

procedure TA.B(var c: Integer);
begin
end;


begin
end.
