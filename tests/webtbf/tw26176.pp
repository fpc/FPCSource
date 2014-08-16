{ %fail }

program tw26176;

{$MODE OBJFPC}
{$MODESWITCH TYPEHELPERS}

type
  TIH = type helper for Int32
    // NO (!) error - Forward declaration not solved "Foo(TObject);"
    procedure Foo(Sender: TObject);
  end;

begin
end.

