{ %NORUN }

program tw25917;

{$APPTYPE CONSOLE}
{$MODE DELPHI}

type
  TA<T1, T2> = class
  end;

  TB<T1, T2> = class
  private
    type
      T3 = record end;

      TC = class(TA<T1, T3>)
      public
        procedure Foo;
      end;
  end;

procedure TB<T1, T2>.TC.Foo;
var
  L: TB<T1, T2>.T3;
begin
end;

var
  x: TB<Pointer, Pointer>;

begin
end.

