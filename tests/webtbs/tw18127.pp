{ %norun% }
program tw18127;

{$mode objfpc}{$H+}

type
  TBar = class
  public
    type
      TSomeInt = integer;
  end;

  TFoo1 = class(TBar)
  public
    const
      one = 1;
    type
      TFoo2 = TSomeInt; // was error: Identifier not found "TSomeInt"
      TFoo3 = class
        function Func: TFoo2;
      end;
  end;

  function TFoo1.TFoo3.Func: TFoo2; // was error: Identifier not found "TFoo2"
  begin
    Result := one; // was error: Identifier not found "one"
  end;

begin
end.

