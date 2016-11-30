{ %NORUN }

program tw30761;

{$mode objfpc}

type
  Ta = class
    public
      procedure Test;
  end;

  Tb = class(Ta)
  end;

  TbHelper = class helper for Tb
    public
      procedure Test(i: integer); overload;
  end;

procedure Ta.Test;
begin
end;

procedure TbHelper.Test(i: integer);
begin
  //Self.Test;
end;

var
  b: Tb;

begin
  b:=Tb.Create;
  b.Test(1); // Error: Wrong number of parameters specified for call to "Test"
  b.Test;
end.

