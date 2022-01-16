{ %NORUN }

program tmoperator1;

{$MODE OBJFPC}
{$modeswitch advancedrecords}

type

  { TFoo }

  TFoo = record
  private
    class operator Initialize(var aFoo: TFoo);
    class operator Finalize(var aFoo: TFoo);
  end;

{ TFoo }

class operator TFoo.Initialize(var aFoo: TFoo);
begin
end;

class operator TFoo.Finalize(var aFoo: TFoo);
begin
end;

begin
end. 