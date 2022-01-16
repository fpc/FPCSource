{ %FAIL }

program tmoperator6;

{$MODE DELPHI}

type

  { TFoo }

  TFoo = record
  private
    class operator Initialize(var aFoo: TFoo): Boolean;
    class operator Finalize(var aFoo: Pointer);
  end;

{ TFoo }

class operator TFoo.Initialize(var aFoo: TFoo): Boolean;
begin
end;

class operator TFoo.Finalize(var aFoo: Pointer);
begin
end;

begin
end. 