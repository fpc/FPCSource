{ %NORUN }

{ for helpers Self always refers to the extended class }
program tchlp56;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TFoo = class
    procedure DoFoo(aFoo: TFoo);
  end;

  TFooHelper = class helper for TFoo
    procedure Test;
  end;

procedure TFoo.DoFoo(aFoo: TFoo);
begin

end;

procedure TFooHelper.Test;
begin
  DoFoo(Self);
end;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test;
end.
