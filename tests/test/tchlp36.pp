{ %NORUN }

program tchlp36;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
    procedure Test;
  end;

  TFoo = class

  end;

  TFooHelper = class helper(TObjectHelper) for TFoo
  end;

procedure TObjectHelper.Test;
begin
end;

var
  f: TFoo;
begin
  f.Test;
end.
