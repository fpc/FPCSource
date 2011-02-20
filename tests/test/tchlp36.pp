{ %FAIL }

{ a class helper must extend a subclass of the parent class helper }
program tchlp36;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TBar = class

  end;

  TBarHelper = class helper for TBar
    procedure Test;
  end;

  TFoo = class

  end;

  TFooHelper = class helper(TBarHelper) for TFoo
  end;

procedure TBarHelper.Test;
begin
end;

begin
end.
