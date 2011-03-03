{ methods defined in a helper have higher priority than those defined in the
  extended type }
program tchlp66;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    function Test: Integer;
  end;

  TFooHelper = class helper for TFoo
  private
    function Test: Integer;
  public
    function AccessTest: Integer;
  end;

function TFoo.Test: Integer;
begin
  Result := 1;
end;

function TFooHelper.Test: Integer;
begin
  Result := 2;
end;

function TFooHelper.AccessTest: Integer;
begin
  Result := Test;
end;

var
  f: TFoo;
  res: Integer;
begin
  f := TFoo.Create;
  res := f.AccessTest;
  Writeln('f.AccessTest: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
