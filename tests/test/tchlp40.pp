{ published is allowed in mode Delphi, but unusable }
program tchlp40;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  {$M+}
  TFoo = class
  end;
  {$M-}

  TFooHelper = class helper for TFoo
  published
    function Test: Integer;
  end;

function TFooHelper.Test: Integer;
begin
  Result := 1;
end;

var
  f: TFoo;
  res: Pointer;
begin
  f := TFoo.Create;
  res := f.MethodAddress('Test');
{$ifdef fpc}
  Writeln('Address of TFoo.Test: ', res);
{$else}
  Writeln('Address of TFoo.Test: ', Integer(res));
{$endif}
  if res <> Nil then
    Halt(1);
  Writeln('ok');
end.
