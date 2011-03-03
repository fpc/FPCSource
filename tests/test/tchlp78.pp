{ size of a class helper is size of a pointer }
program tchlp78;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TObjectHelper = class helper for TObject
  end;

begin
  Writeln('Size of TObjectHelper: ', SizeOf(TObjectHelper));
  if SizeOf(TObjectHelper) <> SizeOf(Pointer) then
    Halt(1);
  Writeln('ok');
end.
