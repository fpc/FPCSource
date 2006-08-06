uses
  sysutils;
var
  b : boolean;
begin
  if not TryStrToBool('true',b) then
    halt(1);
  if not b then
    halt(1);
  if not TryStrToBool('false',b) then
    halt(1);
  if b then
    halt(1);

  if not TryStrToBool('True',b) then
    halt(1);
  if not b then
    halt(1);
  if not TryStrToBool('False',b) then
    halt(1);
  if b then
    halt(1);

  if not TryStrToBool('truE',b) then
    halt(1);
  if not b then
    halt(1);
  if not TryStrToBool('falsE',b) then
    halt(1);
  if b then
    halt(1);

  if not TryStrToBool('TRUE',b) then
    halt(1);
  if not b then
    halt(1);
  if not TryStrToBool('FALSE',b) then
    halt(1);
  if b then
    halt(1);

  if not TryStrToBool('3.1415',b) then
    halt(1);
  if not b then
    halt(1);
  if not TryStrToBool('0.0',b) then
    halt(1);
  if b then
    halt(1);

  if TryStrToBool('',b) then
    halt(1);

  if TryStrToBool('asdf',b) then
    halt(1);

  b:=StrToBool('truE');
  if not b then
    halt(1);
  b:=StrToBool('falsE');
  if b then
    halt(1);

  if not(StrToBoolDef('',true)) then
    halt(1);

  if StrToBoolDef('asdf',false) then
    halt(1);

  writeln('ok');
end.
