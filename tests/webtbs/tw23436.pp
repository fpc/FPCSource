program testcase;

{$APPTYPE CONSOLE}
{$mode objfpc}

begin
  if ByteBool(true) then
    writeln('ByteBool(true) ok')
  else
    halt(1);
  if not ByteBool(false) then
    writeln('ByteBool(not false) ok')
  else
    halt(2);
  if WordBool(true) then
    writeln('WordBool(true) ok')
  else
    halt(3);
  if not WordBool(false) then
    writeln('WordBool(not false) ok')
  else
    halt(4);
  if LongBool(true) then
    writeln('LongBool(true) ok')
  else
    halt(5);
  if not LongBool(false) then
    writeln('LongBool(not false) ok')
  else
    halt(6);
{$ifdef FPC}
  if QWordBool(true) then
    writeln('QWordBool(true) ok')
  else
    halt(7);
  if not QWordBool(false) then
    writeln('QWordBool(not false) ok')
  else
    halt(8);
{$endif FPC}

  Writeln('ok');
end.

