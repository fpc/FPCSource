#!/usr/bin/env instantfpc
{ %OPT=-Sb }
{ -Sb skips a "#!" first line after a UTF-8 BOM }
program tshebang3;

begin
  if {$I %LINENUM%}<>7 then
    halt(1);
  writeln('ok');
end.
