#!/usr/bin/env instantfpc
{ %OPT=-Sb }
{ -Sb skips a "#!" first line; line numbers stay those of the file }
program tshebang1;

begin
  if {$I %LINENUM%}<>7 then
    halt(1);
  writeln('ok');
end.
