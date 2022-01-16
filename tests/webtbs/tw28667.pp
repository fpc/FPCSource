{%CPU=i386}
{%OPT=-O2 -Cg}

{$mode delphi}
program a;

type
  TChars=set of AnsiChar;

function get_char(var buf;var ofs:integer;const max:integer;const st:TChars):AnsiChar;
var
  rbuf:array[0..0] of AnsiChar absolute buf;
begin
  result:=#$20;
  while (ofs<max) and  (rbuf[ofs] in st) do
  begin
    Result:=rbuf[ofs];
    inc(ofs);
    break;
  end;
end;

var
 ofs:integer;
 buf:array[0..100] of ansichar;

begin
  ofs:=0;
  get_char(buf,ofs,100,['=','*'])
end.
