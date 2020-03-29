{ %OPT=-O3 }
{$mode objfpc}
{$R+}
{$H+}
program xx;


uses sysutils;


function strlsIndexOf(str, searched: pansichar; l1, l2: longint): longint;
begin
  result:=-1; //function removed for minimal example
end;


function strindexof(const str, searched: string): longint; inline;
begin
  if str = '' then begin result := 0; exit; end;
  result := strlsIndexOf(pchar(pointer(str)) , pchar(searched), length(str) , length(searched));
  if result < 0 then begin result := 0; exit; end;
  inc(result);
end;


function strBefore(const s, sep: string): string;
var
  i: Integer;
begin
  i := strIndexOf(s, sep); // line 26
  if i = 0 then result := ''
  else result := copy(s, 1, i-1);
end;


begin
  strBefore('hallo', 'a');
end.