{ Source provided for Free Pascal Bug Report 3865 }
{ Submitted by "Jernej" on  2005-04-01 }
{ e-mail: jernejcoder@gmail.com }
uses
  SysUtils;

procedure wantitchars(const text: string; var wot: array of char);
begin
  fillchar(wot, sizeof(wot), 0);
  StrPCopy(wot, text); // FPC ERROR: incompatible type for arg no. 1: got "array of char", expected "pchar"
end;

var
  a : array[0..10] of char;
begin
  wantitchars('test',a);
  writeln(a);
  if a<>'test' then
    halt(1);
end.
