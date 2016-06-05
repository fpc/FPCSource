{ %opt=-g-h }

uses
  sysutils;

var
  pw: pwidechar;
begin
  pw:=widestralloc(1);
  pw^:='a';
  if StrBufSize(pw)<>2 then
    halt(1);
  StrDispose(pw);
end.
