uses
{$ifdef unix}
  cwstring,
{$endif}
  variants,sysutils;
var a:variant;
begin
  a:=VarArrayCreate([0,2,0,2],varVariant);
  if VarArrayDimCount(a)<>2 then
    halt(1);
  VarArrayPut(a,'b',[1,1]);
  if String(VarArrayGet(a,[1,1]))<>'b' then
    halt(2);
  a[2,1]:='asdf';
  if VarArrayGet(a,[2,1])<>'asdf' then
    halt(2);
  a:='';
  writeln('ok');
end.


