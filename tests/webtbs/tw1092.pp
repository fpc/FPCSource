PROGRAM tbug1092;
USES Dos;
const
{$Ifdef Unix}
  path='/etc';
{$else}
  path='c:\';
{$endif}
var
  t : text;
BEGIN
  { create a file }
  assign(t,'tbug1092.tmp');
  rewrite(t);
  close(t);
  if FSearch('tbug1092.tmp',path)<>'tbug1092.tmp' then
   begin
     writeln('FSearch didn''t find file in the current dir!');
     halt(1);
   end;
  erase(t);
END.
