PROGRAM tbug1092;
USES Dos;
const
{$Ifdef linux}
  path='/etc';
{$else}
  path='c:\';
{$endif}
var
  t : text;
BEGIN
  { create a file }
  assign(t,'tbug1092.txt');
  rewrite(t);
  close(t);
  if FSearch('tbug1092.txt',path)<>'tbug1092.txt' then
   begin
     writeln('FSearch didn''t find file in the current dir!');
     halt(1);
   end;
END.
