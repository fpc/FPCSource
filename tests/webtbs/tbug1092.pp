PROGRAM tbug1092;
USES Dos;
const
{$Ifdef linux}
  path='/etc';
{$else}
  path='c:\';
{$endif}
BEGIN
  if FSearch('tbug1092.pp',path))<>'tbug1092.pp' then
   begin
     writeln('FSearch didn''t find file in the current dir!');
     halt(1);
   end;
END.
