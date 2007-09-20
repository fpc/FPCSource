
function DoCheck(Key:WideChar):boolean;
begin
 DoCheck:=(Key in [WideChar(#0), WideChar(#8), WideChar(#10),
    WideChar(#13), WideChar(#27), WideChar(#127)]);
end;

var
  Key:WideChar;
  err : boolean;
begin
  for Key:=WideChar(128) to WideChar(2048) do
   if DoCheck(Key) then
    begin
     writeln(word(Key),' (',Key,') in set');
     err:=true;
    end;
  if err then halt(1);
end.
