
{$ifdef fpc}{$mode Delphi}{$endif}

var
 x:function(x:longint):longint;
 y:pointer absolute x;
begin
  if y<>nil then
   halt(1);
end.
