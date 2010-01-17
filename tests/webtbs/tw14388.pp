program Project1;

{$mode objfpc}{$H+}

type
  TID4 = array[0..3] of char;

function GetID: TID4;
begin
  result:=#1#3#5#9;
end;


var
  ChunkID: TID4;
begin
  ChunkID:=#1#3#5#9;
  if GetID=ChunkID then
    writeln('ok')
  else
    halt(1);
end.
