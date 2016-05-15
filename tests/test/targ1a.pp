{ %NEEDEDAFTER }
program go32v2_crash;

const
  MAX_SIZE = 256;
  SIZE_INC = 8;

type
  TMemArray = array [0..MAX_SIZE div SIZE_INC] of pointer;

var
  i : longint;
  MemArray : TMemArray;

function Size(i: longint) : longint;
begin
  Size:=1+SIZE_INC*i;
end;

begin
  FillChar(MemArray,Sizeof(MemArray),#0);
  for i:=0 to MAX_SIZE div SIZE_INC do
    begin
      GetMem(MemArray[i],Size(i));
    end;
  for i:=0 to MAX_SIZE div SIZE_INC do
    begin
      FreeMem(MemArray[i],Size(i));
    end;
  Writeln(stderr,'Everything is fine');

end.
