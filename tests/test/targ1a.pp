{ %NEEDEDAFTER }
program go32v2_crash;

const
  MAX_SIZE = 256;
  SIZE_INC = 8;

type
  TMemArray = array [0..MAX_SIZE div SIZE_INC] of pointer;

var
  i, length_arg2 : longint;
  err :word;
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
  for i:=1 to MAX_SIZE div SIZE_INC do
    begin
      FreeMem(MemArray[i],Size(i));
    end;
  i:=length(paramstr(1));
  Writeln(stderr,'Everthing is fine, arg1 length=',i);
  val(paramstr(1),length_arg2,err);
  if err=0 then
    begin
      i:=length(paramstr(2));
      if (i<>length_arg2) then
        begin
          Writeln('Length of arg2 is ',i,' not ',length_arg2);
          halt(1);
        end
      else
        Writeln('length of arg2 OK: ',length_arg2);
    end;
end.
