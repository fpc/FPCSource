{ %version=1.1}
{$ifdef fpc}
  {$mode delphi}
{$else}
type
  qword = int64;
{$endif}

var
   b : byte;
   i : smallint;
   i64 : int64;
   q : qword;
   p : pointer;

begin
   p:=pointer(b);
   p:=pointer(i);
   p:=pointer(i64);
   p:=pointer(q);
end.
