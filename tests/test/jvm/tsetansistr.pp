program tsetansistr;

{$mode delphi}
{$modeswitch unicodestrings}

type
  ByteArray = array of byte;

const
  AnsiStrOffset = 1;

function AnsiStringOfBytes(const Src : ByteArray) : AnsiString;
var
 i : integer;
begin
 SetLength(Result, Length(Src));

 for i := 0 to Length(Src) - 1 do
   Result[i + AnsiStrOffset] := Chr(Src[i]);
end;

var
 A : ByteArray;
 B : AnsiString;
begin
 DefaultSystemCodePage:=20127; // ASCII
 SetLength(A, 1); A[0] := $98;
 B := AnsiStringOfBytes(A);
 if ord(B[1]) <> $98 then
   halt(1);
end.
