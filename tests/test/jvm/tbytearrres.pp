program tbytearrres;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  ByteArray = array of byte;

function GetUInt32(Src: array of byte; Offset : integer) : cardinal;
begin
  result:=src[offset];
end;

function JByteArrayToByteArray(A : Arr1jbyte; Start: integer = 0; Count : integer = -1) : ByteArray;
var
  i: longint;
begin
  if count=-1 then
    count:=length(a);
  setlength(result,count);
  for i:=start to start+count-1 do
    result[i-start]:=a[i];
end;

function AddressToInt(X : JNInetAddress) : Cardinal;
begin
 result := GetUInt32(JByteArrayToByteArray(X.getAddress()), 0);
end;

var
  c: cardinal;
begin
  c:=AddressToInt(JNInetAddress.getLocalHost);
  JLSystem.fout.println(int64(c));
end.
