unit utstrcopy;

interface

uses punit,utrtl;

implementation

uses strings;

function test_strcopy : TTeststring;

Type
  TCharArray = array[0..256] of char;
  TLongCharArray = array[0..512] of char;

var
  p: pchar;
  s: TCharArray;
  buf: TLongCharArray;
  i, j, l: longint;
  id : string;
  
begin
  Result:='';
  s:=Default(TCharArray);
  buf:=Default(TLongCharArray);
  for i := 0 to 256 do
    begin
      Str(i,ID);
      fillchar(s,sizeof(s),'b');
      s[i] := #0;
      for j := 0 to 3 do
        begin
          fillchar(buf,sizeof(buf),'a');
          p := strcopy(@buf[j+32],@s[0]);
          if not AssertEquals('Error 0',@buf[j+32],P) then exit;
          for l := 0 to j+31 do
            If not assertEquals('Error 1 (i='+id+')','a',buf[l]) then exit;
          for l := j+32 to j+32+i-1 do
            If not assertEquals('Error 2 (i='+id+')','b',buf[l]) then exit;
          if not AssertEquals('Error 3 (i='+id+')',#0,buf[j+i+32]) then exit;
          for l := j+i+32+1 to 512 do
            If not assertEquals('Error 4 (i='+id+')','a',buf[l]) then exit;
        end;
    end;
end;

begin
  AddTest('test_strcopy',@test_strcopy,EnsureSuite('Strings'));
end.
