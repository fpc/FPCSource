{$mode objfpc}
uses
  SysUtils,Contnrs;

const
  { keys chosen so that they do not collide with the MurmurHash3 based HashString
    ('D' and 'F' share the same bucket) }
  Keys : array[0..5] of AnsiChar = ('A','B','C','D','E','G');

var
  colls : Integer;

// Test for Hashing
procedure Test;
 var HL:TFPHashList;
     i,n:integer;
     dat:array[0..5]of pinteger;

begin
 HL:=TFpHashList.Create;
 HL.Capacity:=389;

 // Create pointer for data
 for i:=0 to 5 do
  begin
   dat[i]:=new(pinteger);
   dat[i]^:=i;
  end;

 // add keys with pointer
 for i:=0 to 5 do
    Writeln('HL.Add: '+Keys[i]+' = Index: '
                    +IntToStr(HL.Add(Keys[i],dat[i])));

 // get collisions
 for i:=0 to 5 do
   begin
    Writeln('--------------');
    Writeln('Collision for Index: '+IntToStr(i));
    n:=HL.FindIndexOf(Keys[i]);
    while n>=0 do
     begin
      Writeln('Index: '+IntToStr(n)+
                      ' | NameOfIndex: '+HL.NameOfIndex(n)+
                      ' | HashOfIndex: '+IntToStr(HL.HashOfIndex(n))+
                      ' | NextCollision: '+IntToStr(HL.GetNextCollision(n)));
      n:=HL.GetNextCollision(n);
      if n<>-1 then
        inc(colls);
     end; //while
   end; //for

 HL.Free;
 for i:=0 to 5 do dispose(dat[i]);
end;

begin
  Test;
  if colls>0 then
    halt(1);
end.

