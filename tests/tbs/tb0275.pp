{ %CPU=i386 }
{ %OPT=-Cg- }
{ Old file: tbs0322.pp }
{  }

{$ifdef fpc}{$asmmode intel}{$endif}
var
  boxes : record
    pbox : longint;
    pbox2 : longint;
  end;
var
  s1,s2 : longint;
begin
asm
  mov s1,type boxes.pbox
  mov s2,type boxes
end;
  if s1<>sizeof(boxes.pbox) then
   begin
     writeln('Wrong size for TYPE');
     halt(1);
   end;
  if s2<>sizeof(boxes) then
   begin
     writeln('Wrong size for TYPE');
     halt(1);
   end;
end.
