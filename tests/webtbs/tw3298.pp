{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3298 }
{ Submitted by "marco" on  2004-09-07 }
{ e-mail:  }
{$mode delphi}

const
  OffsetArray: array[0..3] of cardinal = ($FFFFFFFC,$FFFFFFFD,$FFFFFFFE,$FFFFFFFF);

procedure MMXUnpacked;
var
  l : cardinal;
begin
 asm
  mov esi, offset OffsetArray[2]
  mov eax,[esi]
  mov l,eax
 end;
 if l<>$FFFFFFFE then
   halt(1);
end;
begin
end.
