{ %CPU=i386 }
{ older ppc386 only define cpu86 }
{$ifdef cpu86}
{$define cpui386}
{$endif cpu86}
var
 x,y : byte;
 z : longint;
{$asmmode intel}

procedure test(var x : byte);
begin
  x:=5;
{$ifdef cpui386}
  asm
    mov   edi,$12345678
    mov   edi,x
    mov   dword ptr [edi],78
  end;
{$else cpui386}
  x:=$78;
{$endif cpui386}
end;

begin
  x:=34;
  test(x);
  if x<>78 then
    begin
      Writeln('Problem !!');
      Halt(1);
    end;
end.
