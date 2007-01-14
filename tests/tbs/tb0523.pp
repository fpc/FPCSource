{ %target=i386 }
{$asmmode intel}

var
  a : array[0..1] of byte;
  myresult : boolean;
begin
  a[0]:=$ff;
  a[1]:=$1;
  asm
    mov esi,offset a
    mov dl,1
    test byte ptr [esi+1], dl
    setnz myresult
  end;
  if not(myresult) then
    halt(1);
  writeln('ok');
end.
