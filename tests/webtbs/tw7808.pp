{ %cpu=i386 }
{ %target=go32v2,linux,freebsd,win32}
{ %opt=-Sew }

{$mode delphi}

procedure test(l: longint); stdcall;
begin
  if l<>longint($deadbeef) then
    halt(1);
end;

begin
  asm
    push word $dead
    push word $beef
    call test
  end;
end.
