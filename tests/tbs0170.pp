procedure free1;
begin
end;

procedure free2;
begin
end;

begin
asm
        call {$ifdef dummy}free1{$else}free2{$endif}
end;
end.
