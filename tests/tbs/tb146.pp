{ Old file: tbs0170.pp }
{ Asm, {$ifdef} is seen as a separator                  OK 0.99.9 (PFV) }

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
