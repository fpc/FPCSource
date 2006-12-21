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
{$ifdef CPUI386}
        call {$ifdef dummy}free1{$else}free2{$endif}
{$endif CPUI386}
{$ifdef CPU68K}
        jsr {$ifdef dummy}free1{$else}free2{$endif}
{$endif CPU68K}
{$ifdef CPUARM}
        bl {$ifdef dummy}free1{$else}free2{$endif}
{$endif CPUARM}
end;
end.
