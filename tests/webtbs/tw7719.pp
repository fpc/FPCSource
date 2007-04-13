{ %cpu=i386,x86_64 }
{$asmmode intel}
program test;

{$ifdef CPU386}
  {$define USE_MMX_UNIT}
  { I do not know if SSE2 is always true for x86_64
    but mmx unit is i386 specific PM }
{$endif CPU386}

{$ifdef USE_MMX_UNIT}
uses
  mmx;
{$endif USE_MMX_UNIT}

{$APPTYPE CONSOLE}

procedure call_psrldq;
begin
  asm
    psrldq xmm2,4
  end;
end;

begin
{$ifdef USE_MMX_UNIT}
  if not is_sse2_cpu then
    begin
      Writeln('SSE2 extension not supported by CPU');
      Writeln('SSE2 specific code not run');
    end
  else
{$endif USE_MMX_UNIT}
    begin
      Writeln('SSE2 extension supported by CPU');
      call_psrldq;
      Writeln('SSE2 extension code run');
    end;
end.
