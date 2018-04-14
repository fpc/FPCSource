{ %CPU=i386 }
{ %TARGET=win32 }
{$mode delphi}
program controlc;

{$ASMMODE intel}

uses
  Windows,
  SysUtils, Math;

type
  TSSE=record
    sse1,sse2,sse3,sse4:single;
  end;

  {.$codealign recordmin=16}
  {.$align 16}{.$packrecords 16}
  TSSE2=record
    prefix:longint;
    sse: TSSE;
  end;

  TTestProc = procedure; cdecl;


var
  a: TSSE2 = ( prefix: 0; sse: (sse1: 3.4E38; sse2: 3.4E38; sse3: 3.0; sse4: 4.0));
  b: TSSE2 = (prefix: 0; sse: (sse1: 3.4E38; sse2: 3.4E38; sse3: 0.0; sse4: 0.0));
  c: TSSE2 = (prefix: 0; sse: (sse1: 0.0; sse2: 0.0; sse3: 0.0; sse4: 0.0));

procedure FailureCode; cdecl; assembler;
asm
  movups xmm0, A.sse
  movups xmm1, B.sse
// divps xmm0, xmm1
  mulps xmm0, xmm1 // must be overflow but STATUS_FLOAT_MULTIPLE_FAULTS
  movups c.sse, xmm0
end;

procedure TestSafe(AProc: TTestProc);
begin
  Writeln('-- begin safe ---');
  try
    AProc;
  except
    on E: EOverflow do
    begin
      WriteLn(E.ClassName + ': ' + E.Message);
    end;
    on E : Exception do
      halt(1);
  end;
  Writeln('-- end safe ---');
end;

begin
  Writeln('== Default masking ===');
  TestSafe( FailureCode );

  Writeln('== Unmasked ===');
  SetExceptionMask( [] );
  TestSafe( FailureCode );
  writeln('ok');
end.
