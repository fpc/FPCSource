{ Source provided for Free Pascal Bug Report 2065 }
{ Submitted by "Lavergne Thomas" on  2002-08-01 }
{ e-mail: thomas.lavergne@laposte.net }
program test;

{$ifndef MACOS}
{$APPTYPE CONSOLE}
{$else}
{$APPTYPE TOOL}
{$endif}

{$LONGSTRINGS ON}

type
  TRec = record
    S: string;
    P: Pointer;
  end;

const
  MAX_SIZE = 2;

var
  A: array[0..MAX_SIZE - 1] of TRec;

begin
  WriteLn(A[0].S);
end.
