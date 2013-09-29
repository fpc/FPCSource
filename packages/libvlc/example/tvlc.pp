{$linklib pthread}
program tvlc;

uses cthreads, libvlc, math;

Var
  Args: Array[0..3] of pchar;
  fhandle : pointer;
 
begin
  // This is needed, or loading the VLC libraries will fail with a SIGFPE
  setexceptionmask([exInvalidOp, exDenormalized, exZeroDivide,
                     exOverflow, exUnderflow, exPrecision]);
  LoadLibVLC('libvlc.so.5',False);
  args[0] := PAnsiChar('libvlc.so.5');
  args[2] := NIL;
  FHandle := libvlc_new(1, @args);
end.
