{$IFDEF UNIX}
{$linklib pthread}
{$ENDIF}
{$mode objfpc}
{$H+}

program tvlc;

uses  {$ifdef unix}cthreads,{$endif}libvlc, math;

Var
  Args: Array[0..3] of pchar;
  fhandle : pointer;
 
begin
  // This is needed, or loading the VLC libraries will fail with a SIGFPE
  setexceptionmask([exInvalidOp, exDenormalized, exZeroDivide,
                     exOverflow, exUnderflow, exPrecision]);
  LoadLibVLC(libname,False);
  args[0] := PAnsiChar('libvlc.so.5');
  Args[1] := PansiChar(ParamStr(1));
  args[2] := NIL;
  FHandle := libvlc_new(1, @args);
end.
