Program Example54;

uses BaseUnix,Termio;

{ Program to demonstrate the IOCtl function. }

var
  tios : Termios;

begin
  {$ifdef FreeBSD}
    fpIOCtl(1,TIOCGETA,@tios);  // these constants are very OS dependant.
                                // see the tcgetattr example for a better way
  {$endif}
  WriteLn('Input Flags  : $',hexstr(tios.c_iflag,8));
  WriteLn('Output Flags : $',hexstr(tios.c_oflag,8));
  WriteLn('Line Flags   : $',hexstr(tios.c_lflag,8));
  WriteLn('Control Flags: $',hexstr(tios.c_cflag,8));
end.
