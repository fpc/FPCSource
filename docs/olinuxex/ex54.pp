Program Example54;

uses oldlinux;

{ Program to demonstrate the IOCtl function. }

var
  tios : Termios;
begin
  IOCtl(1,TCGETS,@tios);
  WriteLn('Input Flags  : $',hexstr(tios.c_iflag,8));
  WriteLn('Output Flags : $',hexstr(tios.c_oflag,8));
  WriteLn('Line Flags   : $',hexstr(tios.c_lflag,8));
  WriteLn('Control Flags: $',hexstr(tios.c_cflag,8));
end.
