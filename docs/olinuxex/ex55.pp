Program Example55;

uses oldlinux;

{ Program to demonstrate the TCGetAttr/TCSetAttr/CFMakeRaw functions. }

procedure ShowTermios(var tios:Termios);
begin
  WriteLn('Input Flags  : $',hexstr(tios.c_iflag,8)+#13);
  WriteLn('Output Flags : $',hexstr(tios.c_oflag,8));
  WriteLn('Line Flags   : $',hexstr(tios.c_lflag,8));
  WriteLn('Control Flags: $',hexstr(tios.c_cflag,8));
end;

var
  oldios,
  tios : Termios;
begin
  WriteLn('Old attributes:');
  TCGetAttr(1,tios);
  ShowTermios(tios);
  oldios:=tios;
  Writeln('Setting raw terminal mode');
  CFMakeRaw(tios);
  TCSetAttr(1,TCSANOW,tios);
  WriteLn('Current attributes:');
  TCGetAttr(1,tios);
  ShowTermios(tios);
  TCSetAttr(1,TCSANOW,oldios);
end.
