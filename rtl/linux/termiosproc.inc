{
}

{******************************************************************************
                         IOCtl and Termios calls
******************************************************************************}

Function TCGetAttr(fd:cint;var tios:TermIOS):cint; {$ifdef VER2_0}inline;{$endif}
begin
  TCGetAttr:=fpIOCtl(fd,TCGETS,@tios);
end;


Function TCSetAttr(fd:cint;OptAct:cint;const tios:TermIOS):cint;
var
  nr:culong;
begin
  case OptAct of
   TCSANOW   : nr:=TCSETS;
   TCSADRAIN : nr:=TCSETSW;
   TCSAFLUSH : nr:=TCSETSF;
  else
   begin
     fpsetErrNo(ESysEINVAL);
     TCSetAttr:=-1;
     exit;
   end;
  end;
  TCSetAttr:=fpIOCtl(fd,nr,@Tios);
end;


Procedure CFSetISpeed(var tios:TermIOS;speed:Cardinal); {$ifdef VER2_0}inline;{$endif}
begin
  tios.c_cflag:=(tios.c_cflag and (not CBAUD)) or speed;
end;


Procedure CFSetOSpeed(var tios:TermIOS;speed:Cardinal); {$ifdef VER2_0}inline;{$endif}
begin
   CFSetISpeed(tios,speed);
end;


{ checked against glibc 2.3.3 (FK) }
Procedure CFMakeRaw(var tios:TermIOS);
begin
  with tios do
   begin
     c_iflag:=c_iflag and (not (IGNBRK or BRKINT or PARMRK or ISTRIP or
                                INLCR or IGNCR or ICRNL or IXON));
     c_oflag:=c_oflag and (not OPOST);
     c_lflag:=c_lflag and (not (ECHO or ECHONL or ICANON or ISIG or IEXTEN));
     c_cflag:=(c_cflag and (not (CSIZE or PARENB))) or CS8;
     c_cc[VMIN]:=1;
     c_cc[VTIME]:=0;
   end;
end;


Function TCSendBreak(fd,duration:cint):cint;  {$ifdef VER2_0}inline;{$endif}
begin
  TCSendBreak:=fpIOCtl(fd,TCSBRK,pointer(ptrint(duration)));
end;


Function TCSetPGrp(fd,id:cint):cint;  {$ifdef VER2_0}inline;{$endif}
begin
  TCSetPGrp:=fpIOCtl(fd,TIOCSPGRP,pointer(ptrint(id)));
end;


Function TCGetPGrp(fd:cint;var id:cint):cint;  {$ifdef VER2_0}inline;{$endif}
begin
  TCGetPGrp:=fpIOCtl(fd,TIOCGPGRP,@id);
end;

Function TCDrain(fd:cint):cint;  {$ifdef VER2_0}inline;{$endif}
begin
  TCDrain:=fpIOCtl(fd,TCSBRK,pointer(1));
end;


Function TCFlow(fd,act:cint):cint; {$ifdef VER2_0}inline;{$endif}
begin
   TCFlow:=fpIOCtl(fd,TCXONC,pointer(ptrint(act)));
end;

Function TCFlush(fd,qsel:cint):cint;  {$ifdef VER2_0}inline;{$endif}
begin
  TCFlush:=fpIOCtl(fd,TCFLSH,pointer(ptrint(qsel)));
end;

Function IsATTY (Handle:cint):cint;
{
  Check if the filehandle described by 'handle' is a TTY (Terminal)
}
var
  t : Termios;
begin
  if TCGetAttr(Handle,t)=0 then
    IsAtty:=1
  else
    IsAtty:=0;
end;


Function IsATTY(var f: text):cint;  {$ifdef VER2_0}inline;{$endif}
{
  Idem as previous, only now for text variables.
}
begin
  IsATTY:=IsaTTY(textrec(f).handle);
end;

