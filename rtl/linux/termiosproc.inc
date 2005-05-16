{
  $Id: termiosproc.inc,v 1.7 2005/02/14 17:13:30 peter Exp $
}

{******************************************************************************
                         IOCtl and Termios calls
******************************************************************************}

Function TCGetAttr(fd:cint;var tios:TermIOS):cint;
begin
 {$ifndef BSD}
  TCGetAttr:=fpIOCtl(fd,TCGETS,@tios);
 {$else}
  TCGETAttr:=fpIoCtl(Fd,TIOCGETA,@tios);
 {$endif}
end;


Function TCSetAttr(fd:cint;OptAct:cint;const tios:TermIOS):cint;
var
  nr:cint;
begin
 {$ifndef BSD}
  case OptAct of
   TCSANOW   : nr:=TCSETS;
   TCSADRAIN : nr:=TCSETSW;
   TCSAFLUSH : nr:=TCSETSF;
 {$else}
  case OptAct of
   TCSANOW   : nr:=TIOCSETA;
   TCSADRAIN : nr:=TIOCSETAW;
   TCSAFLUSH : nr:=TIOCSETAF;
  {$endif}
  else
   begin
     fpsetErrNo(ESysEINVAL);
     TCSetAttr:=-1;
     exit;
   end;
  end;
  TCSetAttr:=fpIOCtl(fd,nr,@Tios);
end;


Procedure CFSetISpeed(var tios:TermIOS;speed:Cardinal);
begin
 {$ifndef BSD}
  tios.c_cflag:=(tios.c_cflag and (not CBAUD)) or speed;
 {$else}
  tios.c_ispeed:=speed; {Probably the Bxxxx speed constants}
 {$endif}
end;


Procedure CFSetOSpeed(var tios:TermIOS;speed:Cardinal);
begin
  {$ifndef BSD}
   CFSetISpeed(tios,speed);
  {$else}
   tios.c_ospeed:=speed;
  {$endif}
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


Function TCSendBreak(fd,duration:cint):cint;
begin
  TCSendBreak:=fpIOCtl(fd,TCSBRK,pointer(ptrint(duration)));
end;


Function TCSetPGrp(fd,id:cint):cint;
begin
  TCSetPGrp:=fpIOCtl(fd,TIOCSPGRP,pointer(ptrint(id)));
end;


Function TCGetPGrp(fd:cint;var id:cint):cint;
begin
  TCGetPGrp:=fpIOCtl(fd,TIOCGPGRP,@id);
end;

Function TCDrain(fd:cint):cint;
begin
 {$ifndef BSD}
  TCDrain:=fpIOCtl(fd,TCSBRK,pointer(1));
 {$else}
  TCDrain:=fpIOCtl(fd,TIOCDRAIN,0); {Should set timeout to 1 first?}
 {$endif}
end;


Function TCFlow(fd,act:cint):cint;
begin
  {$ifndef BSD}
   TCFlow:=fpIOCtl(fd,TCXONC,pointer(ptrint(act)));
  {$else}
    case act OF
     TCOOFF :  TCFlow:=fpIoctl(fd,TIOCSTOP,0);
     TCOOn  :  TCFlow:=fpIOctl(Fd,TIOCStart,0);
     TCIOFF :  {N/I}
    end;
  {$endif}
end;

Function TCFlush(fd,qsel:cint):cint;
begin
 {$ifndef BSD}
  TCFlush:=fpIOCtl(fd,TCFLSH,pointer(ptrint(qsel)));
 {$else}
  TCFlush:=fpIOCtl(fd,TIOCFLUSH,pointer(ptrint(qsel)));
 {$endif}
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


Function IsATTY(var f: text):cint;
{
  Idem as previous, only now for text variables.
}
begin
  IsATTY:=IsaTTY(textrec(f).handle);
end;

{
  $Log: termiosproc.inc,v $
  Revision 1.7  2005/02/14 17:13:30  peter
    * truncate log

}
