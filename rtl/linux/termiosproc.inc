{
}

{******************************************************************************
                         IOCtl and Termios calls
******************************************************************************}

{$ifndef FPC_USE_LIBC}

Function TCGetAttr(fd:cint;var tios:TermIOS):cint; inline;
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


Procedure CFSetISpeed(var tios:TermIOS;speed:Cardinal); inline;
begin
  tios.c_cflag:=(tios.c_cflag and (not CBAUD)) or speed;
end;


Procedure CFSetOSpeed(var tios:TermIOS;speed:Cardinal); inline;
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


Function TCSendBreak(fd,duration:cint):cint;  inline;
begin
  TCSendBreak:=fpIOCtl(fd,TCSBRK,pointer(ptrint(duration)));
end;


Function TCSetPGrp(fd,id:cint):cint;  inline;
begin
  TCSetPGrp:=fpIOCtl(fd,TIOCSPGRP,pointer(ptrint(id)));
end;


Function TCGetPGrp(fd:cint;var id:cint):cint;  inline;
begin
  TCGetPGrp:=fpIOCtl(fd,TIOCGPGRP,@id);
end;

Function TCDrain(fd:cint):cint;  inline;
begin
  TCDrain:=fpIOCtl(fd,TCSBRK,pointer(1));
end;


Function TCFlow(fd,act:cint):cint; inline;
begin
   TCFlow:=fpIOCtl(fd,TCXONC,pointer(ptrint(act)));
end;

Function TCFlush(fd,qsel:cint):cint;  inline;
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

Function IsATTY(var f: text):cint;  inline;
{
  Idem as previous, only now for text variables.
}
begin
  IsATTY:=IsaTTY(textrec(f).handle);
end;

{$else}
// We plan to use FPC_USE_LIBC for Debian/kFreeBSD. This means that we need
// to avoid IOCTLs, since those go to the kernel and need to be FreeBSD specific.
// -> reroute as much as possible to libc.

function real_tcsendbreak(fd,duration: cint): cint; cdecl; external name 'tcsendbreak';
function real_tcdrain(fd: cint): cint; cdecl; external name 'tcdrain';
function real_tcflow(fd,act:cint): cint; cdecl; external name 'tcflow';
function real_tcflush(fd,qsel: cint): cint; cdecl; external name 'tcflush';
Function real_TCSetAttr(fd:cint;OptAct:cint;constref tios:TermIOS):cint; cdecl; external name 'tcsetattr';
Function real_TCGetAttr(fd:cint;var tios:TermIOS):cint; cdecl; external name 'tcgetattr';

function real_tcgetpgrp(fd:cint):pid_t; cdecl; external name 'tcgetpgrp';
function real_tcsetpgrp(fd: cint; pgrp: pid_t): cint; cdecl; external name 'tcsetpgrp';

Function TCGetAttr(fd:cint;var tios:TermIOS):cint; inline;
begin
  TCGetAttr:=real_tcgetattr(fd,tios);
end;

Function TCSetAttr(fd:cint;OptAct:cint;const tios:TermIOS):cint;
begin
  TCSetAttr:=Real_TCSetAttr(fd,OptAct,tios);
end;

Procedure CFSetISpeed(var tios:TermIOS;speed:Cardinal); inline;
begin
  tios.c_cflag:=(tios.c_cflag and (not CBAUD)) or speed;
end;


Procedure CFSetOSpeed(var tios:TermIOS;speed:Cardinal); inline;
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

Function TCSendBreak(fd,duration:cint):cint;  inline;
begin
  TCSendBreak:=real_tcsendbreak(fd,duration);
end;

Function TCSetPGrp(fd,id:cint):cint;  inline;
begin
  TCSetPGrp:=real_tcsetpgrp(fd,id);;
end;

Function TCGetPGrp(fd:cint;var id:cint):cint;  inline;
begin
  id:=real_tcgetpgrp(fd);
  tcgetpgrp:=id;  
end;

Function TCDrain(fd:cint):cint;  inline;
begin
  TCDrain:=real_TCDrain(fd);
end;


Function TCFlow(fd,act:cint):cint; inline;
begin
  TCFlow:=real_tcflow(fd,act);
end;

Function TCFlush(fd,qsel:cint):cint;  inline;
begin
  TCFlush:=real_tcflush(fd,qsel);
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

Function IsATTY(var f: text):cint;  inline;
{
  Idem as previous, only now for text variables.
}
begin
  IsATTY:=IsaTTY(textrec(f).handle);
end;
{$endif}
