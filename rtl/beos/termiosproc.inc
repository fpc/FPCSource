{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Termios implementation for FreeBSD

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


{******************************************************************************
                         IOCtl and Termios calls
******************************************************************************}

Function TCGetAttr(fd:cint;var tios:TermIOS):cint;
begin
  TCGETAttr:=fpIoCtl(Fd,TIOCGETA,@tios);
end;


Function TCSetAttr(fd:cint;OptAct:cint;const tios:TermIOS):cint;
var
  nr:cint;
begin
  case OptAct of
   TCSANOW   : nr:=TIOCSETA;
   TCSADRAIN : nr:=TIOCSETAW;
   TCSAFLUSH : nr:=TIOCSETAF;
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
  // field unused under BeOS
  tios.c_ixxxxx:=speed; 
end;


Procedure CFSetOSpeed(var tios:TermIOS;speed:Cardinal);
begin
  // field unused under BeOS
  tios.c_oxxxxx:=speed;
end;



Procedure CFMakeRaw(var tios:TermIOS);
begin
  with tios do
   begin
     c_iflag:=c_iflag and (not (IXOFF or INPCK or BRKINT or
                PARMRK or ISTRIP or INLCR or IGNCR or ICRNL or IXON or
                IGNPAR));
     c_iflag:=c_iflag OR IGNBRK;
     c_oflag:=c_oflag and (not OPOST);
     c_lflag:=c_lflag and (not (ECHO or ECHOE or ECHOK or ECHONL or ICANON or
                                ISIG or IEXTEN or NOFLSH or TOSTOP));
     c_cflag:=(c_cflag and (not (CSIZE or PARENB))) or (CS8 OR cread);
     c_cc[VMIN]:= 1;
     c_cc[VTIME]:= 0;
   end;
end;

Function TCSendBreak(fd,duration:cint):cint;
begin
  TCSendBreak:=fpIOCtl(fd,TIOCSBRK,nil);
end;

Function be_tcsetpgrp(fd, pgrpid : pid_t) : cint; cdecl; external 'root' name 'tcsetpgrp';
Function be_tcgetpgrp(fd : cint) : pid_t; cdecl; external 'root' name 'tcgetpgrp';
Function be_tcdrain(fd : cint) : cint; cdecl; external 'root' name 'tcdrain';
Function be_tcflow(fd, action : cint) : cint; cdecl; external 'root' name 'tcflow';
Function be_tcflush(fd, queue_selector : cint) : cint; cdecl; external 'root' name 'tcflush';


Function TCSetPGrp(fd,id:cint):cint;
begin
  TCSetPGrp := be_tcsetpgrp(fd, id);
end;


Function TCGetPGrp(fd:cint;var id:cint):cint;
begin
  id := be_tcgetpgrp(fd);
end;

Function TCDrain(fd:cint):cint;
begin
  TCDrain := be_tcdrain(fd);
end;


Function TCFlow(fd,act:cint):cint;
begin
  TCFlow := be_tcflow(fd, act);
end;

Function TCFlush(fd,qsel:cint):cint;
begin
  TCFlush := be_tcflush(fd, qsel);
end;

Function BeOSIsATTY (Handle:cint):cint; cdecl; external 'root' name 'isatty';

Function IsATTY (Handle:cint):cint;
{
  Check if the filehandle described by 'handle' is a TTY (Terminal)
}
begin
 IsAtty:= BeOSIsATTY(Handle);
end;

Function IsATTY(var f: text):cint;
{
  Idem as previous, only now for text variables.
}
begin
  IsATTY:=IsaTTY(textrec(f).handle);
end;

