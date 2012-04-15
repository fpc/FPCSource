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

Procedure CFMakeRaw(var tios:TermIOS);
begin
  with tios do
   begin
     c_iflag:=c_iflag and (not (IMAXBEL or IXOFF or INPCK or BRKINT or
                PARMRK or ISTRIP or INLCR or IGNCR or ICRNL or IXON or
                IGNPAR));
     c_iflag:=c_iflag OR IGNBRK;
     c_oflag:=c_oflag and (not OPOST);
     c_lflag:=c_lflag and (not (ECHO or ECHOE or ECHOK or ECHONL or ICANON or
                                ISIG or IEXTEN or NOFLSH or TOSTOP or PENDIN));
     c_cflag:=(c_cflag and (not (CSIZE or PARENB))) or (CS8 OR cread);
     c_cc[VMIN]:=1;
     c_cc[VTIME]:=0;
   end;
end;


Function real_tcsendbreak(fd,duration:cint):cint;cdecl;external name 'tcsendbreak';

Function TCSendBreak(fd,duration:cint):cint; {$ifdef VER2_0}inline;{$endif}
begin
  TCSendBreak:=real_tcsendbreak(fd,duration);
end;



Function TCSetPGrp(fd,id:cint):cint; {$ifdef VER2_0}inline;{$endif}
begin
  TCSetPGrp:=fpIOCtl(fd,TIOCSPGRP,pointer(id));
end;


Function TCGetPGrp(fd:cint;var id:cint):cint; {$ifdef VER2_0}inline;{$endif}
begin
  TCGetPGrp:=fpIOCtl(fd,TIOCGPGRP,@id);
end;

Function real_tcdrain(fd:cint):cint;cdecl;external name 'tcdrain';

Function  TCDrain(fd:cint)     :cint;inline;
begin
  TCDrain:=real_tcdrain(fd);
end;


Function real_tcflow(fd,act:cint):cint;cdecl;external name 'tcflow';

Function  TCFlow(fd,act:cint) :cint;inline;
begin
  TCFlow:=real_tcflow(fd,act);
end;


Function real_tcflush(fd,qsel:cint):cint;cdecl;external name 'tcflush';

Function  TCFlush(fd,qsel:cint):cint; inline;
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
 IsAtty:=ord(TCGetAttr(Handle,t) <> -1);
end;


Function IsATTY(var f: text):cint; {$ifdef VER2_0}inline;{$endif}
{
  Idem as previous, only now for text variables.
}
begin
  IsATTY:=IsaTTY(textrec(f).handle);
end;

