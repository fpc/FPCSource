{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2000 by Marco van de Voort
     member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

Function PClose(Var F:file) : cint;
var
  pl : ^cint;
  res: cint;
begin
  repeat
    res:=fpclose(filerec(F).Handle);
  until (res<>-1) or (fpgeterrno<>ESysEINTR);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  pclose := WaitProcess(pl^);
end;

Function PClose(Var F:text) :cint;
var
  pl  : ^cint;
  res : cint;
begin
  repeat
    res:=fpclose(Textrec(F).Handle);
  until (res<>-1) or (fpgeterrno<>ESysEINTR);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  pclose:= WaitProcess(pl^);
end;


// can't have oldfpccall here, linux doesn't need it.
Function AssignPipe(var pipe_in,pipe_out:cint):cint; [public, alias : 'FPC_SYSC_ASSIGNPIPE'];
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  ret  : longint;
  fdis : array[0..1] of cint;
begin
 fdis[0]:=pipe_in;
 fdis[1]:=pipe_out;
 ret:=pipe(fdis);
 pipe_in:=fdis[0];
 pipe_out:=fdis[1];
 AssignPipe:=ret;
end;

{$define FPC_HAS_GETTIMEZONEFILE}

function GetTimezoneFile:shortstring;
var
  tzenv : pchar;
  l: longint;
  s: shortstring;
  ft : text;
begin
  GetTimeZoneFile:='';
  { the TZ variable holds the name of the timezone (possibly followed by a
    comma and rules), and the timezone files themselves are stored in
    /usr/share/lib/zoneinfo }
  tzenv:=fpgetenv('TZ');
  if assigned(tzenv) then
    begin
      s:=strpas(tzenv);
      l:=pos(',',s);
      if l<>0 then
        s:=copy(s,1,l-1);
      GetTimeZoneFile:='/usr/share/lib/zoneinfo/'+s;
    end
  else
    exit;
  assign(ft,GetTimeZoneFile);
{$push}
{$I-}  
  reset(ft);
  if IOResult=0 then
    close(ft)
  else
    GetTimeZoneFile:='';
{$pop}
end;


{ should probably be defined in ostypes.inc for all OSes }
const
  F_RDLCK =        01;      (* Read lock *)
  F_WRLCK =        02;      (* Write lock *)
  F_UNLCK =        03;      (* Remove lock(s) *)


Function fpFlock (fd,mode : longint) : cint;
  var
    fl : flock;
    cmd : cint;
  begin
    { initialize the flock struct to set lock on entire file }
    fillchar(fl,sizeof(fl),0);

    { In non-blocking lock, use F_SETLK for cmd, F_SETLKW otherwise }
    if (mode and LOCK_NB)<>0 then
      begin
        cmd:=F_SETLK;
        { turn off this bit }
        mode:=mode and not(LOCK_NB);
      end
    else
      cmd:=F_SETLKW;

    case mode of
      LOCK_UN:
        fl.l_type:=fl.l_type or F_UNLCK;
      LOCK_SH:
        fl.l_type:=fl.l_type or F_RDLCK;
      LOCK_EX:
        fl.l_type:=fl.l_type or F_WRLCK;
      else
        begin
          errno:=ESysEINVAL;
          fpFlock:=-1;
	  exit;
        end;
    end;

    fpFlock:=fpFcntl(fd,cmd,fl);

    if (fpFlock=-1) and (errno=ESysEACCES) then
       errno:=ESysEWOULDBLOCK;
  end;

