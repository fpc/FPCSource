{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file implements IPC calls calls for Linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit ipc;

interface

{ ----------------------------------------------------------------------
  General IPC stuff
  ----------------------------------------------------------------------}

Var
  IPCError : longint;

Type
   TKey   = Longint;
   PULong = ^Cardinal;
   PWord  = ^Word;

Const
  { IPC flags for get calls }

  IPC_CREAT  =  1 shl 9;  { create if key is nonexistent }
  IPC_EXCL   =  2 shl 9;  { fail if key exists }
  IPC_NOWAIT =  4 shl 9;  { return error on wait }

  { Actions for ctl calls }

  IPC_RMID = 0;     { remove resource }
  IPC_SET  = 1;     { set ipc_perm options }
  IPC_STAT = 2;     { get ipc_perm options }
  IPC_INFO = 3;     { see ipcs }

type
  PIPC_Perm = ^TIPC_Perm;
  TIPC_Perm = record
    key : TKey;
    uid,
    gid,
    cuid,
    cgid,
    mode,
    seq : Word;
  end;

{ Function to generate a IPC key. }
Function ftok (Path : String; ID : char) : TKey;

{ ----------------------------------------------------------------------
  Shared memory stuff
  ----------------------------------------------------------------------}

Type
  PShmid_DS = ^TShmid_ds;
  TShmid_ds = record
    shm_perm  : TIPC_Perm;
    shm_segsz : longint;
    shm_atime : longint;
    shm_dtime : longint;
    shm_ctime : longint;
    shm_cpid  : word;
    shm_lpid  : word;
    shm_nattch : integer;
    shm_npages : word;
    shm_pages  : Pointer;
    attaches   : pointer;
  end;

  const
     SHM_R      = 4 shl 6;
     SHM_W      = 2 shl 6;
     SHM_RDONLY = 1 shl 12;
     SHM_RND    = 2 shl 12;
     SHM_REMAP  = 4 shl 12;
     SHM_LOCK   = 11;
     SHM_UNLOCK = 12;

type
  PSHMinfo = ^TSHMinfo;
  TSHMinfo = record
    shmmax : longint;
    shmmin : longint;
    shmmni : longint;
    shmseg : longint;
    shmall : longint;
  end;

Function shmget(key: Tkey; size:longint; flag:longint):longint;
Function shmat (shmid:longint; shmaddr:pchar; shmflg:longint):pchar;
Function shmdt (shmaddr:pchar):boolean;
Function shmctl(shmid:longint; cmd:longint; buf: pshmid_ds): Boolean;

{ ----------------------------------------------------------------------
  Message queue stuff
  ----------------------------------------------------------------------}

const
  MSG_NOERROR = 1 shl 12;
  MSG_EXCEPT  = 2 shl 12;

  MSGMNI = 128;
  MSGMAX = 4056;
  MSGMNB = 16384;


type
  PMSG = ^TMSG;
  TMSG = record
    msg_next  : PMSG;
    msg_type  : Longint;
    msg_spot  : PChar;
    msg_stime : Longint;
    msg_ts    : Integer;
  end;

type

  PMSQid_ds = ^TMSQid_ds;
  TMSQid_ds = record
    msg_perm   : TIPC_perm;
    msg_first  : PMsg;
    msg_last   : PMsg;
    msg_stime  : Longint;
    msg_rtime  : Longint;
    msg_ctime  : Longint;
    wwait      : Pointer;
    rwait      : pointer;
    msg_cbytes : word;
    msg_qnum   : word;
    msg_qbytes : word;
    msg_lspid  : word;
    msg_lrpid  : word;
  end;

  PMSGbuf = ^TMSGbuf;
  TMSGbuf = record
    mtype : longint;
    mtext : array[0..0] of char;
  end;

  PMSGinfo = ^TMSGinfo;
  TMSGinfo = record
    msgpool : Longint;
    msgmap  : Longint;
    msgmax  : Longint;
    msgmnb  : Longint;
    msgmni  : Longint;
    msgssz  : Longint;
    msgtql  : Longint;
    msgseg  : Word;
  end;

Function msgget(key: TKey; msgflg:longint):longint;
Function msgsnd(msqid:longint; msgp: PMSGBuf; msgsz: longint; msgflg:longint): Boolean;
Function msgrcv(msqid:longint; msgp: PMSGBuf; msgsz: longint; msgtyp:longint; msgflg:longint): Boolean;
Function msgctl(msqid:longint; cmd: longint; buf: PMSQid_ds): Boolean;

{ ----------------------------------------------------------------------
  Semaphores stuff
  ----------------------------------------------------------------------}

const
  SEM_UNDO = $1000;
  GETPID = 11;
  GETVAL = 12;
  GETALL = 13;
  GETNCNT = 14;
  GETZCNT = 15;
  SETVAL = 16;
  SETALL = 17;

  SEMMNI = 128;
  SEMMSL = 32;
  SEMMNS = (SEMMNI * SEMMSL);
  SEMOPM = 32;
  SEMVMX = 32767;

type
  PSEMid_ds = ^TSEMid_ds;
  TSEMid_ds = record
    sem_perm : tipc_perm;
    sem_otime : longint;
    sem_ctime : longint;
    sem_base         : pointer;
    sem_pending      : pointer;
    sem_pending_last : pointer;
    undo             : pointer;
    sem_nsems : word;
  end;

  PSEMbuf = ^TSEMbuf;
  TSEMbuf = record
    sem_num : word;
    sem_op  : integer;
    sem_flg : integer;
  end;


  PSEMinfo = ^TSEMinfo;
  TSEMinfo = record
    semmap : longint;
    semmni : longint;
    semmns : longint;
    semmnu : longint;
    semmsl : longint;
    semopm : longint;
    semume : longint;
    semusz : longint;
    semvmx : longint;
    semaem : longint;
  end;

  PSEMun = ^TSEMun;
  TSEMun = record
   case longint of
      0 : ( val : longint );
      1 : ( buf : PSEMid_ds );
      2 : ( arr : PWord );
      3 : ( padbuf : PSeminfo );
      4 : ( padpad : pointer );
   end;

Function semget(key:Tkey; nsems:longint; semflg:longint): longint;
Function semop(semid:longint; sops: pointer; nsops: cardinal): Boolean;
Function semctl(semid:longint; semnum:longint; cmd:longint; var arg: tsemun): longint;

implementation

uses BaseUnix,Syscall;

{ The following definitions come from linux/ipc.h }

Const
  CALL_SEMOP   = 1;
  CALL_SEMGET  = 2;
  CALL_SEMCTL  = 3;
  CALL_MSGSND  = 11;
  CALL_MSGRCV  = 12;
  CALL_MSGGET  = 13;
  CALL_MSGCTL  = 14;
  CALL_SHMAT   = 21;
  CALL_SHMDT   = 22;
  CALL_SHMGET  = 23;
  CALL_SHMCTL  = 24;

{ generic call that handles all IPC calls }

function ipccall(Call,First,Second,Third : Longint; P : Pointer) : longint;

{$ifndef bsd}
//Var SR : SysCallRegs;
{$endif}
begin
 {$IFNDEF bsd}
{  SR.Reg2:=Call;
  SR.reg3:=first;
  SR.reg4:=second;
  SR.Reg5:=third;
  SR.Reg6:=Longint(P); }
  ipccall:=do_syscall(syscall_nr_ipc,call,first,second,third,longint(P));
  {$Endif}
 ipcerror:=fpgetErrno;
end;

Function ftok (Path : String; ID : char) : TKey;

Var Info : TStat;

begin
  If fpstat(path,info)<0 then
    ftok:=-1
  else
    begin
    ftok:= (info.st_ino and $FFFF) or ((info.st_dev and $ff) shl 16) or (byte(ID) shl 24)
    end;
end;

function shmget(key: Tkey; size:longint; flag:longint):longint;

begin
  shmget:=ipccall (CALL_SHMGET,key,size,flag,nil);
end;

function shmat (shmid:longint; shmaddr:pchar; shmflg:longint): pchar;

Var raddr : pchar;
    error : longint;

begin
  error:=ipccall(CALL_SHMAT,shmid,shmflg,longint(@raddr),shmaddr);
  If Error<0 then
    shmat:=pchar(error)
  else
    shmat:=raddr;
end;

function shmdt (shmaddr:pchar): boolean;
begin
  shmdt:=ipccall(CALL_SHMDT,0,0,0,shmaddr)<>-1;
end;

function shmctl(shmid:longint; cmd:longint; buf: pshmid_ds): Boolean;
begin
 shmctl:=ipccall(CALL_SHMCTL,shmid,cmd,0,buf)=0;
end;

Function msgget(key:Tkey; msgflg:longint):longint;

begin
  msgget:=ipccall(CALL_MSGGET,key,msgflg,0,Nil);
end;

Function msgsnd(msqid:longint; msgp: PMSGBuf; msgsz: longint; msgflg:longint):Boolean;

begin
  msgsnd:=ipccall(Call_MSGSND,msqid,msgsz,msgflg,msgp)=0;
end;

Function msgrcv(msqid:longint; msgp: PMSGBuf; msgsz: longint; msgtyp:longint; msgflg:longint):Boolean;

Type
  TIPC_Kludge = Record
    msgp   : pmsgbuf;
    msgtyp : longint;
  end;

Var
   tmp : TIPC_Kludge;

begin
  tmp.msgp   := msgp;
  tmp.msgtyp := msgtyp;
  msgrcv:=ipccall(CALL_MSGRCV,msqid,msgsz,msgflg,@tmp)>=0;
end;

Function msgctl(msqid:longint; cmd: longint; buf: PMSQid_ds): Boolean;

begin
  msgctl:=ipccall(CALL_MSGCTL,msqid,cmd,0,buf)=0;
end;

Function semget(key:Tkey; nsems:longint; semflg:longint): longint;

begin
  semget:=ipccall (CALL_SEMGET,key,nsems,semflg,Nil);
end;

Function semop(semid:longint; sops: pointer; nsops:cardinal): Boolean;

begin
  semop:=ipccall (CALL_SEMOP,semid,Longint(nsops),0,Pointer(sops))=0;
end;

Function semctl(semid:longint; semnum:longint; cmd:longint; var arg: tsemun): longint;
begin
  semctl:=ipccall(CALL_SEMCTL,semid,semnum,cmd,@arg);
end;

end.
{
  $Log$
  Revision 1.5  2003-09-14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.4  2002/09/07 16:01:27  peter
    * old logs removed and tabs fixed

}
