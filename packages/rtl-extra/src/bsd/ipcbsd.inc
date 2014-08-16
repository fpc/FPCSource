{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by Marco van de Voort
    member of the Free Pascal development team

    *BSD syscalls for ipc unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function ftok(path:Pchar; id:cint):key_t;

var st:stat;

begin
  if fpstat(path,st)<0 then
   ftok:=key_t(-1)
 else
   ftok:=key_t( byte(id) shl 24 + ((st.st_dev and 255) shl 16) + (st.st_ino and $ffff));
end;

function shmget(key:key_t;size:size_t;flag:cint):cint;
begin
{$ifdef NetBSD}
  shmget:=do_syscall(syscall_nr_shmget, key, size, flag);
{$else NetBSD}
  shmget:=do_syscall(syscall_nr_shmsys,3, key, size, flag);
{$endif NetBSD}
end;

Function shmat (shmid:cint; shmaddr:pointer; shmflg:cint):pointer;
begin
{$ifdef NetBSD}
  shmat:=pointer(do_syscall(syscall_nr_shmat, shmid, TSysParam(shmaddr), shmflg));
{$else NetBSD}
  shmat:=pointer(do_syscall(syscall_nr_shmsys,0, shmid, TSysParam(shmaddr), shmflg));
{$endif NetBSD}
end;

Function shmdt (shmaddr:pointer):cint;

begin
{$ifdef NetBSD}
  shmdt:=do_syscall(syscall_nr_shmdt, TSysParam(shmaddr));
{$else NetBSD}
  shmdt:=do_syscall(syscall_nr_shmsys,2, TSysParam(shmaddr));
{$endif NetBSD}
end;

Function shmctl(shmid:cint; cmd:cint; buf: pshmid_ds): cint;

begin
{$ifdef NetBSD}
  shmctl:= do_syscall(syscall_nr___shmctl13, shmid, cmd, TSysParam(buf));
{$else NetBSD}
  shmctl:= do_syscall(syscall_nr_shmsys,4, shmid, cmd, TSysParam(buf));
{$endif NetBSD}
end;

Function semget(key:Tkey; nsems:cint; semflg:cint): cint;
begin
{$ifdef NetBSD}
  semget:=do_syscall(syscall_nr_semget, key, nsems, semflg);
{$else NetBSD}
  semget:=do_syscall(syscall_nr_semsys,1, key, nsems, semflg);
{$endif NetBSD}
end;

Function semop(semid:cint; sops: psembuf; nsops: cuint): cint;
begin
{$ifdef NetBSD}
  semop:=do_syscall(syscall_nr_semop, semid, TSysParam(sops), nsops, 0);
{$else NetBSD}
  semop:=do_syscall(syscall_nr_semsys,2, semid, TSysParam(sops), nsops, 0);
{$endif NetBSD}
end;

Function semctl(semid:cint; semnum:cint; cmd:cint; var arg: tsemun): cint;
begin
{$ifdef NetBSD}
  semctl:=cint(do_syscall(syscall_nr_semconfig, semid, semnum, cmd,TSysParam(@arg)));
{$else NetBSD}
  semctl:=cint(do_syscall(syscall_nr_semsys, 0, semid, semnum, cmd,TSysParam(@arg)));
{$endif NetBSD}
end;

Function msgget(key: TKey; msgflg:cint):cint;
begin
{$ifdef NetBSD}
  msgget:=do_syscall(syscall_nr_msgget, key, msgflg);
{$else NetBSD}
  msgget:=do_syscall(syscall_nr_msgsys,1, key, msgflg);
{$endif NetBSD}
end;

Function msgsnd(msqid:cint; msgp: PMSGBuf; msgsz: size_t; msgflg:cint): cint;
begin
{$ifdef NetBSD}
  msgsnd:=do_syscall(syscall_nr_msgsnd, msqid, TSysParam(msgp), TSysParam(msgsz), msgflg);
{$else NetBSD}
  msgsnd:=do_syscall(syscall_nr_msgsys,2, msqid, TSysParam(msgp), TSysParam(msgsz), msgflg);
{$endif NetBSD}
end;

Function msgrcv(msqid:cint; msgp: PMSGBuf; msgsz: size_t; msgtyp:clong; msgflg:cint):cint;
begin
{$ifdef NetBSD}
  msgrcv:=(do_syscall(syscall_nr_msgrcv, msqid, TSysParam(msgp), msgsz, msgtyp, msgflg));
{$else NetBSD}
  msgrcv:=(do_syscall(syscall_nr_msgsys,3, msqid, TSysParam(msgp), msgsz, msgtyp, msgflg));
{$endif NetBSD}
end;

Function msgctl(msqid:cint; cmd: cint; buf: PMSQid_ds): cint;
begin
{$ifdef NetBSD}
 msgctl:= (do_syscall(syscall_nr___msgctl13, msqid, cmd, tsysparam(buf)));
{$else NetBSD}
 msgctl:= (do_syscall(syscall_nr_msgsys,0, msqid, cmd, tsysparam(buf)));
{$endif NetBSD}
end;

