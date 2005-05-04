Program shmtool;

uses ipc,strings,Baseunix;

Const SegSize = 100;

var key : Tkey;
    shmid,cntr : longint;
    segptr : pchar;

Procedure USage;

begin
 Writeln ('Usage : shmtool w(rite) text');
 writeln ('                r(ead)');
 writeln ('                d(elete)');
 writeln ('                m(ode change) mode');
 halt(1);
end;

Procedure Writeshm (ID : Longint; ptr : pchar; S : string);

begin
  strpcopy (ptr,s);
end;

Procedure Readshm(ID : longint; ptr : pchar);

begin
  Writeln ('Read : ',ptr);
end;

Procedure removeshm (ID : Longint);

begin
  shmctl (ID,IPC_RMID,Nil);
  writeln ('Shared memory marked for deletion');
end;

Procedure CHangeMode (ID : longint; mode : String);

Var m : word;
    code : integer;
    data : TSHMid_ds;

begin
  val (mode,m,code);
  if code<>0 then
    usage;
  If shmctl (shmid,IPC_STAT,@data)=-1 then
    begin
     writeln ('Error : shmctl :',fpgeterrno);
     halt(1);
    end;
  writeln ('Old permissions : ',data.shm_perm.mode);
  data.shm_perm.mode:=m;
  If shmctl (shmid,IPC_SET,@data)=-1 then
    begin
    writeln ('Error : shmctl :',fpgeterrno);
    halt(1);
    end;
  writeln ('New permissions : ',data.shm_perm.mode);
end;

const ftokpath = '.'#0;

begin
  if paramcount<1 then usage;
   key := ftok (pchar(@ftokpath[1]),ord('S'));
  shmid := shmget(key,segsize,IPC_CREAT or IPC_EXCL or 438);
  If shmid=-1 then
    begin
    Writeln ('Shared memory exists. Opening as client');
    shmid := shmget(key,segsize,0);
    If shmid = -1 then
      begin
      Writeln ('shmget : Error !',fpgeterrno);
      halt(1);
      end
    end
  else
    Writeln ('Creating new shared memory segment.');
  segptr:=shmat(shmid,nil,0);
  if longint(segptr)=-1 then
    begin
    Writeln ('Shmat : error !',fpgeterrno);
    halt(1);
    end;
  case upcase(paramstr(1)[1]) of
    'W' : writeshm (shmid,segptr,paramstr(2));
    'R' : readshm (shmid,segptr);
    'D' : removeshm(shmid);
    'M' : changemode (shmid,paramstr(2));
  else
    begin
    writeln (paramstr(1));
    usage;
    end;
  end;
end.