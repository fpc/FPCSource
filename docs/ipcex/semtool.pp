Program semtool;

{ Program to demonstrat the use of semaphores }

Uses ipc,baseunix;

Const MaxSemValue = 5;

Procedure DoError (Const Msg : String);

begin
  Writeln ('Error : ',msg,' Code : ',fpgeterrno);
  Halt(1);
end;

Function getsemval (ID,Member : longint) : longint;

Var S : TSEMun;

begin
  GetSemVal:=SemCtl(id,member,SEM_GETVAL,S);
end;

Procedure DispVal (ID,member : longint);

begin
  writeln ('Value for member ',member,' is ',GetSemVal(ID,Member));
end;

Function GetMemberCount (ID : Longint) : longint;

Var opts : TSEMun;
    semds : TSEMid_ds;

begin
  opts.buf:=@semds;
  If semctl(Id,0,IPC_STAT,opts)<>-1 then
    GetMemberCount:=semds.sem_nsems
  else
    GetMemberCount:=-1;
end;

Function OpenSem (Key : TKey) : Longint;

begin
  OpenSem:=semget(Key,0,438);
  If OpenSem=-1 then
    DoError ('OpenSem');
end;

Function CreateSem (Key : TKey; Members : Longint) : Longint;

Var Count : Longint;
    Semopts : TSemun;

begin
// the semmsl constant seems kernel specific
{  If members>semmsl then
    DoError ('Sorry, maximum number of semaphores in set exceeded');
}
  Writeln ('Trying to create a new semaphore set with ',members,' members.');
  CreateSem:=semget(key,members,IPC_CREAT or IPC_Excl or 438);
  If CreateSem=-1 then
    DoError ('Semaphore set already exists.');
  Semopts.val:=MaxSemValue; { Initial value of semaphores }
  For Count:=0 to Members-1 do
    semctl(CreateSem,count,SEM_SETVAL,semopts);
end;

Procedure lockSem (ID,Member: Longint);

Var lock : TSEMbuf;

begin
  With lock do
    begin
    sem_num:=0;
    sem_op:=-1;
    sem_flg:=IPC_NOWAIT;
    end;
   if (member<0) or (member>GetMemberCount(ID)-1) then
     DoError ('semaphore member out of range');
   if getsemval(ID,member)=0 then
     DoError ('Semaphore resources exhausted (no lock)');
   lock.sem_num:=member;
   Writeln ('Attempting to lock member ',member, ' of semaphore ',ID);
   if semop(Id,@lock,1)=-1 then
     DoError ('Lock failed')
   else
     Writeln ('Semaphore resources decremented by one');
   dispval(ID,Member);
end;

Procedure UnlockSem (ID,Member: Longint);

Var Unlock : TSEMbuf;

begin
  With Unlock do
    begin
    sem_num:=0;
    sem_op:=1;
    sem_flg:=IPC_NOWAIT;
    end;
   if (member<0) or (member>GetMemberCount(ID)-1) then
     DoError ('semaphore member out of range');
   if getsemval(ID,member)=MaxSemValue then
     DoError ('Semaphore not locked');
   Unlock.sem_num:=member;
   Writeln ('Attempting to unlock member ',member, ' of semaphore ',ID);
   if semop(Id,@unlock,1)=-1 then
     DoError ('Unlock failed')
   else
     Writeln ('Semaphore resources incremented by one');
   dispval(ID,Member);
end;

Procedure RemoveSem (ID : longint);

var S : TSemun;

begin
  If semctl(Id,0,IPC_RMID,s)<>-1 then
    Writeln ('Semaphore removed')
  else
    DoError ('Couldn''t remove semaphore');
end;


Procedure ChangeMode (ID,Mode : longint);

Var rc : longint;
    opts : TSEMun;
    semds : TSEMid_ds;

begin
  opts.buf:=@semds;
  If not semctl (Id,0,IPC_STAT,opts)<>-1 then
    DoError ('Couldn''t stat semaphore');
  Writeln ('Old permissions were : ',semds.sem_perm.mode);
  semds.sem_perm.mode:=mode;
  If semctl(id,0,IPC_SET,opts)<>-1 then
    Writeln ('Set permissions to ',mode)
  else
    DoError ('Couldn''t set permissions');
end;

Procedure PrintSem (ID : longint);

Var I,cnt : longint;

begin
  cnt:=getmembercount(ID);
  Writeln ('Semaphore ',ID,' has ',cnt,' Members');
  For I:=0 to cnt-1 Do
    DispVal(id,i);
end;

Procedure USage;

begin
  Writeln ('Usage : semtool c(reate) <count>');
  Writeln ('                l(ock) <member>');
  Writeln ('                u(nlock) <member>');
  Writeln ('                d(elete)');
  Writeln ('                m(ode) <mode>');
  halt(1);
end;

Function StrToInt (S : String): longint;

Var M : longint;
    C : Integer;

begin
  val (S,M,C);
  If C<>0 Then DoError ('StrToInt : '+S);
  StrToInt:=M;
end;

Var Key : TKey;
    ID : Longint;


const ipckey='.'#0;  

begin
  If ParamCount<1 then USage;
  key:=ftok(@ipckey[1],ORD('s'));
  Case UpCase(Paramstr(1)[1]) of
   'C' : begin
         if paramcount<>2 then usage;
         CreateSem (key,strtoint(paramstr(2)));
         end;
   'L' : begin
         if paramcount<>2 then usage;
         ID:=OpenSem (key);
         LockSem (ID,strtoint(paramstr(2)));
         end;
   'U' : begin
         if paramcount<>2 then usage;
         ID:=OpenSem (key);
         UnLockSem (ID,strtoint(paramstr(2)));
         end;
   'M' : begin
         if paramcount<>2 then usage;
         ID:=OpenSem (key);
         ChangeMode (ID,strtoint(paramstr(2)));
         end;
   'D' : Begin
         ID:=OpenSem(Key);
         RemoveSem(Id);
         end;
   'P' : begin
         ID:=OpenSem(Key);
         PrintSem(Id);
         end;
  else
    Usage
  end;
end.