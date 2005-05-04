program msgtool;

Uses ipc,baseunix;

Type
  PMyMsgBuf = ^TMyMsgBuf;
  TMyMsgBuf = record
    mtype : Longint;
    mtext : string[255];
  end;

Procedure DoError (Const Msg : string);

begin
  Writeln (msg,' returned an error : ',fpgeterrno);
  halt(1);
end;

Procedure SendMessage (Id : Longint;
                       Var Buf : TMyMsgBuf;
                       MType : Longint;
                       Const MText : String);

begin
  Writeln ('Sending message.');
  Buf.mtype:=mtype;
  Buf.Mtext:=mtext;
  If  msgsnd(Id,PMsgBuf(@Buf),256,0)=-1 then
    DoError('msgsnd');
end;

Procedure ReadMessage (ID : Longint;
                       Var Buf : TMyMsgBuf;
                       MType : longint);

begin
  Writeln ('Reading message.');
  Buf.MType:=MType;
  If msgrcv(ID,PMSGBuf(@Buf),256,mtype,0)<>-1 then
    Writeln ('Type : ',buf.mtype,' Text : ',buf.mtext)
  else
    DoError ('msgrcv');
end;

Procedure RemoveQueue ( ID : Longint);

begin
  If msgctl (id,IPC_RMID,Nil)<>-1 then
    Writeln ('Removed Queue with id ',Id);
end;

Procedure ChangeQueueMode (ID,mode : longint);

Var QueueDS : TMSQid_ds;

begin
  If  msgctl (Id,IPC_STAT,@QueueDS)=-1 then
    DoError ('msgctl : stat');
  Writeln ('Old permissions : ',QueueDS.msg_perm.mode);
  QueueDS.msg_perm.mode:=Mode;
  if msgctl (ID,IPC_SET,@QueueDS)=0 then
    Writeln ('New permissions : ',QueueDS.msg_perm.mode)
  else
   DoError ('msgctl : IPC_SET');
end;

procedure usage;

begin
  Writeln ('Usage : msgtool s(end)    <type> <text> (max 255 characters)');
  Writeln ('                r(eceive) <type>');
  Writeln ('                d(elete)');
  Writeln ('                m(ode) <decimal mode>');
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

Var
  Key : TKey;
  ID  : longint;
  Buf : TMyMsgBuf;

const ipckey = '.'#0;

begin
  If Paramcount<1 then Usage;
  key :=Ftok(@ipckey[1],ord('M'));
  ID:=msgget(key,IPC_CREAT or 438);
  If ID<0 then DoError ('MsgGet');
  Case upCase(Paramstr(1)[1]) of
   'S' : If ParamCount<>3 then
           Usage
         else
           SendMessage (id,Buf,StrToInt(Paramstr(2)),paramstr(3));
   'R' : If ParamCount<>2 then
           Usage
         else
           ReadMessage (id,buf,strtoint(Paramstr(2)));
   'D' : If ParamCount<>1 then
           Usage
         else
           RemoveQueue (ID);
   'M' : If ParamCount<>2 then
           Usage
         else
           ChangeQueueMode (id,strtoint(paramstr(2)));
   else
     Usage
   end;
end.