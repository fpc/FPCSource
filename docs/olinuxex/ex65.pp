Program example64;

{ Program to demonstrate the SigRaise function.}

uses oldlinux;

Var
   oa,na : PSigActionRec;

Procedure DoSig(sig : Longint);cdecl;

begin
   writeln('Receiving signal: ',sig);
end;

begin
   new(na);
   new(oa);
   na^.handler.sh:=@DoSig;
   na^.Sa_Mask:=0;
   na^.Sa_Flags:=0;
   na^.Sa_Restorer:=Nil;
   SigAction(SigUsr1,na,oa);
   if LinuxError<>0 then
     begin
     writeln('Error: ',linuxerror,'.');
     halt(1);
     end;
   Writeln('Sending USR1 (',sigusr1,') signal to self.');
   SigRaise(sigusr1);
end.