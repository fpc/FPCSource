Program example64;

{ Program to demonstrate the SigRaise function.}

uses Unix,BaseUnix;

Var
   oa,na : PSigActionRec;

Procedure DoSig(sig : Longint);cdecl;

begin
   writeln('Receiving signal: ',sig);
end;

begin
   new(na);
   new(oa);
   na^.sa_handler:=TSigaction(@DoSig);
   fillchar(na^.Sa_Mask,sizeof(na^.Sa_Mask),#0);
   na^.Sa_Flags:=0;
   {$ifdef Linux}
   // this member is linux only, and afaik even there arcane
   na^.Sa_Restorer:=Nil;
   {$endif}
   if fpSigAction(SigUsr1,na,oa)<>0 then
     begin
     writeln('Error: ',fpgeterrno);
     halt(1);
     end;
   Writeln('Sending USR1 (',sigusr1,') signal to self.');
   SigRaise(sigusr1);
end.