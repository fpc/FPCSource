Program example57;

{ Program to demonstrate the SigAction function.}

{ 
do a kill -USR1 pid from another terminal to see what happens.
replace pid with the real pid of this program. 
You can get this pid by running 'ps'.
}

uses Linux;

Var
   oa,na : PSigActionRec;
   
Procedure DoSig(sig : Longint);cdecl;

begin
   writeln('Receiving signal: ',sig);
end; 

begin
   new(na);
   new(oa);
   na^.Sa_Handler:=@DoSig;
   na^.Sa_Mask:=0;
   na^.Sa_Flags:=0;
   na^.Sa_Restorer:=Nil;
   SigAction(SigUsr1,na,oa);
   if LinuxError<>0 then
     begin
     writeln('Error: ',linuxerror,'.');
     halt(1);
     end;
   readln;
end.
