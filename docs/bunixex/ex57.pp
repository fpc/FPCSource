Program example57;

{ Program to demonstrate the SigAction function.}

{
do a kill -USR1 pid from another terminal to see what happens.
replace pid with the real pid of this program.
You can get this pid by running 'ps'.
}

uses BaseUnix;

Var
   oa,na : PSigActionRec;

Procedure DoSig(sig : cint);cdecl;

begin
   writeln('Receiving signal: ',sig);
end;

begin
   new(na);
   new(oa);
   na^.sa_Handler:=SigActionHandler(@DoSig);
   fillchar(na^.Sa_Mask,sizeof(na^.sa_mask),#0);
   na^.Sa_Flags:=0;
   {$ifdef Linux}               // Linux specific
     na^.Sa_Restorer:=Nil;
   {$endif}
   if fpSigAction(SigUsr1,na,oa)<>0 then
     begin
     writeln('Error: ',fpgeterrno,'.');
     halt(1);
     end;
   Writeln ('Send USR1 signal or press <ENTER> to exit');
   readln;
end.
