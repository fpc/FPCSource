Program example58;

{ Program to demonstrate the Signal function.}

{
do a kill -USR1 pid from another terminal to see what happens.
replace pid with the real pid of this program.
You can get this pid by running 'ps'.
}

uses BaseUnix;

Procedure DoSig(sig : cint);cdecl;

begin
   writeln('Receiving signal: ',sig);
end;

begin
   if fpSignal(SigUsr1,SignalHandler(@DoSig))=signalhandler(SIG_ERR) then
     begin
     writeln('Error: ',fpGetErrno,'.');
     halt(1);
     end;
   Writeln ('Send USR1 signal or press <ENTER> to exit');
   readln;
end.
