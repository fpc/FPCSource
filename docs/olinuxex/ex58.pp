Program example58;

{ Program to demonstrate the Signal function.}

{
do a kill -USR1 pid from another terminal to see what happens.
replace pid with the real pid of this program.
You can get this pid by running 'ps'.
}

uses oldlinux;

Procedure DoSig(sig : Longint);cdecl;

begin
   writeln('Receiving signal: ',sig);
end;

begin
   SigNal(SigUsr1,@DoSig);
   if LinuxError<>0 then
     begin
     writeln('Error: ',linuxerror,'.');
     halt(1);
     end;
   Writeln ('Send USR1 signal or press <ENTER> to exit');
   readln;
end.
