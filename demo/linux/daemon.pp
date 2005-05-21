{---------------------------------------------------------------------------
                                 CncWare
                           (c) Copyright 2000
 ---------------------------------------------------------------------------
  Filename..: daemon.pp
  Programmer: Ken J. Wright
  Date......: 03/21/2000

  Purpose - Program to demonstrate construction of a Linux daemon.

  Usage:
    1) Compile this program.
    2) Run it. You will be immediately returned to a command prompt.
    3) Issue the command: ps ax|grep daemon. This will show you the process
       id of the program "daemon" that you just ran.
    4) Issue the command: tail -f daemon.log. This let's you watch the log file
       being filled with the message in the code below. Press Ctrl/c to break
       out of the tail command.
    5) Issue the command: kill -HUP pid. pid is the process number you saw with
       the ps command above. You will see that a new log file has been created.
    6) Issue the command: kill -TERM pid. This will stop the daemon. Issuing the
       ps command above, you will see that the daemon is no longer running.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |    Date    | Prog | Decription
-------+------------+------+--------------------------------------------------
  1.00 | 03/21/2000 | kjw  | Initial release.
  1.01 | 03/21/2000 | kjw  | Forgot to close input, output, & stderr.
------------------------------------------------------------------------------
}
Program Daemon;
uses SysUtils,BaseUnix;
Var
   { vars for daemonizing }
   bHup,
   bTerm : boolean;
   fLog : text;
   logname : string;
   aOld,
   aTerm,
   aHup : pSigActionRec;
   ps1  : psigset;
   sSet : cardinal;
   pid  : pid_t;
   secs : longint;
   zerosigs : sigset_t;
   hr,mn,sc,sc100 : word;

{ handle SIGHUP & SIGTERM }
procedure DoSig(sig : longint);cdecl;
begin
   case sig of
      SIGHUP : bHup := true;
      SIGTERM : bTerm := true;
   end;
end;

{ open the log file }
Procedure NewLog;
Begin
   Assign(fLog,logname);
   Rewrite(fLog);
   Writeln(flog,'Log created at ',formatdatetime('hh:nn:ss',now));
   Close(fLog);
End;

Begin
   logname := 'daemon.log';
   secs := 10;
   fpsigemptyset(zerosigs);

   { set global daemon booleans }
   bHup := true; { to open log file }
   bTerm := false;

   { block all signals except -HUP & -TERM }
   sSet := $ffffbffe;
   ps1 := @sSet;
   fpsigprocmask(sig_block,ps1,nil);

   { setup the signal handlers }
   new(aOld);
   new(aHup);
   new(aTerm);
   aTerm^.sa_handler{.sh} := SigactionHandler(@DoSig);

   aTerm^.sa_mask := zerosigs;
   aTerm^.sa_flags := 0;
   {$ifndef BSD}                {Linux'ism}
    aTerm^.sa_restorer := nil;
   {$endif}
   aHup^.sa_handler := SigactionHandler(@DoSig);
   aHup^.sa_mask := zerosigs;
   aHup^.sa_flags := 0;
   {$ifndef BSD}                {Linux'ism}
    aHup^.sa_restorer := nil;
   {$endif}
   fpSigAction(SIGTERM,aTerm,aOld);
   fpSigAction(SIGHUP,aHup,aOld);

   { daemonize }
   pid := fpFork;
   Case pid of
      0 : Begin { we are in the child }
         Close(input);  { close standard in }
         Close(output); { close standard out }
         Assign(output,'/dev/null');
         ReWrite(output);
         Close(stderr); { close standard error }
         Assign(stderr,'/dev/null');
         ReWrite(stderr);
      End;
      -1 : secs := 0;     { forking error, so run as non-daemon }
      Else Halt;          { successful fork, so parent dies }
   End;

   { begin processing loop }
   Repeat
      If bHup Then Begin
         {$I-}
         Close(fLog);
         {$I+}
         IOResult;
         NewLog;
         bHup := false;
      End;
      {----------------------}
      { Do your daemon stuff }
      Append(flog);
      Writeln(flog,'daemon code activated at ',formatdatetime('hh:nn:ss',now));
      Close(fLog);
      { the following output goes to the bit bucket }
      Writeln('daemon code activated at ',hr:0,':',mn:0,':',sc:0);
      {----------------------}
      If bTerm Then
         BREAK
      Else
         { wait a while }
         fpSelect(0,nil,nil,nil,secs*1000);
   Until bTerm;
End.
