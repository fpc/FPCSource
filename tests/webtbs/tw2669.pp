{ %version=1.1 }
{ Source provided for Free Pascal Bug Report 2669 }
{ Submitted by "marco" on  2003-09-06 }
{ e-mail: marco+web@freepascal.org }

{$mode Delphi}
Type
   TPop3NextProc = procedure of object;
   t1= class
            procedure server; virtual;
            procedure run; virtual;
            procedure connect; virtual;
   end;

   t2=class
         f1 : t1;
         procedure exec(p:TPop3NextProc);
         procedure callexec;
         constructor create;
         end;


procedure t1.server;

begin
 writeln('server');
end;

procedure t1.run;

begin
 writeln('run');
end;


procedure t1.connect;

begin
 writeln('connect');
end;


constructor t2.create;

begin
  inherited create;
  f1:=t1.create;
end;

procedure t2.exec(p:TPop3NextProc);

begin
 writeln('in exec');
 p;
end;

procedure t2.callexec;

begin
 writeln('callexec');
 exec(f1.server);
 exec(f1.run);
 exec(f1.connect);
end;


var c1 : t2;

begin
  writeln('start');
  c1:=t2.create;
  writeln('after create');
  c1.callexec;
  writeln('end');
end.
