{ Old file: tbs0239.pp }
{ No warning for uninitialized class in IS statements  OK 0.99.11 (PM) }

{$mode delphi}
  uses
    sysutils;

   type
     ttest=class
     end;
     ttest2 = class(ttest)
     end;
     ttestclass=class of ttest;
   var
     i,j:ttest;
     tt:tclass;
   begin
     tt:=ttest;
     i:=ttest.create;
     j:=ttest2.create;
     Writeln('tt is a class of ttest initialized by "tt:=ttest"');
     Writeln('i is a ttest class initialized by "i:=ttest.create"');
     Writeln('j is a ttest class initialized by "j:=ttest2.create"');
     writeln('i is tobject ',i is tobject);
     if not(i is tobject) then
       Halt(1);
     writeln('i is tt ',i is tt);
     if not(i is tt) then
       Halt(1);
     writeln('i is ttest ',i is ttest);
     if not(i is ttest) then
       Halt(1);
     writeln('i is ttest2 ',i is ttest2);
     if (i is ttest2) then
       Halt(1);
     writeln('j is tobject ',j is tobject);
     if not(j is tobject) then
       Halt(1);
     writeln('j is tt ',j is tt);
     if not(j is tt) then
       Halt(1);
     writeln('j is ttest ',j is ttest);
     if not(j is ttest) then
       Halt(1);
     writeln('j is ttest2 ',j is ttest2);
     if not(j is ttest2) then
       Halt(1);
   end.
