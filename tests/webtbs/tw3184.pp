{ Source provided for Free Pascal Bug Report 3184 }
{ Submitted by "Martin Schreiber" on  2004-06-25 }
{ e-mail:  }

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
  uw3184a,uw3184b;

var
 class1: tclass1;
 class2: tclass2;

begin
 class1:= tclass1.create;
 if class1.proc1<>10 then
   halt(1);      //exp: 'tclass1.proc' act: 'tclass1.proc' -> ok
 class1.free;
 class2:= tclass2.create;
 if class2.proc2<>10 then
   halt(1);      //exp: 'tclass1.proc' act: 'tclass0.proc' -> error
 class2.free;
end.
