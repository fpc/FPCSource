{ Source provided for Free Pascal Bug Report 3179 }
{ Submitted by "Martin Schreiber" on  2004-06-21 }
{ e-mail:  }
{$mode objfpc}{$H+}

uses
  uw3179a,uw3179b;

var
 class1: tclass1;
 class2: tclass2;

begin
 class1:= tclass1.create;
 class1.proc1;             //ok
 class1.free;
 class2:= tclass2.create;
 class2.proc2;             //abstracterror in tclass1.proc1
 class2.free;
end.
