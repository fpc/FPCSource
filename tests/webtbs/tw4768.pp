{ %OPT=-O1 }

{ Source provided for Free Pascal Bug Report 4768 }
{ Submitted by "Martin Schreiber" on  2006-02-04 }
{ e-mail:  }
program project1;
{$ifdef FPC}{$mode objfpc}{$h+}{$INTERFACES CORBA}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 sysutils;
 
function later(ref,act: cardinal): boolean;
begin
 result:= not(integer(act-ref) < 0);
end;

function later1(ref,act: cardinal): boolean;
begin
 result:= integer(act-ref) >= 0;
end;

function later2(ref,act: cardinal): boolean;
var
 ca1: cardinal;
begin
 ca1:= act-ref;
 writeln(integer(ca1));
 result:= integer(ca1) >= 0;
end;

function later3(ref,act: cardinal): boolean;
begin
 result:= not(cardinal(ref+act) < 0);
end;

function later4(ref,act: cardinal): boolean;
begin
 result:= cardinal(act+ref) >= 0;
end;

var
 ca1,ca2: cardinal;
 
begin
 ca1:= $7fffffff;
 ca2:= $80000001;
 if not(later(ca1,ca2)) then
   halt(1);
 if not(later1(ca1,ca2)) then
   halt(1);
 if not(later2(ca1,ca2)) then
   halt(1);
 if not(later3(ca1,ca2)) then
   halt(1);
 if not(later4(ca1,ca2)) then
   halt(1);
end.
