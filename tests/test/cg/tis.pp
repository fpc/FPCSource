{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondis()                                       }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{                 secondinline()                                 }
{                 secondadd()                                    }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS:                                                       }
{****************************************************************}
program tis;

{$mode objfpc}


type
{$ifndef fpc}
  smallint = integer;
{$endif}

 tclass1 = class
 end;


 tclass2 = class(tclass1)
 end;

 tclass3 = class
 end;



var
 myclass1 : tclass1;
 myclass2 : tclass2;
 myclass3 : tclass3;
 class1 : class of tclass1;


procedure fail;
begin
  WriteLn('Failure.');
  halt(1);
end;



  function getclass1 : tclass1;
   begin
     getclass1:=myclass1;
   end;

  function getclass2 : tclass2;
   begin
     getclass2:=myclass2;
   end;

  function getclass3 : tclass3;
   begin
     getclass3:=myclass3;
   end;

{ possible types : left : LOC_REFERENCE, LOC_REGISTER }
{ possible types : right : LOC_REFERENCE, LOC_REGISTER }
var
 failed : boolean;
 myclass4 : class of tclass1;
begin
  failed := false;
  { create class instance }
  myclass1:=tclass1.create;
  myclass2:=tclass2.create;
  myclass3:=tclass3.create;
  {if myclass1 is tclass1 }
  Write('Testing left/right : LOC_REGISTER/LOC_REGISTER...');
  if not(getclass1 is tclass1) then
    failed := true;
  if (getclass1 is tclass2) then
    failed := true;
  if not (getclass2 is tclass2) then
    failed := true;
  if (getclass1 is tclass2) then
    failed := true;

  if failed then
    Fail
  else
    WriteLn('Passed!');

  failed := false;
  Write('Testing left/right : LOC_REFERENCE/LOC_REGISTER...');
  if not(myclass1 is tclass1) then
    failed := true;
  if (myclass1 is tclass2) then
    failed := true;
  if not (myclass2 is tclass2) then
    failed := true;
  if (myclass1 is tclass2) then
    failed := true;

  if failed then
    Fail
  else
    WriteLn('Passed!');


  failed := false;
  Write('Testing left/right : LOC_REFERENCE/LOC_REFERENCE...');
  if (myclass1 is class1) then
    failed := true;
  if failed then
    Fail
  else
    WriteLn('Passed!');
end.
