{ Old file: tbs0107.pp }
{ shows page fault problem (run in TRUE DOS mode)       OK ??.?? }

{ PAGE FAULT PROBLEM ... TEST UNDER DOS ONLY! Not windows... }
{ -Cr -g flags                                               }

Program Test1;

type
 myObject = object
   constructor init;
   procedure v;virtual;
 end;

 constructor myobject.init;
 Begin
 end;

 procedure myobject.v;
 Begin
  WriteLn('Hello....');
 end;

var
 my: myobject;
Begin
 my.init;
 my.v;
end.
