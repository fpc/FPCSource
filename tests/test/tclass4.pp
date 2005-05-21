{%RESULT=219 }
{%OPT=-CR}
{$mode objfpc}
program test_class;


type
  tobj1 = class
    constructor create;
    procedure mymethod; virtual;
  end;


  tobj2 = class
    constructor create;
    procedure mymethod; virtual;
  end;


  constructor tobj2.create;
   begin
   end;

  procedure tobj2.mymethod;
   begin
   end;


  constructor tobj1.create;
   begin
   end;

  procedure tobj1.mymethod;
   begin
   end;


var
 _cla1 : tobj1;
 _cla2 : tobj2;
Begin
  _cla1:=tobj1.create;
  _cla2:=tobj2.create;
  tobj1(pointer(_cla2)).mymethod;
end.
