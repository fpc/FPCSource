{ %fail }
{ %opt=-CR }
{ %version=1.1 }

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
  { Detect wrong typecast at compile time }
  tobj1(_cla2).mymethod;
end.
