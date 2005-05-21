{%RESULT=219 }
{ %OPT= -CR }
program test_object;


type
  pobj1 = ^tobj1;
  tobj1 = object
    constructor init;
    procedure mymethod; virtual;
  end;



  pobj2 = ^tobj2;
  tobj2 = object
    constructor init;
    procedure mymethod; virtual;
  end;


  constructor tobj2.init;
   begin
   end;

  procedure tobj2.mymethod;
   begin
   end;


  constructor tobj1.init;
   begin
   end;

  procedure tobj1.mymethod;
   begin
   end;


var
 _obj1 : pobj1;
 _obj2 : pobj2;
Begin
  _obj1:=new(pobj1,init);
  _obj2:=new(pobj2,init);
  pobj1(_obj2)^.mymethod;
end.
