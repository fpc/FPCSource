{$mode objfpc}

type
   tobject2 = class
      i : longint;
      procedure y;
      constructor create;
      class procedure x;
      class procedure v;virtual;
   end;

  procedure tobject2.y;

    begin
        Writeln('Procedure y called');
    end;

  class procedure tobject2.v;

    begin
    end;

  class procedure tobject2.x;

    begin
       v;
    end;

  constructor tobject2.create;

    begin
    end;

  type
     tclass2 = class of tobject2;

  var
     a : class of tobject2;
     object2 : tobject2;

begin
   a:=tobject2;
   a.x;
   tobject2.x;
   object2:=tobject2.create;
   object2:=a.create;
end.
