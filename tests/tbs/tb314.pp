{$mode objfpc}

type
   tobject2 = class
      constructor create;
      function rname : string;
      procedure wname(const s : string);
      property name : string read rname write wname;
   end;

   tclass2 = class of tobject2;

var
   o2 : tobject2;
   c2 : tclass2;

constructor tobject2.create;

  begin
     inherited create;
  end;

procedure tobject2.wname(const s : string);

  begin
  end;

function tobject2.rname : string;

  begin
  end;

begin
   o2:=tobject2.create;
   o2.name:='1234';
   writeln(o2.name);
   o2.destroy;
   c2:=tobject2;
   o2:=c2.create;
   o2.destroy;
end.
