{ %version=1.1 }
{$mode objfpc}
 type
     t = class(tobject)
      constructor Init;
     end;

 constructor t.Init;
 begin
  fail; { constructor will return NULL in ESI now, which is OK }
 end;

 type
     c = class(tobject)
      procedure p;
     end;


 procedure c.p;
  var i:t;
 begin
  i:=t.Init;
  if i<>nil then
    begin
       writeln('Problem with saving a non assigned self');
       halt(1);
    end;
  { returned is NULL in ESI, and AfterConstructor is attempted to call by
    referencing an invalid VMT via ESI}
 end;

 var i:c;

begin
 i:=c.create; i.p;
end.
