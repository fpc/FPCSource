{$mode objfpc}
uses
   dotest,
   sysutils;

procedure d;

  var
     d1 : double;

  begin
     d1:=0;
     d1:=1/d1;
  end;

var
   i : longint;

begin
   d;
   for i:=1 to 20 do
     try
        d;
     except
       on exception do
         ;
     end;
end.