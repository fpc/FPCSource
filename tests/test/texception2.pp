{ %skiptarget=go32v2 }

{$mode objfpc}
uses
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
   for i:=1 to 20 do
     try
       d;
     except
       on exception do
        ;
     end;
   writeln('ok');
end.
