{ %version=1.1 }

{$ifdef fpc}{$mode objfpc}{$endif}

var
  err : boolean;

type
  tc1=class
    constructor Create;overload;
  end;

  tc2=class(tc1)
    constructor Create(l:longint=0);overload;
  end;

constructor tc1.create;
begin
  writeln('tc1.create()');
end;

constructor tc2.create(l:longint);
begin
  writeln('tc2.create()');
  err:=false;
end;

var
  c : tc2;
begin
  err:=true;
  c:=tc2.create();
  c.free;
  if err then
   begin
     writeln('Error!');
     halt(1);
   end;
end.
