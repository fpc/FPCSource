{ %version=1.1 }

{$ifdef fpc}{$mode delphi}{$endif}

var
  err : boolean;

type
  tc1=class(tinterfacedobject)
    constructor Create;overload;
    constructor Create(s:string);overload;
  end;

  tc2=class(tc1)
    constructor Create(l1,l2:longint);overload;
  end;

constructor tc1.create;
begin
  err:=true;
end;

constructor tc1.create(s:string);
begin
  err:=true;
end;

constructor tc2.create(l1,l2:longint);
begin
  { The next line should do nothing }
  inherited;
end;

var
  c : tc2;
begin
  err:=false;
  c:=tc2.create(1,1);
  c.free;
  if err then
   begin
     writeln('Error!');
     halt(1);
   end;
end.
