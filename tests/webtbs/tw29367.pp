program Project1;
{$Mode objfpc}

type
  TFoo = class
    constructor create; virtual;
  end;

  TBar = type TFoo;

  TBaz = class(TBar)
    constructor create; override;
  end;

constructor TFoo.create;
begin
  writeln('TFoo.create');
end;

constructor TBaz.create;
begin
  inherited;
  writeln('TBaz.create');
end;

var
  test1tbar: boolean;

procedure test1(o: TFoo; error: longint); overload;
begin
  writeln('test1 tfoo');
  o.free;
  if test1tbar then
    halt(error);
end;

procedure test1(o: TBar; error: longint); overload;
begin
  writeln('test1 tbar');
  o.free;
  if not test1tbar then
    halt(error);
end;

var
  b: tbar;
begin
  if not tbar.inheritsfrom(tfoo) then
    begin
      writeln('error 1');
      halt(1);
    end;
  if not tbaz.inheritsfrom(tbar) then
    begin
      writeln('error 2');
      halt(2);
   end;
  if tbar.classname<>'TFoo' then
    begin
      writeln('error 3');
      halt(3);
    end;
  if tfoo.classname<>'TFoo' then
    begin
      writeln('error 4');
      halt(4);
    end;
  TBaz.create.free;
  test1tbar:=false;
  test1(tfoo.create,5);
  test1(tbar.create,6);
  b:=tbar.create;
  test1tbar:=true;
  test1(b,7);
end.
