{$mode delphi}

type
  itest = interface(iunknown)
    procedure Foo(); overload;
    procedure Bar(); overload;
    procedure Foo(x: integer); overload;
    procedure Bar(x: integer); overload;
  end;

  ttest = class(tinterfacedobject, itest)
    procedure Foo(); overload;
    procedure Bar(); overload;
    procedure Foo(x: integer); overload;
    procedure Bar(x: integer); overload;
  end;

var
  i : integer;
  err : boolean;

procedure ttest.Foo(); overload; begin writeln('#'); i:=1; end;
procedure ttest.Foo(x: integer); overload; begin writeln('##'); i:=2; end;
procedure ttest.Bar(); overload; begin writeln('###'); i:=3; end;
procedure ttest.Bar(x: integer); overload; begin writeln('####'); i:=4; end;

var
  t: itest;
  a: integer;
begin
  t := ttest.create();
  t.Foo();
  if i<>1 then
    err:=true;
  t.Foo(a);
  if i<>2 then
    err:=true;
  t.Bar();
  if i<>3 then
    err:=true;
  t.Bar(a);
  if i<>4 then
    err:=true;
  t := nil;
  if err then
    halt(1);
end.