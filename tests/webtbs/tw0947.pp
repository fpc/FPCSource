{$mode objfpc}

var
  last,lastt2 : integer;

type
  T1 = class
    procedure SomeMethod(Param: Integer); virtual;
  end;

  T2 = class(T1)
    procedure SomeMethod(Param: Integer); override;
    procedure InheritedMethod(Param: Integer);
    destructor Destroy; override;
  end;

procedure T1.SomeMethod(Param: Integer);
begin
  last:=Param;
  writeln('T1 ', Param);
end;

procedure T2.InheritedMethod(Param: Integer);
begin
  inherited SomeMethod(Param);
end;

procedure T2.SomeMethod(Param: Integer);
begin
  lastt2:=param;
  writeln('T2 ', Param);
end;

destructor T2.Destroy;
begin
  SomeMethod(3);
  inherited SomeMethod(2);
  inherited Destroy;
end;

var
  A: T2;
begin
  Last:=0;
  lastt2:=0;
  A:=T2.Create;
  A.SomeMethod(1); { Ok }
  if lastt2<>1 then
    Halt(1);
  A.InheritedMethod(4); { Ok }
  if last<>4 then
    Halt(1);
  A.Free; { error }
  if last<>2 then
    Halt(1);
  if lastt2<>3 then
    Halt(1);
  Writeln('Bug with calling inherited in destructors solved');
end.
