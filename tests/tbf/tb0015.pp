{ %FAIL }
{ Old file: tbf0097.pp }
{ two errors in bp7 but not in FPC                      OK 0.99.6 (FK) }

{
  This compiles fine with FPC, but not with Bp7 see 2 comments
}

type
  t=object
    s : string;      { No ; needed ? }
    procedure p;
  end;

  t2=object(t)
    procedure p1(p : string);
  end;

procedure t2.p1(p : string);

  begin
  end;

procedure t.p;

var
  s : longint;      { Not allowed with BP7 }
  x : longint;

procedure nested;

  var
     s : longint;

  begin
  end;

begin
end;


begin
end.
