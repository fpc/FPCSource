{
  This compiles fine with FPC, but not with Bp7 see 2 comments
}

type
  t=object
    s : string      { No ; needed ? }
    procedure p;
  end;

procedure t.p;
var
  s : longint;      { Not allowed with BP7 }
begin
end;


begin
end.
