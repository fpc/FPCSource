{ Old file: tbs0252.pp }
{ typecasting not possible within typed const          OK 0.99.13 (PFV) }

type
  wnd=procedure;
  r=record
    w : wnd;
  end;

procedure p;
begin
end;

const
  r1:r=(
   w : wnd(@p);
  );

begin
end.
