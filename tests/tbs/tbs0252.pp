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

