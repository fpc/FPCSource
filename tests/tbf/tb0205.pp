{ %fail }

procedure test(a: array of longint); forward;

type
  tl = type longint;

procedure test(a: array of tl);
begin
end;

begin
end.

