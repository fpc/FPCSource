{ Old file: tbs0237.pp }
{ Can't have sub procedures with names defined in interface OK 0.99.13 (PM) }

unit tb0201;
interface

  procedure sub1(w1,w2:word);

implementation

procedure p1;

  procedure sub1(w:word);
  begin
  end;

begin
end;


procedure sub1(w1,w2:word);
begin
end;

end.
