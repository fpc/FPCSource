program tprocvaranon;

type
  tprocvaranonrec = record
    p: function: longint;
  end;

function test: longint;
begin
  test:=123;
end;

var
  r: tprocvaranonrec;
begin
 r.p:=@test;
 if r.p()<>123 then
   halt(1);
end.
    
