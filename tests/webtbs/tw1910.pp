{$mode delphi}

type
  tprocedure = procedure;

  procedure testprocedure;
   begin
   end;


  procedure proc(const buf);
  var
    p : tprocedure;
  begin
    p:=tprocedure(@buf);
  end;


begin
  proc(testprocedure);
end.
