{ Old file: tbs0014.pp }
{  }

type
   prec = ^trec;

   trec = record
      p : prec;
      l : longint;
   end;

function test(p1,p2 : prec) : boolean;

  begin
     if p1^.l=12 then
     case p1^.l of
        123 : test:=(test(p1^.p,p2^.p) and test(p1^.p,p2^.p)) or
                     (test(p1^.p,p2^.p) and test(p1^.p,p2^.p));
        1234 : test:=(test(p1^.p,p2^.p) and test(p1^.p,p2^.p)) or
                     (test(p1^.p,p2^.p) and test(p1^.p,p2^.p));
     end;
  end;

begin
end.
