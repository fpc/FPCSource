{ %OPT=-Or -So}
{$minenumsize 1}

type
   tenum = (e1,e2,e3);

procedure p1(e : tenum);forward;

procedure p1;

  begin
     e:=tenum(byte(e)*byte(e));
     case e of
        e1 : ;
     else
       begin
          writeln('error');
          halt(1);
       end;
     end;
  end;

begin
   p1(e1);
end.
