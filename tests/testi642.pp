{$i rtl\inc\int64.inc}

procedure dumpqword(q : qword);

  begin
     write('$',hexstr(tqwordrec(q).high,8),' ',hexstr(tqwordrec(q).low,8));
  end;

procedure assignqword(h,l : dword;var q : qword);

  begin
     tqwordrec(q).high:=h;
     tqwordrec(q).low:=l;
  end;

procedure do_error(l : longint);

  begin
     writeln('Error near line',l);
     halt(1);
  end;

{ $define error:=do_error({$line});}

procedure testcmpqword;

  var
     q1,q2,q3 : qword;

  begin
     assignqword(0,5,q1);
     assignqword(6,0,q2);
     assignqword(6,5,q1);
  end;

var
   q : qword;

begin
   assignqword($12345678,$9ABCDEF0,q);
   dumpqword(q);
end.
