procedure proc1(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20 : longint);
  begin
  end;

procedure proc2;
  var
    l : dword;
  begin
    l:=$deadbeef;
    proc1(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20);
    if l<>$deadbeef then
      begin
        writeln('error');
        halt(1);
      end;
  end;

begin
  proc2;
end.
