type
  u_char = byte;
  u_short = word;
  u_long = cardinal;

  wrec = record
    w: word;
  end;

  wrec2 = record
    b1,b2: byte;
  end;

  SunB = record
    s_b1,
    s_b2,
    s_b3,
    s_b4: u_char;
  end;

  SunW = record
    s_w1: wrec;
    s_w2: wrec2;
  end;

  in_addr =  record
    case Integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;

procedure t(i: in_addr);
begin
  if (i.s_un_b.s_b1 <> $de) or
     (i.s_un_b.s_b2 <> $ad) or
     (i.s_un_b.s_b3 <> $be) or
     (i.s_un_b.s_b4 <> $ef) then
    begin
      writeln('error1');
      halt(1);
    end;
end;

procedure t2(i: in_addr);
begin
  if (i.s_un_w.s_w1.w <> $cafe) or
     (i.s_un_w.s_w2.b1 <> $ba) or
     (i.s_un_w.s_w2.b2 <> $be) then
    begin
      writeln('error2');
      halt(2);
    end;
end;


var
  i: in_addr;
begin
  i.s_un_b.s_b1 := $de;
  i.s_un_b.s_b2 := $ad;
  i.s_un_b.s_b3 := $be;
  i.s_un_b.s_b4 := $ef;
  t(i);
  i.s_un_w.s_w1.w := $cafe;
  i.s_un_w.s_w2.b1 := $ba;
  i.s_un_w.s_w2.b2 := $be;
  t2(i);
end.
