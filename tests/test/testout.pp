uses
   dotest;

type
   tr1 = record
      l1,l2 : longint;
   end;

procedure p1(out b : byte);

  begin
     if b<>0 then
       do_error(1001);
     b:=$aa;
  end;

procedure p2(out w : word);

  begin
     if w<>0 then
       do_error(1002);
     w:=$aaaa;
  end;

procedure p3(out d : dword);

  begin
     if d<>0 then
       do_error(1003);
     d:=$aaaaaaaa;
  end;

procedure p4(out r : tr1);

  begin
     if r.l1<>0 then
       do_error(1004);
     if r.l2<>0 then
       do_error(1005);
     r.l1:=$aaaaaaaa;
     r.l2:=$aaaaaaaa;
  end;

procedure p5(out a : ansistring);

  begin
     if a<>'' then
       do_error(1000);
     a:='Now it''s another ansistring';
  end;

var
   b : byte;
   w : word;
   d : dword;
   r1 : tr1;
   a : ansistring;


begin
   b:=$ff;
   w:=$ffff;
   d:=$ffffffff;
   a:='An ansistring';
   r1.l1:=$ffffffff;
   r1.l2:=$ffffffff;

   p1(b);
   if b<>$aa then
     do_error(1100);

   p2(w);
   if w<>$aaaa then
     do_error(1101);

   p3(d);
   if d<>$aaaaaaaa then
     do_error(1102);

   p4(r1);
   if r1.l1<>$aaaaaaaa then
     do_error(1103);
   if r1.l2<>$aaaaaaaa then
     do_error(1104);

   p5(a);
   if a<>'Now it''s another ansistring' then
     do_error(1105);
end.
