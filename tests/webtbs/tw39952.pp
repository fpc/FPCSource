
const
  REC_SIZE=3;

type
  tinst = packed record
    x : word;
    b : byte;
    rec : array[1..REC_SIZE] of int64;
  end;

  tunaligned = packed record
    x, y : byte;
    a, b : int64;
    c,d : qword;
  end;

const
  x_val = 45634;
  b_val = 56;
  rec1_val = $123456;
  rec2_val = $DCBA87654321;
  rec3_val = $ADEFCB4567;
  t : tinst = (
    x : x_val;
    b : b_val;
    rec : (rec1_val,rec2_val,rec3_val)
  );

const
  has_errors : boolean = false;

var
  i : longint;
  tu : tunaligned;
  ia,ib : int64;
  uc,ud : qword;

{$R-}
{$Q-}

begin
  writeln('t.x=',t.x);
  writeln('t.b=',t.b);
  for i:=1 to REC_SIZE do
    writeln('t.rec[',i,']=',t.rec[i]);
  if (t.x <> x_val) then
    begin
      writeln('t.x field is wrong');
      has_errors:=true;
    end;

  if (t.b <> b_val) then
    begin
      writeln('t.b field is wrong');
      has_errors:=true;
    end;

  if (t.rec[1] <> rec1_val) then
    begin
      writeln('t.rec[1] field is wrong');
      has_errors:=true;
    end;

  if (t.rec[2] <> rec2_val) then
    begin
      writeln('t.rec2 field is wrong');
      has_errors:=true;
    end;

  if (t.rec[3] <> rec3_val) then
    begin
      writeln('t.rec[3] field is wrong');
      has_errors:=true;
    end;

  with tu do
    begin
      a:=$0123456789ABCDEF;
      ia:=a;
      b:=swapendian(a);
      ib:=swapendian(ia);
      writeln('a=',hexstr(a,2*sizeof(a)));
      writeln('b=',hexstr(b,2*sizeof(b)));
      writeln('ib=',hexstr(ib,2*sizeof(ib)));
      if (b<>ib) then
        begin
          writeln('b<>ib!!');
          has_errors:=true;
        end;
      c:=$F123456789ABCDEF;
      uc:=c;
      d:=swapendian(c);
      ud:=swapendian(uc);
      writeln('c=',hexstr(c,2*sizeof(c)));
      writeln('d=',hexstr(d,2*sizeof(d)));
      writeln('ud=',hexstr(ud,2*sizeof(ud)));
      if (d<>ud) then
        begin
          writeln('d<>ud!!');
          has_errors:=true;
        end;
    end;
  if (tu.a<>ia) then
    begin
      writeln('tu.a is different from ia');
      has_errors:=true;
    end;

  if (tu.c<>uc) then
    begin
      writeln('tu.c is different from uc');
      has_errors:=true;
    end;

  if has_errors then
    begin
      writeln('Wrong code is generated');
      halt(1);
    end;
end.
