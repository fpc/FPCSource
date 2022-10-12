
const
  REC_SIZE=3;

type
  tinst = packed record
    x : word;
    b : byte;
    rec : array[1..REC_SIZE] of int64;
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


var
  i : longint;
const
  has_errors : boolean = false;

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

  if has_errors then
    begin
      writeln('Wrong code is generated');
      halt(1);
    end;
end.
