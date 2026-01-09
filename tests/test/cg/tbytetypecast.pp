{ Test to check code generation problem for powerpc64 CPU } 
{ Related to commit 35604b0926 PM }

var
  HasError : boolean;
  ErrorCount : longint;

procedure error(i : longint);

begin
  writeln('Error at pos ',i);
  HasError:=true;
  inc(ErrorCount);
end;

type
  TRecord = record
    w1,w2,w3,w4 : word;
  end;

var
  w1, w2 : word;
  b1, b2 : byte;
  rec : TRecord;

function getrec : TRecord;
  begin
    getrec:=rec;
  end;

begin
  HasError:=false;
  ErrorCount:=0;
  b1:=$57;
  w1:=$2D57;
  b2:=$E3;
  w2:=$ABE3;
  if (byte(w1)<>b1) then
    error(1);
  if (byte(w2)<>b2) then
    error(2);
  rec.w1:=w1;

  if (byte(rec.w1)<>b1) then
    error(3);
  if (rec.w1=b1) then
    error(4);
  if (byte(getrec.w1)<>b1) then
    error(5);
  if (getrec.w1<>w1) then
    error(6);
  rec.w1:=$1234;
  rec.w2:=w2;
  rec.w3:=w1;
  if (byte(rec.w2)<>b2) then
    error(7);
  if (rec.w2=b2) then
    error(8);
  if (byte(getrec.w2)<>b2) then
    error(9);
  if (getrec.w2<>w2) then
    error(10);
  if (byte(rec.w3)<>b1) then
    error(11);
  if (rec.w3=b1) then
    error(12);
  if (byte(getrec.w3)<>b1) then
    error(13);
  if (getrec.w3<>w1) then
    error(14);
  if ErrorCount=0 then
    writeln('Test completed without error')
  else
    halt(ErrorCount);
end.
