{ inlining must not lose the scaling state of a Currency value: the inlined
  and the non-inlined form of the same body have to agree }
program tw41865a;

{$mode objfpc}
{$inline on}

var
  raw: Int64 = 100000;   { the internal representation of currency 10.0000 }

procedure Fill(out res: currency);
begin
  PInt64(@res)^ := raw;
end;

function Inl: currency; inline;
begin
  Fill(result);
end;

function NoInl: currency;
begin
  Fill(result);
end;

function AddInl(const c: currency): currency; inline;
begin
  result := c + 1;
end;

function AddNoInl(const c: currency): currency;
begin
  result := c + 1;
end;

function LocInl(const x: currency): currency; inline;
var
  t: currency;
begin
  t := x + 1;
  result := t * 2;
end;

function LocNoInl(const x: currency): currency;
var
  t: currency;
begin
  t := x + 1;
  result := t * 2;
end;

var
  c: currency;
begin
  { the function result used directly as an operand }
  if Inl <> NoInl then
    Halt(1);
  if Inl * 16 <> NoInl * 16 then
    Halt(2);
  if 16 * Inl <> 16 * NoInl then
    Halt(3);
  if Inl + 1 <> NoInl + 1 then
    Halt(4);
  if Inl - 1 <> NoInl - 1 then
    Halt(5);
  if (Inl = 10) <> (NoInl = 10) then
    Halt(6);
  if (Inl < 11) <> (NoInl < 11) then
    Halt(7);

  { a currency parameter inside the inlined body }
  c := 10;
  if AddInl(c) <> AddNoInl(c) then
    Halt(8);
  if AddInl(NoInl) <> AddNoInl(NoInl) then
    Halt(9);

  { a currency local inside the inlined body }
  if LocInl(c) <> LocNoInl(c) then
    Halt(10);
  if LocInl(c) - 1 <> LocNoInl(c) - 1 then
    Halt(11);

  WriteLn('ok');
end.
