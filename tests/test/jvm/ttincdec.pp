{$mode objfpc}

program ttincdec;

{$q+}
{$r+}


type
  tenum = (ea,eb,ec,ed,ef,eg,eh);

procedure testbool;
var
  b: boolean;
  caught: boolean;
begin
  caught := false;
  b := false;
  inc(b);
  try
    inc(b);
  except
    on e: FpcRunTimeError do
      caught := e.errornr=201;
  end;
  if not caught or
     not b then
    halt(1);

  caught := false;
  dec(b);
  try
    dec(b);
  except
    on e: FpcRunTimeError do
      caught := e.errornr=201;
  end;
  if not caught or
     b then
    halt(2);
end;


procedure testchar;
var
  b: char;
  caught: boolean;
begin
  caught := false;
  b := #254;
  inc(b);
  try
    inc(b);
  except
    on e: FpcRunTimeError do
      caught := e.errornr=201;
  end;
  if not caught or
     (b <> #255) then
    halt(3);

  caught := false;
  b := #1;
  dec(b);
  try
    dec(b);
  except
    on e: FpcRunTimeError do
      caught := e.errornr=201;
  end;
  if not caught or
     (b <> #0) then
    halt(4);
end;



procedure testenum;
var
  b: tenum;
  caught: boolean;
begin
  caught := false;
  b := eg;
  inc(b);
  try
    inc(b);
  except
    on e: FpcRunTimeError do
      caught := e.errornr=201;
  end;
  if not caught or
     (b <> eh) then
    halt(5);

  caught := false;
  b := eb;
  dec(b);
  try
    dec(b);
  except
    on e: FpcRunTimeError do
      caught := e.errornr=201;
  end;
  if not caught or
     (b <> ea) then
    halt(6);
end;


begin
  testbool;
  testchar;
  testenum;
end.
