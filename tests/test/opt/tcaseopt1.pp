Procedure TestCaseBool;
Var
  L1, L2: LongWord;
  b : boolean;
begin
  b:=false;

  l1:=$deadbeef;
  case b of
    true:
      l1:=0;
    else
      l1:=1234;
  end;
  if l1<>1234 then
    halt(1);

  l1:=$deadbeef;
  case b of
    false:
      l1:=1234;
    else
      l1:=0;
  end;
  if l1<>1234 then
    halt(1);

  l1:=$deadbeef;
  case b of
    true:
      l1:=0;
    false:
      l1:=1234;
  end;
  if l1<>1234 then
    halt(1);

  l1:=$deadbeef;
  case b of
    false..true:
      l1:=1234;
    else
      l1:=0;
  end;
  if l1<>1234 then
    halt(1);

  l1:=$deadbeef;
  case b of
    false..true:
      l1:=1234;
  end;
  if l1<>1234 then
    halt(1);


  { set b to true }
  b:=true;

  l1:=$deadbeef;
  case b of
    true:
      l1:=0;
    else
      l1:=1234;
  end;
  if l1<>0 then
    halt(1);

  l1:=$deadbeef;
  case b of
    false:
      l1:=1234;
    else
      l1:=0;
  end;
  if l1<>0 then
    halt(1);

  l1:=$deadbeef;
  case b of
    true:
      l1:=0;
    false:
      l1:=1234;
  end;
  if l1<>0 then
    halt(1);

  l1:=$deadbeef;
  case b of
    false..true:
      l1:=1234;
    else
      l1:=0;
  end;
  if l1<>1234 then
    halt(1);

  l1:=$deadbeef;
  case b of
    false..true:
      l1:=1234;
  end;
  if l1<>1234 then
    halt(1);
end;

begin
  TestCaseBool;
  writeln('ok');
end.
