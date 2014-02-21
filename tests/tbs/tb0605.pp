program tb0605;

{$mode objfpc}

type
  aint = longint;

function getint64: int64;
begin
  Result := 64;
end;

function getlongint: longint;
begin
  Result := 32;
end;

function getword: word;
begin
  result := 16;
end;

function getbyte: byte;
begin
  result := 8;
end;

function getaint: longint;
begin
  result:=4;
  case sizeof(aint) of
    8: result:=getint64;
    4: result:=getlongint;
    2: result:=smallint(getword);
    1: result:=shortint(getbyte);
  end;
end;

begin
  if getaint <> 32 then
    Halt(1);
end.
