{$mode objfpc}
uses sysutils;

{$ifndef FPC}
  {$define ENDIAN_LITTLE}
{$endif}
type
  int64rec = record
{$ifdef ENDIAN_LITTLE}
    lo,hi:
{$else }
    hi,lo :
{$endif}
       cardinal;
   end;

var
  haserror,
  error: boolean;
  b: byte;
  s: shortint;
  i: smallint;
  w: word;
  l: longint;
  c: cardinal;
  t: int64;
  q: qword;

{$r+}

begin
  haserror := false;
  b := 255;
  t := b;
  q := b;
  b := 0;
  t := b;
  q := b;

  s := 127;
  t := s;
  q := s;
  s := -128;
  t := s;
  try
    error := true;
    q := s;
  except
    error := false;
  end;
  haserror := haserror or error;

  w := 0;
  t := w;
  q := w;
  w := 65535;
  t := w;
  q := w;

  i := 32767;
  t := i;
  q := i;
  i := -32768;
  t := i;
  try
    error := true;
    q := i;
  except
    error := false;
  end;
  haserror := haserror or error;

  c := 0;
  t := c;
  q := c;
  c := $ffffffff;
  t := c;
  q := c;

  l := -maxlongint-1;
  t := l;
  try
    error := true;
    q := l;
  except
    error := false;
  end;
  haserror := haserror or error;
  l := maxlongint;
  t := l;
  q := l;

  q := 0;
  t := q;
  with int64rec(q) do
  begin
    lo := 0;
    hi := $ffffffff;
  end;
  try
    error := true;
    t := q;
  except
    error := false;
  end;
  haserror := haserror or error;

  with int64rec(t) do
  begin
    lo := 0;
    hi := $80000000;
  end;
  try
    error := true;
    q := t;
  except
    error := false;
  end;
  haserror := haserror or error;
  with int64rec(t) do
  begin
    lo := $ffffffff;
    hi := $7fffffff;
  end;
  q := t;
  if haserror then
    begin
      writeln('64bit range checking still buggy!');
      halt(1);
    end;
end.
