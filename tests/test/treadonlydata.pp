program treadonlydata;

{$define target_supports_rodata}
{$if defined(msdos) or defined(hasamiga) or defined(atari) or defined(palmos)}
{$undef target_supports_rodata}
{$endif}

{$mode  objfpc}

{ needed for "except" to work }
uses
  SysUtils;

{$push}
{$J-}
const
  rc: LongInt = 5;
  rc1: array [0..2] of LongInt = (1, 2, 3);
{$J+}
const
  wc: LongInt = 78;
  wc1: array [0..2] of LongInt = (1, 2, 3);
  has_errors : boolean = false;
{$pop}
var
  p : plongint;

begin
{$ifdef target_supports_rodata}
  try
    p := @rc;
    p^ := 42;
    writeln('Error: Trying to write a read-only longint constant did not generate an exception');
    has_errors:=true;
  except
    writeln('OK: Trying to write read-only data generated exception');
  end;
  try
    p := @rc1[1];
    p^ := 42;
    writeln('Error: Trying to write a read-only longint array data element did not generate an exception');
    has_errors:=true;
  except
    writeln('OK: Trying to write read-only data generated exception');
  end;
{$else}
  try
    p := @rc;
    p^ := 42;
    writeln('Trying to write a read-only longint constant did not generate an exception, as expected');
  except
    writeln('Trying to write a read-only longint constant generated exception, while system is supposed not to support this');
    has_errors:=true;
  end;
  try
    p := @rc1[1];
    p^ := 42;
    writeln('Trying to write a read-only longint array data element did not generate an exception, as expected');
  except
    writeln('Trying to write a read-only longint array data element generated exception, while system is supposed not to support this');
    has_errors:=true;
  end;
{$endif}
  try
    wc := 42;
  except
    writeln('Error: Trying to write normal longint initialized "const" generated exception');
    has_errors:=true;
  end;
  try
    wc1[1] := 42;
  except
    writeln('Error: Trying to write normal array data element generated exception');
    has_errors:=true;
  end;
  if has_errors then
    halt(1);
end.
