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
  rc1: array of LongInt = (1, 2, 3);
{$J+}
const
  wc1: array of LongInt = (1, 2, 3);
{$pop}


begin
{$ifdef target_supports_rodata}
  try
    rc1[1] := 42;
    writeln('Error: Trying to write read-only data did not generate an exception');
    Halt(1);
  except
    writeln('Trying to write read-only data generated exception');
  end;
{$else}
  try
    rc1[1] := 42;
    writeln('Trying to write read-only data did not generate an exception, as expected');
  except
    writeln('Trying to write read-only data generated exception, while system is supposed not to support this');
    halt(2);
  end;
{$endif}
  try
    wc1[1] := 42;
  except
    writeln('Error: Trying to write normal data generated exception');
    halt(3);
  end;
end.
