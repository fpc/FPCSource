{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp86 = double;
{$else FPC_COMP_IS_INT64}
  comp86 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test86(a: cardinal); overload;
  begin
    writeln('cardinal called instead of ansistring');
  end;

procedure test86(a: ansistring); overload;
  begin
    writeln('ansistring called instead of cardinal');
    halt(1)
  end;

var
  x86: cardinal;

  y86: ansistring;
procedure dotest86;
var
  v: variant;

begin
  try
    v := x86;
    test86(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y86;
    test86(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest86;
end. {$endif not bigfile}
