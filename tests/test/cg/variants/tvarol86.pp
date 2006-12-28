{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: cardinal); overload;
  begin
    writeln('cardinal called instead of ansistring');
    writeln('XXX')
  end;

procedure test(a: ansistring); overload;
  begin
    writeln('ansistring called instead of cardinal');
    halt(1)
  end;

var
  v: variant;
  x: cardinal;
  y: ansistring;

begin
  try
    v := x;
    test(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y;
    test(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end.
