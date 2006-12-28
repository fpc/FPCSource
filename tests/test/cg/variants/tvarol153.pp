{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: single); overload;
  begin
    writeln('single called instead of char');
    writeln('XXX')
  end;

procedure test(a: char); overload;
  begin
    writeln('char called instead of single');
    halt(1)
  end;

var
  v: variant;
  x: single;
  y: char;

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
