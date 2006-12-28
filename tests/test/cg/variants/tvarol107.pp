{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: smallint); overload;
  begin
    writeln('smallint called instead of shortint');
    halt(1)
  end;

procedure test(a: shortint); overload;
  begin
    writeln('shortint called instead of smallint');
    writeln('YYY')
  end;

var
  v: variant;
  x: smallint;
  y: shortint;

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
      halt(1);
  end;
end.
