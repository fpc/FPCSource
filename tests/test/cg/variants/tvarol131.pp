{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = currency;
{$endif FPC_COMP_IS_INT64}
procedure test(a: shortint); overload;
  begin
    writeln('shortint called instead of extended');
    writeln('XXX')
  end;

procedure test(a: extended); overload;
  begin
    writeln('extended called instead of shortint');
    halt(1)
  end;

var
  v: variant;
  x: shortint;
  y: extended;

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
