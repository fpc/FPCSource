{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: longint); overload;
  begin
    writeln('longint called instead of shortint');
    halt(1)
  end;

procedure test(a: shortint); overload;
  begin
    writeln('shortint called instead of longint');
    writeln('YYY')
  end;

var
  v: variant;
  x: longint;
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
