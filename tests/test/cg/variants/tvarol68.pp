{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: longint); overload;
  begin
    writeln('longint called instead of word');
    halt(1)
  end;

procedure test(a: word); overload;
  begin
    writeln('word called instead of longint');
    writeln('YYY')
  end;

var
  v: variant;
  x: longint;
  y: word;

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
