{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: ansistring); overload;
  begin
    writeln('ansistring called instead of single');
    halt(1)
  end;

procedure test(a: single); overload;
  begin
    writeln('single called instead of ansistring');
    writeln('YYY')
  end;

var
  v: variant;
  x: ansistring;
  y: single;

begin
  try
    v := x;
    test(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y;
    test(v);
  except
    on E : TObject do
      halt(1);
  end;
end.
