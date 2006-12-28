{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: ansistring); overload;
  begin
    writeln('ansistring called instead of char');
    writeln('XXX')
  end;

procedure test(a: char); overload;
  begin
    writeln('char called instead of ansistring');
    halt(1)
  end;

var
  v: variant;
  x: ansistring;
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
      halt(1);
  end;
end.
