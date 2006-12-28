{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(var a); overload;
  begin
    writeln('formal called instead of int64');
    writeln('XXX')
  end;

procedure test(a: int64); overload;
  begin
    writeln('int64 called instead of formal');
    halt(1)
  end;

var
  v: variant;
  x: longint;
  y: int64;

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
