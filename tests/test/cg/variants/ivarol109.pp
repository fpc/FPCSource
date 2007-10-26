{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp109 = double;
{$else FPC_COMP_IS_INT64}
  comp109 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test109(a: smallint); overload;
  begin
    writeln('smallint called instead of ansistring');
  end;

procedure test109(a: ansistring); overload;
  begin
    writeln('ansistring called instead of smallint');
    halt(1)
  end;

var
  x109: smallint;

  y109: ansistring;
procedure dotest109;
var
  v: variant;

begin
  try
    v := x109;
    test109(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y109;
    test109(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest109;
end. {$endif not bigfile}
