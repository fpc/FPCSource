{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp44 = double;
{$else FPC_COMP_IS_INT64}
  comp44 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test44(a: int64); overload;
  begin
    writeln('int64 called instead of ansistring');
    halt(1)
  end;

procedure test44(a: ansistring); overload;
  begin
    writeln('ansistring called instead of int64');
  end;

var
  x44: int64;

  y44: ansistring;
procedure dotest44;
var
  v: variant;

begin
  try
    v := x44;
    test44(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y44;
    test44(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest44;
end. {$endif not bigfile}
