{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp50 = double;
{$else FPC_COMP_IS_INT64}
  comp50 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test50(a: int64); overload;
  begin
    writeln('int64 called instead of widestring');
    halt(1)
  end;

procedure test50(a: widestring); overload;
  begin
    writeln('widestring called instead of int64');
  end;

var
  x50: int64;

  y50: widestring;
procedure dotest50;
var
  v: variant;

begin
  try
    v := x50;
    test50(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y50;
    test50(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest50;
end. {$endif not bigfile}
