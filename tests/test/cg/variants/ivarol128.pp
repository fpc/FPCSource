{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp128 = double;
{$else FPC_COMP_IS_INT64}
  comp128 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test128(a: shortint); overload;
  begin
    writeln('shortint called instead of ansistring');
  end;

procedure test128(a: ansistring); overload;
  begin
    writeln('ansistring called instead of shortint');
    halt(1)
  end;

var
  x128: shortint;

  y128: ansistring;
procedure dotest128;
var
  v: variant;

begin
  try
    v := x128;
    test128(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y128;
    test128(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest128;
end. {$endif not bigfile}
