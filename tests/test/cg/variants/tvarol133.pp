{ %fail }
{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp133 = double;
{$else FPC_COMP_IS_INT64}
  comp133 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test133(a: shortint); overload;
  begin
    writeln('shortint called instead of boolean');
    writeln('XXX')
  end;

procedure test133(a: boolean); overload;
  begin
    writeln('boolean called instead of shortint');
    writeln('YYY')
  end;

var
  x133: shortint;

  y133: boolean;
procedure dotest133;
var
  v: variant;

begin
  try
    v := x133;
    test133(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y133;
    test133(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest133;
end. {$endif not bigfile}
