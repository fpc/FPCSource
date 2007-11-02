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
  comp12 = double;
{$else FPC_COMP_IS_INT64}
  comp12 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test12(var a); overload;
  begin
    writeln('formal called instead of single');
    writeln('XXX')
  end;

procedure test12(a: single); overload;
  begin
    writeln('single called instead of formal');
    writeln('YYY')
  end;

var
  x12: longint;

  y12: single;
procedure dotest12;
var
  v: variant;

begin
  try
    v := x12;
    test12(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y12;
    test12(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest12;
end. {$endif not bigfile}
