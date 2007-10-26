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
  comp33 = double;
{$else FPC_COMP_IS_INT64}
  comp33 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test33(a: comp33); overload;
  begin
    writeln('comp33 called instead of boolean');
    writeln('XXX')
  end;

procedure test33(a: boolean); overload;
  begin
    writeln('boolean called instead of comp33');
    writeln('YYY')
  end;

var
  x33: comp33;

  y33: boolean;
procedure dotest33;
var
  v: variant;

begin
  try
    v := x33;
    test33(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y33;
    test33(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest33;
end. {$endif not bigfile}
