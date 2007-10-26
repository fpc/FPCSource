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
  comp1 = double;
{$else FPC_COMP_IS_INT64}
  comp1 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test1(var a); overload;
  begin
    writeln('formal called instead of comp1');
    writeln('XXX')
  end;

procedure test1(a: comp1); overload;
  begin
    writeln('comp1 called instead of formal');
    writeln('YYY')
  end;

var
  x1: longint;

  y1: comp1;
procedure dotest1;
var
  v: variant;

begin
  try
    v := x1;
    test1(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y1;
    test1(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest1;
end. {$endif not bigfile}
