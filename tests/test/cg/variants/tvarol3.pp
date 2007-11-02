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
  comp3 = double;
{$else FPC_COMP_IS_INT64}
  comp3 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test3(var a); overload;
  begin
    writeln('formal called instead of currency');
    writeln('XXX')
  end;

procedure test3(a: currency); overload;
  begin
    writeln('currency called instead of formal');
    writeln('YYY')
  end;

var
  x3: longint;

  y3: currency;
procedure dotest3;
var
  v: variant;

begin
  try
    v := x3;
    test3(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y3;
    test3(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest3;
end. {$endif not bigfile}
