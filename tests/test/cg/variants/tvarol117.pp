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
  comp117 = double;
{$else FPC_COMP_IS_INT64}
  comp117 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test117(a: byte); overload;
  begin
    writeln('byte called instead of shortint');
    writeln('XXX')
  end;

procedure test117(a: shortint); overload;
  begin
    writeln('shortint called instead of byte');
    writeln('YYY')
  end;

var
  x117: byte;

  y117: shortint;
procedure dotest117;
var
  v: variant;

begin
  try
    v := x117;
    test117(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y117;
    test117(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest117;
end. {$endif not bigfile}
