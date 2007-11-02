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
  comp78 = double;
{$else FPC_COMP_IS_INT64}
  comp78 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test78(a: longint); overload;
  begin
    writeln('longint called instead of boolean');
    writeln('XXX')
  end;

procedure test78(a: boolean); overload;
  begin
    writeln('boolean called instead of longint');
    writeln('YYY')
  end;

var
  x78: longint;

  y78: boolean;
procedure dotest78;
var
  v: variant;

begin
  try
    v := x78;
    test78(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y78;
    test78(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest78;
end. {$endif not bigfile}
