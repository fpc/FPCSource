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
  comp8 = double;
{$else FPC_COMP_IS_INT64}
  comp8 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test8(var a); overload;
  begin
    writeln('formal called instead of byte');
    writeln('XXX')
  end;

procedure test8(a: byte); overload;
  begin
    writeln('byte called instead of formal');
    writeln('YYY')
  end;

var
  x8: longint;

  y8: byte;
procedure dotest8;
var
  v: variant;

begin
  try
    v := x8;
    test8(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y8;
    test8(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest8;
end. {$endif not bigfile}
