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
  comp124 = double;
{$else FPC_COMP_IS_INT64}
  comp124 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test124(a: byte); overload;
  begin
    writeln('byte called instead of boolean');
    writeln('XXX')
  end;

procedure test124(a: boolean); overload;
  begin
    writeln('boolean called instead of byte');
    writeln('YYY')
  end;

var
  x124: byte;

  y124: boolean;
procedure dotest124;
var
  v: variant;

begin
  try
    v := x124;
    test124(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y124;
    test124(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest124;
end. {$endif not bigfile}
