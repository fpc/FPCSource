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
  comp16 = double;
{$else FPC_COMP_IS_INT64}
  comp16 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test16(var a); overload;
  begin
    writeln('formal called instead of boolean');
    writeln('XXX')
  end;

procedure test16(a: boolean); overload;
  begin
    writeln('boolean called instead of formal');
    writeln('YYY')
  end;

var
  x16: longint;

  y16: boolean;
procedure dotest16;
var
  v: variant;

begin
  try
    v := x16;
    test16(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y16;
    test16(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest16;
end. {$endif not bigfile}
