{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp148 = double;
{$else FPC_COMP_IS_INT64}
  comp148 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test148(a: ansistring); overload;
  begin
    writeln('ansistring called instead of boolean');
    halt(1)
  end;

procedure test148(a: boolean); overload;
  begin
    writeln('boolean called instead of ansistring');
  end;

var
  x148: ansistring;

  y148: boolean;
procedure dotest148;
var
  v: variant;

begin
  try
    v := x148;
    test148(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y148;
    test148(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest148;
end. {$endif not bigfile}
