{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp144 = double;
{$else FPC_COMP_IS_INT64}
  comp144 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test144(a: ansistring); overload;
  begin
    writeln('ansistring called instead of single');
    halt(1)
  end;

procedure test144(a: single); overload;
  begin
    writeln('single called instead of ansistring');
  end;

var
  x144: ansistring;

  y144: single;
procedure dotest144;
var
  v: variant;

begin
  try
    v := x144;
    test144(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y144;
    test144(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest144;
end. {$endif not bigfile}
