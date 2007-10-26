{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp141 = double;
{$else FPC_COMP_IS_INT64}
  comp141 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test141(a: shortstring); overload;
  begin
    writeln('shortstring called instead of boolean');
    halt(1)
  end;

procedure test141(a: boolean); overload;
  begin
    writeln('boolean called instead of shortstring');
  end;

var
  x141: shortstring;

  y141: boolean;
procedure dotest141;
var
  v: variant;

begin
  try
    v := x141;
    test141(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y141;
    test141(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest141;
end. {$endif not bigfile}
