{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp155 = double;
{$else FPC_COMP_IS_INT64}
  comp155 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test155(a: single); overload;
  begin
    writeln('single called instead of widestring');
  end;

procedure test155(a: widestring); overload;
  begin
    writeln('widestring called instead of single');
    halt(1)
  end;

var
  x155: single;

  y155: widestring;
procedure dotest155;
var
  v: variant;

begin
  try
    v := x155;
    test155(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y155;
    test155(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest155;
end. {$endif not bigfile}
