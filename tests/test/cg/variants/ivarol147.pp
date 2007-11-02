{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp147 = double;
{$else FPC_COMP_IS_INT64}
  comp147 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test147(a: ansistring); overload;
  begin
    writeln('ansistring called instead of char');
  end;

procedure test147(a: char); overload;
  begin
    writeln('char called instead of ansistring');
    halt(1)
  end;

var
  x147: ansistring;

  y147: char;
procedure dotest147;
var
  v: variant;

begin
  try
    v := x147;
    test147(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y147;
    test147(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest147;
end. {$endif not bigfile}
