{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp150 = double;
{$else FPC_COMP_IS_INT64}
  comp150 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test150(a: ansistring); overload;
  begin
    writeln('ansistring called instead of widechar');
  end;

procedure test150(a: widechar); overload;
  begin
    writeln('widechar called instead of ansistring');
    halt(1)
  end;

var
  x150: ansistring;

  y150: widechar;
procedure dotest150;
var
  v: variant;

begin
  try
    v := x150;
    test150(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y150;
    test150(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest150;
end. {$endif not bigfile}
