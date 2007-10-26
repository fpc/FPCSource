{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp129 = double;
{$else FPC_COMP_IS_INT64}
  comp129 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test129(a: shortint); overload;
  begin
    writeln('shortint called instead of single');
  end;

procedure test129(a: single); overload;
  begin
    writeln('single called instead of shortint');
    halt(1)
  end;

var
  x129: shortint;

  y129: single;
procedure dotest129;
var
  v: variant;

begin
  try
    v := x129;
    test129(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y129;
    test129(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest129;
end. {$endif not bigfile}
