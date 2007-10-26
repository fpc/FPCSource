{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp107 = double;
{$else FPC_COMP_IS_INT64}
  comp107 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test107(a: smallint); overload;
  begin
    writeln('smallint called instead of shortint');
    halt(1)
  end;

procedure test107(a: shortint); overload;
  begin
    writeln('shortint called instead of smallint');
  end;

var
  x107: smallint;

  y107: shortint;
procedure dotest107;
var
  v: variant;

begin
  try
    v := x107;
    test107(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y107;
    test107(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest107;
end. {$endif not bigfile}
