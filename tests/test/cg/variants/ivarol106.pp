{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp106 = double;
{$else FPC_COMP_IS_INT64}
  comp106 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test106(a: smallint); overload;
  begin
    writeln('smallint called instead of byte');
    halt(1)
  end;

procedure test106(a: byte); overload;
  begin
    writeln('byte called instead of smallint');
  end;

var
  x106: smallint;

  y106: byte;
procedure dotest106;
var
  v: variant;

begin
  try
    v := x106;
    test106(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y106;
    test106(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest106;
end. {$endif not bigfile}
