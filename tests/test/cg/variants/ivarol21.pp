{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp21 = double;
{$else FPC_COMP_IS_INT64}
  comp21 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test21(a: comp21); overload;
  begin
    writeln('comp21 called instead of longint');
    halt(1)
  end;

procedure test21(a: longint); overload;
  begin
    writeln('longint called instead of comp21');
  end;

var
  x21: comp21;

  y21: longint;
procedure dotest21;
var
  v: variant;

begin
  try
    v := x21;
    test21(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y21;
    test21(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest21;
end. {$endif not bigfile}
