{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp53 = double;
{$else FPC_COMP_IS_INT64}
  comp53 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test53(a: currency); overload;
  begin
    writeln('currency called instead of cardinal');
    halt(1)
  end;

procedure test53(a: cardinal); overload;
  begin
    writeln('cardinal called instead of currency');
  end;

var
  x53: currency;

  y53: cardinal;
procedure dotest53;
var
  v: variant;

begin
  try
    v := x53;
    test53(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y53;
    test53(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest53;
end. {$endif not bigfile}
