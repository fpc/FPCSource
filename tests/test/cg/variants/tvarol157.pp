{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp157 = currency;
{$else FPC_COMP_IS_INT64}
  comp157 = comp;
{$endif FPC_COMP_IS_INT64}
{$ifdef FPC_HAS_TYPE_EXTENDED}
procedure test157(a: double); overload;
  begin
    writeln('double called instead of extended');
  end;

procedure test157(a: extended); overload;
  begin
    writeln('extended called instead of double');
    halt(1)
  end;

var
  x157: double;

  y157: extended;
procedure dotest157;
var
  v: variant;

begin
  try
    v := x157;
    test157(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y157;
    test157(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest157;
end. {$endif not bigfile}
{$else FPC_HAS_TYPE_EXTENDED}
begin
end.
{$endif FPC_HAS_TYPE_EXTENDED}
