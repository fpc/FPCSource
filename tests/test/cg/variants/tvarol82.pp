{ %fail }
{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp82 = double;
{$else FPC_COMP_IS_INT64}
  comp82 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test82(a: cardinal); overload;
  begin
    writeln('cardinal called instead of smallint');
    writeln('XXX')
  end;

procedure test82(a: smallint); overload;
  begin
    writeln('smallint called instead of cardinal');
    writeln('YYY')
  end;

var
  x82: cardinal;

  y82: smallint;
procedure dotest82;
var
  v: variant;

begin
  try
    v := x82;
    test82(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y82;
    test82(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest82;
end. {$endif not bigfile}
