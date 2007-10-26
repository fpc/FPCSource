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
  comp5 = double;
{$else FPC_COMP_IS_INT64}
  comp5 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test5(var a); overload;
  begin
    writeln('formal called instead of cardinal');
    writeln('XXX')
  end;

procedure test5(a: cardinal); overload;
  begin
    writeln('cardinal called instead of formal');
    writeln('YYY')
  end;

var
  x5: longint;

  y5: cardinal;
procedure dotest5;
var
  v: variant;

begin
  try
    v := x5;
    test5(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y5;
    test5(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest5;
end. {$endif not bigfile}
