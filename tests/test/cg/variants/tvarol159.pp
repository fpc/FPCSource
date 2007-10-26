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
  comp159 = double;
{$else FPC_COMP_IS_INT64}
  comp159 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test159(a: double); overload;
  begin
    writeln('double called instead of boolean');
    writeln('XXX')
  end;

procedure test159(a: boolean); overload;
  begin
    writeln('boolean called instead of double');
    writeln('YYY')
  end;

var
  x159: double;

  y159: boolean;
procedure dotest159;
var
  v: variant;

begin
  try
    v := x159;
    test159(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y159;
    test159(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest159;
end. {$endif not bigfile}
