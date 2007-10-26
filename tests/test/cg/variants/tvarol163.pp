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
  comp163 = double;
{$else FPC_COMP_IS_INT64}
  comp163 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test163(a: extended); overload;
  begin
    writeln('extended called instead of boolean');
    writeln('XXX')
  end;

procedure test163(a: boolean); overload;
  begin
    writeln('boolean called instead of extended');
    writeln('YYY')
  end;

var
  x163: extended;

  y163: boolean;
procedure dotest163;
var
  v: variant;

begin
  try
    v := x163;
    test163(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y163;
    test163(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest163;
end. {$endif not bigfile}
