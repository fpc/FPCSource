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
  comp14 = currency;
{$else FPC_COMP_IS_INT64}
  comp14 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test14(var a); overload;
  begin
    writeln('formal called instead of extended');
    writeln('XXX')
  end;

procedure test14(a: extended); overload;
  begin
    writeln('extended called instead of formal');
    writeln('YYY')
  end;

var
  x14: longint;

  y14: extended;
procedure dotest14;
var
  v: variant;

begin
  try
    v := x14;
    test14(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y14;
    test14(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest14;
end. {$endif not bigfile}
