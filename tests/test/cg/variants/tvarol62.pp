{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = currency;
{$endif FPC_COMP_IS_INT64}
{$ifdef FPC_HAS_TYPE_EXTENDED}
procedure test(a: currency); overload;
  begin
    writeln('currency called instead of extended');
    writeln('XXX')
  end;

procedure test(a: extended); overload;
  begin
    writeln('extended called instead of currency');
    writeln('YYY')
  end;

var
  v: variant;
  x: currency;
  y: extended;

begin
  try
    v := x;
    test(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y;
    test(v);
  except
    on E : TObject do
      halt(1);
  end;
{$else}
begin
{$endif}
end.
