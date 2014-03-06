{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp66 = double;
{$else FPC_COMP_IS_INT64}
  comp66 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test66(a: currency); overload;
  begin
    writeln('currency called instead of widechar');
  end;

procedure test66(a: widechar); overload;
  begin
    writeln('widechar called instead of currency');
  end;

var
  x66: currency;

  y66: widechar;
procedure dotest66;
var
  v: variant;

begin
  try
    v := x66;
    test66(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y66;
    test66(v);
    Writeln('Expected exception, none was raised');
    Halt(1);
  except
    on E : TObject do
      Writeln('Caught exception, as expected: ',E.ClassName);
  end;
end;

{$ifndef bigfile} begin
  dotest66;
end. {$endif not bigfile}
