{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp93 = double;
{$else FPC_COMP_IS_INT64}
  comp93 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test93(a: cardinal); overload;
  begin
    writeln('cardinal called instead of widechar');
  end;

procedure test93(a: widechar); overload;
  begin
    writeln('widechar called instead of cardinal');
  end;

var
  x93: cardinal;

  y93: widechar;
procedure dotest93;
var
  v: variant;

begin
  try
    v := x93;
    test93(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y93;
    test93(v);
    Writeln('Expected exception, none was raised');
    Halt(1);
  except
    on E : TObject do
      Writeln('Caught exception, as expected : ',E.ClassName);
  end;
end;

{$ifndef bigfile} begin
  dotest93;
end. {$endif not bigfile}
