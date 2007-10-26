{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp156 = double;
{$else FPC_COMP_IS_INT64}
  comp156 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test156(a: single); overload;
  begin
    writeln('single called instead of widechar');
  end;

procedure test156(a: widechar); overload;
  begin
    writeln('widechar called instead of single');
    halt(1)
  end;

var
  x156: single;

  y156: widechar;
procedure dotest156;
var
  v: variant;

begin
  try
    v := x156;
    test156(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y156;
    test156(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest156;
end. {$endif not bigfile}
