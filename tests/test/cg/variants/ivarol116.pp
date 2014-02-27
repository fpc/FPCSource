{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp116 = double;
{$else FPC_COMP_IS_INT64}
  comp116 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test116(a: smallint); overload;
  begin
    writeln('smallint called instead of widechar');
  end;

procedure test116(a: widechar); overload;
  begin
    writeln('widechar called instead of smallint');
  end;

var
  x116: smallint;

  y116: widechar;
procedure dotest116;
var
  v: variant;

begin
  try
    v := x116;
    test116(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y116;
    test116(v);
    Writeln('Expected exception, did not get one');
    Halt(1);
  except
    on E : TObject do
      begin
      Writeln('Caught exception as expected',E.ClassName);
      end;
  end;
end;

{$ifndef bigfile} begin
  dotest116;
end. {$endif not bigfile}
