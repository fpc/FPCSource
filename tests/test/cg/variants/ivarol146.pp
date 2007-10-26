{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp146 = currency;
{$else FPC_COMP_IS_INT64}
  comp146 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test146(a: ansistring); overload;
  begin
    writeln('ansistring called instead of extended');
    halt(1)
  end;

procedure test146(a: extended); overload;
  begin
    writeln('extended called instead of ansistring');
  end;

var
  x146: ansistring;

  y146: extended;
procedure dotest146;
var
  v: variant;

begin
  try
    v := x146;
    test146(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y146;
    test146(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest146;
end. {$endif not bigfile}
