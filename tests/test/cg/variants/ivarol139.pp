{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp139 = currency;
{$else FPC_COMP_IS_INT64}
  comp139 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test139(a: shortstring); overload;
  begin
    writeln('shortstring called instead of extended');
    halt(1)
  end;

procedure test139(a: extended); overload;
  begin
    writeln('extended called instead of shortstring');
  end;

var
  x139: shortstring;

  y139: extended;
procedure dotest139;
var
  v: variant;

begin
  try
    v := x139;
    test139(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y139;
    test139(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest139;
end. {$endif not bigfile}
