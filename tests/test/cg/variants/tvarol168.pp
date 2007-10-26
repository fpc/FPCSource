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
  comp168 = double;
{$else FPC_COMP_IS_INT64}
  comp168 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test168(a: char); overload;
  begin
    writeln('char called instead of widechar');
    writeln('XXX')
  end;

procedure test168(a: widechar); overload;
  begin
    writeln('widechar called instead of char');
    writeln('YYY')
  end;

var
  x168: char;

  y168: widechar;
procedure dotest168;
var
  v: variant;

begin
  try
    v := x168;
    test168(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y168;
    test168(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest168;
end. {$endif not bigfile}
