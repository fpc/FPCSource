{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp166 = double;
{$else FPC_COMP_IS_INT64}
  comp166 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test166(a: char); overload;
  begin
    writeln('char called instead of boolean');
    halt(1)
  end;

procedure test166(a: boolean); overload;
  begin
    writeln('boolean called instead of char');
  end;

var
  x166: char;

  y166: boolean;
procedure dotest166;
var
  v: variant;

begin
  try
    v := x166;
    test166(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y166;
    test166(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest166;
end. {$endif not bigfile}
