program tformal;
{$mode objfpc}

uses
  sysutils;

type
  TFontStyle = (
    fsItalic,
    fsBold,
    fsUnderlined,
    fsStrikeOut
  );
  TFontStyles = set of TFontStyle;

var aFS: TFontStyles;

procedure Any(const Anything);
begin
  aFS:=aFS+TFontStyles(Anything);
  Writeln(IntToHex(PLongInt(@Anything)^, 8));
end;

procedure DoIt;
begin
  Any([fsItalic, fsBold]); //unit1.pas(31,25) Error: Variable identifier expected
  if aFS<>[fsItalic, fsBold] then
    halt(1);
  Any(Cardinal([fsItalic, fsBold])); //ok
end;

begin
  aFS:=[];
  writeln(Cardinal(aFS));
  DoIt;
  writeln(Cardinal(aFS));
  writeln('ok');
end.
