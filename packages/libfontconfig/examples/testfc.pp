uses dynlibs, libfontconfig;

Var
  FC : PFcConfig;
  FL : PFcStrList;
  P : PChar;

begin
  Writeln('Load 1: ',loadfontconfiglib(''));
  Writeln('Load 2: ',loadfontconfiglib(''));
  FC:=FcInitLoadConfigAndFonts();
  if FC=Nil then
    begin
    Writeln('Failed to load config');
    Halt(1);
    end;
  FL:=FcConfigGetFontDirs(FC);
  if FL<>Nil then
    begin
    P:=FcStrListNext(FL);
    While P<>Nil do
      begin
      Writeln('Found font dir: ',P);
      P:=FcStrListNext(FL);
      end;
    FcStrListDone(FL);
    end;
  FcConfigDestroy(FC);
  FCFini();
  Writeln('Done');
  Writeln('Unload 1: ',UnLoadFontConfigLib);
  Writeln('Unload 2: ',UnLoadFontConfigLib);
  Writeln('C');
end.
