uses dynlibs, libfontconfig;

Var
  FC : PFcConfig;
  FL : PFcStrList;
  P : PAnsiChar;
  FN,FN2 : PAnsiChar;
begin
  Writeln('Load 1: ',loadfontconfiglib(''));
  Writeln('Load 2: ',loadfontconfiglib(''));
  FC:=FcInitLoadConfigAndFonts();
  if FC=Nil then
    begin
    Writeln('Failed to load config');
    Halt(1);
    end;
  if assigned(FcGetVersion) then
    writeln('FontConfig version: ',FcGetVersion);

  if assigned(FcConfigFilename) then
    begin
      FN:=FcConfigFilename(Nil);
      Writeln('Default config file is: ',FN,' using deprecated FcConfigFilename function');
    end;
  if assigned(FcConfigGetFilename) then
    begin
      FN2:=FcConfigGetFilename(FC,Nil);
      Writeln('Default config file is: ',FN2,' using FcConfigGetFilename function');
    end;
  FL:=FcConfigGetConfigFiles(FC);
  if FL<>Nil then
    begin
    P:=FcStrListNext(FL);
    While P<>Nil do
      begin
      Writeln('Config file: ',P);
      P:=FcStrListNext(FL);
      end;
    FcStrListDone(FL);
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
