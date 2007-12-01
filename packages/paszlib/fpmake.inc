    StartPackage('paszlib');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/paszlib';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='paszlib';
    {$ENDIF}
    Version:={$i %FPCVERSION%};
    T:=Targets.AddUnit('paszlib');
    T:=Targets.AddUnit('adler');
    T:=Targets.AddUnit('crc');
    T:=Targets.AddUnit('gzio');
    T:=Targets.AddUnit('infblock');
    T:=Targets.AddUnit('infcodes');
    T:=Targets.AddUnit('inffast');
    T:=Targets.AddUnit('inftrees');
    T:=Targets.AddUnit('infutil');
    T:=Targets.AddUnit('trees');
    T:=Targets.AddUnit('zcompres');
    T:=Targets.AddUnit('zdeflate');
    T:=Targets.AddUnit('zinflate');
    T:=Targets.AddUnit('zbase');
    T:=Targets.AddUnit('zuncompr');
    T:=Targets.AddUnit('zutil');
    T:=Targets.AddExampleunit('example');
    T:=Targets.AddExampleunit('minigzip');
    EndPackage;
