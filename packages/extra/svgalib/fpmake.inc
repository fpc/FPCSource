    StartPackage('svgalib');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/svgalib';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='svgalib';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    T:=Targets.AddUnit('svgalib');
    T:=Targets.AddUnit('vgamouse');
    T:=Targets.AddExampleunit('testvga');
    T:=Targets.AddExampleunit('vgatest');
    EndPackage;
