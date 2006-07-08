    StartPackage('gbaunits');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/gbaunits';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='gbaunits';
    {$ENDIF}
    OS:=[gba];
    T:=Targets.AddUnit('gba');
    T:=Targets.AddUnit('gba_bg');
    T:=Targets.AddUnit('gba_bios');
    T:=Targets.AddUnit('gba_blend');
    T:=Targets.AddUnit('gba_core');
    T:=Targets.AddUnit('gba_dma');
    T:=Targets.AddUnit('gba_fade');
    T:=Targets.AddUnit('gba_input');
    T:=Targets.AddUnit('gba_irq');
    T:=Targets.AddUnit('gba_mosaic');
    T:=Targets.AddUnit('gba_regs');
    T:=Targets.AddUnit('gba_sio');
    T:=Targets.AddUnit('gba_sprites');
    T:=Targets.AddUnit('gba_timer');
    T:=Targets.AddUnit('gba_types');
    T:=Targets.AddUnit('gba_video');
    T:=Targets.AddUnit('gba_window');
    T.OS:=[gba];
    EndPackage;
