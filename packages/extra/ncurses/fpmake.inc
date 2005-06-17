    StartPackage('ncurses');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/ncurses';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='ncurses';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    T:=Targets.AddUnit('ncurses');
    T:=Targets.AddUnit('panel');
    T:=Targets.AddUnit('ncrt');
    T:=Targets.AddUnit('ocrt');
    T:=Targets.AddUnit('menu');
    T:=Targets.AddExampleunit('firework');
    T:=Targets.AddExampleunit('testn');
    T:=Targets.AddExampleunit('ocrt_demo');
    T:=Targets.AddExampleunit('edit_demo');
    T:=Targets.AddExampleunit('db_demo');
    T:=Targets.AddExampleunit('screen_demo');
    EndPackage;
