    StartPackage('zvt');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/gnome1/zvt';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='gnome1/zvt';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    Dependencies.Add('gtk');
    Dependencies.Add('imlib');
    T:=Targets.AddUnit('zvt/libzvt');
    EndPackage;
