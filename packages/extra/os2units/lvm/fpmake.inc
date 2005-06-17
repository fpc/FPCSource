    StartPackage('lvm');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/os2units/lvm';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='os2units/lvm';
    {$ENDIF}
    OS:=[os2,emx];
    T:=Targets.AddUnit('lvm');
    EndPackage;
