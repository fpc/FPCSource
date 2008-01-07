    StartPackage('libndsfpc');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/libndsfpc';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='libndsfpc';
    {$ENDIF}
    OS:=[nds];
    T:=Targets.AddUnit('nds7');
    T:=Targets.AddUnit('nds9');
    EndPackage;
