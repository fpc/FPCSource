{ %CPU=i386 }
{$IFDEF FPC}
{$ASMMODE INTEL}
{$ENDIF}
{$N+}

FUNCTION Floor(M2:Comp):LONGINT;assembler;

VAR X : COMP;
    X2 : LONGINT;
    X3 : Double;
    s : single;

ASM
        FLD     QWord Ptr X                  // Here S_IL must be changed to
                                             // S_FL, i.e. the compiler must generate
                                             // fldl "X" instead of fldq "X" which is wrong
        fld     X2                                                      // No mem64, so no problem
        FLD     QWord Ptr X3                 // This one goes wrong under AS
        FilD     QWord Ptr X                         // This one translates to fildq and is accepted?
        fild     X2                                                  // No mem64, so no problem
        FiLD     QWord Ptr X3                // This one translates to fildq and is accepted?
end;

BEGIN
END.
