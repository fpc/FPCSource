PROGRAM NoLabel; { this program compiles fine with TP but not with FP }
LABEL
  N1,
  N2,
  FAIL, { this is a reserved word in constructors only! - FP fails here
}
  More; { label not defined - FP fails, but a warning is enough for that
}
           { since label referenced nowhere }
BEGIN
  N1: Write;
  N2: Write;
  FAIL: Write; 
END.

