{ $OPT= -So }
UNIT tbs0179;
INTERFACE
  PROCEDURE A(B:WORD);
IMPLEMENTATION
  PROCEDURE A;  { <-- works with TP, FP says overloading problem }
  BEGIN
    Write(B);
  END;
END.
