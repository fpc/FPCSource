{ %OPT= -So }

{ Old file: tbs0179.pp }
{ show a problem for -So mode                           OK 0.99.9 (PM) }

UNIT tb153;
INTERFACE
  PROCEDURE A(B:WORD);
IMPLEMENTATION
  PROCEDURE A;  { <-- works with TP, FP says overloading problem }
  BEGIN
    Write(B);
  END;
END.
