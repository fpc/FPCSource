{ Old file: tbs0184.pp }
{ multiple copies of the same constant set are stored in executable OK 0.99.9 (PFV) }

Program Bug0184;

{ multiple copies of the constant sets are stored in the assembler file when
  they are needed more than once}

Var BSet: Set of Byte;
    SSet: Set of 0..31;
    b,c: byte;
    s: 0..31;

Begin
  BSet := BSet + [b];  {creates a big, empty set}
  BSet := BSet + [c];  {creates another one}
  BSet := BSet + [3];  {creates a big set with element three set}
  BSet := BSet + [3];  {and antoher one}

  SSet := SSet + [5];  {creates a small set containing 5}
  SSet := SSet + [s];  {creates a small, empty set}
  SSet := SSet + [5];  {creates another small set containing 5}
  SSet := SSet + [s];  {creates another small, empty set}

{BTW: small constant sets don't have to be stored seperately in the
 executable, as they're simple 32 bit constants, like longints!}

End.
