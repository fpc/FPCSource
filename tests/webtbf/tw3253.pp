{ %fail }

{ Source provided for Free Pascal Bug Report 3253 }
{ Submitted by "marco" on  2004-08-16 }
{ e-mail:  }
PROGRAM TEST;
 {$R+}
 {$Q+}
 CONST Range = 10;
 { The next line is not possible, the ( expects an enumeration declation }
 TYPE Sometype = ((-1)*(Range-1))..(Range-1);
 VAR Somevar: SOMETYPE;
 BEGIN
   Somevar := -9;
   WriteLn(Somevar);
 END.
