{ Old file: tbs0103.pp }
{ problems with boolean typecasts (other type)          OK 0.99.6 (PFV) }


Var
 out: boolean;
 int: byte;
Begin
 { savesize is different! }
 out:=boolean((int AND $20) SHL 4);
end.
