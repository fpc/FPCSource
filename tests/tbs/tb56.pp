{ Old file: tbs0062.pp }
{  shows illegal type conversion for boolean            OK 0.99.6 (PFV) }

Program Bug0062;


var
 myvar:boolean;
Begin
 { by fixing this we also start partly implementing LONGBOOL/WORDBOOL }
 myvar:=boolean(1);      { illegal type conversion }
end.
