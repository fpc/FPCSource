{ %FAIL }
{ Old file: tbf0219.pp }
{ wrong error message                                  OK 0.99.11 (PFV) }

{ Should give '(' expected in line 6 }

   const
     replaces=4;
     replacetab : array[1..replaces,1..2] of string[32]=(
       ':',' or colon',
       'mem8','mem or bits8',
       'mem16','mem or bits16',
       'mem32','mem or bits32'
     )
begin
end.
