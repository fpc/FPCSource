{ Old file: tbs0106.pp }
{ typecasts are now ignored problem (NOT A bugs)         OK 0.99.1 }

{$R-}

{ I think this now occurs with most type casting... }
{ I think type casting is no longer considered??     }

Var
 Sel: Word;
 Sel2: byte;
Begin
 Sel:=word($7fffffff);
 Sel2:=byte($7fff);
end.
