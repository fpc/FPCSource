{ Old file: tbs0152.pp }
{ End value of loop variable must be calculated before loop variable is initialized.                              OK 0.99.11 (PM) }

Program tbs0152;

{
  Shows wrong evaluation of loop boundaries. First end boundary must
  be calculated, only then Loop variable should be initialized.
  Change loop variable to J to see what should be the correct output.
}

PROCEDURE LGrow(VAR S : String;C:CHAR;Count:WORD);

 VAR  I,J :WORD;

BEGIN
  I:=ORD(S[0]);           { Keeping length in local data eases optimalisations}
  IF I<Count THEN
     BEGIN
     Move(S[1],S[Count-I+1],I);
     FOR I:=1 TO Count-I DO
       S[I]:=C;
     S[0]:=CHR(Count);
     END;
END;

Var S : string;

begin
  s:='abcedfghij';
  writeln ('s : ',s);
  lgrow (s,'1',17);
  writeln ('S : ',s);
  if s<>'1111111abcedfghij' then
    begin
       writeln('tbs0152 fails');
       halt(1);
    end;
end.
