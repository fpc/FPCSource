{ Source provided for Free Pascal Bug Report 1485 }
{ Submitted by "Petr Titera" on  2001-05-01 }
{ e-mail: owl@volny.cz }

{$mode objfpc}

Type
        TLang = (French,Czech,English);

Function Test : TLang;
begin
  Test:=French;
  try
    Exit(Czech);
  except
  end;
end;

Begin
        Writeln(Integer(Test));
        if Test<>Czech then
          RunError(1);
        Writeln(Integer(Czech));
End.
