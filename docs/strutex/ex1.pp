{$ifdef fpc}
{$mode objfpc}
{$h+}
{$endif}
Program ex1;

uses StrUtils;

Const
  S : PChar = 'Some very long string with some words in it';
  Starts : Array[1..3] of Integer = (0,10,41);

Procedure DoTest(Sub:String; Start : Integer; O : TStringSearchOptions);

Var
  Res : String;
  P : PChar;

begin
  Write('Searching for "',Sub,'" starting at pos ',Start,' : ');
  P:=SearchBuf(Pchar(S),Length(S),Start,0,Sub,O);
  if (P=Nil) then
      Writeln('Not found')
  else
    begin
    Res:=StringOfChar(' ',Length(Sub));
    SetLength(Res,Length(Sub));
    Move(P^,Res[1],Length(Sub));
    Writeln('Found at pos ',(P-PChar(S)),' : ',Res);
    end;
end;

Procedure DoTests(Sub : String; O : TStringSearchOptions);

Var
  I : Integer;

begin
  Writeln('Searching up');
  For I:=1 to 3 do
    DoTest(Sub,Starts[i],O);
  Include(O,soDown);
  Writeln('Searching down');
  For I:=1 to 3 do
    DoTest(Sub,Starts[i],O);
end;

Procedure DoAllTests(S : String);

begin
  Writeln('No options');
  DoTests(S,[]);
  Writeln('Match case:');
  DoTests(S,[soMatchCase]);
  Writeln('Whole word:');
  DoTests(S,[soWholeWord]);
  Writeln('Match case, whole word:');
  DoTests(S,[soMatchCase,soWholeWord]);
end;

begin
  DoAllTests('very');
  DoAllTests('Very');
  DoAllTests('in');
  DoAllTests('In');
end.