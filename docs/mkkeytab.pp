program mkkeytab;

{
 This program takes the keyboard scan code definitions, (the const section
 of the rtl/unix/keyboard.pp file) and outputs a
 latex table. The final output is done with some reformatting with the
 following sed commands:

 mkkeytab | sed 's/[aA]lt/ALT-/g' | sed 's/[Ss]hift/SHIFT-/g' | sed 's/[Cc]trl/CTRL-/g' > keys.tex

}

uses sysutils,classes;

Function ScanLine (S: String) : String;

Var
  I : Integer;
  KN,KC : String;

begin
  I:=Pos('=',S);
  Result:='';
  If I<>0 then
    begin
    KN:=Trim(Copy(S,1,I-1));
    Delete(KN,1,2);
    Delete(S,1,I);
    I:=Pos(';',S);
    If I<>0 then
      begin
      KC:=Trim(Copy(S,1,I-1));
      Delete(KC,1,1);
      Result:= KC+' & '+KN;
      end;
    end;
end;

Var
  F : text;
  List : TstringList;
  I,RowCount : Integer;
  S: String;

begin
  List:=TstringList.Create;
  Assign(f,'keys.txt');
  Reset(f);
  While not eof(f) do
    begin
    Readln (f,s);
    S:=ScanLine(s);
    If S<>'' then
      List.Add(S);
    end;
  RowCount:=List.Count div 3;
  if (List.Count mod 3)<>0 then
    begin
    Inc(RowCount);
    List.Add('');
    List.Add('');
    end;
  For I:=0 to rowcount-1 do
    Writeln(Format('%-20s & %-20s & %-20s \\',[List[i],List[I+RowCount],List[I+2*RowCount]]));
end.  