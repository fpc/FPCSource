{************************************************}
{                                                }
{   Grep message filter example                  }
{   Copyright (c) 1992 by Borland International  }
{                                                }
{************************************************}

program Grep2Msg;

{ Message filters read input from the target program (in this case, GREP)
  by way of StdIn (by using Read or ReadLn), filter the input, then write
  output back to StdOut (using Write or WriteLn). The IDE takes care of
  redirecting the transfer program's output to the filter program, as well
  as redirecting the filter program's output back to the IDE itself.
}

{$I-,S-}

var
  LineNo, E: Word;
  P1,P2: integer;
  Line: String;
  InputBuffer: array[0..4095] of Char;
  OutputBuffer: array[0..4095] of Char;


{ The first data passed back to the IDE by a message filter must always
  be the string 'BI#PIP#OK', followed by a null terminator.
}
procedure WriteHeader;
begin
  Write('BI#PIP#OK'#0);
end;

{ The beginning of a new file is marked by a #0, the file's name, terminated
  by a #0 character.
}
procedure WriteNewFile(const FileName: String);
begin
  Write(#0, FileName, #0);
end;

{ Each message line begins with a #1, followed the line number (in low/high
  order), followed by the column number (in low/high order), then the
  message text itself, terminated with a #0 character.
}
procedure WriteMessage(Line, Col: Word; const Message: String);
begin
  Write(#1, Chr(Lo(Line)), Chr(Hi(Line)), Chr(Lo(Col)), Chr(Hi(Col)),
    Message, #0);
end;

{ The end of the input stream is marked by a #127 character }
procedure WriteEnd;
begin
  Write(#127);
end;

function TrimLeft(S:String): String;
var
  i: Integer;
  n: String;
begin
  i := 1;
  while (i <= Length(s)) and (s[i] = #32) do Inc(i);
  if i <= Length(s) then
  begin
    Move(s[i], n[1], Length(s) - i + 1);
    n[0] := Char(Length(s) - i + 1);
  end
  else n[0] := #0;
  TrimLeft := n;
end;

const LastFileName: string = '';

begin
  SetTextBuf(Input, InputBuffer);
  SetTextBuf(Output, OutputBuffer);
  WriteHeader;
  while not Eof do
  begin
    ReadLn(Line);
    if Line <> '' then
    begin
      P1:=Pos(':',Line);
      if copy(Line, 1, P1)<>LastFileName then
        begin
          LastFileName:=copy(Line,1,P1-1);
          WriteNewFile(LastFileName);
        end;
      P2:=Pos(':',copy(Line,P1+1,255));
      if P2>0 then
      begin
        Val(Copy(Line, P1+1, P2-1), LineNo, E);
        if E = 0 then WriteMessage(LineNo, 1, TrimLeft(Copy(Line, P1+1+P2, 132)));
      end;
    end;
  end;
  WriteEnd;
end.
