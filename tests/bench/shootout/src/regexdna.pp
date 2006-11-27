{$mode objfpc}
{$H-}
uses
  regexpr;
var
  buffer : PChar;
  buffer2 : ansistring;
  seqlen : longint;
  TextBuf: array[0..$FFF] of byte;

procedure Load;
var
  len : longint;
  buffersize, bufferptr: longint;
  s : Shortstring;
begin
  buffersize:=1024;
  buffer:=getmem(buffersize);
  bufferptr :=0;
  while not eof do begin
    readln(s);
    len:=length(s);
    if (bufferptr+len+1)>buffersize then begin
      inc(buffersize,buffersize);
      reallocmem(buffer,buffersize);
    end;
    move (s[1],buffer[bufferptr],len);
    inc(bufferptr,len);
  end;
  buffer[bufferptr] := #0;
  seqlen:=bufferptr;
  writeln(seqlen);
end;

procedure ReplaceNewline;
  begin
    GenerateRegExprEngine('>.*\n|\n',[],RegExprEngine);
    writeln(RegExprReplace(RegExprEngine,buffer,'',buffer2));
    DestroyRegExprEngine(RegExprEngine);
  end;

begin
  SetTextBuf(input, TextBuf, sizeof(TextBuf));
  Load;
end.
