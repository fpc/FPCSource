{ Old file: tbs0112.pp }
{ still generates an internal error 10                  OK 0.99.1 (FK) }

type
  TextBuf=array[0..127] of char;
  TextRec=record
    BufPtr : ^textbuf;
    BufPos : word;
  end;

Function ReadNumeric(var f:TextRec;var s:string;base:longint):Boolean;
{
  Read Numeric Input, if buffer is empty then return True
}
begin
  while ((base>=10) and (f.BufPtr^[f.BufPos] in ['0'..'9'])) or
        ((base=16) and (f.BufPtr^[f.BufPos] in ['A'..'F'])) or
        ((base=2) and (f.BufPtr^[f.BufPos] in ['0'..'1'])) do
   Begin
   End;
end;

begin
end.
