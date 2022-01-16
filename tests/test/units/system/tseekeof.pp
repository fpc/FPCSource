program TSeekEof;
{$DEFINE DEBUG}
{$I+}

{$IFNDEF FPC}
uses
 Dos;
{$ENDIF FPC}

const
 Line1 = '  123 23 45   ';
 Line2 = '                                            '#9#9#9'              ';

var
 T: text;
 F: file;
 B: byte;
 SeekEofReached: boolean;

begin
 Assign (T, 'tseekeof.txt');
 Assign (F, 'tseekeof.txt');
 Rewrite (T);
 WriteLn (T, Line1);
 WriteLn (T, Line2);
 WriteLn (T, Line2);
 WriteLn (T, Line2);
 WriteLn (T, Line2);
 Close (T);
 TextRec (T).BufSize := 5;
(* Buffer size decreased to make sure that the buffer needs to be read more often *)
 Reset (T);
{$IFDEF DEBUG}
 WriteLn ('Before: BufPos = ', TextRec (T).BufPos, ', BufEnd = ', TextRec (T).BufEnd);
{$ENDIF DEBUG}
 SeekEofReached := SeekEof (T);
{$IFDEF DEBUG}
 WriteLn ('After: BufPos = ', TextRec (T).BufPos, ', BufEnd = ', TextRec (T).BufEnd);
{$ENDIF DEBUG}
 while not (SeekEofReached) do
  begin
   Read (T, B);
{$IFDEF DEBUG}
   WriteLn ('Read: ', B);
{$ENDIF DEBUG}
{$IFDEF DEBUG}
 WriteLn ('Before: BufPos = ', TextRec (T).BufPos, ', BufEnd = ', TextRec (T).BufEnd);
{$ENDIF DEBUG}
   SeekEofReached := SeekEof (T);
{$IFDEF DEBUG}
 WriteLn ('After: BufPos = ', TextRec (T).BufPos, ', BufEnd = ', TextRec (T).BufEnd);
{$ENDIF DEBUG}
  end;
{$IFDEF DEBUG}
 WriteLn ('SeekEof reached');
{$ENDIF DEBUG}
 if not (Eof (T)) then
  begin
{$IFDEF DEBUG}
   WriteLn ('File not at EOF after SeekEof!');
{$ENDIF DEBUG}
   Close (T);
   Erase (F);
   Halt (1);
  end
 else
{$IFDEF DEBUG}
  WriteLn ('File at EOF after SeekEof');
{$ENDIF DEBUG}
 Close (T);
 Erase (F);
end.
