program bug1;

{$mode objfpc}{$H+}
{$r+}

uses Classes;

type

  TCTEntry = record
    Name: AnsiString;
    g: Integer;
  end;

  TCT = record
    Size: Integer;
    Names: array of PChar;
    IReps: array of TCTEntry;
  end;


const

  C: array [0..2] of TCTEntry =
  ((Name:'A'; g:0),
   (Name:'B'; g:0),
   (Name:'C'; g:1));


var
  CTs: array [0..1] of TCT;
  p: Integer;

  procedure A(T: array of TCTEntry);
  var
    i: Integer;
  begin
    with CTs[p] do begin
      Size := Length(T);
      Setlength(IReps, Size);
      Setlength(Names, Size+1);
      Names[Size] := nil;
      for i := 0 to Size-1 do begin
        Names[i] := PChar(T[i].Name);
        IReps[i] := T[i];
      end;
    end;
  end;

begin
  p := 0;
  A(C);
end.

