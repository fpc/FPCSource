{ %NORUN }

unit ureccomp60;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

interface

type
  TChildRec = record
    C: Integer;
  end;

  TComposed = record
    A: Integer;
    B: Integer;
    contains child: TChildRec;
    D: Integer;
  end;

implementation
end.
