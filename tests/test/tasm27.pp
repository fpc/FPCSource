{ %CPU=i8086 }

{$IFDEF FPC}
{$MODE TP}
{$ENDIF}
program tasm27;

type
  TRec = record
    Str: String[8];
    Arr: array [-5..10] of Integer;
  end;

var
  Rec: TRec;

procedure x; assembler;
asm
  dd Rec.Str                    { dd Rec.Str }
  dd Rec.Str[0]                 { dd Rec.Str }
  dd Rec.Arr                    { dd Rec.Arr }
  dd Rec.Arr[2]                 { dd Rec.Arr+2 }
  dd 5[7]                       { dd 12 }
  dd 5+[7]                      { dd 12 }
  dd 5-[7]                      { dd -2 }
  dd [5]                        { dd 5 }

  dd byte ptr Rec.Str           { dd Rec.Str }
  dd byte ptr Rec.Str[0]        { dd Rec.Str }
  dd byte ptr Rec.Arr           { dd Rec.Arr }
  dd byte ptr Rec.Arr[2]        { dd Rec.Arr+2 }
  dd byte ptr 5[7]              { dd 12 }
  dd byte ptr 5+[7]             { dd 12 }
  dd byte ptr 5-[7]             { dd -2 }
  dd byte ptr [5]               { dd 5 }

  dd word ptr Rec.Str           { dd Rec.Str }
  dd word ptr Rec.Str[0]        { dd Rec.Str }
  dd word ptr Rec.Arr           { dd Rec.Arr }
  dd word ptr Rec.Arr[2]        { dd Rec.Arr+2 }
  dd word ptr 5[7]              { dd 12 }
  dd word ptr 5+[7]             { dd 12 }
  dd word ptr 5-[7]             { dd -2 }
  dd word ptr [5]               { dd 5 }
end;

begin
end.
