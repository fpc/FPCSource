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

  dd dword ptr Rec.Str          { dd Rec.Str }
  dd dword ptr Rec.Str[0]       { dd Rec.Str }
  dd dword ptr Rec.Arr          { dd Rec.Arr }
  dd dword ptr Rec.Arr[2]       { dd Rec.Arr+2 }
  dd dword ptr 5[7]             { dd 12 }
  dd dword ptr 5+[7]            { dd 12 }
  dd dword ptr 5-[7]            { dd -2 }
  dd dword ptr [5]              { dd 5 }

  dd qword ptr Rec.Str          { dd Rec.Str }
  dd qword ptr Rec.Str[0]       { dd Rec.Str }
  dd qword ptr Rec.Arr          { dd Rec.Arr }
  dd qword ptr Rec.Arr[2]       { dd Rec.Arr+2 }
  dd qword ptr 5[7]             { dd 12 }
  dd qword ptr 5+[7]            { dd 12 }
  dd qword ptr 5-[7]            { dd -2 }
  dd qword ptr [5]              { dd 5 }

  dd tbyte ptr Rec.Str          { dd Rec.Str }
  dd tbyte ptr Rec.Str[0]       { dd Rec.Str }
  dd tbyte ptr Rec.Arr          { dd Rec.Arr }
  dd tbyte ptr Rec.Arr[2]       { dd Rec.Arr+2 }
  dd tbyte ptr 5[7]             { dd 12 }
  dd tbyte ptr 5+[7]            { dd 12 }
  dd tbyte ptr 5-[7]            { dd -2 }
  dd tbyte ptr [5]              { dd 5 }
end;

begin
end.
