{ Source provided for Free Pascal Bug Report 2966 }
{ Submitted by "Alexey Barkovoy" on  2004-02-09 }
{ e-mail: clootie@ixbt.com }
program Project1;

type
  {$ALIGN 8} // Can be 8, 16, 32 other value
  TType1 = record
    f: Word;
  end;

  TType2 = packed record
    f, g: Word;
  end;

  TType3 = record
    f, g: Word;
  end;

var
  t: TType2;
  t2: TType3;
begin
  WriteLn('Type1 = ', SizeOf(TType1));
  WriteLn('Type2 = ', SizeOf(TType2));
  WriteLn('Type3 = ', SizeOf(TType3));
  WriteLn('It''s OK = ', DWORD(t));
  WriteLn('This is the bug = ', DWORD(t2)); // Compiler stops here with "Error: Illegal type conversion"
end.
