program DumpClass;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

const
  VMT_COUNT = 100;


type
  TMethodNameTableEntry = packed record
      Name: PShortstring;
      Addr: Pointer;
    end;

  TMethodNameTable = packed record
    Count: DWord;
    Entries: packed array[0..9999999] of TMethodNameTableEntry;
  end;
  PMethodNameTable =  ^TMethodNameTable;

  TPointerArray = packed array[0..9999999] of Pointer;
  PPointerArray = ^TPointerArray;

  PFieldInfo = ^TFieldInfo;
  TFieldInfo = packed record
    FieldOffset: LongWord;
    ClassTypeIndex: Word;
    Name: ShortString;
  end;

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
    Count: Word;
    Entries: array[Word] of TPersistentClass;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
    FieldCount: Word;
    ClassTable: PFieldClassTable;
    { Fields: array[Word] of TFieldInfo;  Elements have variant size! }
  end;

{$M+}
  TMyTest = class(TObject)
  published
    F1: TMyTest;
    F2: TMyTest;
    procedure P1; virtual;
    procedure P2; virtual;
  end;
{$M-}

  TMyTest2 = class(TMyTest)
    F3: TMyTest;
    F4: TMyTest;
    procedure P2; override;
    procedure P3; virtual;
  end;

  TMyPersistent = class(TPersistent)
    procedure P1; virtual;
    procedure P2; virtual;
  end;

procedure TMyTest.P1;
begin
end;

procedure TMyTest.P2;
begin
end;

procedure TMyTest2.P2;
begin
end;

procedure TMyTest2.P3;
begin
end;

procedure TMyPersistent.P1;
begin
end;

procedure TMyPersistent.P2;
begin
end;

procedure ClassDump(AClass: TClass);
var
  Cvmt: PPointerArray;
  Cmnt: PMethodNameTable;
  Cft:  PFieldTable;
  FieldOffset: LongWord;
  fi:  PFieldInfo;
  Indent: String;
  n, idx: Integer;
  SearchAddr: Pointer;
begin
  WriteLn('---------------------------------------------');
  WriteLn('Dump of ', AClass.ClassName);
  WriteLn('---------------------------------------------');
  Indent := '';
  while AClass <> nil do
  begin
    WriteLn(Indent, 'Processing ', AClass.Classname);
    Indent := Indent + ' ';

    //---
    Cmnt := PPointer(Pointer(AClass) + vmtMethodTable)^;
    if Cmnt <> nil
    then begin
      WriteLn(Indent, 'Method count: ', IntToStr(Cmnt^.Count));

      Cvmt := Pointer(AClass) + vmtMethodStart;

      for n := 0 to Cmnt^.Count - 1 do
      begin
        Write(Indent, 'Search: ', Cmnt^.Entries[n].Name^);

        SearchAddr := Cmnt^.Entries[n].Addr;
        for idx := 0 to VMT_COUNT - 1 do
        begin
          if Cvmt^[idx] = SearchAddr
          then begin
            WriteLn(Indent, ' Found at index: ', IntToStr(idx));
            Break;
          end;
          if idx = VMT_COUNT - 1
          then begin
            WriteLn('[WARNING] VMT entry "', Cmnt^.Entries[n].Name^, '" not found in "', AClass.ClassName, '"');
            Break;
          end;
        end;
      end;
    end;


    //---
    Cft := PPointer(Pointer(AClass) + vmtFieldTable)^;
    if Cft <> nil
    then begin
      WriteLn(Indent, 'Field count: ', Cft^.FieldCount);
      fi := @Cft^.ClassTable + SizeOf(Cft^.ClassTable);
      for n := 0 to Cft^.FieldCount - 1 do
      begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
        pointer(fi):=align(fi,sizeof(pointer));
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

        Move(fi^.FieldOffset, FieldOffset, SizeOf(FieldOffset));
        WriteLn(Indent, ' ', n, ': ', fi^.Name, ' @', FieldOffset);
        fi := @fi^.name + 1 + Ord(fi^.name[0]);
      end;
      WriteLn(Indent, 'Field class count: ', Cft^.ClassTable^.Count);
      for n := 0 to Cft^.ClassTable^.Count - 1 do
      begin
        WriteLn(Indent, ' ', n, ': ', Cft^.ClassTable^.Entries[n].ClassName);
      end;
    end;

    AClass := AClass.ClassParent;
  end;
end;

begin
  ClassDump(TMyTest);
  ClassDump(TMyTest2);
  ClassDump(TPersistent);
  ClassDump(TMyPersistent);
end.
