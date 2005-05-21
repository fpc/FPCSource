program DumpMethods;

{$mode objfpc}{$H+}

uses
  Classes,  SysUtils;

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

{$M+}
  TMyTest = class(TObject)
//  published
    procedure P1; virtual;
    procedure P2; virtual;
  end;
{$M-}

  TMyTest2 = class(TMyTest)
//  published
    procedure P2; override;
    procedure P3; virtual;
  end;

  TMyPersistent = class(TPersistent)
//  published
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

procedure DumpClass(AClass: TClass);
var
  Cvmt: PPointerArray;
  Cmnt: PMethodNameTable;
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
    Cmnt := PPointer(Pointer(AClass) + vmtMethodTable)^;
    if Cmnt <> nil
    then begin
      WriteLn(Indent, 'Method count: ', IntToStr(Cmnt^.Count));

      Cvmt := Pointer(AClass) + vmtMethodStart;

      for n := 0 to Cmnt^.Count - 1 do
      begin
        WriteLn(Indent, 'Search: ', Cmnt^.Entries[n].Name^);

        SearchAddr := Cmnt^.Entries[n].Addr;
        for idx := 0 to VMT_COUNT - 1 do
        begin
          if Cvmt^[idx] = SearchAddr
          then begin
            WriteLn(Indent, 'Found at index: ', IntToStr(idx));
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
    AClass := AClass.ClassParent;
  end;
end;

begin
  DumpClass(TMyTest);
  DumpClass(TMyTest2);
  DumpClass(TPersistent);
  DumpClass(TMyPersistent);
end.
