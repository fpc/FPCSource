{ Every entry of a class's interface table refers to the type information of
  its interface, so the interface can be told where it has no GUID, and
  without looking its GUID up among the types a program happens to know. }
program tintfentry1;

{$mode objfpc}{$H+}

uses
  typinfo;

type
  {$interfaces com}
  IOne = interface
    ['{3C399564-7FB7-4EC0-8ADD-2A9AE15E7968}']
    procedure One;
  end;

  ITwo = interface(IOne)
    ['{34CCD6BD-DB3F-4931-B024-E4EE514A34D0}']
    procedure Two;
  end;

  { no GUID: only the type tells these two apart }
  IBare = interface
    procedure Bare;
  end;

  IBare2 = interface
    procedure Bare2;
  end;

  generic IGen<T> = interface
    function Get: T;
  end;

  IGenLong = specialize IGen<LongInt>;

  {$interfaces corba}
  ICorba = interface
    ['corba one']
    procedure Corba;
  end;
  {$interfaces com}

  { lists only the descendant interface }
  TTwo = class(TInterfacedObject, ITwo)
    procedure One;
    procedure Two;
  end;

  TBoth = class(TInterfacedObject, IOne, IBare, IBare2)
    procedure One;
    procedure Bare;
    procedure Bare2;
  end;

  TChild = class(TBoth, IGenLong)
    function Get: LongInt;
  end;

  TCorba = class(TObject, ICorba)
    procedure Corba;
  end;

  { nothing else in the program asks for this interface's type information }
  IUnasked = interface
    procedure Unasked;
  end;

  TUnasked = class(TInterfacedObject, IUnasked)
    procedure Unasked;
  end;

  TDelegating = class(TInterfacedObject, IOne, IBare)
  private
    FInner: IOne;
  public
    procedure Bare;
    property Inner: IOne read FInner implements IOne;
  end;

procedure TTwo.One; begin end;
procedure TTwo.Two; begin end;
procedure TBoth.One; begin end;
procedure TBoth.Bare; begin end;
procedure TBoth.Bare2; begin end;
function TChild.Get: LongInt; begin Result := 0; end;
procedure TCorba.Corba; begin end;
procedure TDelegating.Bare; begin end;
procedure TUnasked.Unasked; begin end;

var
  gErrors: LongInt = 0;
  gInfo: PTypeInfo;

procedure Check(aClass: TClass; const aExpected: array of PTypeInfo);
var
  lTable: PInterfaceTable;
  lEntry: PInterfaceEntry;
  lInfo: PTypeInfo;
  lIdx: SizeInt;
begin
  lTable := aClass.GetInterfaceTable;
  if not Assigned(lTable) then
    begin
      writeln(aClass.ClassName, ': no interface table');
      Inc(gErrors);
      Exit;
    end;
  if lTable^.EntryCount <> SizeUInt(Length(aExpected)) then
    begin
      writeln(aClass.ClassName, ': ', lTable^.EntryCount, ' entries, expected ', Length(aExpected));
      Inc(gErrors);
      Exit;
    end;
  lEntry := @lTable^.Entries[0];
  for lIdx := 0 to High(aExpected) do
    begin
      if not Assigned(lEntry^.ITypeInfoRef) then
        begin
          writeln(aClass.ClassName, ', entry ', lIdx, ': no reference to a type information');
          Inc(gErrors);
        end
      else
        begin
          lInfo := lEntry^.ITypeInfo;
          if lInfo <> aExpected[lIdx] then
            begin
              if Assigned(lInfo) then
                writeln(aClass.ClassName, ', entry ', lIdx, ': type information of "', lInfo^.Name, '", expected "', aExpected[lIdx]^.Name, '"')
              else
                writeln(aClass.ClassName, ', entry ', lIdx, ': no type information, expected "', aExpected[lIdx]^.Name, '"');
              Inc(gErrors);
            end;
        end;
      Inc(lEntry);
    end;
end;

begin
  Check(TInterfacedObject, [TypeInfo(IUnknown)]);
  Check(TTwo, [TypeInfo(ITwo)]);
  Check(TBoth, [TypeInfo(IOne), TypeInfo(IBare), TypeInfo(IBare2)]);
  Check(TChild, [TypeInfo(IGenLong)]);
  Check(TCorba, [TypeInfo(ICorba)]);
  Check(TDelegating, [TypeInfo(IOne), TypeInfo(IBare)]);
  { an interface whose type information the program asks for nowhere else }
  gInfo := nil;
  if Assigned(TUnasked.GetInterfaceTable) and (TUnasked.GetInterfaceTable^.EntryCount = 1) then
    gInfo := TUnasked.GetInterfaceTable^.Entries[0].ITypeInfo;
  if not Assigned(gInfo) or (gInfo^.Kind <> tkInterface) or (gInfo^.Name <> 'IUnasked') then
    begin
      writeln('TUnasked: its entry does not lead to the type information of IUnasked');
      Inc(gErrors);
    end;
  { the two interfaces without a GUID are different types }
  if TypeInfo(IBare) = TypeInfo(IBare2) then
    begin
      writeln('IBare and IBare2 share their type information');
      Inc(gErrors);
    end;
  if gErrors <> 0 then
    Halt(1);
  writeln('ok');
end.
