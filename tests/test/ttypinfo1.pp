program ttypinfo1;

uses
  utypinfo1,
  typinfo;

var
  i,j: integer;
  UnitInfo: PUnitInfo;
  UnitList: PUnitInfoList;
  ATypeInfo: PTypeInfo;
  hasunit: boolean;
  hastype1: boolean;
  hastype2: boolean;
  hastype3: boolean;

begin
  hasunit:=false;
  hastype1:=false;
  hastype2:=false;
  hastype3:=false;
  UnitList:=GetUnitList;
  if UnitList^.UnitCount < 2 then
    Halt(1);

  for i := 0 to UnitList^.UnitCount-1 do
    begin
      UnitInfo:=UnitList^.Units[i];
      if UnitInfo^.UnitName = 'utypinfo1' then
        begin
          hasunit:=true;
          ATypeInfo:=GetFirstTypeinfoFromUnit(UnitInfo);
          if ATypeInfo^.Name <> 'TRec' then
            halt(2);
          if Assigned(GetNextTypeInfo(ATypeInfo)) then
            halt(3);
        end;

      if UnitInfo^.UnitName = 'System' then
        begin
          ATypeInfo:=GetFirstTypeinfoFromUnit(UnitInfo);
          while assigned(ATypeInfo) do
            begin
              if ATypeInfo^.Name='ShortInt' then
                hastype1:=True;
              if ATypeInfo^.Name='TVarRec' then
                hastype2:=True;
              if ATypeInfo^.Name='TResourceManager' then
                hastype3:=True;
              ATypeInfo:=GetNextTypeInfo(ATypeInfo);
            end;
        end;
    end;

  if not hasunit then
    halt(4);
  if not hastype1 or not hastype2 or not hastype3 then
    halt(5);
  writeln('ok');
end.

