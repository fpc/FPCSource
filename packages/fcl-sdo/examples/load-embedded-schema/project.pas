program project;

{$mode objfpc}{$H+}

uses
  SysUtils,
  sdo, sdo_datafactory, sdo_serialization,
  sdo_serialization_xml, sdo_xsd_helper;

const
  s_manager = 'Manager';
  s_is_manager = 'IsManager';
  s_member = 'Member';
  s_name = 'Name';
  s_person_type = 'Person';
  s_project_leader = 'ProjectLeader';
  s_project_list_type = 'ProjectList';
  s_project_name = 'ProjectName';
  s_project = 'Project';
  s_project_type = 'ProjectType';
  s_uri = 'uri:sample';

  function IndentStr(const AIndent : Integer) : string;
  begin
    Result := StringOfChar(' ',3*AIndent);
  end;
  
  function ToString(const AObj : ISDODataObject; const AIndent : Integer) : string;overload;forward;
  function ToString(const AObj : ISDODataObjectList; const AItemType : ISDOType; const AIndent : Integer) : string;overload;
  var
    crs : ISDOCursor;
    oldPos : ISDOCursorBookmark;
  begin
    crs := AObj.getCursor();
    oldPos := crs.GetBookmark();
    try
      crs.Reset();
      if AItemType.isDataType() then begin
        while crs.MoveNext() do begin
          Result := Format('%s, %s',[Result,AObj.getString()]);
        end;
      end else begin
        while crs.MoveNext() do begin
          Result := Result + sLinebreak + IndentStr(AIndent+1) + '(' +// sLineBreak + 
                      Copy(IndentStr(1),2,1024) + TrimLeft(ToSTring(AObj.getDataObject(),AIndent+1)) + sLineBreak + 
                    IndentStr(AIndent+1) + ')';
        end;
        Result := Copy(IndentStr(1),2,1024) + TrimLeft(Result);
      end;
      if ( Length(Result) > 0 ) and ( Result[1] = ',' ) then
        Delete(Result,1,1);
    finally
      crs.GotoBookmark(oldPos);
    end;
    Result := sLineBreak + IndentStr(AIndent) + '[' + //sLineBreak +
                Result +
              sLineBreak + IndentStr(AIndent) + ']';
  end;

  function ToString(const AObj : ISDODataObject; const AIndent : Integer) : string;overload;
  var
    pls : ISDOPropertyList;
    p : ISDOProperty;
    i, c : PtrInt;
  begin
    Result := '';
    if ( AObj = nil ) then begin
      Result := sLineBreak + IndentStr(AIndent + 1) +'<nil>';
    end else begin
      pls := AObj.getInstanceProperties();
      c := pls.getCount();
      if ( c > 0 ) then begin
        for i := 0 to Pred(c) do begin
          p := pls.getItem(i);
          if ( p.getTypeEnum() = ChangeSummaryType ) then
            Continue;
          if not p.isMany() then begin
            if p.getType().isDataType() then begin
              Result := Result + sLineBreak +
                        IndentStr(AIndent + 1) + p.getName() + ' : ' + AObj.getString(p);
            end else begin
              Result := Result + sLineBreak +
                        IndentStr(AIndent + 1) + p.getName() + ' :' + //sLineBreak +
                        ToString(AObj.getDataObject(p),AIndent+1);
            end;
          end else begin
              Result := Result + sLineBreak +
                        IndentStr(AIndent + 1) + p.getName() + ' :' + //sLineBreak +
                        ToString(AObj.getList(p),p.getType(),AIndent+2);
          end;
        end;
        if ( Length(Result) > 0 ) and ( Result[1] = ',' ) then
          Delete(Result,1,1);
      end;
    end;
    Result := IndentStr(AIndent) + Result;
  end;

  procedure GenerateSchemaFileFromFactory(AFactory : ISDODataFactory; AFileName : string);
  var
    xsdHelper : IXSDHelper;     
  begin
    xsdHelper := TXSDHelper.Create(AFactory); 
    xsdHelper.Generate(AFactory.getTypes(),s_uri,AFileName);
  end;

procedure MainProc();
var
  fact : ISDODataFactory;
  projList, proj, pers,b : ISDODataObject;
  serializer : ISDOSerializer;
  locDataFileName, locSchemaFileName : string;
begin
  fact := TSDODataFactory.Create();
  locDataFileName := ExpandFileName('.' + PathDelim + 'data-with-schema.xml');
  
  //Load data from the file
  serializer := TSDOSerializer.Create(fact,TSDOSerializerStreamXML.Create());
  projList := serializer.load(locDataFileName);
  
  locSchemaFileName := ExpandFileName('.' + PathDelim + 'schema.xsd');
  GenerateSchemaFileFromFactory(fact,locSchemaFileName);
  WriteLn('Schema file generated !');
  
  Writeln;
  WriteLn('Data :');
  WriteLn(ToString(projList,0));
end;

begin
  MainProc();
end.

