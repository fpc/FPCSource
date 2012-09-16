program sample;

{$mode objfpc}{$H+}

uses
  SysUtils,
  sdo, sdo_consts, sdo_datafactory, sdo_xsd_helper, sdo_xpath_helper;

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

  procedure PopulateFactoryByCode(AFactory : ISDODataFactory);
  begin
    // add the types
    AFactory.AddType(s_uri,s_project_list_type,[]);
    AFactory.AddType(s_uri,s_project_type,[]);
    AFactory.AddType(s_uri,s_person_type,[]);

    // Fill the informations of the project list type
    AFactory.addProperty(s_uri,s_project_list_type,s_project,s_uri,s_project_type,[pfIsMany,pfIsContainment]);
    AFactory.addProperty(s_uri,s_project_list_type,s_changeSummary,sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType],[pfIsReadOnly]);

    // Fill the informations of the project type
    AFactory.addProperty(s_uri,s_project_type,s_project_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    AFactory.addProperty(s_uri,s_project_type,s_member,s_uri,s_person_type,[pfIsMany,pfIsContainment]);
    AFactory.addProperty(s_uri,s_project_type,s_project_leader,s_uri,s_person_type,[]);

    // Fill the informations of the person type
    AFactory.addProperty(s_uri,s_person_type,s_name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
    AFactory.addProperty(s_uri,s_person_type,s_is_manager,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[pfIsAttribute]);
    AFactory.addProperty(s_uri,s_person_type,s_manager,s_uri,s_person_type,[]);
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
  changeLS : ISDOChangedDataObjectList;
  i : Integer;
  x : ISDODataObject;
begin
  fact := TSDODataFactory.Create();
  PopulateFactoryByCode(fact);
  generateschemafilefromfactory(fact,'out.xsd');
  projList := fact.createNew(s_uri,s_project_list_type); 
  // stop changes tracking
  projList.getChangeSummary().endLogging();
  //Add some data
  proj := projList.createDataObject(s_project);
    //add the project object to the list
    projList.getList(s_project).append(proj);
    proj.setString(s_project_name,'WST');
    pers := proj.createDataObject(s_member);
      proj.getList(s_member).append(pers);
      pers.setString(s_name,'Inoussa O.');
      pers.setBoolean(s_is_manager,True);

  //Add some data
  proj := projList.createDataObject(s_project);
    //add the project object to the list
    projList.getList(s_project).append(proj);
    proj.setString(s_project_name,'sample project');
    pers := proj.createDataObject(s_member);
      proj.getList(s_member).append(pers);
      pers.setString(s_name,'Inoussa O.');
      pers.setBoolean(s_is_manager,True);
    pers := proj.createDataObject(s_member);
      proj.getList(s_member).append(pers);
      pers.setString(s_name,'David KIS');
      // this demonstrates object finding by sdo xpath expression
      pers.setDataObject(s_manager,proj.getDataObject('Member[Name="Inoussa O."]'));

  // start changes trackink now
  projList.getChangeSummary.beginLogging();
    proj.setString(s_project_name,'Object Pascal Project');
    pers.setString(s_name,'D.K.');
    pers := proj.createDataObject(s_member);
      proj.getList(s_member).append(pers);
      pers.setString(s_name,'Kis');
      pers.setBoolean(s_is_manager,False);
      // this demonstrates object finding by sdo xpath expression
      pers.setDataObject(s_manager,projList.getDataObject('Project[0]/Member[Name="Inoussa O."]'));
   projList.setDataObject('Project[0]/ProjectLeader',projList.getDataObject('Project[0]/Member[Name="Inoussa O."]'));

  proj := projList.createDataObject(s_project);
    projList.getList(s_project).append(proj);
    proj.setString(s_project_name,'SDO');
    pers := proj.createDataObject(s_member);
      proj.getList(s_member).append(pers);
      pers.setString(s_name,'SDO dev manager');
      pers.setBoolean(s_is_manager,True);
    pers := proj.createDataObject(s_member);
      proj.getList(s_member).append(pers);
      pers.setString(s_name,'SDO dev 1');
      pers.setDataObject(s_manager,projList.getDataObject('Project[2]/Member[Name="SDO dev manager"]'));
    pers := proj.createDataObject(s_member);
      proj.getList(s_member).append(pers);
      pers.setString(s_name,'SDO dev 2');
      pers.setDataObject(s_manager,projList.getDataObject('Project[2]/Member[Name="SDO dev manager"]'));
    proj.setDataObject('ProjectLeader',proj.getDataObject('Member[Name="SDO dev manager"]'));
  
  projList.getList(s_project).delete(0);
  
  //save it now to file
  changeLS := projList.getChangeSummary.getChangedDataObjects();
  WriteLn('There are ',changeLS.size(), ' changed objects :');
  for i := 0 to changeLS.size() - 1 do begin
    WriteLn('    ChangeType = ',changeLS.getType(i));
    x := changeLS.getDataObject(i);
    WriteLn('    Object Path = ',getXpath(changeLS.getDataObject(i)));
  end;

  
  //revert changes
  projList.getChangeSummary.undoChanges();
  WriteLn('There are ',changeLS.size(), ' changed objects after changes undo.');

end;

begin
  MainProc();
end.

