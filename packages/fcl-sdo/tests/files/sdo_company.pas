{
This unit has been produced by ws_helper.
  Input unit name : "sdo_company".
  This unit name  : "sdo_company".
  Date            : "20/09/2007 16:32:08".
}
unit sdo_company;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$IFNDEF FPC}
  {$DEFINE WST_RECORD_RTTI}
{$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'company.xsd';
  sUNIT_NAME = 'sdo_company';

type

// ID = unable to resolve this symbol.
  CompanyType_departmentsArray = class;
  CompanyType = class;
  DepartmentType_employeesArray = class;
  DepartmentType = class;
  EmployeeType = class;

  CompanyType = class(TBaseComplexRemotable)
  private
    Fdepartments : CompanyType_departmentsArray;
    Fname : string;
    FemployeeOfTheMonth : string;
  private
    function Hasname() : Boolean;
    function HasemployeeOfTheMonth() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property departments : CompanyType_departmentsArray read Fdepartments write Fdepartments;
    property name : string read Fname write Fname stored Hasname;
    property employeeOfTheMonth : string read FemployeeOfTheMonth write FemployeeOfTheMonth stored HasemployeeOfTheMonth;
  end;

  DepartmentType = class(TBaseComplexRemotable)
  private
    Femployees : DepartmentType_employeesArray;
    Fname : string;
    Flocation : string;
    Fnumber : integer;
  private
    function Hasname() : Boolean;
    function Haslocation() : Boolean;
    function Hasnumber() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property employees : DepartmentType_employeesArray read Femployees write Femployees;
    property name : string read Fname write Fname stored Hasname;
    property location : string read Flocation write Flocation stored Haslocation;
    property number : integer read Fnumber write Fnumber stored Hasnumber;
  end;

  EmployeeType = class(TBaseComplexRemotable)
  private
    Fname : string;
    FSN : ID;
    Fmanager : boolean;
  private
    function Hasname() : Boolean;
    function HasSN() : Boolean;
    function Hasmanager() : Boolean;
  published
    property name : string read Fname write Fname stored Hasname;
    property SN : ID read FSN write FSN stored HasSN;
    property manager : boolean read Fmanager write Fmanager stored Hasmanager;
  end;

  CompanyType_departmentsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): DepartmentType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : DepartmentType Read GetItem;Default;
  end;

  DepartmentType_employeesArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): EmployeeType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : EmployeeType Read GetItem;Default;
  end;

Implementation
uses metadata_repository, record_rtti, wst_types;

{ CompanyType }

constructor CompanyType.Create();
begin
  inherited Create();
  Fdepartments := CompanyType_departmentsArray.Create();
end;

destructor CompanyType.Destroy();
begin
  if Assigned(Fdepartments) then
    FreeAndNil(Fdepartments);
  inherited Destroy();
end;

function CompanyType.Hasname() : Boolean;
begin
  Result := ( Fname <> '' );
end;

function CompanyType.HasemployeeOfTheMonth() : Boolean;
begin
  Result := ( FemployeeOfTheMonth <> '' );
end;

{ DepartmentType }

constructor DepartmentType.Create();
begin
  inherited Create();
  Femployees := DepartmentType_employeesArray.Create();
end;

destructor DepartmentType.Destroy();
begin
  if Assigned(Femployees) then
    FreeAndNil(Femployees);
  inherited Destroy();
end;

function DepartmentType.Hasname() : Boolean;
begin
  Result := ( Fname <> '' );
end;

function DepartmentType.Haslocation() : Boolean;
begin
  Result := ( Flocation <> '' );
end;

function DepartmentType.Hasnumber() : Boolean;
begin
  Result := ( Fnumber <> integer(0) );
end;

function EmployeeType.Hasname() : Boolean;
begin
  Result := ( Fname <> '' );
end;

function EmployeeType.HasSN() : Boolean;
begin
  Result := ( FSN <> ID(0) );
end;

function EmployeeType.Hasmanager() : Boolean;
begin
  Result := ( Fmanager <> boolean(0) );
end;

{ CompanyType_departmentsArray }

function CompanyType_departmentsArray.GetItem(AIndex: Integer): DepartmentType;
begin
  Result := Inherited GetItem(AIndex) As DepartmentType;
end;

class function CompanyType_departmentsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= DepartmentType;
end;

{ DepartmentType_employeesArray }

function DepartmentType_employeesArray.GetItem(AIndex: Integer): EmployeeType;
begin
  Result := Inherited GetItem(AIndex) As EmployeeType;
end;

class function DepartmentType_employeesArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= EmployeeType;
end;


initialization
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CompanyType),'CompanyType');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(DepartmentType),'DepartmentType');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(EmployeeType),'EmployeeType');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CompanyType_departmentsArray),'CompanyType_departmentsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CompanyType_departmentsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(DepartmentType_employeesArray),'DepartmentType_employeesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(DepartmentType_employeesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);

  CompanyType.RegisterAttributeProperty('name');
  CompanyType.RegisterAttributeProperty('employeeOfTheMonth');
  DepartmentType.RegisterAttributeProperty('name');
  DepartmentType.RegisterAttributeProperty('location');
  DepartmentType.RegisterAttributeProperty('number');
  EmployeeType.RegisterAttributeProperty('name');
  EmployeeType.RegisterAttributeProperty('SN');
  EmployeeType.RegisterAttributeProperty('manager');


End.
