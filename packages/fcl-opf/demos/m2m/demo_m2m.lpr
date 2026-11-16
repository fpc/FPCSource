{ demo_m2m - Many-to-many relationships end-to-end
  ================================================
  This demo shows how to:
  - Register a many-to-many (N-M) relationship between two classes
  - Create associations between objects via AddAssociation
  - Load associated objects from both sides via LoadAssociations
  - Cascade delete: deleting an entity removes junction rows but not the other side

  Domain model:
    TStudent <-> TCourse  (junction table: student_course)
    A student can enrol in many courses; a course can have many students.

  Compile:  fpc demo_m2m.lpr -Fu../../src
  Run:      ./demo_m2m
}
program demo_m2m;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,          { TOPManager, TOPConnection }
  Opf.Registry,         { TOPRegistry, TPersisterDef }
  Opf.Fields,           { TOPFieldMap }
  Opf.Sql,              { TOPSQLBuilder, dSQLite }
  Opf.Driver,
  Opf.M2M,              { OPM2MRegistry - M2M relationship registration }
  Opf.Persister.Rtti;   { TRTTIPersister }

const
  DB_FILE = 'demo_m2m.db';

type
  { TStudent:
    One side of the many-to-many relationship.
    ID and Name are persisted columns. CourseList holds associated TCourse
    objects loaded by LoadAssociations - it is NOT a database column. }
  TStudent = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FCourseList: TObjectList;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    { CourseList must be published so LoadAssociations can find it via RTTI.
      It is NOT a database column - we remove it from the field map after
      registering the persister. }
    property CourseList: TObjectList read FCourseList;
  end;

  { TCourse:
    The other side of the many-to-many relationship.
    ID and Title are persisted columns. StudentList holds associated TStudent
    objects loaded by LoadAssociations - it is not a database column. }
    
  TCourse = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FTitle: String;
    FStudentList: TObjectList;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    { StudentList must be published so LoadAssociations can find it via RTTI.
      It is NOT a database column - we remove it from the field map after
      registering the persister. }
    property StudentList: TObjectList read FStudentList;
  end;

{ IOPPersistenceState implementations }

function TStudent.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TStudent.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TStudent.Create;
begin
  inherited Create;
  { OwnsObjects=True by default - the list frees items when cleared or freed }
  FCourseList := TObjectList.Create(True);
end;

destructor TStudent.Destroy;
begin
  FCourseList.Free;
  inherited Destroy;
end;

function TCourse.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TCourse.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TCourse.Create;
begin
  inherited Create;
  FStudentList := TObjectList.Create(True);
end;

destructor TCourse.Destroy;
begin
  FStudentList.Free;
  inherited Destroy;
end;

{ Helper variables and procedures }

var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;

{ ExecSQL: execute a DDL or DML statement on the setup transaction. }
procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := aSQL;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.ExecSQL;
    SetupTrans.Commit;
  finally
    Q.Free;
  end;
end;

{ CountRows:  return the number of rows in a table. }
function CountRows(const aTable: String): Integer;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := 'SELECT COUNT(*) FROM ' + aTable;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.Open;
    Result := Q.Fields[0].AsInteger;
    Q.Close;
    SetupTrans.Commit;
  finally
    Q.Free;
  end;
end;

{ Database setup and teardown }

procedure SetupDatabase;
var
  OpConn: TOPConnection;
  Def: TPersisterDef;
  Idx: Integer;
begin
  { Remove any leftover database file from a previous run }
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  { Create SQLite connection }
  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  { Create the entity tables.
    Student and Course are the two entities in our many-to-many relationship. }
  ExecSQL('CREATE TABLE Student (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecSQL('CREATE TABLE Course (ID INTEGER PRIMARY KEY, Title TEXT)');

  { Create the junction table.
    This table holds pairs of (StudentID, CourseID) representing associations.
    The composite primary key prevents duplicate associations. }
  ExecSQL('CREATE TABLE student_course (' +
          'StudentID INTEGER NOT NULL, ' +
          'CourseID INTEGER NOT NULL, ' +
          'PRIMARY KEY (StudentID, CourseID))');

  { Register TStudent with the RTTI persister.
    The RTTI persister auto-discovers published properties (ID, Name, CourseList).
    CourseList is published (for RTTI access by LoadAssociations) but is NOT a
    database column - remove it from the field map. }
  Def := OPRegistry.RegisterPersister(TStudent, TRTTIPersister);
  Idx := Def.FieldMap.IndexOfPropertyName('CourseList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);

  { Register TCourse - same pattern, removing StudentList from field map }
  Def := OPRegistry.RegisterPersister(TCourse, TRTTIPersister);
  Idx := Def.FieldMap.IndexOfPropertyName('StudentList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);

  { Register the many-to-many relationship.
    Parameters:
      ClassA, ClassB        - the two entity classes
      JunctionTable         - name of the junction (link) table
      ForeignKeyA           - junction column referencing ClassA's PK
      ForeignKeyB           - junction column referencing ClassB's PK
      DBCascade             - False means the ORM handles junction cleanup on delete
                              (True would skip ORM cleanup, trusting DB ON DELETE CASCADE)
      ListPropertyNameA     - property on ClassA that holds associated ClassB objects
      ListPropertyNameB     - property on ClassB that holds associated ClassA objects }
  OPM2MRegistry.RegisterManyToMany(
    TStudent, TCourse,
    'student_course',
    'StudentID', 'CourseID',
    False,
    'CourseList', 'StudentList'
  );

  { Configure the ORM connection }
  OPSQLBuilder.SQLDialect := dSQLite;
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  { Clear singletons in correct order:
    1. Manager (uses Registry and M2M Registry)
    2. M2M Registry (unique to M2M demos)
    3. Registry (persister definitions) }
  TOPManager.ClearInstance;
  TOPManyToManyRegistry.ClearInstance;
  TOPRegistry.ClearInstance;

  { Free transaction BEFORE connection }
  if Assigned(SetupTrans) then
  begin
    SetupTrans.Free;
    SetupTrans := nil;
  end;
  if Assigned(Conn) then
  begin
    if Conn.Connected then
      Conn.Close;
    Conn.Free;
  end;

  { Clean up - delete the SQLite database file }
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

{ Part 1: Association Management
  Demonstrates creating entities and linking them via AddAssociation.
  Each AddAssociation call inserts a row into the junction table. }

procedure DemoPart1_AssociationManagement;
var
  Alice, Bob: TStudent;
  Math, History, Science: TCourse;
begin
  WriteLn('=== Part 1: Association Management ===');
  WriteLn;

  { Create two students }
  Alice := TStudent.Create;
  Alice.ID := 1;
  Alice.Name := 'Alice';
  Alice.FState := opsNew;
  OPManager.Save(Alice);
  WriteLn('Created student: ID=', Alice.ID, ' Name="', Alice.Name, '"');

  Bob := TStudent.Create;
  Bob.ID := 2;
  Bob.Name := 'Bob';
  Bob.FState := opsNew;
  OPManager.Save(Bob);
  WriteLn('Created student: ID=', Bob.ID, ' Name="', Bob.Name, '"');

  { Create three courses }
  Math := TCourse.Create;
  Math.ID := 10;
  Math.Title := 'Math';
  Math.FState := opsNew;
  OPManager.Save(Math);
  WriteLn('Created course:  ID=', Math.ID, ' Title="', Math.Title, '"');

  History := TCourse.Create;
  History.ID := 11;
  History.Title := 'History';
  History.FState := opsNew;
  OPManager.Save(History);
  WriteLn('Created course:  ID=', History.ID, ' Title="', History.Title, '"');

  Science := TCourse.Create;
  Science.ID := 12;
  Science.Title := 'Science';
  Science.FState := opsNew;
  OPManager.Save(Science);
  WriteLn('Created course:  ID=', Science.ID, ' Title="', Science.Title, '"');
  WriteLn;

  { Add associations via AddAssociation.
    Each call inserts a row into the student_course junction table.
    AddAssociation is order-independent: AddAssociation(Alice, Math)
    and AddAssociation(Math, Alice) are equivalent - the ORM resolves
    which object maps to which FK column using the registered M2M def. }
  WriteLn('Adding associations...');

  OPManager.AddAssociation(Alice, Math);
  WriteLn('  Alice <-> Math      (junction row: StudentID=1, CourseID=10)');

  OPManager.AddAssociation(Alice, History);
  WriteLn('  Alice <-> History   (junction row: StudentID=1, CourseID=11)');

  OPManager.AddAssociation(Bob, History);
  WriteLn('  Bob   <-> History   (junction row: StudentID=2, CourseID=11)');

  OPManager.AddAssociation(Bob, Science);
  WriteLn('  Bob   <-> Science   (junction row: StudentID=2, CourseID=12)');

  WriteLn;
  WriteLn('Junction table "student_course" now has ', CountRows('student_course'), ' rows');
  WriteLn('  (Alice has 2 courses, Bob has 2 courses, History is shared)');
  WriteLn;

  { Free the local objects - they were only needed to set up the data.
    The data lives in the database; we will reload from DB in later parts. }
  Alice.Free;
  Bob.Free;
  Math.Free;
  History.Free;
  Science.Free;
end;

{ Part 2: Loading Associations.
  Demonstrates bidirectional loading: load courses for a student,
  and load students for a course. LoadAssociations uses an INNER JOIN
  on the junction table to find associated objects. }

procedure DemoPart2_LoadingAssociations;
var
  Student: TStudent;
  Course: TCourse;
  I: Integer;
begin
  WriteLn('=== Part 2: Loading Associations ===');
  WriteLn;

  { Load courses for Alice (ID=1).
    LoadAssociations(Student, 'CourseList') resolves the property name
    via ListPropertyNameA registered in the M2M def. It queries:
      SELECT c.* FROM Course c
      INNER JOIN student_course sc ON sc.CourseID = c.ID
      WHERE sc.StudentID = 1
    The results are loaded as TCourse objects into Student.CourseList. }
  Student := TStudent.Create;
  try
    Student.ID := 1;
    Student.FState := opsKey;
    OPManager.Load(Student);
    WriteLn('Student: ID=', Student.ID, ' Name="', Student.Name, '"');

    OPManager.LoadAssociations(Student, 'CourseList');
    WriteLn('  Courses loaded: ', Student.CourseList.Count);
    for I := 0 to Student.CourseList.Count - 1 do
      WriteLn('    [', I, '] ID=', TCourse(Student.CourseList[I]).ID,
              ' Title="', TCourse(Student.CourseList[I]).Title, '"');
  finally
    Student.Free;
  end;
  WriteLn;

  { Load students for History (ID=11).
    LoadAssociations(Course, 'StudentList') resolves via ListPropertyNameB.
    This demonstrates bidirectionality - you can traverse the relationship
    from either side. It queries:
      SELECT s.* FROM Student s
      INNER JOIN student_course sc ON sc.StudentID = s.ID
      WHERE sc.CourseID = 11
    Both Alice and Bob are enrolled in History, so both appear. }
  Course := TCourse.Create;
  try
    Course.ID := 11;
    Course.FState := opsKey;
    OPManager.Load(Course);
    WriteLn('Course: ID=', Course.ID, ' Title="', Course.Title, '"');

    OPManager.LoadAssociations(Course, 'StudentList');
    WriteLn('  Students loaded: ', Course.StudentList.Count);
    for I := 0 to Course.StudentList.Count - 1 do
      WriteLn('    [', I, '] ID=', TStudent(Course.StudentList[I]).ID,
              ' Name="', TStudent(Course.StudentList[I]).Name, '"');
  finally
    Course.Free;
  end;
  WriteLn;
end;

{ Part 3: Cascade Delete:
  Demonstrates what happens when you delete an entity that has M2M associations.
  The ORM's DeleteM2MJunctionRows finds all M2M defs for the entity's class
  and, for each def with DBCascade=False, deletes the junction rows.
  The associated entities on the other side are NOT deleted. }

procedure DemoPart3_CascadeDelete;
var
  Student: TStudent;
begin
  WriteLn('=== Part 3: Cascade Delete ===');
  WriteLn;

  { Show state before delete }
  WriteLn('Before delete:');
  WriteLn('  Students:       ', CountRows('Student'));
  WriteLn('  Courses:        ', CountRows('Course'));
  WriteLn('  Junction rows:  ', CountRows('student_course'));
  WriteLn;

  { Delete Alice (ID=1).
    The ORM will:
    1. Find all M2M defs for TStudent (student_course)
    2. Since DBCascade=False, delete junction rows: DELETE FROM student_course WHERE StudentID=1
    3. Delete the student row: DELETE FROM Student WHERE ID=1
    4. Courses are NOT deleted - only the junction (link) rows are removed }
  Student := TStudent.Create;
  try
    Student.ID := 1;
    Student.FState := opsKey;
    OPManager.Load(Student);
    WriteLn('Deleting student: ID=', Student.ID, ' Name="', Student.Name, '"');
    OPManager.Delete(Student);
    WriteLn('Student deleted.');
  finally
    Student.Free;
  end;
  WriteLn;

  { Show state after delete.
    - Students: 2 -> 1 (Alice removed)
    - Courses: 3 -> 3 (all courses remain - M2M delete only removes junction rows)
    - Junction rows: 4 -> 2 (Alice's 2 associations removed, Bob's 2 remain) }
  WriteLn('After delete:');
  WriteLn('  Students:       ', CountRows('Student'));
  WriteLn('  Courses:        ', CountRows('Course'));
  WriteLn('  Junction rows:  ', CountRows('student_course'));
  WriteLn('  (Alice''s junction rows removed, but all 3 courses are preserved)');
  WriteLn;
end;

{ === Main program === }

begin
  SetupDatabase;
  try
    DemoPart1_AssociationManagement;
    DemoPart2_LoadingAssociations;
    DemoPart3_CascadeDelete;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
