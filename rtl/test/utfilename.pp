unit utfilename;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
interface

uses
  SysUtils;

implementation

uses punit, utrtl;

Function TestFuncs(testidx:integer;const res,expected: string) : Boolean;
begin
  Result:=AssertEquals('Failure at '+IntToStr(TestIdx),expected,res);
end;

Function TestFileName : String;

begin
  Result:='';
  // Default Unix
  AllowDirectorySeparators:=['/','\'];
  AllowDriveSeparators:=[];
  If not TestFuncs(1,ExtractFilePath('./:'),'./') then exit;
  If not TestFuncs(2,ExtractFileName('./:'),':') then exit;
  If not TestFuncs(3,ExtractFileDrive('./:'),'') then exit;

  If not TestFuncs(4,ExtractFilePath('C:/blah:blah'),'C:/') then exit;
  If not TestFuncs(5,ExtractFileName('C:/blah:blah'),'blah:blah') then exit;
  If not TestFuncs(6,ExtractFileDrive('C:/blah:blah'),'') then exit;

  If not TestFuncs(7,ExtractFilePath('./\'),'./\') then exit;
  If not TestFuncs(8,ExtractFileName('./\'),'') then exit;
  If not TestFuncs(9,ExtractFileDrive('./\'),'') then exit;

  If not TestFuncs(10,ExtractFilePath('./c:'),'./') then exit;
  If not TestFuncs(11,ExtractFileName('./c:'),'c:') then exit;
  If not TestFuncs(12,ExtractFileDrive('./c:'),'') then exit;

  If not TestFuncs(13,ExtractFilePath('\\server\share\file'),'\\server\share\') then exit;
  If not TestFuncs(14,ExtractFileName('\\server\share\file'),'file') then exit;
  If not TestFuncs(15,ExtractFileDrive('\\server\share\file'),'\\server\share') then exit;

  // Kylix compatibility mode
  AllowDirectorySeparators:=['/'];
  AllowDriveSeparators:=[];
  If not TestFuncs(101,ExtractFilePath('./:'),'./') then exit;
  If not TestFuncs(102,ExtractFileName('./:'),':') then exit;
  If not TestFuncs(103,ExtractFileDrive('./:'),'') then exit;

  If not TestFuncs(104,ExtractFilePath('C:/blah:blah'),'C:/') then exit;
  If not TestFuncs(105,ExtractFileName('C:/blah:blah'),'blah:blah') then exit;
  If not TestFuncs(106,ExtractFileDrive('C:/blah:blah'),'') then exit;

  If not TestFuncs(107,ExtractFilePath('./\'),'./') then exit;
  If not TestFuncs(108,ExtractFileName('./\'),'\') then exit;
  If not TestFuncs(109,ExtractFileDrive('./\'),'') then exit;

  If not TestFuncs(110,ExtractFilePath('./c:'),'./') then exit;
  If not TestFuncs(111,ExtractFileName('./c:'),'c:') then exit;
  If not TestFuncs(112,ExtractFileDrive('./c:'),'') then exit;

  If not TestFuncs(113,ExtractFilePath('\\server\share\file'),'') then exit;
  If not TestFuncs(114,ExtractFileName('\\server\share\file'),'\\server\share\file') then exit;
  If not TestFuncs(115,ExtractFileDrive('\\server\share\file'),'') then exit;

  // Default Windows/DOS/SO2
  AllowDirectorySeparators:=['/','\'];
  AllowDriveSeparators:=[':'];
  If not TestFuncs(201,ExtractFilePath('./:'),'./:') then exit;
  If not TestFuncs(202,ExtractFileName('./:'),'') then exit;
  If not TestFuncs(203,ExtractFileDrive('./:'),'') then exit;

  If not TestFuncs(204,ExtractFilePath('C:/blah:blah'),'C:/blah:') then exit;
  If not TestFuncs(205,ExtractFileName('C:/blah:blah'),'blah') then exit;
  If not TestFuncs(206,ExtractFileDrive('C:/blah:blah'),'C:') then exit;

  If not TestFuncs(207,ExtractFilePath('./\'),'./\') then exit;
  If not TestFuncs(208,ExtractFileName('./\'),'') then exit;
  If not TestFuncs(209,ExtractFileDrive('./\'),'') then exit;

  If not TestFuncs(210,ExtractFilePath('./c:'),'./c:') then exit;
  If not TestFuncs(211,ExtractFileName('./c:'),'') then exit;
  If not TestFuncs(212,ExtractFileDrive('./c:'),'') then exit;

  If not TestFuncs(213,ExtractFilePath('\\server\share\file'),'\\server\share\') then exit;
  If not TestFuncs(214,ExtractFileName('\\server\share\file'),'file') then exit;
  If not TestFuncs(215,ExtractFileDrive('\\server\share\file'),'\\server\share') then exit;

  // Windows/DOS/SO2 Delphi Compatibility
  AllowDirectorySeparators:=['\'];
  AllowDriveSeparators:=[':'];
  If not TestFuncs(301,ExtractFilePath('./:'),'./:') then exit;
  If not TestFuncs(302,ExtractFileName('./:'),'') then exit;
  If not TestFuncs(303,ExtractFileDrive('./:'),'') then exit;

  If not TestFuncs(304,ExtractFilePath('C:/blah:blah'),'C:/blah:') then exit;
  If not TestFuncs(305,ExtractFileName('C:/blah:blah'),'blah') then exit;
  If not TestFuncs(306,ExtractFileDrive('C:/blah:blah'),'C:') then exit;

  If not TestFuncs(307,ExtractFilePath('./\'),'./\') then exit;
  If not TestFuncs(308,ExtractFileName('./\'),'') then exit;
  If not TestFuncs(309,ExtractFileDrive('./\'),'') then exit;

  If not TestFuncs(310,ExtractFilePath('./c:'),'./c:') then exit;
  If not TestFuncs(311,ExtractFileName('./c:'),'') then exit;
  If not TestFuncs(312,ExtractFileDrive('./c:'),'') then exit;

  If not TestFuncs(313,ExtractFilePath('\\server\share\file'),'\\server\share\') then exit;
  If not TestFuncs(314,ExtractFileName('\\server\share\file'),'file') then exit;
  If not TestFuncs(315,ExtractFileDrive('\\server\share\file'),'\\server\share') then exit;
end;
  
begin
  SysutilsTest('TestFileName',@TestFileName);
end.

