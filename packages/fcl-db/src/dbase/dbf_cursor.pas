unit dbf_cursor;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Pascal Ganaye,Micha Nelissen and other members of the
    Free Pascal development team

    DBF cursor implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
interface

{$I dbf_common.inc}

uses
  SysUtils,
  Classes,
  dbf_pgfile,
  dbf_common;

type

//====================================================================
  TVirtualCursor = class(TObject)
  private
    FFile: TPagedFile;

  protected
    function GetPhysicalRecNo: Integer; virtual; abstract;
    function GetSequentialRecNo: Integer; virtual; abstract;
    function GetSequentialRecordCount: Integer; virtual; abstract;
    procedure SetPhysicalRecNo(RecNo: Integer); virtual; abstract;
    procedure SetSequentialRecNo(RecNo: Integer); virtual; abstract;

  public
    constructor Create(pFile: TPagedFile);
    destructor Destroy; override;

    function  RecordSize: Integer;

    function  Next: Boolean; virtual; abstract;
    function  Prev: Boolean; virtual; abstract;
    procedure First; virtual; abstract;
    procedure Last; virtual; abstract;

    property PagedFile: TPagedFile read FFile;
    property PhysicalRecNo: Integer read GetPhysicalRecNo write SetPhysicalRecNo;
    property SequentialRecNo: Integer read GetSequentialRecNo write SetSequentialRecNo;
    property SequentialRecordCount: Integer read GetSequentialRecordCount;
  end;

implementation

constructor TVirtualCursor.Create(pFile: TPagedFile);
begin
  FFile := pFile;
end;

destructor TVirtualCursor.Destroy; {override;}
begin
end;

function TVirtualCursor.RecordSize : Integer;
begin
  if FFile = nil then
    Result := 0
  else
    Result := FFile.RecordSize;
end;

end.

