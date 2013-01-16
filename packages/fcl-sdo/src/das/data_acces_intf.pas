{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements a basic SDO data access interface.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$I sdo_global.inc}
unit data_acces_intf;

interface
uses
  SysUtils, Classes, DB;

const
  s_DatabaseName = 'DatabaseName';
  s_HostName = 'HostName';
  s_Password = 'Password';
  s_UserName = 'UserName';

const
  sCONFIGURATION =  'CONFIGURATION';
  sDB_ALIAS       = 'DBALIAS';
  sSERVER_ADDRESS = 'SERVER_ADDRESS';
  sSERVER_INFO    = 'SERVER_INFO';
    
type

  EDataAccessException = class(Exception);

  TDACTransactionHandle = ^TDACTransactionStruct;
  TDACTransactionStruct = record end;

  TDataAccessInterface = class;

  TDacFactory = class
  public
    function CreateDAC(AParams : TStrings) : TDataAccessInterface;virtual;abstract;
  end;

  TSequenceValueType = Integer;
  TDataAccessInterface = class
  protected
    procedure CheckInTransaction();
  public
    class function New(const AName : string; AParams : TStrings) : TDataAccessInterface;overload;
    class function New(AParams : TStrings) : TDataAccessInterface;overload;
    class procedure RegisterFactory(const AName : string; AFactory : TDacFactory);

    function GetTransactionHanlde() : TDACTransactionHandle;virtual;abstract;
    procedure SetTransactionHanlde(const ANewX : TDACTransactionHandle);virtual;abstract;
    procedure RestoreTransactionHanlde();virtual;abstract;
    function IsIntransaction() : Boolean;virtual;abstract;
    //returns FALSE if there is a already transaction, TRUE otherwhise
    function StartTransaction() : Boolean;virtual;abstract;
    procedure RollbackTransaction(); virtual;abstract;
    procedure CommitTransaction();virtual;abstract;

    function ExecuteDataset(
      const AQuery : string;
      const AParams : array of Variant
    ) : TDataSet;overload;virtual;abstract;
    function ExecuteDataset(const AQuery : string) : TDataSet;overload;virtual;abstract;

    procedure ExecuteNonDataset(
      const AQuery : string;
      const AParams : array of Variant
    );overload;virtual;abstract;
    procedure ExecuteNonDataset(const AQuery : string);overload;virtual;abstract;
    procedure ExecuteNonDatasetSP(
      const ASPName : string;
      const AParamNames : array of string;
      const AParamValues : array of Variant
    );overload;virtual;abstract;
    procedure ExecuteNonDatasetSP(const ASPName : string);overload;virtual;abstract;

    function GetNextSequenceValue(const ASequenceName : string) : TSequenceValueType;virtual;abstract;
  end;


resourcestring
  sNO_ACTIVE_TRANSACTION = 'No active transaction.';

implementation

var
  FFactoryList : TStringList;

{ TDataAccessInterface }

procedure TDataAccessInterface.CheckInTransaction();
begin
  if not IsIntransaction() then
    raise EDataAccessException.Create(sNO_ACTIVE_TRANSACTION);
end;

class function TDataAccessInterface.New(AParams : TStrings) : TDataAccessInterface;
begin
  Assert(FFactoryList.Count > 0);
  Result := New(FFactoryList[0],AParams);
end;

class function TDataAccessInterface.New(const AName: string; AParams : TStrings): TDataAccessInterface;
var
  i : Integer;
begin
  i := FFactoryList.IndexOf(AName);
  if ( i >= 0 ) then
    Result := TDacFactory(FFactoryList.Objects[i]).CreateDAC(AParams)
  else
    raise EDataAccessException.CreateFmt('DAC not found : %s.',[AName]);
end;

class procedure TDataAccessInterface.RegisterFactory(const AName: string; AFactory: TDacFactory);
begin
  Assert(Assigned(AFactory));
  if ( FFactoryList.IndexOf(AName) = -1 ) then
    FFactoryList.AddObject(AName,AFactory);
end;

initialization
  FFactoryList := TStringList.Create();
  FFactoryList.Duplicates := dupIgnore;
  FFactoryList.Sorted := True;

finalization
  FreeAndNil(FFactoryList);
  
end.
