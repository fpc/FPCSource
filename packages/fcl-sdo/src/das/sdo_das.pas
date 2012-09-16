{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements basic SDO DAS definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_das;

interface
uses
  SysUtils, Classes, DB, 
  sdo, data_acces_intf;

type

  TSDODASOption = ( sdoAddProperty );
  TSDODASOptions = set of TSDODASOption;

  ESDODASException = class(ESDOException)
  end;

  ISDODAS = interface;
  ISDODASObjectHandler = interface;


  ISDODASObjectHandler = interface
    ['{A67BA4DC-05DC-44FD-9EE8-921732D9CFDB}']
    procedure UpdateDataStore(
      const ADas : ISDODAS;
      const AObject : ISDODataObject;
      const AUpdateKind : TChangeType
    );
  end;

  ISDODAS = interface
    ['{3EC60A22-2474-49D9-A9AB-DFCBE92E39C7}'] 
    function ExecuteQuery(
      const ADac : TDataAccessInterface;
      const AQuery : string;
      const AQueryParams : array of Variant;
      const ARowType : ISDOType;
      const AResList : ISDODataObjectList;
      const AOptions : TSDODASOptions
    ) : ISDODataObjectList;overload; 
    function ExecuteQuery(
      const ADac : TDataAccessInterface;
      const AQuery : string;
      const ARowType : ISDOType;
      const AResList : ISDODataObjectList;
      const AOptions : TSDODASOptions
    ) : ISDODataObjectList;overload;
    function ExecuteQuery(
      const ADac : TDataAccessInterface;
      const AQuery : string;     
      const AQueryParams : array of Variant 
    ) : ISDODataObject;overload;  
    function ExecuteQuery(
      const ADac : TDataAccessInterface;
      const AQuery : string
    ) : ISDODataObject;overload;
    function ExecuteQuery(
      const ADac          : TDataAccessInterface;
      const AQuery        : string;
      const AContainer    : ISDODataObject;
      const ADestListName : string;
      const AOptions      : TSDODASOptions
    ) : ISDODataObjectList;overload;
    procedure UpdateDataStore(
      const ADac : TDataAccessInterface;
      const AChanges : ISDOChangeSummary
    );

    function GetDataFactory() : ISDODataFactory;
    procedure RegisterObjectHandler(
      const ATargetObjectType : ISDOType;
      const AHandler : ISDODASObjectHandler
    );
    function FindObjectHandler(const ATargetObjectType : ISDOType) : ISDODASObjectHandler;
  end;

  TSDODAS_Implementor = function(AFactory : ISDODataFactory) : ISDODAS;

var
  DefaultImplementor : TSDODAS_Implementor = nil;

implementation


end.
