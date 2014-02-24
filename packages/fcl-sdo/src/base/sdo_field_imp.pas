{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements basic SDO Field definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_field_imp;

interface
uses SysUtils, Classes,
     sdo_types, sdo, sdo_type;

const
  BIT_ORDER_SET  = 1;
  BIT_ORDER_NULL = 2;
  BIT_ORDER_BUFFER = 3;

type

  ISDOField = interface
    ['{BD6E436E-0274-4B55-8346-593BA9FD5F4F}']
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    );

    function getBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBoolean;
    procedure setBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOBoolean
    );

    function getByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOByte;
    procedure setByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOByte
    );

{$IFDEF HAS_SDO_BYTES}
    function getBytes(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBytes;
    procedure setBytes(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
            AValue  : TSDOBytes
    );
{$ENDIF HAS_SDO_BYTES}

    function getCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOChar;
    procedure setCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOChar
    );

{$IFDEF HAS_SDO_CURRENCY}
    function getCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOCurrency;
    procedure setCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOCurrency
    );
{$ENDIF HAS_SDO_CURRENCY}

    function getDate(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDODate;
    procedure setDate(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDODate
    );

{$IFDEF HAS_SDO_DOUBLE}
    function getDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDODouble;
    procedure setDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDODouble
    );
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
    function getFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOFloat;
    procedure setFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOFloat
    );
{$ENDIF HAS_SDO_FLOAT}

    function getInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOInteger;
    procedure setInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOInteger
    );

    function getLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOLong;
    procedure setLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOLong
    );

    function getShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOShort;
    procedure setShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOShort
    );

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );

    function getDataObject(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : ISDODataObject;
    procedure setDataObject(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : ISDODataObject
    );

    function getChangeSummary(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : ISDOChangeSummary;
    procedure setChangeSummary(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : ISDOChangeSummary
    );

    function isSet(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : Boolean;
    procedure unset(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );
    function isNull(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : Boolean;
    procedure setNull(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );
  end;

  TSDOBaseField = class(TInterfacedObject,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; virtual;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); virtual;

    function getBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBoolean; virtual;
    procedure setBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOBoolean
    ); virtual;

    function getByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOByte; virtual;
    procedure setByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOByte
    ); virtual;

{$IFDEF HAS_SDO_BYTES}
    function getBytes(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBytes; virtual;
    procedure setBytes(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
            AValue  : TSDOBytes
    ); virtual;
{$ENDIF HAS_SDO_BYTES}

    function getCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOChar; virtual;
    procedure setCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOChar
    ); virtual;

{$IFDEF HAS_SDO_CURRENCY}
    function getCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOCurrency; virtual;
    procedure setCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOCurrency
    ); virtual;
{$ENDIF HAS_SDO_CURRENCY}

    function getDate(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDODate;virtual;
    procedure setDate(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDODate
    ); virtual;

{$IFDEF HAS_SDO_DOUBLE}
    function getDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDODouble; virtual;
    procedure setDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDODouble
    ); virtual;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
    function getFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOFloat; virtual;
    procedure setFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOFloat
    ); virtual;
{$ENDIF HAS_SDO_FLOAT}

    function getInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOInteger; virtual;
    procedure setInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOInteger
    ); virtual;

    function getLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOLong; virtual;
    procedure setLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOLong
    ); virtual;

    function getShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOShort; virtual;
    procedure setShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOShort
    ); virtual;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString; virtual;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    ); virtual;

    function getDataObject(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : ISDODataObject; virtual;
    procedure setDataObject(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : ISDODataObject
    ); virtual;

    function getChangeSummary(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : ISDOChangeSummary; virtual;
    procedure setChangeSummary(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : ISDOChangeSummary
    ); virtual;


    function isSet(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : Boolean;virtual;
    procedure unset(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );virtual;
    function isNull(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : Boolean;virtual;
    procedure setNull(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );virtual;
  end;

  TSDOBooleanField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBoolean;override;
    procedure setBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOBoolean
    );override;

    function getByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOByte;override;
    procedure setByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOByte
    );override;

    function getCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOChar; override;
    procedure setCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOChar
    ); override;

    function getInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOInteger;override;
    procedure setInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOInteger
    );override;

    function getLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOLong; override;
    procedure setLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOLong
    ); override;

    function getShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOShort; override;
    procedure setShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOShort
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

  end;

  TSDOIntegerField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBoolean;override;
    procedure setBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOBoolean
    );override;

    function getByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOByte;override;
    procedure setByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOByte
    );override;

    function getCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOChar; override;
    procedure setCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOChar
    ); override;

    function getInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOInteger;override;
    procedure setInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOInteger
    );override;

    function getLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOLong; override;
    procedure setLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOLong
    ); override;

    function getShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOShort; override;
    procedure setShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOShort
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

  end;

  TSDOBaseStringField = class(TSDOBaseField,IInterface,ISDOField)
  private
    procedure FreeBuffer(const ABuffer : TSDOFieldBuffer);
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBoolean;override;
    procedure setBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOBoolean
    );override;

    function getByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOByte;override;
    procedure setByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOByte
    );override;

    function getCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOChar; override;
    procedure setCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOChar
    ); override;

    function getInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOInteger;override;
    procedure setInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOInteger
    );override;

    function getLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOLong; override;
    procedure setLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOLong
    ); override;

    function getShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOShort; override;
    procedure setShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOShort
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

    procedure unset(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );override;
    procedure setNull(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );override;
  end;

  TSDOStringField = class(TSDOBaseStringField)
  end;

  TSDOObjectField = class(TSDOBaseField,IInterface,ISDOField)
  private
    procedure FreeBuffer(const ABuffer : TSDOFieldBuffer);
  protected
    function getDataObject(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : ISDODataObject;override;
    procedure setDataObject(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : ISDODataObject
    );override;
    procedure unset(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );override;
    procedure setNull(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );override;
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;
  end;

  TSDOChangeSummaryField = class(TSDOBaseField,IInterface,ISDOField)
  private
    procedure FreeBuffer(const ABuffer : TSDOFieldBuffer);
  protected
    function getChangeSummary(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : ISDOChangeSummary;override;
    procedure setChangeSummary(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : ISDOChangeSummary
    );override;
    procedure unset(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );override;
    procedure setNull(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );override;
  end;

  TSDOByteField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBoolean;override;
    procedure setBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOBoolean
    );override;

    function getByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOByte; override;
    procedure setByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOByte
    );override;

    function getCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOChar; override;
    procedure setCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOChar
    ); override;

    function getInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOInteger;override;
    procedure setInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOInteger
    );override;

    function getLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOLong; override;
    procedure setLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOLong
    ); override;

    function getShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOShort; override;
    procedure setShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOShort
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

  end;

{$IFDEF HAS_SDO_BYTES}
  TSDOBytesField = class(TSDOBaseField,IInterface,ISDOField)
  private
    procedure FreeBuffer(const ABuffer : TSDOFieldBuffer);
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getBytes(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBytes; override;
    procedure setBytes(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
            AValue  : TSDOBytes
    );override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

    procedure setNull(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    );override;
  end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
  TSDOCharField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBoolean;override;
    procedure setBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOBoolean
    );override;

    function getByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOByte; override;
    procedure setByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOByte
    );override;

    function getCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOChar; override;
    procedure setCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOChar
    ); override;

    function getInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOInteger;override;
    procedure setInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOInteger
    );override;

    function getLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOLong; override;
    procedure setLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOLong
    ); override;

    function getShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOShort; override;
    procedure setShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOShort
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

  end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
  TSDOCurrencyField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

    function getCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOCurrency; override;
    procedure setCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOCurrency
    ); override;
{$IFDEF HAS_SDO_DOUBLE}
    function getDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDODouble; override;
    procedure setDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDODouble
    ); override;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
    function getFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOFloat; override;
    procedure setFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOFloat
    ); override;
{$ENDIF HAS_SDO_FLOAT}
  end;
{$ENDIF HAS_SDO_CURRENCY}

  TSDODateField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getDate(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDODate; override;
    procedure setDate(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDODate
    );override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;
  end;

{$IFDEF HAS_SDO_LONG}
  TSDOLongField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBoolean;override;
    procedure setBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOBoolean
    );override;

    function getByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOByte; override;
    procedure setByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOByte
    );override;

    function getCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOChar; override;
    procedure setCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOChar
    ); override;

    function getInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOInteger;override;
    procedure setInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOInteger
    );override;

    function getLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOLong; override;
    procedure setLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOLong
    ); override;

    function getShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOShort; override;
    procedure setShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOShort
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

  end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
  TSDOShortField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOBoolean;override;
    procedure setBoolean(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOBoolean
    );override;

    function getByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOByte; override;
    procedure setByte(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOByte
    );override;

    function getCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOChar; override;
    procedure setCharacter(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOChar
    ); override;

    function getInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOInteger;override;
    procedure setInteger(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOInteger
    );override;

    function getLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOLong; override;
    procedure setLong(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOLong
    ); override;

    function getShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOShort; override;
    procedure setShort(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOShort
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

  end;
{$ENDIF HAS_SDO_SHORT}

{$IFDEF HAS_SDO_DOUBLE}
  TSDODoubleField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

    function getCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOCurrency; override;
    procedure setCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOCurrency
    ); override;

    function getDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDODouble; override;
    procedure setDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDODouble
    ); override;

    function getFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOFloat; override;
    procedure setFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOFloat
    ); override;
  end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
  TSDOFloatField = class(TSDOBaseField,IInterface,ISDOField)
  protected
    function getVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOVariant; override;
    procedure setVariant(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOVariant
    ); override;

    function getString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOString;override;
    procedure setString(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOString
    );override;

    function getCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOCurrency; override;
    procedure setCurrency(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOCurrency
    ); override;

    function getDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDODouble; override;
    procedure setDouble(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDODouble
    ); override;

    function getFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt
    ) : TSDOFloat; override;
    procedure setFloat(
      const ABuffer : TSDOFieldBuffer;
      const AOffset : PtrUInt;
      const AValue  : TSDOFloat
    ); override;
  end;
{$ENDIF HAS_SDO_FLOAT}

  function getField(const AKind : TSDOTypeKind) : ISDOField ;{$IFDEF USE_INLINE}inline;{$ENDIF}


implementation

uses
  sdo_imp_utils, sdo_date_utils, Variants;

var
  FieldMAP : array[TSDOTypeKind] of ISDOField;

function getField(const AKind : TSDOTypeKind) : ISDOField ;
begin
  Result := FieldMAP[AKind];
end;

procedure PrepareMap();
begin
  FieldMAP[BooleanType] := TSDOBooleanField.Create();
  FieldMAP[ByteType] := TSDOByteField.Create();
{$IFDEF HAS_SDO_BYTES}
  FieldMAP[BytesType] := TSDOBytesField.Create();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
  FieldMAP[CharacterType] := TSDOCharField.Create();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
  FieldMAP[CurrencyType] := TSDOCurrencyField.Create();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
  FieldMAP[DoubleType] := TSDODoubleField.Create();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
  FieldMAP[FloatType] := TSDOFloatField.Create();
{$ENDIF HAS_SDO_FLOAT}
  FieldMAP[DateTimeType] := TSDODateField.Create();
  FieldMAP[IntegerType] := TSDOIntegerField.Create();
{$IFDEF HAS_SDO_LONG}
  FieldMAP[LongType] := TSDOLongField.Create();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
  FieldMAP[ShortType] := TSDOShortField.Create();
{$ENDIF HAS_SDO_ShORT}
  FieldMAP[StringType] := TSDOStringField.Create();

  FieldMAP[ObjectType] := TSDOObjectField.Create();
  FieldMAP[ChangeSummaryType] := TSDOChangeSummaryField.Create();
end;

procedure UnprepareMap();
var
  e : TSDOTypeKind;
begin
  for e := Low(TSDOTypeKind) to High(TSDOTypeKind) do begin
    FieldMAP[e] := nil;
  end;
end;

{ TSDOBaseField }

{$WARNINGS OFF}
function TSDOBaseField.getBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOBoolean;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOByte;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getBytes(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOBytes;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getChangeSummary(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : ISDOChangeSummary;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOChar;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getDataObject(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
): ISDODataObject;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getDate(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDODate;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOInteger;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOLong;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOShort;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSDOBaseField.getString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOString;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

function TSDOBaseField.isNull(const ABuffer: TSDOFieldBuffer; const AOffset: PtrUInt): Boolean;
var
  locBuffer : PByte;
begin
  locBuffer := ABuffer;
  if ( AOffset <> 0 ) then
    Inc(locBuffer,AOffset);
  Result := IsBitON(locBuffer^,BIT_ORDER_NULL);
end;

function TSDOBaseField.isSet(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : Boolean;
var
  locBuffer : PByte;
begin
  locBuffer := ABuffer;
  if ( AOffset <> 0 ) then
    Inc(locBuffer,AOffset);
  Result := IsBitON(locBuffer^,BIT_ORDER_SET);
end;

{$WARNINGS OFF}
procedure TSDOBaseField.setBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt; const AValue: TSDOBoolean
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSDOBaseField.setByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOByte
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSDOBaseField.setBytes(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
        AValue: TSDOBytes
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSDOBaseField.setChangeSummary(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: ISDOChangeSummary
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSDOBaseField.setCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOChar
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSDOBaseField.setDataObject(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue  : ISDODataObject
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSDOBaseField.setDate(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDODate
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSDOBaseField.setInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOInteger
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSDOBaseField.setLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOLong
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

procedure TSDOBaseField.setNull(const ABuffer: TSDOFieldBuffer; const AOffset: PtrUInt);
var
  rawBuffer : PByte;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  SetBit(rawBuffer^,BIT_ORDER_SET,True);
  SetBit(rawBuffer^,BIT_ORDER_NULL,True);
end;

{$WARNINGS OFF}
procedure TSDOBaseField.setShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOShort
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSDOBaseField.setString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOString
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

procedure TSDOBaseField.unset(const ABuffer: TSDOFieldBuffer; const AOffset: PtrUInt);
var
  rawBuffer : PByte;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  SetBit(rawBuffer^,BIT_ORDER_SET,False);
end;

{$IFDEF HAS_SDO_CURRENCY}
{$WARNINGS OFF}
function TSDOBaseField.getCurrency(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOCurrency;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;

procedure TSDOBaseField.setCurrency(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOCurrency
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
{$WARNINGS OFF}
function TSDOBaseField.getDouble(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDODouble;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;

procedure TSDOBaseField.setDouble(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDODouble
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
{$WARNINGS OFF}
function TSDOBaseField.getFloat(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOFloat;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;

procedure TSDOBaseField.setFloat(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOFloat
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}
{$ENDIF HAS_SDO_FLOAT}

{ TSDOBooleanField }

function TSDOBooleanField.getBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
): TSDOBoolean;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := False;
  end else begin
    Inc(buff);
    Result := ( buff^ <> 0 );
  end;
end;

function TSDOBooleanField.getByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOByte;
begin
  Result := TSDOConvertHelper.BoolToByte(getBoolean(ABuffer,AOffset));
end;

function TSDOBooleanField.getCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOChar;
begin
  Result := TSDOConvertHelper.BoolToChar(getBoolean(ABuffer,AOffset));
end;

function TSDOBooleanField.getInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOInteger;
begin
  Result := TSDOConvertHelper.BoolToInteger(getBoolean(ABuffer,AOffset));
end;

function TSDOBooleanField.getLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOLong;
begin
  Result := TSDOConvertHelper.BoolToLong(getBoolean(ABuffer,AOffset));
end;

function TSDOBooleanField.getShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOShort;
begin
  Result := TSDOConvertHelper.BoolToShort(getBoolean(ABuffer,AOffset));
end;

function TSDOBooleanField.getString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
): TSDOString;
begin
  Result := TSDOConvertHelper.BoolToString(getBoolean(ABuffer,AOffset));
end;

function TSDOBooleanField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getBoolean(ABuffer,AOffset);
end;

procedure TSDOBooleanField.setBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOBoolean
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  if AValue then
    buff^ := 1
  else
    buff^ := 0;
end;

procedure TSDOBooleanField.setByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOByte
);
begin
  setBoolean(ABuffer,AOffset,( AValue <> 0 ));
end;

procedure TSDOBooleanField.setCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOChar
);
begin
  setBoolean(ABuffer,AOffset,TSDOConvertHelper.CharToBool(AValue));
end;

procedure TSDOBooleanField.setInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOInteger
);
begin
  setBoolean(ABuffer,AOffset,TSDOConvertHelper.IntegerToBool(AValue));
end;

procedure TSDOBooleanField.setLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOLong
);
begin
  setBoolean(ABuffer,AOffset,TSDOConvertHelper.LongToBool(AValue));
end;

procedure TSDOBooleanField.setShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOShort
);
begin
  setBoolean(ABuffer,AOffset,TSDOConvertHelper.ShortToBool(AValue));
end;

procedure TSDOBooleanField.setString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOString
);
begin
  setBoolean(ABuffer,AOffset,TSDOConvertHelper.StringToBool(AValue));
end;

procedure TSDOBooleanField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setBoolean(ABuffer,AOffset,AValue);
end;

{ TSDOIntegerField }

function TSDOIntegerField.getBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
): TSDOBoolean;
begin
  Result := TSDOConvertHelper.IntegerToBool(getInteger(ABuffer,AOffset));
end;

function TSDOIntegerField.getByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOByte;
begin
  Result := getInteger(ABuffer,AOffset);
end;

function TSDOIntegerField.getCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOChar;
begin
  Result := TSDOConvertHelper.IntegerToChar(getInteger(ABuffer,AOffset));
end;

function TSDOIntegerField.getInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOInteger;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := 0;
  end else begin
    Inc(buff);
    Result := PSDOInteger(buff)^;
  end;
end;

function TSDOIntegerField.getLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOLong;
begin
  Result := getInteger(ABuffer,AOffset);
end;

function TSDOIntegerField.getShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOShort;
begin
  Result := getInteger(ABuffer,AOffset);
end;

function TSDOIntegerField.getString(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOString;
begin
  Result := TSDOConvertHelper.IntegerToString(getInteger(ABuffer,AOffset));
end;

function TSDOIntegerField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getInteger(ABuffer,AOffset);
end;

procedure TSDOIntegerField.setBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt; const AValue: TSDOBoolean
);
begin
  setInteger(ABuffer,AOffset,TSDOConvertHelper.BoolToInteger(AValue));
end;

procedure TSDOIntegerField.setByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOByte
);
begin
  setInteger(ABuffer,AOffset,AValue);
end;

procedure TSDOIntegerField.setCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOChar
);
begin
  setInteger(ABuffer,AOffset,TSDOConvertHelper.CharToInteger(AValue));
end;

procedure TSDOIntegerField.setInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt; const AValue: TSDOInteger
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  PSDOInteger(buff)^ := AValue;
end;

procedure TSDOIntegerField.setLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOLong
);
begin
  setInteger(ABuffer,AOffset,AValue);
end;

procedure TSDOIntegerField.setShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOShort
);
begin
  setInteger(ABuffer,AOffset,AValue);
end;

procedure TSDOIntegerField.setString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt; const AValue: TSDOString
);
begin
  setInteger(ABuffer,AOffset,TSDOConvertHelper.StringToInteger(AValue));
end;

procedure TSDOIntegerField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setInteger(ABuffer,AOffset,AValue);
end;

{ TSDOBaseStringField }

procedure TSDOBaseStringField.FreeBuffer(const ABuffer : TSDOFieldBuffer);
var
  rawBuffer : PByte;
  strBuffer : PPSDOString;
begin
  rawBuffer := PByte(ABuffer);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    Inc(rawBuffer);
    strBuffer := PPSDOString(rawBuffer);
    SetLength(strBuffer^^,0);
    FreeMem(strBuffer^,SizeOf(PSDOString));
    strBuffer^ := nil;
    Dec(rawBuffer);
    SetBit(rawBuffer^,BIT_ORDER_BUFFER,False);
  end;
end;

function TSDOBaseStringField.getBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOBoolean;
begin
  Result := TSDOConvertHelper.StringToBool(getString(ABuffer,AOffset));
end;

function TSDOBaseStringField.getByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOByte;
begin
  Result := TSDOConvertHelper.StringToByte(getString(ABuffer,AOffset));
end;

function TSDOBaseStringField.getCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOChar;
begin
  Result := TSDOConvertHelper.StringToChar(getString(ABuffer,AOffset));
end;

function TSDOBaseStringField.getInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOInteger;
begin
  Result := TSDOConvertHelper.StringToInteger(getString(ABuffer,AOffset));
end;

function TSDOBaseStringField.getLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOLong;
begin
  Result := TSDOConvertHelper.StringToLong(getString(ABuffer,AOffset));
end;

function TSDOBaseStringField.getShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOShort;
begin
  Result := TSDOConvertHelper.StringToShort(getString(ABuffer,AOffset));
end;

function TSDOBaseStringField.getString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOString;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := '';
  end else begin
    if IsBitON(buff^,BIT_ORDER_BUFFER) then begin
      Inc(buff);
      Result := PPSDOString(buff)^^
    end else begin
      Result := '';
    end;
  end;
end;

function TSDOBaseStringField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getString(ABuffer,AOffset);
end;

procedure TSDOBaseStringField.setBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOBoolean
);
begin
  setString(ABuffer,AOffset,TSDOConvertHelper.BoolToString(AValue));
end;

procedure TSDOBaseStringField.setByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOByte
);
begin
  setString(ABuffer,AOffset,TSDOConvertHelper.ByteToString(AValue));
end;

procedure TSDOBaseStringField.setCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOChar
);
begin
  setString(ABuffer,AOffset,AValue);
end;

procedure TSDOBaseStringField.setInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt; const AValue: TSDOInteger
);
begin
  setString(ABuffer,AOffset,TSDOConvertHelper.IntegerToString(AValue));
end;

procedure TSDOBaseStringField.setLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOLong
);
begin
  setString(ABuffer,AOffset,TSDOConvertHelper.LongToString(AValue));
end;

procedure TSDOBaseStringField.setNull(const ABuffer: TSDOFieldBuffer; const AOffset: PtrUInt);
var
  rawBuffer : PByte;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    FreeBuffer(rawBuffer);
  end;
  inherited setNull(ABuffer,AOffset);
end;

procedure TSDOBaseStringField.setShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOShort
);
begin
  setString(ABuffer,AOffset,TSDOConvertHelper.ShortToString(AValue));
end;

procedure TSDOBaseStringField.setString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt; const AValue: TSDOString
);
var
  rawBuffer : PByte;
  strBuffer : PPSDOString;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  SetBit(rawBuffer^,BIT_ORDER_SET,True);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    SetBit(rawBuffer^,BIT_ORDER_NULL,False);
    Inc(rawBuffer);
    strBuffer := PPSDOString(rawBuffer);
  end else begin;
    SetBit(rawBuffer^,BIT_ORDER_NULL,False);
    SetBit(rawBuffer^,BIT_ORDER_BUFFER,True);
    Inc(rawBuffer);
    strBuffer := PPSDOString(rawBuffer);
    GetMem(strBuffer^,SizeOf(PSDOString));
    FillChar(strBuffer^^,SizeOf(TSDOString),#0);
  end;
  strBuffer^^ := AValue;
end;

procedure TSDOBaseStringField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setString(ABuffer,AOffset,AValue);
end;

procedure TSDOBaseStringField.unset(const ABuffer: TSDOFieldBuffer; const AOffset: PtrUInt);
//var
  //rawBuffer : PByte;
begin
  {rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  do not free the buffer, in case of a default value!
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    FreeBuffer(rawBuffer);
  end;}
  inherited unset(ABuffer,AOffset);
end;


{ TSDOObjectField }

procedure TSDOObjectField.FreeBuffer(const ABuffer: TSDOFieldBuffer);
var
  rawBuffer : PByte;
  objBuffer : PPSDODataObject;
begin
  rawBuffer := PByte(ABuffer);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    Inc(rawBuffer);
    objBuffer := PPSDODataObject(rawBuffer);
    objBuffer^^ := nil;
    FreeMem(objBuffer^,SizeOf(PSDODataObject));
    objBuffer^ := nil;
    Dec(rawBuffer);
    SetBit(rawBuffer^,BIT_ORDER_BUFFER,False);
  end;
end;

function TSDOObjectField.getDataObject(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : ISDODataObject;
var
  buff : PByte;
  objBuffer : PPSDODataObject;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := nil;
  end else begin
    if IsBitON(buff^,BIT_ORDER_BUFFER) then begin
      Inc(buff);
      objBuffer := PPSDODataObject(buff);
      Result := objBuffer^^;
    end else begin
      Result := nil;
    end;
  end;
end;

function TSDOObjectField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getDataObject(ABuffer,AOffset);
end;

procedure TSDOObjectField.setDataObject(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue  : ISDODataObject
);
var
  rawBuffer : PByte;
  objBuffer : PPSDODataObject;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    SetBit(rawBuffer^,BIT_ORDER_NULL,False);
    Inc(rawBuffer);
    objBuffer := PPSDODataObject(rawBuffer);
  end else begin;
    SetBit(rawBuffer^,BIT_ORDER_SET,True);
    SetBit(rawBuffer^,BIT_ORDER_BUFFER,True);
    SetBit(rawBuffer^,BIT_ORDER_NULL,False);
    Inc(rawBuffer);
    objBuffer := PPSDODataObject(rawBuffer);
    GetMem(objBuffer^,SizeOf(PSDODataObject));
    FillChar(objBuffer^^,SizeOf(ISDODataObject),#0);
  end;
  objBuffer^^ := AValue;
end;

procedure TSDOObjectField.setVariant(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue  : TSDOVariant
);
var
  x : IInterface;
  obj : ISDODataObject;
begin
  if VarIsNull(AValue) then begin
    setNull(ABuffer,AOffset);
    exit;
  end;
  if not VarIsType(AValue,varUnknown) then
    raise ESDOInvalidConversionException.Create(ClassName);
  x := AValue;
  if (x = nil) then begin
    obj := nil;
  end else begin
    if not Supports(x,ISDODataObject,obj) then
      raise ESDOInvalidConversionException.Create(ClassName);
  end;
  setDataObject(ABuffer,AOffset,obj);
end;

procedure TSDOObjectField.setNull(const ABuffer: TSDOFieldBuffer; const AOffset: PtrUInt);
var
  rawBuffer : PByte;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    FreeBuffer(rawBuffer);
  end;
  inherited setNull(ABuffer,AOffset);
end;

procedure TSDOObjectField.unset(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
);
var
  rawBuffer : PByte;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    FreeBuffer(rawBuffer);
  end;
  inherited unset(ABuffer,AOffset);
end;

{ TSDOChangeSummaryField }

procedure TSDOChangeSummaryField.FreeBuffer(const ABuffer: TSDOFieldBuffer);
var
  rawBuffer : PByte;
  objBuffer : PPSDOChangeSummary;
begin
  rawBuffer := PByte(ABuffer);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    Inc(rawBuffer);
    objBuffer := PPSDOChangeSummary(rawBuffer);
    objBuffer^^ := nil;
    FreeMem(objBuffer^,SizeOf(PPSDOChangeSummary));
    objBuffer^ := nil;
    Dec(rawBuffer);
    SetBit(rawBuffer^,BIT_ORDER_BUFFER,False);
  end;
end;

function TSDOChangeSummaryField.getChangeSummary(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
): ISDOChangeSummary;
var
  buff : PByte;
  objBuffer : PPSDOChangeSummary;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := nil;
  end else begin
    if IsBitON(buff^,BIT_ORDER_BUFFER) then begin
      Inc(buff);
      objBuffer := PPSDOChangeSummary(buff);
      Result := objBuffer^^;
    end else begin
      Result := nil;
    end;
  end;
end;

procedure TSDOChangeSummaryField.setChangeSummary(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: ISDOChangeSummary
);
var
  rawBuffer : PByte;
  objBuffer : PPSDOChangeSummary;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    SetBit(rawBuffer^,BIT_ORDER_NULL,False);
    Inc(rawBuffer);
    objBuffer := PPSDOChangeSummary(rawBuffer);
  end else begin;
    SetBit(rawBuffer^,BIT_ORDER_SET,True);
    SetBit(rawBuffer^,BIT_ORDER_BUFFER,True);
    SetBit(rawBuffer^,BIT_ORDER_NULL,False);
    Inc(rawBuffer);
    objBuffer := PPSDOChangeSummary(rawBuffer);
    GetMem(objBuffer^,SizeOf(PSDOChangeSummary));
    FillChar(objBuffer^^,SizeOf(ISDOChangeSummary),#0);
  end;
  objBuffer^^ := AValue;
end;

procedure TSDOChangeSummaryField.setNull(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
);
var
  rawBuffer : PByte;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    FreeBuffer(rawBuffer);
  end;
  inherited setNull(ABuffer,AOffset);
end;

procedure TSDOChangeSummaryField.unset(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
);
var
  rawBuffer : PByte;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    FreeBuffer(rawBuffer);
  end;
  inherited unset(ABuffer,AOffset);
end;

{ TSDOByteField }

function TSDOByteField.getBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOBoolean;
begin
  Result := TSDOConvertHelper.ByteToBool(getByte(ABuffer,AOffset));
end;

function TSDOByteField.getByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
): TSDOByte;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := 0;
  end else begin
    Inc(buff);
    Result := buff^;
  end;
end;

function TSDOByteField.getCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOChar;
begin
  Result := TSDOConvertHelper.ByteToChar(getByte(ABuffer,AOffset));
end;

function TSDOByteField.getInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOInteger;
begin
  Result := getByte(ABuffer,AOffset);
end;

function TSDOByteField.getLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOLong;
begin
  Result := getByte(ABuffer,AOffset);
end;

function TSDOByteField.getShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOShort;
begin
  Result := getByte(ABuffer,AOffset);
end;

function TSDOByteField.getString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOString;
begin
  Result := TSDOConvertHelper.ByteToString(getByte(ABuffer,AOffset));
end;

function TSDOByteField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getByte(ABuffer,AOffset);
end;

procedure TSDOByteField.setBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOBoolean
);
begin
  setByte(ABuffer,AOffset,TSDOConvertHelper.BoolToByte(AValue));
end;

procedure TSDOByteField.setByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOByte
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  buff^ := AValue;
end;

procedure TSDOByteField.setCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOChar
);
begin
  setByte(ABuffer,AOffset,TSDOConvertHelper.CharToByte(AValue));
end;

procedure TSDOByteField.setInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOInteger
);
begin
  setByte(ABuffer,AOffset,AValue);
end;

procedure TSDOByteField.setLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOLong
);
begin
  setByte(ABuffer,AOffset,AValue);
end;

procedure TSDOByteField.setShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOShort
);
begin
  setByte(ABuffer,AOffset,AValue);
end;

procedure TSDOByteField.setString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOString
);
begin
  setByte(ABuffer,AOffset,TSDOConvertHelper.StringToByte(AValue));
end;

procedure TSDOByteField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setByte(ABuffer,AOffset,AValue);
end;

{ TSDODateField }

function TSDODateField.getDate(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDODate;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := ZERO_DATE;
  end else begin
    Inc(buff);
    Result := PSDODate(buff)^;
  end;
end;

function TSDODateField.getString(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOString;
begin
  Result := TSDOConvertHelper.DateToString(getDate(ABuffer,AOffset));
end;

function TSDODateField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := NormalizeToUTC(getDate(ABuffer,AOffset));
end;

procedure TSDODateField.setDate(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDODate
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  PSDODate(buff)^ := AValue;
end;

procedure TSDODateField.setString(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOString
);
begin
  setDate(ABuffer,AOffset,TSDOConvertHelper.StringToDate(AValue));
end;

{$IFDEF HAS_SDO_CHAR}
procedure TSDODateField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setDate(ABuffer,AOffset,DateTimeToDateTimeRec(VarToDateTime(AValue)));
end;

{ TSDOCharField }

function TSDOCharField.getBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOBoolean;
begin
  Result := TSDOConvertHelper.CharToBool(getCharacter(ABuffer,AOffset));
end;

function TSDOCharField.getByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOByte;
begin
  Result := TSDOConvertHelper.CharToByte(getCharacter(ABuffer,AOffset));
end;

function TSDOCharField.getCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOChar;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := #0;
  end else begin
    Inc(buff);
    Result := PSDOChar(buff)^;
  end;
end;

function TSDOCharField.getInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOInteger;
begin
  Result := TSDOConvertHelper.CharToInteger(getCharacter(ABuffer,AOffset));
end;

function TSDOCharField.getLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOLong;
begin
  Result := TSDOConvertHelper.CharToLong(getCharacter(ABuffer,AOffset));
end;

function TSDOCharField.getShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOShort;
begin
  Result := TSDOConvertHelper.CharToShort(getCharacter(ABuffer,AOffset));
end;

function TSDOCharField.getString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOString;
begin
  Result := getCharacter(ABuffer,AOffset);
end;

function TSDOCharField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getCharacter(ABuffer,AOffset);
end;

procedure TSDOCharField.setBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOBoolean
);
begin
  setCharacter(ABuffer,AOffset,TSDOConvertHelper.BoolToChar(AValue));
end;

procedure TSDOCharField.setByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOByte
);
begin
  setCharacter(ABuffer,AOffset,TSDOConvertHelper.ByteToChar(AValue));
end;

procedure TSDOCharField.setCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOChar
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  PSDOChar(buff)^ := AValue;
end;

procedure TSDOCharField.setInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOInteger
);
begin
  setCharacter(ABuffer,AOffset,TSDOConvertHelper.IntegerToChar(AValue));
end;

procedure TSDOCharField.setLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOLong
);
begin
  setCharacter(ABuffer,AOffset,TSDOConvertHelper.LongToChar(AValue));
end;

procedure TSDOCharField.setShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOShort);
begin
  setCharacter(ABuffer,AOffset,TSDOConvertHelper.ShortToChar(AValue));
end;

procedure TSDOCharField.setString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOString
);
begin
  setCharacter(ABuffer,AOffset,TSDOConvertHelper.StringToChar(AValue));
end;
{$ENDIF HAS_SDO_CHAR}


{$IFDEF HAS_SDO_CURRENCY}
function TSDOCurrencyField.getString(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOString;
begin
  Result := TSDOConvertHelper.CurrencyToString(getCurrency(ABuffer,AOffset));
end;

procedure TSDOCurrencyField.setString(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOString
);
begin
  setCurrency(ABuffer,AOffset,TSDOConvertHelper.StringToCurrency(AValue));
end;

function TSDOCurrencyField.getCurrency(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOCurrency;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := 0;
  end else begin
    Inc(buff);
    Result := PSDOCurrency(buff)^;
  end;
end;

procedure TSDOCurrencyField.setCurrency(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOCurrency
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  PSDOCurrency(buff)^ := AValue;
end;

function TSDOCurrencyField.getDouble(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDODouble;
begin
  Result := getCurrency(ABuffer,AOffset);
end;

procedure TSDOCurrencyField.setDouble(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDODouble
);
begin
  setCurrency(ABuffer,AOffset,AValue);
end;

function TSDOCurrencyField.getFloat(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOFloat;
begin
  Result := getCurrency(ABuffer,AOffset);
end;

procedure TSDOCurrencyField.setFloat(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOFloat
);
begin
  setCurrency(ABuffer,AOffset,AValue);
end;
{$ENDIF HAS_SDO_CURRENCY}


{$IFDEF HAS_SDO_LONG}
procedure TSDOCharField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then begin
    setNull(ABuffer,AOffset)
  end else begin
    setCharacter(ABuffer,AOffset,AValue);
  end;
end;

{ TSDOLongField }

function TSDOLongField.getBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOBoolean;
begin
  Result := TSDOConvertHelper.LongToBool(getLong(ABuffer,AOffset));
end;

function TSDOLongField.getByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOByte;
begin
  Result := getLong(ABuffer,AOffset);
end;

function TSDOLongField.getCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOChar;
begin
  Result := TSDOConvertHelper.LongToChar(getLong(ABuffer,AOffset));
end;

function TSDOLongField.getInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOInteger;
begin
  Result := getLong(ABuffer,AOffset);
end;

function TSDOLongField.getLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOLong;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := 0;
  end else begin
    Inc(buff);
    Result := PSDOLong(buff)^;
  end;
end;

function TSDOLongField.getShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOShort;
begin
  Result := getLong(ABuffer,AOffset);
end;

function TSDOLongField.getString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOString;
begin
  Result := TSDOConvertHelper.LongToString(getLong(ABuffer,AOffset));
end;

function TSDOLongField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getLong(ABuffer,AOffset);
end;

procedure TSDOLongField.setBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOBoolean
);
begin
  setLong(ABuffer,AOffset,TSDOConvertHelper.BoolToLong(AValue));
end;

procedure TSDOLongField.setByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOByte
);
begin
  setLong(ABuffer,AOffset,AValue);
end;

procedure TSDOLongField.setCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOChar
);
begin
  setLong(ABuffer,AOffset,TSDOConvertHelper.CharToLong(AValue));
end;

procedure TSDOLongField.setInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOInteger
);
begin
  setLong(ABuffer,AOffset,AValue);
end;

procedure TSDOLongField.setLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOLong
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  PSDOLong(buff)^ := AValue;
end;

procedure TSDOLongField.setShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOShort);
begin
  setLong(ABuffer,AOffset,AValue);
end;

procedure TSDOLongField.setString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOString
);
begin
  setLong(ABuffer,AOffset,TSDOConvertHelper.StringToLong(AValue));
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOLongField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setLong(ABuffer,AOffset,AValue);
end;

{ TSDOShortField }

function TSDOShortField.getBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOBoolean;
begin
  Result := TSDOConvertHelper.ShortToBool(getShort(ABuffer,AOffset));
end;

function TSDOShortField.getByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOByte;
begin
  Result := getShort(ABuffer,AOffset);
end;

function TSDOShortField.getCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOChar;
begin
  Result := TSDOConvertHelper.ShortToChar(getShort(ABuffer,AOffset));
end;

function TSDOShortField.getInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOInteger;
begin
  Result := getShort(ABuffer,AOffset);
end;

function TSDOShortField.getLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOLong;
begin
 Result := getShort(ABuffer,AOffset);
end;

function TSDOShortField.getShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOShort;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := 0;
  end else begin
    Inc(buff);
    Result := PSDOShort(buff)^;
  end;
end;

function TSDOShortField.getString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOString;
begin
  Result := TSDOConvertHelper.ShortToString(getShort(ABuffer,AOffset));
end;

function TSDOShortField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getShort(ABuffer,AOffset);
end;

procedure TSDOShortField.setBoolean(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOBoolean
);
begin
  setShort(ABuffer,AOffset,TSDOConvertHelper.BooltoShort(AValue));
end;

procedure TSDOShortField.setByte(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOByte
);
begin
  setShort(ABuffer,AOffset,AValue);
end;

procedure TSDOShortField.setCharacter(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOChar
);
begin
  setShort(ABuffer,AOffset,TSDOConvertHelper.ChartoShort(AValue));
end;

procedure TSDOShortField.setInteger(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOInteger
);
begin
  setShort(ABuffer,AOffset,AValue);
end;

procedure TSDOShortField.setLong(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOLong
);
begin
  setShort(ABuffer,AOffset,AValue);
end;

procedure TSDOShortField.setShort(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOShort
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  PSDOShort(buff)^ := AValue;
end;

procedure TSDOShortField.setString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOString
);
begin
  setShort(ABuffer,AOffset,TSDOConvertHelper.StringtoShort(AValue));
end;
{$ENDIF HAS_SDO_SHORT}


{$IFDEF HAS_SDO_BYTES}
procedure TSDOShortField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setShort(ABuffer,AOffset,AValue);
end;

{ TSDOBytesField }

procedure TSDOBytesField.FreeBuffer(const ABuffer: TSDOFieldBuffer);
var
  rawBuffer : PByte;
  valBuffer : PPSDOBytes;
begin
  rawBuffer := PByte(ABuffer);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    Inc(rawBuffer);
    valBuffer := PPSDOBytes(rawBuffer);
    SetLength(valBuffer^^,0);
    FreeMem(valBuffer^,SizeOf(PPSDOBytes));
    valBuffer^ := nil;
    Dec(rawBuffer);
    SetBit(rawBuffer^,BIT_ORDER_BUFFER,False);
  end;
end;

function TSDOBytesField.getBytes(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOBytes;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := nil;
  end else begin
    if IsBitON(buff^,BIT_ORDER_BUFFER) then begin
      Inc(buff);
      Result := Copy(PPSDOBytes(buff)^^);
    end else begin
      Result := nil;
    end;
  end;
end;

function TSDOBytesField.getString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOString;
begin
  Result := TSDOConvertHelper.BytesToString(getBytes(ABuffer,AOffset));
end;

function TSDOBytesField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getBytes(ABuffer,AOffset);
end;

procedure TSDOBytesField.setBytes(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
        AValue: TSDOBytes
);
var
  rawBuffer : PByte;
  valBuffer : PPSDOBytes;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  SetBit(rawBuffer^,BIT_ORDER_SET,True);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    SetBit(rawBuffer^,BIT_ORDER_NULL,False);
    Inc(rawBuffer);
    valBuffer := PPSDOBytes(rawBuffer);
  end else begin;
    SetBit(rawBuffer^,BIT_ORDER_NULL,False);
    SetBit(rawBuffer^,BIT_ORDER_BUFFER,True);
    Inc(rawBuffer);
    valBuffer := PPSDOBytes(rawBuffer);
    GetMem(valBuffer^,SizeOf(PPSDOBytes));
    FillChar(valBuffer^^,SizeOf(TSDOBytes),#0);
  end;
  valBuffer^^ := Copy(AValue);
end;

procedure TSDOBytesField.setNull(const ABuffer: TSDOFieldBuffer; const AOffset: PtrUInt);
var
  rawBuffer : PByte;
begin
  rawBuffer := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(rawBuffer,AOffset);
  if IsBitON(rawBuffer^,BIT_ORDER_BUFFER) then begin
    FreeBuffer(rawBuffer);
  end;
  inherited setNull(ABuffer,AOffset);
end;

procedure TSDOBytesField.setString(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOString
);
begin
  setBytes(ABuffer,AOffset,TSDOConvertHelper.StringToBytes(AValue));
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_DOUBLE}
function TSDODoubleField.getString(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOString;
begin
  Result := TSDOConvertHelper.FloatToString(getDouble(ABuffer,AOffset));
end;

procedure TSDODoubleField.setString(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOString
);
begin
  setDouble(ABuffer,AOffset,TSDOConvertHelper.StringToFloat(AValue));
end;

function TSDODoubleField.getDouble(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDODouble;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := 0;
  end else begin
    Inc(buff);
    Result := PSDODouble(buff)^;
  end;
end;

procedure TSDODoubleField.setDouble(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDODouble
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  PSDODouble(buff)^ := AValue;
end;

function TSDODoubleField.getCurrency(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOCurrency;
begin
  Result := getDouble(ABuffer,AOffset);
end;

procedure TSDODoubleField.setCurrency(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOCurrency
);
begin
  setDouble(ABuffer,AOffset,AValue);
end;

function TSDODoubleField.getFloat(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOFloat;
begin
  Result := getDouble(ABuffer,AOffset);
end;

procedure TSDODoubleField.setFloat(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOFloat
);
begin
  setDouble(ABuffer,AOffset,AValue);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
function TSDOFloatField.getString(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOString;
begin
  Result := TSDOConvertHelper.FloatToString(getFloat(ABuffer,AOffset));
end;

procedure TSDOFloatField.setString(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOString
);
begin
  setFloat(ABuffer,AOffset,TSDOConvertHelper.StringToFloat(AValue));
end;

function TSDOFloatField.getFloat(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOFloat;
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  if IsBitON(buff^,BIT_ORDER_NULL) then begin
    Result := 0;
  end else begin
    Inc(buff);
    Result := PSDOFloat(buff)^;
  end;
end;

procedure TSDOFloatField.setFloat(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOFloat
);
var
  buff : PByte;
begin
  buff := PByte(ABuffer);
  if ( AOffset <> 0 ) then
    Inc(buff,AOffset);
  SetBit(buff^,BIT_ORDER_SET,True);
  SetBit(buff^,BIT_ORDER_NULL,False);
  Inc(buff);
  PSDOFloat(buff)^ := AValue;
end;

function TSDOFloatField.getCurrency(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDOCurrency;
begin
  Result := getFloat(ABuffer,AOffset);
end;

procedure TSDOFloatField.setCurrency(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDOCurrency
);
begin
  setFloat(ABuffer,AOffset,AValue);
end;

function TSDOFloatField.getDouble(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt
) : TSDODouble;
begin
  Result := getFloat(ABuffer,AOffset);
end;

procedure TSDOFloatField.setDouble(
  const ABuffer : TSDOFieldBuffer;
  const AOffset : PtrUInt;
  const AValue : TSDODouble
);
begin
  setFloat(ABuffer,AOffset,AValue);
end;
{$ENDIF HAS_SDO_FLOAT}

{$WARNINGS OFF}
function TSDOBaseField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;

procedure TSDOBaseField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  raise ESDOInvalidConversionException.Create(ClassName);
end;
{$WARNINGS ON}

procedure TSDOBytesField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
var
  tempValue : TSDOBytes;
  i, c : Integer;
begin
  if VarIsNull(AValue) or VarIsEmpty(AValue) then begin
    setNull(ABuffer,AOffset)
  end else begin
    if not VarIsArray(AValue) then
      raise ESDOInvalidConversionException.Create(ClassName);
    c := VarArrayHighBound(AValue,1) - VarArrayLowBound(AValue,1) + 1;
    if (c > 0) then begin
      SetLength(tempValue,c);
      for i := VarArrayLowBound(AValue,1) to VarArrayHighBound(AValue,1) do
        tempValue[i] := AValue[i];
    end else begin
      tempValue := nil;
    end;
    setBytes(ABuffer,AOffset,tempValue);
  end;
end;

function TSDOCurrencyField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getCurrency(ABuffer,AOffset);
end;

procedure TSDOCurrencyField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setCurrency(ABuffer,AOffset,AValue);
end;

function TSDODoubleField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getDouble(ABuffer,AOffset);
end;

procedure TSDODoubleField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
 if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setDouble(ABuffer,AOffset,AValue);
end;

function TSDOFloatField.getVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt
) : TSDOVariant;
begin
  if isNull(ABuffer,AOffset) then
    Result := Null
  else
    Result := getFloat(ABuffer,AOffset);
end;

procedure TSDOFloatField.setVariant(
  const ABuffer: TSDOFieldBuffer;
  const AOffset: PtrUInt;
  const AValue: TSDOVariant
);
begin
  if VarIsNull(AValue) then
    setNull(ABuffer,AOffset)
  else
    setFloat(ABuffer,AOffset,AValue);
end;

initialization
  PrepareMap();

finalization
  UnprepareMap();

end.

