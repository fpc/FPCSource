{ Parsed from Webkit.framework DOMCharacterData.h }

{$ifdef TYPES}
{$ifndef DOMCHARACTERDATA_PAS_T}
{$define DOMCHARACTERDATA_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCHARACTERDATA_PAS_R}
{$define DOMCHARACTERDATA_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCHARACTERDATA_PAS_F}
{$define DOMCHARACTERDATA_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCHARACTERDATA_PAS_S}
{$define DOMCHARACTERDATA_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCharacterData = objcclass;
  DOMCharacterDataPointer = ^DOMCharacterData;
  DOMCharacterDataPtr = DOMCharacterDataPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCHARACTERDATA_PAS_C}
{$define DOMCHARACTERDATA_PAS_C}

{ DOMCharacterData }
  DOMCharacterData = objcclass external (DOMNode)
    
  public
    procedure setData (newValue: NSString); message 'setData:';
    function data: NSString; message 'data';
    function length: cuint; message 'length';
    function substringData_length(offset: cuint; length_: cuint): NSString; message 'substringData:length:';
    procedure appendData(data_: NSString); message 'appendData:';
    procedure insertData_data(offset: cuint; data_: NSString); message 'insertData:data:';
    procedure deleteData_length(offset: cuint; length_: cuint); message 'deleteData:length:';
    procedure replaceData_length_data(offset: cuint; length_: cuint; data_: NSString); message 'replaceData:length:data:';
  end;

{ DOMCharacterDataDeprecatedCategory }
  DOMCharacterDataDeprecatedCategory = objccategory external (DOMCharacterData)
    function substringData(offset: cuint; length_: cuint): NSString; message 'substringData:offset:'; deprecated 'in_webkit_version_3 and later';
    procedure insertData(offset: cuint; data_: NSString); message 'insertData:offset:'; deprecated 'in_webkit_version_3 and later';
    procedure deleteData(offset: cuint; length_: cuint); message 'deleteData:offset:'; deprecated 'in_webkit_version_3 and later';
    procedure replaceData(offset: cuint; length_: cuint; data_: NSString); message 'replaceData:offset:length:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
