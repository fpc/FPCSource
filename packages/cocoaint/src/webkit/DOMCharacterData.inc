{ Parsed from Webkit.framework DOMCharacterData.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCHARACTERDATA_PAS_C}
{$define DOMCHARACTERDATA_PAS_C}

{ DOMCharacterData }
  DOMCharacterData = objcclass(DOMNode)
    
  public
    class function alloc: DOMCharacterData; message 'alloc';

    procedure setData (newValue: NSString); message 'setData:';
    function data: NSString; message 'data';
    function length: cuint; message 'length';
    function substringData_length(offset: cuint; length_: cuint): NSString; message 'substringData:length:';
    procedure appendData(data_: NSStringPointer); message 'appendData:';
    procedure insertData_data(offset: cuint; data_: NSStringPointer); message 'insertData:data:';
    procedure deleteData_length(offset: cuint; length_: cuint); message 'deleteData:length:';
    procedure replaceData_length_data(offset: cuint; length_: cuint; data_: NSStringPointer); message 'replaceData:length:data:';

    { Category: DOMCharacterDataDeprecated }
    function substringData(offset: cuint; length_: cuint): NSString; message 'substringData:offset:';
    procedure insertData(offset: cuint; data_: NSStringPointer); message 'insertData:offset:';
    procedure deleteData(offset: cuint; length_: cuint); message 'deleteData:offset:';
    procedure replaceData(offset: cuint; length_: cuint; data_: NSStringPointer); message 'replaceData:offset:length:';
  end; external;

{$endif}
{$endif}
