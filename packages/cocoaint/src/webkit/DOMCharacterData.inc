{ Parsed from WebKit.framework DOMCharacterData.h }


{$ifdef TYPES}
type
  DOMCharacterDataPtr = ^DOMCharacterData;
{$endif}

{$ifdef CLASSES}

type
  DOMCharacterData = objcclass external (DOMNode)
  public
    procedure setData(newValue: NSString); message 'setData:';
    function data: NSString; message 'data';
    function length: cuint; message 'length';
    function substringData_length (offset: cuint; length_: cuint): NSString; message 'substringData:length:'; { available in 10_5 }
    procedure appendData (data_: NSString); message 'appendData:';
    procedure insertData_data (offset: cuint; data_: NSString); message 'insertData:data:'; { available in 10_5 }
    procedure deleteData_length (offset: cuint; length_: cuint); message 'deleteData:length:'; { available in 10_5 }
    procedure replaceData_length_data (offset: cuint; length_: cuint; data_: NSString); message 'replaceData:length:data:'; { available in 10_5 }
  end;


type
  DOMCharacterDataDeprecated = objccategory external (DOMCharacterData)
    function substringData (offset: cuint): NSString; message 'substringData:'; deprecated 'in 10_4, 10_5';
    procedure insertData (offset: cuint); message 'insertData:'; deprecated 'in 10_4, 10_5';
    procedure deleteData (offset: cuint); message 'deleteData:'; deprecated 'in 10_4, 10_5';
    procedure replaceData_length (offset: cuint; data_: NSString); message 'replaceData:length:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

