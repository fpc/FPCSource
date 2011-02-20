{ Parsed from Appkit.framework NSDocumentScripting.h }

{$ifdef TYPES}
{$ifndef NSDOCUMENTSCRIPTING_PAS_T}
{$define NSDOCUMENTSCRIPTING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDOCUMENTSCRIPTING_PAS_R}
{$define NSDOCUMENTSCRIPTING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDOCUMENTSCRIPTING_PAS_F}
{$define NSDOCUMENTSCRIPTING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDOCUMENTSCRIPTING_PAS_S}
{$define NSDOCUMENTSCRIPTING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSDOCUMENTSCRIPTING_PAS_C}
{$define NSDOCUMENTSCRIPTING_PAS_C}

{ NSScripting_NSDocumentCategory }
  NSScripting_NSDocumentCategory = objccategory external name 'NSScripting' (NSDocument)
    function lastComponentOfFileName: NSString; message 'lastComponentOfFileName';
    procedure setLastComponentOfFileName(str: NSString); message 'setLastComponentOfFileName:';
    function handleSaveScriptCommand(command: NSScriptCommand): id; message 'handleSaveScriptCommand:';
    function handleCloseScriptCommand(command: NSCloseCommand): id; message 'handleCloseScriptCommand:';
    function handlePrintScriptCommand(command: NSScriptCommand): id; message 'handlePrintScriptCommand:';
    function objectSpecifier: NSScriptObjectSpecifier; message 'objectSpecifier';
  end;

{$endif}
{$endif}
