{ Parsed from Appkit.framework NSUserInterfaceValidation.h }

{$ifdef TYPES}
{$ifndef NSUSERINTERFACEVALIDATION_PAS_T}
{$define NSUSERINTERFACEVALIDATION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSUSERINTERFACEVALIDATION_PAS_R}
{$define NSUSERINTERFACEVALIDATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSUSERINTERFACEVALIDATION_PAS_F}
{$define NSUSERINTERFACEVALIDATION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSUSERINTERFACEVALIDATION_PAS_S}
{$define NSUSERINTERFACEVALIDATION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSValidatedUserInterfaceItemProtocol = objcprotocol;
  NSUserInterfaceValidationsProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSUSERINTERFACEVALIDATION_PAS_P}
{$define NSUSERINTERFACEVALIDATION_PAS_P}
  
{ NSValidatedUserInterfaceItem Protocol }
  NSValidatedUserInterfaceItemProtocol = objcprotocol external name 'NSValidatedUserInterfaceItem'
    function action: SEL; message 'action';
    function tag: NSInteger; message 'tag';
  end;
  
{ NSUserInterfaceValidations Protocol }
  NSUserInterfaceValidationsProtocol = objcprotocol external name 'NSUserInterfaceValidations'
    function validateUserInterfaceItem(anItem: NSValidatedUserInterfaceItemProtocol): Boolean; message 'validateUserInterfaceItem:';
  end;
{$endif}
{$endif}
