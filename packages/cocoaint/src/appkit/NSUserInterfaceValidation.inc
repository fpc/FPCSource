{ Parsed from Appkit.framework NSUserInterfaceValidation.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:07 CET 2010 }


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
  NSValidatedUserInterfaceItemProtocol = objcprotocol
    function action: SEL; message 'action';
    function tag: NSInteger; message 'tag';
  end; external name 'NSValidatedUserInterfaceItem';
  
{ NSUserInterfaceValidations Protocol }
  NSUserInterfaceValidationsProtocol = objcprotocol
    function validateUserInterfaceItem(anItem: id): Boolean; message 'validateUserInterfaceItem:';
  end; external name 'NSUserInterfaceValidations';
{$endif}
{$endif}
