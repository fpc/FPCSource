{ Parsed from Quartzcore.framework CIPlugInInterface.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:07 CET 2010 }


{$ifdef TYPES}
{$ifndef CIPLUGININTERFACE_PAS_T}
{$define CIPLUGININTERFACE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CIPLUGININTERFACE_PAS_R}
{$define CIPLUGININTERFACE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CIPLUGININTERFACE_PAS_F}
{$define CIPLUGININTERFACE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CIPLUGININTERFACE_PAS_S}
{$define CIPLUGININTERFACE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CIPlugInRegistrationProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef CIPLUGININTERFACE_PAS_P}
{$define CIPLUGININTERFACE_PAS_P}
  
{ CIPlugInRegistration Protocol }
  CIPlugInRegistrationProtocol = objcprotocol
    function load(host: Pointer): Boolean; message 'load:';
  end; external name 'CIPlugInRegistration';
{$endif}
{$endif}
