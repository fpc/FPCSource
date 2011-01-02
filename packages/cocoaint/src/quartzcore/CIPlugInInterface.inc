{ Parsed from Quartzcore.framework CIPlugInInterface.h }
{ Version: 2.1.4 - Sun Jan 2 15:08:20 CET 2011 }


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
  CIPlugInRegistrationProtocol = objcprotocol external name 'CIPlugInRegistration'
    function load(host: Pointer): Boolean; message 'load:';
  end;
{$endif}
{$endif}
