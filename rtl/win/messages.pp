{$IFNDEF FPC_DOTTEDUNITS}
unit Messages;
{$ENDIF FPC_DOTTEDUNITS}


interface

{$IFDEF FPC_DOTTEDUNITS}
  uses
    WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
  uses
    windows;
{$ENDIF FPC_DOTTEDUNITS}

{$DEFINE read_interface}
{$DEFINE MESSAGESUNIT}
{$I messages.inc}

implementation

end.
