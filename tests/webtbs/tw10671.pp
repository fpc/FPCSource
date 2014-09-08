program tw10671;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  SysUtils;

const
  VER_MAJ = 10000;
  VER_MIN = 100;
  VER_REL = 1;

const
  MY_VERSION = 020200;

{$IF MY_VERSION >= ((VER_MAJ*2) + (VER_MIN*1) + (VER_REL*0))}
  {$MESSAGE Info 'Arithmetic in compile-time expressions works!'}
{$ELSE}
  {$Message Error 'Arithmetic in compile-time expressions fails!'}
{$IFEND}

begin
end.
