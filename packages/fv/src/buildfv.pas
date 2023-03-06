{

  Unit to build all units of Free Vision
}
{$IFNDEF FPC_DOTTEDUNITS}
unit buildfv;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  FreeVision.Fvcommon,
  System.Objects,
  FreeVision.Drivers,
{  FreeVision.Memory,}
  FreeVision.Fvconsts,
{  System.Resources.Resource,}
  FreeVision.Views,
  FreeVision.Validate,
  FreeVision.Msgbox,
  FreeVision.Dialogs,
  FreeVision.Menus,
  FreeVision.App,
  FreeVision.Stddlg,
  FreeVision.Asciitab,
  FreeVision.Tabs,
  FreeVision.Outline,
  FreeVision.Memory,
  FreeVision.Colortxt,
  FreeVision.Statuses,
  FreeVision.Histlist,
  FreeVision.Inplong,
  FreeVision.Editors,
  FreeVision.Gadgets,
  FreeVision.Timeddlg,
  FreeVision.Time;
{$ELSE FPC_DOTTEDUNITS}
uses
  fvcommon,
  objects,
  drivers,
{  memory,}
  fvconsts,
{  resource,}
  views,
  validate,
  msgbox,
  dialogs,
  menus,
  app,
  stddlg,
  asciitab,
  tabs,
  outline,
  memory,
  colortxt,
  statuses,
  histlist,
  inplong,
  editors,
  gadgets,
  timeddlg,
  time;
{$ENDIF FPC_DOTTEDUNITS}

implementation

end.
