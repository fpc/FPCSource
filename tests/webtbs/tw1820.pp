{ %version=1.1 }
{ %TARGET=win32 }
unit tw1820;
interface

{$mode Delphi}

Uses Windows;

type
  IEnumWorkItems = interface (IUnknown)
      ['{148BD528-A2AB-11CE-B11F-00AA00530503}']
  end;


const
  IEnumTasks = IEnumWorkItems;

implementation

end.
