  
type
  TResourceHandle = Cardinal;
  HMODULE = Cardinal;
  HGLOBAL = Cardinal;
{$ifdef UNICODE}
  {roozbeh : maybe it shoud be moved after ifndef mswindows,as there is one declared in wince,base.inc}
  MAKEINTRESOURCE = pwidechar;
{$else}
  MAKEINTRESOURCE = pchar;
{$endif}  
  
{$ifndef MSWINDOWS }
{ windows has this in the windows unit. It probably would better
  fit into the classes/sysutils unit but because it is resource
  related, I put it here (FK)
}
const
  RT_CURSOR = MAKEINTRESOURCE(1);
  RT_BITMAP = MAKEINTRESOURCE(2);
  RT_ICON = MAKEINTRESOURCE(3);
  RT_MENU = MAKEINTRESOURCE(4);
  RT_DIALOG = MAKEINTRESOURCE(5);
  RT_STRING = MAKEINTRESOURCE(6);
  RT_FONTDIR = MAKEINTRESOURCE(7);
  RT_FONT = MAKEINTRESOURCE(8);
  RT_ACCELERATOR = MAKEINTRESOURCE(9);
  RT_RCDATA = MAKEINTRESOURCE(10);
  RT_MESSAGETABLE = MAKEINTRESOURCE(11);
  RT_GROUP_CURSOR = MAKEINTRESOURCE(12);
  RT_GROUP_ICON = MAKEINTRESOURCE(13);
  RT_VERSION = MAKEINTRESOURCE(16);
{$endif MSWINDOWS }
    
// Win32 API compatible Resource functions
Function HINSTANCE : HMODULE;
Function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: PChar): TResourceHandle;
Function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: AnsiString): TResourceHandle;
Function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
Function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
Function LockResource(ResData: HGLOBAL): Pointer;
Function UnlockResource(ResData: HGLOBAL): LongBool;
Function FreeResource(ResData: HGLOBAL): LongBool;

      
