{$ifdef NDS_INTERFACE}
//const
//  CONSOLE_USE_COLOR255 = 16;

type
  ConsolePrint = function(con: pointer; c: cchar): pcbool;

  ConsoleFont = record
    gfx: pcuint16;
    pal: pcuint16;
    numColors: cuint16;
      
    bpp: cuint8;
    
    asciiOffset: cuint16;
    numChars: cuint16;
    convertSingleColor: cbool;
  end;
  PConsoleFont = ^ConsoleFont;
  TConsoleFont = ConsoleFont;
  
  PrintConsole = record
    font: ConsoleFont;
    
    fontBgMap: pcuint16;
    fontBgGfx: pcuint16;
    
    mapBase: cuint8;
    gfxBase: cuint8;
    
    bgLayer: cuint8;
    bgId: cint;
    
    cursorX: cint;
    cursorY: cint;
    
    prevCursorX: cint;
    prevCursorY: cint;
    
    consoleWidth: cint;
    consoleHeight: cint;

    windowX: cint;
    windowY: cint;
    windowWidth: cint;
    windowHeight: cint;
    
    tabSize: cint;
    
    fontCharOffset: cuint16;
    
    
    fontCurPal: cuint16;
    
    PrintChar: ConsolePrint;
    
    consoleInitialised: cbool;
    loadGraphics: cbool;
  end;
  PPrintConsole = ^PrintConsole;
  TPrintConsole = PrintConsole;
  


type
  DebugDevice = integer;
const
  DebugDevice_NULL: DebugDevice = $0;
  DebugDevice_NOCASH: DebugDevice = $1;
  DebugDevice_CONSOLE: DebugDevice = $02;
  

{$endif NDS_INTERFACE}

{$ifdef NDS_INTERFACE}

procedure consoleSetFont(console: PPrintConsole; font: PConsoleFont); cdecl; external;
procedure consoleSetWindow(console: PPrintConsole; x, y, width, height: cint); cdecl; external;
function consoleGetDefault(): PPrintConsole; cdecl; external;
function consoleSelect(console: PPrintConsole): PPrintConsole; cdecl; external;
function consoleInit(console: PPrintConsole; layer: cint; atype: BgType; size: BgSize; mapBase, 
  tileBase: cint; mainDisplay: cbool; loadGraphics: cbool): pPrintConsole; cdecl; external;
function consoleDemoInit(): PPrintConsole; cdecl; external;
procedure consoleClear(); cdecl; external;
procedure consoleDebugInit(device: DebugDevice); cdecl; external;

{$endif NDS_INTERFACE}
