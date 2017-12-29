library pas2jslib;

{$mode objfpc}
{$H+}

uses
  SysUtils, Classes, FPPJsSrcMap, Pas2jsFileCache, Pas2jsCompiler;


exports
  GetPas2JSCompiler,
  FreePas2JSCompiler,
  RunPas2JSCompiler,
  SetPas2JSReadPasCallBack,
  SetPas2JSWriteJSCallBack,
  SetPas2JSCompilerLogCallBack,
  GetPas2JSCompilerLastError;

end.

