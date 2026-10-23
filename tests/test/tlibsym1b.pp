{ %target=linux,android,darwin,win32,win64 }
{ %skiptarget=$nosharedlib }
{ %needlibrary }
{ %delfiles=tlibsym1a }

{ Tests GetSymbolByAddress and the backtrace fallback with the library manager of cthreads (unix) or the system unit (windows) }
program tlibsym1b;

{$mode objfpc}{$H+}

{$ifdef unix}
uses
  cthreads, dl;
{$endif}

type
  TCallback = procedure;
  TCallProc = procedure(aCallback: TCallback); cdecl;

var
  gFailed: Boolean = False;
  gFrames: array[0..31] of CodePointer;
  gFrameCount: SizeInt;


// Reports aMessage as a failure when aCondition is false
procedure Check(aCondition: Boolean; const aMessage: string);

begin
  if not aCondition then
    begin
    Writeln('failed: ',aMessage);
    gFailed:=True;
    end;
end;


// Returns True when aText contains aPart, ignoring case
function ContainsText(const aText, aPart: string): Boolean;

begin
  Result:=Pos(UpCase(aPart),UpCase(aText))>0;
end;


// Returns True when aText ends with aSuffix, ignoring case
function EndsWith(const aText, aSuffix: string): Boolean;

begin
  Result:=(Length(aText)>=Length(aSuffix)) and (UpCase(Copy(aText,Length(aText)-Length(aSuffix)+1,Length(aSuffix)))=UpCase(aSuffix));
end;


// Returns the directory of the program, including the trailing separator
function ProgramDir: string;

var
  i: SizeInt;

begin
  Result:=ParamStr(0);
  i:=Length(Result);
  while (i>0) and not (Result[i] in ['/','\']) do
    Dec(i);
  if i=0 then
    Result:=''
  else
    SetLength(Result,i);
end;


// Returns aValue as hexadecimal digits without leading zeroes
function TrimmedHex(aValue: PtrUInt): string;

begin
  Result:=HexStr(aValue,SizeOf(PtrUInt)*2);
  while (Length(Result)>1) and (Result[1]='0') do
    Delete(Result,1,1);
end;


// Returns the address where the module containing aAddr is loaded, nil when unknown
function ModuleBase(aAddr: CodePointer; aLib: TLibHandle): Pointer;

{$ifdef unix}
var
  lDlInfo: dl_info;
{$endif}

begin
{$ifdef windows}
  if aLib<>NilHandle then
    Result:=Pointer(aLib)
  else
    Result:=Pointer(HInstance);
{$else}
  FillChar(lDlInfo,SizeOf(lDlInfo),0);
  if dladdr(aAddr,@lDlInfo)<>0 then
    Result:=lDlInfo.dli_fbase
  else
    Result:=nil;
{$endif}
end;


// Returns True when the program file is a position dependent ELF executable
function ProgramIsElfExec: Boolean;

var
  lFile: File;
  lHeader: array[0..17] of Byte;
  lRead: LongInt;

begin
  Result:=False;
  Assign(lFile,ParamStr(0));
  FileMode:=0;
  Reset(lFile,1);
  BlockRead(lFile,lHeader,SizeOf(lHeader),lRead);
  Close(lFile);
  Result:=(lRead=SizeOf(lHeader)) and (lHeader[0]=$7F) and (lHeader[1]=Ord('E')) and (PWord(@lHeader[16])^=2);
end;


// Stores the current call stack in gFrames
procedure CaptureFrames;

begin
  gFrameCount:=CaptureBacktrace(0,Length(gFrames),@gFrames[0]);
end;


var
  lLibName: string;
  lLib: TLibHandle;
  lFunc: CodePointer;
  lCall: TCallProc;
  lInfo: TSymbolAddressInfo;
  lHeap, lLibBase: Pointer;
  lExpected: PtrUInt;
  lFound: Boolean;
  lLine: ShortString;
  i: SizeInt;

begin
{$ifdef windows}
  lLibName:='tlibsym1a.'+SharedSuffix;
{$else}
  lLibName:='libtlibsym1a.'+SharedSuffix;
{$endif}
  lLib:=LoadLibrary(ProgramDir+lLibName);
  if lLib=NilHandle then
    begin
    Writeln('cannot load ',ProgramDir+lLibName,': ',GetLoadErrorStr);
    Halt(1);
    end;
  lFunc:=GetProcedureAddress(lLib,'libsym_func');
  Check(lFunc<>nil,'libsym_func is exported');
  lCall:=TCallProc(GetProcedureAddress(lLib,'libsym_call'));
  Check(Assigned(lCall),'libsym_call is exported');
  if gFailed then
    Halt(1);

  Check(GetSymbolByAddress(lFunc,lInfo),'GetSymbolByAddress finds libsym_func');
  Check(lInfo.SymbolName='libsym_func','symbol name of libsym_func: '+lInfo.SymbolName);
  Check(lInfo.SymbolAddr=lFunc,'symbol address of libsym_func');
  Check(EndsWith(lInfo.ModuleName,lLibName),'module of libsym_func: '+lInfo.ModuleName);
  lLibBase:=ModuleBase(lFunc,lLib);
  Check(lInfo.ModuleOffset=PtrUInt(lFunc)-PtrUInt(lLibBase),'module offset of libsym_func: $'+TrimmedHex(lInfo.ModuleOffset));

  Check(GetSymbolByAddress(CodePointer(PByte(lFunc)+1),lInfo),'GetSymbolByAddress finds an address inside libsym_func');
  Check(lInfo.SymbolName='libsym_func','symbol name inside libsym_func: '+lInfo.SymbolName);
  Check(lInfo.SymbolAddr=lFunc,'symbol address inside libsym_func');

  lLine:=SysBackTraceStr(CodePointer(PByte(lFunc)+1));
  Writeln(lLine);
  Check(Pos('libsym_func+$1',lLine)>0,'SysBackTraceStr shows symbol and offset: '+lLine);
  Check(Pos('('+lLibName+'+$'+TrimmedHex(PtrUInt(lFunc)+1-PtrUInt(lLibBase))+')',lLine)>0,'SysBackTraceStr shows the module and offset: '+lLine);
  Check(Pos('  $'+HexStr(CodePointer(PByte(lFunc)+1)),lLine)=1,'SysBackTraceStr starts with the address: '+lLine);

  lHeap:=GetMem(16);
  Check(not GetSymbolByAddress(lHeap,lInfo),'GetSymbolByAddress fails for a heap address');
  Check((lInfo.SymbolName='') and (lInfo.SymbolAddr=nil) and (lInfo.ModuleName='') and (lInfo.ModuleOffset=0),'failed lookup clears the results');
  FreeMem(lHeap);
  Check(not GetSymbolByAddress(nil,lInfo),'GetSymbolByAddress fails for nil');

  Check(GetSymbolByAddress(@CaptureFrames,lInfo) or (lInfo.ModuleName=''),'lookup in the program does not fail with a module name');
  Writeln('program address: ',SysBackTraceStr(@CaptureFrames));
{$if defined(linux) or defined(android) or defined(windows)}
  if lInfo.ModuleName<>'' then
    begin
    if ProgramIsElfExec then
      lExpected:=PtrUInt(@CaptureFrames)
    else
      lExpected:=PtrUInt(@CaptureFrames)-PtrUInt(ModuleBase(@CaptureFrames,NilHandle));
    Check(lInfo.ModuleOffset=lExpected,'module offset in the program: $'+TrimmedHex(lInfo.ModuleOffset)+', expected $'+TrimmedHex(lExpected));
    end;
{$endif}

  gFrameCount:=0;
  lCall(@CaptureFrames);
  lFound:=False;
  for i:=0 to gFrameCount-1 do
    if GetSymbolByAddress(gFrames[i],lInfo) and EndsWith(lInfo.ModuleName,lLibName) then
      begin
      lFound:=True;
      lLine:=SysBackTraceStr(gFrames[i]);
      Writeln(lLine);
      Check(Pos('('+lLibName+'+$'+TrimmedHex(lInfo.ModuleOffset)+')',lLine)>0,'SysBackTraceStr of the library frame shows the module and offset: '+lLine);
      if lInfo.SymbolName<>'' then
        Check(Pos('  '+lInfo.SymbolName+'+$',lLine)>0,'SysBackTraceStr of the library frame shows the symbol: '+lLine);
      lLine:=BackTraceStrFunc(gFrames[i]);
      Check(ContainsText(lLine,lLibName) or ContainsText(lLine,'libsym_call'),'backtrace line of the library frame: '+lLine);
      end;
  Check(lFound,'a frame of the backtrace lies in the library');

  Check(UnloadLibrary(lLib),'UnloadLibrary');
  if gFailed then
    Halt(1);
  Writeln('ok');
end.
