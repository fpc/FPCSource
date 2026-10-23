{ Tests GetSymbolByAddress for an address in the program without dynlibs: module only on win32/win64, a clean failure elsewhere }
program tlibsym1c;

{$mode objfpc}{$H+}

var
  gFailed: Boolean = False;


// Reports aMessage as a failure when aCondition is false
procedure Check(aCondition: Boolean; const aMessage: string);

begin
  if not aCondition then
    begin
    Writeln('failed: ',aMessage);
    gFailed:=True;
    end;
end;


// Procedure whose address is looked up
procedure Target;

begin
end;


{$if declared(GetSymbolByAddress)}
var
  lInfo: TSymbolAddressInfo;
{$endif}

begin
{$if declared(GetSymbolByAddress)}
  lInfo.SymbolName:='x';
  lInfo.SymbolAddr:=@Target;
  lInfo.ModuleName:='x';
  lInfo.ModuleOffset:=1;
{$if defined(win32) or defined(win64)}
  Check(GetSymbolByAddress(@Target,lInfo),'GetSymbolByAddress finds the module of the program');
  Check(UpCase(Copy(lInfo.ModuleName,Length(lInfo.ModuleName)-12,13))='TLIBSYM1C.EXE','module of the program: '+lInfo.ModuleName);
  Check((lInfo.SymbolName='') and (lInfo.SymbolAddr=nil),'the program exports no symbol: '+lInfo.SymbolName);
  Check(lInfo.ModuleOffset=CodePtrUInt(@Target)-CodePtrUInt(HInstance),'module offset is the RVA in the program');
  Check(Pos('(TLIBSYM1C.EXE+$',UpCase(BackTraceStrFunc(@Target)))>0,'backtrace line shows the module and offset: '+BackTraceStrFunc(@Target));
{$else}
  Check(not GetSymbolByAddress(@Target,lInfo),'GetSymbolByAddress fails when no library manager provides symbol lookup');
  Check((lInfo.SymbolName='') and (lInfo.SymbolAddr=nil) and (lInfo.ModuleName='') and (lInfo.ModuleOffset=0),'failed lookup clears the results');
{$endif}
  Check(not GetSymbolByAddress(nil,lInfo),'GetSymbolByAddress fails for nil');
{$endif}
  Check(Pos('  $'+HexStr(@Target),BackTraceStrFunc(@Target))=1,'backtrace line starts with the address: '+BackTraceStrFunc(@Target));
  if gFailed then
    Halt(1);
  Writeln('ok');
end.
