{
  $Id$

  GL unit creation tool helpers
  (c) 1999 Sebastian Guenther, sg@freepascal.org
}

{$MODE objfpc}
{$H+}

unit buildgl;

interface
uses SysUtils, Classes;

type

  TDefReader = class
  protected
    FInterfaceBlock, FProcs: TStringList;
  public
    constructor Create(const Filename: String);
    property InterfaceBlock: TStringList read FInterfaceBlock;
    property Procs: TStringList read FProcs;
  end;


implementation


constructor TDefReader.Create(const Filename: String);
type
  TCurState = (stateNothing, stateCopyInterface, stateProcs);
var
  f: Text;
  s: String;
  state: TCurState;
begin
  state := stateNothing;
  FInterfaceBlock := TStringList.Create;
  FProcs := TStringList.Create;

  Assign(f, Filename);
  Reset(f);
  while not EOF(f) do begin
    ReadLn(f, s);
    if Copy(s, 1, 1) = '#' then continue;  // Skip comments
    if s = '%COPY_INTERFACE' then
      state := stateCopyInterface
    else if s = '%PROCS' then
      state := stateProcs
    else if s = '%END' then
      state := stateNothing
    else
      case state of
        stateCopyInterface: InterfaceBlock.Add(s);
        stateProcs: Procs.Add(s);
      end;
  end;
  Close(f);
end;

end.


{
  $Log$
  Revision 1.2  2000-07-13 11:33:28  michael
  + removed logs
 
}
