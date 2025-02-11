{$IFNDEF FPC_DOTTEDUNITS}
unit job.threading;
{$ENDIF}

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  Wasm.Job.Js, Wasm.Job.Shared;
  {$ELSE}
  job.js, job.shared;
  {$ENDIF}

// imported functions from browser
function __job_share_object(
  ObjID: TJOBObjectID; // Object to share
  ThreadID: TThreadID // Thread to share with. Set to 0 to share with all objects
): TJOBResult; external JOBExportName name JOBFn_ShareObject;

implementation

Procedure ShareJobObject(aObjID: TJOBObjectID; aThreadID: TThreadID; out aResult: TJOBResult);

begin
  aResult:=__job_share_object(aObjId,aThreadId);
end;

initialization
  JobShareObjectCallBack:=@ShareJobObject;
end.

