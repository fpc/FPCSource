{
  Contains the Interbase/Firebird-functions calls
  In this stage only the calls needed for IBConnection are implemented
  Other calls could be simply implemented, using copy-paste from ibase60
  
  Call InitialiseIbase60 before using any of the calls, and call ReleaseIbase60
  when finished.
}

unit ibase60dyn;

{$mode objfpc}{$H+}
{$MACRO on}

interface

uses
  dynlibs,sysutils;

{$IFDEF Unix}
  {$LINKLIB c}
  {$LINKLIB crypt}
  {$DEFINE gdsdecl:=cdecl}
  const
    gdslib = 'libgds.so';
    fbclib = 'libfbclient.so';
{$ENDIF}
{$IFDEF Win32}
  {$DEFINE gdsdecl:=stdcall}
  const
    gdslib = 'gds32.dll';
    fbclib = 'fbclient.dll';
{$ENDIF}

{$i ibase60types.inc}

var isc_attach_database : function (_para1:PISC_STATUS; _para2:smallint; _para3:Pchar; _para4:Pisc_db_handle; _para5:smallint;
             _para6:Pchar):ISC_STATUS; gdsdecl;
    isc_interprete : function (_para1:Pchar; _para2:PPISC_STATUS):ISC_STATUS; gdsdecl;
    isc_commit_transaction : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; gdsdecl;
    isc_rollback_transaction : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; gdsdecl;
    isc_start_transaction : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint; args:array of const):ISC_STATUS; cdecl;
    isc_commit_retaining : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; gdsdecl;
    isc_rollback_retaining : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; gdsdecl;
    isc_detach_database : function (_para1:PISC_STATUS; _para2:Pisc_db_handle):ISC_STATUS; gdsdecl;
    isc_vax_integer : function (_para1:Pchar; _para2:smallint):ISC_LONG; gdsdecl;
    isc_dsql_free_statement : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word):ISC_STATUS; gdsdecl;
    isc_dsql_allocate_statement : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_stmt_handle):ISC_STATUS; gdsdecl;
    isc_dsql_prepare : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:PXSQLDA):ISC_STATUS; gdsdecl;
    isc_dsql_describe : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; gdsdecl;
    isc_dsql_execute : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:PXSQLDA):ISC_STATUS; gdsdecl;
    isc_dsql_fetch : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; gdsdecl;
    isc_decode_date : procedure (_para1:PISC_QUAD; _para2:pointer); gdsdecl;
    isc_decode_sql_date : procedure (_para1:PISC_DATE; _para2:pointer); gdsdecl;
    isc_decode_sql_time : procedure (_para1:PISC_TIME; _para2:pointer); gdsdecl;
    isc_decode_timestamp : procedure (_para1:PISC_TIMESTAMP; _para2:pointer); gdsdecl;
    isc_database_info : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:smallint; _para4:Pchar; _para5:smallint;
             _para6:Pchar):ISC_STATUS; gdsdecl;


Procedure InitialiseIBase60;
Procedure ReleaseIBase60;

var IBaseLibraryHandle : TLibHandle;

implementation

var RefCount : integer;

Procedure InitialiseIBase60;

begin
  inc(RefCount);
  if RefCount = 1 then
    begin
    IBaseLibraryHandle := loadlibrary(fbclib);
    if IBaseLibraryHandle = nilhandle then
      begin
      IBaseLibraryHandle := loadlibrary(gdslib);
      if loadlibrary(gdslib) = nilhandle then
        begin
        RefCount := 0;
        Raise EInOutError.Create('Can not load Firebird or Interbase client. Is it installed? ('+gdslib+' or '+fbclib+')');
        end;
      end;
    pointer(isc_attach_database) := GetProcedureAddress(IBaseLibraryHandle,'isc_attach_database');
    pointer(isc_interprete) := GetProcedureAddress(IBaseLibraryHandle,'isc_interprete');
    pointer(isc_commit_transaction) := GetProcedureAddress(IBaseLibraryHandle,'isc_commit_transaction');
    pointer(isc_rollback_transaction) := GetProcedureAddress(IBaseLibraryHandle,'isc_rollback_transaction');
    pointer(isc_start_transaction) := GetProcedureAddress(IBaseLibraryHandle,'isc_start_transaction');
    pointer(isc_commit_retaining) := GetProcedureAddress(IBaseLibraryHandle,'isc_commit_retaining');
    pointer(isc_rollback_retaining) := GetProcedureAddress(IBaseLibraryHandle,'isc_rollback_retaining');
    pointer(isc_detach_database) := GetProcedureAddress(IBaseLibraryHandle,'isc_detach_database');
    pointer(isc_vax_integer) := GetProcedureAddress(IBaseLibraryHandle,'isc_vax_integer');
    pointer(isc_dsql_free_statement) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_free_statement');
    pointer(isc_dsql_allocate_statement) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_allocate_statement');
    pointer(isc_dsql_prepare) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_prepare');
    pointer(isc_dsql_describe) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_describe');
    pointer(isc_dsql_execute) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_execute');
    pointer(isc_dsql_fetch) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_fetch');
    pointer(isc_decode_date) := GetProcedureAddress(IBaseLibraryHandle,'isc_decode_date');
    pointer(isc_decode_sql_date) := GetProcedureAddress(IBaseLibraryHandle,'isc_decode_sql_date');
    pointer(isc_decode_sql_time) := GetProcedureAddress(IBaseLibraryHandle,'isc_decode_sql_time');
    pointer(isc_decode_timestamp) := GetProcedureAddress(IBaseLibraryHandle,'isc_decode_timestamp');
    pointer(isc_database_info) := GetProcedureAddress(IBaseLibraryHandle,'isc_database_info');
    end;
end;

Procedure ReleaseIBase60;

begin
  if RefCount > 0 then dec(RefCount);
  if RefCount = 0 then
    begin
    if not UnloadLibrary(IBaseLibraryHandle) then inc(RefCount);
    end;
end;

// This function is also defined in ibase60!
function XSQLDA_LENGTH(n: Integer): Integer;
begin
  Result := SizeOf(XSQLDA) + (n - 1) * SizeOf(XSQLVAR);
end;

end.

