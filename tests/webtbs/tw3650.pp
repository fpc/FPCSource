{ %target=win32 }

{ Source provided for Free Pascal Bug Report 3650 }
{ Submitted by "Sergey" on  2005-02-11 }
{ e-mail: sergey@michint.kiev.ua }
// Title: stdcall and variant/record parameters
// Succeded: dcc32.exe, Delphi6
// Failed: fpc-1.9.6, fpc-1.9.7
{$IFDEF FPC}
{$mode delphi}
{$C+}
{$ELSE}
{$AppType CONSOLE}
uses Variants;
{$ENDIF}

function Test1(guid: TGUID; p1, p2: Pointer): HRESULT; stdcall;
begin
  Writeln(Integer(Addr(p1))-Integer(Addr(guid)));
  Writeln(Integer(Addr(p2))-Integer(Addr(p1)));
  Assert(Integer(Addr(p1))-Integer(Addr(guid)) = 16, 'bug rec_std (?)');
  // Fixes: compiler/i386/cpupara.pas:141  when s/8/16/
end;

function Test2(v: VARIANT; p1, p2: Pointer): HRESULT; stdcall;
begin
  Writeln(Integer(Addr(p1))-Integer(Addr(v)));
  Writeln(Integer(Addr(p2))-Integer(Addr(p1)));
  Assert(Integer(Addr(p1))-Integer(Addr(v)) = 16, 'bug var_std (?)');
end;

function Test1c(guid: TGUID; p1, p2: Pointer): HRESULT; cdecl;
begin
  Writeln(Integer(Addr(p1))-Integer(Addr(guid)));
  Writeln(Integer(Addr(p2))-Integer(Addr(p1)));
  Assert(Integer(Addr(p1))-Integer(Addr(guid)) = 16, 'bug rec_c (?)');
end;

function Test2c(v: VARIANT; p1, p2: Pointer): HRESULT; cdecl;
begin
  Writeln(Integer(Addr(p1))-Integer(Addr(v)));
  Writeln(Integer(Addr(p2))-Integer(Addr(p1)));
  Assert(Integer(Addr(p1))-Integer(Addr(v)) = 16, 'bug var_c (?)');
end;

begin
  Write('SizeOf(TGUID)=', SizeOf(TGUID));
  Writeln(' SizeOf(VARIANT)=', SizeOf(VARIANT));
  Test1(IUnknown, nil, nil); {try "s/nil/@Sin/" - you'll get 'Error while linking'}
  Test2(Null, nil, nil);
  Test1c(IUnknown, nil, nil);
  Test2c(Null, nil, nil);
end.
(*
// Fix for 1.9.7: compiler/i386/cpupara.pas:128 in
// function ti386paramanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
// when
        { Only vs_const, vs_value here }
        case def.deftype of
{!}       variantdef:
{!}         result:=not(calloption in [pocall_stdcall,pocall_cdecl{,pocall_cppdecl???}]);
{!}       formaldef :
            result:=true;
          recorddef :
            begin
              { Win32 passes small records on the stack for call by
                value }
              if (target_info.system=system_i386_win32) and
                 (calloption in [pocall_stdcall,pocall_cdecl,pocall_cppdecl]) and
                 (varspez=vs_value) and
                 (def.size<=8{or 16?}) then
                result:=false
              else
{!}             result:=not(calloption in [pocall_stdcall{!},pocall_cdecl,pocall_cppdecl]) and (def.size>sizeof(aint));
            end;

*)
