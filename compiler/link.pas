{
    $Id$
    Copyright (c) 1998 by the FPC development team

    This unit handles the linker and binder calls for programs and
    libraries

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
Unit link;

Interface

uses cobjects;

Type TLinker = Object
     { Internal variables. Don't access directly }
       {$ifdef linux}
       LinkToC : Boolean;                 { Should we link to the C libs? }
       GccLibraryPath : String;           { Where is GCCLIB ? }
       DynamicLinker : String;            { What Dynamic linker ? }
       {$endif}
       OFiles, LibFiles : TStringContainer;
       Strip : Boolean;                   { Strip symbols ? }
       MakeLib : Boolean;                 { If unit : Make library ?}
       ExeName,                           { FileName of the exe to be created }
       LibName     : String;              { FileName of the lib to be created }
       LinkResName : String[32];          { Name of response file }
       LinkOptions : String;              { Additional options to the linker }
       LibrarySearchPath : String;        { Where to look for libraries }
     { Methods }
       Constructor Init;
       Procedure SetFileName(const s:string);
       function  FindObjectFile(s : string) : string;
       Procedure AddLibraryFile(S : String);
       Procedure AddObjectFile(S : String);
       Function  FindLinker : String;      { Find linker, sets Name }
       Function  DoExec(const command,para:string):boolean;
       Function  WriteResponseFile : Boolean;
       Function  Link:boolean;
       Procedure Make_Library;
     end;
     PLinker=^TLinker;

Var Linker : TLinker;


Implementation

uses
  Script,globals,systems,dos,verbose;

Constructor TLinker.Init;
begin
  OFiles.Init;
  LibFiles.Init;
  OFiles.Doubles:=False;
  LibFiles.Doubles:=False;
  Strip:=false;
  LinkOptions:='';
  LinkResName:='link.res';
  ExeName:='';
  LibName:='';
{$ifdef linux}
  LinkToC:=False;
  LibrarySearchPath:='';
  DynamicLinker:='/lib/ld-linux.so.1';
{$endif}
end;


Procedure TLinker.SetFileName(const s:string);
var
  path:dirstr;
  name:namestr;
  ext:extstr;
begin
  FSplit(s,path,name,ext);
  LibName:=Path+Name+target_info.DllExt;
  ExeName:=Path+Name+target_info.ExeExt;
end;


var
  LastLDBin : string;
Function TLinker.FindLinker:string;
var
  ldfound : boolean;
begin
  if LastLDBin='' then
   begin
     if (target_info.target=target_WIN32) then
     { the win32 linker has another name to allow cross compiling between }
     { DOS and Win32, I think it should be possible to compile an ld      }
     { with handles coff and pe, but I don't know how      (FK)           }
       LastLDBin:=FindExe('ldw',ldfound)
     else
       LastLDBin:=FindExe('ld',ldfound);
     if (not ldfound) and (not externlink) then
      begin
        Message1(exec_w_linker_not_found,LastLDBin);
        externlink:=true;
      end;
     if ldfound then
      Message1(exec_u_using_linker,LastLDBin);
   end;
  FindLinker:=LastLDBin;
end;


{ searches an object file }
function TLinker.FindObjectFile(s:string) : string;
var
  found : boolean;
begin
  if pos('.',s)=0 then
   s:=s+target_info.objext;
  s:=FixFileName(s);
  if FileExists(s) then
   begin
     Findobjectfile:=s;
     exit;
   end;
  findobjectfile:=search(s,'.;'+unitsearchpath+';'+exepath,found)+s;
  if (not externasm) and (not found) then
   Message1(exec_e_objfile_not_found,s);
end;


Procedure TLInker.AddObjectFile (S : String);
begin
  if pos('.',s)=0 then
   s:=s+target_info.objext;
  s:=FixFileName(s);
  OFiles.Insert (S);
end;


Procedure TLInker.AddLibraryFile(S:String);
begin
  if pos('.',s)=0 then
   s:=s+target_info.dllext;
  LibFiles.Insert (S);
end;


Function TLinker.DoExec(const command,para:string):boolean;
begin
  DoExec:=true;
  if not externlink then
   begin
     swapvectors;
     exec(command,para);
     swapvectors;
     if (dosexitcode<>0) then
      begin
        Message(exec_w_error_while_linking);
        DoExec:=false;
        exit;
      end
     else
      if (dosError<>0) then
       begin
         Message(exec_w_cant_call_linker);
         ExternLink:=true;
       end;
   end;
  if externlink then
   AsmRes.AddLinkCommand (Command,Para,ExeName);
end;


Function TLinker.WriteResponseFile : Boolean;
Var
  LinkResponse : Text;
  i            : longint;
  prtobj,s     : string;
begin
{ Open linkresponse and write header }
  assign(linkresponse,inputdir+LinkResName);
  rewrite(linkresponse);

{ Write Header and set runtime object (prt0) }
  case target_info.target of
   target_WIN32 : begin
                    prtobj:='';
                    writeln(linkresponse,'INPUT (');
                  end;
   target_linux : begin
                    if cs_profile in aktswitches then
                     prtobj:='gprt0'
                    else
                     prtobj:='prt0';
{$ifdef Linux}
                    if LinkToC then
                     writeln(linkresponse,'SEARCH_DIR ('+GCCLibraryPath +')');
{$endif}
                    writeln(linkresponse,'INPUT (');
                  end;
  else
   prtobj:='prt0';
  end;          

{ add objectfiles, start with prt0 always }
  if prtobj<>'' then
   Writeln(linkresponse,FindObjectFile(prtobj));
  while not OFiles.Empty do
   begin
     s:=Findobjectfile(OFiles.Get);
     if s<>'' then
      Writeln(linkresponse,s);
   end;

{ Write libraries like -l<lib> }
  While not LibFiles.Empty do
   begin
     S:=LibFiles.Get;
     i:=Pos(target_info.dllext,S);
     if i>0 then
      Delete(S,i,255);
   {OS/2 linker supports -l, but not for import libraries.
    For now, this fix should do it, as we don't support other libraries yet
    but we need to think of something better.}
   if target_info.target=target_OS2 then
     writeln(linkresponse,s)
   else
     Writeln (LinkResponse,'-l'+S);
   end;

{ Write End of response file }
  if target_info.target in [target_WIN32,target_linux] then
    Writeln (LinkResponse,')');

{ Close response }
  close(linkresponse);
  WriteResponseFile:=True;
end;


Function TLinker.link:boolean;
var
  bindbin    : string[80];
  bindfound  : boolean;
  _stacksize,i,
  _heapsize  : longint;
  s,s2       : string[10];
  dummy      : file;
  success    : boolean;
begin
{$ifdef linux}
  if LinkToC then
   begin
     AddObjectFile('/usr/lib/crt0.o');
     AddObjectFile(FindObjectFile('lprt'));
     AddLibraryFile('libc.a');
     AddLibraryFile('libgcc.a');
   end;
{$endif Linux}

{ Create Linkoptions }
  case target_info.target of
     target_GO32V1:
       LinkOptions:=LinkOptions+' -oformat coff-go32';
     target_GO32V2:
       LinkOptions:=LinkOptions+' -oformat coff-go32-exe';
      target_linux: begin
                      if cs_profile in aktswitches then
                       begin
                         AddLibraryFile('gmon');
                         AddLibraryFile('c');
                       end;     
                    end;
  end;

{$ifdef linux}
  If not LibFiles.Empty then
   LinkOptions:='-dynamic-linker='+DynamicLinker+' '+LinkOptions;
{$endif linux}

  if Strip then
   LinkOptions:=LinkOptions+' -s';

{ Write used files and libraries }
  WriteResponseFile;

{ Call linker }
  if not externlink then
   Message1(exec_i_linking,ExeName);
{$ifdef linux}
  success:=DoExec(FindLinker,LinkOptions+' -o '+exename+' '+inputdir+LinkResName);
{$else}
  if target_info.target=target_WIN32 then
    success:=DoExec(FindLinker,LinkOptions+' -o '+exename+' '+inputdir+LinkResName)
  else
    success:=DoExec(FindLinker,LinkOptions+' -o '+exename+' @'+inputdir+LinkResName);
{$endif}

{Bind}
  if target_info.target=target_os2 then
   begin
   {Calculate the stack and heap size in kilobytes, rounded upwards.}
     _stacksize:=(stacksize+1023) shr 10;
   {Minimum stacksize for EMX is 32K.}
     if _stacksize<32 then
      _stacksize:=32;
     str(_stacksize,s);
     _heapsize:=(heapsize+1023) shr 10;
     str(_heapsize,s2);
     bindbin:=FindExe('emxbind',bindfound);
     if (not bindfound) and (not externlink) then
      begin
        Message(exec_w_binder_not_found);
        externlink:=true;
      end;
     DoExec(bindbin,'-k'+s+' -o '+exename+'.exe '+exename+' -aim -s'+s2);
   end;
  if (success) and (not externlink) then
   begin
     assign(dummy,LinkResName);
     {$I-}
      erase(dummy);
     {$I+}
     i:=ioresult;
   end;
  link:=success;   { otherwise a recursive call to link method }
end;


Procedure TLinker.Make_Library;
var
{$ifndef linux}
  arbin : string;
  arfound : boolean;
{$endif}
begin
  if cs_shared_lib in initswitches then
   begin
     WriteResponseFile;
{$ifdef linux}
     DoExec(FindLinker,' -o '+libname+'.so -shared link.res');
{$else}
     arbin:=FindExe('ar',arfound);
     if (not arfound) and (not externlink) then
      begin
        Message(exec_w_ar_not_found);
        externlink:=true;
      end;
     DoExec(arbin,'rs '+libname+'.a');
{$endif}
   end;
end;

end.
{
  $Log$
  Revision 1.3  1998-04-16 10:54:30  daniel
  * Fixed linking for OS/2.

  Revision 1.2  1998/03/30 09:50:49  michael
  + fix for library support.

  Revision 1.1.1.1  1998/03/25 11:18:13  root
  * Restored version

  Revision 1.31  1998/03/13 22:45:58  florian
    * small bug fixes applied

  Revision 1.30  1998/03/11 22:22:52  florian
    * Fixed circular unit uses, when the units are not in the current dir (from Peter)
    * -i shows correct info, not <lf> anymore (from Peter)
    * linking with shared libs works again (from Peter)

  Revision 1.29  1998/03/10 16:27:39  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.28  1998/03/10 01:17:19  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.27  1998/03/05 22:43:47  florian
    * some win32 support stuff added

  Revision 1.26  1998/03/04 01:35:04  peter
    * messages for unit-handling and assembler/linker
    * the compiler compiles without -dGDB, but doesn't work yet
    + -vh for Hint

  Revision 1.25  1998/03/02 01:48:42  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.24  1998/03/01 22:46:12  florian
    + some win95 linking stuff
    * a couple of bugs fixed:
      bug0055,bug0058,bug0059,bug0064,bug0072,bug0093,bug0095,bug0098

  Revision 1.23  1998/02/28 03:56:15  carl
    + replaced target_info.short_name by target_info.target (a bit faster)

  Revision 1.22  1998/02/26 11:57:09  daniel
  * New assembler optimizations commented out, because of bugs.
  * Use of dir-/name- and extstr.

  Revision 1.21  1998/02/25 20:26:41  michael
  + fixed linking for linux

  Revision 1.20  1998/02/24 14:20:53  peter
    + tstringcontainer.empty
    * ld -T option restored for linux
    * libraries are placed before the objectfiles in a .PPU file
    * removed 'uses link' from files.pas

  Revision 1.19  1998/02/23 02:54:23  carl
    * bugfix of recusrive call to link

  Revision 1.18  1998/02/22 23:03:18  peter
    * renamed msource->mainsource and name->unitname
    * optimized filename handling, filename is not seperate anymore with
      path+name+ext, this saves stackspace and a lot of fsplit()'s
    * recompiling of some units in libraries fixed
    * shared libraries are working again
    + $LINKLIB <lib> to support automatic linking to libraries
    + libraries are saved/read from the ppufile, also allows more libraries
      per ppufile

  Revision 1.17  1998/02/19 00:11:00  peter
    * fixed -g to work again
    * fixed some typos with the scriptobject

  Revision 1.16  1998/02/18 13:48:16  michael
  + Implemented an OS independent AsmRes object.

  Revision 1.15  1998/02/18 08:55:26  michael
  * Removed double declaration of LinkerOptions

  Revision 1.14  1998/02/17 21:20:50  peter
    + Script unit
    + __EXIT is called again to exit a program
    - target_info.link/assembler calls
    * linking works again for dos
    * optimized a few filehandling functions
    * fixed stabs generation for procedures

  Revision 1.13  1998/02/16 13:46:40  michael
  + Further integration of linker object:
    - all options pertaining to linking go directly to linker object
    - removed redundant variables/procedures, especially in OS_TARG...

  Revision 1.12  1998/02/16 12:51:31  michael
  + Implemented linker object

  Revision 1.11  1998/02/15 21:16:21  peter
    * all assembler outputs supported by assemblerobject
    * cleanup with assembleroutputs, better .ascii generation
    * help_constructor/destructor are now added to the externals
    - generation of asmresponse is not outputformat depended

  Revision 1.10  1998/02/14 01:45:21  peter
    * more fixes
    - pmode target is removed
    - search_as_ld is removed, this is done in the link.pas/assemble.pas
    + findexe() to search for an executable (linker,assembler,binder)

  Revision 1.9  1998/02/13 22:26:28  peter
    * fixed a few SigSegv's
    * INIT$$ was not written for linux!
    * assembling and linking works again for linux and dos
    + assembler object, only attasmi3 supported yet
    * restore pp.pas with AddPath etc.

  Revision 1.8  1998/02/13 10:35:09  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

>>>>>>> h:/cvs/compiler/link.pas
  Revision 1.7  1998/02/02 00:55:32  peter
    * defdatei -> deffile and some german comments to english
    * search() accepts : as seperater under linux
    * search for ppc.cfg doesn't open a file (and let it open)
    * reorganize the reading of parameters/file a bit
    * all the PPC_ environments are now for all platforms

  Revision 1.6  1998/02/01 15:02:11  florian
    * swapvectors around exec inserted

  Revision 1.5  1998/01/28 13:48:39  michael
  + Initial implementation for making libs from within FPC. Not tested, as compiler does not run

  Revision 1.4  1998/01/25 18:45:43  peter
    + Search for as and ld at startup
    + source_info works the same as target_info
    + externlink allows only external linking

  Revision 1.3  1998/01/24 00:36:07  florian
    + small fix to get it working with DOS (dynamiclinker isn't declared for dos)

  Revision 1.2  1998/01/23 22:19:17  michael
  + Implemented setting of dynamic linker name (linux only).
    Declared Make_library
    -Fd switch sets linker (linux only)
  * Reinstated -E option of Pierre

  Revision 1.1  1998/01/23 17:57:41  michael
  + Initial implementation.

}
