{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by Florian Klaempfl
    member of the Free Pascal development team

    This is the install program for the DOS version of Free Pascal

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$A+,B-,D+,E+,F-,G-,I-,L+,N-,O-,P-,Q+,R+,S+,T-,V-,X+,Y+}
program install;

  uses
     app,dialogs,views,objects,menus,drivers,strings,msgbox,dos,unzip,ziptypes;

  var
     binpath,startpath : string;
     successfull : boolean;

  const
     version = '0';
     release = '99';
     patchlevel = '8';

     filenr = version+release+patchlevel;

     doc_version = '110';

{*****************************************************************************
                                  Helpers
*****************************************************************************}

  procedure uppervar(var s : string);

    var
       i : integer;

    begin
       for i:=1 to length(s) do
         s[i]:=upcase(s[i]);
    end;

  function file_exists(const f : string;const path : string) : boolean;

    begin
       file_exists:=fsearch(f,path)<>'';
    end;


  function diskspace(const path,zipfile : string) : string;
    var
      compressed,uncompressed : longint;
      s : string;
    begin
      s:=path+zipfile+#0;
      uncompressed:=UnzipSize(@s[1],compressed);
      uncompressed:=uncompressed shr 10;
      str(uncompressed,s);
      diskspace:=' ('+s+' Kb)';
    end;



  function createdir(const s : string) : boolean;

    var
       result : longint;

    begin
       chdir(s);
       if ioresult=0 then
         begin
            result:=messagebox('The installation directory exists already. '+
              'Do want to enter a new installation directory ?',nil,
              mferror+mfyesbutton+mfnobutton);
            createdir:=result=cmyes;
            exit;
         end;
       mkdir(s);
       if ioresult<>0 then
         begin
            messagebox('The installation directory couldn''t be created',
              @s,mferror+mfokbutton);
            createdir:=true;
            exit;
         end;
       createdir:=false;
    end;


  procedure changedir(const s : string);

    begin
       chdir(s);
       if ioresult<>0 then
         begin
            messagebox('Error when changing directory ',@s,mferror+mfokbutton);
            halt(1);
         end;
    end;


{*****************************************************************************
                               TUnZipDialog
*****************************************************************************}

  type
     punzipdialog=^tunzipdialog;
     tunzipdialog=object(tdialog)
         filetext : pstatictext;
         constructor Init(var Bounds: TRect; ATitle: TTitleStr);
         procedure do_unzip(s:string);
     end;

  constructor tunzipdialog.init;
    var
      r : trect;
    begin
      inherited init(bounds,atitle);
      R.Assign(11, 4, 38, 5);
      filetext:=new(pstatictext,init(r,'File: '));
      insert(filetext);
    end;


  procedure tunzipdialog.do_unzip(s : string);
    var
      fn,dir,wild : string;
    begin
       s:=s+'.ZIP';
       Disposestr(filetext^.text);
       filetext^.Text:=NewStr('File: '+s);
       filetext^.drawview;
       if not(file_exists(s,startpath)) then
         begin
            messagebox('File: '+s+' missed for the selected installation. '+
                       'Installation doesn''t becomes complete',nil,mferror+mfokbutton);
            halt(1);
         end;
       fn:=startpath+'\'+s+#0;
       dir:='.'#0;
       wild:='*.*'#0;
       FileUnzipEx(@fn[1],@dir[1],@wild[1]);
       if doserror<>0 then
         begin
            messagebox('Error when extracting. Disk full?',nil,mferror+mfokbutton);
            halt(1);
         end;
    end;


{*****************************************************************************
                               TInstallDialog
*****************************************************************************}

  type
     pinstalldialog = ^tinstalldialog;
     tinstalldialog = object(tdialog)
        constructor init;
     end;
  var
     mask_components : longint;


  constructor tinstalldialog.init;
    var
       r : trect;
       line : integer;
       p,f : pview;
       s : string;

    const breite = 76;
          hoehe = 20;
          x1 = (80-breite) div 2;
          y1 = (23-hoehe) div 2;
          x2 = x1+breite;
          y2 = y1+hoehe;

    begin
       r.assign(x1,y1,x2,y2);
       inherited init(r,'Install');
       line:=2;
       r.assign(3,line+1,28,line+2);
       p:=new(pinputline,init(r,79));
       f:=p;
       s:='C:\PP';
       p^.setdata(s);
       insert(p);
       r.assign(3,line,8,line+1);
       insert(new(plabel,init(r,'~P~ath',p)));
       insert(p);
       inc(line,3);
       r.assign(3,line+1,breite-3,line+11);
       p:=new(pcheckboxes,init(r,
         newsitem('~B~asic system (required)'+diskspace(startpath,'BASEDOS.ZIP'),
         newsitem('GNU ~L~inker and GNU Assembler (required)'+diskspace(startpath,'GNUASLD.ZIP'),
         newsitem('D~e~mos'+diskspace(startpath,'DEMO.ZIP'),
         newsitem('GNU ~D~ebugger'+diskspace(startpath,'GDB.ZIP'),
         newsitem('GNU ~U~tilities (required to recompile run time library)'+diskspace(startpath,'GNUUTILS.ZIP'),
         newsitem('Documentation (~H~TML)'+diskspace(startpath,'DOCS.ZIP'),
         newsitem('Documentation (~P~ostscript)'+diskspace(startpath,'DOC'+doc_version+'PS.ZIP'),
         newsitem('~R~un time library sources'+diskspace(startpath,'RL'+filenr+'S.ZIP'),
         newsitem('~C~ompiler sources'+diskspace(startpath,'PP'+filenr+'S.ZIP'),
         newsitem('Documentation sources (La~T~eX)'+diskspace(startpath,'DOC'+doc_version+'.ZIP'),
         nil
       ))))))))))));
       pcluster(p)^.enablemask:=mask_components;
       insert(p);

       r.assign(3,line,14,line+1);
       insert(new(plabel,init(r,'~C~omponents',p)));

       inc(line,12);
       { Free Vision
       r.assign(3,line+1,breite-3,line+3);
       p:=new(pcheckboxes,init(r,
         newsitem('~B~asic system',
         newsitem('~D~ocumentation',
         newsitem('S~a~mples',
         newsitem('~S~ources',
         nil
       ))))));
       pcluster(p)^.enablemask:=mask_freevision;
       insert(p);
       r.assign(3,line,15,line+1);
       insert(new(plabel,init(r,'~F~ree Vision',p)));
       inc(line,4);
       }
       r.assign((breite div 2)-14,line,(breite div 2)-4,line+2);
       insert(new(pbutton,init(r,'~O~k',cmok,bfdefault)));
       r.assign((breite div 2)+4,line,(breite div 2)+14,line+2);
       insert(new(pbutton,init(r,'~C~ancel',cmcancel,bfnormal)));
       f^.select;
    end;


{*****************************************************************************
                                TApp
*****************************************************************************}

  const
     cmstart = 1000;

  type
     tapp = object(tapplication)
         procedure initmenubar;virtual;
         procedure handleevent(var event : tevent);virtual;
         procedure do_installdialog;
     end;


  procedure tapp.do_installdialog;
    var
       p : pinstalldialog;
       p2 : punzipdialog;
       p3 : pstatictext;
       r : trect;
       c : word;
       t : text;
       installdata : record
                       path : string[79];
                       components : word;
                     end;
       f : file;
    label
      newpath;
    begin
      installdata.path:='C:\PP';
      installdata.components:=0;

      mask_components:=$0;

      { searching files }
      if file_exists('BASEDOS.ZIP',startpath) then
        inc(mask_components,1);

      if file_exists('GNUASLD.ZIP',startpath) then
        inc(mask_components,2);

      if file_exists('DEMO.ZIP',startpath) then
        inc(mask_components,4);

      if file_exists('GDB.ZIP',startpath) then
        inc(mask_components,8);

      if file_exists('GNUUTILS.ZIP',startpath) then
        inc(mask_components,16);

      if file_exists('DOCS.ZIP',startpath) then
        inc(mask_components,32);

      if file_exists('DOC+doc_version+PS.ZIP',startpath) then
        inc(mask_components,64);

      if file_exists('RL'+filenr+'S.ZIP',startpath) then
        inc(mask_components,128);

      if file_exists('PP'+filenr+'S.ZIP',startpath) then
        inc(mask_components,256);

      if file_exists('DOC+doc_version+S.ZIP',startpath) then
        inc(mask_components,512);

      while true do
        begin
      newpath:
           p:=new(pinstalldialog,init);
           { default settings }
           c:=executedialog(p,@installdata);
           if c=cmok then
             begin
                if installdata.path[length(installdata.path)]='\' then
                  dec(byte(installdata.path[0]));
                uppervar(installdata.path);
                binpath:=installdata.path+'\BIN';
                if createdir(installdata.path) then
                  goto newpath;
                changedir(installdata.path);

                r.assign(20,7,60,16);
                p2:=new(punzipdialog,init(r,'Extracting files'));
                desktop^.insert(p2);

                if (installdata.components and 1)<>0 then
                   p2^.do_unzip('BASEDOS');

                if (installdata.components and 2)<>0 then
                   p2^.do_unzip('GNUASLD');

                if (installdata.components and 4)<>0 then
                   p2^.do_unzip('DEMO');

                if (installdata.components and 8)<>0 then
                   p2^.do_unzip('GDB');

                if (installdata.components and 16)<>0 then
                   p2^.do_unzip('GNUUTILS');

                if (installdata.components and 32)<>0 then
                   p2^.do_unzip('DOCS');

                if (installdata.components and 64)<>0 then
                   p2^.do_unzip('DOC+doc_version+PS');

                if (installdata.components and 128)<>0 then
                   p2^.do_unzip('RL'+filenr+'S');

                if (installdata.components and 256)<>0 then
                   p2^.do_unzip('PP'+filenr+'S');

                if (installdata.components and 512)<>0 then
                   p2^.do_unzip('DOC+doc_version+S');

                assign(t,'BIN\PPC386.CFG');
                rewrite(t);
                writeln(t,'-l');
                writeln(t,'#ifdef GO32V1');
                writeln(t,'-Up',installdata.path+'\RTL\DOS\GO32V1');
                writeln(t,'#endif GO32V1');
                writeln(t,'#ifdef GO32V2');
                writeln(t,'-Up',installdata.path+'\RTL\DOS\GO32V2');
                writeln(t,'#endif GO32V2');
                writeln(t,'#ifdef Win32');
                writeln(t,'-Up',installdata.path+'\RTL\WIN32');
                writeln(t,'#endif Win32');
                close(t);

                desktop^.delete(p2);
                dispose(p2,done);
                messagebox('Installation successfull',nil,mfinformation+mfokbutton);
                successfull:=true;
             end;
           break;
        end;
    end;

  procedure tapp.handleevent(var event : tevent);

    label
       insertdisk1,insertdisk2,newpath;

    begin
       inherited handleevent(event);
       if event.what=evcommand then
         if event.command=cmstart then
           begin
              clearevent(event);
              do_installdialog;
              if successfull then
               begin
                 event.what:=evcommand;
                 event.command:=cmquit;
                 handleevent(event);
               end;
           end;
    end;

  procedure tapp.initmenubar;
    var
       r : trect;
    begin
       getextent(r);
       r.b.y:=r.a.y+1;
       menubar:=new(pmenubar,init(r,newmenu(
          newsubmenu('~I~nstallation',hcnocontext,newmenu(
            newitem('~S~tart','',kbnokey,cmstart,hcnocontext,
            newline(
            newitem('~E~xit','Alt+X',kbaltx,cmquit,hcnocontext,
            nil)))
          ),
       nil))));
    end;


var
  installapp : tapp;
begin
   getdir(0,startpath);
   successfull:=false;
   installapp.init;
   installapp.do_installdialog;
   installapp.done;
   if successfull then
     begin
        writeln('Extend your PATH variable with');
        writeln(binpath);
        writeln;
        writeln('To compile files enter PPC386 [file]');
        chdir(startpath);
     end;
end.
{
  $Log$
  Revision 1.4  1998-09-10 10:50:49  florian
    * DOS install program updated

  Revision 1.3  1998/09/09 13:39:58  peter
    + internal unzip
    * dialog is showed automaticly

  Revision 1.2  1998/04/07 22:47:57  florian
    + version/release/patch numbers as string added

}
