{
  $Id$
}
{*
 * Copyright (c) 1999 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 * 
 * The contents of this file constitute Original Code as defined in and
 * are subject to the Apple Public Source License Version 1.1 (the
 * "License").  You may not use this file except in compliance with the
 * License.  Please obtain a copy of the License at
 * http://www.apple.com/publicsource and read it before using this file.
 * 
 * This Original Code and all software distributed under the License are
 * distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.  Please see the
 * License for the specific language governing rights and limitations
 * under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 *
 *
 * Copyright (c) 1989, 1991, 1993
 *  The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *  This product includes software developed by the University of
 *  California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *}

const
  MAXPATHLEN = 1024;
  MAXNAMLEN = 255;

function getcwd_physical(pt: pchar; size: size_t): pchar;
var
  dp: pdirent;
  dir: pdir;
  dev: dev_t;
  ino: ino_t;
  first: longint;
  bpt, bup: pchar;
  s: stat;
  root_dev: dev_t;
  root_ino: ino_t;
  ptsize, upsize: size_t;
  save_errno: longint;
  ept, eup, up: pchar;
  len, off: size_t;

  err, notfound: boolean;

  {*
   * If no buffer specified by the user, allocate one as necessary.
   * If a buffer is specified, the size has to be non-zero.  The path
   * is built from the end of the buffer backwards.
   *}
begin
  err := false;
  notfound := false;
  if (pt <> nil) then
    begin
      ptsize := 0;
      if (size = 0) then
        begin
          errno := ESysEINVAL;
          getcwd_physical := nil;
          exit;
        end;
      ept := pt + size;
    end
  else
    begin
      ptsize := 1024;
      getmem(pt,ptsize);
      ept := pt + ptsize;
    end;
  bpt := ept - 1;
  bpt^ := #0;

  {*
   * Allocate bytes (1024 - malloc space) for the string of "../"'s.
   * Should always be enough (it's 340 levels).  If it's not, allocate
   * as necessary.  Special case the first stat, it's ".", not "..".
   *}
  upsize := 1024;
  getmem(up,1024);
  eup := up + MAXPATHLEN;
  bup := up;
  up[0] := '.';
  up[1] := #0;

  { Save root values, so know when to stop. }
  if (Fpstat('/', s) = 0) THEN
    begin
      root_dev := s.st_dev;
      root_ino := s.st_ino;
    
      errno := 0;      { XXX readdir has no error return. }

      first := 1;
      repeat
        { Stat the current level. }
        if (fplstat(up, s) <> 0) then
          err := true
        else
          begin
            { Save current node values. }
            ino := s.st_ino;
            dev := s.st_dev;
        
            { Check for reaching root. }
            if ((root_dev = dev) and (root_ino = ino)) then
              begin
                dec(bpt);
                bpt^ := '/';
                {*
                * It's unclear that it's a requirement to copy the
                * path to the beginning of the buffer, but it's always
                * been that way and stuff would probably break'.
                *}
                move(bpt^, pt^, ept - bpt);
                freemem(up);
                getcwd_physical := pt;
                exit;
              end;
        
            {
            * Build pointer to the parent directory, allocating memory
            * as necessary.  Max length is 3 for "../", the largest
            * possible component name, plus a trailing NULL.
            *}
            if (bup + 3  + MAXNAMLEN + 1 >= eup) then
              begin
                upsize := upsize*2;
                reallocmem(up,upsize);
                bup := up;
                eup := up + upsize;
              end;

            bup^ := '.';
            inc(bup);
            bup^ := '.';
            inc(bup);
            bup^ := #0;

            {* Open and stat parent directory. *}
            dir := Fpopendir(up);
            if (pdir <> nil) then
              begin
                if (fpfstat(dir^.dd_fd,s) <> 0) then
                  err := true;
              end
            else
              err := true;
            
            if not err then
              begin
                { Add trailing slash for next directory. }
                bup^ := '/';
                inc(bup);
            
                {*
                * If it's a mount point, have to stat each element because
                * the inode number in the directory is for the entry in the
                * parent directory, not the inode number of the mounted file.
                *'}
                save_errno := 0;
                if (s.st_dev = dev) then
                  begin
                    repeat
                      dp := Fpreaddir(dir);
                      notfound := dp = nil;
                    until notfound or
                          (dp^.d_fileno = ino);
                  end
                else
                  begin
                    repeat
                      dp := Fpreaddir(dir);
                      if (dp = nil) then
                        notfound := true
                      else
                        begin
                          if (dp^.d_name[0] = '.') and
                             ((dp^.d_name[1] = #0) or
                              ((dp^.d_name[1] = '.') and
                               (dp^.d_name[2] = #0))) then
                            continue;
                          move(dp^.d_name, bup^, dp^.d_namlen + 1);

                          { Save the first error for later. }
                          if (fplstat(up,s) <> 0) then
                            begin
                              if (save_errno = 0) then
                                save_errno := errno;
                              errno := 0;
                              continue;
                            end;
                        end;
                    until notfound or
                          ((s.st_dev = dev) and
                           (s.st_ino = ino));
                  end;
            
                {*
                * Check for length of the current name, preceding slash,
                * leading slash.
                *}
                if not (notfound) then
                  begin
                    // was: (first ? 1 : 2), first can only be 1 or 0
                    if (bpt - pt <= dp^.d_namlen + ((first xor 1) + 1)) then
                      begin
                        if ( ptsize <> 0) then
                          begin
                            errno := ESysERANGE;
                            err := true;
                          end
                        else
                          begin
                            off := bpt - pt;
                            len := ept - bpt;
                            ptsize := ptsize *2;
                            reallocmem(pt,ptsize);
                            bpt := pt + off;
                            ept := pt + ptsize;
                            move(bpt^, (ept - len)^, len);
                            bpt := ept - len;
                          end;
                      end;
                    if (first = 0) then
                      begin
                        dec(bpt);
                        bpt^ := '/';
                      end;
                    dec(bpt,dp^.d_namlen);
                    move(dp^.d_name, bpt^, dp^.d_namlen);
                    Fpclosedir(dir);
                
                    { Truncate any file name. }
                    bup^ := #0;
                    first := 0;
                  end;
              end;
          end;
      until err or notfound;
    
    if (notfound) then
      begin
        {*
        * If readdir set errno, use it, not any saved error; otherwise,
        * didn't find the current directory in its parent directory, set
        * errno to ENOENT.'
        *}
        if (errno = 0) then
          if save_errno <> 0 then
            errno := save_errno
          else
            errno := ESysENOENT;
      end;
  end;
  if (err) then
    begin
      if (ptsize <> 0) then
        freemem(pt);
      freemem(up);
      getcwd_physical := nil;
    end;
end;


function getcwd(pt: pchar; size: size_t): pchar;
var
  pwd: pchar;
  pwdlen: size_t;
  dev: dev_t;
  ino: ino_t;
  s: stat;
begin
(*
  { Check $PWD -- if it's right, it's fast. }
  if ((pwd = getenv("PWD")) != NULL && pwd[0] == '/' && stat(pwd, &s) != -1) {
    dev = s.st_dev;
    ino = s.st_ino;
    if (stat(".", &s) != -1 && dev == s.st_dev && ino == s.st_ino) {
      pwdlen = strlen(pwd);
      if (pt) {
        if (!size) {
          errno = EINVAL;
          return (NULL);
        }
        if (pwdlen + 1 > size) {
          errno = ERANGE;
          return (NULL);
        }
      } else if ((pt = malloc(pwdlen + 1)) == NULL) {
        errno = ENOMEM;
        return (NULL);
      }
      memmove(pt, pwd, pwdlen);
      pt[pwdlen] = '\0';
      return (pt);
    }
  }
*)
  getcwd := (getcwd_physical(pt, size));
end;

{
  $Log$
  Revision 1.1  2003-05-26 21:29:16  jonas
    - disabled nanosleep for darwin for now
    + getcwd for darwin

}
