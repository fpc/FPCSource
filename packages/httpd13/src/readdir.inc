{ Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 }

{
 * Structures and types used to implement opendir/readdir/closedir
 * on Windows 95/NT.
 }

{$ifdef WINDOWS}

{#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#ifndef API_EXPORT
# define API_EXPORT(type)    __declspec(dllexport) type __stdcall
#endif}

{ struct dirent - same as Unix }

const
  _MAX_FNAME = 256;
  
type
  dirent = record
    d_ino: clong;                         { inode (always 1 in WIN32) }
    d_off: off_t;                         { offset to this dirent }
    d_reclen: cushort;                    { length of d_name }
    d_name: array[0.._MAX_FNAME] of Char;    { filename (null terminated) }
  end;

{ typedef DIR - not the same as Unix }
  DIR = record
    handle: clong;                 { _findfirst/_findnext handle }
    offset: cshort;                { offset into directory }
    finished: cshort;              { 1 if there are not more files }
//    struct _finddata_t fileinfo; { from _findfirst/_findnext }
    fileinfo: Pointer;             { from _findfirst/_findnext }
    dir: PChar;                    { the dir we are reading }
    dent: dirent;                  { the dirent to return }
  end;

  PDIR = ^DIR;
  
{ Function prototypes }
{API_EXPORT(DIR *) opendir(const char *);
API_EXPORT(struct dirent *) readdir(DIR *);
API_EXPORT(int) closedir(DIR *);}

  va_list = Pointer;

{
 * Simplified declarations for other platforms
 }

{$else}

  PDIR = Pointer;

  va_list = Pointer;

{$endif} { WINDOWS }

