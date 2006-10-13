{
 apriconv.pas

 Copyright (C) 2006 Felipe Monteiro de Carvalho

 This unit is a pascal binding for the Apache 2.0.58 headers.
 The headers were released under the following copyright:
}
{ Copyright 2000-2005 The Apache Software Foundation or its licensors, as
 * applicable.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 }
unit apriconv;

interface

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

{$IFNDEF FPC}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE WINDOWS}
{$ENDIF}

{$ifdef Unix}
  {$PACKRECORDS C}
{$endif}

uses apr;

const
{$IFDEF WINDOWS}
  LibAPRIconv = 'libapriconv.dll';
{$ELSE}
  LibAPRIconv = '';
{$ENDIF}

{$include apr_iconv.inc}
{$include api_version.inc}

implementation

end.

