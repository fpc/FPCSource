{ All base packages. Each package is included in it's own dir }
{ If this file is included from the main packages dir, ALLPACKAGES is defined. }
{ If this file is included from the base packages dir, BASEPACKAGES is defined. }

{ paszlib } 
{$i paszlib/fpmake.inc}

{ pasjpeg } 
{$i pasjpeg/fpmake.inc}

{ regexpr } 
{$i regexpr/fpmake.inc}

{ netdb } 
{$i netdb/fpmake.inc}

{ md5 } 
{$i md5/fpmake.inc}

{ gdbint } 
{$i gdbint/fpmake.inc}

{ libasync } 
{$i libasync/fpmake.inc}

{ mysql } 
{$i mysql/fpmake.inc}

{ ibase } 
{$i ibase/fpmake.inc}

{ postgres } 
{$i postgres/fpmake.inc}

{ oracle } 
{$i oracle/fpmake.inc}

{ odbc } 
{$i odbc/fpmake.inc}

{ pthreads } 
{$i pthreads/fpmake.inc}

{ sqlite } 
{$i sqlite/fpmake.inc}

{ libc } 
{$i libc/fpmake.inc}
