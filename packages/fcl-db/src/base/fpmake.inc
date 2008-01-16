{ Make DB directory and all subdirectories }
{ FULLDB will be defined if included from db directory }
{ FULLFCL will be defined if included from main FCL directory }

Targets.DefaultDir:='db';
T:=Targets.AddUnit('dbconst');
T.ResourceStrings:=True;
T:=Targets.AddUnit('db');
T:=Targets.AddUnit('ddg_ds');
T:=Targets.AddUnit('ddg_rec');
T:=Targets.AddUnit('dbwhtml');
T.ResourceStrings:=True;

{  sdf directory }
{$i sdf/fpmake.inc}

{ memds directory }
{$i memds/fpmake.inc}

{ sqldb directory }
{$i sqldb/fpmake.inc}

{ interbase directory }
{$i interbase/fpmake.inc }

{ sqlite directory }
{$i sqlite/fpmake.inc}

{ dbase directory}
{$i dbase/fpmake.inc}

{ mysql directory}
{$i mysql/fpmake.inc}

{ directory}
{ $i /fpmake.inc}
    