{
  Contains the non-external functions for MySQL v3
}

{
  Adapted from mysql3.pp by Bram Kuijvenhoven (Hexis BV, The Netherlands)
}

Function mysql_reload(mysql : PMYSQL) : Longint;

begin
   mysql_reload:=mysql_refresh(mysql,REFRESH_GRANT)
end;
