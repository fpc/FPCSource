program mysqls;

uses mysql,mysql_com;


begin
  Writeln ('MYSQL : ',sizeof(TMYSQL));
  writeln ('MYSQL_DATA : ',sizeof(TMYSQL_DATA));
  writeln ('MYSQL_ROWS : ',sizeof(TMYSQL_ROWS));
  writeln ('MYSQL_ROW : ',sizeof(TMYSQL_ROW));
  writeln ('MYSQL_FIELD : ',sizeof(TMYSQL_FIELD));
  writeln ('MYSQL_RES : ',sizeof(TMYSQL_RES));
  writeln ('MEM_ROOT : ',sizeof(TMEM_ROOT));
  writeln ('my_bool : ',sizeof(my_bool));
  writeln ('TNET : ',sizeof(TNET));
  writeln ('USED_MEM : ',sizeof(TUSED_MEM));
end.
  $Log$
  Revision 1.2  2000-07-13 11:33:26  michael
  + removed logs
 
}
