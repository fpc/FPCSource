#include <mysql.h>
#include <mysql_com.h>
#include <stdio.h>

main ()
{
  my_bool i;
  NET n;
  enum enum_net_type nettype;
  struct st_mysql_options options;
  my_ulonglong llong;
  
  printf ("MYSQL : %d options : %d\n",sizeof(MYSQL),sizeof(options));
  printf ("MYSQL_DATA : %d\n",sizeof(MYSQL_DATA));
  printf ("MYSQL_ROWS : %d\n",sizeof(MYSQL_ROWS));
  printf ("MYSQL_ROW : %d\n",sizeof(MYSQL_ROW));
  printf ("MYSQL_FIELD : %d\n",sizeof(MYSQL_FIELD));
  printf ("MYSQL_RES : %d\n",sizeof(MYSQL_RES));
  printf ("MEM_ROOT : %d\n",sizeof(MEM_ROOT));
  printf ("my_bool : %d\n",sizeof(my_bool));
  printf ("NET : %d NET.nettype : %d\n",sizeof(NET),sizeof(nettype));
  printf ("USED_MEM : %d\n",sizeof(USED_MEM));
  printf ("new: %d\n",sizeof(char [MYSQL_ERRMSG_SIZE]));
  printf ("longlong: %d\n",sizeof(llong));
  i=n.error;
}

