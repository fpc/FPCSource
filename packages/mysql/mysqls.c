#include <mysql.h>
#include <stdio.h>

main ()
{
  my_bool i;
  NET n;
  printf ("MYSQL : %d\n",sizeof(MYSQL));
  printf ("MYSQL_DATA : %d\n",sizeof(MYSQL_DATA));
  printf ("MYSQL_ROWS : %d\n",sizeof(MYSQL_ROWS));
  printf ("MYSQL_ROW : %d\n",sizeof(MYSQL_ROW));
  printf ("MYSQL_FIELD : %d\n",sizeof(MYSQL_FIELD));
  printf ("MYSQL_RES : %d\n",sizeof(MYSQL_RES));
  printf ("MEM_ROOT : %d\n",sizeof(MEM_ROOT));
  printf ("my_bool : %d\n",sizeof(my_bool));
  printf ("NET : %d\n",sizeof(NET));
  printf ("USED_MEM : %d\n",sizeof(USED_MEM));
  printf ("new: %d\n",sizeof(char [MYSQL_ERRMSG_SIZE]));
  i=n.error;
}

