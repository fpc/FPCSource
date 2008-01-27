/*********************************************************************/
/* BACKWARD.FNC - Reverse the words in a string                      */
/*                                                                   */
/* This program is called by CALLREXX.EXE.                           */
/*                                                                   */
/* Input:  A string of words                                         */
/*                                                                   */
/* Output: A string containing the same words but in opposite order  */
/*                                                                   */
/*********************************************************************/

Parse Arg Data                         /* get argument string        */
OutString = ''                         /* initialize output to empty */
Count = Words(Data)                    /* find number of words       */
Do i = Count To 1 By -1                /* for each word in string    */
   OutString = OutString Word(Data,i)  /*   add word to output string*/
End /* end do */
Return Strip(OutString)                /* return reversed string,    */
                                       /* removing any blanks on the */
                                       /* front or back.             */

