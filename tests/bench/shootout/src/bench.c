GCC 
Back to the Win32 Shootout
 Back to dada's perl lab
 
[The Original Shootout]   [NEWS]   [FAQ]   [Methodology]   [Platform
Details]   [Acknowledgements]   [Scorecard]   


All Source For gcc


Ackermann's Function




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>

int 
Ack(int M, int N) {
    if (M == 0) return( N + 1 );
    if (N == 0) return( Ack(M - 1, 1) );
    return( Ack(M - 1, Ack(M, (N - 1))) );
}

int
main(int argc, char *argv[]) {
    int n = ((argc == 2) ? atoi(argv[1]) : 1);

    printf("Ack(3,%d): %d\n", n, Ack(3, n));
    return(0);
}





Array Access




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 *
 * this program is modified from:
 *   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
 * Timing Trials, or, the Trials of Timing: Experiments with Scripting
 * and User-Interface Languages</a> by Brian W. Kernighan and
 * Christopher J. Van Wyk.
 *
 * I added free() to deallocate memory.
 */

#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char *argv[]) {
    int n = ((argc == 2) ? atoi(argv[1]) : 1);
    int i, k, *x, *y;
    
    x = (int *) calloc(n, sizeof(int));
    y = (int *) calloc(n, sizeof(int));

    for (i = 0; i < n; i++) {
    x[i] = i + 1;
    }
    for (k=0; k<1000; k++) {
    for (i = n-1; i >= 0; i--) {
        y[i] += x[i];
    }
    }

    fprintf(stdout, "%d %d\n", y[0], y[n-1]);

    free(x);
    free(y);

    return(0);
}



Count Lines/Words/Chars




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 *
 * this program is modified from:
 *   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
 * Timing Trials, or, the Trials of Timing: Experiments with Scripting
 * and User-Interface Languages</a> by Brian W. Kernighan and
 * Christopher J. Van Wyk.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define    IN    1    
#define    OUT    0    

int
main() {
    int i, c, nl, nw, nc, state, nread;
    char buf[4096];

    state = OUT;
    nl = nw = nc = 0;
    while ((nread = read(0, buf, sizeof(buf))) > 0) {
    nc += nread;
    for (i=0; i<nread; i++) {
        c = buf[i];
        if (c == '\n')
        ++nl;
        if (c == ' ' || c == '\n' || c == '\t')
        state = OUT;
        else if (state == OUT) {
        state = IN;
        ++nw;
        }
    }
    }
    printf("%d %d %d\n", nl, nw, nc);
    return(0);
}



Echo Client/Server




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>


typedef int (*SOCKACTION_P)(int,struct sockaddr *,socklen_t);
#define DATA "Hello there sailor\n"

void myabort (char *m) { fprintf(stderr, "%s\n", m); exit(1); }
void sysabort (char *m) { perror(m); exit(1); }

int sigchld = 0;
void reaper (int sig) { sigchld = 1; }

int 
genericSock(int port,SOCKACTION_P action,char *actionExceptionText) {
    int ss, optval = 1;
    struct sockaddr_in sin;
    if ((ss = socket(PF_INET, SOCK_STREAM, 0)) == -1)
    sysabort("socket");
    if (setsockopt(ss, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)) ==
-1)
    sysabort("setsockopt");
    memset(&sin,0,sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    sin.sin_port = port; 
    if (action(ss, (struct sockaddr *)&sin,(socklen_t)sizeof(sin)) == -1)
    sysabort(actionExceptionText);

    return(ss);
}

int
server_sock () {
    int ss = genericSock(0,(SOCKACTION_P)bind,"server/bind");
    return(listen(ss,2),ss);
}

int
client_sock (int port) {
    return(genericSock(port,(SOCKACTION_P)connect,"client/connect"));
}

int
get_port (int sock) {
    struct sockaddr_in sin;
    socklen_t slen = sizeof(sin);
    if (getsockname(sock, (struct sockaddr *)&sin, &slen) == -1)
    sysabort("server/getsockname");
    return(sin.sin_port);
}    

void
echo_client (int n, int port) {
    int i, sock, olen, len, nwritten, nread;
    char *offset, obuf[64], ibuf[64];
    char *end = ibuf + sizeof(ibuf);

    sock = client_sock(port);
    strcpy(obuf, DATA);
    olen = strlen(obuf);
    for (i=0; i<n; i++) {
    len = olen;
    offset = obuf;
    while (len > 0) {
        if ((nwritten = write(sock, offset, len)) == -1)
        sysabort("client/write");
        offset += nwritten;
        len -= nwritten;
    }
    offset = ibuf;
    while ((nread = read(sock, offset, (end - offset))) > 0) {
        offset += nread;
        if (*(offset-1) == '\n') break;
    }
    if (nread == -1)
        sysabort("client/read");
    *offset = 0;
    if ((strcmp(obuf, ibuf)) != 0) {
        char mbuf[128];
        sprintf(mbuf, "client: \"%s\" ne \"%s\"", obuf, ibuf);
        myabort(mbuf);
    }
    }
    close(sock);
}

void
echo_server (int n) {
    int ssock, csock, len, nwritten, total_bytes;
    pid_t pid;
    char buf[64], *offset;
    struct sockaddr_in sin;
    socklen_t slen = sizeof(sin);
    int status;

    ssock = server_sock();
    signal(SIGCHLD, reaper);
    if ((pid = fork()) == -1)
    sysabort("server/fork");
    if (pid) {
    
    if ((csock = accept(ssock, (struct sockaddr *)&sin, &slen)) == -1)
        sysabort("server/accept");
    total_bytes = 0;
    while ((len = read(csock, buf, sizeof(buf))) > 0) {
        if (sigchld) myabort("server/sigchld");
        offset = buf;
        total_bytes += len;
        while (len > 0) {
        if ((nwritten = write(csock, offset, len)) == -1)
            sysabort("server/write");
        offset += nwritten;
        len -= nwritten;
        }
    }
    if (len == -1)
        sysabort("server/read");
    close(csock);
    fprintf(stdout, "server processed %d bytes\n", total_bytes);
    } else {
    
    echo_client(n, get_port(ssock));
    }
    wait(&status);
}

int
main(int argc, char *argv[]) {
    echo_server((argc == 2) ? atoi(argv[1]) : 1);
    return(0);
}



Exception Mechanisms




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

int HI = 0, LO = 0;

static jmp_buf Hi_exception;
static jmp_buf Lo_exception;

void blowup (int n) {
    if (n & 1) {
    longjmp(Lo_exception, 1);
    } else {
    longjmp(Hi_exception, 1);
    }
}

void lo_function (volatile int n) {
    if (setjmp(Lo_exception) != 0) {
    LO++;
    } else {
    blowup(n);
    }
}

void hi_function (volatile int n) {
    if (setjmp(Hi_exception) != 0) {
    HI++;
    } else {
    lo_function(n);
    }
}

void some_function (int n) {
    hi_function(n);
}

int
main(int argc, char *argv[]) {
    int volatile N = ((argc == 2) ? atoi(argv[1]) : 1);

    while (N) {
    some_function(N--);
    }
    printf("Exceptions: HI=%d / LO=%d\n", HI, LO);
    return(0);
}



Fibonacci Numbers




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>

unsigned long
fib(unsigned long n) {
    if (n < 2)
    return(1);
    else
    return(fib(n-2) + fib(n-1));
}

int
main(int argc, char *argv[]) {
    int N = ((argc == 2) ? atoi(argv[1]) : 1);
    printf("%ld\n", fib(N));
    return(0);
}



Hash (Associative Array) Access




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "simple_hash.h"

int main(int argc, char *argv[]) {
    int i, c=0, n = ((argc == 2) ? atoi(argv[1]) : 1);
    char buf[32];

    struct ht_ht *ht = ht_create(n);

    for (i=1; i<=n; i++) {
    sprintf(buf, "%x", i);
    (ht_find_new(ht, buf))->val = i;
    }

    for (i=n; i>0; i--) {
    sprintf(buf, "%d", i);
    if (ht_find(ht, buf)) c++;
    }

    ht_destroy(ht);

    printf("%d\n", c);
    return(0);
}



Hashes, Part II




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "simple_hash.h"

int main(int argc, char *argv[]) {
    int i, n = ((argc == 2) ? atoi(argv[1]) : 1);
    char buf[32];
    struct ht_ht *ht1 = ht_create(10000);
    struct ht_ht *ht2 = ht_create(10000);
    struct ht_node *node;

    for (i=0; i<=9999; ++i) {
    sprintf(buf, "foo_%d", i);
    ht_find_new(ht1, buf)->val = i;
    }

    for (i=0; i<n; ++i) {
    for (node=ht_first(ht1); node; node=ht_next(ht1)) {
        ht_find_new(ht2, node->key)->val += node->val;
    }
    }

    printf("%d %d %d %d\n",
       (ht_find(ht1, "foo_1"))->val,
       (ht_find(ht1, "foo_9999"))->val,
       (ht_find(ht2, "foo_1"))->val,
       (ht_find(ht2, "foo_9999"))->val);

    ht_destroy(ht1);
    ht_destroy(ht2);
    return(0);
}



Heapsort




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#define IM 139968
#define IA   3877
#define IC  29573

double
gen_random(double max) {
    static long last = 42;
    return( max * (last = (last * IA + IC) % IM) / IM );
}

void
heapsort(int n, double *ra) {
    int i, j;
    int ir = n;
    int l = (n >> 1) + 1;
    double rra;

    for (;;) {
    if (l > 1) {
        rra = ra[--l];
    } else {
        rra = ra[ir];
        ra[ir] = ra[1];
        if (--ir == 1) {
        ra[1] = rra;
        return;
        }
    }
    i = l;
    j = l << 1;
    while (j <= ir) {
        if (j < ir && ra[j] < ra[j+1]) { ++j; }
        if (rra < ra[j]) {
        ra[i] = ra[j];
        j += (i = j);
        } else {
        j = ir + 1;
        }
    }
    ra[i] = rra;
    }
}

int
main(int argc, char *argv[]) {
    int N = ((argc == 2) ? atoi(argv[1]) : 1);
    double *ary;
    int i;
    
    
    ary = (double *)malloc((N+1) * sizeof(double));
    for (i=1; i<=N; i++) {
    ary[i] = gen_random(1);
    }

    heapsort(N, ary);

    printf("%.10g\n", ary[N]);

    free(ary);
    return(0);
}




Hello World




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>

int main() {
    fputs("hello world\n", stdout);
    return(0);
}




List Operations




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define SIZE 10000

// a simple Double Linked List
// the head node is special, it's val is length of list
typedef struct DLL {
    int val;
    struct DLL *next;    
    struct DLL *prev;    
} DLL;

inline int list_length(DLL *head) { return(head->val); }
inline int list_empty(DLL *head) { return(list_length(head) == 0); }
inline DLL *list_first(DLL *head) { return(head->next); }
inline DLL *list_last(DLL *head) { return(head->prev); }

void list_push_tail(DLL *head, DLL *item) {
    DLL *tail = head->prev;
    tail->next = item;
    item->next = head;
    head->prev = item;
    item->prev = tail;
    head->val++;
}

DLL *list_pop_tail(DLL *head) {
    DLL *prev, *tail;
    if (list_empty(head)) return(NULL);
    tail = head->prev;
    prev = tail->prev;
    prev->next = head;
    head->prev = prev;
    head->val--;
    return(tail);
}

void list_push_head(DLL *head, DLL *item) {
    DLL *next = head->next;
    head->next = item;
    next->prev = item;
    item->next = next;
    item->prev = head;
    head->val++;
}

DLL *list_pop_head(DLL *head) {
    DLL *next;
    if (list_empty(head)) return(NULL);
    next = head->next;
    head->next = next->next;
    next->next->prev = head;
    head->val--;
    return(next);
}

int list_equal(DLL *x, DLL *y) {
    DLL *xp, *yp;
    // first val's checked will be list lengths
    for (xp=x, yp=y; xp->next != x; xp=xp->next, yp=yp->next) {
    if (xp->val != yp->val) return(0);
    }
    if (xp->val != yp->val) return(0);
    return(yp->next == y);
}

void list_print(char *msg, DLL *x) {
    DLL *xp, *first = x->next;
    int i = 0;
    printf(msg);
    printf("length: %d\n", list_length(x));
    for (xp=x->next; xp->next != first; xp=xp->next) {
    printf("i:%3d  v:%3d  n:%3d  p:%3d\n", ++i,
           xp->val, xp->next->val, xp->prev->val);
    }
    printf("[last entry points to list head]\n");
    printf("[val of next of tail is:  %d]\n", xp->next->val);
}

DLL *list_new() {
    DLL *l = (DLL *)malloc(sizeof(DLL));
    l->next = l;
    l->prev = l;
    l->val = 0;
    return(l);
}


DLL *list_sequence(int from, int to) {
    int size, tmp, i, j;
    DLL *l;
    if (from > to) {
    tmp = from; from = to; to = tmp;
    }
    size = to - from + 1;
    l = (DLL *)malloc((size+1) * sizeof(DLL));
    from--;
    for (i=0, j=1; i<size; ++i, ++j) {
    l[i].next = &l[i+1];
    l[j].prev = &l[j-1];
    l[i].val = from++;
    }
    l[0].prev = &l[size];
    l[size].next = &l[0];
    l[size].prev = &l[size-1];
    l[size].val = from;
    l[0].val = size;
    return(l);
}

DLL *list_copy(DLL *x) {
    int i, j, size = list_length(x);
    DLL *xp, *l = (DLL *)malloc((size+1) * sizeof(DLL));
    for (i=0, j=1, xp=x; i<size; i++, j++, xp=xp->next) {
    l[i].next = &l[j];
    l[j].prev = &l[i];
    l[i].val = xp->val;
    }
    l[0].prev = &l[size];
    l[size].next = &l[0];
    l[size].val = list_last(x)->val;
    return(l);
}

void list_reverse (DLL *head) {
    DLL *tmp, *p = head;
    do {
    tmp = p->next;
    p->next = p->prev;
    p->prev = tmp;
    p = tmp;
    } while (p != head);
}

int test_lists() {
    int len = 0;
    // create a list of integers (li1) from 1 to SIZE
    DLL *li1 = list_sequence(1, SIZE);
    // copy the list to li2
    DLL *li2 = list_copy(li1);
    // remove each individual item from left side of li2 and
    // append to right side of li3 (preserving order)
    DLL *li3 = list_new();
    // compare li2 and li1 for equality
    if (!list_equal(li2, li1)) {
    fprintf(stderr, "li2 and li1 are not equal\n");
    exit(1);
    }
    while (!list_empty(li2)) {
    list_push_tail(li3, list_pop_head(li2));
    }
    // li2 must now be empty
    if (!list_empty(li2)) {
    fprintf(stderr, "li2 should be empty now\n");
    exit(1);
    }
    // remove each individual item from right side of li3 and
    // append to right side of li2 (reversing list)
    while (!list_empty(li3)) {
    list_push_tail(li2, list_pop_tail(li3));
    }
    // li3 must now be empty
    if (!list_empty(li3)) {
    fprintf(stderr, "li3 should be empty now\n");
    exit(1);
    }
    // reverse li1 in place
    list_reverse(li1);
    // check that li1's first item is now SIZE
    if (list_first(li1)->val != SIZE) {
    fprintf(stderr, "li1 first value wrong, wanted %d, got %d\n",
        SIZE, list_first(li1)->val);
    exit(1);
    }
    // check that li1's last item is now 1
    if (list_last(li1)->val != 1) {
    fprintf(stderr, "last value wrong, wanted %d, got %d\n",
        SIZE, list_last(li1)->val);
    exit(1);
    }
    // check that li2's first item is now SIZE
    if (list_first(li2)->val != SIZE) {
    fprintf(stderr, "li2 first value wrong, wanted %d, got %d\n",
        SIZE, list_first(li2)->val);
    exit(1);
    }
    // check that li2's last item is now 1
    if (list_last(li2)->val != 1) {
    fprintf(stderr, "last value wrong, wanted %d, got %d\n",
        SIZE, list_last(li2)->val);
    exit(1);
    }
    // check that li1's length is still SIZE
    if (list_length(li1) != SIZE) {
    fprintf(stderr, "li1 size wrong, wanted %d, got %d\n",
        SIZE, list_length(li1));
    exit(1);
    }
    // compare li1 and li2 for equality
    if (!list_equal(li1, li2)) {
    fprintf(stderr, "li1 and li2 are not equal\n");
    exit(1);
    }
    len = list_length(li1);
    free(li1);
    free(li2);
    free(li3);
    // return the length of the list
    return(len);
}

int main(int argc, char *argv[]) {
    int n = ((argc == 2) ? atoi(argv[1]) : 1);
    int result = 0;
    while(n--) result = test_lists();
    printf("%d\n", result);
    return 0;
}



Matrix Multiplication




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define SIZE 30

int **mkmatrix(int rows, int cols) {
    int i, j, count = 1;
    int **m = (int **) malloc(rows * sizeof(int *));
    for (i=0; i<rows; i++) {
    m[i] = (int *) malloc(cols * sizeof(int));
    for (j=0; j<cols; j++) {
        m[i][j] = count++;
    }
    }
    return(m);
}

void zeromatrix(int rows, int cols, int **m) {
    int i, j;
    for (i=0; i<rows; i++)
    for (j=0; j<cols; j++)
        m[i][j] = 0;
}

void freematrix(int rows, int **m) {
    while (--rows > -1) { free(m[rows]); }
    free(m);
}

int **mmult(int rows, int cols, int **m1, int **m2, int **m3) {
    int i, j, k, val;
    for (i=0; i<rows; i++) {
    for (j=0; j<cols; j++) {
        val = 0;
        for (k=0; k<cols; k++) {
        val += m1[i][k] * m2[k][j];
        }
        m3[i][j] = val;
    }
    }
    return(m3);
}

int main(int argc, char *argv[]) {
    int i, n = ((argc == 2) ? atoi(argv[1]) : 1);
    
    int **m1 = mkmatrix(SIZE, SIZE);
    int **m2 = mkmatrix(SIZE, SIZE);
    int **mm = mkmatrix(SIZE, SIZE);

    for (i=0; i<n; i++) {
    mm = mmult(SIZE, SIZE, m1, m2, mm);
    }
    printf("%d %d %d %d\n", mm[0][0], mm[2][3], mm[3][2], mm[4][4]);

    freematrix(SIZE, m1);
    freematrix(SIZE, m2);
    freematrix(SIZE, mm);
    return(0);
}



Method Calls




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>

#define true  1
#define false 0


#define TOGGLE \
    char state; \
    char (*value)(struct Toggle *); \
    struct Toggle *(*activate)(struct Toggle *)

#define DESTROY  free

typedef struct Toggle {
    TOGGLE;
} Toggle;

char toggle_value(Toggle *this) {
    return(this->state);
}
Toggle *toggle_activate(Toggle *this) {
    this->state = !this->state;
    return(this);
}
Toggle *init_Toggle(Toggle *this, char start_state) {
    this->state = start_state;
    this->value = toggle_value;
    this->activate = toggle_activate;
    return(this);
}
Toggle *new_Toggle(char start_state) {
    Toggle *this = (Toggle *)malloc(sizeof(Toggle));
    return(init_Toggle(this, start_state));
}


typedef struct NthToggle {
    TOGGLE;
    int count_max;
    int counter;
} NthToggle;

NthToggle *nth_toggle_activate(NthToggle *this) {
    if (++this->counter >= this->count_max) {
    this->state = !this->state;
    this->counter = 0;
    }
    return(this);
}
NthToggle *init_NthToggle(NthToggle *this, int max_count) {
    this->count_max = max_count;
    this->counter = 0;
    this->activate = (Toggle *(*)(Toggle *))nth_toggle_activate;
    return(this);
}
NthToggle *new_NthToggle(char start_state, int max_count) {
    NthToggle *this = (NthToggle *)malloc(sizeof(NthToggle));
    this = (NthToggle *)init_Toggle((Toggle *)this, start_state);
    return(init_NthToggle(this, max_count));
}


int main(int argc, char *argv[]) {
    int i, n = ((argc == 2) ? atoi(argv[1]) : 1);
    Toggle *tog;
    NthToggle *ntog;
    char val = true;

    tog = new_Toggle(true);
    for (i=0; i<n; i++) {
    val = tog->activate(tog)->value(tog);
    }
    fputs(val ? "true\n" : "false\n", stdout);
    DESTROY(tog);
    
    val = true;
    ntog = new_NthToggle(val, 3);
    for (i=0; i<n; i++) {
    val = ntog->activate(ntog)->value(ntog);
    }
    fputs(val ? "true\n" : "false\n", stdout);
    DESTROY(ntog);
    return 0;
}



Nested Loops




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int
main(int argc, char *argv[]) {
    int n = ((argc == 2) ? atoi(argv[1]) : 1);
    int a, b, c, d, e, f, x=0;
    
    for (a=0; a<n; a++)
    for (b=0; b<n; b++)
        for (c=0; c<n; c++)
        for (d=0; d<n; d++)
            for (e=0; e<n; e++)
            for (f=0; f<n; f++)
                x++;

    printf("%d\n", x);
    return(0);
}



Object Instantiation




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>


enum {false, true};

#define TOGGLE \
    char state; \
    char (*value)(struct Toggle *); \
    struct Toggle *(*activate)(struct Toggle *)

#define DESTROY  free

typedef struct Toggle {
    TOGGLE;
} Toggle;

char toggle_value(Toggle *this) {
    return(this->state);
}
Toggle *toggle_activate(Toggle *this) {
    this->state = !this->state;
    return(this);
}
Toggle *init_Toggle(Toggle *this, char start_state) {
    this->state = start_state;
    this->value = toggle_value;
    this->activate = toggle_activate;
    return(this);
}
Toggle *new_Toggle(char start_state) {
    Toggle *this = (Toggle *)malloc(sizeof(Toggle));
    return(init_Toggle(this, start_state));
}


typedef struct NthToggle {
    TOGGLE;
    int count_max;
    int counter;
} NthToggle;

NthToggle *nth_toggle_activate(NthToggle *this) {
    if (++this->counter >= this->count_max) {
    this->state = !this->state;
    this->counter = 0;
    }
    return(this);
}
NthToggle *init_NthToggle(NthToggle *this, int max_count) {
    this->count_max = max_count;
    this->counter = 0;
    this->activate = (Toggle *(*)(Toggle *))nth_toggle_activate;
    return(this);
}
NthToggle *new_NthToggle(char start_state, int max_count) {
    NthToggle *this = (NthToggle *)malloc(sizeof(NthToggle));
    this = (NthToggle *)init_Toggle((Toggle *)this, start_state);
    return(init_NthToggle(this, max_count));
}


int main(int argc, char *argv[]) {
    int i, n = ((argc == 2) ? atoi(argv[1]) : 1);
    Toggle *tog;
    NthToggle *ntog;

    tog = new_Toggle(true);
    for (i=0; i<5; i++) {
    fputs((tog->activate(tog)->value(tog)) ? "true\n" : "false\n", stdout);
    }
    DESTROY(tog);
    for (i=0; i<n; i++) {
    tog = new_Toggle(true);
    DESTROY(tog);
    }
    
    fputs("\n", stdout);

    ntog = new_NthToggle(true, 3);
    for (i=0; i<8; i++) {
    fputs((ntog->activate(ntog)->value(ntog)) ? "true\n" : "false\n",
stdout);
    }
    DESTROY(ntog);
    for (i=0; i<n; i++) {
    ntog = new_NthToggle(true, 3);
    DESTROY(ntog);
    }
    return 0;
}



Producer/Consumer Threads




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <pthread.h>

pthread_mutex_t mutex;
pthread_cond_t control;
void producer(unsigned int *arg);
void consumer(unsigned int *arg);
unsigned int count, data, consumed, produced;


int
main(int argc, char *argv[]) {
    unsigned int n = ((argc == 2) ? atoi(argv[1]) : 1);
    pthread_t t1, t2;
    
    count = data = consumed = produced = 0;

    if (pthread_mutex_init(&mutex, NULL)) {
    perror("pthread_mutex_init");
    exit(1);
    }
    if (pthread_cond_init(&control, NULL)) {
    perror("pthread_cond_init");
    exit(1);
    }
    if (pthread_create(&t1, (pthread_attr_t *)NULL,
               (void * (*)(void *))producer, (void *)&n)) {
    perror("pthread_create");
    exit(1);
    }
    if (pthread_create(&t2, (pthread_attr_t *)NULL,
               (void * (*)(void *))consumer, (void *)&n)) {
    perror("pthread_create");
    exit(1);
    }
  
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    fprintf(stdout, "%d %d\n", produced, consumed);
    return(0);
}


void producer(unsigned int *arg) {
    unsigned int i, n = *arg;
    for (i=1; i<=n; i++) {
    pthread_mutex_lock(&mutex);
    while (count == 1) {
        pthread_cond_wait(&control, &mutex);
    }
    data = i;
    count = 1;
    pthread_cond_signal(&control);
    pthread_mutex_unlock(&mutex);
    produced++;
    }
}
 

void consumer(unsigned int *arg) {
    unsigned int i = 0, n = *arg;
    while (1) {
    pthread_mutex_lock(&mutex);
    while (count == 0) {
        pthread_cond_wait(&control, &mutex);
    }
    i = data;
    count = 0;
    pthread_cond_signal(&control);
    pthread_mutex_unlock(&mutex);
    consumed++;
    if (i == n) return;
    }
}




Random Number Generator




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define IM 139968
#define IA 3877
#define IC 29573

double
gen_random(double max) {
    static long last = 42;
    
    last = (last * IA + IC) % IM;
    return( max * last / IM );
}

int
main(int argc, char *argv[]) {
    int N = ((argc == 2) ? atoi(argv[1]) : 1);
    double result = 0;
    
    while (N--) {
    result = gen_random(100.0);
    }
    printf("%.9f\n", result);
    return(0);
}



Regular Expression Matching




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <pcre.h>
#include <string.h>

#define MAXLINES   100
#define MAXLINELEN 132

char *pattern = 
"(?:^|[^\\d\\(])"        
"(\\()?"            
"(\\d\\d\\d)"            
"(?(1)\\))"            
"[ ]"                
"(\\d\\d\\d)"            
"[ -]"                
"(\\d\\d\\d\\d)"        
"\\D"                
;


int
main(int argc, char *argv[]) {
    int NUM = ((argc == 2) ? atoi(argv[1]) : 1);
    int count;
    char *cptr = "";
    char **phones;
    pcre *re;
    int erroffset;
    const char *errptr;
    int n, lines = 0;
    char num[256];
    int i, j, k, matchlen;
    char *matchoffset;
    int nmatches;
    int *ovec, ovecsize;
    pcre_extra *study;

    phones = (char **)malloc(MAXLINES * sizeof(char *));
    if (!phones) {
    fprintf(stderr, "malloc for phones array failed\n");
    exit(1);
    }
    lines = 0;
    while (cptr) {
    phones[lines] = (char *)malloc(MAXLINELEN);
    if (!phones[lines]) {
        fprintf(stderr, "malloc to hold line #%d failed\n", lines);
        exit(1);
    }
    cptr = fgets(phones[lines], MAXLINELEN, stdin);
    lines++;
    if (lines > MAXLINES) {
        fprintf(stderr, "MAXLINES is too small\n");
        exit(1);
    }
    }

    re = pcre_compile(pattern, 0, &errptr, &erroffset, NULL);
    if (!re) {
    fprintf(stderr, "can't open compile regexp\n");
    exit(1);
    }

    study = pcre_study(re, 0, &errptr);

    if (pcre_fullinfo(re, NULL, PCRE_INFO_CAPTURECOUNT, &nmatches) != 0) {
    fprintf(stderr, "pcre_fullinfo failed\n");
    exit(1);
    }
    nmatches++;            

    ovecsize = sizeof(int) * nmatches * 3;
    ovec = (int *)malloc(ovecsize);
    if (!ovec) {
    fprintf(stderr, "malloc for ovec array failed\n");
    exit(1);
    }

    count = 0;
    while (NUM--) {
    for (i=0; i<lines; i++) {
        n = pcre_exec(re, study,
              phones[i], strlen(phones[i]), 0,
              0, ovec, ovecsize);
        if (n == nmatches) {
        
        k = 2*2;    
        
        j = 0;
        num[j++] = '(';
        matchoffset = phones[i] + ovec[k];
        matchlen = ovec[k+1] - ovec[k];
        strncpy(num+j, matchoffset, matchlen);
        j += matchlen; k += 2;
        num[j++] = ')';
        
        num[j++] = ' ';
        
        matchoffset = phones[i] + ovec[k];
        matchlen = ovec[k+1] - ovec[k];
        strncpy(num+j, matchoffset, matchlen);
        j += matchlen; k += 2;
        
        num[j++] = '-';
        
        matchoffset = phones[i] + ovec[k];
        matchlen = ovec[k+1] - ovec[k];
        strncpy(num+j, matchoffset, matchlen);
        j += matchlen; k += 2;
        
        num[j] = 0;
        if (0 == NUM) {
            count++;
            printf("%d: %s\n", count, num);
        }
        }
    }
    }

    for (i=0; i<MAXLINES; i++) {
    free(phones[i]);
    }
    free(phones);
    free(ovec);

    return(0);
}




Reverse a File




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAXREAD 4096

int main(int argc, char *argv[]) {
    int nread, len = 0, size = (4 * MAXREAD);
    char *cp, *offset, *buf = malloc(size + 1);

    while (1) {
    if ((nread = read(0, (buf + len), MAXREAD)) > 0) {
        len += nread;
        if (MAXREAD > (size - len)) {
        size <<= 1;
        if (0 == (buf = realloc(buf, size + 1))) {
            fprintf(stderr, "realloc failed\n");
            exit(1);
        }
        }
    } else {
        if ( 0 == nread) break;
        if (-1 == nread) {
        perror("read");
        exit(1);
        }
    }    
    }
    offset = buf + len;
    *offset = 0;
    for (cp = offset; cp > buf; --cp) {
    if ('\n' == *cp) {
        *offset = 0;
        if (cp < offset)
        fputs(offset = cp+1, stdout);
    }
    }
    if (cp < offset) {
    *offset = 0;
    fputs(cp, stdout);
    }
    free(buf);
    return(0);
}



Sieve of Erathostenes




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char *argv[]) {
    int NUM = ((argc == 2) ? atoi(argv[1]) : 1);
    static char flags[8192 + 1];
    long i, k;
    int count = 0;

    while (NUM--) {
    count = 0; 
    for (i=2; i <= 8192; i++) {
        flags[i] = 1;
    }
    for (i=2; i <= 8192; i++) {
        if (flags[i]) {
        // remove all multiples of prime: i
        for (k=i+i; k <= 8192; k+=i) {
            flags[k] = 0;
        }
        count++;
        }
    }
    }
    printf("Count: %d\n", count);
    return(0);
}




Spell Checker




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 * with help from Brad Knotwell
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "simple_hash.h"

#define MAXLINELEN 128

struct ht_ht *dict = NULL;

int handleInput(FILE *input,void (*hashManipFn)(char *))
{
    int wordbufsize = 80,i = 0;
    char *cp, *wordbuf = (char *)malloc(wordbufsize + 1);
    char line[MAXLINELEN];
    
    if((wordbuf = malloc(wordbufsize+1)) == NULL) 
        return(fprintf(stderr,"malloc\n"),0);

    while (fgets(line, MAXLINELEN, input))
    for (cp=line; *cp > 0; cp++) {
        if (isspace(*cp)) {
        if (i) {
            wordbuf[i] = '\0';
                    hashManipFn(wordbuf);
            i = 0;
        }
        } else {
        wordbuf[i++] = *cp;
        if (i == wordbufsize) {
            wordbufsize *= 2;
            if((wordbuf = realloc(wordbuf, wordbufsize + 1)) == NULL)
                        return(fprintf(stderr, "realloc\n"), 0);
        }
        }
        }

    free(wordbuf);
    return(1);
}

void spellCheck(char *key) { 
    if (ht_find_new(dict,key)->val != 1) printf("%s\n",key);
}

void hashLoad(char *key) { ht_find_new(dict,key)->val = 1; }
 
int main(int argc, char *argv[]) {
    FILE *fh;
    int rc;

    /*  
        ht_create doesn't handle malloc and calloc failures 
        so this is superfluous 
    */
    if((dict = ht_create(40000)) == NULL)
        return(fprintf(stderr,"hash creation failed\n"),EXIT_FAILURE);
    
    if ((fh = fopen("Usr.Dict.Words", "r")) == NULL) 
        return(fprintf(stderr,"couldn't open dictionary\n"),EXIT_FAILURE);

    rc = ((handleInput(fh,hashLoad) && handleInput(stdin,spellCheck)) ?
EXIT_SUCCESS : EXIT_FAILURE);

    ht_destroy(dict);
    return(rc);
}



Statistical Moments




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAXLINELEN 128

int
compare_doubles (const double *a, const double *b) {
    return (int) (*a - *b);
}


int
main() {
    char line[MAXLINELEN];
    int i, n = 0, mid = 0;
    double sum = 0.0;
    double mean = 0.0;
    double average_deviation = 0.0;
    double standard_deviation = 0.0;
    double variance = 0.0;
    double skew = 0.0;
    double kurtosis = 0.0;
    double median = 0.0;
    double deviation = 0.0;
    int array_size = 4096;

    double *nums = (double *)malloc(array_size * sizeof(double));

    while (fgets(line, MAXLINELEN, stdin)) {
    sum += (nums[n++] = atof(line));
    if (n == array_size) {
        array_size *= 2;
        nums = (double *)realloc(nums, array_size * sizeof(double));
    }
    }
    mean = sum/n;
    for (i=0; i<n; i++) {
    deviation = nums[i] - mean;
    average_deviation += fabs(deviation);
    variance += pow(deviation,2);
    skew += pow(deviation,3);
    kurtosis += pow(deviation,4);
    }
    average_deviation /= n;
    variance /= (n - 1);
    standard_deviation = sqrt(variance);
    if (variance) {
    skew /= (n * variance * standard_deviation);
    kurtosis = (kurtosis/(n * variance * variance)) - 3.0;
    }

    qsort(nums, n, sizeof (double),
      (int (*)(const void *, const void *))compare_doubles);
    mid = (n/2);
    median = n % 2 ? nums[mid] : (nums[mid] + nums[mid-1])/2;

    free(nums);

    printf("n:                  %d\n", n);
    printf("median:             %f\n", median);
    printf("mean:               %f\n", mean);
    printf("average_deviation:  %f\n", average_deviation);
    printf("standard_deviation: %f\n", standard_deviation);
    printf("variance:           %f\n", variance);
    printf("skew:               %f\n", skew);
    printf("kurtosis:           %f\n", kurtosis);
    return(0);
}



String Concatenation




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define STUFF "hello\n"

int
main(int argc, char *argv[]) {
    int n = ((argc == 2) ? atoi(argv[1]) : 1);
    int i, buflen = 32;
    char *strbuf = calloc(sizeof(char), buflen);
    char *strend = strbuf;
    int stufflen = strlen(STUFF);

    if (!strbuf) { perror("calloc strbuf"); exit(1); }
    for (i=0; i<n; i++) {
    if (((strbuf+buflen)-strend) < (stufflen+1)) {
        buflen = 2*buflen;
        strbuf = realloc(strbuf, buflen);
        if (!strbuf) { perror("realloc strbuf"); exit(1); }
        strend = strbuf + strlen(strbuf);
    }
    
    strcat(strend, STUFF);
    strend += stufflen;
    }
    fprintf(stdout, "%d\n", strlen(strbuf));
    free(strbuf);

    return(0);
}



Sum a Column of Integers




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <stdlib.h>

#define MAXLINELEN 128

int
main() {
    int sum = 0;
    char line[MAXLINELEN];

    while (fgets(line, MAXLINELEN, stdin)) {
    sum += atoi(line);
    }
    printf("%d\n", sum);
    return(0);
}




Word Frequency Count




/* -*- mode: c -*-
 * $Id: bench.c,v 1.1 2002/06/05 20:34:46 michael Exp $
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdio.h>
#include <ctype.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#include "simple_hash.h"

typedef int (*comparator)(const void *, const void *);

int cmp_hash(struct ht_node **a, struct ht_node **b) {
    int val = (*b)->val - (*a)->val;
    return((val == 0) ? strcmp((*b)->key, (*a)->key) : val);
}

int main() {
    int bufsize = 80;
    char *buf = (char *)malloc(bufsize + 1);
    char c;
    int i = 0;
    struct ht_ht *ht = ht_create(75000);
    struct ht_node **sort_array, **sort_tmp, *node;

    while ((c = getchar()) > 0) {
        if (isalpha(c)) {
            buf[i++] = tolower(c);
        if (i == bufsize) {
        bufsize *= 2;
        buf = realloc(buf, bufsize + 1);
        } 
        } else {
        if (i > 0) {
        buf[i] = '\0';
        ++(ht_find_new(ht, buf)->val);
        i = 0;
        }
        }
    }
    free(buf);

    sort_array = sort_tmp =
    malloc(sizeof(struct ht_node *) * ht_count(ht));

    for (node=ht_first(ht); (*sort_tmp++ = node) != 0; node=ht_next(ht)) ;

    qsort(sort_array, ht_count(ht), sizeof(struct ht_node *),
      (comparator)cmp_hash);

    for (i=0; i<ht_count(ht); i++)
    printf("%7d\t%s\n", ht_val(sort_array[i]), ht_key(sort_array[i])); 

    ht_destroy(ht);
    return(0);
}
