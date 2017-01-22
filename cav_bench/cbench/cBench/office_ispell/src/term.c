static char Rcs_Id[] = "$Id: term.c,v 1.48 1994/10/25 05:46:11 geoff Exp $";
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;
__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned long long int __u_quad_t;
__extension__ typedef __u_quad_t __dev_t;
__extension__ typedef unsigned int __uid_t;
__extension__ typedef unsigned int __gid_t;
__extension__ typedef unsigned long int __ino_t;
__extension__ typedef __u_quad_t __ino64_t;
__extension__ typedef unsigned int __mode_t;
__extension__ typedef unsigned int __nlink_t;
__extension__ typedef long int __off_t;
__extension__ typedef __quad_t __off64_t;
__extension__ typedef int __pid_t;
__extension__ typedef struct {
    int __val[2];
} __fsid_t;
__extension__ typedef long int __clock_t;
__extension__ typedef unsigned long int __rlim_t;
__extension__ typedef __u_quad_t __rlim64_t;
__extension__ typedef unsigned int __id_t;
__extension__ typedef long int __time_t;
__extension__ typedef unsigned int __useconds_t;
__extension__ typedef long int __suseconds_t;
__extension__ typedef int __daddr_t;
__extension__ typedef long int __swblk_t;
__extension__ typedef int __key_t;
__extension__ typedef int __clockid_t;
__extension__ typedef void *__timer_t;
__extension__ typedef long int __blksize_t;
__extension__ typedef long int __blkcnt_t;
__extension__ typedef __quad_t __blkcnt64_t;
__extension__ typedef unsigned long int __fsblkcnt_t;
__extension__ typedef __u_quad_t __fsblkcnt64_t;
__extension__ typedef unsigned long int __fsfilcnt_t;
__extension__ typedef __u_quad_t __fsfilcnt64_t;
__extension__ typedef int __ssize_t;
typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;
__extension__ typedef int __intptr_t;
__extension__ typedef unsigned int __socklen_t;
typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;
typedef __loff_t loff_t;
typedef __ino_t ino_t;
typedef __dev_t dev_t;
typedef __gid_t gid_t;
typedef __mode_t mode_t;
typedef __nlink_t nlink_t;
typedef __uid_t uid_t;
typedef __off_t off_t;
typedef __pid_t pid_t;
typedef __id_t id_t;
typedef __ssize_t ssize_t;
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;
typedef __key_t key_t;
typedef __time_t time_t;
typedef __clockid_t clockid_t;
typedef __timer_t timer_t;
typedef __typeof__(((int *) 0) - ((int *) 0)) ptrdiff_t;
typedef __typeof__(sizeof(int)) size_t;
typedef int wchar_t;
typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
typedef int int8_t __attribute__ ((__mode__(__QI__)));
typedef int int16_t __attribute__ ((__mode__(__HI__)));
typedef int int32_t __attribute__ ((__mode__(__SI__)));
typedef int int64_t __attribute__ ((__mode__(__DI__)));
typedef unsigned int u_int8_t __attribute__ ((__mode__(__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__(__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__(__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__(__DI__)));
typedef int register_t __attribute__ ((__mode__(__word__)));
typedef int __sig_atomic_t;
typedef struct {
    unsigned long int __val[(1024 / (8 * sizeof(unsigned long int)))];
} __sigset_t;
typedef __sigset_t sigset_t;
struct timespec {
    __time_t tv_sec;
    long int tv_nsec;
};
struct timeval {
    __time_t tv_sec;
    __suseconds_t tv_usec;
};
typedef __suseconds_t suseconds_t;
typedef long int __fd_mask;
typedef struct {
    __fd_mask __fds_bits[1024 / (8 * (int) sizeof(__fd_mask))];
}
fd_set;
typedef __fd_mask fd_mask;
extern int select(int __nfds, fd_set * __restrict __readfds, fd_set * __restrict __writefds, fd_set * __restrict __exceptfds, struct timeval *__restrict __timeout);
extern int pselect(int __nfds, fd_set * __restrict __readfds, fd_set * __restrict __writefds, fd_set * __restrict __exceptfds, const struct timespec *__restrict __timeout, const __sigset_t * __restrict __sigmask);
__extension__ extern unsigned int gnu_dev_major(unsigned long long int __dev) __attribute__ ((__nothrow__));
__extension__ extern unsigned int gnu_dev_minor(unsigned long long int __dev) __attribute__ ((__nothrow__));
__extension__ extern unsigned long long int gnu_dev_makedev(unsigned int __major, unsigned int __minor) __attribute__ ((__nothrow__));
typedef __blkcnt_t blkcnt_t;
typedef __fsblkcnt_t fsblkcnt_t;
typedef __fsfilcnt_t fsfilcnt_t;
typedef unsigned long int pthread_t;
typedef union {
    char __size[36];
    long int __align;
} pthread_attr_t;
typedef struct __pthread_internal_slist {
    struct __pthread_internal_slist *__next;
} __pthread_slist_t;
typedef union {
    struct __pthread_mutex_s {
	int __lock;
	unsigned int __count;
	int __owner;
	int __kind;
	unsigned int __nusers;
	__extension__ union {
	    int __spins;
	    __pthread_slist_t __list;
	};
    } __data;
    char __size[24];
    long int __align;
} pthread_mutex_t;
typedef union {
    char __size[4];
    long int __align;
} pthread_mutexattr_t;
typedef union {
    struct {
	int __lock;
	unsigned int __futex;
	__extension__ unsigned long long int __total_seq;
	__extension__ unsigned long long int __wakeup_seq;
	__extension__ unsigned long long int __woken_seq;
	void *__mutex;
	unsigned int __nwaiters;
	unsigned int __broadcast_seq;
    } __data;
    char __size[48];
    __extension__ long long int __align;
} pthread_cond_t;
typedef union {
    char __size[4];
    long int __align;
} pthread_condattr_t;
typedef unsigned int pthread_key_t;
typedef int pthread_once_t;
typedef union {
    struct {
	int __lock;
	unsigned int __nr_readers;
	unsigned int __readers_wakeup;
	unsigned int __writer_wakeup;
	unsigned int __nr_readers_queued;
	unsigned int __nr_writers_queued;
	unsigned char __flags;
	unsigned char __shared;
	unsigned char __pad1;
	unsigned char __pad2;
	int __writer;
    } __data;
    char __size[32];
    long int __align;
} pthread_rwlock_t;
typedef union {
    char __size[8];
    long int __align;
} pthread_rwlockattr_t;
typedef volatile int pthread_spinlock_t;
typedef union {
    char __size[20];
    long int __align;
} pthread_barrier_t;
typedef union {
    char __size[4];
    int __align;
} pthread_barrierattr_t;
struct dirent {
    __ino_t d_ino;
    __off_t d_off;
    unsigned short int d_reclen;
    unsigned char d_type;
    char d_name[256];
};
enum { DT_UNKNOWN = 0, DT_FIFO = 1, DT_CHR = 2, DT_DIR = 4, DT_BLK = 6, DT_REG = 8, DT_LNK = 10, DT_SOCK = 12, DT_WHT = 14 };
typedef struct __dirstream DIR;
extern DIR *opendir(__const char *__name) __attribute__ ((__nonnull__(1)));
extern DIR *fdopendir(int __fd);
extern int closedir(DIR * __dirp) __attribute__ ((__nonnull__(1)));
extern struct dirent *readdir(DIR * __dirp) __attribute__ ((__nonnull__(1)));
extern int readdir_r(DIR * __restrict __dirp, struct dirent *__restrict __entry, struct dirent **__restrict __result) __attribute__ ((__nonnull__(1, 2, 3)));
extern void rewinddir(DIR * __dirp) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern void seekdir(DIR * __dirp, long int __pos) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern long int telldir(DIR * __dirp) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int dirfd(DIR * __dirp) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int scandir(__const char *__restrict __dir, struct dirent ***__restrict __namelist, int (*__selector) (__const struct dirent *), int (*__cmp) (__const struct dirent **, __const struct dirent **)) __attribute__ ((__nonnull__(1, 2)));
extern int alphasort(__const struct dirent **__e1, __const struct dirent **__e2) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern __ssize_t getdirentries(int __fd, char *__restrict __buf, size_t __nbytes, __off_t * __restrict __basep) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2, 4)));
struct _IO_FILE;
typedef struct _IO_FILE FILE;
typedef struct _IO_FILE __FILE;
typedef struct {
    int __count;
    union {
	unsigned int __wch;
	char __wchb[4];
    } __value;
} __mbstate_t;
typedef struct {
    __off_t __pos;
    __mbstate_t __state;
} _G_fpos_t;
typedef struct {
    __off64_t __pos;
    __mbstate_t __state;
} _G_fpos64_t;
typedef int _G_int16_t __attribute__ ((__mode__(__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__(__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__(__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__(__SI__)));
typedef __builtin_va_list va_list;
typedef __builtin_va_list __gnuc_va_list;
struct _IO_jump_t;
struct _IO_FILE;
typedef void _IO_lock_t;
struct _IO_marker {
    struct _IO_marker *_next;
    struct _IO_FILE *_sbuf;
    int _pos;
};
enum __codecvt_result { __codecvt_ok, __codecvt_partial, __codecvt_error, __codecvt_noconv };
struct _IO_FILE {
    int _flags;
    char *_IO_read_ptr;
    char *_IO_read_end;
    char *_IO_read_base;
    char *_IO_write_base;
    char *_IO_write_ptr;
    char *_IO_write_end;
    char *_IO_buf_base;
    char *_IO_buf_end;
    char *_IO_save_base;
    char *_IO_backup_base;
    char *_IO_save_end;
    struct _IO_marker *_markers;
    struct _IO_FILE *_chain;
    int _fileno;
    int _flags2;
    __off_t _old_offset;
    unsigned short _cur_column;
    signed char _vtable_offset;
    char _shortbuf[1];
    _IO_lock_t *_lock;
    __off64_t _offset;
    void *__pad1;
    void *__pad2;
    void *__pad3;
    void *__pad4;
    size_t __pad5;
    int _mode;
    char _unused2[15 * sizeof(int) - 4 * sizeof(void *) - sizeof(size_t)];
};
typedef struct _IO_FILE _IO_FILE;
struct _IO_FILE_plus;
extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
typedef __ssize_t __io_read_fn(void *__cookie, char *__buf, size_t __nbytes);
typedef __ssize_t __io_write_fn(void *__cookie, __const char *__buf, size_t __n);
typedef int __io_seek_fn(void *__cookie, __off64_t * __pos, int __w);
typedef int __io_close_fn(void *__cookie);
extern int __underflow(_IO_FILE *);
extern int __uflow(_IO_FILE *);
extern int __overflow(_IO_FILE *, int);
extern int _IO_getc(_IO_FILE * __fp);
extern int _IO_putc(int __c, _IO_FILE * __fp);
extern int _IO_feof(_IO_FILE * __fp) __attribute__ ((__nothrow__));
extern int _IO_ferror(_IO_FILE * __fp) __attribute__ ((__nothrow__));
extern int _IO_peekc_locked(_IO_FILE * __fp);
extern void _IO_flockfile(_IO_FILE *) __attribute__ ((__nothrow__));
extern void _IO_funlockfile(_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_ftrylockfile(_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_vfscanf(_IO_FILE * __restrict, const char *__restrict, __gnuc_va_list, int *__restrict);
extern int _IO_vfprintf(_IO_FILE * __restrict, const char *__restrict, __gnuc_va_list);
extern __ssize_t _IO_padn(_IO_FILE *, int, __ssize_t);
extern size_t _IO_sgetn(_IO_FILE *, void *, size_t);
extern __off64_t _IO_seekoff(_IO_FILE *, __off64_t, int, int);
extern __off64_t _IO_seekpos(_IO_FILE *, __off64_t, int);
extern void _IO_free_backup_area(_IO_FILE *) __attribute__ ((__nothrow__));
typedef _G_fpos_t fpos_t;
extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;
extern int remove(__const char *__filename) __attribute__ ((__nothrow__));
extern int rename(__const char *__old, __const char *__new) __attribute__ ((__nothrow__));
extern int renameat(int __oldfd, __const char *__old, int __newfd, __const char *__new) __attribute__ ((__nothrow__));
extern FILE *tmpfile(void);
extern char *tmpnam(char *__s) __attribute__ ((__nothrow__));
extern char *tmpnam_r(char *__s) __attribute__ ((__nothrow__));
extern char *tempnam(__const char *__dir, __const char *__pfx) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));
extern int fclose(FILE * __stream);
extern int fflush(FILE * __stream);
extern int fflush_unlocked(FILE * __stream);
extern FILE *fopen(__const char *__restrict __filename, __const char *__restrict __modes);
extern FILE *freopen(__const char *__restrict __filename, __const char *__restrict __modes, FILE * __restrict __stream);
extern FILE *fdopen(int __fd, __const char *__modes) __attribute__ ((__nothrow__));
extern FILE *fmemopen(void *__s, size_t __len, __const char *__modes) __attribute__ ((__nothrow__));
extern FILE *open_memstream(char **__bufloc, size_t * __sizeloc) __attribute__ ((__nothrow__));
extern void setbuf(FILE * __restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__));
extern int setvbuf(FILE * __restrict __stream, char *__restrict __buf, int __modes, size_t __n) __attribute__ ((__nothrow__));
extern void setbuffer(FILE * __restrict __stream, char *__restrict __buf, size_t __size) __attribute__ ((__nothrow__));
extern void setlinebuf(FILE * __stream) __attribute__ ((__nothrow__));
extern int fprintf(FILE * __restrict __stream, __const char *__restrict __format, ...);
extern int printf(__const char *__restrict __format, ...);
extern int sprintf(char *__restrict __s, __const char *__restrict __format, ...) __attribute__ ((__nothrow__));
extern int vfprintf(FILE * __restrict __s, __const char *__restrict __format, __gnuc_va_list __arg);
extern int vprintf(__const char *__restrict __format, __gnuc_va_list __arg);
extern int vsprintf(char *__restrict __s, __const char *__restrict __format, __gnuc_va_list __arg) __attribute__ ((__nothrow__));
extern int snprintf(char *__restrict __s, size_t __maxlen, __const char *__restrict __format, ...) __attribute__ ((__nothrow__)) __attribute__ ((__format__(__printf__, 3, 4)));
extern int vsnprintf(char *__restrict __s, size_t __maxlen, __const char *__restrict __format, __gnuc_va_list __arg) __attribute__ ((__nothrow__)) __attribute__ ((__format__(__printf__, 3, 0)));
extern int vdprintf(int __fd, __const char *__restrict __fmt, __gnuc_va_list __arg) __attribute__ ((__format__(__printf__, 2, 0)));
extern int fscanf(FILE * __restrict __stream, __const char *__restrict __format, ...);
extern int scanf(__const char *__restrict __format, ...);
extern int sscanf(__const char *__restrict __s, __const char *__restrict __format, ...) __attribute__ ((__nothrow__));
extern int fscanf(FILE * __restrict __stream, __const char *__restrict __format, ...) __asm__("" "__isoc99_fscanf");
extern int scanf(__const char *__restrict __format, ...) __asm__("" "__isoc99_scanf");
extern int sscanf(__const char *__restrict __s, __const char *__restrict __format, ...) __asm__("" "__isoc99_sscanf") __attribute__ ((__nothrow__));
extern int vfscanf(FILE * __restrict __s, __const char *__restrict __format, __gnuc_va_list __arg) __attribute__ ((__format__(__scanf__, 2, 0)));
extern int vscanf(__const char *__restrict __format, __gnuc_va_list __arg) __attribute__ ((__format__(__scanf__, 1, 0)));
extern int vsscanf(__const char *__restrict __s, __const char *__restrict __format, __gnuc_va_list __arg) __attribute__ ((__nothrow__)) __attribute__ ((__format__(__scanf__, 2, 0)));
extern int vfscanf(FILE * __restrict __s, __const char *__restrict __format, __gnuc_va_list __arg) __asm__("" "__isoc99_vfscanf") __attribute__ ((__format__(__scanf__, 2, 0)));
extern int vscanf(__const char *__restrict __format, __gnuc_va_list __arg) __asm__("" "__isoc99_vscanf") __attribute__ ((__format__(__scanf__, 1, 0)));
extern int vsscanf(__const char *__restrict __s, __const char *__restrict __format, __gnuc_va_list __arg) __asm__("" "__isoc99_vsscanf") __attribute__ ((__nothrow__)) __attribute__ ((__format__(__scanf__, 2, 0)));
extern int fgetc(FILE * __stream);
extern int getc(FILE * __stream);
extern int getchar(void);
extern int getc_unlocked(FILE * __stream);
extern int getchar_unlocked(void);
extern int fgetc_unlocked(FILE * __stream);
extern int fputc(int __c, FILE * __stream);
extern int putc(int __c, FILE * __stream);
extern int putchar(int __c);
extern int fputc_unlocked(int __c, FILE * __stream);
extern int putc_unlocked(int __c, FILE * __stream);
extern int putchar_unlocked(int __c);
extern int getw(FILE * __stream);
extern int putw(int __w, FILE * __stream);
extern char *fgets(char *__restrict __s, int __n, FILE * __restrict __stream);
extern char *gets(char *__s);
extern __ssize_t __getdelim(char **__restrict __lineptr, size_t * __restrict __n, int __delimiter, FILE * __restrict __stream);
extern __ssize_t getdelim(char **__restrict __lineptr, size_t * __restrict __n, int __delimiter, FILE * __restrict __stream);
extern int fputs(__const char *__restrict __s, FILE * __restrict __stream);
extern int puts(__const char *__s);
extern int ungetc(int __c, FILE * __stream);
extern size_t fread(void *__restrict __ptr, size_t __size, size_t __n, FILE * __restrict __stream);
extern size_t fwrite(__const void *__restrict __ptr, size_t __size, size_t __n, FILE * __restrict __s);
extern size_t fread_unlocked(void *__restrict __ptr, size_t __size, size_t __n, FILE * __restrict __stream);
extern size_t fwrite_unlocked(__const void *__restrict __ptr, size_t __size, size_t __n, FILE * __restrict __stream);
extern int fseek(FILE * __stream, long int __off, int __whence);
extern long int ftell(FILE * __stream);
extern void rewind(FILE * __stream);
extern int fseeko(FILE * __stream, __off_t __off, int __whence);
extern __off_t ftello(FILE * __stream);
extern int fgetpos(FILE * __restrict __stream, fpos_t * __restrict __pos);
extern int fsetpos(FILE * __stream, __const fpos_t * __pos);
extern void clearerr(FILE * __stream) __attribute__ ((__nothrow__));
extern int feof(FILE * __stream) __attribute__ ((__nothrow__));
extern int ferror(FILE * __stream) __attribute__ ((__nothrow__));
extern void clearerr_unlocked(FILE * __stream) __attribute__ ((__nothrow__));
extern int feof_unlocked(FILE * __stream) __attribute__ ((__nothrow__));
extern int ferror_unlocked(FILE * __stream) __attribute__ ((__nothrow__));
extern void perror(__const char *__s);
extern int sys_nerr;
extern __const char *__const sys_errlist[];
extern int fileno(FILE * __stream) __attribute__ ((__nothrow__));
extern int fileno_unlocked(FILE * __stream) __attribute__ ((__nothrow__));
extern FILE *popen(__const char *__command, __const char *__modes);
extern int pclose(FILE * __stream);
extern char *ctermid(char *__s) __attribute__ ((__nothrow__));
extern void flockfile(FILE * __stream) __attribute__ ((__nothrow__));
extern int ftrylockfile(FILE * __stream) __attribute__ ((__nothrow__));
extern void funlockfile(FILE * __stream) __attribute__ ((__nothrow__));
typedef unsigned char ichar_t;
struct dent {
    struct dent *next;
    char *word;
    long mask[(32 / 32)];
};
struct flagent {
    ichar_t *strip;
    ichar_t *affix;
    short flagbit;
    short stripl;
    short affl;
    short numconds;
    short flagflags;
    char conds[128 + 100];
};
union ptr_union {
    struct flagptr *fp;
    struct flagent *ent;
};
struct flagptr {
    union ptr_union pu;
    int numents;
};
struct strchartype {
    char *name;
    char *deformatter;
    char *suffixes;
};
struct hashheader {
    unsigned short magic;
    unsigned short compileoptions;
    short maxstringchars;
    short maxstringcharlen;
    short compoundmin;
    short compoundbit;
    int stringsize;
    int lstringsize;
    int tblsize;
    int stblsize;
    int ptblsize;
    int sortval;
    int nstrchars;
    int nstrchartype;
    int strtypestart;
    char nrchars[5];
    char texchars[13];
    char compoundflag;
    char defhardflag;
    char flagmarker;
    unsigned short sortorder[128 + 100];
    ichar_t lowerconv[128 + 100];
    ichar_t upperconv[128 + 100];
    char wordchars[128 + 100];
    char upperchars[128 + 100];
    char lowerchars[128 + 100];
    char boundarychars[128 + 100];
    char stringstarts[128];
    char stringchars[100][10 + 1];
    unsigned int stringdups[100];
    int dupnos[100];
    unsigned short magic2;
};
struct success {
    struct dent *dictent;
    struct flagent *prefix;
    struct flagent *suffix;
};
extern char *BC;
extern char *cd;
extern char *cl;
extern char *cm;
extern char *ho;
extern char *nd;
extern char *so;
extern char *se;
extern int sg;
extern char *ti;
extern char *te;
extern int li;
extern int co;
extern int contextsize;
extern char contextbufs[10][8192];
extern int contextoffset;
extern char *currentchar;
extern char ctoken[100 + 20];
extern ichar_t itoken[100 + 20];
extern char termcap[2048];
extern char termstr[2048];
extern char *termptr;
extern int numhits;
extern struct success hits[10];
extern char *hashstrings;
extern struct hashheader hashheader;
extern struct dent *hashtbl;
extern int hashsize;
extern char hashname[4096];
extern int aflag;
extern int cflag;
extern int lflag;
extern int incfileflag;
extern int nodictflag;
extern int uerasechar;
extern int ukillchar;
extern unsigned int laststringch;
extern int defdupchar;
extern int numpflags;
extern int numsflags;
extern struct flagptr pflagindex[128 + 100];
extern struct flagent *pflaglist;
extern struct flagptr sflagindex[128 + 100];
extern struct flagent *sflaglist;
extern struct strchartype *chartypes;
extern FILE *infile;
extern FILE *outfile;
extern FILE *infile1;
extern FILE *outfile1;
extern char *askfilename;
extern int changes;
extern int readonly;
extern int quit;
extern char possibilities[100][100 + 20];
extern int pcount;
extern int maxposslen;
extern int easypossibilities;
extern int Trynum;
extern ichar_t Try[128 + 100];
extern int minimenusize;
extern int eflag;
extern int dumpflag;
extern int fflag;
extern int sflag;
extern int vflag;
extern int xflag;
extern int deftflag;
extern int tflag;
extern int prefstringchar;
extern int terse;
extern char tempfile[4096];
extern int minword;
extern int sortit;
extern int compoundflag;
extern int tryhardflag;
extern char *currentfile;
extern int math_mode;
extern char LaTeX_Mode;
extern int addvheader(struct dent *ent);
extern void askmode(void);
extern void backup(void);
extern int cap_ok(ichar_t * word, struct success *hit, int len);
extern int casecmp(char *a, char *b, int canonical);
extern void chupcase(char *s);
extern void checkfile(void);
extern void checkline(FILE * ofile);
extern void chk_aff(ichar_t * word, ichar_t * ucword, int len, int ignoreflagbits, int allhits, int pfxopts, int sfxopts);
extern int combinecaps(struct dent *hdr, struct dent *newent);
extern int compoundgood(ichar_t * word, int pfxopts);
extern void copyout(char **cc, int cnt);
extern void correct(char *ctok, int ctokl, ichar_t * itok, int itokl, char **curchar);
extern char *do_regex_lookup(char *expr, int whence);
extern void done(int);
extern void dumpmode(void);
extern void erase(void);
extern int expand_pre(char *croot, ichar_t * rootword, long mask[], int option, char *extra);
extern int expand_suf(char *croot, ichar_t * rootword, long mask[], int crossonly, int option, char *extra);
extern int findfiletype(char *name, int searchnames, int *deformatter);
extern void flagpr(ichar_t * word, int preflag, int prestrip, int preadd, int sufflag, int sufadd);
extern void givehelp(int interactive);
extern int good(ichar_t * word, int ignoreflagbits, int allhits, int pfxopts, int sfxopts);
extern int hash(ichar_t * word, int hashtablesize);
extern int ichartostr(char *out, ichar_t * in, int outlen, int canonical);
extern char *ichartosstr(ichar_t * in, int canonical);
extern int ins_root_cap(ichar_t * word, ichar_t * pattern, int prestrip, int preadd, int sufstrip, int sufadd, struct dent *firstdent, struct flagent *pfxent, struct flagent *sufent);
extern void inverse(void);
extern int linit(void);
extern struct dent *lookup(ichar_t * word, int dotree);
extern void lowcase(ichar_t * string);
extern int makedent(char *lbuf, int lbuflen, struct dent *d);
extern void makepossibilities(ichar_t * word);
extern void move(int row, int col);
extern void normal(void);
extern char *printichar(int in);
extern int shellescape(char *buf);
extern char *skipoverword(char *bufp);
extern void stop(void);
extern int stringcharlen(char *bufp, int canonical);
extern int strtoichar(ichar_t * out, char *in, int outlen, int canonical);
extern ichar_t *strtosichar(char *in, int canonical);
extern void terminit(void);
extern void toutent(FILE * outfile, struct dent *hent, int onlykeep);
extern void treeinit(char *persdict, char *LibDict);
extern void treeinsert(char *word, int wordlen, int keep);
extern struct dent *treelookup(ichar_t * word);
extern void treeoutput(void);
extern void upcase(ichar_t * string);
extern long whatcap(ichar_t * word);
extern char *xgets(char *string, int size, FILE * stream);
extern void yyinit(void);
extern int yyopen(char *file);
extern int yyparse(void);
extern void myfree(void *area);
extern void *mymalloc(unsigned int);
extern void *myrealloc(void *area, unsigned int size, unsigned int oldsize);
typedef struct {
    int quot;
    int rem;
} div_t;
typedef struct {
    long int quot;
    long int rem;
} ldiv_t;
__extension__ typedef struct {
    long long int quot;
    long long int rem;
} lldiv_t;
extern size_t __ctype_get_mb_cur_max(void) __attribute__ ((__nothrow__));
extern double atof(__const char *__nptr) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern int atoi(__const char *__nptr) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern long int atol(__const char *__nptr) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
__extension__ extern long long int atoll(__const char *__nptr) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern double strtod(__const char *__restrict __nptr, char **__restrict __endptr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern float strtof(__const char *__restrict __nptr, char **__restrict __endptr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern long double strtold(__const char *__restrict __nptr, char **__restrict __endptr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern long int strtol(__const char *__restrict __nptr, char **__restrict __endptr, int __base) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern unsigned long int strtoul(__const char *__restrict __nptr, char **__restrict __endptr, int __base) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
__extension__ extern long long int strtoq(__const char *__restrict __nptr, char **__restrict __endptr, int __base) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
__extension__ extern unsigned long long int strtouq(__const char *__restrict __nptr, char **__restrict __endptr, int __base) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
__extension__ extern long long int strtoll(__const char *__restrict __nptr, char **__restrict __endptr, int __base) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
__extension__ extern unsigned long long int strtoull(__const char *__restrict __nptr, char **__restrict __endptr, int __base) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern char *l64a(long int __n) __attribute__ ((__nothrow__));
extern long int a64l(__const char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern long int random(void) __attribute__ ((__nothrow__));
extern void srandom(unsigned int __seed) __attribute__ ((__nothrow__));
extern char *initstate(unsigned int __seed, char *__statebuf, size_t __statelen) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern char *setstate(char *__statebuf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
struct random_data {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
};
extern int random_r(struct random_data *__restrict __buf, int32_t * __restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int srandom_r(unsigned int __seed, struct random_data *__buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern int initstate_r(unsigned int __seed, char *__restrict __statebuf, size_t __statelen, struct random_data *__restrict __buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2, 4)));
extern int setstate_r(char *__restrict __statebuf, struct random_data *__restrict __buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int rand(void) __attribute__ ((__nothrow__));
extern void srand(unsigned int __seed) __attribute__ ((__nothrow__));
extern int rand_r(unsigned int *__seed) __attribute__ ((__nothrow__));
extern double drand48(void) __attribute__ ((__nothrow__));
extern double erand48(unsigned short int __xsubi[3]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern long int lrand48(void) __attribute__ ((__nothrow__));
extern long int nrand48(unsigned short int __xsubi[3]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern long int mrand48(void) __attribute__ ((__nothrow__));
extern long int jrand48(unsigned short int __xsubi[3]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern void srand48(long int __seedval) __attribute__ ((__nothrow__));
extern unsigned short int *seed48(unsigned short int __seed16v[3]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern void lcong48(unsigned short int __param[7]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
struct drand48_data {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    unsigned long long int __a;
};
extern int drand48_r(struct drand48_data *__restrict __buffer, double *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int erand48_r(unsigned short int __xsubi[3], struct drand48_data *__restrict __buffer, double *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int lrand48_r(struct drand48_data *__restrict __buffer, long int *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int nrand48_r(unsigned short int __xsubi[3], struct drand48_data *__restrict __buffer, long int *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int mrand48_r(struct drand48_data *__restrict __buffer, long int *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int jrand48_r(unsigned short int __xsubi[3], struct drand48_data *__restrict __buffer, long int *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int srand48_r(long int __seedval, struct drand48_data *__buffer) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern int seed48_r(unsigned short int __seed16v[3], struct drand48_data *__buffer) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int lcong48_r(unsigned short int __param[7], struct drand48_data *__buffer) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern void *malloc(size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));
extern void *calloc(size_t __nmemb, size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));
extern void *realloc(void *__ptr, size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
extern void free(void *__ptr) __attribute__ ((__nothrow__));
extern void cfree(void *__ptr) __attribute__ ((__nothrow__));
extern void *alloca(size_t __size) __attribute__ ((__nothrow__));
extern void *valloc(size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));
extern int posix_memalign(void **__memptr, size_t __alignment, size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern void abort(void) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern int atexit(void (*__func) (void)) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int on_exit(void (*__func) (int __status, void *__arg), void *__arg) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern void exit(int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern void _Exit(int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern char *getenv(__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern char *__secure_getenv(__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int putenv(char *__string) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int setenv(__const char *__name, __const char *__value, int __replace) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern int unsetenv(__const char *__name) __attribute__ ((__nothrow__));
extern int clearenv(void) __attribute__ ((__nothrow__));
extern char *mktemp(char *__template) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int mkstemp(char *__template) __attribute__ ((__nonnull__(1)));
extern int mkstemps(char *__template, int __suffixlen) __attribute__ ((__nonnull__(1)));
extern char *mkdtemp(char *__template) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int system(__const char *__command);
extern char *realpath(__const char *__restrict __name, char *__restrict __resolved) __attribute__ ((__nothrow__));
typedef int (*__compar_fn_t) (__const void *, __const void *);
extern void *bsearch(__const void *__key, __const void *__base, size_t __nmemb, size_t __size, __compar_fn_t __compar) __attribute__ ((__nonnull__(1, 2, 5)));
extern void qsort(void *__base, size_t __nmemb, size_t __size, __compar_fn_t __compar) __attribute__ ((__nonnull__(1, 4)));
extern int abs(int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long int labs(long int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
__extension__ extern long long int llabs(long long int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern div_t div(int __numer, int __denom) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern ldiv_t ldiv(long int __numer, long int __denom) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
__extension__ extern lldiv_t lldiv(long long int __numer, long long int __denom) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern char *ecvt(double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3, 4)));
extern char *fcvt(double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3, 4)));
extern char *gcvt(double __value, int __ndigit, char *__buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3)));
extern char *qecvt(long double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3, 4)));
extern char *qfcvt(long double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3, 4)));
extern char *qgcvt(long double __value, int __ndigit, char *__buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3)));
extern int ecvt_r(double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign, char *__restrict __buf, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3, 4, 5)));
extern int fcvt_r(double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign, char *__restrict __buf, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3, 4, 5)));
extern int qecvt_r(long double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign, char *__restrict __buf, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3, 4, 5)));
extern int qfcvt_r(long double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign, char *__restrict __buf, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(3, 4, 5)));
extern int mblen(__const char *__s, size_t __n) __attribute__ ((__nothrow__));
extern int mbtowc(wchar_t * __restrict __pwc, __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__));
extern int wctomb(char *__s, wchar_t __wchar) __attribute__ ((__nothrow__));
extern size_t mbstowcs(wchar_t * __restrict __pwcs, __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__));
extern size_t wcstombs(char *__restrict __s, __const wchar_t * __restrict __pwcs, size_t __n) __attribute__ ((__nothrow__));
extern int rpmatch(__const char *__response) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int posix_openpt(int __oflag);
extern int getloadavg(double __loadavg[], int __nelem) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern void *memcpy(void *__restrict __dest, __const void *__restrict __src, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern void *memmove(void *__dest, __const void *__src, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern void *memccpy(void *__restrict __dest, __const void *__restrict __src, int __c, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern void *memset(void *__s, int __c, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int memcmp(__const void *__s1, __const void *__s2, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern void *memchr(__const void *__s, int __c, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern char *strcpy(char *__restrict __dest, __const char *__restrict __src) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern char *strncpy(char *__restrict __dest, __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern char *strcat(char *__restrict __dest, __const char *__restrict __src) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern char *strncat(char *__restrict __dest, __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int strcmp(__const char *__s1, __const char *__s2) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern int strncmp(__const char *__s1, __const char *__s2, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern int strcoll(__const char *__s1, __const char *__s2) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern size_t strxfrm(char *__restrict __dest, __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
typedef struct __locale_struct {
    struct locale_data *__locales[13];
    const unsigned short int *__ctype_b;
    const int *__ctype_tolower;
    const int *__ctype_toupper;
    const char *__names[13];
} *__locale_t;
typedef __locale_t locale_t;
extern int strcoll_l(__const char *__s1, __const char *__s2, __locale_t __l) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2, 3)));
extern size_t strxfrm_l(char *__dest, __const char *__src, size_t __n, __locale_t __l) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2, 4)));
extern char *strdup(__const char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__(1)));
extern char *strndup(__const char *__string, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__(1)));
extern char *strchr(__const char *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern char *strrchr(__const char *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern size_t strcspn(__const char *__s, __const char *__reject) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern size_t strspn(__const char *__s, __const char *__accept) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern char *strpbrk(__const char *__s, __const char *__accept) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern char *strstr(__const char *__haystack, __const char *__needle) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern char *strtok(char *__restrict __s, __const char *__restrict __delim) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern char *__strtok_r(char *__restrict __s, __const char *__restrict __delim, char **__restrict __save_ptr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2, 3)));
extern char *strtok_r(char *__restrict __s, __const char *__restrict __delim, char **__restrict __save_ptr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2, 3)));
extern size_t strlen(__const char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern size_t strnlen(__const char *__string, size_t __maxlen) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern char *strerror(int __errnum) __attribute__ ((__nothrow__));
extern int strerror_r(int __errnum, char *__buf, size_t __buflen) __asm__("" "__xpg_strerror_r") __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern char *strerror_l(int __errnum, __locale_t __l) __attribute__ ((__nothrow__));
extern void __bzero(void *__s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern void bcopy(__const void *__src, void *__dest, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern void bzero(void *__s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int bcmp(__const void *__s1, __const void *__s2, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern char *index(__const char *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern char *rindex(__const char *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1)));
extern int ffs(int __i) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int strcasecmp(__const char *__s1, __const char *__s2) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern int strncasecmp(__const char *__s1, __const char *__s2, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
extern char *strsep(char **__restrict __stringp, __const char *__restrict __delim) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern char *strsignal(int __sig) __attribute__ ((__nothrow__));
extern char *__stpcpy(char *__restrict __dest, __const char *__restrict __src) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern char *stpcpy(char *__restrict __dest, __const char *__restrict __src) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern char *__stpncpy(char *__restrict __dest, __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern char *stpncpy(char *__restrict __dest, __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern char *index();
extern char *rindex();
extern int tgetent();
extern int tgetnum();
extern char *tgetstr();
extern char *tgoto();
extern char *tputs();
extern int __sigismember(__const __sigset_t *, int);
extern int __sigaddset(__sigset_t *, int);
extern int __sigdelset(__sigset_t *, int);
typedef __sig_atomic_t sig_atomic_t;
typedef union sigval {
    int sival_int;
    void *sival_ptr;
} sigval_t;
typedef struct siginfo {
    int si_signo;
    int si_errno;
    int si_code;
    union {
	int _pad[((128 / sizeof(int)) - 3)];
	struct {
	    __pid_t si_pid;
	    __uid_t si_uid;
	} _kill;
	struct {
	    int si_tid;
	    int si_overrun;
	    sigval_t si_sigval;
	} _timer;
	struct {
	    __pid_t si_pid;
	    __uid_t si_uid;
	    sigval_t si_sigval;
	} _rt;
	struct {
	    __pid_t si_pid;
	    __uid_t si_uid;
	    int si_status;
	    __clock_t si_utime;
	    __clock_t si_stime;
	} _sigchld;
	struct {
	    void *si_addr;
	} _sigfault;
	struct {
	    long int si_band;
	    int si_fd;
	} _sigpoll;
    } _sifields;
} siginfo_t;
enum { SI_ASYNCNL = -60, SI_TKILL = -6, SI_SIGIO, SI_ASYNCIO, SI_MESGQ, SI_TIMER, SI_QUEUE, SI_USER, SI_KERNEL = 0x80 };
enum { ILL_ILLOPC = 1, ILL_ILLOPN, ILL_ILLADR, ILL_ILLTRP, ILL_PRVOPC, ILL_PRVREG, ILL_COPROC, ILL_BADSTK };
enum { FPE_INTDIV = 1, FPE_INTOVF, FPE_FLTDIV, FPE_FLTOVF, FPE_FLTUND, FPE_FLTRES, FPE_FLTINV, FPE_FLTSUB };
enum { SEGV_MAPERR = 1, SEGV_ACCERR };
enum { BUS_ADRALN = 1, BUS_ADRERR, BUS_OBJERR };
enum { TRAP_BRKPT = 1, TRAP_TRACE };
enum { CLD_EXITED = 1, CLD_KILLED, CLD_DUMPED, CLD_TRAPPED, CLD_STOPPED, CLD_CONTINUED };
enum { POLL_IN = 1, POLL_OUT, POLL_MSG, POLL_ERR, POLL_PRI, POLL_HUP };
typedef struct sigevent {
    sigval_t sigev_value;
    int sigev_signo;
    int sigev_notify;
    union {
	int _pad[((64 / sizeof(int)) - 3)];
	__pid_t _tid;
	struct {
	    void (*_function) (sigval_t);
	    void *_attribute;
	} _sigev_thread;
    } _sigev_un;
} sigevent_t;
enum { SIGEV_SIGNAL = 0, SIGEV_NONE, SIGEV_THREAD, SIGEV_THREAD_ID = 4 };
typedef void (*__sighandler_t) (int);
extern __sighandler_t __sysv_signal(int __sig, __sighandler_t __handler) __attribute__ ((__nothrow__));
extern __sighandler_t signal(int __sig, __sighandler_t __handler) __attribute__ ((__nothrow__));
extern int kill(__pid_t __pid, int __sig) __attribute__ ((__nothrow__));
extern int killpg(__pid_t __pgrp, int __sig) __attribute__ ((__nothrow__));
extern int raise(int __sig) __attribute__ ((__nothrow__));
extern __sighandler_t ssignal(int __sig, __sighandler_t __handler) __attribute__ ((__nothrow__));
extern int gsignal(int __sig) __attribute__ ((__nothrow__));
extern void psignal(int __sig, __const char *__s);
extern void psiginfo(__const siginfo_t * __pinfo, __const char *__s);
extern int __sigpause(int __sig_or_mask, int __is_sig);
extern int sigblock(int __mask) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int sigsetmask(int __mask) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int siggetmask(void) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
typedef __sighandler_t sig_t;
extern int sigemptyset(sigset_t * __set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int sigfillset(sigset_t * __set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int sigaddset(sigset_t * __set, int __signo) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int sigdelset(sigset_t * __set, int __signo) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int sigismember(__const sigset_t * __set, int __signo) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
struct sigaction {
    union {
	__sighandler_t sa_handler;
	void (*sa_sigaction) (int, siginfo_t *, void *);
    } __sigaction_handler;
    __sigset_t sa_mask;
    int sa_flags;
    void (*sa_restorer) (void);
};
extern int sigprocmask(int __how, __const sigset_t * __restrict __set, sigset_t * __restrict __oset) __attribute__ ((__nothrow__));
extern int sigsuspend(__const sigset_t * __set) __attribute__ ((__nonnull__(1)));
extern int sigaction(int __sig, __const struct sigaction *__restrict __act, struct sigaction *__restrict __oact) __attribute__ ((__nothrow__));
extern int sigpending(sigset_t * __set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int sigwait(__const sigset_t * __restrict __set, int *__restrict __sig) __attribute__ ((__nonnull__(1, 2)));
extern int sigwaitinfo(__const sigset_t * __restrict __set, siginfo_t * __restrict __info) __attribute__ ((__nonnull__(1)));
extern int sigtimedwait(__const sigset_t * __restrict __set, siginfo_t * __restrict __info, __const struct timespec *__restrict __timeout) __attribute__ ((__nonnull__(1)));
extern int sigqueue(__pid_t __pid, int __sig, __const union sigval __val) __attribute__ ((__nothrow__));
extern __const char *__const _sys_siglist[65];
extern __const char *__const sys_siglist[65];
struct sigvec {
    __sighandler_t sv_handler;
    int sv_mask;
    int sv_flags;
};
extern int sigvec(int __sig, __const struct sigvec *__vec, struct sigvec *__ovec) __attribute__ ((__nothrow__));
typedef __signed__ char __s8;
typedef unsigned char __u8;
typedef __signed__ short __s16;
typedef unsigned short __u16;
typedef __signed__ int __s32;
typedef unsigned int __u32;
__extension__ typedef __signed__ long long __s64;
__extension__ typedef unsigned long long __u64;
typedef unsigned short umode_t;
typedef struct {
    unsigned long fds_bits[(1024 / (8 * sizeof(unsigned long)))];
} __kernel_fd_set;
typedef void (*__kernel_sighandler_t) (int);
typedef int __kernel_key_t;
typedef int __kernel_mqd_t;
typedef unsigned long __kernel_ino_t;
typedef unsigned short __kernel_mode_t;
typedef unsigned short __kernel_nlink_t;
typedef long __kernel_off_t;
typedef int __kernel_pid_t;
typedef unsigned short __kernel_ipc_pid_t;
typedef unsigned short __kernel_uid_t;
typedef unsigned short __kernel_gid_t;
typedef unsigned int __kernel_size_t;
typedef int __kernel_ssize_t;
typedef int __kernel_ptrdiff_t;
typedef long __kernel_time_t;
typedef long __kernel_suseconds_t;
typedef long __kernel_clock_t;
typedef int __kernel_timer_t;
typedef int __kernel_clockid_t;
typedef int __kernel_daddr_t;
typedef char *__kernel_caddr_t;
typedef unsigned short __kernel_uid16_t;
typedef unsigned short __kernel_gid16_t;
typedef unsigned int __kernel_uid32_t;
typedef unsigned int __kernel_gid32_t;
typedef unsigned short __kernel_old_uid_t;
typedef unsigned short __kernel_old_gid_t;
typedef unsigned short __kernel_old_dev_t;
typedef long long __kernel_loff_t;
typedef struct {
    int val[2];
} __kernel_fsid_t;
typedef __u16 __le16;
typedef __u16 __be16;
typedef __u32 __le32;
typedef __u32 __be32;
typedef __u64 __le64;
typedef __u64 __be64;
typedef __u16 __sum16;
typedef __u32 __wsum;
struct _fpx_sw_bytes {
    __u32 magic1;
    __u32 extended_size;
    __u64 xstate_bv;
    __u32 xstate_size;
    __u32 padding[7];
};
struct _fpreg {
    unsigned short significand[4];
    unsigned short exponent;
};
struct _fpxreg {
    unsigned short significand[4];
    unsigned short exponent;
    unsigned short padding[3];
};
struct _xmmreg {
    unsigned long element[4];
};
struct _fpstate {
    unsigned long cw;
    unsigned long sw;
    unsigned long tag;
    unsigned long ipoff;
    unsigned long cssel;
    unsigned long dataoff;
    unsigned long datasel;
    struct _fpreg _st[8];
    unsigned short status;
    unsigned short magic;
    unsigned long _fxsr_env[6];
    unsigned long mxcsr;
    unsigned long reserved;
    struct _fpxreg _fxsr_st[8];
    struct _xmmreg _xmm[8];
    unsigned long padding1[44];
    union {
	unsigned long padding2[12];
	struct _fpx_sw_bytes sw_reserved;
    };
};
struct sigcontext {
    unsigned short gs, __gsh;
    unsigned short fs, __fsh;
    unsigned short es, __esh;
    unsigned short ds, __dsh;
    unsigned long edi;
    unsigned long esi;
    unsigned long ebp;
    unsigned long esp;
    unsigned long ebx;
    unsigned long edx;
    unsigned long ecx;
    unsigned long eax;
    unsigned long trapno;
    unsigned long err;
    unsigned long eip;
    unsigned short cs, __csh;
    unsigned long eflags;
    unsigned long esp_at_signal;
    unsigned short ss, __ssh;
    struct _fpstate *fpstate;
    unsigned long oldmask;
    unsigned long cr2;
};
struct _xsave_hdr {
    __u64 xstate_bv;
    __u64 reserved1[2];
    __u64 reserved2[5];
};
struct _ymmh_state {
    __u32 ymmh_space[64];
};
struct _xstate {
    struct _fpstate fpstate;
    struct _xsave_hdr xstate_hdr;
    struct _ymmh_state ymmh;
};
extern int sigreturn(struct sigcontext *__scp) __attribute__ ((__nothrow__));
extern int siginterrupt(int __sig, int __interrupt) __attribute__ ((__nothrow__));
struct sigstack {
    void *ss_sp;
    int ss_onstack;
};
enum { SS_ONSTACK = 1, SS_DISABLE };
typedef struct sigaltstack {
    void *ss_sp;
    int ss_flags;
    size_t ss_size;
} stack_t;
extern int sigstack(struct sigstack *__ss, struct sigstack *__oss) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int sigaltstack(__const struct sigaltstack *__restrict __ss, struct sigaltstack *__restrict __oss) __attribute__ ((__nothrow__));
extern int pthread_sigmask(int __how, __const __sigset_t * __restrict __newmask, __sigset_t * __restrict __oldmask) __attribute__ ((__nothrow__));
extern int pthread_kill(pthread_t __threadid, int __signo) __attribute__ ((__nothrow__));
extern int __libc_current_sigrtmin(void) __attribute__ ((__nothrow__));
extern int __libc_current_sigrtmax(void) __attribute__ ((__nothrow__));
typedef __useconds_t useconds_t;
typedef __intptr_t intptr_t;
typedef __socklen_t socklen_t;
extern int access(__const char *__name, int __type) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int faccessat(int __fd, __const char *__file, int __type, int __flag) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern __off_t lseek(int __fd, __off_t __offset, int __whence) __attribute__ ((__nothrow__));
extern int close(int __fd);
extern ssize_t read(int __fd, void *__buf, size_t __nbytes);
extern ssize_t write(int __fd, __const void *__buf, size_t __n);
extern int pipe(int __pipedes[2]) __attribute__ ((__nothrow__));
extern unsigned int alarm(unsigned int __seconds) __attribute__ ((__nothrow__));
extern unsigned int sleep(unsigned int __seconds);
extern __useconds_t ualarm(__useconds_t __value, __useconds_t __interval) __attribute__ ((__nothrow__));
extern int usleep(__useconds_t __useconds);
extern int pause(void);
extern int chown(__const char *__file, __uid_t __owner, __gid_t __group) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int fchown(int __fd, __uid_t __owner, __gid_t __group) __attribute__ ((__nothrow__));
extern int lchown(__const char *__file, __uid_t __owner, __gid_t __group) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int fchownat(int __fd, __const char *__file, __uid_t __owner, __gid_t __group, int __flag) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern int chdir(__const char *__path) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int fchdir(int __fd) __attribute__ ((__nothrow__));
extern char *getcwd(char *__buf, size_t __size) __attribute__ ((__nothrow__));
extern char *getwd(char *__buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1))) __attribute__ ((__deprecated__));
extern int dup(int __fd) __attribute__ ((__nothrow__));
extern int dup2(int __fd, int __fd2) __attribute__ ((__nothrow__));
extern char **__environ;
extern int execve(__const char *__path, char *__const __argv[], char *__const __envp[]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int fexecve(int __fd, char *__const __argv[], char *__const __envp[]) __attribute__ ((__nothrow__));
extern int execv(__const char *__path, char *__const __argv[]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int execle(__const char *__path, __const char *__arg, ...) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int execl(__const char *__path, __const char *__arg, ...) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int execvp(__const char *__file, char *__const __argv[]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int execlp(__const char *__file, __const char *__arg, ...) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int nice(int __inc) __attribute__ ((__nothrow__));
extern void _exit(int __status) __attribute__ ((__noreturn__));
enum { _PC_LINK_MAX, _PC_MAX_CANON, _PC_MAX_INPUT, _PC_NAME_MAX, _PC_PATH_MAX, _PC_PIPE_BUF, _PC_CHOWN_RESTRICTED, _PC_NO_TRUNC, _PC_VDISABLE, _PC_SYNC_IO, _PC_ASYNC_IO, _PC_PRIO_IO, _PC_SOCK_MAXBUF, _PC_FILESIZEBITS, _PC_REC_INCR_XFER_SIZE, _PC_REC_MAX_XFER_SIZE, _PC_REC_MIN_XFER_SIZE, _PC_REC_XFER_ALIGN, _PC_ALLOC_SIZE_MIN, _PC_SYMLINK_MAX, _PC_2_SYMLINKS };
enum { _SC_ARG_MAX, _SC_CHILD_MAX, _SC_CLK_TCK, _SC_NGROUPS_MAX, _SC_OPEN_MAX, _SC_STREAM_MAX, _SC_TZNAME_MAX, _SC_JOB_CONTROL, _SC_SAVED_IDS, _SC_REALTIME_SIGNALS, _SC_PRIORITY_SCHEDULING, _SC_TIMERS, _SC_ASYNCHRONOUS_IO, _SC_PRIORITIZED_IO, _SC_SYNCHRONIZED_IO, _SC_FSYNC, _SC_MAPPED_FILES, _SC_MEMLOCK, _SC_MEMLOCK_RANGE, _SC_MEMORY_PROTECTION, _SC_MESSAGE_PASSING, _SC_SEMAPHORES, _SC_SHARED_MEMORY_OBJECTS, _SC_AIO_LISTIO_MAX, _SC_AIO_MAX, _SC_AIO_PRIO_DELTA_MAX, _SC_DELAYTIMER_MAX, _SC_MQ_OPEN_MAX, _SC_MQ_PRIO_MAX, _SC_VERSION, _SC_PAGESIZE, _SC_RTSIG_MAX, _SC_SEM_NSEMS_MAX, _SC_SEM_VALUE_MAX, _SC_SIGQUEUE_MAX, _SC_TIMER_MAX, _SC_BC_BASE_MAX, _SC_BC_DIM_MAX, _SC_BC_SCALE_MAX, _SC_BC_STRING_MAX, _SC_COLL_WEIGHTS_MAX, _SC_EQUIV_CLASS_MAX, _SC_EXPR_NEST_MAX, _SC_LINE_MAX, _SC_RE_DUP_MAX, _SC_CHARCLASS_NAME_MAX, _SC_2_VERSION, _SC_2_C_BIND, _SC_2_C_DEV, _SC_2_FORT_DEV, _SC_2_FORT_RUN, _SC_2_SW_DEV, _SC_2_LOCALEDEF, _SC_PII, _SC_PII_XTI, _SC_PII_SOCKET, _SC_PII_INTERNET, _SC_PII_OSI, _SC_POLL, _SC_SELECT, _SC_UIO_MAXIOV, _SC_IOV_MAX = _SC_UIO_MAXIOV, _SC_PII_INTERNET_STREAM, _SC_PII_INTERNET_DGRAM, _SC_PII_OSI_COTS, _SC_PII_OSI_CLTS, _SC_PII_OSI_M, _SC_T_IOV_MAX, _SC_THREADS, _SC_THREAD_SAFE_FUNCTIONS, _SC_GETGR_R_SIZE_MAX, _SC_GETPW_R_SIZE_MAX, _SC_LOGIN_NAME_MAX, _SC_TTY_NAME_MAX, _SC_THREAD_DESTRUCTOR_ITERATIONS, _SC_THREAD_KEYS_MAX, _SC_THREAD_STACK_MIN, _SC_THREAD_THREADS_MAX, _SC_THREAD_ATTR_STACKADDR, _SC_THREAD_ATTR_STACKSIZE, _SC_THREAD_PRIORITY_SCHEDULING, _SC_THREAD_PRIO_INHERIT, _SC_THREAD_PRIO_PROTECT, _SC_THREAD_PROCESS_SHARED, _SC_NPROCESSORS_CONF, _SC_NPROCESSORS_ONLN, _SC_PHYS_PAGES, _SC_AVPHYS_PAGES, _SC_ATEXIT_MAX, _SC_PASS_MAX, _SC_XOPEN_VERSION, _SC_XOPEN_XCU_VERSION, _SC_XOPEN_UNIX, _SC_XOPEN_CRYPT, _SC_XOPEN_ENH_I18N, _SC_XOPEN_SHM, _SC_2_CHAR_TERM, _SC_2_C_VERSION, _SC_2_UPE, _SC_XOPEN_XPG2, _SC_XOPEN_XPG3, _SC_XOPEN_XPG4, _SC_CHAR_BIT, _SC_CHAR_MAX, _SC_CHAR_MIN, _SC_INT_MAX, _SC_INT_MIN, _SC_LONG_BIT, _SC_WORD_BIT, _SC_MB_LEN_MAX, _SC_NZERO, _SC_SSIZE_MAX, _SC_SCHAR_MAX, _SC_SCHAR_MIN, _SC_SHRT_MAX, _SC_SHRT_MIN, _SC_UCHAR_MAX, _SC_UINT_MAX, _SC_ULONG_MAX, _SC_USHRT_MAX, _SC_NL_ARGMAX, _SC_NL_LANGMAX, _SC_NL_MSGMAX, _SC_NL_NMAX, _SC_NL_SETMAX, _SC_NL_TEXTMAX, _SC_XBS5_ILP32_OFF32, _SC_XBS5_ILP32_OFFBIG, _SC_XBS5_LP64_OFF64, _SC_XBS5_LPBIG_OFFBIG, _SC_XOPEN_LEGACY, _SC_XOPEN_REALTIME, _SC_XOPEN_REALTIME_THREADS, _SC_ADVISORY_INFO, _SC_BARRIERS, _SC_BASE, _SC_C_LANG_SUPPORT, _SC_C_LANG_SUPPORT_R, _SC_CLOCK_SELECTION, _SC_CPUTIME, _SC_THREAD_CPUTIME, _SC_DEVICE_IO, _SC_DEVICE_SPECIFIC, _SC_DEVICE_SPECIFIC_R, _SC_FD_MGMT, _SC_FIFO, _SC_PIPE, _SC_FILE_ATTRIBUTES, _SC_FILE_LOCKING, _SC_FILE_SYSTEM, _SC_MONOTONIC_CLOCK, _SC_MULTI_PROCESS, _SC_SINGLE_PROCESS, _SC_NETWORKING, _SC_READER_WRITER_LOCKS, _SC_SPIN_LOCKS, _SC_REGEXP, _SC_REGEX_VERSION, _SC_SHELL, _SC_SIGNALS, _SC_SPAWN, _SC_SPORADIC_SERVER, _SC_THREAD_SPORADIC_SERVER, _SC_SYSTEM_DATABASE, _SC_SYSTEM_DATABASE_R, _SC_TIMEOUTS, _SC_TYPED_MEMORY_OBJECTS, _SC_USER_GROUPS, _SC_USER_GROUPS_R, _SC_2_PBS, _SC_2_PBS_ACCOUNTING, _SC_2_PBS_LOCATE, _SC_2_PBS_MESSAGE, _SC_2_PBS_TRACK, _SC_SYMLOOP_MAX, _SC_STREAMS, _SC_2_PBS_CHECKPOINT, _SC_V6_ILP32_OFF32, _SC_V6_ILP32_OFFBIG, _SC_V6_LP64_OFF64, _SC_V6_LPBIG_OFFBIG, _SC_HOST_NAME_MAX, _SC_TRACE, _SC_TRACE_EVENT_FILTER, _SC_TRACE_INHERIT, _SC_TRACE_LOG, _SC_LEVEL1_ICACHE_SIZE, _SC_LEVEL1_ICACHE_ASSOC, _SC_LEVEL1_ICACHE_LINESIZE, _SC_LEVEL1_DCACHE_SIZE, _SC_LEVEL1_DCACHE_ASSOC, _SC_LEVEL1_DCACHE_LINESIZE, _SC_LEVEL2_CACHE_SIZE, _SC_LEVEL2_CACHE_ASSOC, _SC_LEVEL2_CACHE_LINESIZE, _SC_LEVEL3_CACHE_SIZE, _SC_LEVEL3_CACHE_ASSOC, _SC_LEVEL3_CACHE_LINESIZE, _SC_LEVEL4_CACHE_SIZE, _SC_LEVEL4_CACHE_ASSOC, _SC_LEVEL4_CACHE_LINESIZE, _SC_IPV6 = _SC_LEVEL1_ICACHE_SIZE + 50, _SC_RAW_SOCKETS, _SC_V7_ILP32_OFF32, _SC_V7_ILP32_OFFBIG, _SC_V7_LP64_OFF64, _SC_V7_LPBIG_OFFBIG, _SC_SS_REPL_MAX, _SC_TRACE_EVENT_NAME_MAX, _SC_TRACE_NAME_MAX, _SC_TRACE_SYS_MAX, _SC_TRACE_USER_EVENT_MAX, _SC_XOPEN_STREAMS, _SC_THREAD_ROBUST_PRIO_INHERIT, _SC_THREAD_ROBUST_PRIO_PROTECT };
enum { _CS_PATH, _CS_V6_WIDTH_RESTRICTED_ENVS, _CS_GNU_LIBC_VERSION, _CS_GNU_LIBPTHREAD_VERSION, _CS_V5_WIDTH_RESTRICTED_ENVS, _CS_V7_WIDTH_RESTRICTED_ENVS, _CS_LFS_CFLAGS = 1000, _CS_LFS_LDFLAGS, _CS_LFS_LIBS, _CS_LFS_LINTFLAGS, _CS_LFS64_CFLAGS, _CS_LFS64_LDFLAGS, _CS_LFS64_LIBS, _CS_LFS64_LINTFLAGS, _CS_XBS5_ILP32_OFF32_CFLAGS = 1100, _CS_XBS5_ILP32_OFF32_LDFLAGS, _CS_XBS5_ILP32_OFF32_LIBS, _CS_XBS5_ILP32_OFF32_LINTFLAGS, _CS_XBS5_ILP32_OFFBIG_CFLAGS, _CS_XBS5_ILP32_OFFBIG_LDFLAGS, _CS_XBS5_ILP32_OFFBIG_LIBS, _CS_XBS5_ILP32_OFFBIG_LINTFLAGS, _CS_XBS5_LP64_OFF64_CFLAGS, _CS_XBS5_LP64_OFF64_LDFLAGS, _CS_XBS5_LP64_OFF64_LIBS, _CS_XBS5_LP64_OFF64_LINTFLAGS, _CS_XBS5_LPBIG_OFFBIG_CFLAGS, _CS_XBS5_LPBIG_OFFBIG_LDFLAGS, _CS_XBS5_LPBIG_OFFBIG_LIBS, _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS, _CS_POSIX_V6_ILP32_OFF32_CFLAGS, _CS_POSIX_V6_ILP32_OFF32_LDFLAGS, _CS_POSIX_V6_ILP32_OFF32_LIBS, _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS, _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS, _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS, _CS_POSIX_V6_ILP32_OFFBIG_LIBS, _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS, _CS_POSIX_V6_LP64_OFF64_CFLAGS, _CS_POSIX_V6_LP64_OFF64_LDFLAGS, _CS_POSIX_V6_LP64_OFF64_LIBS, _CS_POSIX_V6_LP64_OFF64_LINTFLAGS, _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS, _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS, _CS_POSIX_V6_LPBIG_OFFBIG_LIBS, _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS, _CS_POSIX_V7_ILP32_OFF32_CFLAGS, _CS_POSIX_V7_ILP32_OFF32_LDFLAGS, _CS_POSIX_V7_ILP32_OFF32_LIBS, _CS_POSIX_V7_ILP32_OFF32_LINTFLAGS, _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS, _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS, _CS_POSIX_V7_ILP32_OFFBIG_LIBS, _CS_POSIX_V7_ILP32_OFFBIG_LINTFLAGS, _CS_POSIX_V7_LP64_OFF64_CFLAGS, _CS_POSIX_V7_LP64_OFF64_LDFLAGS, _CS_POSIX_V7_LP64_OFF64_LIBS, _CS_POSIX_V7_LP64_OFF64_LINTFLAGS, _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS, _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS, _CS_POSIX_V7_LPBIG_OFFBIG_LIBS, _CS_POSIX_V7_LPBIG_OFFBIG_LINTFLAGS };
extern long int pathconf(__const char *__path, int __name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern long int fpathconf(int __fd, int __name) __attribute__ ((__nothrow__));
extern long int sysconf(int __name) __attribute__ ((__nothrow__));
extern size_t confstr(int __name, char *__buf, size_t __len) __attribute__ ((__nothrow__));
extern __pid_t getpid(void) __attribute__ ((__nothrow__));
extern __pid_t getppid(void) __attribute__ ((__nothrow__));
extern __pid_t getpgrp(void) __attribute__ ((__nothrow__));
extern __pid_t __getpgid(__pid_t __pid) __attribute__ ((__nothrow__));
extern int setpgid(__pid_t __pid, __pid_t __pgid) __attribute__ ((__nothrow__));
extern int setpgrp(void) __attribute__ ((__nothrow__));
extern __pid_t setsid(void) __attribute__ ((__nothrow__));
extern __uid_t getuid(void) __attribute__ ((__nothrow__));
extern __uid_t geteuid(void) __attribute__ ((__nothrow__));
extern __gid_t getgid(void) __attribute__ ((__nothrow__));
extern __gid_t getegid(void) __attribute__ ((__nothrow__));
extern int getgroups(int __size, __gid_t __list[]) __attribute__ ((__nothrow__));
extern int setuid(__uid_t __uid) __attribute__ ((__nothrow__));
extern int setreuid(__uid_t __ruid, __uid_t __euid) __attribute__ ((__nothrow__));
extern int seteuid(__uid_t __uid) __attribute__ ((__nothrow__));
extern int setgid(__gid_t __gid) __attribute__ ((__nothrow__));
extern int setregid(__gid_t __rgid, __gid_t __egid) __attribute__ ((__nothrow__));
extern int setegid(__gid_t __gid) __attribute__ ((__nothrow__));
extern __pid_t fork(void) __attribute__ ((__nothrow__));
extern __pid_t vfork(void) __attribute__ ((__nothrow__));
extern char *ttyname(int __fd) __attribute__ ((__nothrow__));
extern int ttyname_r(int __fd, char *__buf, size_t __buflen) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern int isatty(int __fd) __attribute__ ((__nothrow__));
extern int ttyslot(void) __attribute__ ((__nothrow__));
extern int link(__const char *__from, __const char *__to) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int linkat(int __fromfd, __const char *__from, int __tofd, __const char *__to, int __flags) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2, 4)));
extern int symlink(__const char *__from, __const char *__to) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern ssize_t readlink(__const char *__restrict __path, char *__restrict __buf, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));
extern int symlinkat(__const char *__from, int __tofd, __const char *__to) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 3)));
extern ssize_t readlinkat(int __fd, __const char *__restrict __path, char *__restrict __buf, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2, 3)));
extern int unlink(__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int unlinkat(int __fd, __const char *__name, int __flag) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(2)));
extern int rmdir(__const char *__path) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern __pid_t tcgetpgrp(int __fd) __attribute__ ((__nothrow__));
extern int tcsetpgrp(int __fd, __pid_t __pgrp_id) __attribute__ ((__nothrow__));
extern char *getlogin(void);
extern int getlogin_r(char *__name, size_t __name_len) __attribute__ ((__nonnull__(1)));
extern int setlogin(__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern char *optarg;
extern int optind;
extern int opterr;
extern int optopt;
extern int getopt(int ___argc, char *const *___argv, const char *__shortopts) __attribute__ ((__nothrow__));
extern int gethostname(char *__name, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int sethostname(__const char *__name, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int sethostid(long int __id) __attribute__ ((__nothrow__));
extern int getdomainname(char *__name, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int setdomainname(__const char *__name, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int vhangup(void) __attribute__ ((__nothrow__));
extern int revoke(__const char *__file) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int profil(unsigned short int *__sample_buffer, size_t __size, size_t __offset, unsigned int __scale) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int acct(__const char *__name) __attribute__ ((__nothrow__));
extern char *getusershell(void) __attribute__ ((__nothrow__));
extern void endusershell(void) __attribute__ ((__nothrow__));
extern void setusershell(void) __attribute__ ((__nothrow__));
extern int daemon(int __nochdir, int __noclose) __attribute__ ((__nothrow__));
extern int chroot(__const char *__path) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern char *getpass(__const char *__prompt) __attribute__ ((__nonnull__(1)));
extern int fsync(int __fd);
extern long int gethostid(void);
extern void sync(void) __attribute__ ((__nothrow__));
extern int getpagesize(void) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int getdtablesize(void) __attribute__ ((__nothrow__));
extern int truncate(__const char *__file, __off_t __length) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));
extern int ftruncate(int __fd, __off_t __length) __attribute__ ((__nothrow__));
extern int brk(void *__addr) __attribute__ ((__nothrow__));
extern void *sbrk(intptr_t __delta) __attribute__ ((__nothrow__));
extern long int syscall(long int __sysno, ...) __attribute__ ((__nothrow__));
extern int lockf(int __fd, int __cmd, __off_t __len);
extern int fdatasync(int __fildes);
extern int cbench_print;
typedef unsigned char cc_t;
typedef unsigned int speed_t;
typedef unsigned int tcflag_t;
struct termios {
    tcflag_t c_iflag;
    tcflag_t c_oflag;
    tcflag_t c_cflag;
    tcflag_t c_lflag;
    cc_t c_line;
    cc_t c_cc[32];
    speed_t c_ispeed;
    speed_t c_ospeed;
};
extern speed_t cfgetospeed(__const struct termios *__termios_p) __attribute__ ((__nothrow__));
extern speed_t cfgetispeed(__const struct termios *__termios_p) __attribute__ ((__nothrow__));
extern int cfsetospeed(struct termios *__termios_p, speed_t __speed) __attribute__ ((__nothrow__));
extern int cfsetispeed(struct termios *__termios_p, speed_t __speed) __attribute__ ((__nothrow__));
extern int cfsetspeed(struct termios *__termios_p, speed_t __speed) __attribute__ ((__nothrow__));
extern int tcgetattr(int __fd, struct termios *__termios_p) __attribute__ ((__nothrow__));
extern int tcsetattr(int __fd, int __optional_actions, __const struct termios *__termios_p) __attribute__ ((__nothrow__));
extern void cfmakeraw(struct termios *__termios_p) __attribute__ ((__nothrow__));
extern int tcsendbreak(int __fd, int __duration) __attribute__ ((__nothrow__));
extern int tcdrain(int __fd);
extern int tcflush(int __fd, int __queue_selector) __attribute__ ((__nothrow__));
extern int tcflow(int __fd, int __action) __attribute__ ((__nothrow__));
void erase(void);
void move(int row, int col);
void inverse(void);
void normal(void);
void backup(void);
static int putch(int c);
void terminit(void);
void done(int signo);
static void onstop(int signo);
void stop(void);
int shellescape(char *buf);
void erase()
{
} void move(row, col)

int row;
int col;
{
} void inverse()
{
} void normal()
{
} void backup()
{
} static int putch(c)

int c;
{
    return putchar(c);
}

static struct termios sbuf;
static struct termios osbuf;
static int termchanged = 0;
static void (*oldint) ();
static void (*oldterm) ();
static void (*oldttin) ();
static void (*oldttou) ();
static void (*oldtstp) ();
void terminit()
{
    if (getenv("COLUMNS") != ((void *) 0))
	co = atoi(getenv("COLUMNS"));
    if (getenv("LINES") != ((void *) 0))
	li = atoi(getenv("LINES"));
    if (contextsize == 0)
	contextsize = (li * 10) / 100;
    if (contextsize > 10)
	contextsize = 10;
    else if (contextsize < 2)
	contextsize = 2;
    if (li < contextsize + 8 && contextsize > 2) {
	contextsize = li - 8;
	if (contextsize < 2)
	    contextsize = 2;
    }
    if (li < 2 + 8)
	if (cbench_print)
	    fprintf(stderr, "Screen too small:  need at least %d lines\n", 2 + 8);
    if (!isatty(0)) {
	if (cbench_print)
	    fprintf(stderr, "Can't deal with non-interactive use yet.\n");
	exit(1);
    }
    (void) tcgetattr(0, &osbuf);
    termchanged = 1;
    sbuf = osbuf;
    sbuf.c_lflag &= ~(0000010 | 0000040 | 0000100 | 0000002);
    sbuf.c_oflag &= ~(0000001);
    sbuf.c_iflag &= ~(0000100 | 0000200 | 0000400);
    sbuf.c_cc[6] = 1;
    sbuf.c_cc[5] = 1;
    (void) tcsetattr(0, 0, &sbuf);
    uerasechar = osbuf.c_cc[2];
    ukillchar = osbuf.c_cc[3];
    if ((oldint = signal(2, ((__sighandler_t) 1))) != ((__sighandler_t) 1))
	(void) signal(2, done);
    if ((oldterm = signal(15, ((__sighandler_t) 1))) != ((__sighandler_t) 1))
	(void) signal(15, done);
    if ((oldttin = signal(21, ((__sighandler_t) 1))) != ((__sighandler_t) 1))
	(void) signal(21, onstop);
    if ((oldttou = signal(22, ((__sighandler_t) 1))) != ((__sighandler_t) 1))
	(void) signal(22, onstop);
    if ((oldtstp = signal(20, ((__sighandler_t) 1))) != ((__sighandler_t) 1))
	(void) signal(20, onstop);
} void done(signo)

int signo;
{
    if (tempfile[0] != '\0')
	(void) unlink(tempfile);
    if (termchanged) {
    }
    exit(0);
}

static void onstop(signo)
int signo;
{
    (void) tcsetattr(0, 0, &osbuf);
    (void) signal(signo, ((__sighandler_t) 0));
    (void) kill(0, signo);
    (void) signal(signo, onstop);
    (void) tcsetattr(0, 0, &sbuf);
} void stop()
{
    onstop(20);
} int shellescape(buf)

char *buf;
{
    char *argv[100];
    char *cp = buf;
    int i = 0;
    int termstat;
    while (*cp != '\0') {
	while (*cp == ' ' || *cp == '\t')
	    ++cp;
	if (*cp == '\0')
	    break;
	argv[i++] = cp;
	while (*cp != ' ' && *cp != '\t' && *cp != '\0')
	    ++cp;
	if (*cp != '\0')
	    *cp++ = '\0';
    }
    argv[i] = ((void *) 0);
    (void) tcsetattr(0, 0, &osbuf);
    (void) signal(2, oldint);
    (void) signal(15, oldterm);
    (void) signal(21, oldttin);
    (void) signal(22, oldttou);
    (void) signal(20, oldtstp);
    if ((i = fork()) == 0) {
	(void) execvp(argv[0], (char **) argv);
	_exit(123);
    } else if (i > 0) {
	while (wait(&termstat) != i);
	termstat = (termstat == (123 << 8)) ? 0 : -1;
    } else {
	(void) printf("Couldn't fork, try later.\r\n");
	termstat = -1;
    } if (oldint != ((__sighandler_t) 1))
	(void) signal(2, done);
    if (oldterm != ((__sighandler_t) 1))
	(void) signal(15, done);
    if (oldttin != ((__sighandler_t) 1))
	(void) signal(21, onstop);
    if (oldttou != ((__sighandler_t) 1))
	(void) signal(22, onstop);
    if (oldtstp != ((__sighandler_t) 1))
	(void) signal(20, onstop);
    (void) tcsetattr(0, 0, &sbuf);
    if (termstat) {
	(void) printf("\n-- Type space to continue --");
	(void) fflush(outfile1);
	while (getchar() != ' ');
    }
    return (termstat);
}
