typedef __typeof__(((int *) 0) - ((int *) 0)) ptrdiff_t;
typedef __typeof__(sizeof(int)) size_t;
typedef int wchar_t;
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
extern int dprintf(int __fd, __const char *__restrict __fmt, ...) __attribute__ ((__format__(__printf__, 2, 3)));
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
extern __ssize_t getline(char **__restrict __lineptr, size_t * __restrict __n, FILE * __restrict __stream);
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
extern void __assert_fail(__const char *__assertion, __const char *__file, unsigned int __line, __const char *__function) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern void __assert_perror_fail(int __errnum, __const char *__file, unsigned int __line, __const char *__function) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern void __assert(const char *__assertion, const char *__file, int __line) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
typedef short word;
typedef long longword;
typedef unsigned short uword;
typedef unsigned long ulongword;
struct gsm_state {
    word dp0[280];
    word z1;
    longword L_z2;
    int mp;
    word u[8];
    word LARpp[2][8];
    word j;
    word nrp;
    word v[9];
    word msr;
    char verbose;
    char fast;
};
extern word gsm_mult(word a, word b);
extern longword gsm_L_mult(word a, word b);
extern word gsm_mult_r(word a, word b);
extern word gsm_div(word num, word denum);
extern word gsm_add(word a, word b);
extern longword gsm_L_add(longword a, longword b);
extern word gsm_sub(word a, word b);
extern longword gsm_L_sub(longword a, longword b);
extern word gsm_abs(word a);
extern word gsm_norm(longword a);
extern longword gsm_L_asl(longword a, int n);
extern word gsm_asl(word a, int n);
extern longword gsm_L_asr(longword a, int n);
extern word gsm_asr(word a, int n);
extern void Gsm_Coder(struct gsm_state *S, word * s, word * LARc, word * Nc, word * bc, word * Mc, word * xmaxc, word * xMc);
extern void Gsm_Long_Term_Predictor(struct gsm_state *S, word * d, word * dp, word * e, word * dpp, word * Nc, word * bc);
extern void Gsm_LPC_Analysis(struct gsm_state *S, word * s, word * LARc);
extern void Gsm_Preprocess(struct gsm_state *S, word * s, word * so);
extern void Gsm_Encoding(struct gsm_state *S, word * e, word * ep, word * xmaxc, word * Mc, word * xMc);
extern void Gsm_Short_Term_Analysis_Filter(struct gsm_state *S, word * LARc, word * d);
extern void Gsm_Decoder(struct gsm_state *S, word * LARcr, word * Ncr, word * bcr, word * Mcr, word * xmaxcr, word * xMcr, word * s);
extern void Gsm_Decoding(struct gsm_state *S, word xmaxcr, word Mcr, word * xMcr, word * erp);
extern void Gsm_Long_Term_Synthesis_Filtering(struct gsm_state *S, word Ncr, word bcr, word * erp, word * drp);
void Gsm_RPE_Decoding(struct gsm_state *S, word xmaxcr, word Mcr, word * xMcr, word * erp);
void Gsm_RPE_Encoding(struct gsm_state *S, word * e, word * xmaxc, word * Mc, word * xMc);
extern void Gsm_Short_Term_Synthesis_Filter(struct gsm_state *S, word * LARcr, word * drp, word * s);
extern void Gsm_Update_of_reconstructed_short_time_residual_signal(word * dpp, word * ep, word * dp);
extern word gsm_A[8], gsm_B[8], gsm_MIC[8], gsm_MAC[8];
extern word gsm_INVA[8];
extern word gsm_DLB[4], gsm_QLB[4];
extern word gsm_H[11];
extern word gsm_NRFAC[8];
extern word gsm_FAC[8];
extern void gsm_debug_words(char *name, int, int, word *);
extern void gsm_debug_longwords(char *name, int, int, longword *);
extern void gsm_debug_longword(char *name, longword);
extern void gsm_debug_word(char *name, word);
typedef struct gsm_state *gsm;
typedef short gsm_signal;
typedef unsigned char gsm_byte;
typedef gsm_byte gsm_frame[33];
extern gsm gsm_create(void);
extern void gsm_destroy(gsm);
extern int gsm_print(FILE *, gsm, gsm_byte *);
extern int gsm_option(gsm, int, int *);
extern void gsm_encode(gsm, gsm_signal *, gsm_byte *);
extern int gsm_decode(gsm, gsm_byte *, gsm_signal *);
extern int gsm_explode(gsm, gsm_byte *, gsm_signal *);
extern void gsm_implode(gsm, gsm_signal *, gsm_byte *);
static void Weighting_filter(register word * e, word * x)
{
    register longword L_result;
    register int k;
    e -= 5;
    for (k = 0; k <= 39; k++) {
	L_result = 8192 >> 1;
	L_result += (e[k + 0] * (longword) - 134);
	L_result += (e[k + 1] * (longword) - 374);
	L_result += (e[k + 3] * (longword) 2054);
	L_result += (e[k + 4] * (longword) 5741);
	L_result += (e[k + 5] * (longword) 8192);
	L_result += (e[k + 6] * (longword) 5741);
	L_result += (e[k + 7] * (longword) 2054);
	L_result += (e[k + 9] * (longword) - 374);
	L_result += (e[k + 10] * (longword) - 134);
	L_result = ((L_result) >> (13));
	x[k] = (L_result < ((-32767) - 1) ? ((-32767) - 1) : (L_result > (32767) ? (32767) : L_result));
    }
}

static void RPE_grid_selection(word * x, word * xM, word * Mc_out)
{
    register int i;
    register longword L_result, L_temp;
    longword EM;
    word Mc;
    longword L_common_0_3;
    EM = 0;
    Mc = 0;
    L_result = 0;
    L_temp = ((x[0 + 3 * 1]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 2]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 3]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 4]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 5]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 6]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 7]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 8]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 9]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 10]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 11]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[0 + 3 * 12]) >> (2));
    L_result += L_temp * L_temp;;
    L_common_0_3 = L_result;
    L_temp = ((x[0 + 3 * 0]) >> (2));
    L_result += L_temp * L_temp;;
    L_result <<= 1;
    EM = L_result;
    L_result = 0;
    L_temp = ((x[1 + 3 * 0]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 1]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 2]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 3]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 4]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 5]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 6]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 7]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 8]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 9]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 10]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 11]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[1 + 3 * 12]) >> (2));
    L_result += L_temp * L_temp;;
    L_result <<= 1;
    if (L_result > EM) {
	Mc = 1;
	EM = L_result;
    }
    L_result = 0;
    L_temp = ((x[2 + 3 * 0]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 1]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 2]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 3]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 4]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 5]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 6]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 7]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 8]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 9]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 10]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 11]) >> (2));
    L_result += L_temp * L_temp;;
    L_temp = ((x[2 + 3 * 12]) >> (2));
    L_result += L_temp * L_temp;;
    L_result <<= 1;
    if (L_result > EM) {
	Mc = 2;
	EM = L_result;
    }
    L_result = L_common_0_3;
    L_temp = ((x[3 + 3 * 12]) >> (2));
    L_result += L_temp * L_temp;;
    L_result <<= 1;
    if (L_result > EM) {
	Mc = 3;
	EM = L_result;
    }
    for (i = 0; i <= 12; i++)
	xM[i] = x[Mc + 3 * i];
    *Mc_out = Mc;
}

static void APCM_quantization_xmaxc_to_exp_mant(word xmaxc, word * exp_out, word * mant_out)
{
    word exp, mant;
    exp = 0;
    if (xmaxc > 15)
	exp = ((xmaxc) >> (3)) - 1;
    mant = xmaxc - (exp << 3);
    if (mant == 0) {
	exp = -4;
	mant = 7;
    } else {
	while (mant <= 7) {
	    mant = mant << 1 | 1;
	    exp--;
	}
	mant -= 8;
    }
    ((exp >= -4 && exp <= 6) ? (void) (0) : __assert_fail("exp >= -4 && exp <= 6", "rpe.c", 249, __PRETTY_FUNCTION__));
    ((mant >= 0 && mant <= 7) ? (void) (0) : __assert_fail("mant >= 0 && mant <= 7", "rpe.c", 250, __PRETTY_FUNCTION__));
    *exp_out = exp;
    *mant_out = mant;
} static void APCM_quantization(word * xM, word * xMc, word * mant_out, word * exp_out, word * xmaxc_out)
{
    int i, itest;
    word xmax, xmaxc, temp, temp1, temp2;
    word exp, mant;
    xmax = 0;
    for (i = 0; i <= 12; i++) {
	temp = xM[i];
	temp = ((temp) < 0 ? ((temp) == ((-32767) - 1) ? (32767) : -(temp)) : (temp));
	if (temp > xmax)
	    xmax = temp;
    }
    exp = 0;
    temp = ((xmax) >> (9));
    itest = 0;
    for (i = 0; i <= 5; i++) {
	itest |= (temp <= 0);
	temp = ((temp) >> (1));
	((exp <= 5) ? (void) (0) : __assert_fail("exp <= 5", "rpe.c", 293, __PRETTY_FUNCTION__));
	if (itest == 0)
	    exp++;
    }
    ((exp <= 6 && exp >= 0) ? (void) (0) : __assert_fail("exp <= 6 && exp >= 0", "rpe.c", 297, __PRETTY_FUNCTION__));
    temp = exp + 5;
    ((temp <= 11 && temp >= 0) ? (void) (0) : __assert_fail("temp <= 11 && temp >= 0", "rpe.c", 300, __PRETTY_FUNCTION__));
    xmaxc = gsm_add(((xmax) >> (temp)), exp << 3);
    APCM_quantization_xmaxc_to_exp_mant(xmaxc, &exp, &mant);
    ((exp <= 4096 && exp >= -4096) ? (void) (0) : __assert_fail("exp <= 4096 && exp >= -4096", "rpe.c", 323, __PRETTY_FUNCTION__));
    ((mant >= 0 && mant <= 7) ? (void) (0) : __assert_fail("mant >= 0 && mant <= 7", "rpe.c", 324, __PRETTY_FUNCTION__));
    temp1 = 6 - exp;
    temp2 = gsm_NRFAC[mant];
    for (i = 0; i <= 12; i++) {
	((temp1 >= 0 && temp1 < 16) ? (void) (0) : __assert_fail("temp1 >= 0 && temp1 < 16", "rpe.c", 331, __PRETTY_FUNCTION__));
	temp = xM[i] << temp1;
	temp = (((((longword) (temp) * (longword) (temp2))) >> (15)));
	temp = ((temp) >> (12));
	xMc[i] = temp + 4;
    } *mant_out = mant;
    *exp_out = exp;
    *xmaxc_out = xmaxc;
} static void APCM_inverse_quantization(register word * xMc, word mant, word exp, register word * xMp)
{
    int i;
    word temp, temp1, temp2, temp3;
    longword ltmp;
    ((mant >= 0 && mant <= 7) ? (void) (0) : __assert_fail("mant >= 0 && mant <= 7", "rpe.c", 364, __PRETTY_FUNCTION__));
    temp1 = gsm_FAC[mant];
    temp2 = gsm_sub(6, exp);
    temp3 = gsm_asl(1, gsm_sub(temp2, 1));
    for (i = 13; i--;) {
	((*xMc <= 7 && *xMc >= 0) ? (void) (0) : __assert_fail("*xMc <= 7 && *xMc >= 0", "rpe.c", 372, __PRETTY_FUNCTION__));
	temp = (*xMc++ << 1) - 7;
	((temp <= 7 && temp >= -7) ? (void) (0) : __assert_fail("temp <= 7 && temp >= -7", "rpe.c", 376, __PRETTY_FUNCTION__));
	temp <<= 12;
	temp = (((((longword) (temp1) * (longword) (temp) + 16384)) >> (15)));
	temp = ((ulongword) ((ltmp = (longword) (temp) + (longword) (temp3)) - ((-32767) - 1)) > (32767) - ((-32767) - 1) ? (ltmp > 0 ? (32767) : ((-32767) - 1)) : ltmp);
	*xMp++ = gsm_asr(temp, temp2);
}} static void RPE_grid_positioning(word Mc, register word * xMp, register word * ep)
{
    int i = 13;
    ((0 <= Mc && Mc <= 3) ? (void) (0) : __assert_fail("0 <= Mc && Mc <= 3", "rpe.c", 402, __PRETTY_FUNCTION__));
    switch (Mc) {
    case 3:
	*ep++ = 0;
    case 2:
	do {
	    *ep++ = 0;
    case 1:
	    *ep++ = 0;
    case 0:
	    *ep++ = *xMp++;
	} while (--i);
    }
    while (++Mc < 4)
	*ep++ = 0;
}

void Gsm_RPE_Encoding(struct gsm_state *S, word * e, word * xmaxc, word * Mc, word * xMc)
{
    word x[40];
    word xM[13], xMp[13];
    word mant, exp;
    Weighting_filter(e, x);
    RPE_grid_selection(x, xM, Mc);
    APCM_quantization(xM, xMc, &mant, &exp, xmaxc);
    APCM_inverse_quantization(xMc, mant, exp, xMp);
    RPE_grid_positioning(*Mc, xMp, e);
} void Gsm_RPE_Decoding(struct gsm_state *S, word xmaxcr, word Mcr, word * xMcr, word * erp)
{
    word exp, mant;
    word xMp[13];
    APCM_quantization_xmaxc_to_exp_mant(xmaxcr, &exp, &mant);
    APCM_inverse_quantization(xMcr, mant, exp, xMp);
    RPE_grid_positioning(Mcr, xMp, erp);
}
