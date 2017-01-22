typedef unsigned char byte;
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
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
typedef unsigned long int ulong_;
typedef unsigned short int ushort_;
typedef unsigned int uint_;
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
typedef int bool;
typedef const char *ptr_ord_t;
typedef double floatp;
typedef const char *client_name_t;
typedef ushort bits16;
typedef uint bits32;
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
int unlink(const char *);
typedef struct gs_memory_struct_type_s gs_memory_struct_type_t;
typedef const gs_memory_struct_type_t *gs_memory_type_ptr_t;
typedef client_name_t struct_name_t;
uint gs_struct_type_size(gs_memory_type_ptr_t);
struct_name_t gs_struct_type_name(gs_memory_type_ptr_t);
typedef struct gc_state_s gc_state_t;
typedef struct gs_ptr_procs_s {
    void (*unmark) (void *, gc_state_t *);
    bool(*mark) (void *, gc_state_t *);
    void *(*reloc) (const void *, gc_state_t *);
} gs_ptr_procs_t;
typedef const gs_ptr_procs_t *gs_ptr_type_t;
extern const gs_ptr_procs_t ptr_struct_procs;
extern const gs_ptr_procs_t ptr_string_procs;
extern const gs_ptr_procs_t ptr_const_string_procs;
typedef struct gs_gc_root_s gs_gc_root_t;
struct gs_gc_root_s {
    gs_gc_root_t *next;
    gs_ptr_type_t ptype;
    void **p;
};
typedef struct gs_memory_status_s {
    ulong allocated;
    ulong used;
} gs_memory_status_t;
struct gs_memory_s;
typedef struct gs_memory_s gs_memory_t;
typedef struct gs_memory_procs_s {
    byte *(*alloc_bytes) (gs_memory_t * mem, uint nbytes, client_name_t cname);
    byte *(*alloc_bytes_immovable) (gs_memory_t * mem, uint nbytes, client_name_t cname);
    void *(*alloc_struct) (gs_memory_t * mem, gs_memory_type_ptr_t pstype, client_name_t cname);
    void *(*alloc_struct_immovable) (gs_memory_t * mem, gs_memory_type_ptr_t pstype, client_name_t cname);
    byte *(*alloc_byte_array) (gs_memory_t * mem, uint num_elements, uint elt_size, client_name_t cname);
    byte *(*alloc_byte_array_immovable) (gs_memory_t * mem, uint num_elements, uint elt_size, client_name_t cname);
    void *(*alloc_struct_array) (gs_memory_t * mem, uint num_elements, gs_memory_type_ptr_t pstype, client_name_t cname);
    void *(*alloc_struct_array_immovable) (gs_memory_t * mem, uint num_elements, gs_memory_type_ptr_t pstype, client_name_t cname);
    void *(*resize_object) (gs_memory_t * mem, void *obj, uint new_num_elements, client_name_t cname);
    uint(*object_size) (gs_memory_t * mem, const void *obj);
    gs_memory_type_ptr_t(*object_type) (gs_memory_t * mem, const void *obj);
    void (*free_object) (gs_memory_t * mem, void *data, client_name_t cname);
    byte *(*alloc_string) (gs_memory_t * mem, uint nbytes, client_name_t cname);
    byte *(*alloc_string_immovable) (gs_memory_t * mem, uint nbytes, client_name_t cname);
    byte *(*resize_string) (gs_memory_t * mem, byte * data, uint old_num, uint new_num, client_name_t cname);
    void (*free_string) (gs_memory_t * mem, byte * data, uint nbytes, client_name_t cname);
    void (*register_root) (gs_memory_t * mem, gs_gc_root_t * root, gs_ptr_type_t ptype, void **pp, client_name_t cname);
    void (*unregister_root) (gs_memory_t * mem, gs_gc_root_t * root, client_name_t cname);
    void (*status) (gs_memory_t * mem, gs_memory_status_t * status);
    void (*enable_free) (gs_memory_t * mem, bool enable);
} gs_memory_procs_t;
void gs_ignore_free_object(gs_memory_t * mem, void *data, client_name_t cname);
void gs_ignore_free_string(gs_memory_t * mem, byte * data, uint nbytes, client_name_t cname);
struct gs_memory_s {
    gs_memory_procs_t procs;
};
extern gs_memory_t gs_memory_default;
extern long gs_buildtime;
extern const char *gs_copyright;
extern const char *gs_product;
extern long gs_revision;
extern long gs_revisiondate;
extern long gs_serialnumber;
extern const char *gs_doc_directory;
extern const char *gs_lib_default_path;
extern const char *gs_init_file;
typedef struct ref_s ref;
typedef ushort ref_packed;
typedef enum { t__invalid, t_boolean, t_dictionary, t_file, t_array, t_mixedarray, t_shortarray, t_unused_array_, t_struct, t_astruct, t_fontID, t_integer, t_mark, t_name, t_null, t_operator, t_real, t_save, t_string, t_device, t_oparray, t_next_index } ref_type;
typedef struct attr_print_mask_s {
    ushort mask;
    ushort value;
    char print;
} attr_print_mask;
typedef struct dict_s dict;
typedef struct name_s name;
typedef struct stream_s stream;
typedef struct gx_device_s gx_device;
typedef struct obj_header_s obj_header_t;
typedef int (*op_proc_p) (ref *);
struct tas_s {
    ushort type_attrs;
    ushort rsize;
};
struct ref_s {
    struct tas_s tas;
    union v {
	long intval;
	ushort boolval;
	float realval;
	ulong saveid;
	byte *bytes;
	const byte *const_bytes;
	ref *refs;
	const ref *const_refs;
	name *pname;
	const name *const_pname;
	dict *pdict;
	const dict *const_pdict;
	const ref_packed *packed;
	op_proc_p opproc;
	struct stream_s *pfile;
	struct gx_device_s *pdevice;
	obj_header_t *pstruct;
    } value;
};
typedef enum { i_vm_foreign = 0, i_vm_system, i_vm_global, i_vm_local, i_vm_max = i_vm_local } i_vm_space;
typedef struct gs_ref_memory_s gs_ref_memory_t;
typedef union vm_spaces_s {
    gs_ref_memory_t *indexed[4];
    struct _ssn {
	gs_ref_memory_t *foreign;
	gs_ref_memory_t *system;
	gs_ref_memory_t *global;
	gs_ref_memory_t *local;
    } named;
} vm_spaces;
void gs_reclaim(vm_spaces * pspaces, bool global);
typedef enum { avm_foreign = (i_vm_foreign << 2), avm_system = (i_vm_system << 2), avm_global = (i_vm_global << 2), avm_local = (i_vm_local << 2), avm_max = avm_local } avm_space;
typedef struct {
    const char *oname;
    op_proc_p proc;
} op_def;
typedef const op_def *op_def_ptr;
ushort op_find_index(const ref *);
extern const op_def **op_def_table;
extern uint op_def_count;
typedef struct op_array_table_s {
    ref table;
    ushort *nx_table;
    uint count;
    uint base_index;
    uint attrs;
    ref *root_p;
} op_array_table;
extern op_array_table op_array_table_global, op_array_table_local;
void op_index_ref(uint, ref *);
typedef ulong gs_id;
typedef struct gs_string_s {
    byte *data;
    uint size;
} gs_string;
typedef struct gs_const_string_s {
    const byte *data;
    uint size;
} gs_const_string;
typedef struct gs_point_s {
    double x, y;
} gs_point;
typedef struct gs_int_point_s {
    int x, y;
} gs_int_point;
typedef struct gs_log2_scale_point_s {
    int x, y;
} gs_log2_scale_point;
typedef struct gs_rect_s {
    gs_point p, q;
} gs_rect;
typedef struct gs_int_rect_s {
    gs_int_point p, q;
} gs_int_rect;
typedef struct struct_shared_procs_s struct_shared_procs_t;
struct gs_memory_struct_type_s {
    uint ssize;
    struct_name_t sname;
    const struct_shared_procs_t *shared;
    void (*clear_marks) (void *pre, uint size);
    gs_ptr_type_t(*enum_ptrs) (void *ptr, uint size, uint index, const void **pep);
    void (*reloc_ptrs) (void *ptr, uint size, gc_state_t * gcst);
    void (*finalize) (void *ptr);
};
gs_ptr_type_t gs_no_struct_enum_ptrs(void *ptr, uint size, uint index, const void **pep);
void gs_no_struct_reloc_ptrs(void *ptr, uint size, gc_state_t * gcst);
void *gs_reloc_struct_ptr(const void *, gc_state_t *);
void gs_reloc_string(gs_string *, gc_state_t *);
void gs_reloc_const_string(gs_const_string *, gc_state_t *);
extern const gs_memory_struct_type_t st_free;
extern const gs_memory_struct_type_t st_bytes;
typedef struct stream_state_s stream_state;
typedef struct stream_template_s stream_template;
typedef struct stream_cursor_read_s {
    const byte *ptr;
    const byte *limit;
    byte *_skip;
} stream_cursor_read;
typedef struct stream_cursor_write_s {
    const byte *_skip;
    byte *ptr;
    byte *limit;
} stream_cursor_write;
typedef union stream_cursor_s {
    stream_cursor_read r;
    stream_cursor_write w;
} stream_cursor;
int s_no_report_error(stream_state *, const char *);
struct stream_state_s {
    const stream_template *template;
    gs_memory_t *memory;
    int (*report_error) (stream_state *, const char *);
};
extern const gs_memory_struct_type_t st_stream_state;
typedef struct {
    int (*available) (stream *, long *);
    int (*seek) (stream *, long);
    void (*reset) (stream *);
    int (*flush) (stream *);
    int (*close) (stream *);
    int (*process) (stream_state *, stream_cursor_read *, stream_cursor_write *, bool);
    int (*switch_mode) (stream *, bool);
} stream_procs;
struct stream_s {
    const stream_template *template;
    gs_memory_t *memory;
    int (*report_error) (stream_state *, const char *);
    stream_cursor cursor;
    byte *cbuf;
    uint bsize;
    uint cbsize;
    short end_status;
    byte foreign;
    byte modes;
    gs_string cbuf_string;
    long position;
    stream_procs procs;
    stream *strm;
    int is_temp;
    int inline_temp;
    stream_state *state;
    ushort read_id;
    ushort write_id;
    stream *prev, *next;
    int save_count;
    FILE *file;
    uint file_modes;
    int (*save_close) (stream *);
};
int savailable(stream *, long *);
int sclose(stream *);
int sswitch(stream *, bool);
int spgetcc(stream *, bool);
int sgets(stream *, byte *, uint, uint *);
int sungetc(stream *, byte);
int spskip(stream *, long, long *);
int s_process_read_buf(stream *);
int spputc(stream *, byte);
int sputs(stream *, const byte *, uint, uint *);
int s_process_write_buf(stream *, bool);
long stell(stream *);
int spseek(stream *, long);
stream *s_alloc(gs_memory_t *, client_name_t);
stream_state *s_alloc_state(gs_memory_t *, gs_memory_type_ptr_t, client_name_t);
void sread_string(stream *, const byte *, uint), swrite_string(stream *, byte *, uint);
void sread_file(stream *, FILE *, byte *, uint), swrite_file(stream *, FILE *, byte *, uint), sappend_file(stream *, FILE *, byte *, uint);
void s_std_init(stream *, byte *, uint, const stream_procs *, int);
void s_disable(stream *);
int s_std_null(stream *);
void s_std_read_reset(stream *), s_std_write_reset(stream *);
int s_std_read_flush(stream *), s_std_write_flush(stream *), s_std_noavailable(stream *, long *), s_std_noseek(stream *, long), s_std_close(stream *), s_std_switch_mode(stream *, bool);
int s_filter_write_flush(stream *), s_filter_close(stream *);
int zget_stdin(stream **);
int zget_stdout(stream **);
int zget_stderr(stream **);
extern bool gs_stdin_is_interactive;
extern stream *invalid_file_entry;
int file_switch_to_read(const ref *);
int file_switch_to_write(const ref *);
extern const uint file_default_buffer_size;
FILE *lib_fopen(const char *);
int lib_file_open(const char *, uint, byte *, uint, uint *, ref *);
int file_read_string(const byte *, uint, ref *);
int filter_open(const char *, uint, ref *, const stream_procs *, const stream_template *, const stream_state *);
void make_stream_file(ref *, stream *, const char *);
int file_close_finish(stream *);
int file_close_disable(stream *);
int file_close_file(stream *);
int file_close(ref *);
stream *file_alloc_stream(gs_memory_t *, client_name_t);
void file_save(void);
int zreadline_from(stream *, byte *, uint, uint *, bool *);
void gs_exit_with_code(int exit_status, int code);
void gs_exit(int exit_status);
extern int gs_exit_status;
typedef struct gs_main_instance_s gs_main_instance;
gs_main_instance *gs_main_instance_default(void);
void gs_get_real_stdio(FILE * stdfiles[3]);
void gs_main_init0(gs_main_instance * minst, FILE * in, FILE * out, FILE * err, int max_lib_paths);
void gs_main_init1(gs_main_instance * minst);
void gs_main_init2(gs_main_instance * minst);
void gs_main_add_lib_path(gs_main_instance * minst, const char *path);
void gs_main_set_lib_paths(gs_main_instance * minst);
int gs_main_lib_open(gs_main_instance * minst, const char *fname, ref * pfile);
int gs_main_run_file(gs_main_instance * minst, const char *fname, int user_errors, int *pexit_code, ref * perror_object);
int gs_main_run_string(gs_main_instance * minst, const char *str, int user_errors, int *pexit_code, ref * perror_object);
int gs_main_run_string_with_length(gs_main_instance * minst, const char *str, uint length, int user_errors, int *pexit_code, ref * perror_object);
int gs_main_run_file_open(gs_main_instance * minst, const char *file_name, ref * pfref);
int gs_main_run_string_begin(gs_main_instance * minst, int user_errors, int *pexit_code, ref * perror_object);
int gs_main_run_string_continue(gs_main_instance * minst, const char *str, uint length, int user_errors, int *pexit_code, ref * perror_object);
int gs_main_run_string_end(gs_main_instance * minst, int user_errors, int *pexit_code, ref * perror_object);
int gs_push_boolean(gs_main_instance * minst, bool value);
int gs_push_integer(gs_main_instance * minst, long value);
int gs_push_real(gs_main_instance * minst, floatp value);
int gs_push_string(gs_main_instance * minst, byte * chars, uint length, bool read_only);
int gs_pop_boolean(gs_main_instance * minst, bool * result);
int gs_pop_integer(gs_main_instance * minst, long *result);
int gs_pop_real(gs_main_instance * minst, float *result);
int gs_pop_string(gs_main_instance * minst, gs_string * result);
void gs_debug_dump_stack(int code, ref * perror_object);
void gs_main_finit(gs_main_instance * minst, int exit_status, int code);
int gs_interpret(ref * pref, int user_errors, int *pexit_code, ref * perror_object);
typedef struct gs_file_path_s {
    ref container;
    ref list;
    const char *env;
    const char *final;
    uint count;
} gs_file_path;
struct gs_main_instance_s {
    FILE *fstdin;
    FILE *fstdout;
    FILE *fstderr;
    uint memory_chunk_size;
    ulong name_table_size;
    int init_done;
    int user_errors;
    bool search_here_first;
    bool run_start;
    gs_file_path lib_path;
};
extern const gs_main_instance gs_main_instance_init_values;
const gs_main_instance gs_main_instance_init_values = { 0, 0, 0, 20000, 0, -1, 0, 1, 1 };

struct {
    struct tas_s tas;
    const char *value;
} gs_init_file_array[] = { { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_type1.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_dps1.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_lev2.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_res.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_typ42.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_cidfn.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_cmap.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_setpd.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_btokn.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_dps2.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_mex_e.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_mro_e.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_pdf_e.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_wan_e.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_pdf.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_l2img.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "pdf_base.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "pdf_draw.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "pdf_font.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "pdf_main.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "pdf_sec.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "pdf_2ps.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_cff.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_ttf.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "gs_pdfwr.ps"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, 0},};

struct {
    struct tas_s tas;
    const char *value;
} gs_emulator_name_array[] = { { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "PostScript"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "PostScriptLevel1"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "PostScriptLevel2"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, "PDF"}, { {
(t_string << 8) + (0x20 + 0x40) + avm_foreign, 0}, 0},};

extern op_def_ptr zchar1_op_defs(void), zfont1_op_defs(void), zmisc1_op_defs(void), zvmem2_op_defs(void), zdps1_l2_op_defs(void), zmisc2_op_defs(void), zusparam_op_defs(void), zchar42_op_defs(void), zfont42_op_defs(void), zcid_op_defs(void), zcie_l2_op_defs(void), zcrd_l2_op_defs(void), zfcmap_op_defs(void), zfont0_op_defs(void), zchar2_op_defs(void), zfdcte_op_defs(void), zfdctd_op_defs(void), zdevice2_l2_op_defs(void), ziodev2_l2_op_defs(void), zmedia2_l2_op_defs(void), zbseq_l2_op_defs(void), zcolor1_op_defs(void), zht1_op_defs(void), zupath_l2_op_defs(void), ireclaim_l2_op_defs(void), zchar2_l2_op_defs(void), zfdecode_op_defs(void), zfilter2_op_defs(void), zarith_op_defs(void), zarray_op_defs(void), zcontrol_op_defs(void), zdict_op_defs(void), zfile_op_defs(void), zfileio_op_defs(void), zfilter_op_defs(void), zfproc_op_defs(void), zgeneric_op_defs(void), ziodev_op_defs(void), zmath_op_defs(void), zmisc_op_defs(void), zpacked_op_defs(void), zrelbit_op_defs(void), zstack_op_defs(void), zstring_op_defs(void), zsysvm_op_defs(void), ztoken_op_defs(void), ztype_op_defs(void), zvmem_op_defs(void), zchar_op_defs(void), zcolor_op_defs(void), zdevice_op_defs(void), zfont_op_defs(void), zfont2_op_defs(void), zgstate_op_defs(void), zht_op_defs(void), zimage_op_defs(void), zmatrix_op_defs(void), zpaint_op_defs(void), zpath_op_defs(void), zfbcp_op_defs(void), zhsb_op_defs(void), zpath1_op_defs(void), zpcolor_l2_op_defs(void), zcolor2_l2_op_defs(void), zcsindex_l2_op_defs(void), zht2_l2_op_defs(void), zimage2_l2_op_defs(void), zcssepr_l2_op_defs(void), zfilterx_op_defs(void), zfzlib_op_defs(void), interp_op_defs(void);
op_def_ptr(*(op_defs_all[]))(void) = {
zchar1_op_defs, zfont1_op_defs, zmisc1_op_defs, zvmem2_op_defs, zdps1_l2_op_defs, zmisc2_op_defs, zusparam_op_defs, zchar42_op_defs, zfont42_op_defs, zcid_op_defs, zcie_l2_op_defs, zcrd_l2_op_defs, zfcmap_op_defs, zfont0_op_defs, zchar2_op_defs, zfdcte_op_defs, zfdctd_op_defs, zdevice2_l2_op_defs, ziodev2_l2_op_defs, zmedia2_l2_op_defs, zbseq_l2_op_defs, zcolor1_op_defs, zht1_op_defs, zupath_l2_op_defs, ireclaim_l2_op_defs, zchar2_l2_op_defs, zfdecode_op_defs, zfilter2_op_defs, zarith_op_defs, zarray_op_defs, zcontrol_op_defs, zdict_op_defs, zfile_op_defs, zfileio_op_defs, zfilter_op_defs, zfproc_op_defs, zgeneric_op_defs, ziodev_op_defs, zmath_op_defs, zmisc_op_defs, zpacked_op_defs, zrelbit_op_defs, zstack_op_defs, zstring_op_defs, zsysvm_op_defs, ztoken_op_defs, ztype_op_defs, zvmem_op_defs, zchar_op_defs, zcolor_op_defs, zdevice_op_defs, zfont_op_defs, zfont2_op_defs, zgstate_op_defs, zht_op_defs, zimage_op_defs, zmatrix_op_defs, zpaint_op_defs, zpath_op_defs, zfbcp_op_defs, zhsb_op_defs, zpath1_op_defs, zpcolor_l2_op_defs, zcolor2_l2_op_defs, zcsindex_l2_op_defs, zht2_l2_op_defs, zimage2_l2_op_defs, zcssepr_l2_op_defs, zfilterx_op_defs, zfzlib_op_defs, interp_op_defs, 0};
