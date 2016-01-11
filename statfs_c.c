#include <errno.h>
#include <string.h>
#include <sys/vfs.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static value
copy_statfs (struct statfs *buf)
{
    CAMLparam0 ();
    CAMLlocal2 (bufv, v);
    bufv = caml_alloc (9, 0);
    v = copy_int64 (buf->f_type); caml_modify (&Field (bufv, 0), v);
    v = copy_int64 (buf->f_bsize); caml_modify (&Field (bufv, 1), v);
    v = copy_int64 (buf->f_blocks); caml_modify (&Field (bufv, 2), v);
    v = copy_int64 (buf->f_bfree); caml_modify (&Field (bufv, 3), v);
    v = copy_int64 (buf->f_bavail); caml_modify (&Field (bufv, 4), v);
    v = copy_int64 (buf->f_files); caml_modify (&Field (bufv, 5), v);
    v = copy_int64 (buf->f_ffree); caml_modify (&Field (bufv, 6), v);
    caml_modify (&Field (bufv, 7), Val_unit);
    v = copy_int64 (buf->f_namelen); caml_modify (&Field (bufv, 8), v);
    CAMLreturn (bufv);
}

CAMLprim value
statfs_statfs (value pathv)
{
   CAMLparam1 (pathv);
   CAMLlocal1 (bufv);
   const char *path = String_val (pathv);
   struct statfs buf;
   if (statfs (path, &buf) == -1)
     caml_failwith (strerror (errno));
   bufv = copy_statfs (&buf);
   CAMLreturn (bufv);
}

CAMLprim value
statfs_fstatfs (value fdv)
{
   CAMLparam1 (fdv);
   CAMLlocal1 (bufv);
   int fd = Int_val (fdv);
   struct statfs buf;
   if (fstatfs (fd, &buf) == -1)
     caml_failwith (strerror (errno));
   bufv = copy_statfs (&buf);
   CAMLreturn (bufv);
}


