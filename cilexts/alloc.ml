
(** Library of tests/operations on allocation functions *)

let allocFNs = ["kmalloc";
                "malloc";
                "realloc";
                "calloc";
                "__builtin_alloca";
                "xalloc";
                "vmalloc";
                "kmem_cache_alloc"; (* assume race-free *)
                "mempool_alloc";    (* assume race-free *)

                (* Apache *)
                "apr_palloc";
                "apr_pcalloc";

                (* OpenSSL *)
                "CRYPTO_malloc";
                "CRYPTO_realloc";
                "CRYPTO_realloc_clean";
                "CRYPTO_malloc_locked";
                "CRYPTO_remalloc";

               ]

(** Tests if given string is the name of an allocation function
    @param name      string to test
    @return true if string is the name of an allocator function  *)
let isAlloc (fname:string) =
  List.mem fname allocFNs

(* TODO add interface for reading in more... if list grows, 
   switch to a hash table? *)

