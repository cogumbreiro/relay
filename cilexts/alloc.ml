
(** Tests/operations on allocation functions *)

let allocFNs = ["kmalloc";
                "malloc";
                "realloc";
                "calloc";
                "__builtin_alloca";
                "xalloc";
                "vmalloc";
                "valloc";
                "kmem_cache_alloc";
                "mempool_alloc";
                (* __alloc_percpu ?*)

                (* Apache *)
                "apr_palloc";
                "apr_pcalloc";
                "apr_bucket_alloc";

                (* OpenSSL *)
                "CRYPTO_malloc";
                "CRYPTO_realloc";
                "CRYPTO_realloc_clean";
                "CRYPTO_malloc_locked";
                "CRYPTO_remalloc";

               ]

let allocFuns = Hashtbl.create 10

let _ = 
  List.iter (fun vname -> Hashtbl.add allocFuns vname ()) allocFNs
    
(** Tests if given string is the name of an allocation function
    @param name      string to test
    @return true if string is the name of an allocator function  *)
let isAlloc (fname:string) =
  Hashtbl.mem allocFuns fname

(* TODO add interface for reading in more... *)

