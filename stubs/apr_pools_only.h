#ifndef APR_POOLS_H
#define APR_POOLS_H

/** Modified version for JAN to stub-it-up... 
 * only list the exposed functions and nothing else! 
 * For use w/ the pre-processed and CIL'ified httpd_comb.c */



#ifdef __cplusplus
extern "C" {
#endif

  /**********/

/*
 * Initialization
 */

  apr_status_t apr_pool_initialize(void) {
    return rand ();
  }


  void apr_pool_terminate(void) {
    return;
  }

/*
 * Pool creation/destruction
 */

  apr_status_t apr_pool_create_ex(apr_pool_t **newpool,
                                  apr_pool_t *parent,
                                  apr_abortfunc_t abort_fn,
                                  apr_allocator_t *allocator) {
    return 0;
  }
  //#define apr_pool_create_ex nopStat4


  apr_status_t apr_pool_create_ex_debug(apr_pool_t **newpool,
                                        apr_pool_t *parent,
                                        apr_abortfunc_t abort_fn,
                                        apr_allocator_t *allocator,
                                        const char *file_line) {
    return 0;
  }

  //#define apr_pool_create_ex_debug nopStat5

#define apr_pool_create(newpool, parent) \
    apr_pool_create_ex(newpool, parent, 0, 0)

  apr_allocator_t * apr_pool_allocator_get(apr_pool_t *pool) {
    return 0;
  }
  //#define apr_pool_allocator_get nopPtr1

  
  void apr_pool_clear(apr_pool_t *p) {
    return;
  }
  //#define apr_pool_clear nop1 

void apr_pool_clear_debug(apr_pool_t *p,
                          const char *file_line) {
  return;
}
  //#define apr_pool_clear_debug nop2
  
  void apr_pool_destroy(apr_pool_t *p) {
    return;
  }
  //#define apr_pool_destroy nop1

  void apr_pool_destroy_debug(apr_pool_t *p,
                              const char *file_line) {
    return;
  }
//#define apr_pool_destroy_debug nop2


/***** Allocation *****/

/* Leave this, we handle it separately */
void * apr_palloc(apr_pool_t *p, apr_size_t size);


/* APR_DECLARE(void *) apr_palloc_debug(apr_pool_t *p, apr_size_t size,
                                     const char *file_line);
                                     */
#define apr_palloc_debug(p, s, f) \
  apr_palloc(p, s)

/**
 * Allocate a block of memory from a pool and set all of the memory to 0
 * @param p The pool to allocate from
 * @param size The amount of memory to allocate
 * @return The allocated memory
 */
#if defined(DOXYGEN)
APR_DECLARE(void *) apr_pcalloc(apr_pool_t *p, apr_size_t size);
#elif !APR_POOL_DEBUG
#define apr_pcalloc(p, size) memset(apr_palloc(p, size), 0, size)
#endif

/* APR_DECLARE(void *) apr_pcalloc_debug(apr_pool_t *p, apr_size_t size,
                                      const char *file_line);
*/
#define apr_pcalloc_debug(p, s, f) \
  memset(apr_palloc(p, size), 0, size)

/*
 * Pool Properties
 */


  void apr_pool_abort_set(apr_abortfunc_t abortfunc,
                          apr_pool_t *pool) {
    return;
  }

  //#define apr_pool_abort_set nop2

  apr_abortfunc_t apr_pool_abort_get(apr_pool_t *pool) {
    return 0; // pick one, or actually rip it from pool?
  }
  //#define apr_pool_abort_get nopPtr1

  apr_pool_t * apr_pool_parent_get(apr_pool_t *pool) {
    return 0; // ? 
  }
  //#define apr_pool_parent_get nopPtr1

  int apr_pool_is_ancestor(apr_pool_t *a, apr_pool_t *b) {
    return 0;
  }
  //#define apr_pool_is_ancestor nopPtr2

  void apr_pool_tag(apr_pool_t *pool, const char *tag) {
    return;
  }
  //#define apr_pool_tag nop2


apr_status_t apr_pool_userdata_set(
                                   const void *data,
                                   const char *key,
                                   apr_status_t (*cleanup)(void *),
                                   apr_pool_t *pool) {
  return 0;
}
  //#define apr_pool_userdata_set nopStat4


apr_status_t apr_pool_userdata_setn(
                                    const void *data,
                                    const char *key,
                                    apr_status_t (*cleanup)(void *),
                                    apr_pool_t *pool) {
  return 0;
}
  //#define apr_pool_userdata_setn nopStat4


  apr_status_t apr_pool_userdata_get(void **data, const char *key,
                                     apr_pool_t *pool) {
    return 0;
  }                                             
  //#define apr_pool_userdata_get nopStat3



  void apr_pool_cleanup_register(
                                 apr_pool_t *p,
                                 const void *data,
                                 apr_status_t (*plain_cleanup)(void *),
                                 apr_status_t (*child_cleanup)(void *)) {
    return;
  }
  //#define apr_pool_cleanup_register nop4


  void apr_pool_cleanup_kill(apr_pool_t *p, const void *data,
                             apr_status_t (*cleanup)(void *)) {
    return;
  }
  //#define apr_pool_cleanup_kill nop3


  void apr_pool_child_cleanup_set(
                                  apr_pool_t *p,
                                  const void *data,
                                  apr_status_t (*plain_cleanup)(void *),
                                  apr_status_t (*child_cleanup)(void *)) {
    return;
  }
  //#define apr_pool_child_cleanup_set nop4


  apr_status_t apr_pool_cleanup_run(
                                    apr_pool_t *p,
                                    void *data,
                                    apr_status_t (*cleanup)(void *)) {
    return 0;
  }
  //#define apr_pool_cleanup_run nopStat3

  apr_status_t apr_pool_cleanup_null(void *data) {
    return 0;
  }
  //#define apr_pool_cleanup_null nopStat1
  
  void apr_pool_cleanup_for_exec(void) {
    return;
  }
  //#define apr_pool_cleanup_for_exec nop0


  void apr_pool_join(apr_pool_t *p, apr_pool_t *sub) {
    return;
  }
  //#define apr_pool_join nop2

  apr_pool_t * apr_pool_find(const void *mem) {
    return 0;
  }
  //#define apr_pool_find nopPtr1


  apr_size_t apr_pool_num_bytes(apr_pool_t *p, int recurse) {
    return 0;
  }
  //#define apr_pool_num_bytes nopStat2

  void apr_pool_lock(apr_pool_t *pool, int flag) {
    return;
  }
  //#define apr_pool_lock nop2



#ifdef __cplusplus
}
#endif

#endif /* !APR_POOLS_H */
