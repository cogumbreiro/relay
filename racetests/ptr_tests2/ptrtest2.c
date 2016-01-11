

extern struct _IO_FILE *stdin ;
extern struct _IO_FILE *stderr ;
extern struct _IO_FILE *stdout ;


////////////////////////////////////////////////////////////////////////
///// FROM HTTPD


typedef unsigned int size_t;
typedef unsigned int __socklen_t;
typedef __socklen_t socklen_t;
typedef unsigned short sa_family_t;
struct sockaddr {
   sa_family_t sa_family ;
   char sa_data[14] ;
};

extern void *memcpy(void * __restrict  __dest , void const   * __restrict  __src , size_t __n ) ;
extern void *malloc(size_t __size )  __attribute__((__malloc__)) ;


struct sockaddr_in;
struct sockaddr_in6;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned short apr_uint16_t;
typedef int apr_int32_t;
typedef long long apr_int64_t;
typedef socklen_t apr_socklen_t;
struct apr_pool_t;
typedef struct apr_pool_t apr_pool_t;
struct apr_array_header_t;
typedef struct apr_array_header_t apr_array_header_t;
struct apr_array_header_t {
   apr_pool_t *pool ;
   int elt_size ;
   int nelts ;
   int nalloc ;
   char *elts ;
};
typedef apr_int64_t apr_interval_time_t;
struct apr_file_t;
typedef struct apr_file_t apr_file_t;
typedef uint16_t in_port_t;
typedef uint32_t in_addr_t;
struct in_addr {
   in_addr_t s_addr ;
};
union __anonunion_in6_u_54 {
   uint8_t u6_addr8[16] ;
   uint16_t u6_addr16[8] ;
   uint32_t u6_addr32[4] ;
};
struct in6_addr {
   union __anonunion_in6_u_54 in6_u ;
};
struct sockaddr_in {
   sa_family_t sin_family ;
   in_port_t sin_port ;
   struct in_addr sin_addr ;
   unsigned char sin_zero[(int )(((sizeof(struct sockaddr ) - sizeof(unsigned short )) -
                                  sizeof(in_port_t )) - sizeof(struct in_addr ))] ;
};
struct sockaddr_in6 {
   sa_family_t sin6_family ;
   in_port_t sin6_port ;
   uint32_t sin6_flowinfo ;
   struct in6_addr sin6_addr ;
   uint32_t sin6_scope_id ;
};
typedef apr_uint16_t apr_port_t;
struct apr_sockaddr_t;
typedef struct apr_sockaddr_t apr_sockaddr_t;
union __anonunion_sa_57 {
   struct sockaddr_in sin ;
   struct sockaddr_in6 sin6 ;
};
struct apr_sockaddr_t {
   apr_pool_t *pool ;
   char *hostname ;
   char *servname ;
   apr_port_t port ;
   apr_int32_t family ;
   union __anonunion_sa_57 sa ;
   apr_socklen_t salen ;
   int ipaddr_len ;
   int addr_str_len ;
   void *ipaddr_ptr ;
   apr_sockaddr_t *next ;
};
struct ap_method_list_t;
typedef struct ap_method_list_t ap_method_list_t;
struct ap_method_list_t {
   apr_int64_t method_mask ;
   apr_array_header_t *method_list ;
};
struct ap_conf_vector_t;
struct process_rec;
typedef struct process_rec process_rec;
struct server_rec;
typedef struct server_rec server_rec;
struct process_rec {
   apr_pool_t *pool ;
   apr_pool_t *pconf ;
   int argc ;
   char const   * const  *argv ;
   char const   *short_name ;
};
struct server_addr_rec;
typedef struct server_addr_rec server_addr_rec;
struct server_addr_rec {
   server_addr_rec *next ;
   apr_sockaddr_t *host_addr ;
   apr_port_t host_port ;
   char *virthost ;
};
struct server_rec {
   process_rec *process ;
   server_rec *next ;
   char const   *defn_name ;
   unsigned int defn_line_number ;
   char *server_admin ;
   char *server_hostname ;
   apr_port_t port ;
   char *error_fname ;
   apr_file_t *error_log ;
   int loglevel ;
   int is_virtual ;
   struct ap_conf_vector_t *module_config ;
   struct ap_conf_vector_t *lookup_defaults ;
   server_addr_rec *addrs ;
   apr_interval_time_t timeout ;
   apr_interval_time_t keep_alive_timeout ;
   int keep_alive_max ;
   int keep_alive ;
   char const   *path ;
   int pathlen ;
   apr_array_header_t *names ;
   apr_array_header_t *wild_names ;
   int limit_req_line ;
   int limit_req_fieldsize ;
   int limit_req_fields ;
};
struct ap_directive_t;
typedef struct ap_directive_t ap_directive_t;
struct ap_directive_t {
   char const   *directive ;
   char const   *args ;
   struct ap_directive_t *next ;
   struct ap_directive_t *first_child ;
   struct ap_directive_t *parent ;
   void *data ;
   char const   *filename ;
   int line_num ;
};
enum cmd_how {
    RAW_ARGS = 0,
    TAKE1 = 1,
    TAKE2 = 2,
    ITERATE = 3,
    ITERATE2 = 4,
    FLAG = 5,
    NO_ARGS = 6,
    TAKE12 = 7,
    TAKE3 = 8,
    TAKE23 = 9,
    TAKE123 = 10,
    TAKE13 = 11
};
struct cmd_parms_struct;
typedef struct cmd_parms_struct cmd_parms;
union __anonunion_cmd_func_69 {
   char const   *(*no_args)(cmd_parms *parms , void *mconfig ) ;
   char const   *(*raw_args)(cmd_parms *parms , void *mconfig , char const   *args ) ;
   char const   *(*take1)(cmd_parms *parms , void *mconfig , char const   *w ) ;
   char const   *(*take2)(cmd_parms *parms , void *mconfig , char const   *w , char const   *w2 ) ;
   char const   *(*take3)(cmd_parms *parms , void *mconfig , char const   *w , char const   *w2 ,
                          char const   *w3 ) ;
   char const   *(*flag)(cmd_parms *parms , void *mconfig , int on ) ;
};
typedef union __anonunion_cmd_func_69 cmd_func;
struct command_struct;
typedef struct command_struct command_rec;
struct command_struct {
   char const   *name ;
   cmd_func func ;
   void *cmd_data ;
   int req_override ;
   enum cmd_how args_how ;
   char const   *errmsg ;
};
struct ap_configfile_t;
typedef struct ap_configfile_t ap_configfile_t;
struct ap_configfile_t {
   int (*getch)(void *param ) ;
   void *(*getstr)(void *buf , size_t bufsiz , void *param ) ;
   int (*close)(void *param ) ;
   void *param ;
   char const   *name ;
   unsigned int line_number ;
};
struct cmd_parms_struct {
   void *info ;
   int override ;
   apr_int64_t limited ;
   apr_array_header_t *limited_xmethods ;
   ap_method_list_t *xlimited ;
   ap_configfile_t *config_file ;
   ap_directive_t *directive ;
   apr_pool_t *pool ;
   apr_pool_t *temp_pool ;
   server_rec *server ;
   char *path ;
   command_rec const   *cmd ;
   struct ap_conf_vector_t *context ;
   ap_directive_t const   *err_directive ;
};
struct module_struct;
typedef struct module_struct module;
struct module_struct {
   int version ;
   int minor_version ;
   int module_index ;
   char const   *name ;
   void *dynamic_load_handle ;
   struct module_struct *next ;
   unsigned long magic ;
   void (*rewrite_args)(process_rec *process ) ;
   void *(*create_dir_config)(apr_pool_t *p , char *dir ) ;
   void *(*merge_dir_config)(apr_pool_t *p , void *base_conf , void *new_conf ) ;
   void *(*create_server_config)(apr_pool_t *p , server_rec *s ) ;
   void *(*merge_server_config)(apr_pool_t *p , void *base_conf , void *new_conf ) ;
   command_rec const   *cmds ;
   void (*register_hooks)(apr_pool_t *p ) ;
};
typedef unsigned long long __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long __ino_t;
typedef long __off_t;
typedef __ino_t ino_t;
typedef __dev_t dev_t;
typedef __gid_t gid_t;
typedef __uid_t uid_t;
typedef __off_t off_t;
typedef size_t apr_size_t;
typedef off_t apr_off_t;
typedef int apr_status_t;
typedef apr_int64_t apr_time_t;
typedef uid_t apr_uid_t;
typedef gid_t apr_gid_t;
enum __anonenum_apr_filetype_e_42 {
    APR_NOFILE = 0,
    APR_REG = 1,
    APR_DIR = 2,
    APR_CHR = 3,
    APR_BLK = 4,
    APR_PIPE = 5,
    APR_LNK = 6,
    APR_SOCK = 7,
    APR_UNKFILE = 127
};
typedef enum __anonenum_apr_filetype_e_42 apr_filetype_e;
typedef apr_int32_t apr_fileperms_t;
typedef ino_t apr_ino_t;
typedef dev_t apr_dev_t;
struct apr_finfo_t;
typedef struct apr_finfo_t apr_finfo_t;
struct apr_finfo_t {
   apr_pool_t *pool ;
   apr_int32_t valid ;
   apr_fileperms_t protection ;
   apr_filetype_e filetype ;
   apr_uid_t user ;
   apr_gid_t group ;
   apr_ino_t inode ;
   apr_dev_t device ;
   apr_int32_t nlink ;
   apr_off_t size ;
   apr_off_t csize ;
   apr_time_t atime ;
   apr_time_t mtime ;
   apr_time_t ctime ;
   char const   *fname ;
   char const   *name ;
   struct apr_file_t *filehand ;
};
struct apr_ipsubnet_t;
typedef struct apr_ipsubnet_t apr_ipsubnet_t;
struct apr_table_t;
typedef struct apr_table_t apr_table_t;
enum __anonenum_apr_read_type_e_60 {
    APR_BLOCK_READ = 0,
    APR_NONBLOCK_READ = 1
};
typedef enum __anonenum_apr_read_type_e_60 apr_read_type_e;
struct apr_bucket_brigade;
typedef struct apr_bucket_brigade apr_bucket_brigade;
struct apr_bucket;
typedef struct apr_bucket apr_bucket;
struct apr_bucket_alloc_t;
typedef struct apr_bucket_alloc_t apr_bucket_alloc_t;
struct apr_bucket_type_t;
typedef struct apr_bucket_type_t apr_bucket_type_t;
enum __anonenum_is_metadata_61 {
    APR_BUCKET_DATA = 0,
    APR_BUCKET_METADATA = 1
};
struct apr_bucket_type_t {
   char const   *name ;
   int num_func ;
   enum __anonenum_is_metadata_61 is_metadata ;
   void (*destroy)(void *data ) ;
   apr_status_t (*read)(apr_bucket *b , char const   **str , apr_size_t *len , apr_read_type_e block ) ;
   apr_status_t (*setaside)(apr_bucket *e , apr_pool_t *pool ) ;
   apr_status_t (*split)(apr_bucket *e , apr_size_t point ) ;
   apr_status_t (*copy)(apr_bucket *e , apr_bucket **c ) ;
};
struct __anonstruct_link_62 {
   struct apr_bucket *next ;
   struct apr_bucket *prev ;
};
struct apr_bucket {
   struct __anonstruct_link_62 link ;
   apr_bucket_type_t const   *type ;
   apr_size_t length ;
   apr_off_t start ;
   void *data ;
   void (*free)(void *e ) ;
   apr_bucket_alloc_t *list ;
};
struct apr_bucket_list {
   struct apr_bucket *next ;
   struct apr_bucket *prev ;
};
struct apr_bucket_brigade {
   apr_pool_t *p ;
   struct apr_bucket_list list ;
   apr_bucket_alloc_t *bucket_alloc ;
};
struct htaccess_result {
   char const   *dir ;
   int override ;
   struct ap_conf_vector_t *htaccess ;
   struct htaccess_result  const  *next ;
};
struct conn_rec;
typedef struct conn_rec conn_rec;
struct request_rec;
typedef struct request_rec request_rec;
struct apr_uri_t;
typedef struct apr_uri_t apr_uri_t;
struct hostent;
struct apr_uri_t {
   char *scheme ;
   char *hostinfo ;
   char *user ;
   char *password ;
   char *hostname ;
   char *port_str ;
   char *path ;
   char *query ;
   char *fragment ;
   struct hostent *hostent ;
   apr_port_t port ;
   unsigned int is_initialized : 1 ;
   unsigned int dns_looked_up : 1 ;
   unsigned int dns_resolved : 1 ;
};
struct ap_filter_t;
struct request_rec {
   apr_pool_t *pool ;
   conn_rec *connection ;
   server_rec *server ;
   request_rec *next ;
   request_rec *prev ;
   request_rec *main ;
   char *the_request ;
   int assbackwards ;
   int proxyreq ;
   int header_only ;
   char *protocol ;
   int proto_num ;
   char const   *hostname ;
   apr_time_t request_time ;
   char const   *status_line ;
   int status ;
   char const   *method ;
   int method_number ;
   apr_int64_t allowed ;
   apr_array_header_t *allowed_xmethods ;
   ap_method_list_t *allowed_methods ;
   apr_off_t sent_bodyct ;
   apr_off_t bytes_sent ;
   apr_time_t mtime ;
   int chunked ;
   char const   *range ;
   apr_off_t clength ;
   apr_off_t remaining ;
   apr_off_t read_length ;
   int read_body ;
   int read_chunked ;
   unsigned int expecting_100 ;
   apr_table_t *headers_in ;
   apr_table_t *headers_out ;
   apr_table_t *err_headers_out ;
   apr_table_t *subprocess_env ;
   apr_table_t *notes ;
   char const   *content_type ;
   char const   *handler ;
   char const   *content_encoding ;
   apr_array_header_t *content_languages ;
   char *vlist_validator ;
   char *user ;
   char *ap_auth_type ;
   int no_cache ;
   int no_local_copy ;
   char *unparsed_uri ;
   char *uri ;
   char *filename ;
   char *canonical_filename ;
   char *path_info ;
   char *args ;
   apr_finfo_t finfo ;
   apr_uri_t parsed_uri ;
   int used_path_info ;
   struct ap_conf_vector_t *per_dir_config ;
   struct ap_conf_vector_t *request_config ;
   struct htaccess_result  const  *htaccess ;
   struct ap_filter_t *output_filters ;
   struct ap_filter_t *input_filters ;
   struct ap_filter_t *proto_output_filters ;
   struct ap_filter_t *proto_input_filters ;
   int eos_sent ;
};
enum __anonenum_ap_conn_keepalive_e_69 {
    AP_CONN_UNKNOWN = 0,
    AP_CONN_CLOSE = 1,
    AP_CONN_KEEPALIVE = 2
};
typedef enum __anonenum_ap_conn_keepalive_e_69 ap_conn_keepalive_e;
struct conn_rec {
   apr_pool_t *pool ;
   server_rec *base_server ;
   void *vhost_lookup_data ;
   apr_sockaddr_t *local_addr ;
   apr_sockaddr_t *remote_addr ;
   char *remote_ip ;
   char *remote_host ;
   char *remote_logname ;
   unsigned int aborted : 1 ;
   ap_conn_keepalive_e keepalive ;
   int double_reverse : 2 ;
   int keepalives ;
   char *local_ip ;
   char *local_host ;
   long id ;
   struct ap_conf_vector_t *conn_config ;
   apr_table_t *notes ;
   struct ap_filter_t *input_filters ;
   struct ap_filter_t *output_filters ;
   void *sbh ;
   struct apr_bucket_alloc_t *bucket_alloc ;
};
enum __anonenum_ap_input_mode_t_70 {
    AP_MODE_READBYTES = 0,
    AP_MODE_GETLINE = 1,
    AP_MODE_EATCRLF = 2,
    AP_MODE_SPECULATIVE = 3,
    AP_MODE_EXHAUSTIVE = 4,
    AP_MODE_INIT = 5
};
typedef enum __anonenum_ap_input_mode_t_70 ap_input_mode_t;
typedef struct ap_filter_t ap_filter_t;
union ap_filter_func {
   apr_status_t (*out_func)(ap_filter_t *f , apr_bucket_brigade *b ) ;
   apr_status_t (*in_func)(ap_filter_t *f , apr_bucket_brigade *b , ap_input_mode_t mode ,
                           apr_read_type_e block , apr_off_t readbytes ) ;
};
typedef union ap_filter_func ap_filter_func;
enum __anonenum_ap_filter_type_71 {
    AP_FTYPE_RESOURCE = 10,
    AP_FTYPE_CONTENT_SET = 20,
    AP_FTYPE_PROTOCOL = 30,
    AP_FTYPE_TRANSCODE = 40,
    AP_FTYPE_CONNECTION = 50,
    AP_FTYPE_NETWORK = 60
};
typedef enum __anonenum_ap_filter_type_71 ap_filter_type;
struct ap_filter_rec_t;
typedef struct ap_filter_rec_t ap_filter_rec_t;
struct ap_filter_rec_t {
   char const   *name ;
   ap_filter_func filter_func ;
   int (*filter_init_func)(ap_filter_t *f ) ;
   ap_filter_type ftype ;
   struct ap_filter_rec_t *next ;
};
struct ap_filter_t {
   ap_filter_rec_t *frec ;
   void *ctx ;
   ap_filter_t *next ;
   request_rec *r ;
   conn_rec *c ;
};
typedef int ap_HOOK_access_checker_t(request_rec *r );
enum allowdeny_type {
    T_ENV = 0,
    T_ALL = 1,
    T_IP = 2,
    T_HOST = 3,
    T_FAIL = 4
};
union __anonunion_x_81 {
   char *from ;
   apr_ipsubnet_t *ip ;
};
struct __anonstruct_allowdeny_80 {
   apr_int64_t limited ;
   union __anonunion_x_81 x ;
   enum allowdeny_type type ;
};
typedef struct __anonstruct_allowdeny_80 allowdeny;
struct __anonstruct_access_dir_conf_82 {
   int order[64] ;
   apr_array_header_t *allows ;
   apr_array_header_t *denys ;
};
typedef struct __anonstruct_access_dir_conf_82 access_dir_conf;
struct apr_allocator_t;
typedef struct apr_allocator_t apr_allocator_t;
struct require_line;
typedef struct require_line require_line;
struct require_line {
   apr_int64_t method_mask ;
   char *requirement ;
};
typedef int ap_HOOK_check_user_id_t(request_rec *r );
typedef int ap_HOOK_auth_checker_t(request_rec *r );
struct __anonstruct_auth_config_rec_90 {
   char *auth_pwfile ;
   char *auth_grpfile ;
   int auth_authoritative ;
};
typedef struct __anonstruct_auth_config_rec_90 auth_config_rec;
typedef int __ssize_t;
typedef __ssize_t ssize_t;
typedef unsigned int apr_uint32_t;
typedef ssize_t apr_ssize_t;
struct apr_hash_t;
typedef struct apr_hash_t apr_hash_t;
typedef void apr_opt_fn_t(void);
struct apr_table_entry_t;
typedef struct apr_table_entry_t apr_table_entry_t;
struct apr_table_entry_t {
   char *key ;
   char *val ;
   apr_uint32_t key_checksum ;
};
struct __anonstruct_regex_t_71 {
   void *re_pcre ;
   size_t re_nsub ;
   size_t re_erroffset ;
};
typedef struct __anonstruct_regex_t_71 regex_t;
typedef int regoff_t;
struct __anonstruct_regmatch_t_72 {
   regoff_t rm_so ;
   regoff_t rm_eo ;
};
typedef struct __anonstruct_regmatch_t_72 regmatch_t;
typedef int ap_HOOK_post_config_t(apr_pool_t *pconf , apr_pool_t *plog , apr_pool_t *ptemp ,
                                  server_rec *s );
typedef int ap_HOOK_fixups_t(request_rec *r );
enum __anonenum_states_95 {
    PRE_HEAD = 0,
    PARSE_HEAD = 1,
    PARSE_DIRECTIVE = 2,
    PARSE_TAG = 3,
    PARSE_TAIL = 4,
    PARSED = 5
};
typedef enum __anonenum_states_95 states;
struct bndm_t;
typedef struct bndm_t bndm_t;
struct include_filter_ctx {
   states state ;
   long flags ;
   int if_nesting_level ;
   apr_size_t parse_pos ;
   int bytes_parsed ;
   apr_status_t status ;
   int output_now ;
   int output_flush ;
   apr_bucket *head_start_bucket ;
   apr_size_t head_start_index ;
   apr_bucket *tag_start_bucket ;
   apr_size_t tag_start_index ;
   apr_bucket *tail_start_bucket ;
   apr_size_t tail_start_index ;
   char *combined_tag ;
   char *curr_tag_pos ;
   apr_size_t directive_length ;
   apr_size_t tag_length ;
   char *error_str ;
   char *error_str_override ;
   char *time_str ;
   char *time_str_override ;
   apr_pool_t *pool ;
   apr_bucket_brigade *ssi_tag_brigade ;
   bndm_t *start_seq_pat ;
   char *start_seq ;
   int start_seq_len ;
   char *end_seq ;
   char *re_string ;
   regmatch_t (*re_result)[10] ;
};
typedef struct include_filter_ctx include_ctx_t;
typedef int include_handler_fn_t(include_ctx_t *ctx , apr_bucket_brigade **bb , request_rec *r ,
                                 ap_filter_t *f , apr_bucket *head_ptr , apr_bucket **inserted_head );
typedef void apr_OFN_ap_ssi_get_tag_and_value_t(include_ctx_t *ctx , char **tag ,
                                                char **tag_val , int dodecode );
typedef char *apr_OFN_ap_ssi_parse_string_t(request_rec *r , include_ctx_t *ctx ,
                                            char const   *in , char *out , apr_size_t length ,
                                            int leave_name );
typedef void apr_OFN_ap_register_include_handler_t(char *tag , include_handler_fn_t *func );
enum xbithack {
    xbithack_off = 0,
    xbithack_on = 1,
    xbithack_full = 2
};
struct bndm_t {
   unsigned int T[256] ;
   unsigned int x ;
};
struct __anonstruct_include_dir_config_96 {
   char *default_error_msg ;
   char *default_time_fmt ;
   enum xbithack *xbithack ;
};
typedef struct __anonstruct_include_dir_config_96 include_dir_config;
struct __anonstruct_include_server_config_97 {
   char *default_start_tag ;
   char *default_end_tag ;
   int start_tag_len ;
   bndm_t start_seq_pat ;
   char *undefinedEcho ;
   int undefinedEchoLen ;
};
typedef struct __anonstruct_include_server_config_97 include_server_config;
enum __anonenum_encode_98 {
    E_NONE = 0,
    E_URL = 1,
    E_ENTITY = 2
};
enum token_type {
    token_string = 0,
    token_re = 1,
    token_and = 2,
    token_or = 3,
    token_not = 4,
    token_eq = 5,
    token_ne = 6,
    token_rbrace = 7,
    token_lbrace = 8,
    token_group = 9,
    token_ge = 10,
    token_le = 11,
    token_gt = 12,
    token_lt = 13
};
struct token {
   enum token_type type ;
   char *value ;
};
struct parse_node {
   struct parse_node *left ;
   struct parse_node *right ;
   struct parse_node *parent ;
   struct token token ;
   int value ;
   int done ;
};
typedef int __pid_t;
typedef __pid_t pid_t;
struct apr_time_exp_t;
typedef struct apr_time_exp_t apr_time_exp_t;
struct apr_time_exp_t {
   apr_int32_t tm_usec ;
   apr_int32_t tm_sec ;
   apr_int32_t tm_min ;
   apr_int32_t tm_hour ;
   apr_int32_t tm_mday ;
   apr_int32_t tm_mon ;
   apr_int32_t tm_year ;
   apr_int32_t tm_wday ;
   apr_int32_t tm_yday ;
   apr_int32_t tm_isdst ;
   apr_int32_t tm_gmtoff ;
};
struct apr_proc_t;
typedef struct apr_proc_t apr_proc_t;
struct apr_proc_t {
   pid_t pid ;
   apr_file_t *in ;
   apr_file_t *out ;
   apr_file_t *err ;
};
typedef char const   *ap_log_handler_fn_t(request_rec *r , char *a );
typedef void *ap_log_writer_init(apr_pool_t *p , server_rec *s , char const   *name );
typedef apr_status_t ap_log_writer(request_rec *r , void *handle , char const   **portions ,
                                   int *lengths , int nelts , apr_size_t len );
struct ap_log_handler {
   ap_log_handler_fn_t *func ;
   int want_orig_default ;
};
typedef struct ap_log_handler ap_log_handler;
typedef void apr_OFN_ap_register_log_handler_t(apr_pool_t *p , char *tag , ap_log_handler_fn_t *func ,
                                               int def );
typedef void apr_OFN_ap_log_set_writer_init_t(ap_log_writer_init *func );
typedef void apr_OFN_ap_log_set_writer_t(ap_log_writer *func );
typedef int ap_HOOK_pre_config_t(apr_pool_t *pconf , apr_pool_t *plog , apr_pool_t *ptemp );
typedef int ap_HOOK_open_logs_t(apr_pool_t *pconf , apr_pool_t *plog , apr_pool_t *ptemp ,
                                server_rec *s );
typedef void ap_HOOK_child_init_t(apr_pool_t *pchild , server_rec *s );
struct piped_log;
typedef struct piped_log piped_log;
struct piped_log {
   apr_pool_t *p ;
   apr_file_t *fds[2] ;
   char *program ;
   apr_proc_t *pid ;
};
typedef int ap_HOOK_log_transaction_t(request_rec *r );
struct __anonstruct_multi_log_state_97 {
   char const   *default_format_string ;
   apr_array_header_t *default_format ;
   apr_array_header_t *config_logs ;
   apr_array_header_t *server_config_logs ;
   apr_table_t *formats ;
};
typedef struct __anonstruct_multi_log_state_97 multi_log_state;
struct __anonstruct_buffered_log_98 {
   apr_file_t *handle ;
   apr_size_t outcnt ;
   char outbuf[4096] ;
};
typedef struct __anonstruct_buffered_log_98 buffered_log;
struct __anonstruct_config_log_state_99 {
   char const   *fname ;
   char const   *format_string ;
   apr_array_header_t *format ;
   void *log_writer ;
   char *condition_var ;
};
typedef struct __anonstruct_config_log_state_99 config_log_state;
struct __anonstruct_log_format_item_100 {
   ap_log_handler_fn_t *func ;
   char *arg ;
   int condition_sense ;
   int want_orig ;
   apr_array_header_t *conditions ;
};
typedef struct __anonstruct_log_format_item_100 log_format_item;
struct __anonstruct_cached_request_time_101 {
   unsigned int t ;
   char timestr[32] ;
   unsigned int t_validate ;
};
typedef struct __anonstruct_cached_request_time_101 cached_request_time;
struct __anonstruct_env_dir_config_rec_76 {
   apr_table_t *vars ;
   apr_table_t *unsetenv ;
};
typedef struct __anonstruct_env_dir_config_rec_76 env_dir_config_rec;
struct apr_strmatch_pattern;
typedef struct apr_strmatch_pattern apr_strmatch_pattern;
struct apr_strmatch_pattern {
   char const   *(*compare)(apr_strmatch_pattern const   *this_pattern , char const   *s ,
                            apr_size_t slen ) ;
   char const   *pattern ;
   apr_size_t length ;
   void *context ;
};
typedef int ap_HOOK_header_parser_t(request_rec *r );
typedef int ap_HOOK_post_read_request_t(request_rec *r );
enum special {
    SPECIAL_NOT = 0,
    SPECIAL_REMOTE_ADDR = 1,
    SPECIAL_REMOTE_HOST = 2,
    SPECIAL_REMOTE_USER = 3,
    SPECIAL_REQUEST_URI = 4,
    SPECIAL_REQUEST_METHOD = 5,
    SPECIAL_REQUEST_PROTOCOL = 6,
    SPECIAL_SERVER_ADDR = 7
};
struct __anonstruct_sei_entry_90 {
   char *name ;
   regex_t *pnamereg ;
   char *regex ;
   regex_t *preg ;
   apr_strmatch_pattern const   *pattern ;
   apr_table_t *features ;
   enum special special_type ;
   int icase ;
};
typedef struct __anonstruct_sei_entry_90 sei_entry;
struct __anonstruct_sei_cfg_rec_91 {
   apr_array_header_t *conditionals ;
};
typedef struct __anonstruct_sei_cfg_rec_91 sei_cfg_rec;
struct apr_socket_t;
typedef struct apr_socket_t apr_socket_t;
typedef int ap_HOOK_process_connection_t(conn_rec *c );
typedef char const   *ap_HOOK_http_method_t(request_rec const   *r );
typedef apr_port_t ap_HOOK_default_port_t(request_rec const   *r );
typedef int ap_HOOK_create_request_t(request_rec *r );
typedef int ap_HOOK_map_to_storage_t(request_rec *r );
struct ap_sb_handle_t;
typedef struct ap_sb_handle_t ap_sb_handle_t;
typedef unsigned long __rlim_t;
struct iovec {
   void *iov_base ;
   size_t iov_len ;
};
typedef __rlim_t rlim_t;
struct rlimit {
   rlim_t rlim_cur ;
   rlim_t rlim_max ;
};
typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
struct apr_bucket_refcount;
typedef struct apr_bucket_refcount apr_bucket_refcount;
struct apr_bucket_refcount {
   int refcount ;
};
typedef int apr_table_do_callback_fn_t(void *rec , char const   *key , char const   *value );
struct apr_hash_index_t;
typedef struct apr_hash_index_t apr_hash_index_t;
typedef unsigned char allow_options_t;
typedef unsigned char overrides_t;
typedef unsigned long etag_components_t;
enum __anonenum_server_signature_e_74 {
    srv_sig_unset = 0,
    srv_sig_off = 1,
    srv_sig_on = 2,
    srv_sig_withmail = 3
};
typedef enum __anonenum_server_signature_e_74 server_signature_e;
struct __anonstruct_core_dir_config_75 {
   char *d ;
   unsigned int d_components ;
   allow_options_t opts ;
   allow_options_t opts_add ;
   allow_options_t opts_remove ;
   overrides_t override ;
   char *ap_default_type ;
   int satisfy ;
   char *ap_auth_type ;
   char *ap_auth_name ;
   apr_array_header_t *ap_requires ;
   char **response_code_strings ;
   unsigned int hostname_lookups : 4 ;
   int do_rfc1413 : 2 ;
   int content_md5 : 2 ;
   unsigned int use_canonical_name : 2 ;
   unsigned int d_is_fnmatch : 1 ;
   unsigned int add_default_charset : 2 ;
   char const   *add_default_charset_name ;
   struct rlimit *limit_cpu ;
   struct rlimit *limit_mem ;
   struct rlimit *limit_nproc ;
   apr_off_t limit_req_body ;
   long limit_xml_body ;
   server_signature_e server_signature ;
   int loglevel ;
   apr_array_header_t *sec_file ;
   regex_t *r ;
   char const   *mime_type ;
   char const   *handler ;
   char const   *output_filters ;
   char const   *input_filters ;
   int accept_path_info ;
   apr_hash_t *ct_output_filters ;
   etag_components_t etag_bits ;
   etag_components_t etag_add ;
   etag_components_t etag_remove ;
   unsigned int enable_mmap : 2 ;
   unsigned int enable_sendfile : 2 ;
};
typedef struct __anonstruct_core_dir_config_75 core_dir_config;
struct ap_bucket_error;
typedef struct ap_bucket_error ap_bucket_error;
struct ap_bucket_error {
   apr_bucket_refcount refcount ;
   int status ;
   char const   *data ;
};
enum __anonenum_state_98 {
    BODY_NONE = 0,
    BODY_LENGTH = 1,
    BODY_CHUNK = 2
};
struct http_filter_ctx {
   apr_off_t remaining ;
   apr_off_t limit ;
   apr_off_t limit_used ;
   enum __anonenum_state_98 state ;
   int eos_sent ;
};
typedef struct http_filter_ctx http_ctx_t;
struct header_struct {
   apr_pool_t *pool ;
   apr_bucket_brigade *bb ;
};
typedef struct header_struct header_struct;
struct header_filter_ctx {
   int headers_sent ;
};
typedef struct header_filter_ctx header_filter_ctx;
struct byterange_ctx {
   apr_bucket_brigade *bb ;
   int num_ranges ;
   char *boundary ;
   char *bound_head ;
};
typedef struct byterange_ctx byterange_ctx;
typedef struct ap_conf_vector_t ap_conf_vector_t;
typedef int ap_HOOK_type_checker_t(request_rec *r );
struct attrib_info {
   char *name ;
   int offset ;
};
typedef struct attrib_info attrib_info;
struct extension_info {
   char *forced_type ;
   char *encoding_type ;
   char *language_type ;
   char *handler ;
   char *charset_type ;
   char *input_filters ;
   char *output_filters ;
};
typedef struct extension_info extension_info;
struct __anonstruct_mime_dir_config_88 {
   apr_hash_t *extension_mappings ;
   apr_array_header_t *remove_mappings ;
   char *default_language ;
   int multimatch ;
   int use_path_info ;
};
typedef struct __anonstruct_mime_dir_config_88 mime_dir_config;
struct param_s {
   char *attr ;
   char *val ;
   struct param_s *next ;
};
typedef struct param_s param;
struct __anonstruct_content_type_89 {
   char const   *type ;
   apr_size_t type_len ;
   char const   *subtype ;
   apr_size_t subtype_len ;
   param *param ;
};
typedef struct __anonstruct_content_type_89 content_type;
typedef long __clock_t;
typedef __clock_t clock_t;
typedef unsigned long pthread_t;
typedef int ap_HOOK_handler_t(request_rec *r );
typedef pthread_t apr_os_thread_t;
struct tms {
   clock_t tms_utime ;
   clock_t tms_stime ;
   clock_t tms_cutime ;
   clock_t tms_cstime ;
};
typedef int ap_generation_t;
enum __anonenum_ap_scoreboard_e_94 {
    SB_NOT_SHARED = 1,
    SB_SHARED = 2
};
typedef enum __anonenum_ap_scoreboard_e_94 ap_scoreboard_e;
struct worker_score;
typedef struct worker_score worker_score;
struct worker_score {
   int thread_num ;
   apr_os_thread_t tid ;
   unsigned char status ;
   unsigned long access_count ;
   apr_off_t bytes_served ;
   unsigned long my_access_count ;
   apr_off_t my_bytes_served ;
   apr_off_t conn_bytes ;
   unsigned short conn_count ;
   apr_time_t start_time ;
   apr_time_t stop_time ;
   struct tms times ;
   apr_time_t last_used ;
   char client[32] ;
   char request[64] ;
   char vhost[32] ;
};
struct __anonstruct_global_score_95 {
   int server_limit ;
   int thread_limit ;
   ap_scoreboard_e sb_type ;
   ap_generation_t running_generation ;
   apr_time_t restart_time ;
};
typedef struct __anonstruct_global_score_95 global_score;
struct process_score;
typedef struct process_score process_score;
struct process_score {
   pid_t pid ;
   ap_generation_t generation ;
   ap_scoreboard_e sb_type ;
   int quiescing ;
};
struct __anonstruct_scoreboard_96 {
   global_score *global ;
   process_score *parent ;
   worker_score **servers ;
};
typedef struct __anonstruct_scoreboard_96 scoreboard;
struct stat_opt {
   int id ;
   char const   *form_data_str ;
   char const   *hdr_out_str ;
};
struct apr_dir_t;
typedef struct apr_dir_t apr_dir_t;
struct item {
   char *type ;
   char *apply_to ;
   char *apply_path ;
   char *data ;
};
struct ai_desc_t {
   char *pattern ;
   char *description ;
   int full_path ;
   int wildcards ;
};
typedef struct ai_desc_t ai_desc_t;
struct autoindex_config_struct {
   char *default_icon ;
   apr_int32_t opts ;
   apr_int32_t incremented_opts ;
   apr_int32_t decremented_opts ;
   int name_width ;
   int name_adjust ;
   int desc_width ;
   int desc_adjust ;
   int icon_width ;
   int icon_height ;
   char default_keyid ;
   char default_direction ;
   apr_array_header_t *icon_list ;
   apr_array_header_t *alt_list ;
   apr_array_header_t *desc_list ;
   apr_array_header_t *ign_list ;
   apr_array_header_t *hdr_list ;
   apr_array_header_t *rdme_list ;
};
typedef struct autoindex_config_struct autoindex_config_rec;
struct ent {
   char *name ;
   char *icon ;
   char *alt ;
   char *desc ;
   apr_off_t size ;
   apr_time_t lm ;
   struct ent *next ;
   int ascending ;
   int ignore_case ;
   int version_sort ;
   char key ;
   int isdir ;
};
typedef int apr_seek_where_t;
enum __anonenum_apr_cmdtype_e_52 {
    APR_SHELLCMD = 0,
    APR_PROGRAM = 1,
    APR_PROGRAM_ENV = 2,
    APR_PROGRAM_PATH = 3
};
typedef enum __anonenum_apr_cmdtype_e_52 apr_cmdtype_e;
struct apr_procattr_t;
typedef struct apr_procattr_t apr_procattr_t;
enum __anonenum_apr_kill_conditions_e_55 {
    APR_KILL_NEVER = 0,
    APR_KILL_ALWAYS = 1,
    APR_KILL_AFTER_TIMEOUT = 2,
    APR_JUST_WAIT = 3,
    APR_KILL_ONLY_ONCE = 4
};
typedef enum __anonenum_apr_kill_conditions_e_55 apr_kill_conditions_e;
enum __anonenum_prog_types_96 {
    RUN_AS_SSI = 0,
    RUN_AS_CGI = 1
};
typedef enum __anonenum_prog_types_96 prog_types;
struct __anonstruct_cgi_exec_info_t_97 {
   apr_int32_t in_pipe ;
   apr_int32_t out_pipe ;
   apr_int32_t err_pipe ;
   int process_cgi ;
   apr_cmdtype_e cmd_type ;
   apr_int32_t detached ;
   prog_types prog_type ;
   apr_bucket_brigade **bb ;
   include_ctx_t *ctx ;
   ap_filter_t *next ;
};
typedef struct __anonstruct_cgi_exec_info_t_97 cgi_exec_info_t;
typedef apr_status_t apr_OFN_ap_cgi_build_command_t(char const   **cmd , char const   ***argv ,
                                                    request_rec *r , apr_pool_t *p ,
                                                    cgi_exec_info_t *e_info );
struct __anonstruct_cgi_server_conf_98 {
   char const   *logname ;
   long logbytes ;
   apr_size_t bufbytes ;
};
typedef struct __anonstruct_cgi_server_conf_98 cgi_server_conf;
struct __anonstruct_neg_dir_config_91 {
   int forcelangpriority ;
   apr_array_header_t *language_priority ;
};
typedef struct __anonstruct_neg_dir_config_91 neg_dir_config;
struct accept_rec {
   char *name ;
   float quality ;
   float level ;
   char *charset ;
};
typedef struct accept_rec accept_rec;
struct var_rec {
   request_rec *sub_req ;
   char const   *mime_type ;
   char const   *file_name ;
   apr_off_t body ;
   char const   *content_encoding ;
   apr_array_header_t *content_languages ;
   char const   *content_charset ;
   char const   *description ;
   float lang_quality ;
   float encoding_quality ;
   float charset_quality ;
   float mime_type_quality ;
   float source_quality ;
   float level ;
   apr_off_t bytes ;
   int lang_index ;
   int is_pseudo_html ;
   float level_matched ;
   int mime_stars ;
   int definite ;
};
typedef struct var_rec var_rec;
struct __anonstruct_negotiation_state_92 {
   apr_pool_t *pool ;
   request_rec *r ;
   neg_dir_config *conf ;
   char *dir_name ;
   int accept_q ;
   float default_lang_quality ;
   apr_array_header_t *accepts ;
   apr_array_header_t *accept_encodings ;
   apr_array_header_t *accept_charsets ;
   apr_array_header_t *accept_langs ;
   apr_array_header_t *avail_vars ;
   int count_multiviews_variants ;
   int is_transparent ;
   int dont_fiddle_headers ;
   int ua_supports_trans ;
   int send_alternates ;
   int may_choose ;
   int use_rvsa ;
};
typedef struct __anonstruct_negotiation_state_92 negotiation_state;
enum header_state {
    header_eof = 0,
    header_seen = 1,
    header_sep = 2
};
enum algorithm_results {
    alg_choice = 1,
    alg_list = 2
};
struct dir_config_struct {
   apr_array_header_t *index_names ;
};
typedef struct dir_config_struct dir_config_rec;
struct __anonstruct_imap_conf_rec_91 {
   char *imap_menu ;
   char *imap_default ;
   char *imap_base ;
};
typedef struct __anonstruct_imap_conf_rec_91 imap_conf_rec;
struct __anonstruct_action_dir_config_90 {
   apr_table_t *action_types ;
   char const   *scripted[64] ;
   int configured ;
};
typedef struct __anonstruct_action_dir_config_90 action_dir_config;
typedef int ap_HOOK_translate_name_t(request_rec *r );
struct __anonstruct_ap_unix_identity_t_80 {
   uid_t uid ;
   gid_t gid ;
   int userdir ;
};
typedef struct __anonstruct_ap_unix_identity_t_80 ap_unix_identity_t;
typedef ap_unix_identity_t *ap_HOOK_get_suexec_identity_t(request_rec const   *r );
struct __anonstruct_userdir_config_82 {
   int globally_disabled ;
   char *userdir ;
   apr_table_t *enabled_users ;
   apr_table_t *disabled_users ;
};
typedef struct __anonstruct_userdir_config_82 userdir_config;
struct __anonstruct_alias_entry_80 {
   char const   *real ;
   char const   *fake ;
   char *handler ;
   regex_t *regexp ;
   int redir_status ;
};
typedef struct __anonstruct_alias_entry_80 alias_entry;
struct __anonstruct_alias_server_conf_81 {
   apr_array_header_t *aliases ;
   apr_array_header_t *redirects ;
};
typedef struct __anonstruct_alias_server_conf_81 alias_server_conf;
struct __anonstruct_alias_dir_conf_82 {
   apr_array_header_t *redirects ;
};
typedef struct __anonstruct_alias_dir_conf_82 alias_dir_conf;
struct apr_dso_handle_t;
typedef struct apr_dso_handle_t apr_dso_handle_t;
typedef void *apr_dso_handle_sym_t;
struct moduleinfo {
   char const   *name ;
   module *modp ;
};
typedef struct moduleinfo moduleinfo;
struct so_server_conf {
   apr_array_header_t *loaded_modules ;
};
typedef struct so_server_conf so_server_conf;
struct __anonstruct___sigset_t_2 {
   unsigned long __val[(int )(1024U / (8U * sizeof(unsigned long )))] ;
};
typedef struct __anonstruct___sigset_t_2 __sigset_t;
typedef __sigset_t sigset_t;
typedef short apr_int16_t;
union sigval {
   int sival_int ;
   void *sival_ptr ;
};
typedef union sigval sigval_t;
struct __anonstruct__kill_17 {
   __pid_t si_pid ;
   __uid_t si_uid ;
};
struct __anonstruct__timer_18 {
   int si_tid ;
   int si_overrun ;
   sigval_t si_sigval ;
};
struct __anonstruct__rt_19 {
   __pid_t si_pid ;
   __uid_t si_uid ;
   sigval_t si_sigval ;
};
struct __anonstruct__sigchld_20 {
   __pid_t si_pid ;
   __uid_t si_uid ;
   int si_status ;
   __clock_t si_utime ;
   __clock_t si_stime ;
};
struct __anonstruct__sigfault_21 {
   void *si_addr ;
};
struct __anonstruct__sigpoll_22 {
   long si_band ;
   int si_fd ;
};
union __anonunion__sifields_16 {
   int _pad[(int )(128U / sizeof(int ) - 3U)] ;
   struct __anonstruct__kill_17 _kill ;
   struct __anonstruct__timer_18 _timer ;
   struct __anonstruct__rt_19 _rt ;
   struct __anonstruct__sigchld_20 _sigchld ;
   struct __anonstruct__sigfault_21 _sigfault ;
   struct __anonstruct__sigpoll_22 _sigpoll ;
};
struct siginfo {
   int si_signo ;
   int si_errno ;
   int si_code ;
   union __anonunion__sifields_16 _sifields ;
};
typedef struct siginfo siginfo_t;
union __anonunion___sigaction_handler_34 {
   void (*sa_handler)(int  ) ;
   void (*sa_sigaction)(int  , siginfo_t * , void * ) ;
};
struct sigaction {
   union __anonunion___sigaction_handler_34 __sigaction_handler ;
   __sigset_t sa_mask ;
   int sa_flags ;
   void (*sa_restorer)(void) ;
};
enum __anonenum_apr_exit_why_e_54 {
    APR_PROC_EXIT = 1,
    APR_PROC_SIGNAL = 2,
    APR_PROC_SIGNAL_CORE = 4
};
typedef enum __anonenum_apr_exit_why_e_54 apr_exit_why_e;
typedef int apr_lockmech_e;
struct apr_proc_mutex_t;
typedef struct apr_proc_mutex_t apr_proc_mutex_t;
struct apr_shm_t;
typedef struct apr_shm_t apr_shm_t;
typedef void apr_sigfunc_t(int  );
struct ap_listen_rec;
typedef struct ap_listen_rec ap_listen_rec;
struct ap_listen_rec {
   ap_listen_rec *next ;
   apr_socket_t *sd ;
   apr_sockaddr_t *bind_addr ;
   apr_status_t (*accept_func)(void **csd , ap_listen_rec *lr , apr_pool_t *ptrans ) ;
   int active ;
};
struct ap_pod_t;
typedef struct ap_pod_t ap_pod_t;
struct ap_pod_t {
   apr_file_t *pod_in ;
   apr_file_t *pod_out ;
   apr_pool_t *p ;
   apr_sockaddr_t *sa ;
};
enum __anonenum_apr_datatype_e_103 {
    APR_NO_DESC = 0,
    APR_POLL_SOCKET = 1,
    APR_POLL_FILE = 2,
    APR_POLL_LASTDESC = 3
};
typedef enum __anonenum_apr_datatype_e_103 apr_datatype_e;
union __anonunion_apr_descriptor_104 {
   apr_file_t *f ;
   apr_socket_t *s ;
};
typedef union __anonunion_apr_descriptor_104 apr_descriptor;
struct apr_pollfd_t;
typedef struct apr_pollfd_t apr_pollfd_t;
struct apr_pollfd_t {
   apr_pool_t *p ;
   apr_datatype_e desc_type ;
   apr_int16_t reqevents ;
   apr_int16_t rtnevents ;
   apr_descriptor desc ;
   void *client_data ;
};
typedef long long __off64_t;
struct _IO_FILE;
typedef struct _IO_FILE FILE;
typedef void _IO_lock_t;
struct _IO_marker {
   struct _IO_marker *_next ;
   struct _IO_FILE *_sbuf ;
   int _pos ;
};
struct _IO_FILE {
   int _flags ;
   char *_IO_read_ptr ;
   char *_IO_read_end ;
   char *_IO_read_base ;
   char *_IO_write_base ;
   char *_IO_write_ptr ;
   char *_IO_write_end ;
   char *_IO_buf_base ;
   char *_IO_buf_end ;
   char *_IO_save_base ;
   char *_IO_backup_base ;
   char *_IO_save_end ;
   struct _IO_marker *_markers ;
   struct _IO_FILE *_chain ;
   int _fileno ;
   int _flags2 ;
   __off_t _old_offset ;
   unsigned short _cur_column ;
   signed char _vtable_offset ;
   char _shortbuf[1] ;
   _IO_lock_t *_lock ;
   __off64_t _offset ;
   void *__pad1 ;
   void *__pad2 ;
   int _mode ;
   char _unused2[(int )(15U * sizeof(int ) - 2U * sizeof(void *))] ;
};
struct ap_LINK_header_parser_t {
   ap_HOOK_header_parser_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_header_parser_t ap_LINK_header_parser_t;
struct ap_LINK_pre_config_t {
   ap_HOOK_pre_config_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_pre_config_t ap_LINK_pre_config_t;
struct ap_LINK_post_config_t {
   ap_HOOK_post_config_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_post_config_t ap_LINK_post_config_t;
struct ap_LINK_open_logs_t {
   ap_HOOK_open_logs_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_open_logs_t ap_LINK_open_logs_t;
struct ap_LINK_child_init_t {
   ap_HOOK_child_init_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_child_init_t ap_LINK_child_init_t;
struct ap_LINK_handler_t {
   ap_HOOK_handler_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_handler_t ap_LINK_handler_t;
typedef int ap_HOOK_quick_handler_t(request_rec *r , int lookup_uri );
struct ap_LINK_quick_handler_t {
   ap_HOOK_quick_handler_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_quick_handler_t ap_LINK_quick_handler_t;
typedef void ap_HOOK_optional_fn_retrieve_t(void);
struct ap_LINK_optional_fn_retrieve_t {
   ap_HOOK_optional_fn_retrieve_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_optional_fn_retrieve_t ap_LINK_optional_fn_retrieve_t;
struct __anonstruct__hooks_99 {
   apr_array_header_t *link_header_parser ;
   apr_array_header_t *link_pre_config ;
   apr_array_header_t *link_post_config ;
   apr_array_header_t *link_open_logs ;
   apr_array_header_t *link_child_init ;
   apr_array_header_t *link_handler ;
   apr_array_header_t *link_quick_handler ;
   apr_array_header_t *link_optional_fn_retrieve ;
};
struct __anonstruct_arr_elts_param_t_100 {
   apr_array_header_t *array ;
   int curr_idx ;
};
typedef struct __anonstruct_arr_elts_param_t_100 arr_elts_param_t;
struct __anonstruct_fnames_101 {
   char *fname ;
};
typedef struct __anonstruct_fnames_101 fnames;
typedef void ap_HOOK_error_log_t(char const   *file , int line , int level , apr_status_t status ,
                                 server_rec const   *s , request_rec const   *r ,
                                 apr_pool_t *pool , char const   *errstr );
struct ap_LINK_error_log_t {
   ap_HOOK_error_log_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_error_log_t ap_LINK_error_log_t;
struct __anonstruct_TRANS_87 {
   char *t_name ;
   int t_val ;
};
typedef struct __anonstruct_TRANS_87 TRANS;
struct __anonstruct__hooks_88 {
   apr_array_header_t *link_error_log ;
};
typedef void apr_getopt_err_fn_t(void *arg , char const   *err  , ...);
struct apr_getopt_t;
typedef struct apr_getopt_t apr_getopt_t;
struct apr_getopt_t {
   apr_pool_t *cont ;
   apr_getopt_err_fn_t *errfn ;
   void *errarg ;
   int ind ;
   int opt ;
   int reset ;
   int argc ;
   char const   **argv ;
   char const   *place ;
   int interleave ;
   int skip_start ;
   int skip_end ;
};
typedef int apr_OFN_ap_signal_server_t(int * , apr_pool_t * );
struct name_chain;
typedef struct name_chain name_chain;
struct name_chain {
   name_chain *next ;
   server_addr_rec *sar ;
   server_rec *server ;
};
struct ipaddr_chain;
typedef struct ipaddr_chain ipaddr_chain;
struct ipaddr_chain {
   ipaddr_chain *next ;
   server_addr_rec *sar ;
   server_rec *server ;
   name_chain *names ;
};
struct hostent {
   char *h_name ;
   char **h_aliases ;
   int h_addrtype ;
   int h_length ;
   char **h_addr_list ;
};
struct vastrs {
   va_list args ;
   int arg ;
   char const   *curpos ;
};
struct apr_xlate_t;
typedef struct apr_xlate_t apr_xlate_t;
struct apr_md5_ctx_t;
typedef struct apr_md5_ctx_t apr_md5_ctx_t;
struct apr_md5_ctx_t {
   apr_uint32_t state[4] ;
   apr_uint32_t count[2] ;
   unsigned char buffer[64] ;
   apr_xlate_t *xlate ;
};
struct exploded_time_cache_element {
   apr_int64_t t ;
   apr_time_exp_t xt ;
   apr_int64_t t_validate ;
};
enum __anonenum_apr_shutdown_how_e_55 {
    APR_SHUTDOWN_READ = 0,
    APR_SHUTDOWN_WRITE = 1,
    APR_SHUTDOWN_READWRITE = 2
};
typedef enum __anonenum_apr_shutdown_how_e_55 apr_shutdown_how_e;
typedef conn_rec *ap_HOOK_create_connection_t(apr_pool_t *p , server_rec *server ,
                                              apr_socket_t *csd , long conn_id , void *sbh ,
                                              apr_bucket_alloc_t *alloc );
struct ap_LINK_create_connection_t {
   ap_HOOK_create_connection_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_create_connection_t ap_LINK_create_connection_t;
typedef int ap_HOOK_pre_connection_t(conn_rec *c , void *csd );
struct ap_LINK_pre_connection_t {
   ap_HOOK_pre_connection_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_pre_connection_t ap_LINK_pre_connection_t;
struct ap_LINK_process_connection_t {
   ap_HOOK_process_connection_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_process_connection_t ap_LINK_process_connection_t;
struct __anonstruct__hooks_97 {
   apr_array_header_t *link_create_connection ;
   apr_array_header_t *link_process_connection ;
   apr_array_header_t *link_pre_connection ;
};
enum __anonenum_apr_wait_how_e_53 {
    APR_WAIT = 0,
    APR_NOWAIT = 1
};
typedef enum __anonenum_apr_wait_how_e_53 apr_wait_how_e;
struct passwd {
   char *pw_name ;
   char *pw_passwd ;
   __uid_t pw_uid ;
   __gid_t pw_gid ;
   char *pw_gecos ;
   char *pw_dir ;
   char *pw_shell ;
};
struct group {
   char *gr_name ;
   char *gr_passwd ;
   __gid_t gr_gid ;
   char **gr_mem ;
};
struct apr_text;
typedef struct apr_text apr_text;
struct apr_text {
   char const   *text ;
   struct apr_text *next ;
};
struct apr_text_header;
typedef struct apr_text_header apr_text_header;
struct apr_text_header {
   apr_text *first ;
   apr_text *last ;
};
struct apr_xml_attr;
struct apr_xml_elem;
typedef struct apr_xml_elem apr_xml_elem;
struct apr_xml_doc;
typedef struct apr_xml_doc apr_xml_doc;
struct apr_xml_attr {
   char const   *name ;
   int ns ;
   char const   *value ;
   struct apr_xml_attr *next ;
};
struct apr_xml_ns_scope;
struct apr_xml_elem {
   char const   *name ;
   int ns ;
   char const   *lang ;
   apr_text_header first_cdata ;
   apr_text_header following_cdata ;
   struct apr_xml_elem *parent ;
   struct apr_xml_elem *next ;
   struct apr_xml_elem *first_child ;
   struct apr_xml_attr *attr ;
   struct apr_xml_elem *last_child ;
   struct apr_xml_ns_scope *ns_scope ;
   void *priv ;
};
struct apr_xml_doc {
   apr_xml_elem *root ;
   apr_array_header_t *namespaces ;
};
struct apr_xml_parser;
typedef struct apr_xml_parser apr_xml_parser;
struct filter_trie_node;
typedef struct filter_trie_node filter_trie_node;
struct __anonstruct_filter_trie_child_ptr_76 {
   int c ;
   filter_trie_node *child ;
};
typedef struct __anonstruct_filter_trie_child_ptr_76 filter_trie_child_ptr;
struct filter_trie_node {
   ap_filter_rec_t *frec ;
   filter_trie_child_ptr *children ;
   int nchildren ;
   int size ;
};
typedef long __time_t;
typedef long __suseconds_t;
typedef __time_t time_t;
struct timeval {
   __time_t tv_sec ;
   __suseconds_t tv_usec ;
};
struct _pthread_fastlock {
   long __status ;
   int __spinlock ;
};
struct _pthread_descr_struct;
typedef struct _pthread_descr_struct *_pthread_descr;
typedef unsigned int pthread_key_t;
struct __anonstruct_pthread_mutex_t_6 {
   int __m_reserved ;
   int __m_count ;
   _pthread_descr __m_owner ;
   int __m_kind ;
   struct _pthread_fastlock __m_lock ;
};
typedef struct __anonstruct_pthread_mutex_t_6 pthread_mutex_t;
typedef int (*apr_abortfunc_t)(int retcode );
struct apr_memnode_t;
typedef struct apr_memnode_t apr_memnode_t;
struct apr_memnode_t {
   apr_memnode_t *next ;
   apr_memnode_t **ref ;
   apr_uint32_t index ;
   apr_uint32_t free_index ;
   char *first_avail ;
   char *endp ;
};
struct apr_thread_mutex_t;
typedef struct apr_thread_mutex_t apr_thread_mutex_t;
typedef apr_uint32_t apr_fileattrs_t;
enum __anonenum_apr_interface_e_56 {
    APR_LOCAL = 0,
    APR_REMOTE = 1
};
typedef enum __anonenum_apr_interface_e_56 apr_interface_e;
struct apr_hdtr_t;
typedef struct apr_hdtr_t apr_hdtr_t;
struct apr_hdtr_t {
   struct iovec *headers ;
   int numheaders ;
   struct iovec *trailers ;
   int numtrailers ;
};
struct apr_mmap_t;
typedef struct apr_mmap_t apr_mmap_t;
struct __anonstruct_link_58 {
   struct apr_mmap_t *next ;
   struct apr_mmap_t *prev ;
};
struct apr_mmap_t {
   apr_pool_t *cntxt ;
   void *mm ;
   apr_size_t size ;
   int unused ;
   struct __anonstruct_link_58 link ;
};
struct apr_thread_t;
typedef struct apr_thread_t apr_thread_t;
struct apr_threadattr_t;
typedef struct apr_threadattr_t apr_threadattr_t;
struct apr_thread_once_t;
typedef struct apr_thread_once_t apr_thread_once_t;
struct apr_threadkey_t;
typedef struct apr_threadkey_t apr_threadkey_t;
typedef int ap_HOOK_get_mgmt_items_t(apr_pool_t *p , char const   *val , apr_hash_t *ht );
struct apr_global_mutex_t;
typedef struct apr_global_mutex_t apr_global_mutex_t;
struct __dirstream;
typedef struct __dirstream DIR;
struct tm {
   int tm_sec ;
   int tm_min ;
   int tm_hour ;
   int tm_mday ;
   int tm_mon ;
   int tm_year ;
   int tm_wday ;
   int tm_yday ;
   int tm_isdst ;
   long tm_gmtoff ;
   char const   *tm_zone ;
};
struct apr_os_proc_mutex_t {
   int crossproc ;
   pthread_mutex_t *pthread_interproc ;
   pthread_mutex_t *intraproc ;
};
typedef int apr_os_file_t;
typedef DIR apr_os_dir_t;
typedef int apr_os_sock_t;
typedef struct apr_os_proc_mutex_t apr_os_proc_mutex_t;
typedef pthread_key_t apr_os_threadkey_t;
typedef struct timeval apr_os_imp_time_t;
typedef struct tm apr_os_exp_time_t;
typedef void *apr_os_dso_handle_t;
typedef void *apr_os_shm_t;
struct apr_os_sock_info_t {
   apr_os_sock_t *os_sock ;
   struct sockaddr *local ;
   struct sockaddr *remote ;
   int family ;
   int type ;
};
typedef struct apr_os_sock_info_t apr_os_sock_info_t;
struct apr_os_global_mutex_t {
   apr_pool_t *pool ;
   apr_proc_mutex_t *proc_mutex ;
   apr_thread_mutex_t *thread_mutex ;
};
typedef struct apr_os_global_mutex_t apr_os_global_mutex_t;
typedef void ap_HOOK_insert_filter_t(request_rec *r );
typedef int ap_HOOK_pre_mpm_t(apr_pool_t *p , ap_scoreboard_e sb_type );
struct apr_getopt_option_t;
typedef struct apr_getopt_option_t apr_getopt_option_t;
struct apr_getopt_option_t {
   char const   *name ;
   int optch ;
   int has_arg ;
   char const   *description ;
};
struct apr_vformatter_buff_t;
typedef struct apr_vformatter_buff_t apr_vformatter_buff_t;
struct apr_vformatter_buff_t {
   char *curpos ;
   char *endpos ;
};
struct apr_pollset_t;
typedef struct apr_pollset_t apr_pollset_t;
struct apr_thread_cond_t;
typedef struct apr_thread_cond_t apr_thread_cond_t;
struct apr_thread_rwlock_t;
typedef struct apr_thread_rwlock_t apr_thread_rwlock_t;
struct __anonstruct_apr_version_t_103 {
   int major ;
   int minor ;
   int patch ;
   int is_dev ;
};
typedef struct __anonstruct_apr_version_t_103 apr_version_t;
enum tm_lock {
    apr_anylock_none = 0,
    apr_anylock_procmutex = 1,
    apr_anylock_threadmutex = 2,
    apr_anylock_readlock = 3,
    apr_anylock_writelock = 4
};
union apr_anylock_u_t {
   apr_proc_mutex_t *pm ;
   apr_thread_mutex_t *tm ;
   apr_thread_rwlock_t *rw ;
};
struct apr_anylock_t {
   enum tm_lock type ;
   union apr_anylock_u_t lock ;
};
typedef struct apr_anylock_t apr_anylock_t;
struct apr_dbm_t;
typedef struct apr_dbm_t apr_dbm_t;
struct __anonstruct_apr_datum_t_104 {
   char *dptr ;
   apr_size_t dsize ;
};
typedef struct __anonstruct_apr_datum_t_104 apr_datum_t;
struct apr_md4_ctx_t;
typedef struct apr_md4_ctx_t apr_md4_ctx_t;
struct apr_md4_ctx_t {
   apr_uint32_t state[4] ;
   apr_uint32_t count[2] ;
   unsigned char buffer[64] ;
   apr_xlate_t *xlate ;
};
struct apr_queue_t;
typedef struct apr_queue_t apr_queue_t;
struct apr_reslist_t;
typedef struct apr_reslist_t apr_reslist_t;
struct apr_rmm_t;
typedef struct apr_rmm_t apr_rmm_t;
typedef apr_size_t apr_rmm_off_t;
struct apr_sdbm_t;
typedef struct apr_sdbm_t apr_sdbm_t;
struct __anonstruct_apr_sdbm_datum_t_105 {
   char *dptr ;
   int dsize ;
};
typedef struct __anonstruct_apr_sdbm_datum_t_105 apr_sdbm_datum_t;
struct apr_sha1_ctx_t;
typedef struct apr_sha1_ctx_t apr_sha1_ctx_t;
struct apr_sha1_ctx_t {
   apr_uint32_t digest[5] ;
   apr_uint32_t count_lo ;
   apr_uint32_t count_hi ;
   apr_uint32_t data[16] ;
   int local ;
};
struct __anonstruct_apr_uuid_t_106 {
   unsigned char data[16] ;
};
typedef struct __anonstruct_apr_uuid_t_106 apr_uuid_t;
struct ap_LINK_pre_mpm_t {
   ap_HOOK_pre_mpm_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_pre_mpm_t ap_LINK_pre_mpm_t;
struct __anonstruct__hooks_96 {
   apr_array_header_t *link_pre_mpm ;
};
struct ap_sb_handle_t {
   int child_num ;
   int thread_num ;
};
struct ap_LINK_post_read_request_t {
   ap_HOOK_post_read_request_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_post_read_request_t ap_LINK_post_read_request_t;
struct ap_LINK_log_transaction_t {
   ap_HOOK_log_transaction_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_log_transaction_t ap_LINK_log_transaction_t;
struct ap_LINK_http_method_t {
   ap_HOOK_http_method_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_http_method_t ap_LINK_http_method_t;
struct ap_LINK_default_port_t {
   ap_HOOK_default_port_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_default_port_t ap_LINK_default_port_t;
struct __anonstruct__hooks_98 {
   apr_array_header_t *link_post_read_request ;
   apr_array_header_t *link_log_transaction ;
   apr_array_header_t *link_http_method ;
   apr_array_header_t *link_default_port ;
};
struct content_length_ctx {
   int data_sent ;
};
struct __anonstruct_old_write_filter_ctx_99 {
   apr_bucket_brigade *bb ;
};
typedef struct __anonstruct_old_write_filter_ctx_99 old_write_filter_ctx;
struct ap_vrprintf_data {
   apr_vformatter_buff_t vbuff ;
   request_rec *r ;
   char *buff ;
};
struct apr_bucket_file;
typedef struct apr_bucket_file apr_bucket_file;
struct apr_bucket_file {
   apr_bucket_refcount refcount ;
   apr_file_t *fd ;
   apr_pool_t *readpool ;
   int can_mmap ;
};
struct core_output_filter_ctx {
   apr_bucket_brigade *b ;
   apr_pool_t *deferred_write_pool ;
};
typedef struct core_output_filter_ctx core_output_filter_ctx_t;
struct core_filter_ctx {
   apr_bucket_brigade *b ;
};
typedef struct core_filter_ctx core_ctx_t;
struct core_net_rec {
   apr_socket_t *client_socket ;
   conn_rec *c ;
   core_output_filter_ctx_t *out_ctx ;
   core_ctx_t *in_ctx ;
};
typedef struct core_net_rec core_net_rec;
struct __anonstruct_core_request_config_77 {
   struct apr_bucket_brigade *bb ;
   void **notes ;
   int deliver_script ;
};
typedef struct __anonstruct_core_request_config_77 core_request_config;
struct __anonstruct_core_server_config_80 {
   char const   *ap_document_root ;
   char *access_name ;
   apr_array_header_t *sec_dir ;
   apr_array_header_t *sec_url ;
};
typedef struct __anonstruct_core_server_config_80 core_server_config;
struct ap_LINK_get_mgmt_items_t {
   ap_HOOK_get_mgmt_items_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_get_mgmt_items_t ap_LINK_get_mgmt_items_t;
typedef void apr_OFN_ap_logio_add_bytes_out_t(conn_rec *c , apr_off_t bytes );
typedef int proxy_HOOK_create_req_t(request_rec *r , request_rec *pr );
struct __anonstruct__hooks_107 {
   apr_array_header_t *link_get_mgmt_items ;
};
struct reorder_sort_rec {
   ap_conf_vector_t *elt ;
   int orig_index ;
};
enum __anonenum_what_108 {
    MSG = 0,
    LOCAL_PATH = 1,
    REMOTE_PATH = 2
};
enum server_token_type {
    SrvTk_MAJOR = 0,
    SrvTk_MINOR = 1,
    SrvTk_MINIMAL = 2,
    SrvTk_OS = 3,
    SrvTk_FULL = 4,
    SrvTk_PRODUCT_ONLY = 5
};
struct net_time_filter_ctx {
   apr_socket_t *csd ;
   int first_line ;
};
typedef struct net_time_filter_ctx net_time_filter_ctx_t;
struct ap_LINK_create_request_t {
   ap_HOOK_create_request_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_create_request_t ap_LINK_create_request_t;
struct ap_LINK_translate_name_t {
   ap_HOOK_translate_name_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_translate_name_t ap_LINK_translate_name_t;
struct ap_LINK_map_to_storage_t {
   ap_HOOK_map_to_storage_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_map_to_storage_t ap_LINK_map_to_storage_t;
struct ap_LINK_check_user_id_t {
   ap_HOOK_check_user_id_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_check_user_id_t ap_LINK_check_user_id_t;
struct ap_LINK_fixups_t {
   ap_HOOK_fixups_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_fixups_t ap_LINK_fixups_t;
struct ap_LINK_type_checker_t {
   ap_HOOK_type_checker_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_type_checker_t ap_LINK_type_checker_t;
struct ap_LINK_access_checker_t {
   ap_HOOK_access_checker_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_access_checker_t ap_LINK_access_checker_t;
struct ap_LINK_auth_checker_t {
   ap_HOOK_auth_checker_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_auth_checker_t ap_LINK_auth_checker_t;
struct ap_LINK_insert_filter_t {
   ap_HOOK_insert_filter_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_insert_filter_t ap_LINK_insert_filter_t;
struct __anonstruct__hooks_94 {
   apr_array_header_t *link_translate_name ;
   apr_array_header_t *link_map_to_storage ;
   apr_array_header_t *link_check_user_id ;
   apr_array_header_t *link_fixups ;
   apr_array_header_t *link_type_checker ;
   apr_array_header_t *link_access_checker ;
   apr_array_header_t *link_auth_checker ;
   apr_array_header_t *link_insert_filter ;
   apr_array_header_t *link_create_request ;
};
struct walk_walked_t {
   ap_conf_vector_t *matched ;
   ap_conf_vector_t *merged ;
};
typedef struct walk_walked_t walk_walked_t;
struct walk_cache_t {
   char const   *cached ;
   ap_conf_vector_t **dir_conf_tested ;
   ap_conf_vector_t *dir_conf_merged ;
   ap_conf_vector_t *per_dir_result ;
   apr_array_header_t *walked ;
};
typedef struct walk_cache_t walk_cache_t;
struct core_opts_t {
   allow_options_t opts ;
   allow_options_t add ;
   allow_options_t remove ;
   overrides_t override ;
};
typedef struct core_opts_t core_opts_t;
typedef int __key_t;
typedef unsigned short ushort;
enum __rlimit_resource {
    RLIMIT_CPU = 0,
    RLIMIT_FSIZE = 1,
    RLIMIT_DATA = 2,
    RLIMIT_STACK = 3,
    RLIMIT_CORE = 4,
    RLIMIT_RSS = 5,
    RLIMIT_NOFILE = 7,
    RLIMIT_OFILE = 7,
    RLIMIT_AS = 9,
    RLIMIT_NPROC = 6,
    RLIMIT_MEMLOCK = 8,
    RLIMIT_LOCKS = 10,
    RLIMIT_NLIMITS = 11,
    RLIM_NLIMITS = 11
};
typedef enum __rlimit_resource __rlimit_resource_t;
struct ipc_perm {
   __key_t __key ;
   __uid_t uid ;
   __gid_t gid ;
   __uid_t cuid ;
   __gid_t cgid ;
   unsigned short mode ;
   unsigned short __pad1 ;
   unsigned short __seq ;
   unsigned short __pad2 ;
   unsigned long __unused1 ;
   unsigned long __unused2 ;
};
struct ap_LINK_get_suexec_identity_t {
   ap_HOOK_get_suexec_identity_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct ap_LINK_get_suexec_identity_t ap_LINK_get_suexec_identity_t;
struct __anonstruct_unixd_config_rec_76 {
   char const   *user_name ;
   uid_t user_id ;
   gid_t group_id ;
   int suexec_enabled ;
};
typedef struct __anonstruct_unixd_config_rec_76 unixd_config_rec;
struct semid_ds {
   struct ipc_perm sem_perm ;
   __time_t sem_otime ;
   unsigned long __unused1 ;
   __time_t sem_ctime ;
   unsigned long __unused2 ;
   unsigned long sem_nsems ;
   unsigned long __unused3 ;
   unsigned long __unused4 ;
};
struct __anonstruct__hooks_94___0 {
   apr_array_header_t *link_get_suexec_identity ;
};
union semun {
   long val ;
   struct semid_ds *buf ;
   ushort *array ;
};
struct real_pcre;
struct real_pcre_extra;
typedef struct real_pcre pcre;
typedef struct real_pcre_extra pcre_extra;
typedef int BOOL;
typedef unsigned char uschar;
struct real_pcre {
   unsigned long magic_number ;
   size_t size ;
   unsigned char const   *tables ;
   unsigned long options ;
   unsigned short top_bracket ;
   unsigned short top_backref ;
   uschar first_char ;
   uschar req_char ;
   uschar code[1] ;
};
typedef struct real_pcre real_pcre;
struct real_pcre_extra {
   uschar options ;
   uschar start_bits[32] ;
};
typedef struct real_pcre_extra real_pcre_extra;
struct compile_data {
   uschar const   *lcc ;
   uschar const   *fcc ;
   uschar const   *cbits ;
   uschar const   *ctypes ;
};
typedef struct compile_data compile_data;
struct match_data {
   int errorcode ;
   int *offset_vector ;
   int offset_end ;
   int offset_max ;
   uschar const   *lcc ;
   uschar const   *ctypes ;
   BOOL offset_overflow ;
   BOOL notbol ;
   BOOL noteol ;
   BOOL utf8 ;
   BOOL endonly ;
   BOOL notempty ;
   uschar const   *start_pattern ;
   uschar const   *start_subject ;
   uschar const   *end_subject ;
   uschar const   *start_match ;
   uschar const   *end_match_ptr ;
   int end_offset_top ;
};
typedef struct match_data match_data;
struct eptrblock {
   struct eptrblock *prev ;
   uschar const   *saved_eptr ;
};
typedef struct eptrblock eptrblock;
struct apr_bucket_heap;
typedef struct apr_bucket_heap apr_bucket_heap;
struct apr_bucket_heap {
   apr_bucket_refcount refcount ;
   char *base ;
   apr_size_t alloc_len ;
   void (*free_func)(void *data ) ;
};
struct apr_bucket_pool;
typedef struct apr_bucket_pool apr_bucket_pool;
struct apr_bucket_pool {
   apr_bucket_heap heap ;
   char const   *base ;
   apr_pool_t *pool ;
   apr_bucket_alloc_t *list ;
};
struct apr_bucket_mmap;
typedef struct apr_bucket_mmap apr_bucket_mmap;
struct apr_bucket_mmap {
   apr_bucket_refcount refcount ;
   apr_mmap_t *mmap ;
};
typedef apr_status_t (*apr_brigade_flush)(apr_bucket_brigade *bb , void *ctx );
struct brigade_vprintf_data_t {
   apr_vformatter_buff_t vbuff ;
   apr_bucket_brigade *b ;
   apr_brigade_flush *flusher ;
   void *ctx ;
   char *cbuff ;
};
union apr_bucket_structs;
typedef union apr_bucket_structs apr_bucket_structs;
union apr_bucket_structs {
   apr_bucket b ;
   apr_bucket_heap heap ;
   apr_bucket_pool pool ;
   apr_bucket_mmap mmap ;
   apr_bucket_file file ;
};
struct node_header_t {
   apr_size_t size ;
   apr_bucket_alloc_t *alloc ;
   apr_memnode_t *memnode ;
   struct node_header_t *next ;
};
typedef struct node_header_t node_header_t;
struct apr_bucket_alloc_t {
   apr_pool_t *pool ;
   apr_allocator_t *allocator ;
   node_header_t *freelist ;
   apr_memnode_t *blocks ;
};
typedef unsigned char apr_byte_t;
union endianTest {
   long Long ;
   char Char[(int )sizeof(long )] ;
};
typedef unsigned long long apr_uint64_t;
struct apr_sdbm_t {
   apr_pool_t *pool ;
   apr_file_t *dirf ;
   apr_file_t *pagf ;
   apr_int32_t flags ;
   long maxbno ;
   long curbit ;
   long hmask ;
   long blkptr ;
   int keyptr ;
   long blkno ;
   long pagbno ;
   char pagbuf[1024] ;
   long dirbno ;
   char dirbuf[4096] ;
   int lckcnt ;
};
struct __anonstruct_apr_dbm_type_t_53 {
   char const   *name ;
   apr_status_t (*open)(apr_dbm_t **pdb , char const   *pathname , apr_int32_t mode ,
                        apr_fileperms_t perm , apr_pool_t *pool ) ;
   void (*close)(apr_dbm_t *dbm ) ;
   apr_status_t (*fetch)(apr_dbm_t *dbm , apr_datum_t key , apr_datum_t *pvalue ) ;
   apr_status_t (*store)(apr_dbm_t *dbm , apr_datum_t key , apr_datum_t value ) ;
   apr_status_t (*del)(apr_dbm_t *dbm , apr_datum_t key ) ;
   int (*exists)(apr_dbm_t *dbm , apr_datum_t key ) ;
   apr_status_t (*firstkey)(apr_dbm_t *dbm , apr_datum_t *pkey ) ;
   apr_status_t (*nextkey)(apr_dbm_t *dbm , apr_datum_t *pkey ) ;
   void (*freedatum)(apr_dbm_t *dbm , apr_datum_t data ) ;
   void (*getusednames)(apr_pool_t *pool , char const   *pathname , char const   **used1 ,
                        char const   **used2 ) ;
};
typedef struct __anonstruct_apr_dbm_type_t_53 apr_dbm_type_t;
struct apr_dbm_t {
   apr_pool_t *pool ;
   void *file ;
   int errcode ;
   char const   *errmsg ;
   apr_dbm_type_t const   *type ;
};
typedef unsigned long __u_long;
typedef __u_long u_long;
typedef int int32_t;
typedef unsigned char u_int8_t;
typedef unsigned int u_int32_t;
typedef u_int32_t db_pgno_t;
typedef u_int32_t db_recno_t;
typedef u_int32_t db_timeout_t;
typedef u_int32_t roff_t;
struct __db;
typedef struct __db DB;
struct __db_dbt;
typedef struct __db_dbt DBT;
struct __db_env;
typedef struct __db_env DB_ENV;
struct __db_ilock;
typedef struct __db_ilock DB_LOCK_ILOCK;
struct __db_lock_stat;
typedef struct __db_lock_stat DB_LOCK_STAT;
struct __db_lock_u;
typedef struct __db_lock_u DB_LOCK;
struct __db_lockreq;
typedef struct __db_lockreq DB_LOCKREQ;
struct __db_log_cursor;
typedef struct __db_log_cursor DB_LOGC;
struct __db_log_stat;
typedef struct __db_log_stat DB_LOG_STAT;
struct __db_lsn;
typedef struct __db_lsn DB_LSN;
struct __db_mpool;
typedef struct __db_mpool DB_MPOOL;
struct __db_mpool_fstat;
typedef struct __db_mpool_fstat DB_MPOOL_FSTAT;
struct __db_mpool_stat;
typedef struct __db_mpool_stat DB_MPOOL_STAT;
struct __db_mpoolfile;
typedef struct __db_mpoolfile DB_MPOOLFILE;
struct __db_preplist;
typedef struct __db_preplist DB_PREPLIST;
struct __db_rep_stat;
typedef struct __db_rep_stat DB_REP_STAT;
struct __db_txn;
typedef struct __db_txn DB_TXN;
struct __db_txn_active;
typedef struct __db_txn_active DB_TXN_ACTIVE;
struct __db_txn_stat;
typedef struct __db_txn_stat DB_TXN_STAT;
struct __db_txnmgr;
typedef struct __db_txnmgr DB_TXNMGR;
struct __dbc;
typedef struct __dbc DBC;
struct __dbc_internal;
typedef struct __dbc_internal DBC_INTERNAL;
struct __fh_t;
typedef struct __fh_t DB_FH;
struct __fname;
typedef struct __fname FNAME;
struct __key_range;
typedef struct __key_range DB_KEY_RANGE;
struct __mpoolfile;
typedef struct __mpoolfile MPOOLFILE;
struct __mutex_t;
typedef struct __mutex_t DB_MUTEX;
struct __db_dbt {
   void *data ;
   u_int32_t size ;
   u_int32_t ulen ;
   u_int32_t dlen ;
   u_int32_t doff ;
   u_int32_t flags ;
};
enum __anonenum_db_lockmode_t_54 {
    DB_LOCK_NG = 0,
    DB_LOCK_READ = 1,
    DB_LOCK_WRITE = 2,
    DB_LOCK_WAIT = 3,
    DB_LOCK_IWRITE = 4,
    DB_LOCK_IREAD = 5,
    DB_LOCK_IWR = 6,
    DB_LOCK_DIRTY = 7,
    DB_LOCK_WWRITE = 8
};
typedef enum __anonenum_db_lockmode_t_54 db_lockmode_t;
enum __anonenum_db_lockop_t_55 {
    DB_LOCK_DUMP = 0,
    DB_LOCK_GET = 1,
    DB_LOCK_GET_TIMEOUT = 2,
    DB_LOCK_INHERIT = 3,
    DB_LOCK_PUT = 4,
    DB_LOCK_PUT_ALL = 5,
    DB_LOCK_PUT_OBJ = 6,
    DB_LOCK_PUT_READ = 7,
    DB_LOCK_TIMEOUT = 8,
    DB_LOCK_TRADE = 9,
    DB_LOCK_UPGRADE_WRITE = 10
};
typedef enum __anonenum_db_lockop_t_55 db_lockop_t;
struct __db_lock_stat {
   u_int32_t st_id ;
   u_int32_t st_cur_maxid ;
   u_int32_t st_maxlocks ;
   u_int32_t st_maxlockers ;
   u_int32_t st_maxobjects ;
   u_int32_t st_nmodes ;
   u_int32_t st_nlocks ;
   u_int32_t st_maxnlocks ;
   u_int32_t st_nlockers ;
   u_int32_t st_maxnlockers ;
   u_int32_t st_nobjects ;
   u_int32_t st_maxnobjects ;
   u_int32_t st_nconflicts ;
   u_int32_t st_nrequests ;
   u_int32_t st_nreleases ;
   u_int32_t st_nnowaits ;
   u_int32_t st_ndeadlocks ;
   db_timeout_t st_locktimeout ;
   u_int32_t st_nlocktimeouts ;
   db_timeout_t st_txntimeout ;
   u_int32_t st_ntxntimeouts ;
   u_int32_t st_region_wait ;
   u_int32_t st_region_nowait ;
   u_int32_t st_regsize ;
};
struct __db_ilock {
   db_pgno_t pgno ;
   u_int8_t fileid[20] ;
   u_int32_t type ;
};
struct __db_lock_u {
   size_t off ;
   u_int32_t ndx ;
   u_int32_t gen ;
   db_lockmode_t mode ;
};
struct __db_lockreq {
   db_lockop_t op ;
   db_lockmode_t mode ;
   db_timeout_t timeout ;
   DBT *obj ;
   DB_LOCK lock ;
};
struct __db_lsn {
   u_int32_t file ;
   u_int32_t offset ;
};
struct __db_log_cursor {
   DB_ENV *dbenv ;
   DB_FH *c_fh ;
   DB_LSN c_lsn ;
   u_int32_t c_len ;
   u_int32_t c_prev ;
   DBT c_dbt ;
   u_int8_t *bp ;
   u_int32_t bp_size ;
   u_int32_t bp_rlen ;
   DB_LSN bp_lsn ;
   u_int32_t bp_maxrec ;
   int (*close)(DB_LOGC * , u_int32_t  ) ;
   int (*get)(DB_LOGC * , DB_LSN * , DBT * , u_int32_t  ) ;
   u_int32_t flags ;
};
struct __db_log_stat {
   u_int32_t st_magic ;
   u_int32_t st_version ;
   int st_mode ;
   u_int32_t st_lg_bsize ;
   u_int32_t st_lg_size ;
   u_int32_t st_w_bytes ;
   u_int32_t st_w_mbytes ;
   u_int32_t st_wc_bytes ;
   u_int32_t st_wc_mbytes ;
   u_int32_t st_wcount ;
   u_int32_t st_wcount_fill ;
   u_int32_t st_scount ;
   u_int32_t st_region_wait ;
   u_int32_t st_region_nowait ;
   u_int32_t st_cur_file ;
   u_int32_t st_cur_offset ;
   u_int32_t st_disk_file ;
   u_int32_t st_disk_offset ;
   u_int32_t st_regsize ;
   u_int32_t st_maxcommitperflush ;
   u_int32_t st_mincommitperflush ;
};
enum __anonenum_DB_CACHE_PRIORITY_57 {
    DB_PRIORITY_VERY_LOW = 1,
    DB_PRIORITY_LOW = 2,
    DB_PRIORITY_DEFAULT = 3,
    DB_PRIORITY_HIGH = 4,
    DB_PRIORITY_VERY_HIGH = 5
};
typedef enum __anonenum_DB_CACHE_PRIORITY_57 DB_CACHE_PRIORITY;
struct __anonstruct_q_58 {
   struct __db_mpoolfile *tqe_next ;
   struct __db_mpoolfile **tqe_prev ;
};
struct __db_mpoolfile {
   DB_MUTEX *mutexp ;
   DB_FH *fhp ;
   u_int32_t ref ;
   u_int32_t pinref ;
   struct __anonstruct_q_58 q ;
   int ftype ;
   DBT *pgcookie ;
   u_int8_t *fileid ;
   int32_t lsn_offset ;
   u_int32_t clear_len ;
   DB_MPOOL *dbmp ;
   MPOOLFILE *mfp ;
   void *addr ;
   size_t len ;
   int (*close)(DB_MPOOLFILE * , u_int32_t  ) ;
   int (*get)(DB_MPOOLFILE * , db_pgno_t * , u_int32_t  , void * ) ;
   void (*get_fileid)(DB_MPOOLFILE * , u_int8_t * ) ;
   void (*last_pgno)(DB_MPOOLFILE * , db_pgno_t * ) ;
   int (*open)(DB_MPOOLFILE * , char const   * , u_int32_t  , int  , size_t  ) ;
   int (*put)(DB_MPOOLFILE * , void * , u_int32_t  ) ;
   void (*refcnt)(DB_MPOOLFILE * , db_pgno_t * ) ;
   int (*set)(DB_MPOOLFILE * , void * , u_int32_t  ) ;
   int (*set_clear_len)(DB_MPOOLFILE * , u_int32_t  ) ;
   int (*set_fileid)(DB_MPOOLFILE * , u_int8_t * ) ;
   int (*set_ftype)(DB_MPOOLFILE * , int  ) ;
   int (*set_lsn_offset)(DB_MPOOLFILE * , int32_t  ) ;
   int (*set_pgcookie)(DB_MPOOLFILE * , DBT * ) ;
   int (*set_priority)(DB_MPOOLFILE * , DB_CACHE_PRIORITY  ) ;
   void (*set_unlink)(DB_MPOOLFILE * , int  ) ;
   int (*sync)(DB_MPOOLFILE * ) ;
   u_int32_t flags ;
};
struct __db_mpool_stat {
   u_int32_t st_gbytes ;
   u_int32_t st_bytes ;
   u_int32_t st_ncache ;
   u_int32_t st_regsize ;
   u_int32_t st_map ;
   u_int32_t st_cache_hit ;
   u_int32_t st_cache_miss ;
   u_int32_t st_page_create ;
   u_int32_t st_page_in ;
   u_int32_t st_page_out ;
   u_int32_t st_ro_evict ;
   u_int32_t st_rw_evict ;
   u_int32_t st_page_trickle ;
   u_int32_t st_pages ;
   u_int32_t st_page_clean ;
   u_int32_t st_page_dirty ;
   u_int32_t st_hash_buckets ;
   u_int32_t st_hash_searches ;
   u_int32_t st_hash_longest ;
   u_int32_t st_hash_examined ;
   u_int32_t st_hash_nowait ;
   u_int32_t st_hash_wait ;
   u_int32_t st_hash_max_wait ;
   u_int32_t st_region_nowait ;
   u_int32_t st_region_wait ;
   u_int32_t st_alloc ;
   u_int32_t st_alloc_buckets ;
   u_int32_t st_alloc_max_buckets ;
   u_int32_t st_alloc_pages ;
   u_int32_t st_alloc_max_pages ;
};
struct __db_mpool_fstat {
   char *file_name ;
   size_t st_pagesize ;
   u_int32_t st_map ;
   u_int32_t st_cache_hit ;
   u_int32_t st_cache_miss ;
   u_int32_t st_page_create ;
   u_int32_t st_page_in ;
   u_int32_t st_page_out ;
};
enum __anonenum_db_recops_59 {
    DB_TXN_ABORT = 0,
    DB_TXN_APPLY = 1,
    DB_TXN_BACKWARD_ALLOC = 2,
    DB_TXN_BACKWARD_ROLL = 3,
    DB_TXN_FORWARD_ROLL = 4,
    DB_TXN_GETPGNOS = 5,
    DB_TXN_OPENFILES = 6,
    DB_TXN_POPENFILES = 7,
    DB_TXN_PRINT = 8
};
typedef enum __anonenum_db_recops_59 db_recops;
struct __anonstruct_links_60 {
   struct __db_txn *tqe_next ;
   struct __db_txn **tqe_prev ;
};
struct __txn_event;
struct __anonstruct_events_61 {
   struct __txn_event *tqh_first ;
   struct __txn_event **tqh_last ;
};
struct __kids {
   struct __db_txn *tqh_first ;
   struct __db_txn **tqh_last ;
};
struct __anonstruct_klinks_62 {
   struct __db_txn *tqe_next ;
   struct __db_txn **tqe_prev ;
};
struct __db_txn {
   DB_TXNMGR *mgrp ;
   DB_TXN *parent ;
   DB_LSN last_lsn ;
   u_int32_t txnid ;
   roff_t off ;
   db_timeout_t lock_timeout ;
   db_timeout_t expire ;
   void *txn_list ;
   struct __anonstruct_links_60 links ;
   struct __anonstruct_events_61 events ;
   struct __kids kids ;
   struct __anonstruct_klinks_62 klinks ;
   void *api_internal ;
   u_int32_t cursors ;
   int (*abort)(DB_TXN * ) ;
   int (*commit)(DB_TXN * , u_int32_t  ) ;
   int (*discard)(DB_TXN * , u_int32_t  ) ;
   u_int32_t (*id)(DB_TXN * ) ;
   int (*prepare)(DB_TXN * , u_int8_t * ) ;
   int (*set_timeout)(DB_TXN * , db_timeout_t  , u_int32_t  ) ;
   u_int32_t flags ;
};
struct __db_txn_active {
   u_int32_t txnid ;
   u_int32_t parentid ;
   DB_LSN lsn ;
};
struct __db_txn_stat {
   DB_LSN st_last_ckp ;
   time_t st_time_ckp ;
   u_int32_t st_last_txnid ;
   u_int32_t st_maxtxns ;
   u_int32_t st_naborts ;
   u_int32_t st_nbegins ;
   u_int32_t st_ncommits ;
   u_int32_t st_nactive ;
   u_int32_t st_nrestores ;
   u_int32_t st_maxnactive ;
   DB_TXN_ACTIVE *st_txnarray ;
   u_int32_t st_region_wait ;
   u_int32_t st_region_nowait ;
   u_int32_t st_regsize ;
};
struct __db_preplist {
   DB_TXN *txn ;
   u_int8_t gid[128] ;
};
struct __db_rep_stat {
   u_int32_t st_status ;
   DB_LSN st_next_lsn ;
   DB_LSN st_waiting_lsn ;
   u_int32_t st_dupmasters ;
   int st_env_id ;
   int st_env_priority ;
   u_int32_t st_gen ;
   u_int32_t st_log_duplicated ;
   u_int32_t st_log_queued ;
   u_int32_t st_log_queued_max ;
   u_int32_t st_log_queued_total ;
   u_int32_t st_log_records ;
   u_int32_t st_log_requested ;
   int st_master ;
   u_int32_t st_master_changes ;
   u_int32_t st_msgs_badgen ;
   u_int32_t st_msgs_processed ;
   u_int32_t st_msgs_recover ;
   u_int32_t st_msgs_send_failures ;
   u_int32_t st_msgs_sent ;
   u_int32_t st_newsites ;
   int st_nsites ;
   u_int32_t st_nthrottles ;
   u_int32_t st_outdated ;
   u_int32_t st_txns_applied ;
   u_int32_t st_elections ;
   u_int32_t st_elections_won ;
   int st_election_cur_winner ;
   u_int32_t st_election_gen ;
   DB_LSN st_election_lsn ;
   int st_election_nsites ;
   int st_election_priority ;
   int st_election_status ;
   int st_election_tiebreaker ;
   int st_election_votes ;
};
enum __anonenum_DBTYPE_63 {
    DB_BTREE = 1,
    DB_HASH = 2,
    DB_RECNO = 3,
    DB_QUEUE = 4,
    DB_UNKNOWN = 5
};
typedef enum __anonenum_DBTYPE_63 DBTYPE;
struct __anonstruct_dblistlinks_64 {
   struct __db *le_next ;
   struct __db **le_prev ;
};
struct __cq_fq {
   struct __dbc *tqh_first ;
   struct __dbc **tqh_last ;
};
struct __cq_aq {
   struct __dbc *tqh_first ;
   struct __dbc **tqh_last ;
};
struct __cq_jq {
   struct __dbc *tqh_first ;
   struct __dbc **tqh_last ;
};
struct __anonstruct_s_secondaries_65 {
   struct __db *lh_first ;
};
struct __anonstruct_s_links_66 {
   struct __db *le_next ;
   struct __db **le_prev ;
};
struct __db {
   u_int32_t pgsize ;
   int (*db_append_recno)(DB * , DBT * , db_recno_t  ) ;
   void (*db_feedback)(DB * , int  , int  ) ;
   int (*dup_compare)(DB * , DBT const   * , DBT const   * ) ;
   void *app_private ;
   DB_ENV *dbenv ;
   DBTYPE type ;
   DB_MPOOLFILE *mpf ;
   DB_CACHE_PRIORITY priority ;
   DB_MUTEX *mutexp ;
   u_int8_t fileid[20] ;
   u_int32_t adj_fileid ;
   FNAME *log_filename ;
   db_pgno_t meta_pgno ;
   u_int32_t lid ;
   u_int32_t cur_lid ;
   u_int32_t associate_lid ;
   DB_LOCK handle_lock ;
   long cl_id ;
   DBT my_rskey ;
   DBT my_rkey ;
   DBT my_rdata ;
   DB_FH *saved_open_fhp ;
   struct __anonstruct_dblistlinks_64 dblistlinks ;
   struct __cq_fq free_queue ;
   struct __cq_aq active_queue ;
   struct __cq_jq join_queue ;
   struct __anonstruct_s_secondaries_65 s_secondaries ;
   struct __anonstruct_s_links_66 s_links ;
   u_int32_t s_refcnt ;
   int (*s_callback)(DB * , DBT const   * , DBT const   * , DBT * ) ;
   DB *s_primary ;
   void *api_internal ;
   void *bt_internal ;
   void *h_internal ;
   void *q_internal ;
   void *xa_internal ;
   int (*associate)(DB * , DB_TXN * , DB * , int (*)(DB * , DBT const   * , DBT const   * ,
                                                     DBT * ) , u_int32_t  ) ;
   int (*close)(DB * , u_int32_t  ) ;
   int (*cursor)(DB * , DB_TXN * , DBC ** , u_int32_t  ) ;
   int (*del)(DB * , DB_TXN * , DBT * , u_int32_t  ) ;
   void (*err)(DB * , int  , char const   *  , ...) ;
   void (*errx)(DB * , char const   *  , ...) ;
   int (*fd)(DB * , int * ) ;
   int (*get)(DB * , DB_TXN * , DBT * , DBT * , u_int32_t  ) ;
   int (*pget)(DB * , DB_TXN * , DBT * , DBT * , DBT * , u_int32_t  ) ;
   int (*get_byteswapped)(DB * , int * ) ;
   int (*get_type)(DB * , DBTYPE * ) ;
   int (*join)(DB * , DBC ** , DBC ** , u_int32_t  ) ;
   int (*key_range)(DB * , DB_TXN * , DBT * , DB_KEY_RANGE * , u_int32_t  ) ;
   int (*open)(DB * , DB_TXN * , char const   * , char const   * , DBTYPE  , u_int32_t  ,
               int  ) ;
   int (*put)(DB * , DB_TXN * , DBT * , DBT * , u_int32_t  ) ;
   int (*remove)(DB * , char const   * , char const   * , u_int32_t  ) ;
   int (*rename)(DB * , char const   * , char const   * , char const   * , u_int32_t  ) ;
   int (*truncate)(DB * , DB_TXN * , u_int32_t * , u_int32_t  ) ;
   int (*set_append_recno)(DB * , int (*)(DB * , DBT * , db_recno_t  ) ) ;
   int (*set_alloc)(DB * , void *(*)(size_t  ) , void *(*)(void * , size_t  ) , void (*)(void * ) ) ;
   int (*set_cachesize)(DB * , u_int32_t  , u_int32_t  , int  ) ;
   int (*set_cache_priority)(DB * , DB_CACHE_PRIORITY  ) ;
   int (*set_dup_compare)(DB * , int (*)(DB * , DBT const   * , DBT const   * ) ) ;
   int (*set_encrypt)(DB * , char const   * , u_int32_t  ) ;
   void (*set_errcall)(DB * , void (*)(char const   * , char * ) ) ;
   void (*set_errfile)(DB * , FILE * ) ;
   void (*set_errpfx)(DB * , char const   * ) ;
   int (*set_feedback)(DB * , void (*)(DB * , int  , int  ) ) ;
   int (*set_flags)(DB * , u_int32_t  ) ;
   int (*set_lorder)(DB * , int  ) ;
   int (*set_pagesize)(DB * , u_int32_t  ) ;
   int (*set_paniccall)(DB * , void (*)(DB_ENV * , int  ) ) ;
   int (*stat)(DB * , void * , u_int32_t  ) ;
   int (*sync)(DB * , u_int32_t  ) ;
   int (*upgrade)(DB * , char const   * , u_int32_t  ) ;
   int (*verify)(DB * , char const   * , char const   * , FILE * , u_int32_t  ) ;
   int (*set_bt_compare)(DB * , int (*)(DB * , DBT const   * , DBT const   * ) ) ;
   int (*set_bt_maxkey)(DB * , u_int32_t  ) ;
   int (*set_bt_minkey)(DB * , u_int32_t  ) ;
   int (*set_bt_prefix)(DB * , size_t (*)(DB * , DBT const   * , DBT const   * ) ) ;
   int (*set_h_ffactor)(DB * , u_int32_t  ) ;
   int (*set_h_hash)(DB * , u_int32_t (*)(DB * , void const   * , u_int32_t  ) ) ;
   int (*set_h_nelem)(DB * , u_int32_t  ) ;
   int (*set_re_delim)(DB * , int  ) ;
   int (*set_re_len)(DB * , u_int32_t  ) ;
   int (*set_re_pad)(DB * , int  ) ;
   int (*set_re_source)(DB * , char const   * ) ;
   int (*set_q_extentsize)(DB * , u_int32_t  ) ;
   int (*db_am_remove)(DB * , DB_TXN * , char const   * , char const   * , DB_LSN * ) ;
   int (*db_am_rename)(DB * , DB_TXN * , char const   * , char const   * , char const   * ) ;
   int (*stored_get)(DB * , DB_TXN * , DBT * , DBT * , u_int32_t  ) ;
   int (*stored_close)(DB * , u_int32_t  ) ;
   u_int32_t am_ok ;
   u_int32_t flags ;
};
struct __anonstruct_links_67 {
   DBC *tqe_next ;
   DBC **tqe_prev ;
};
struct __dbc {
   DB *dbp ;
   DB_TXN *txn ;
   struct __anonstruct_links_67 links ;
   DBT *rskey ;
   DBT *rkey ;
   DBT *rdata ;
   DBT my_rskey ;
   DBT my_rkey ;
   DBT my_rdata ;
   u_int32_t lid ;
   u_int32_t locker ;
   DBT lock_dbt ;
   DB_LOCK_ILOCK lock ;
   DB_LOCK mylock ;
   long cl_id ;
   DBTYPE dbtype ;
   DBC_INTERNAL *internal ;
   int (*c_close)(DBC * ) ;
   int (*c_count)(DBC * , db_recno_t * , u_int32_t  ) ;
   int (*c_del)(DBC * , u_int32_t  ) ;
   int (*c_dup)(DBC * , DBC ** , u_int32_t  ) ;
   int (*c_get)(DBC * , DBT * , DBT * , u_int32_t  ) ;
   int (*c_pget)(DBC * , DBT * , DBT * , DBT * , u_int32_t  ) ;
   int (*c_put)(DBC * , DBT * , DBT * , u_int32_t  ) ;
   int (*c_am_bulk)(DBC * , DBT * , u_int32_t  ) ;
   int (*c_am_close)(DBC * , db_pgno_t  , int * ) ;
   int (*c_am_del)(DBC * ) ;
   int (*c_am_destroy)(DBC * ) ;
   int (*c_am_get)(DBC * , DBT * , DBT * , u_int32_t  , db_pgno_t * ) ;
   int (*c_am_put)(DBC * , DBT * , DBT * , u_int32_t  , db_pgno_t * ) ;
   int (*c_am_writelock)(DBC * ) ;
   int (*c_real_get)(DBC * , DBT * , DBT * , u_int32_t  ) ;
   u_int32_t flags ;
};
struct __key_range {
   double less ;
   double equal ;
   double greater ;
};
struct __anonstruct_dblist_68 {
   struct __db *lh_first ;
};
struct __anonstruct_links_69 {
   struct __db_env *tqe_next ;
   struct __db_env **tqe_prev ;
};
struct __db_env {
   FILE *db_errfile ;
   char const   *db_errpfx ;
   void (*db_errcall)(char const   * , char * ) ;
   void (*db_feedback)(DB_ENV * , int  , int  ) ;
   void (*db_paniccall)(DB_ENV * , int  ) ;
   void *(*db_malloc)(size_t  ) ;
   void *(*db_realloc)(void * , size_t  ) ;
   void (*db_free)(void * ) ;
   u_int32_t verbose ;
   void *app_private ;
   int (*app_dispatch)(DB_ENV * , DBT * , DB_LSN * , db_recops  ) ;
   u_int8_t *lk_conflicts ;
   u_int32_t lk_modes ;
   u_int32_t lk_max ;
   u_int32_t lk_max_lockers ;
   u_int32_t lk_max_objects ;
   u_int32_t lk_detect ;
   db_timeout_t lk_timeout ;
   u_int32_t lg_bsize ;
   u_int32_t lg_size ;
   u_int32_t lg_regionmax ;
   u_int32_t mp_gbytes ;
   u_int32_t mp_bytes ;
   size_t mp_size ;
   int mp_ncache ;
   size_t mp_mmapsize ;
   int rep_eid ;
   u_int32_t tx_max ;
   time_t tx_timestamp ;
   db_timeout_t tx_timeout ;
   int panic_errval ;
   char *db_home ;
   char *db_log_dir ;
   char *db_tmp_dir ;
   char **db_data_dir ;
   int data_cnt ;
   int data_next ;
   int db_mode ;
   void *reginfo ;
   DB_FH *lockfhp ;
   int (**recover_dtab)(DB_ENV * , DBT * , DB_LSN * , db_recops  , void * ) ;
   size_t recover_dtab_size ;
   void *cl_handle ;
   long cl_id ;
   int db_ref ;
   long shm_key ;
   u_int32_t tas_spins ;
   DB_MUTEX *dblist_mutexp ;
   struct __anonstruct_dblist_68 dblist ;
   struct __anonstruct_links_69 links ;
   int xa_rmid ;
   DB_TXN *xa_txn ;
   void *api1_internal ;
   void *api2_internal ;
   char *passwd ;
   size_t passwd_len ;
   void *crypto_handle ;
   DB_MUTEX *mt_mutexp ;
   int mti ;
   u_long *mt ;
   int (*close)(DB_ENV * , u_int32_t  ) ;
   int (*dbremove)(DB_ENV * , DB_TXN * , char const   * , char const   * , u_int32_t  ) ;
   int (*dbrename)(DB_ENV * , DB_TXN * , char const   * , char const   * , char const   * ,
                   u_int32_t  ) ;
   void (*err)(DB_ENV const   * , int  , char const   *  , ...) ;
   void (*errx)(DB_ENV const   * , char const   *  , ...) ;
   int (*open)(DB_ENV * , char const   * , u_int32_t  , int  ) ;
   int (*remove)(DB_ENV * , char const   * , u_int32_t  ) ;
   int (*set_data_dir)(DB_ENV * , char const   * ) ;
   int (*set_alloc)(DB_ENV * , void *(*)(size_t  ) , void *(*)(void * , size_t  ) ,
                    void (*)(void * ) ) ;
   int (*set_app_dispatch)(DB_ENV * , int (*)(DB_ENV * , DBT * , DB_LSN * , db_recops  ) ) ;
   int (*set_encrypt)(DB_ENV * , char const   * , u_int32_t  ) ;
   void (*set_errcall)(DB_ENV * , void (*)(char const   * , char * ) ) ;
   void (*set_errfile)(DB_ENV * , FILE * ) ;
   void (*set_errpfx)(DB_ENV * , char const   * ) ;
   int (*set_feedback)(DB_ENV * , void (*)(DB_ENV * , int  , int  ) ) ;
   int (*set_flags)(DB_ENV * , u_int32_t  , int  ) ;
   int (*set_paniccall)(DB_ENV * , void (*)(DB_ENV * , int  ) ) ;
   int (*set_rpc_server)(DB_ENV * , void * , char const   * , long  , long  , u_int32_t  ) ;
   int (*set_shm_key)(DB_ENV * , long  ) ;
   int (*set_tas_spins)(DB_ENV * , u_int32_t  ) ;
   int (*set_tmp_dir)(DB_ENV * , char const   * ) ;
   int (*set_verbose)(DB_ENV * , u_int32_t  , int  ) ;
   void *lg_handle ;
   int (*set_lg_bsize)(DB_ENV * , u_int32_t  ) ;
   int (*set_lg_dir)(DB_ENV * , char const   * ) ;
   int (*set_lg_max)(DB_ENV * , u_int32_t  ) ;
   int (*set_lg_regionmax)(DB_ENV * , u_int32_t  ) ;
   int (*log_archive)(DB_ENV * , char *** , u_int32_t  ) ;
   int (*log_cursor)(DB_ENV * , DB_LOGC ** , u_int32_t  ) ;
   int (*log_file)(DB_ENV * , DB_LSN const   * , char * , size_t  ) ;
   int (*log_flush)(DB_ENV * , DB_LSN const   * ) ;
   int (*log_put)(DB_ENV * , DB_LSN * , DBT const   * , u_int32_t  ) ;
   int (*log_stat)(DB_ENV * , DB_LOG_STAT ** , u_int32_t  ) ;
   void *lk_handle ;
   int (*set_lk_conflicts)(DB_ENV * , u_int8_t * , int  ) ;
   int (*set_lk_detect)(DB_ENV * , u_int32_t  ) ;
   int (*set_lk_max)(DB_ENV * , u_int32_t  ) ;
   int (*set_lk_max_locks)(DB_ENV * , u_int32_t  ) ;
   int (*set_lk_max_lockers)(DB_ENV * , u_int32_t  ) ;
   int (*set_lk_max_objects)(DB_ENV * , u_int32_t  ) ;
   int (*lock_detect)(DB_ENV * , u_int32_t  , u_int32_t  , int * ) ;
   int (*lock_dump_region)(DB_ENV * , char * , FILE * ) ;
   int (*lock_get)(DB_ENV * , u_int32_t  , u_int32_t  , DBT const   * , db_lockmode_t  ,
                   DB_LOCK * ) ;
   int (*lock_put)(DB_ENV * , DB_LOCK * ) ;
   int (*lock_id)(DB_ENV * , u_int32_t * ) ;
   int (*lock_id_free)(DB_ENV * , u_int32_t  ) ;
   int (*lock_id_set)(DB_ENV * , u_int32_t  , u_int32_t  ) ;
   int (*lock_stat)(DB_ENV * , DB_LOCK_STAT ** , u_int32_t  ) ;
   int (*lock_vec)(DB_ENV * , u_int32_t  , u_int32_t  , DB_LOCKREQ * , int  , DB_LOCKREQ ** ) ;
   int (*lock_downgrade)(DB_ENV * , DB_LOCK * , db_lockmode_t  , u_int32_t  ) ;
   void *mp_handle ;
   int (*set_mp_mmapsize)(DB_ENV * , size_t  ) ;
   int (*set_cachesize)(DB_ENV * , u_int32_t  , u_int32_t  , int  ) ;
   int (*memp_dump_region)(DB_ENV * , char * , FILE * ) ;
   int (*memp_fcreate)(DB_ENV * , DB_MPOOLFILE ** , u_int32_t  ) ;
   int (*memp_nameop)(DB_ENV * , u_int8_t * , char const   * , char const   * , char const   * ) ;
   int (*memp_register)(DB_ENV * , int  , int (*)(DB_ENV * , db_pgno_t  , void * ,
                                                  DBT * ) , int (*)(DB_ENV * , db_pgno_t  ,
                                                                    void * , DBT * ) ) ;
   int (*memp_stat)(DB_ENV * , DB_MPOOL_STAT ** , DB_MPOOL_FSTAT *** , u_int32_t  ) ;
   int (*memp_sync)(DB_ENV * , DB_LSN * ) ;
   int (*memp_trickle)(DB_ENV * , int  , int * ) ;
   void *rep_handle ;
   int (*rep_elect)(DB_ENV * , int  , int  , u_int32_t  , int * ) ;
   int (*rep_flush)(DB_ENV * ) ;
   int (*rep_process_message)(DB_ENV * , DBT * , DBT * , int * ) ;
   int (*rep_start)(DB_ENV * , DBT * , u_int32_t  ) ;
   int (*rep_stat)(DB_ENV * , DB_REP_STAT ** , u_int32_t  ) ;
   int (*set_rep_election)(DB_ENV * , u_int32_t  , u_int32_t  , u_int32_t  , u_int32_t  ) ;
   int (*set_rep_limit)(DB_ENV * , u_int32_t  , u_int32_t  ) ;
   int (*set_rep_request)(DB_ENV * , u_int32_t  , u_int32_t  ) ;
   int (*set_rep_timeout)(DB_ENV * , u_int32_t  , u_int32_t  ) ;
   int (*set_rep_transport)(DB_ENV * , int  , int (*)(DB_ENV * , DBT const   * , DBT const   * ,
                                                      int  , u_int32_t  ) ) ;
   void *tx_handle ;
   int (*set_tx_max)(DB_ENV * , u_int32_t  ) ;
   int (*set_tx_timestamp)(DB_ENV * , time_t * ) ;
   int (*txn_begin)(DB_ENV * , DB_TXN * , DB_TXN ** , u_int32_t  ) ;
   int (*txn_checkpoint)(DB_ENV * , u_int32_t  , u_int32_t  , u_int32_t  ) ;
   int (*txn_id_set)(DB_ENV * , u_int32_t  , u_int32_t  ) ;
   int (*txn_recover)(DB_ENV * , DB_PREPLIST * , long  , long * , u_int32_t  ) ;
   int (*txn_stat)(DB_ENV * , DB_TXN_STAT ** , u_int32_t  ) ;
   int (*set_timeout)(DB_ENV * , db_timeout_t  , u_int32_t  ) ;
   int test_abort ;
   int test_copy ;
   u_int32_t flags ;
};
struct __anonstruct_real_file_t_73 {
   DB *bdb ;
   DBC *curs ;
};
typedef struct __anonstruct_real_file_t_73 real_file_t;
struct __anonstruct_GDBM_FILE_58 {
   int dummy[10] ;
};
typedef struct __anonstruct_GDBM_FILE_58 *GDBM_FILE;
typedef int gdbm_error;
typedef GDBM_FILE real_file_t___0;
typedef apr_sdbm_datum_t *cvt_datum_t;
typedef apr_sdbm_datum_t result_datum_t;
typedef apr_sdbm_t *real_file_t___1;
struct __anonstruct_TSortData_54 {
   void (*dummy)(void * ) ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct __anonstruct_TSortData_54 TSortData;
struct tsort_ {
   void *pData ;
   int nPredecessors ;
   struct tsort_ **ppPredecessors ;
   struct tsort_ *pNext ;
};
typedef struct tsort_ TSort;
struct __anonstruct_HookSortEntry_55 {
   char const   *szHookName ;
   apr_array_header_t **paHooks ;
};
typedef struct __anonstruct_HookSortEntry_55 HookSortEntry;
typedef void apr_HOOK__optional_t(void);
struct apr_LINK__optional_t {
   apr_HOOK__optional_t *pFunc ;
   char const   *szName ;
   char const   * const  *aszPredecessors ;
   char const   * const  *aszSuccessors ;
   int nOrder ;
};
typedef struct apr_LINK__optional_t apr_LINK__optional_t;
struct schemes_t;
typedef struct schemes_t schemes_t;
struct schemes_t {
   char const   *name ;
   apr_port_t default_port ;
};
typedef struct apr_xml_attr apr_xml_attr;
struct XML_ParserStruct;
typedef struct XML_ParserStruct *XML_Parser;
typedef char XML_Char;
typedef char XML_LChar;
enum XML_Error {
    XML_ERROR_NONE = 0,
    XML_ERROR_NO_MEMORY = 1,
    XML_ERROR_SYNTAX = 2,
    XML_ERROR_NO_ELEMENTS = 3,
    XML_ERROR_INVALID_TOKEN = 4,
    XML_ERROR_UNCLOSED_TOKEN = 5,
    XML_ERROR_PARTIAL_CHAR = 6,
    XML_ERROR_TAG_MISMATCH = 7,
    XML_ERROR_DUPLICATE_ATTRIBUTE = 8,
    XML_ERROR_JUNK_AFTER_DOC_ELEMENT = 9,
    XML_ERROR_PARAM_ENTITY_REF = 10,
    XML_ERROR_UNDEFINED_ENTITY = 11,
    XML_ERROR_RECURSIVE_ENTITY_REF = 12,
    XML_ERROR_ASYNC_ENTITY = 13,
    XML_ERROR_BAD_CHAR_REF = 14,
    XML_ERROR_BINARY_ENTITY_REF = 15,
    XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF = 16,
    XML_ERROR_MISPLACED_XML_PI = 17,
    XML_ERROR_UNKNOWN_ENCODING = 18,
    XML_ERROR_INCORRECT_ENCODING = 19,
    XML_ERROR_UNCLOSED_CDATA_SECTION = 20,
    XML_ERROR_EXTERNAL_ENTITY_HANDLING = 21,
    XML_ERROR_NOT_STANDALONE = 22,
    XML_ERROR_UNEXPECTED_STATE = 23,
    XML_ERROR_ENTITY_DECLARED_IN_PE = 24,
    XML_ERROR_FEATURE_REQUIRES_XML_DTD = 25,
    XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING = 26
};
enum XML_Status {
    XML_STATUS_ERROR = 0,
    XML_STATUS_OK = 1
};
struct apr_xml_parser {
   apr_xml_doc *doc ;
   apr_pool_t *p ;
   apr_xml_elem *cur_elem ;
   int error ;
   XML_Parser xp ;
   enum XML_Error xp_err ;
};
struct apr_xml_ns_scope {
   char const   *prefix ;
   int ns ;
   int emptyURI ;
   struct apr_xml_ns_scope *next ;
};
typedef struct apr_xml_ns_scope apr_xml_ns_scope;
struct rmm_block_t {
   apr_size_t size ;
   apr_rmm_off_t prev ;
   apr_rmm_off_t next ;
};
typedef struct rmm_block_t rmm_block_t;
struct rmm_hdr_block_t {
   apr_size_t abssize ;
   apr_rmm_off_t firstused ;
   apr_rmm_off_t firstfree ;
};
typedef struct rmm_hdr_block_t rmm_hdr_block_t;
struct apr_rmm_t {
   apr_pool_t *p ;
   rmm_hdr_block_t *base ;
   apr_size_t size ;
   apr_anylock_t lock ;
};
union grainbit {
   long l ;
   long *pl ;
};
struct apr_res_t;
struct __anonstruct_link_42 {
   struct apr_res_t *next ;
   struct apr_res_t *prev ;
};
struct apr_res_t {
   apr_time_t freed ;
   void *opaque ;
   struct __anonstruct_link_42 link ;
};
typedef struct apr_res_t apr_res_t;
struct apr_resring_t {
   struct apr_res_t *next ;
   struct apr_res_t *prev ;
};
typedef struct apr_resring_t apr_resring_t;
struct apr_reslist_t {
   apr_pool_t *pool ;
   int ntotal ;
   int nidle ;
   int min ;
   int smax ;
   int hmax ;
   apr_interval_time_t ttl ;
   apr_status_t (*constructor)(void **resource , void *params , apr_pool_t *pool ) ;
   apr_status_t (*destructor)(void *resource , void *params , apr_pool_t *pool ) ;
   void *params ;
   apr_resring_t avail_list ;
   apr_resring_t free_list ;
   apr_thread_mutex_t *listlock ;
   apr_thread_cond_t *avail ;
};
struct apr_queue_t {
   void **data ;
   unsigned int nelts ;
   unsigned int in ;
   unsigned int out ;
   unsigned int bounds ;
   apr_thread_mutex_t *one_big_mutex ;
   apr_thread_cond_t *not_empty ;
   apr_thread_cond_t *not_full ;
   int terminated ;
};
typedef void *iconv_t;
struct apr_xlate_t {
   apr_pool_t *pool ;
   char *frompage ;
   char *topage ;
   char *sbcs_table ;
   iconv_t ich ;
};
enum __anonenum_boolean_e_61 {
    NO = 0,
    YES = 1
};
typedef enum __anonenum_boolean_e_61 boolean_e;
typedef long wide_int;
typedef unsigned long u_wide_int;
typedef apr_int64_t widest_int;
typedef apr_uint64_t u_widest_int;
typedef int bool_int;
enum __anonenum_adjust_62 {
    LEFT = 0,
    RIGHT = 1
};
enum var_type_enum {
    IS_QUAD = 0,
    IS_LONG = 1,
    IS_SHORT = 2,
    IS_INT = 3
};
struct apr_hash_entry_t;
typedef struct apr_hash_entry_t apr_hash_entry_t;
struct apr_hash_entry_t {
   apr_hash_entry_t *next ;
   unsigned int hash ;
   void const   *key ;
   apr_ssize_t klen ;
   void const   *val ;
};
struct apr_hash_index_t {
   apr_hash_t *ht ;
   apr_hash_entry_t *this ;
   apr_hash_entry_t *next ;
   unsigned int index ;
};
struct apr_hash_t {
   apr_pool_t *pool ;
   apr_hash_entry_t **array ;
   apr_hash_index_t iterator ;
   unsigned int count ;
   unsigned int max ;
};
struct apr_table_t {
   apr_array_header_t a ;
   apr_uint32_t index_initialized ;
   int index_first[32] ;
   int index_last[32] ;
};
struct overlap_key {
   apr_table_entry_t *elt ;
   int level ;
   int skip ;
   int black ;
   struct overlap_key *tree_parent ;
   struct overlap_key *tree_left ;
   struct overlap_key *tree_right ;
   int color ;
   struct overlap_key *merge_next ;
   struct overlap_key *merge_last ;
};
typedef struct overlap_key overlap_key;
enum __anonenum_blocking_60 {
    BLK_UNKNOWN = 0,
    BLK_OFF = 1,
    BLK_ON = 2
};
struct apr_file_t {
   apr_pool_t *pool ;
   int filedes ;
   char *fname ;
   apr_int32_t flags ;
   int eof_hit ;
   int is_pipe ;
   apr_interval_time_t timeout ;
   int buffered ;
   enum __anonenum_blocking_60 blocking ;
   int ungetchar ;
   char *buffer ;
   int bufpos ;
   unsigned long dataRead ;
   int direction ;
   unsigned long filePtr ;
   struct apr_thread_mutex_t *thlock ;
};
typedef unsigned int __mode_t;
typedef __mode_t mode_t;
struct dirent {
   __ino_t d_ino ;
   __off_t d_off ;
   unsigned short d_reclen ;
   unsigned char d_type ;
   char d_name[256] ;
};
struct apr_dir_t {
   apr_pool_t *pool ;
   char *dirname ;
   DIR *dirstruct ;
   struct dirent *entry ;
};
typedef unsigned int __nlink_t;
typedef long __blksize_t;
typedef long __blkcnt_t;
struct timespec {
   __time_t tv_sec ;
   long tv_nsec ;
};
struct stat {
   __dev_t st_dev ;
   unsigned short __pad1 ;
   __ino_t st_ino ;
   __mode_t st_mode ;
   __nlink_t st_nlink ;
   __uid_t st_uid ;
   __gid_t st_gid ;
   __dev_t st_rdev ;
   unsigned short __pad2 ;
   __off_t st_size ;
   __blksize_t st_blksize ;
   __blkcnt_t st_blocks ;
   struct timespec st_atim ;
   struct timespec st_mtim ;
   struct timespec st_ctim ;
   unsigned long __unused4 ;
   unsigned long __unused5 ;
};
struct flock {
   short l_type ;
   short l_whence ;
   __off_t l_start ;
   __off_t l_len ;
   __pid_t l_pid ;
};
struct __anonstruct_best_63 {
   int base ;
   int len ;
};
struct apr_socket_t {
   apr_pool_t *cntxt ;
   int socketdes ;
   int type ;
   int protocol ;
   apr_sockaddr_t *local_addr ;
   apr_sockaddr_t *remote_addr ;
   apr_interval_time_t timeout ;
   int local_port_unknown ;
   int local_interface_unknown ;
   int remote_addr_unknown ;
   apr_int32_t netmask ;
   apr_int32_t inherit ;
};
struct servent {
   char *s_name ;
   char **s_aliases ;
   int s_port ;
   char *s_proto ;
};
struct addrinfo {
   int ai_flags ;
   int ai_family ;
   int ai_socktype ;
   int ai_protocol ;
   socklen_t ai_addrlen ;
   struct sockaddr *ai_addr ;
   char *ai_canonname ;
   struct addrinfo *ai_next ;
};
struct apr_ipsubnet_t {
   int family ;
   apr_uint32_t sub[4] ;
   apr_uint32_t mask[4] ;
};
struct linger {
   int l_onoff ;
   int l_linger ;
};
struct apr_procattr_t {
   apr_pool_t *pool ;
   apr_file_t *parent_in ;
   apr_file_t *child_in ;
   apr_file_t *parent_out ;
   apr_file_t *child_out ;
   apr_file_t *parent_err ;
   apr_file_t *child_err ;
   char *currdir ;
   apr_int32_t cmdtype ;
   apr_int32_t detached ;
   struct rlimit *limit_cpu ;
   struct rlimit *limit_mem ;
   struct rlimit *limit_nproc ;
   struct rlimit *limit_nofile ;
};
union __anonunion___u_81 {
   int __in ;
   int __i ;
};
union __anonunion___u_82 {
   int __in ;
   int __i ;
};
union __anonunion___u_83 {
   int __in ;
   int __i ;
};
union __anonunion___u_84 {
   int __in ;
   int __i ;
};
union __anonunion___u_85 {
   int __in ;
   int __i ;
};
struct __sched_param {
   int __sched_priority ;
};
struct __pthread_attr_s {
   int __detachstate ;
   int __schedpolicy ;
   struct __sched_param __schedparam ;
   int __inheritsched ;
   int __scope ;
   size_t __guardsize ;
   int __stackaddr_set ;
   void *__stackaddr ;
   size_t __stacksize ;
};
typedef struct __pthread_attr_s pthread_attr_t;
typedef int pthread_once_t;
struct apr_thread_t {
   apr_pool_t *pool ;
   pthread_t *td ;
   void *data ;
   void *(*func)(apr_thread_t * , void * ) ;
   apr_status_t exitval ;
};
struct apr_threadattr_t {
   apr_pool_t *pool ;
   pthread_attr_t *attr ;
};
struct apr_thread_once_t {
   pthread_once_t once ;
};
struct apr_threadkey_t {
   apr_pool_t *pool ;
   pthread_key_t key ;
};
struct apr_other_child_rec_t;
typedef struct apr_other_child_rec_t apr_other_child_rec_t;
struct apr_other_child_rec_t {
   apr_pool_t *p ;
   struct apr_other_child_rec_t *next ;
   apr_proc_t *proc ;
   void (*maintenance)(int  , void * , int  ) ;
   void *data ;
   apr_os_file_t write_fd ;
};
struct apr_proc_mutex_unix_lock_methods_t {
   unsigned int flags ;
   apr_status_t (*create)(apr_proc_mutex_t * , char const   * ) ;
   apr_status_t (*acquire)(apr_proc_mutex_t * ) ;
   apr_status_t (*tryacquire)(apr_proc_mutex_t * ) ;
   apr_status_t (*release)(apr_proc_mutex_t * ) ;
   apr_status_t (*cleanup)(void * ) ;
   apr_status_t (*child_init)(apr_proc_mutex_t ** , apr_pool_t * , char const   * ) ;
   char const   *name ;
};
typedef struct apr_proc_mutex_unix_lock_methods_t apr_proc_mutex_unix_lock_methods_t;
struct apr_proc_mutex_t {
   apr_pool_t *pool ;
   apr_proc_mutex_unix_lock_methods_t const   *meth ;
   apr_proc_mutex_unix_lock_methods_t const   *inter_meth ;
   int curr_locked ;
   char *fname ;
   apr_file_t *interproc ;
   pthread_mutex_t *pthread_interproc ;
};
struct apr_thread_mutex_t {
   apr_pool_t *pool ;
   pthread_mutex_t mutex ;
   apr_os_thread_t owner ;
   int owner_ref ;
   char nested ;
};
struct apr_global_mutex_t {
   apr_pool_t *pool ;
   apr_proc_mutex_t *proc_mutex ;
   apr_thread_mutex_t *thread_mutex ;
};
typedef __key_t key_t;
struct __anonstruct_pthread_mutexattr_t_7 {
   int __mutexkind ;
};
typedef struct __anonstruct_pthread_mutexattr_t_7 pthread_mutexattr_t;
struct sembuf {
   unsigned short sem_num ;
   short sem_op ;
   short sem_flg ;
};
struct __anonstruct_sem_t_81 {
   struct _pthread_fastlock __sem_lock ;
   int __sem_value ;
   _pthread_descr __sem_waiting ;
};
typedef struct __anonstruct_sem_t_81 sem_t;
union semun___0 {
   int val ;
   struct semid_ds *buf ;
   unsigned short *array ;
};
typedef long long __pthread_cond_align_t;
struct __anonstruct_pthread_cond_t_4 {
   struct _pthread_fastlock __c_lock ;
   _pthread_descr __c_waiting ;
   char __padding[(int )(((48U - sizeof(struct _pthread_fastlock )) - sizeof(_pthread_descr )) -
                         sizeof(__pthread_cond_align_t ))] ;
   __pthread_cond_align_t __align ;
};
typedef struct __anonstruct_pthread_cond_t_4 pthread_cond_t;
struct __anonstruct_pthread_condattr_t_5 {
   int __dummy ;
};
typedef struct __anonstruct_pthread_condattr_t_5 pthread_condattr_t;
struct apr_thread_cond_t {
   apr_pool_t *pool ;
   pthread_cond_t *cond ;
};
struct _pthread_rwlock_t {
   struct _pthread_fastlock __rw_lock ;
   int __rw_readers ;
   _pthread_descr __rw_writer ;
   _pthread_descr __rw_read_waiting ;
   _pthread_descr __rw_write_waiting ;
   int __rw_kind ;
   int __rw_pshared ;
};
typedef struct _pthread_rwlock_t pthread_rwlock_t;
struct __anonstruct_pthread_rwlockattr_t_8 {
   int __lockkind ;
   int __pshared ;
};
typedef struct __anonstruct_pthread_rwlockattr_t_8 pthread_rwlockattr_t;
struct apr_thread_rwlock_t {
   apr_pool_t *pool ;
   pthread_rwlock_t *rwlock ;
};
typedef long __fd_mask;
struct __anonstruct_fd_set_3 {
   __fd_mask fds_bits[(int )(1024U / (8U * sizeof(__fd_mask )))] ;
};
typedef struct __anonstruct_fd_set_3 fd_set;
struct timezone {
   int tz_minuteswest ;
   int tz_dsttime ;
};
typedef struct timezone * __restrict  __timezone_ptr_t;
typedef unsigned long shmatt_t;
struct shmid_ds {
   struct ipc_perm shm_perm ;
   size_t shm_segsz ;
   __time_t shm_atime ;
   unsigned long __unused1 ;
   __time_t shm_dtime ;
   unsigned long __unused2 ;
   __time_t shm_ctime ;
   unsigned long __unused3 ;
   __pid_t shm_cpid ;
   __pid_t shm_lpid ;
   shmatt_t shm_nattch ;
   unsigned long __unused4 ;
   unsigned long __unused5 ;
};
struct apr_shm_t {
   apr_pool_t *pool ;
   void *base ;
   void *usable ;
   apr_size_t reqsize ;
   apr_size_t realsize ;
   char const   *filename ;
   int shmid ;
};
struct apr_allocator_t {
   apr_uint32_t max_index ;
   apr_uint32_t max_free_index ;
   apr_uint32_t current_free_index ;
   apr_thread_mutex_t *mutex ;
   apr_pool_t *owner ;
   apr_memnode_t *free[20] ;
};
struct cleanup_t;
typedef struct cleanup_t cleanup_t;
struct process_chain {
   apr_proc_t *pid ;
   apr_kill_conditions_e kill_how ;
   struct process_chain *next ;
};
struct apr_pool_t {
   apr_pool_t *parent ;
   apr_pool_t *child ;
   apr_pool_t *sibling ;
   apr_pool_t **ref ;
   cleanup_t *cleanups ;
   apr_allocator_t *allocator ;
   struct process_chain *subprocesses ;
   int (*abort_fn)(int retcode ) ;
   apr_hash_t *user_data ;
   char const   *tag ;
   apr_memnode_t *active ;
   apr_memnode_t *self ;
   char *self_first_avail ;
};
struct psprintf_data {
   apr_vformatter_buff_t vbuff ;
   apr_memnode_t *node ;
   apr_pool_t *pool ;
   apr_byte_t got_a_new_node ;
   apr_memnode_t *free ;
};
struct cleanup_t {
   struct cleanup_t *next ;
   void const   *data ;
   apr_status_t (*plain_cleanup_fn)(void *data ) ;
   apr_status_t (*child_cleanup_fn)(void *data ) ;
};
typedef unsigned long nfds_t;
struct pollfd {
   int fd ;
   short events ;
   short revents ;
};
struct apr_pollset_t {
   apr_uint32_t nelts ;
   apr_uint32_t nalloc ;
   struct pollfd *pollset ;
   apr_pollfd_t *query_set ;
   apr_pollfd_t *result_set ;
   apr_pool_t *pool ;
};
struct apr_dso_handle_t {
   apr_pool_t *pool ;
   void *handle ;
   char const   *errormsg ;
};
#pragma merger(0,"/tmp/cil-Fs5tByA2.i","-g -pthread")
struct module_struct core_module ;
struct module_struct access_module ;
struct module_struct auth_module ;
struct module_struct include_module ;
struct module_struct log_config_module ;
struct module_struct env_module ;
struct module_struct setenvif_module ;
struct module_struct mpm_prefork_module ;
struct module_struct http_module ;
struct module_struct mime_module ;
struct module_struct status_module ;
struct module_struct autoindex_module ;
struct module_struct asis_module ;
struct module_struct cgi_module ;
struct module_struct negotiation_module ;
struct module_struct dir_module ;
struct module_struct imap_module ;
struct module_struct actions_module ;
struct module_struct userdir_module ;
struct module_struct alias_module ;
struct module_struct so_module ;
module *ap_prelinked_modules[22]  = 
  {      & core_module,      & access_module,      & auth_module,      & include_module, 
        & log_config_module,      & env_module,      & setenvif_module,      & mpm_prefork_module, 
        & http_module,      & mime_module,      & status_module,      & autoindex_module, 
        & asis_module,      & cgi_module,      & negotiation_module,      & dir_module, 
        & imap_module,      & actions_module,      & userdir_module,      & alias_module, 
        & so_module,      (module *)((void *)0)};
module *ap_preloaded_modules[22]  = 
  {      & core_module,      & access_module,      & auth_module,      & include_module, 
        & log_config_module,      & env_module,      & setenvif_module,      & mpm_prefork_module, 
        & http_module,      & mime_module,      & status_module,      & autoindex_module, 
        & asis_module,      & cgi_module,      & negotiation_module,      & dir_module, 
        & imap_module,      & actions_module,      & userdir_module,      & alias_module, 
        & so_module,      (module *)((void *)0)};
#pragma merger(0,"/tmp/cil-Qh2uTZ0N.i","-g -pthread")
char *apr_strerror(apr_status_t statcode , char *buf , apr_size_t bufsize ) ;
extern void *memset(void *__s , int __c , size_t __n ) ;
extern char *strchr(char const   *__s , int __c )  __attribute__((__pure__)) ;
extern size_t strlen(char const   *__s )  __attribute__((__pure__)) ;
extern int strcasecmp(char const   *__s1 , char const   *__s2 )  __attribute__((__pure__)) ;
extern int strncasecmp(char const   *__s1 , char const   *__s2 , size_t __n )  __attribute__((__pure__)) ;
void *apr_palloc(apr_pool_t *pool , apr_size_t size ) ;
char *apr_pstrdup(apr_pool_t *a , char const   *s ) ;
apr_status_t apr_ipsubnet_create(apr_ipsubnet_t **ipsub , char const   *ipstr , char const   *mask_or_numbits ,
                                 apr_pool_t *p ) ;
int apr_ipsubnet_test(apr_ipsubnet_t *ipsub , apr_sockaddr_t *sa ) ;
apr_array_header_t *apr_array_make(apr_pool_t *p , int nelts , int elt_size ) ;
void *apr_array_push(apr_array_header_t *arr ) ;
char const   *apr_table_get(apr_table_t const   *t , char const   *key ) ;
char const   *ap_get_remote_host(conn_rec *conn , void *dir_config , int type , int *str_is_ip ) ;
int ap_satisfies(request_rec *r ) ;
void ( /* format attribute */  ap_log_rerror)(char const   *file , int line , int level ,
                                              apr_status_t status , request_rec const   *r ,
                                              char const   *fmt  , ...) ;
int ap_some_auth_required(request_rec *r ) ;
void ap_hook_access_checker(ap_HOOK_access_checker_t *pf , char const   * const  *aszPre___1 ,
                            char const   * const  *aszSucc___2 , int nOrder ) ;
extern  __attribute__((__noreturn__)) void exit(int __status ) ;



////////////


apr_status_t apr_unix_file_cleanup(void *thefile ) ;

static void piped_log_maintenance(int reason , void *data , int status ) ;

extern ssize_t write(int __fd , void const   *__buf , size_t __n ) ;
extern int *__errno_location(void)  __attribute__((__const__)) ;
extern FILE *freopen(char const   * __restrict  __filename , 
                     char const   * __restrict  __modes ,
                     FILE * __restrict  __stream ) ;

extern int pthread_mutex_init(pthread_mutex_t * __restrict  __mutex , pthread_mutexattr_t const   * __restrict  __mutex_attr ) ;
extern int pthread_mutex_lock(pthread_mutex_t *__mutex ) ;
extern int pthread_mutex_unlock(pthread_mutex_t *__mutex ) ;
extern int pthread_mutexattr_init(pthread_mutexattr_t *__attr ) ;
extern int pthread_mutexattr_destroy(pthread_mutexattr_t *__attr ) ;
extern int pthread_mutexattr_setpshared(pthread_mutexattr_t *__attr , int __pshared ) ;
extern int semget(key_t __key , int __nsems , int __semflg ) ;
extern int semop(int __semid , struct sembuf *__sops , size_t __nsops ) ;
extern int flock(int __fd , int __operation ) ;
extern void *mmap(void *__addr , size_t __len , int __prot , int __flags , int __fd ,
                  __off_t __offset ) ;
extern int munmap(void *__addr , size_t __len ) ;
extern sem_t *sem_open(char const   *__name , int __oflag  , ...) ;
extern int sem_close(sem_t *__sem ) ;
extern int sem_unlink(char const   *__name ) ;
extern int sem_wait(sem_t *__sem ) ;
extern int sem_post(sem_t *__sem ) ;


apr_status_t apr_file_close(apr_file_t *file ) ;

#pragma merger(0,"/tmp/cil-0wTY2QWb.i","-g -pthread")
apr_status_t apr_initialize(void) ;
void apr_terminate2(void) ;
apr_status_t apr_generate_random_bytes(unsigned char *buf , int length ) ;
apr_status_t apr_pool_initialize(void) ;
void apr_pool_terminate(void) ;
void apr_allocator_destroy(apr_allocator_t *allocator ) ;
apr_memnode_t *apr_allocator_alloc(apr_allocator_t *allocator , apr_size_t size ) ;
void apr_allocator_free(apr_allocator_t *allocator , apr_memnode_t *node ) ;
void apr_allocator_set_owner(apr_allocator_t *allocator , apr_pool_t *pool ) ;
apr_pool_t *apr_allocator_owner_get(apr_allocator_t *allocator ) ;
apr_pool_t *apr_allocator_get_owner(apr_allocator_t *allocator ) ;
void apr_allocator_set_max_free(apr_allocator_t *allocator , apr_size_t size ) ;
apr_status_t apr_thread_mutex_create(apr_thread_mutex_t **mutex , unsigned int flags ,
                                     apr_pool_t *pool ) ;
apr_status_t apr_thread_mutex_lock(apr_thread_mutex_t *mutex ) ;
apr_status_t apr_thread_mutex_trylock(apr_thread_mutex_t *mutex ) ;
apr_status_t apr_thread_mutex_unlock(apr_thread_mutex_t *mutex ) ;
apr_status_t apr_thread_mutex_destroy(apr_thread_mutex_t *mutex ) ;
apr_pool_t *apr_thread_mutex_pool_get(apr_thread_mutex_t const   *ob ) ;
void apr_allocator_mutex_set(apr_allocator_t *allocator , apr_thread_mutex_t *mutex ) ;
void apr_allocator_set_mutex(apr_allocator_t *allocator , apr_thread_mutex_t *mutex ) ;
apr_thread_mutex_t *apr_allocator_mutex_get(apr_allocator_t *allocator ) ;
apr_thread_mutex_t *apr_allocator_get_mutex(apr_allocator_t *allocator ) ;
apr_status_t apr_pool_create_ex_debug(apr_pool_t **newpool , apr_pool_t *parent ,
                                      int (*abort_fn)(int retcode ) , apr_allocator_t *allocator ,
                                      char const   *file_line ) ;
apr_allocator_t *apr_pool_allocator_get(apr_pool_t *pool ) ;
void apr_pool_clear_debug(apr_pool_t *pool , char const   *file_line ) ;
void apr_pool_destroy_debug(apr_pool_t *pool , char const   *file_line ) ;
void *apr_palloc_debug(apr_pool_t *pool , apr_size_t size , char const   *file_line ) ;
void *apr_pcalloc_debug(apr_pool_t *pool , apr_size_t size , char const   *file_line ) ;
void apr_pool_abort_set(int (*abort_fn)(int retcode ) , apr_pool_t *pool ) ;
void apr_pool_set_abort(int (*abort_fn)(int retcode ) , apr_pool_t *pool ) ;
apr_abortfunc_t apr_pool_abort_get(apr_pool_t *pool ) ;
apr_abortfunc_t apr_pool_get_abort(apr_pool_t *pool ) ;
apr_pool_t *apr_pool_parent_get(apr_pool_t *pool ) ;
apr_pool_t *apr_pool_get_parent(apr_pool_t *pool ) ;
int apr_pool_is_ancestor(apr_pool_t *a , apr_pool_t *b ) ;
apr_status_t apr_pool_userdata_set(void const   *data , char const   *key , apr_status_t (*cleanup)(void * ) ,
                                   apr_pool_t *pool ) ;
apr_status_t apr_pool_userdata_setn(void const   *data , char const   *key , apr_status_t (*cleanup)(void * ) ,
                                    apr_pool_t *pool ) ;
apr_status_t apr_pool_userdata_get(void **data , char const   *key , apr_pool_t *pool ) ;
void apr_pool_child_cleanup_set(apr_pool_t *p , void const   *data , apr_status_t (*plain_cleanup_fn)(void * ) ,
                                apr_status_t (*child_cleanup_fn)(void * ) ) ;
void apr_pool_cleanup_for_exec(void) ;
void *apr_array_pop(apr_array_header_t *arr ) ;
void apr_array_cat(apr_array_header_t *dst , apr_array_header_t const   *src ) ;
apr_array_header_t *apr_array_copy_hdr(apr_pool_t *p , apr_array_header_t const   *arr ) ;
void apr_table_merge(apr_table_t *t , char const   *key , char const   *val ) ;
int apr_table_vdo(apr_table_do_callback_fn_t *comp , void *rec , apr_table_t const   *t ,
                  va_list vp ) ;
void apr_sort_hooks(void) ;
void apr_show_hook(char const   *szName , char const   * const  *aszPre___1 , char const   * const  *aszSucc___2 ) ;
void apr_optional_hook_add(char const   *szName , void (*pfn)(void) , char const   * const  *aszPre___1 ,
                           char const   * const  *aszSucc___2 , int nOrder ) ;
apr_array_header_t *apr_optional_hook_get(char const   *szName ) ;
apr_status_t apr_time_ansi_put(apr_time_t *result , time_t input ) ;
apr_status_t apr_time_exp_tz(apr_time_exp_t *result , apr_time_t input , apr_int32_t offs ) ;
apr_status_t apr_explode_time(apr_time_exp_t *result , apr_time_t input , apr_int32_t offs ) ;
apr_status_t apr_explode_localtime(apr_time_exp_t *result , apr_time_t input ) ;
apr_status_t apr_time_exp_get(apr_time_t *t , apr_time_exp_t *xt ) ;
apr_status_t apr_time_exp_gmt_get(apr_time_t *t , apr_time_exp_t *xt ) ;
apr_status_t apr_implode_gmt(apr_time_t *t , apr_time_exp_t *xt ) ;
apr_status_t apr_rfc822_date(char *date_str , apr_time_t t ) ;
void apr_time_clock_hires(apr_pool_t *p ) ;
apr_status_t apr_uid_current(apr_uid_t *uid , apr_gid_t *gid , apr_pool_t *p ) ;
apr_status_t apr_current_userid(apr_uid_t *uid , apr_gid_t *gid , apr_pool_t *p ) ;
apr_status_t apr_uid_name_get(char **username , apr_uid_t userid , apr_pool_t *p ) ;
apr_status_t apr_uid_get(apr_uid_t *uid , apr_gid_t *gid , char const   *username ,
                         apr_pool_t *p ) ;
apr_status_t apr_uid_homepath_get(char **dirname , char const   *username , apr_pool_t *p ) ;
apr_status_t apr_gid_name_get(char **groupname , apr_gid_t groupid , apr_pool_t *p ) ;
apr_status_t apr_group_name_get(char **groupname , apr_gid_t groupid , apr_pool_t *p ) ;
apr_status_t apr_get_groupname(char **groupname , apr_gid_t groupid , apr_pool_t *p ) ;
apr_status_t apr_gid_get(apr_gid_t *groupid , char const   *groupname , apr_pool_t *p ) ;
apr_status_t apr_get_groupid(apr_gid_t *groupid , char const   *groupname , apr_pool_t *p ) ;
apr_status_t apr_dir_rewind(apr_dir_t *thedir ) ;
apr_status_t apr_filepath_get(char **defpath , apr_int32_t flags , apr_pool_t *p ) ;
apr_status_t apr_filepath_set(char const   *path , apr_pool_t *p ) ;
apr_status_t apr_filepath_encoding(int *style , apr_pool_t *p ) ;
apr_status_t apr_file_remove(char const   *path , apr_pool_t *pool ) ;
apr_status_t apr_file_rename(char const   *from_path , char const   *to_path , apr_pool_t *p ) ;
apr_status_t apr_file_copy(char const   *from_path , char const   *to_path , apr_fileperms_t perms ,
                           apr_pool_t *pool ) ;
apr_status_t apr_file_append(char const   *from_path , char const   *to_path , apr_fileperms_t perms ,
                             apr_pool_t *pool ) ;
apr_status_t apr_file_open_stdout(apr_file_t **thefile , apr_pool_t *pool ) ;
apr_status_t apr_file_open_stdin(apr_file_t **thefile , apr_pool_t *pool ) ;
apr_status_t apr_file_writev(apr_file_t *thefile , struct iovec  const  *vec , apr_size_t nvec ,
                             apr_size_t *nbytes ) ;
apr_status_t apr_file_putc(char ch , apr_file_t *thefile ) ;
apr_status_t apr_file_dup(apr_file_t **new_file , apr_file_t *old_file , apr_pool_t *p ) ;
apr_status_t apr_file_setaside(apr_file_t **new_file , apr_file_t *old_file , apr_pool_t *p ) ;
apr_status_t apr_file_namedpipe_create(char const   *filename , apr_fileperms_t perm ,
                                       apr_pool_t *pool ) ;
apr_status_t apr_file_pipe_timeout_get(apr_file_t *thepipe , apr_interval_time_t *timeout ) ;
apr_status_t apr_file_lock(apr_file_t *thefile , int type ) ;
apr_status_t apr_file_unlock(apr_file_t *thefile ) ;
apr_status_t apr_file_name_get(char const   **fname , apr_file_t *thefile ) ;
apr_status_t apr_file_data_get(void **data , char const   *key , apr_file_t *file ) ;
apr_status_t apr_file_data_set(apr_file_t *file , void *data , char const   *key ,
                               apr_status_t (*cleanup)(void * ) ) ;
apr_status_t apr_file_perms_set(char const   *fname , apr_fileperms_t perms ) ;
apr_status_t apr_file_attrs_set(char const   *fname , apr_fileattrs_t attributes ,
                                apr_fileattrs_t attr_mask , apr_pool_t *pool ) ;
apr_status_t apr_dir_make(char const   *path , apr_fileperms_t perm , apr_pool_t *pool ) ;
apr_status_t apr_dir_make_recursive(char const   *path , apr_fileperms_t perm , apr_pool_t *pool ) ;
apr_status_t apr_dir_remove(char const   *path , apr_pool_t *pool ) ;
apr_status_t apr_file_trunc(apr_file_t *fp , apr_off_t offset ) ;
apr_int32_t apr_file_flags_get(apr_file_t *f ) ;
apr_pool_t *apr_file_pool_get(apr_file_t const   *ob ) ;
void apr_file_set_inherit(apr_file_t *thefile ) ;
apr_status_t apr_file_inherit_unset(apr_file_t *thefile ) ;
void apr_file_unset_inherit(apr_file_t *thefile ) ;
apr_status_t apr_file_mktemp(apr_file_t **fp , char *template , apr_int32_t flags ,
                             apr_pool_t *p ) ;
apr_status_t apr_socket_create_ex(apr_socket_t **new , int ofamily , int type , int protocol ,
                                  apr_pool_t *cont ) ;
apr_status_t apr_socket_shutdown(apr_socket_t *thesocket , apr_shutdown_how_e how ) ;
apr_status_t apr_socket_bind(apr_socket_t *sock , apr_sockaddr_t *sa ) ;
apr_status_t apr_socket_listen(apr_socket_t *sock , apr_int32_t backlog ) ;
apr_status_t apr_socket_accept(apr_socket_t **new , apr_socket_t *sock , apr_pool_t *connection_context ) ;
apr_status_t apr_accept(apr_socket_t **new , apr_socket_t *sock , apr_pool_t *connection_context ) ;
apr_status_t apr_socket_connect(apr_socket_t *sock , apr_sockaddr_t *sa ) ;
apr_status_t apr_gethostname(char *buf , int len , apr_pool_t *cont ) ;
apr_status_t apr_socket_data_get(void **data , char const   *key , apr_socket_t *sock ) ;
apr_status_t apr_socket_data_set(apr_socket_t *sock , void *data , char const   *key ,
                                 apr_status_t (*cleanup)(void * ) ) ;
apr_status_t apr_socket_send(apr_socket_t *sock , char const   *buf , apr_size_t *len ) ;
apr_status_t apr_socket_sendv(apr_socket_t *sock , struct iovec  const  *vec , apr_int32_t nvec ,
                              apr_size_t *len ) ;
apr_status_t apr_sendv(apr_socket_t *sock , struct iovec  const  *vec , apr_int32_t nvec ,
                       apr_size_t *len ) ;
apr_status_t apr_socket_sendto(apr_socket_t *sock , apr_sockaddr_t *where , apr_int32_t flags ,
                               char const   *buf , apr_size_t *len ) ;
apr_status_t apr_sendto(apr_socket_t *sock , apr_sockaddr_t *where , apr_int32_t flags ,
                        char const   *buf , apr_size_t *len ) ;
apr_status_t apr_socket_recvfrom(apr_sockaddr_t *from , apr_socket_t *sock , apr_int32_t flags ,
                                 char *buf , apr_size_t *len ) ;
apr_status_t apr_recvfrom(apr_sockaddr_t *from , apr_socket_t *sock , apr_int32_t flags ,
                          char *buf , apr_size_t *len ) ;
apr_status_t apr_socket_sendfile(apr_socket_t *sock , apr_file_t *file , apr_hdtr_t *hdtr ,
                                 apr_off_t *offset , apr_size_t *len , apr_int32_t flags ) ;
apr_status_t apr_sendfile(apr_socket_t *sock , apr_file_t *file , apr_hdtr_t *hdtr ,
                          apr_off_t *offset , apr_size_t *len , apr_int32_t flags ) ;
apr_status_t apr_socket_recv(apr_socket_t *sock , char *buf , apr_size_t *len ) ;
apr_status_t apr_setsocketopt(apr_socket_t *sock , apr_int32_t opt , apr_int32_t on ) ;
apr_status_t apr_socket_opt_get(apr_socket_t *sock , apr_int32_t opt , apr_int32_t *on ) ;
apr_status_t apr_getsocketopt(apr_socket_t *sock , apr_int32_t opt , apr_int32_t *on ) ;
apr_status_t apr_socket_timeout_get(apr_socket_t *sock , apr_interval_time_t *t ) ;
apr_status_t apr_socket_addr_get(apr_sockaddr_t **sa , apr_interface_e which , apr_socket_t *sock ) ;
apr_status_t apr_sockaddr_port_set(apr_sockaddr_t *sockaddr , apr_port_t port ) ;
apr_status_t apr_sockaddr_ip_set(apr_sockaddr_t *sockaddr , char const   *addr ) ;
apr_status_t apr_socket_from_file(apr_socket_t **newsock , apr_file_t *file ) ;
apr_status_t apr_getservbyname(apr_sockaddr_t *sockaddr , char const   *servname ) ;
apr_status_t apr_socket_protocol_get(apr_socket_t *sock , int *protocol ) ;
apr_status_t apr_socket_inherit_set(apr_socket_t *thesocket ) ;
void apr_socket_set_inherit(apr_socket_t *thesocket ) ;
apr_status_t apr_socket_inherit_unset(apr_socket_t *thesocket ) ;
void apr_socket_unset_inherit(apr_socket_t *thesocket ) ;
apr_status_t apr_mmap_create(apr_mmap_t **new , apr_file_t *file , apr_off_t offset ,
                             apr_size_t size , apr_int32_t flag , apr_pool_t *cont ) ;
apr_status_t apr_mmap_dup(apr_mmap_t **new_mmap , apr_mmap_t *old_mmap , apr_pool_t *p ,
                          int transfer_ownership ) ;
apr_status_t apr_mmap_delete(apr_mmap_t *mm ) ;
apr_status_t apr_mmap_offset(void **addr , apr_mmap_t *mmap___0 , apr_off_t offset ) ;
apr_status_t apr_brigade_pflatten(apr_bucket_brigade *bb , char **c , apr_size_t *len ,
                                  apr_pool_t *pool ) ;
apr_status_t apr_brigade_split_line(apr_bucket_brigade *bbOut , apr_bucket_brigade *bbIn ,
                                    apr_read_type_e block , apr_off_t maxbytes ) ;
apr_status_t apr_brigade_to_iovec(apr_bucket_brigade *b , struct iovec *vec , int *nvec ) ;
apr_status_t apr_brigade_putc(apr_bucket_brigade *b , apr_status_t (*flush)(apr_bucket_brigade *bb ,
                                                                            void *ctx ) ,
                              void *ctx , char c ) ;
apr_status_t apr_brigade_printf(apr_bucket_brigade *b , apr_status_t (*flush)(apr_bucket_brigade *bb ,
                                                                              void *ctx ) ,
                                void *ctx , char const   *fmt  , ...) ;
void apr_bucket_alloc_destroy(apr_bucket_alloc_t *list ) ;
void *apr_bucket_alloc(apr_size_t size , apr_bucket_alloc_t *list ) ;
void apr_bucket_free(void *mem ) ;
apr_status_t apr_bucket_setaside_noop(apr_bucket *data , apr_pool_t *pool ) ;
apr_status_t apr_bucket_setaside_notimpl(apr_bucket *data , apr_pool_t *pool ) ;
apr_status_t apr_bucket_split_notimpl(apr_bucket *data , apr_size_t point ) ;
apr_status_t apr_bucket_copy_notimpl(apr_bucket *e , apr_bucket **c ) ;
void apr_bucket_destroy_noop(void *data ) ;
apr_status_t apr_bucket_simple_split(apr_bucket *a , apr_size_t point ) ;
apr_status_t apr_bucket_simple_copy(apr_bucket *a , apr_bucket **b ) ;
apr_bucket *apr_bucket_shared_make(apr_bucket *b , void *data , apr_off_t start ,
                                   apr_size_t length ) ;
int apr_bucket_shared_destroy(void *data ) ;
apr_status_t apr_bucket_shared_split(apr_bucket *a , apr_size_t point ) ;
apr_status_t apr_bucket_shared_copy(apr_bucket *a , apr_bucket **b ) ;
apr_bucket *apr_bucket_eos_make(apr_bucket *b ) ;
apr_bucket *apr_bucket_flush_make(apr_bucket *b ) ;
apr_bucket *apr_bucket_immortal_make(apr_bucket *b , char const   *buf , apr_size_t length ) ;
apr_bucket *apr_bucket_transient_make(apr_bucket *b , char const   *buf , apr_size_t length ) ;
apr_bucket *apr_bucket_heap_make(apr_bucket *b , char const   *buf , apr_size_t length ,
                                 void (*free_func)(void *data ) ) ;
apr_bucket *apr_bucket_pool_make(apr_bucket *b , char const   *buf , apr_size_t length ,
                                 apr_pool_t *pool ) ;
apr_bucket *apr_bucket_mmap_create(apr_mmap_t *mm , apr_off_t start , apr_size_t length ,
                                   apr_bucket_alloc_t *list ) ;
apr_bucket *apr_bucket_mmap_make(apr_bucket *b , apr_mmap_t *mm , apr_off_t start ,
                                 apr_size_t length ) ;
apr_bucket *apr_bucket_socket_create(apr_socket_t *p , apr_bucket_alloc_t *list ) ;
apr_bucket *apr_bucket_socket_make(apr_bucket *b , apr_socket_t *p ) ;
apr_bucket *apr_bucket_pipe_make(apr_bucket *b , apr_file_t *p ) ;
apr_bucket *apr_bucket_file_make(apr_bucket *b , apr_file_t *fd , apr_off_t offset ,
                                 apr_size_t len , apr_pool_t *p ) ;
apr_status_t apr_bucket_file_enable_mmap(apr_bucket *e , int enabled ) ;
void ap_add_version_component(apr_pool_t *pconf___0 , char const   *component ) ;
apr_port_t apr_uri_port_of_scheme(char const   *scheme_str ) ;
apr_port_t apr_uri_default_port_for_scheme(char const   *scheme_str ) ;
int apr_uri_parse_hostinfo(apr_pool_t *p , char const   *hostinfo , apr_uri_t *uptr ) ;
apr_status_t apr_threadattr_create(apr_threadattr_t **new , apr_pool_t *pool ) ;
apr_status_t apr_threadattr_detach_set(apr_threadattr_t *attr , apr_int32_t on ) ;
apr_status_t apr_threadattr_detach_get(apr_threadattr_t *attr ) ;
apr_status_t apr_thread_create(apr_thread_t **new , apr_threadattr_t *attr , void *(*func)(apr_thread_t * ,
                                                                                           void * ) ,
                               void *data , apr_pool_t *pool ) ;
apr_status_t apr_thread_exit(apr_thread_t *thd , apr_status_t retval ) ;
apr_status_t apr_thread_join(apr_status_t *retval , apr_thread_t *thd ) ;
void apr_thread_yield(void) ;
apr_status_t apr_thread_once_init(apr_thread_once_t **control , apr_pool_t *p ) ;
apr_status_t apr_thread_once(apr_thread_once_t *control , void (*func)(void) ) ;
apr_status_t apr_thread_detach(apr_thread_t *thd ) ;
apr_status_t apr_thread_data_get(void **data , char const   *key , apr_thread_t *thread ) ;
apr_status_t apr_thread_data_set(void *data , char const   *key , apr_status_t (*cleanup)(void * ) ,
                                 apr_thread_t *thread ) ;
apr_status_t apr_threadkey_private_create(apr_threadkey_t **key , void (*dest)(void * ) ,
                                          apr_pool_t *pool ) ;
apr_status_t apr_threadkey_private_get(void **new , apr_threadkey_t *key ) ;
apr_status_t apr_threadkey_private_set(void *priv , apr_threadkey_t *key ) ;
apr_status_t apr_threadkey_private_delete(apr_threadkey_t *key ) ;
apr_status_t apr_threadkey_data_get(void **data , char const   *key , apr_threadkey_t *threadkey ) ;
apr_status_t apr_threadkey_data_set(void *data , char const   *key , apr_status_t (*cleanup)(void * ) ,
                                    apr_threadkey_t *threadkey ) ;
apr_status_t apr_procattr_child_out_set(struct apr_procattr_t *attr , apr_file_t *child_out ,
                                        apr_file_t *parent_out ) ;
apr_status_t apr_procattr_child_err_set(struct apr_procattr_t *attr , apr_file_t *child_err ,
                                        apr_file_t *parent_err ) ;
apr_status_t apr_proc_fork(apr_proc_t *proc , apr_pool_t *pool ) ;
apr_status_t apr_setup_signal_thread(void) ;
apr_status_t apr_signal_thread(int (*signal_handler)(int signum ) ) ;
apr_pool_t *apr_thread_pool_get(apr_thread_t const   *ob ) ;
apr_status_t ap_register_provider(apr_pool_t *pool , char const   *provider_group ,
                                  char const   *provider_name , char const   *provider_version ,
                                  void const   *provider ) ;
void *ap_lookup_provider(char const   *provider_group , char const   *provider_name ,
                         char const   *provider_version ) ;
unsigned int apr_hash_count(apr_hash_t *ht ) ;
apr_hash_t *apr_hash_overlay(apr_pool_t *p , apr_hash_t const   *overlay , apr_hash_t const   *base ) ;
apr_pool_t *apr_hash_pool_get(apr_hash_t const   *ob ) ;
void apr_register_optional_fn(char const   *szName , apr_opt_fn_t *pfn ) ;
apr_opt_fn_t *apr_retrieve_optional_fn(char const   *szName ) ;
int ap_allow_overrides(request_rec *r ) ;
void ap_custom_response(request_rec *r , int status , char const   *string ) ;
int ap_core_translate(request_rec *r ) ;
char const   *ap_auth_type(request_rec *r ) ;
char const   *ap_auth_name(request_rec *r ) ;
apr_size_t ap_register_request_note(void) ;
void **ap_get_request_note(request_rec *r , apr_size_t note_num ) ;
void ap_add_per_dir_conf(server_rec *s , void *dir_config ) ;
void ap_add_per_url_conf(server_rec *s , void *url_config ) ;
void ap_add_file_conf(core_dir_config *conf , void *url_config ) ;
char const   *ap_limit_section(cmd_parms *cmd , void *dummy , char const   *arg ) ;
void ap_hook_get_mgmt_items(ap_HOOK_get_mgmt_items_t *pf , char const   * const  *aszPre___1 ,
                            char const   * const  *aszSucc___2 , int nOrder ) ;
int ap_run_get_mgmt_items(apr_pool_t *p , char const   *val , apr_hash_t *ht ) ;
apr_array_header_t *ap_hook_get_get_mgmt_items(void) ;
apr_status_t apr_proc_mutex_trylock(apr_proc_mutex_t *mutex ) ;
apr_status_t apr_proc_mutex_destroy(apr_proc_mutex_t *mutex ) ;
apr_status_t apr_proc_mutex_cleanup(void *mutex ) ;
apr_pool_t *apr_proc_mutex_pool_get(apr_proc_mutex_t const   *ob ) ;
apr_status_t apr_global_mutex_create(apr_global_mutex_t **mutex , char const   *fname ,
                                     apr_lockmech_e mech , apr_pool_t *pool ) ;
apr_status_t apr_global_mutex_child_init(apr_global_mutex_t **mutex , char const   *fname ,
                                         apr_pool_t *pool ) ;
apr_status_t apr_global_mutex_lock(apr_global_mutex_t *mutex ) ;
apr_status_t apr_global_mutex_trylock(apr_global_mutex_t *mutex ) ;
apr_status_t apr_global_mutex_unlock(apr_global_mutex_t *mutex ) ;
apr_status_t apr_global_mutex_destroy(apr_global_mutex_t *mutex ) ;
apr_pool_t *apr_global_mutex_pool_get(apr_global_mutex_t const   *ob ) ;
apr_status_t apr_dso_unload(apr_dso_handle_t *handle ) ;
apr_status_t apr_shm_create(apr_shm_t **m , apr_size_t reqsize , char const   *filename ,
                            apr_pool_t *pool ) ;
apr_status_t apr_shm_destroy(apr_shm_t *m ) ;
apr_status_t apr_shm_attach(apr_shm_t **m , char const   *filename , apr_pool_t *pool ) ;
apr_status_t apr_shm_detach(apr_shm_t *m ) ;
void *apr_shm_baseaddr_get(apr_shm_t const   *m ) ;
apr_size_t apr_shm_size_get(apr_shm_t const   *m ) ;
apr_pool_t *apr_shm_pool_get(apr_shm_t const   *ob ) ;
apr_status_t apr_os_global_mutex_get(apr_os_global_mutex_t *ospmutex , apr_global_mutex_t *pmutex ) ;
apr_status_t apr_os_file_get(apr_os_file_t *thefile , apr_file_t *file ) ;
apr_status_t apr_os_dir_get(apr_os_dir_t **thedir , apr_dir_t *dir ) ;
apr_status_t apr_os_sock_get(apr_os_sock_t *thesock , apr_socket_t *sock ) ;
apr_status_t apr_os_proc_mutex_get(apr_os_proc_mutex_t *ospmutex , apr_proc_mutex_t *pmutex ) ;
apr_status_t apr_os_exp_time_get(apr_os_exp_time_t **ostime , apr_time_exp_t *aprtime ) ;
apr_status_t apr_os_imp_time_get(apr_os_imp_time_t **ostime , apr_time_t *aprtime ) ;
apr_status_t apr_os_shm_get(apr_os_shm_t *osshm , apr_shm_t *shm ) ;
apr_status_t apr_os_thread_get(apr_os_thread_t **thethd , apr_thread_t *thd ) ;
apr_status_t apr_os_threadkey_get(apr_os_threadkey_t *thekey , apr_threadkey_t *key ) ;
apr_status_t apr_os_thread_put(apr_thread_t **thd , apr_os_thread_t *thethd , apr_pool_t *pool ) ;
apr_status_t apr_os_threadkey_put(apr_threadkey_t **key , apr_os_threadkey_t *thekey ,
                                  apr_pool_t *pool ) ;
apr_os_thread_t apr_os_thread_current(void) ;
int apr_os_thread_equal(apr_os_thread_t tid1 , apr_os_thread_t tid2 ) ;
apr_status_t apr_os_file_put(apr_file_t **file , apr_os_file_t *thefile , apr_int32_t flags ,
                             apr_pool_t *pool ) ;
apr_status_t apr_os_pipe_put(apr_file_t **file , apr_os_file_t *thefile , apr_pool_t *pool ) ;
apr_status_t apr_os_dir_put(apr_dir_t **dir , apr_os_dir_t *thedir , apr_pool_t *pool ) ;
apr_status_t apr_os_sock_put(apr_socket_t **sock , apr_os_sock_t *thesock , apr_pool_t *cont ) ;
apr_status_t apr_os_sock_make(apr_socket_t **apr_sock , apr_os_sock_info_t *os_sock_info ,
                              apr_pool_t *cont ) ;
apr_status_t apr_os_proc_mutex_put(apr_proc_mutex_t **pmutex , apr_os_proc_mutex_t *ospmutex ,
                                   apr_pool_t *pool ) ;
apr_status_t apr_os_imp_time_put(apr_time_t *aprtime , apr_os_imp_time_t **ostime ,
                                 apr_pool_t *cont ) ;
apr_status_t apr_os_exp_time_put(apr_time_exp_t *aprtime , apr_os_exp_time_t **ostime ,
                                 apr_pool_t *cont ) ;
apr_status_t apr_os_shm_put(apr_shm_t **m , apr_os_shm_t *osshm , apr_pool_t *pool ) ;
apr_status_t apr_os_dso_handle_put(apr_dso_handle_t **aprdso , apr_os_dso_handle_t osdso ,
                                   apr_pool_t *pool ) ;
apr_status_t apr_os_dso_handle_get(apr_os_dso_handle_t *osdso , apr_dso_handle_t *aprdso ) ;
char const   *apr_os_default_encoding(apr_pool_t *pool ) ;
char const   *apr_os_locale_encoding(apr_pool_t *pool ) ;
void ap_get_mime_headers_core(request_rec *r , apr_bucket_brigade *bb ) ;
apr_time_t ap_rationalize_mtime(request_rec *r , apr_time_t mtime ) ;
void ap_setup_make_content_type(apr_pool_t *pool ) ;
apr_status_t ap_send_fd(apr_file_t *fd , request_rec *r , apr_off_t offset , apr_size_t len ,
                        apr_size_t *nbytes ) ;
size_t ap_send_mmap(apr_mmap_t *mm , request_rec *r , size_t offset , size_t length ) ;
int ap_rwrite(void const   *buf , int nbyte , request_rec *r ) ;
int ap_vrprintf(request_rec *r , char const   *fmt , va_list va ) ;
int ap_rflush(request_rec *r ) ;
void ap_note_auth_failure(request_rec *r ) ;
void ap_note_digest_auth_failure(request_rec *r ) ;
int ap_getline(char *s , int n , request_rec *r , int fold ) ;
apr_status_t ap_rgetline_core(char **s , apr_size_t n , apr_size_t *read___0 , request_rec *r ,
                              int fold , apr_bucket_brigade *bb ) ;
apr_array_header_t *ap_hook_get_post_read_request(void) ;
apr_array_header_t *ap_hook_get_log_transaction(void) ;
char const   *ap_run_http_method(request_rec const   *r ) ;
apr_array_header_t *ap_hook_get_http_method(void) ;
apr_array_header_t *ap_hook_get_default_port(void) ;
apr_bucket *ap_bucket_error_make(apr_bucket *b , int error , char const   *buf , apr_pool_t *p ) ;
apr_status_t ap_content_length_filter(ap_filter_t *f , apr_bucket_brigade *b ) ;
apr_status_t ap_old_write_filter(ap_filter_t *f , apr_bucket_brigade *bb ) ;
request_rec *ap_sub_req_method_uri(char const   *method , char const   *new_file ,
                                   request_rec const   *r , ap_filter_t *next_filter ) ;
apr_status_t ap_sub_req_output_filter(ap_filter_t *f , apr_bucket_brigade *bb ) ;
int ap_is_initial_req(request_rec *r ) ;
apr_array_header_t *ap_hook_get_create_request(void) ;
int ap_run_translate_name(request_rec *r ) ;
apr_array_header_t *ap_hook_get_translate_name(void) ;
int ap_run_map_to_storage(request_rec *r ) ;
apr_array_header_t *ap_hook_get_map_to_storage(void) ;
int ap_run_check_user_id(request_rec *r ) ;
apr_array_header_t *ap_hook_get_check_user_id(void) ;
int ap_run_fixups(request_rec *r ) ;
apr_array_header_t *ap_hook_get_fixups(void) ;
int ap_run_type_checker(request_rec *r ) ;
apr_array_header_t *ap_hook_get_type_checker(void) ;
int ap_run_access_checker(request_rec *r ) ;
apr_array_header_t *ap_hook_get_access_checker(void) ;
int ap_run_auth_checker(request_rec *r ) ;
apr_array_header_t *ap_hook_get_auth_checker(void) ;
void ap_hook_insert_filter(ap_HOOK_insert_filter_t *pf , char const   * const  *aszPre___1 ,
                           char const   * const  *aszSucc___2 , int nOrder ) ;
apr_array_header_t *ap_hook_get_insert_filter(void) ;
int ap_location_walk(request_rec *r ) ;
int ap_directory_walk(request_rec *r ) ;
int ap_file_walk(request_rec *r ) ;
int ap_calc_scoreboard_size(void) ;
global_score *ap_get_scoreboard_global(void) ;
void ap_hook_pre_mpm(ap_HOOK_pre_mpm_t *pf , char const   * const  *aszPre___1 , char const   * const  *aszSucc___2 ,
                     int nOrder ) ;
apr_array_header_t *ap_hook_get_pre_mpm(void) ;
ap_unix_identity_t *ap_run_get_suexec_identity(request_rec const   *r ) ;
apr_array_header_t *ap_hook_get_get_suexec_identity(void) ;
void unixd_set_rlimit(cmd_parms *cmd , struct rlimit **plimit , char const   *arg ,
                      char const   *arg2 , int type ) ;
apr_status_t unixd_set_global_mutex_perms(apr_global_mutex_t *gmutex ) ;
apr_status_t apr_xlate_open(apr_xlate_t **convset , char const   *topage , char const   *frompage ,
                            apr_pool_t *pool ) ;
apr_status_t apr_xlate_sb_get(apr_xlate_t *convset , int *onoff ) ;
apr_status_t apr_xlate_get_sb(apr_xlate_t *convset , int *onoff ) ;
apr_status_t apr_xlate_conv_buffer(apr_xlate_t *convset , char const   *inbuf , apr_size_t *inbytes_left ,
                                   char *outbuf , apr_size_t *outbytes_left ) ;
apr_int32_t apr_xlate_conv_byte(apr_xlate_t *convset , unsigned char inchar ) ;
apr_status_t apr_xlate_close(apr_xlate_t *convset ) ;
apr_status_t apr_md5_set_xlate(apr_md5_ctx_t *context , apr_xlate_t *xlate ) ;
apr_status_t apr_md5(unsigned char *digest , unsigned char const   *input , apr_size_t inputLen ) ;
apr_status_t apr_md5_encode(char const   *pw , char const   *salt , char *result ,
                            apr_size_t nbytes ) ;
void apr_text_append(apr_pool_t *p , apr_text_header *hdr , char const   *text ) ;
apr_status_t apr_xml_parse_file(apr_pool_t *p , apr_xml_parser **parser , apr_xml_doc **ppdoc ,
                                apr_file_t *xmlfd , apr_size_t buffer_length ) ;
void apr_xml_to_text(apr_pool_t *p , apr_xml_elem const   *elem , int style , apr_array_header_t *namespaces ,
                     int *ns_map , char const   **pbuf , apr_size_t *psize ) ;
char const   *apr_xml_empty_elem(apr_pool_t *p , apr_xml_elem const   *elem ) ;
char const   *apr_xml_quote_string(apr_pool_t *p , char const   *s , int quotes ) ;
void apr_xml_quote_elem(apr_pool_t *p , apr_xml_elem *elem ) ;
int apr_xml_insert_uri(apr_array_header_t *uri_array , char const   *uri ) ;
int apr_is_fnmatch(char const   *pattern ) ;
apr_status_t apr_getopt_long(apr_getopt_t *os , apr_getopt_option_t const   *opts ,
                             int *optch , char const   **optarg ) ;
char const   *apr_filepath_name_get(char const   *pathname ) ;
int apr_vformatter(int (*flush_func)(apr_vformatter_buff_t *b ) , apr_vformatter_buff_t *vbuff ,
                   char const   *fmt , va_list ap ) ;
apr_status_t apr_password_get(char const   *prompt , char *pwbuf , apr_size_t *bufsiz ) ;
apr_status_t apr_poll_setup(apr_pollfd_t **new , apr_int32_t num , apr_pool_t *cont ) ;
apr_status_t apr_poll_socket_add(apr_pollfd_t *aprset , apr_socket_t *sock , apr_int16_t event ) ;
apr_status_t apr_poll_socket_mask(apr_pollfd_t *aprset , apr_socket_t *sock , apr_int16_t events ) ;
apr_status_t apr_poll_socket_remove(apr_pollfd_t *aprset , apr_socket_t *sock ) ;
apr_status_t apr_poll_socket_clear(apr_pollfd_t *aprset , apr_int16_t events ) ;
apr_status_t apr_poll_revents_get(apr_int16_t *event , apr_socket_t *sock , apr_pollfd_t *aprset ) ;
apr_status_t apr_pollset_create(apr_pollset_t **pollset , apr_uint32_t size , apr_pool_t *p ,
                                apr_uint32_t flags ) ;
apr_status_t apr_pollset_destroy(apr_pollset_t *pollset ) ;
apr_status_t apr_pollset_add(apr_pollset_t *pollset , apr_pollfd_t const   *descriptor ) ;
apr_status_t apr_pollset_remove(apr_pollset_t *pollset , apr_pollfd_t const   *descriptor ) ;
apr_status_t apr_pollset_poll(apr_pollset_t *pollset , apr_interval_time_t timeout ,
                              apr_int32_t *num , apr_pollfd_t const   **descriptors ) ;
char const   *apr_signal_get_description(int signum ) ;
char *apr_pstrcatv(apr_pool_t *a , struct iovec  const  *vec , apr_size_t nvec , apr_size_t *nbytes ) ;
char *apr_collapse_spaces(char *dest , char const   *src ) ;
char *apr_strtok(char *str , char const   *sep , char **last ) ;
char *apr_ltoa(apr_pool_t *p , long n ) ;
apr_int64_t apr_strtoi64(char const   *nptr , char **endptr , int base ) ;
apr_status_t apr_thread_cond_create(apr_thread_cond_t **cond , apr_pool_t *pool ) ;
apr_status_t apr_thread_cond_wait(apr_thread_cond_t *cond , apr_thread_mutex_t *mutex ) ;
apr_status_t apr_thread_cond_timedwait(apr_thread_cond_t *cond , apr_thread_mutex_t *mutex ,
                                       apr_interval_time_t timeout ) ;
apr_status_t apr_thread_cond_signal(apr_thread_cond_t *cond ) ;
apr_status_t apr_thread_cond_broadcast(apr_thread_cond_t *cond ) ;
apr_status_t apr_thread_cond_destroy(apr_thread_cond_t *cond ) ;
apr_pool_t *apr_thread_cond_pool_get(apr_thread_cond_t const   *ob ) ;
apr_status_t apr_thread_rwlock_create(apr_thread_rwlock_t **rwlock , apr_pool_t *pool ) ;
apr_status_t apr_thread_rwlock_rdlock(apr_thread_rwlock_t *rwlock ) ;
apr_status_t apr_thread_rwlock_tryrdlock(apr_thread_rwlock_t *rwlock ) ;
apr_status_t apr_thread_rwlock_wrlock(apr_thread_rwlock_t *rwlock ) ;
apr_status_t apr_thread_rwlock_trywrlock(apr_thread_rwlock_t *rwlock ) ;
apr_status_t apr_thread_rwlock_unlock(apr_thread_rwlock_t *rwlock ) ;
apr_status_t apr_thread_rwlock_destroy(apr_thread_rwlock_t *rwlock ) ;
apr_pool_t *apr_thread_rwlock_pool_get(apr_thread_rwlock_t const   *ob ) ;
void apr_version(apr_version_t *pvsn ) ;
char const   *apr_version_string(void) ;
int apr_base64_encode_binary(char *encoded , unsigned char const   *string , int len ) ;
int apr_base64_decode_binary(unsigned char *bufplain , char const   *bufcoded ) ;
int apr_date_checkmask(char const   *data , char const   *mask ) ;
apr_time_t apr_date_parse_rfc(char const   *date ) ;
apr_status_t apr_dbm_open_ex(apr_dbm_t **pdb , char const   *type , char const   *pathname ,
                             apr_int32_t mode , apr_fileperms_t perm , apr_pool_t *pool ) ;
apr_status_t apr_dbm_open(apr_dbm_t **pdb , char const   *pathname , apr_int32_t mode ,
                          apr_fileperms_t perm , apr_pool_t *pool ) ;
void apr_dbm_close(apr_dbm_t *dbm ) ;
apr_status_t apr_dbm_fetch(apr_dbm_t *dbm , apr_datum_t key , apr_datum_t *pvalue ) ;
apr_status_t apr_dbm_store(apr_dbm_t *dbm , apr_datum_t key , apr_datum_t value ) ;
apr_status_t apr_dbm_delete(apr_dbm_t *dbm , apr_datum_t key ) ;
int apr_dbm_exists(apr_dbm_t *dbm , apr_datum_t key ) ;
apr_status_t apr_dbm_firstkey(apr_dbm_t *dbm , apr_datum_t *pkey ) ;
apr_status_t apr_dbm_nextkey(apr_dbm_t *dbm , apr_datum_t *pkey ) ;
void apr_dbm_freedatum(apr_dbm_t *dbm , apr_datum_t data ) ;
char *apr_dbm_geterror(apr_dbm_t *dbm , int *errcode , char *errbuf , apr_size_t errbufsize ) ;
apr_status_t apr_dbm_get_usednames_ex(apr_pool_t *p , char const   *type , char const   *pathname ,
                                      char const   **used1 , char const   **used2 ) ;
void apr_dbm_get_usednames(apr_pool_t *p , char const   *pathname , char const   **used1 ,
                           char const   **used2 ) ;
apr_status_t apr_md4_init(apr_md4_ctx_t *context ) ;
apr_status_t apr_md4_set_xlate(apr_md4_ctx_t *context , apr_xlate_t *xlate ) ;
apr_status_t apr_md4_update(apr_md4_ctx_t *context , unsigned char const   *input ,
                            apr_size_t inputLen ) ;
apr_status_t apr_md4_final(unsigned char *digest , apr_md4_ctx_t *context ) ;
apr_status_t apr_md4(unsigned char *digest , unsigned char const   *input , apr_size_t inputLen ) ;
apr_status_t apr_queue_create(apr_queue_t **q , unsigned int queue_capacity , apr_pool_t *a ) ;
apr_status_t apr_queue_push(apr_queue_t *queue , void *data ) ;
apr_status_t apr_queue_pop(apr_queue_t *queue , void **data ) ;
apr_status_t apr_queue_trypush(apr_queue_t *queue , void *data ) ;
apr_status_t apr_queue_trypop(apr_queue_t *queue , void **data ) ;
unsigned int apr_queue_size(apr_queue_t *queue ) ;
apr_status_t apr_queue_interrupt_all(apr_queue_t *queue ) ;
apr_status_t apr_queue_term(apr_queue_t *queue ) ;
apr_status_t apr_reslist_create(apr_reslist_t **reslist , int min , int smax , int hmax ,
                                apr_interval_time_t ttl , apr_status_t (*con)(void **resource ,
                                                                              void *params ,
                                                                              apr_pool_t *pool ) ,
                                apr_status_t (*de)(void *resource , void *params ,
                                                   apr_pool_t *pool ) , void *params ,
                                apr_pool_t *pool ) ;
apr_status_t apr_reslist_destroy(apr_reslist_t *reslist ) ;
apr_status_t apr_reslist_acquire(apr_reslist_t *reslist , void **resource ) ;
apr_status_t apr_reslist_release(apr_reslist_t *reslist , void *resource ) ;
apr_status_t apr_rmm_init(apr_rmm_t **rmm , apr_anylock_t *lock , void *base , apr_size_t size ,
                          apr_pool_t *p ) ;
apr_status_t apr_rmm_destroy(apr_rmm_t *rmm ) ;
apr_status_t apr_rmm_attach(apr_rmm_t **rmm , apr_anylock_t *lock , void *base , apr_pool_t *p ) ;
apr_status_t apr_rmm_detach(apr_rmm_t *rmm ) ;
apr_rmm_off_t apr_rmm_malloc(apr_rmm_t *rmm , apr_size_t reqsize ) ;
apr_rmm_off_t apr_rmm_realloc(apr_rmm_t *rmm , void *entity , apr_size_t reqsize ) ;
apr_rmm_off_t apr_rmm_calloc(apr_rmm_t *rmm , apr_size_t reqsize ) ;
apr_status_t apr_rmm_free(apr_rmm_t *rmm , apr_rmm_off_t this ) ;
void *apr_rmm_addr_get(apr_rmm_t *rmm , apr_rmm_off_t entity ) ;
apr_rmm_off_t apr_rmm_offset_get(apr_rmm_t *rmm , void *entity ) ;
apr_size_t apr_rmm_overhead_get(int n ) ;
apr_status_t apr_sdbm_open(apr_sdbm_t **db , char const   *file , apr_int32_t flags ,
                           apr_fileperms_t perms , apr_pool_t *p ) ;
apr_status_t apr_sdbm_close(apr_sdbm_t *db ) ;
apr_status_t apr_sdbm_lock(apr_sdbm_t *db , int type ) ;
apr_status_t apr_sdbm_unlock(apr_sdbm_t *db ) ;
apr_status_t apr_sdbm_fetch(apr_sdbm_t *db , apr_sdbm_datum_t *val , apr_sdbm_datum_t key ) ;
apr_status_t apr_sdbm_store(apr_sdbm_t *db , apr_sdbm_datum_t key , apr_sdbm_datum_t val ,
                            int flags ) ;
apr_status_t apr_sdbm_delete(apr_sdbm_t *db , apr_sdbm_datum_t key ) ;
apr_status_t apr_sdbm_firstkey(apr_sdbm_t *db , apr_sdbm_datum_t *key ) ;
apr_status_t apr_sdbm_nextkey(apr_sdbm_t *db , apr_sdbm_datum_t *key ) ;
int apr_sdbm_rdonly(apr_sdbm_t *db ) ;
void apr_sha1_base64(char const   *clear , int len , char *out ) ;
void apr_sha1_init(apr_sha1_ctx_t *sha_info ) ;
void apr_sha1_update(apr_sha1_ctx_t *sha_info , char const   *buf , unsigned int count ) ;
void apr_sha1_update_binary(apr_sha1_ctx_t *sha_info , unsigned char const   *buffer ,
                            unsigned int count ) ;
void apr_sha1_final(unsigned char *digest , apr_sha1_ctx_t *sha_info ) ;
void apr_uuid_get(apr_uuid_t *uuid ) ;
void apr_uuid_format(char *buffer , apr_uuid_t const   *uuid ) ;
apr_status_t apr_uuid_parse(apr_uuid_t *uuid , char const   *uuid_str ) ;
void apu_version(apr_version_t *pvsn ) ;
char const   *apu_version_string(void) ;



scoreboard *ap_scoreboard_image ;
static int server_limit___1  ;
static int thread_limit___0  ;
static apr_size_t scoreboard_size  ;



//////// Simplified func

apr_pool_t *get_pool_ptr() {
  return 0;
}

apr_status_t get_status () {
  return 0;
}

char *apr_strerror(apr_status_t statcode , char *buf , apr_size_t bufsize ) 
{ 
  *buf = 'a';

  return buf;
}


static apr_status_t limit_proc(apr_procattr_t *attr ) 
{

  if ((unsigned int )attr->limit_cpu != (unsigned int )((void *)0)) {
    return 1;
  }
  if ((unsigned int )attr->limit_nproc != (unsigned int )((void *)0)) {
    return 2;
  }
  if ((unsigned int )attr->limit_nofile != (unsigned int )((void *)0)) {
    return 3;
  }
  if ((unsigned int )attr->limit_mem != (unsigned int )((void *)0)) {
    return 4;
  }
  return (0);

}


int ap_default_loglevel  =    4;

static void log_error_core(char const   *file , int line , int level , 
                           apr_status_t status ,
                           server_rec const *s , request_rec const *r , 
                           apr_pool_t *pool ,
                           char const   *fmt , va_list args ) 
{ char errstr[8192] ;
  apr_size_t len ;
  apr_size_t errstrlen ;
  apr_file_t *logf ;
  char const   *referer ;
  int level_and_mask ;

  logf = (apr_file_t *)((void *)0);
  level_and_mask = level & 7;
  if (level_and_mask > ap_default_loglevel) {
    return;
  } else {
    goto _L;
  }
  _L:  logf = 0;//stderr_log;
  if (level_and_mask > s->loglevel) {
    return;
  } else {
    goto _L___0;
  }
  _L___0: ;
  
  logf = s->error_log;
  if (level_and_mask > s->loglevel) {
    return;
  }
  return;
}


static apr_status_t thread_mutex_cleanup(void *data ) 
{ apr_thread_mutex_t *mutex ;
  apr_status_t rv ;

  {
  mutex = (apr_thread_mutex_t *)data;
  rv = pthread_mutex_destroy(& mutex->mutex);
  return (rv);
}
}


apr_status_t apr_thread_mutex_lock(apr_thread_mutex_t *mutex ) 
{ apr_status_t rv ;
  rv = pthread_mutex_lock(& mutex->mutex);
  return rv;
}


apr_status_t apr_thread_mutex_unlock(apr_thread_mutex_t *mutex ) 
{ apr_status_t status ;
  status = pthread_mutex_unlock(& mutex->mutex);
  return (status);
  
}

apr_status_t apr_thread_mutex_create(apr_thread_mutex_t **mutex , unsigned int flags ,
                                     apr_pool_t *pool ) 
{ apr_thread_mutex_t *new_mutex ;
  pthread_mutexattr_t mattr ;
  apr_status_t rv ;
  void *tmp ;

  {
    tmp = apr_palloc(pool, sizeof(apr_thread_mutex_t ));
    new_mutex = (apr_thread_mutex_t *)memset(tmp, 0, sizeof(apr_thread_mutex_t ));
    if ((unsigned int )new_mutex == (unsigned int )((void *)0)) {
      return (12);
    }
    new_mutex->pool = pool;
    new_mutex->nested = (char )(flags & 1U);
    rv = get_status();//pthread_mutexattr_init(& mattr);
    if (rv) {
      thread_mutex_cleanup((void *)new_mutex);
      return (rv);
    }
    rv = pthread_mutex_init((pthread_mutex_t * __restrict  )(& new_mutex->mutex), (pthread_mutexattr_t const   * __restrict  )(& mattr));
    if (rv) {
      thread_mutex_cleanup((void *)new_mutex);
      return (rv);
    }
    rv = get_status();//pthread_mutexattr_destroy(& mattr);
    if (rv) {
      thread_mutex_cleanup((void *)new_mutex);
      return (rv);
    }
    (*mutex) = new_mutex;
    return (0);
  }
}


/////////// PRE-REQ FUNCS

ap_filter_rec_t *ap_old_write_func ;
static char *server_version  =    (char *)((void *)0);
static int version_locked  =    0;
static enum server_token_type ap_server_tokens  =    (enum server_token_type )4;


char const   *apr_table_get(apr_table_t const   *t , char const   *key ) 
{ apr_table_entry_t *next_elt ;
  apr_table_entry_t *end_elt ;
  apr_uint32_t checksum ;
  int hash ;
  char const   *k ;
  apr_uint32_t c ;
  int tmp ;

  {
  if ((unsigned int )key == (unsigned int )((void *)0)) {
    return ((char const   *)((void *)0));
  }
  hash = 31 & (int )(*((unsigned char *)key));
  if (! (t->index_initialized & (unsigned int )(1 << hash))) {
    return ((char const   *)((void *)0));
  }
  k = key;
  c = (unsigned int )(*k);
  checksum = c;
  checksum <<= 8;
  if (c) {
    k ++;
    c = (unsigned int )(*k);
    checksum |= c;
  }
  checksum <<= 8;
  if (c) {
    k ++;
    c = (unsigned int )(*k);
    checksum |= c;
  }
  checksum <<= 8;
  if (c) {
    k ++;
    c = (unsigned int )(*k);
    checksum |= c;
  }
  checksum &= 3755991007U;
  next_elt = (apr_table_entry_t *)t->a.elts + t->index_first[hash];
  end_elt = (apr_table_entry_t *)t->a.elts + t->index_last[hash];
  while ((unsigned int )next_elt <= (unsigned int )end_elt) {
    if (checksum == next_elt->key_checksum) {
      tmp = strcasecmp((char const   *)next_elt->key, key);
      if (tmp) {
        goto _L;
      } else {
        return ((char const   *)next_elt->val);
      }
    } else {
      _L: ;
    }
    next_elt ++;
  }
  return ((char const   *)((void *)0));
}
}

static void table_reindex(apr_table_t *t ) 
{ int i ;
  int hash ;
  apr_table_entry_t *next_elt ;

  {
  next_elt = (apr_table_entry_t *)t->a.elts;
  t->index_initialized = 0U;
  i = 0;
  while (i < t->a.nelts) {
    hash = 31 & (int )(*((unsigned char *)next_elt->key));
    t->index_last[hash] = i;
    if (! (t->index_initialized & (unsigned int )(1 << hash))) {
      t->index_first[hash] = i;
      t->index_initialized |= (unsigned int )(1 << hash);
    }
    i ++;
    next_elt ++;
  }
  return;
}
}

void apr_table_setn(apr_table_t *t , char const   *key , char const   *val ) 
{ apr_table_entry_t *next_elt ;
  apr_table_entry_t *end_elt ;
  apr_table_entry_t *table_end ;
  apr_uint32_t checksum ;
  int hash ;
  char const   *k ;
  apr_uint32_t c ;
  int must_reindex ;
  apr_table_entry_t *dst_elt ;
  apr_table_entry_t *tmp ;
  int tmp___0 ;
  apr_table_entry_t *tmp___1 ;
  int tmp___2 ;

  {
  k = key;
  c = (unsigned int )(*k);
  checksum = c;
  checksum <<= 8;
  if (c) {
    k ++;
    c = (unsigned int )(*k);
    checksum |= c;
  }
  checksum <<= 8;
  if (c) {
    k ++;
    c = (unsigned int )(*k);
    checksum |= c;
  }
  checksum <<= 8;
  if (c) {
    k ++;
    c = (unsigned int )(*k);
    checksum |= c;
  }
  checksum &= 3755991007U;
  hash = 31 & (int )(*((unsigned char *)key));
  if (! (t->index_initialized & (unsigned int )(1 << hash))) {
    t->index_first[hash] = t->a.nelts;
    t->index_initialized |= (unsigned int )(1 << hash);
    goto add_new_elt;
  }
  next_elt = (apr_table_entry_t *)t->a.elts + t->index_first[hash];
  end_elt = (apr_table_entry_t *)t->a.elts + t->index_last[hash];
  table_end = (apr_table_entry_t *)t->a.elts + t->a.nelts;
  while ((unsigned int )next_elt <= (unsigned int )end_elt) {
    if (checksum == next_elt->key_checksum) {
      tmp___2 = strcasecmp((char const   *)next_elt->key, key);
      if (tmp___2) {
        goto _L___0;
      } else {
        must_reindex = 0;
        dst_elt = (apr_table_entry_t *)((void *)0);
        next_elt->val = (char *)val;
        next_elt ++;
        while ((unsigned int )next_elt <= (unsigned int )end_elt) {
          if (checksum == next_elt->key_checksum) {
            tmp___0 = strcasecmp((char const   *)next_elt->key, key);
            if (tmp___0) {
              goto _L;
            } else {
              t->a.nelts = t->a.nelts - 1;
              if (! dst_elt) {
                dst_elt = next_elt;
              }
            }
          } else {
            _L: 
            if (dst_elt) {
              tmp = dst_elt;
              dst_elt ++;
              (*tmp) = (*next_elt);
              must_reindex = 1;
            }
          }
          next_elt ++;
        }
        if (dst_elt) {
          while ((unsigned int )next_elt < (unsigned int )table_end) {
            tmp___1 = dst_elt;
            dst_elt ++;
            (*tmp___1) = (*next_elt);
            next_elt ++;
          }
          must_reindex = 1;
        }
        if (must_reindex) {
          table_reindex(t);
        }
        return;
      }
    } else {
      _L___0: ;
    }
    next_elt ++;
  }
  add_new_elt: 
  t->index_last[hash] = t->a.nelts;
  next_elt = (apr_table_entry_t *)apr_array_push_noclear(& t->a);
  next_elt->key = (char *)key;
  next_elt->val = (char *)val;
  next_elt->key_checksum = checksum;
  return;
}
}


static int use_range_x(request_rec *r ) 
{ char const   *ua ;
  char const   *tmp ;
  char *tmp___0 ;
  int tmp___1 ;

  {
  tmp = apr_table_get((apr_table_t const   *)r->headers_in, "Request-Range");
  if (tmp) {
    goto _L;
  } else {
    ua = apr_table_get((apr_table_t const   *)r->headers_in, "User-Agent");
    if (ua) {
      tmp___0 = strstr(ua, "MSIE 3");
      if (tmp___0) {
        _L: 
        tmp___1 = 1;
      } else {
        goto _L___0;
      }
    } else {
      _L___0: 
      tmp___1 = 0;
    }
  }
  return (tmp___1);
}
}


char const   *ap_get_server_version(void) 
{ char const   *tmp ;

  {
  if (server_version) {
    tmp = (char const   *)server_version;
  } else {
    tmp = "Apache/2.0.44";
  }
  return (tmp);
}
}

static char const   server_built[21]  = 
  {      (char const   )'M',      (char const   )'a',      (char const   )'r',      (char const   )' ', 
        (char const   )'1',      (char const   )'0',      (char const   )' ',      (char const   )'2', 
        (char const   )'0',      (char const   )'0',      (char const   )'6',      (char const   )' ', 
        (char const   )'1',      (char const   )'5',      (char const   )':',      (char const   )'4', 
        (char const   )'2',      (char const   )':',      (char const   )'1',      (char const   )'3', 
        (char const   )'\000'};

char const   *ap_get_server_built(void) 
{ 

  {
  return (server_built);
}
}


static apr_status_t eos_bucket_read(apr_bucket *b , char const   **str , apr_size_t *len ,
                                    apr_read_type_e block ) 
{ 

  {
  (*str) = (char const   *)((void *)0);
  (*len) = 0U;
  return (0);
}
}

void apr_bucket_destroy_noop(void *data ) 
{ 

  {
  return;
}
}

apr_status_t apr_bucket_setaside_noop(apr_bucket *data , apr_pool_t *pool ) 
{ 

  {
  return (0);
}
}

apr_status_t apr_bucket_split_notimpl(apr_bucket *data , apr_size_t point ) 
{ 

  {
  return (70023);
}
}



struct apr_bucket_type_t  const  apr_bucket_type_eos  = 
     {"EOS", 5, (enum __anonenum_is_metadata_61 )1, & apr_bucket_destroy_noop, & eos_bucket_read,
    & apr_bucket_setaside_noop, & apr_bucket_split_notimpl, & apr_bucket_simple_copy};
#pragma merger(0,"/tmp/cil-SpQoWLlv.i","-g -pthread")
struct apr_bucket_type_t  const  apr_bucket_type_pipe ;


apr_status_t ap_pass_brigade(ap_filter_t *next , apr_bucket_brigade *bb ) 
{ apr_bucket *e ;
  apr_status_t tmp ;

  {
  if (next) {
    e = bb->list.prev;
    if (e) {
      if ((unsigned int )e->type == (unsigned int )(& apr_bucket_type_eos)) {
        if (next->r) {
          (next->r)->eos_sent = 1;
        } else {
          goto _L___0;
        }
      } else {
        goto _L___0;
      }
    } else {
      _L___0: ;
    }
    tmp = ((*((next->frec)->filter_func.out_func)))(next, bb);
    return (tmp);
  }
  return (-1);
}
}

apr_status_t ap_filter_flush(apr_bucket_brigade *bb , void *ctx ) 
{ ap_filter_t *f ;
  apr_status_t tmp ;

  {
  f = (ap_filter_t *)ctx;
  tmp = ap_pass_brigade(f, bb);
  return (tmp);
}
}


static apr_status_t buffer_output(request_rec *r , char const   *str , apr_size_t len ) 
{ conn_rec *c ;
  ap_filter_t *f ;
  old_write_filter_ctx *ctx ;
  void *tmp ;
  apr_bucket_brigade *bb ;
  apr_bucket_brigade *tmp___0 ;
  apr_bucket *b ;
  apr_bucket *tmp___1 ;
  apr_bucket *ap__b ;
  apr_status_t tmp___2 ;
  apr_status_t tmp___3 ;

  {
  c = r->connection;
  if (len == 0U) {
    return (0);
  }
  f = r->output_filters;
  while ((unsigned int )f != (unsigned int )((void *)0)) {
    if ((unsigned int )ap_old_write_func == (unsigned int )f->frec) {
      break;
    }
    f = f->next;
  }
  if ((unsigned int )f == (unsigned int )((void *)0)) {
    tmp = apr_palloc(r->pool, sizeof((*ctx)));
    ctx = (old_write_filter_ctx *)memset(tmp, 0, sizeof((*ctx)));
    //ap_add_output_filter("OLD_WRITE", (void *)ctx, r, r->connection);
    f = r->output_filters;
  }
  if ((unsigned int )f != (unsigned int )r->output_filters) {
    tmp___0 = apr_brigade_create(r->pool, c->bucket_alloc);
    bb = tmp___0;
    tmp___1 = 0;//apr_bucket_transient_create(str, len, c->bucket_alloc);
    b = tmp___1;
    while (1) {
      ap__b = b;
      while (1) {
        ap__b->link.next = (struct apr_bucket *)((char *)(& bb->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                (char *)((void *)0)));
        ap__b->link.prev = ((struct apr_bucket *)((char *)(& bb->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                 (char *)((void *)0))))->link.prev;
        (((struct apr_bucket *)((char *)(& bb->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                               (char *)((void *)0))))->link.prev)->link.next = ap__b;
        ((struct apr_bucket *)((char *)(& bb->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                              (char *)((void *)0))))->link.prev = ap__b;
        break;
      }
      break;
    }
    tmp___2 = ap_pass_brigade(r->output_filters, bb);
    return (tmp___2);
  }
  ctx = (old_write_filter_ctx *)(r->output_filters)->ctx;
  if ((unsigned int )ctx->bb == (unsigned int )((void *)0)) {
    ctx->bb = apr_brigade_create(r->pool, c->bucket_alloc);
  }
  tmp___3 = 0;//apr_brigade_write(ctx->bb, & ap_filter_flush, (void *)f->next, str, len);
  return (tmp___3);
}
}


int ap_vrprintf(request_rec *r , char const   *fmt , va_list va ) 
{ apr_size_t written ;
  struct ap_vrprintf_data vd ;
  char vrprintf_buf[8192] ;
  int n ;
  apr_status_t tmp ;

  {
  vd.vbuff.curpos = vrprintf_buf;
  vd.vbuff.endpos = vrprintf_buf + 8192;
  vd.r = r;
  vd.buff = vrprintf_buf;
  if ((r->connection)->aborted) {
    return (-1);
  }
  written = 0;//(apr_size_t )apr_vformatter(& r_flush, & vd.vbuff, fmt, va);
  (*(vd.vbuff.curpos)) = (char )'\000';
  if (written != 4294967295U) {
    n = vd.vbuff.curpos - vrprintf_buf;
    tmp = buffer_output(r, (char const   *)(vrprintf_buf), (unsigned int )n);
    if (tmp != 0) {
      return (-1);
    }
    written += (unsigned int )n;
  }
  return ((int )written);
}
}


int ( /* format attribute */  ap_rprintf)(request_rec *r , char const   *fmt  , ...) 
{ va_list va ;
  int n ;

  {
  if ((r->connection)->aborted) {
    return (-1);
  }
  __builtin_stdarg_start(va, fmt);
  n = ap_vrprintf(r, fmt, va);
  __builtin_va_end(va);
  return (n);
}
}


int ap_rputc(int c , request_rec *r ) 
{ char c2 ;
  apr_status_t tmp ;

  {
  c2 = (char )c;
  if ((r->connection)->aborted) {
    return (-1);
  }
  tmp = buffer_output(r, (char const   *)(& c2), 1U);
  if (tmp != 0) {
    return (-1);
  }
  return (c);
}
}

int ap_rputs(char const   *str , request_rec *r ) 
{ apr_size_t len ;
  apr_status_t tmp ;

  {
  if ((r->connection)->aborted) {
    return (-1);
  }
  len = strlen(str);
  tmp = buffer_output(r, str, len);
  if (tmp != 0) {
    return (-1);
  }
  return ((int )len);
}
}



int ap_rvputs(request_rec *r  , ...) 
{ va_list va ;
  char const   *s ;
  apr_size_t len ;
  apr_size_t written ;
  apr_status_t tmp ;

  {
  written = 0U;
  if ((r->connection)->aborted) {
    return (-1);
  }
  __builtin_stdarg_start(va, r);
  while (1) {
    s = __builtin_va_arg(va, char const   *);
    if ((unsigned int )s == (unsigned int )((void *)0)) {
      break;
    }
    len = strlen(s);
    tmp = buffer_output(r, s, len);
    if (tmp != 0) {
      return (-1);
    }
    written += len;
  }
  __builtin_va_end(va);
  return ((int )written);
}
}

static void format_byte_out(request_rec *r , apr_off_t bytes ) 
{ 

  {
  if (bytes < 5120L) {
    ap_rprintf(r, "%d B", (int )bytes);
  } else {
    if (bytes < 524288L) {
      ap_rprintf(r, "%.1f kB", (float )bytes / (float )1024);
    } else {
      if (bytes < 536870912L) {
        ap_rprintf(r, "%.1f MB", (float )bytes / (float )1048576L);
      } else {
        ap_rprintf(r, "%.1f GB", (float )bytes / (float )1073741824L);
      }
    }
  }
  return;
}
}


char *apr_cpystrn(char *dst , char const   *src , apr_size_t dst_size ) 
{ char *d ;
  char *end ;

  {
  if (dst_size == 0U) {
    return (dst);
  }
  d = dst;
  end = (dst + dst_size) - 1;
  while ((unsigned int )d < (unsigned int )end) {
    (*d) = (char )(*src);
    if (! (*d)) {
      return (d);
    }
    d ++;
    src ++;
  }
  (*d) = (char )'\000';
  return (d);
}
}


int ap_exists_scoreboard_image(void) 
{ int tmp ;

  {
  if (ap_scoreboard_image) {
    tmp = 1;
  } else {
    tmp = 0;
  }
  return (tmp);
}
}


worker_score *ap_get_scoreboard_worker(int x , int y ) 
{ 

  {
  if (x < 0) {
    goto _L___1;
  } else {
    if (server_limit___1 < x) {
      _L___1: 
      goto _L___0;
    } else {
      if (y < 0) {
        goto _L___0;
      } else {
        if (thread_limit___0 < y) {
          _L___0: 
          return ((worker_score *)((void *)0));
        }
      }
    }
  }
  return ((*(ap_scoreboard_image->servers + x)) + y);
}
}
process_score *ap_get_scoreboard_process(int x ) 
{ 

  {
  if (x < 0) {
    goto _L;
  } else {
    if (server_limit___1 < x) {
      _L: 
      return ((process_score *)((void *)0));
    }
  }
  return (ap_scoreboard_image->parent + x);
}
}
global_score *ap_get_scoreboard_global(void) 
{ 

  {
  return (ap_scoreboard_image->global);
}
}


apr_status_t apr_tokenize_to_argv(char const   *arg_str , char ***argv_out , apr_pool_t *token_context ) 
{ char const   *cp ;
  char const   *ct ;
  char *cleaned ;
  char *dirty ;
  int escaped ;
  int isquoted ;
  int numargs ;
  int argnum ;
  char *tmp ;

  {
  numargs = 0;
  cp = arg_str;
  while (1) {
    if ((int const   )(*cp) == 32) {
      goto _L;
    } else {
      if ((int const   )(*cp) == 9) {
        _L: ;
      } else {
        break;
      }
    }
    cp ++;
  }
  ct = cp;
  numargs = 1;
  while ((int const   )(*ct) != 0) {
    isquoted = 0;
    if ((int const   )(*ct) == 34) {
      isquoted = 1;
      ct ++;
    } else {
      if ((int const   )(*ct) == 39) {
        isquoted = 2;
        ct ++;
      }
    }
    while ((int const   )(*ct) != 0) {
      if (isquoted) {
        if ((int const   )(*ct) == 32) {
          goto _L___6;
        } else {
          if ((int const   )(*ct) == 9) {
            _L___6: 
            goto _L___2;
          } else {
            goto _L___5;
          }
        }
      } else {
        _L___5: 
        if ((int const   )(*ct) == 92) {
          if ((int const   )(*(ct + 1)) == 32) {
            goto _L___4;
          } else {
            if ((int const   )(*(ct + 1)) == 9) {
              _L___4: 
              goto _L___3;
            } else {
              if ((int const   )(*(ct + 1)) == 34) {
                _L___3: 
                goto _L___2;
              } else {
                if ((int const   )(*(ct + 1)) == 39) {
                  _L___2: 
                  ct ++;
                  goto __Cont;
                } else {
                  goto _L___1;
                }
              }
            }
          }
        } else {
          _L___1: ;
        }
      }
      if (! isquoted) {
        if ((int const   )(*ct) == 32) {
          goto _L___12;
        } else {
          if ((int const   )(*ct) == 9) {
            _L___12: 
            goto _L___9;
          } else {
            goto _L___11;
          }
        }
      } else {
        _L___11: 
        if (isquoted == 1) {
          if ((int const   )(*ct) == 34) {
            _L___9: 
            goto _L___7;
          } else {
            goto _L___10;
          }
        } else {
          _L___10: 
          if (isquoted == 2) {
            if ((int const   )(*ct) == 39) {
              _L___7: 
              break;
            } else {
              goto _L___8;
            }
          } else {
            _L___8: ;
          }
        }
      }
      __Cont: 
      ct ++;
    }
    if ((int const   )(*ct) != 0) {
      ct ++;
    }
    numargs ++;
    while (1) {
      if ((int const   )(*ct) == 32) {
        goto _L___13;
      } else {
        if ((int const   )(*ct) == 9) {
          _L___13: ;
        } else {
          break;
        }
      }
      ct ++;
    }
  }
  (*argv_out) = (char **)apr_palloc(token_context, (unsigned int )numargs * sizeof(char *));
  argnum = 0;
  while (argnum < numargs - 1) {
    isquoted = 0;
    if ((int const   )(*cp) == 34) {
      isquoted = 1;
      cp ++;
    } else {
      if ((int const   )(*cp) == 39) {
        isquoted = 2;
        cp ++;
      }
    }
    ct = cp;
    while ((int const   )(*cp) != 0) {
      if (isquoted) {
        if ((int const   )(*cp) == 32) {
          goto _L___20;
        } else {
          if ((int const   )(*cp) == 9) {
            _L___20: 
            goto _L___16;
          } else {
            goto _L___19;
          }
        }
      } else {
        _L___19: 
        if ((int const   )(*cp) == 92) {
          if ((int const   )(*(cp + 1)) == 32) {
            goto _L___18;
          } else {
            if ((int const   )(*(cp + 1)) == 9) {
              _L___18: 
              goto _L___17;
            } else {
              if ((int const   )(*(cp + 1)) == 34) {
                _L___17: 
                goto _L___16;
              } else {
                if ((int const   )(*(cp + 1)) == 39) {
                  _L___16: 
                  cp ++;
                  goto __Cont___0;
                } else {
                  goto _L___15;
                }
              }
            }
          }
        } else {
          _L___15: ;
        }
      }
      if (! isquoted) {
        if ((int const   )(*cp) == 32) {
          goto _L___26;
        } else {
          if ((int const   )(*cp) == 9) {
            _L___26: 
            goto _L___23;
          } else {
            goto _L___25;
          }
        }
      } else {
        _L___25: 
        if (isquoted == 1) {
          if ((int const   )(*cp) == 34) {
            _L___23: 
            goto _L___21;
          } else {
            goto _L___24;
          }
        } else {
          _L___24: 
          if (isquoted == 2) {
            if ((int const   )(*cp) == 39) {
              _L___21: 
              break;
            } else {
              goto _L___22;
            }
          } else {
            _L___22: ;
          }
        }
      }
      __Cont___0: 
      cp ++;
    }
    cp ++;
    (*((*argv_out) + argnum)) = (char *)apr_palloc(token_context, (unsigned int )(cp -
                                                                                  ct));
    apr_cpystrn((*((*argv_out) + argnum)), ct, (unsigned int )(cp - ct));
    dirty = (*((*argv_out) + argnum));
    cleaned = dirty;
    escaped = 0;
    while ((*dirty)) {
      if (! escaped) {
        if ((int )(*dirty) == 92) {
          escaped = 1;
        } else {
          goto _L___27;
        }
      } else {
        _L___27: 
        escaped = 0;
        tmp = cleaned;
        cleaned ++;
        (*tmp) = (*dirty);
      }
      dirty ++;
    }
    (*cleaned) = (char)0;
    while (1) {
      if ((int const   )(*cp) == 32) {
        goto _L___28;
      } else {
        if ((int const   )(*cp) == 9) {
          _L___28: ;
        } else {
          break;
        }
      }
      cp ++;
    }
    argnum ++;
  }
  (*((*argv_out) + argnum)) = (char *)((void *)0);
  return (0);
}
}

char *ap_escape_html(apr_pool_t *p , char const   *s ) 
{ int i ;
  int j ;
  char *x ;
  char *tmp ;

  {
  i = 0;
  j = 0;
  while ((int const   )(*(s + i)) != 0) {
    if ((int const   )(*(s + i)) == 60) {
      goto _L;
    } else {
      if ((int const   )(*(s + i)) == 62) {
        _L: 
        j += 3;
      } else {
        if ((int const   )(*(s + i)) == 38) {
          j += 4;
        }
      }
    }
    i ++;
  }
  if (j == 0) {
    tmp = 0;//apr_pstrmemdup(p, s, (unsigned int )i);
    return (tmp);
  }
  x = (char *)apr_palloc(p, (unsigned int )((i + j) + 1));
  i = 0;
  j = 0;
  while ((int const   )(*(s + i)) != 0) {
    if ((int const   )(*(s + i)) == 60) {
      memcpy((void * __restrict  )(x + j), (void const   * __restrict  )"&lt;", 4U);
      j += 3;
    } else {
      if ((int const   )(*(s + i)) == 62) {
        memcpy((void * __restrict  )(x + j), (void const   * __restrict  )"&gt;",
               4U);
        j += 3;
      } else {
        if ((int const   )(*(s + i)) == 38) {
          memcpy((void * __restrict  )(x + j), (void const   * __restrict  )"&amp;",
                 5U);
          j += 4;
        } else {
          (*(x + j)) = (char )(*(s + i));
        }
      }
    }
    i ++;
    j ++;
  }
  (*(x + j)) = (char )'\000';
  return (x);
}
}



__inline static apr_memnode_t *allocator_alloc(apr_allocator_t *allocator , 
                                               apr_size_t size ) 
{ apr_memnode_t *node ;
  apr_memnode_t **ref ;
  apr_uint32_t i ;
  apr_uint32_t index___0 ;
  apr_uint32_t max_index ;

  {
  size = ((size + ((sizeof(apr_memnode_t ) + 7U) & 4294967288U)) + 4095U) & 4294963200U;
  if (size < 8192U) {
    size = 8192U;
  }
  index___0 = (size >> 12) - 1U;
  if (index___0 <= allocator->max_index) {
    if (allocator->mutex) {
      apr_thread_mutex_lock(allocator->mutex);
    }
    max_index = allocator->max_index;
    ref = & allocator->free[index___0];
    i = index___0;
    while (1) {
      if ((unsigned int )(*ref) == (unsigned int )((void *)0)) {
        if (! (i < max_index)) {
          goto _L;
        }
      } else {
        _L: 
        break;
      }
      ref ++;
      i ++;
    }
    node = (*ref);
    if ((unsigned int )node != (unsigned int )((void *)0)) {
      (*ref) = node->next;
      if ((unsigned int )(*ref) == (unsigned int )((void *)0)) {
        if (i >= max_index) {
          while (1) {
            ref --;
            max_index --;
            if ((unsigned int )(*ref) == (unsigned int )((void *)0)) {
              if (! (max_index > 0U)) {
                goto _L___0;
              }
            } else {
              _L___0: 
              break;
            }
          }
          allocator->max_index = max_index;
        } else {
          goto _L___1;
        }
      } else {
        _L___1: ;
      }
      allocator->current_free_index += node->index;
      if (allocator->current_free_index > allocator->max_free_index) {
        allocator->current_free_index = allocator->max_free_index;
      }
      if (allocator->mutex) {
        apr_thread_mutex_unlock(allocator->mutex);
      }
      node->next = (apr_memnode_t *)((void *)0);
      node->first_avail = (char *)node + ((sizeof(apr_memnode_t ) + 7U) & 4294967288U);
      return (node);
    }
    if (allocator->mutex) {
      apr_thread_mutex_unlock(allocator->mutex);
    }
  } else {
    if (allocator->free[0]) {
      if (allocator->mutex) {
        apr_thread_mutex_lock(allocator->mutex);
      }
      ref = & allocator->free[0];
      while (1) {
        node = (*ref);
        if ((unsigned int )node != (unsigned int )((void *)0)) {
          if (! (index___0 > node->index)) {
            goto _L___2;
          }
        } else {
          _L___2: 
          break;
        }
        ref = & node->next;
      }
      if (node) {
        (*ref) = node->next;
        allocator->current_free_index += node->index;
        if (allocator->current_free_index > allocator->max_free_index) {
          allocator->current_free_index = allocator->max_free_index;
        }
        if (allocator->mutex) {
          apr_thread_mutex_unlock(allocator->mutex);
        }
        node->next = (apr_memnode_t *)((void *)0);
        node->first_avail = (char *)node + ((sizeof(apr_memnode_t ) + 7U) & 4294967288U);
        return (node);
      }
      if (allocator->mutex) {
        apr_thread_mutex_unlock(allocator->mutex);
      }
    }
  }
  node = (apr_memnode_t *)malloc(size);
  if ((unsigned int )node == (unsigned int )((void *)0)) {
    return ((apr_memnode_t *)((void *)0));
  }
  node->next = (apr_memnode_t *)((void *)0);
  node->index = index___0;
  node->first_avail = (char *)node + ((sizeof(apr_memnode_t ) + 7U) & 4294967288U);
  node->endp = (char *)node + size;
  return (node);
}
}


apr_status_t apr_file_flush(apr_file_t *thefile ) 
{ apr_int64_t written ;
  int *tmp ;
  int *tmp___0 ;

  {
  if (thefile->buffered) {
    written = 0LL;
    if (thefile->direction == 1) {
      if (thefile->bufpos) {
        while (1) {
          written = (apr_int64_t )write(thefile->filedes, (void const   *)thefile->buffer,
                                        (unsigned int )thefile->bufpos);
          if (written == -1LL) {
            tmp = __errno_location();
            if (! ((*tmp) == 4)) {
              goto _L;
            }
          } else {
            _L: 
            break;
          }
        }
        if (written == -1LL) {
          tmp___0 = __errno_location();
          return ((*tmp___0));
        }
        thefile->filePtr = (unsigned long )((long long )thefile->filePtr + written);
        thefile->bufpos = 0;
      } else {
        goto _L___0;
      }
    } else {
      _L___0: ;
    }
  }
  return (0);
}
}

apr_memnode_t *apr_allocator_alloc(apr_allocator_t *allocator , apr_size_t size ) 
{ apr_memnode_t *tmp ;

  {
  tmp = allocator_alloc(allocator, size);
  return (tmp);
}
}


apr_status_t apr_unix_file_cleanup(void *thefile ) 
{ apr_file_t *file ;
  apr_status_t flush_rv ;
  apr_status_t rv ;
  int rc ;
  int *tmp ;
  apr_status_t tmp___0 ;

  {
  file = (apr_file_t *)thefile;
  flush_rv = 0;
  rv = 0;
  if (file->buffered) {
    flush_rv = apr_file_flush(file);
  }
  rc = close(file->filedes);
  if (rc == 0) {
    file->filedes = -1;
    if (file->flags & 256) {
      unlink((char const   *)file->fname);
    }
    if (file->thlock) {
      rv = 0;//apr_thread_mutex_destroy(file->thlock);
    }
  } else {
    tmp = __errno_location();
    rv = (*tmp);
  }
  if (rv != 0) {
    tmp___0 = rv;
  } else {
    tmp___0 = flush_rv;
  }
  return (tmp___0);
}
}

void apr_pool_cleanup_kill(apr_pool_t *p , void const   *data , apr_status_t (*cleanup_fn)(void * ) ) 
{ cleanup_t *c ;
  cleanup_t **lastp ;

  {
  if ((unsigned int )p == (unsigned int )((void *)0)) {
    return;
  }
  c = p->cleanups;
  lastp = & p->cleanups;
  while (c) {
    if ((unsigned int )c->data == (unsigned int )data) {
      if ((unsigned int )c->plain_cleanup_fn == (unsigned int )cleanup_fn) {
        (*lastp) = c->next;
        break;
      } else {
        goto _L;
      }
    } else {
      _L: ;
    }
    lastp = & c->next;
    c = c->next;
  }
  return;
}
}


void *apr_palloc(apr_pool_t *pool , apr_size_t size ) 
{ apr_memnode_t *active ;
  apr_memnode_t *node ;
  void *mem ;
  apr_uint32_t free_index ;

  {
  size = (size + 7U) & 4294967288U;
  active = pool->active;
  if (size < (unsigned int )(active->endp - active->first_avail)) {
    mem = (void *)active->first_avail;
    active->first_avail = active->first_avail + size;
    return (mem);
  }
  node = active->next;
  if (size < (unsigned int )(node->endp - node->first_avail)) {
    (*(node->ref)) = node->next;
    (node->next)->ref = node->ref;
  } else {
    node = allocator_alloc(pool->allocator, size);
    if ((unsigned int )node == (unsigned int )((void *)0)) {
      if (pool->abort_fn) {
        ((*(pool->abort_fn)))(12);
      }
      return ((void *)0);
    }
  }
  node->free_index = 0U;
  mem = (void *)node->first_avail;
  node->first_avail = node->first_avail + size;
  node->ref = active->ref;
  (*(node->ref)) = node;
  node->next = active;
  active->ref = & node->next;
  pool->active = node;
  free_index = (unsigned int )((((((active->endp - active->first_avail) + 1) + 4095) &
                                 -4096) - 4096) >> 12);
  active->free_index = free_index;
  node = active->next;
  if (free_index >= node->free_index) {
    return (mem);
  }
  while (1) {
    node = node->next;
    if (! (free_index < node->free_index)) {
      break;
    }
  }
  (*(active->ref)) = active->next;
  (active->next)->ref = active->ref;
  active->ref = node->ref;
  (*(active->ref)) = active;
  active->next = node;
  node->ref = & active->next;
  return (mem);
}
}

static void *apr_array_push_noclear(apr_array_header_t *arr ) 
{ int new_size ;
  int tmp ;
  char *new_data ;

  {
  if (arr->nelts == arr->nalloc) {
    if (arr->nalloc <= 0) {
      tmp = 1;
    } else {
      tmp = arr->nalloc * 2;
    }
    new_size = tmp;
    new_data = (char *)apr_palloc(arr->pool, (unsigned int )(arr->elt_size * new_size));
    memcpy((void * __restrict  )new_data, (void const   * __restrict  )arr->elts,
           (unsigned int )(arr->nalloc * arr->elt_size));
    arr->elts = new_data;
    arr->nalloc = new_size;
  }
  arr->nelts = arr->nelts + 1;
  return ((void *)(arr->elts + arr->elt_size * (arr->nelts - 1)));
}
}


extern int fprintf(FILE * __restrict  __stream , char const   * __restrict  __format 
                   , ...) ;

apr_status_t apr_proc_detach(int daemonize ) 
{ int x ;
  int *tmp ;
  __pid_t tmp___0 ;
  int *tmp___1 ;
  FILE *tmp___2 ;
  int *tmp___3 ;
  FILE *tmp___4 ;
  int *tmp___5 ;
  FILE *tmp___6 ;

  {
  chdir("/");
  if (daemonize) {
    x = fork();
    if (x > 0) {
      exit(0);
    } else {
      if (x == -1) {
        perror("fork");
        fprintf((FILE * __restrict  )stderr, (char const   * __restrict  )"unable to fork new process\n");
        exit(1);
      }
    }
  }
  tmp___0 = setsid();
  if (tmp___0 == -1) {
    tmp = __errno_location();
    return ((*tmp));
  }
  tmp___2 = freopen((char const   * __restrict  )"/dev/null", (char const   * __restrict  )"r",
                    (FILE * __restrict  )stdin);
  if ((unsigned int )tmp___2 == (unsigned int )((void *)0)) {
    tmp___1 = __errno_location();
    return ((*tmp___1));
  }
  tmp___4 = freopen((char const   * __restrict  )"/dev/null", (char const   * __restrict  )"w",
                    (FILE * __restrict  )stdout);
  if ((unsigned int )tmp___4 == (unsigned int )((void *)0)) {
    tmp___3 = __errno_location();
    return ((*tmp___3));
  }
  tmp___6 = freopen((char const   * __restrict  )"/dev/null", (char const   * __restrict  )"w",
                    (FILE * __restrict  )stderr);
  if ((unsigned int )tmp___6 == (unsigned int )((void *)0)) {
    tmp___5 = __errno_location();
    return ((*tmp___5));
  }
  return (0);
}
}


static apr_other_child_rec_t *other_children  =    (apr_other_child_rec_t *)((void *)0);

static apr_status_t other_child_cleanup(void *data ) 
{ apr_other_child_rec_t **pocr ;
  apr_other_child_rec_t *nocr ;

  {
  pocr = & other_children;
  while ((*pocr)) {
    if ((unsigned int )((*pocr))->data == (unsigned int )data) {
      nocr = ((*pocr))->next;
      ((*(((*pocr))->maintenance)))(3, ((*pocr))->data, -1);
      (*pocr) = nocr;
      return (0);
    }
    pocr = & ((*pocr))->next;
  }
  return (0);
}
}


void apr_proc_other_child_unregister(void *data ) 
{ apr_other_child_rec_t *cur ;

  {
  cur = other_children;
  while (cur) {
    if ((unsigned int )cur->data == (unsigned int )data) {
      break;
    }
    cur = cur->next;
  }
  apr_pool_cleanup_kill(cur->p, (void const   *)cur->data, & other_child_cleanup);
  other_child_cleanup(data);
  return;
}
}


void (ap_log_error) (char const   *file , int line , 
                     int level , apr_status_t status , server_rec const   *s ,
                     char const   *fmt  , ...) 
{ va_list args ;

  {
  __builtin_stdarg_start(args, fmt);
  log_error_core(file, line, level, status, s, (request_rec const   *)((void *)0),
                 (apr_pool_t *)((void *)0), fmt, args);
  __builtin_va_end(args);
  return;
}
}

apr_status_t apr_proc_kill(apr_proc_t *proc , int signum ) 
{ int *tmp ;
  int tmp___0 ;

  {
  tmp___0 = kill(proc->pid, signum);
  if (tmp___0 == -1) {
    tmp = __errno_location();
    return ((*tmp));
  }
  return (0);
}
}

apr_status_t apr_procattr_create(apr_procattr_t **new , apr_pool_t *pool ) 
{ void *tmp ;

  {
  tmp = apr_palloc(pool, sizeof(apr_procattr_t ));
  (*new) = (apr_procattr_t *)memset(tmp, 0, sizeof(apr_procattr_t ));
  if ((unsigned int )(*new) == (unsigned int )((void *)0)) {
    return (12);
  }
  ((*new))->pool = pool;
  ((*new))->cmdtype = 1;
  return (0);
}
}

char *apr_pstrdup(apr_pool_t *a , char const   *s ) 
{ char *res ;
  apr_size_t len ;
  size_t tmp ;

  {
  if ((unsigned int )s == (unsigned int )((void *)0)) {
    return ((char *)((void *)0));
  }
  tmp = strlen(s);
  len = tmp + 1U;
  res = (char *)apr_palloc(a, len);
  memcpy((void * __restrict  )res, (void const   * __restrict  )s, len);
  return (res);
}
}


static apr_status_t _file_dup(apr_file_t **new_file , apr_file_t *old_file , apr_pool_t *p ,
                              int which_dup ) 
{ int rv ;
  void *tmp ;
  int *tmp___1 ;

  {
  if ((unsigned int )(*new_file) == (unsigned int )((void *)0)) {
    if (which_dup == 1) {
      tmp = apr_palloc(p, sizeof(apr_file_t ));
      (*new_file) = (apr_file_t *)memset(tmp, 0, sizeof(apr_file_t ));
      if ((unsigned int )(*new_file) == (unsigned int )((void *)0)) {
        return (12);
      }
      ((*new_file))->pool = p;
    } else {
      return (22);
    }
  }
  if (which_dup == 2) {
    rv = dup2(old_file->filedes, ((*new_file))->filedes);
  } else {
    ((*new_file))->filedes = dup(old_file->filedes);
    rv = ((*new_file))->filedes;
  }
  if (rv == -1) {
    tmp___1 = __errno_location();
    return ((*tmp___1));
  }
  ((*new_file))->fname = apr_pstrdup(p, (char const   *)old_file->fname);
  ((*new_file))->buffered = old_file->buffered;
  if (((*new_file))->buffered) {
    if (! ((*new_file))->thlock) {
      if (old_file->thlock) {
        apr_thread_mutex_create(& ((*new_file))->thlock, 0U, p);
      } else {
        goto _L___0;
      }
    } else {
      goto _L___0;
    }
  } else {
    _L___0: ;
  }
  if (((*new_file))->buffered) {
    if (! ((*new_file))->buffer) {
      ((*new_file))->buffer = (char *)apr_palloc(p, 4096U);
    } else {
      goto _L___1;
    }
  } else {
    _L___1: ;
  }
  ((*new_file))->blocking = old_file->blocking;
  ((*new_file))->ungetchar = old_file->ungetchar;
  ((*new_file))->flags = old_file->flags & -16777217;
  return (0);
}
}


apr_status_t apr_file_dup2(apr_file_t *new_file , apr_file_t *old_file , apr_pool_t *p ) 
{ apr_status_t tmp ;

  {
  tmp = _file_dup(& new_file, old_file, p, 2);
  return (tmp);
}
}


apr_status_t apr_procattr_child_in_set(struct apr_procattr_t *attr , apr_file_t *child_in ,
                                       apr_file_t *parent_in ) 
{ 

  {
  if ((unsigned int )attr->child_in == (unsigned int )((void *)0)) {
    if ((unsigned int )attr->parent_in == (unsigned int )((void *)0)) {
      apr_file_pipe_create(& attr->child_in, & attr->parent_in, attr->pool);
    } else {
      goto _L;
    }
  } else {
    _L: ;
  }
  if ((unsigned int )child_in != (unsigned int )((void *)0)) {
    apr_file_dup2(attr->child_in, child_in, attr->pool);
  }
  if ((unsigned int )parent_in != (unsigned int )((void *)0)) {
    apr_file_dup2(attr->parent_in, parent_in, attr->pool);
  }
  return (0);
}
}

apr_status_t apr_pool_cleanup_null(void *data ) 
{ 

  {
  return (0);
}
}


void apr_pool_cleanup_register(apr_pool_t *p , void const   *data , apr_status_t (*plain_cleanup_fn)(void *data ) ,
                               apr_status_t (*child_cleanup_fn)(void *data ) ) 
{ cleanup_t *c ;

  {
  if ((unsigned int )p != (unsigned int )((void *)0)) {
    c = (cleanup_t *)apr_palloc(p, sizeof(cleanup_t ));
    c->data = data;
    c->plain_cleanup_fn = plain_cleanup_fn;
    c->child_cleanup_fn = child_cleanup_fn;
    c->next = p->cleanups;
    p->cleanups = c;
  }
  return;
}
}


apr_status_t apr_file_pipe_create(apr_file_t **in , apr_file_t **out , apr_pool_t *pool ) 
{ int filedes[2] ;
  int *tmp ;
  int tmp___0 ;
  void *tmp___1 ;
  void *tmp___3 ;

  {
  tmp___0 = pipe(filedes);
  if (tmp___0 == -1) {
    tmp = __errno_location();
    return ((*tmp));
  }
  tmp___1 = apr_palloc(pool, sizeof(apr_file_t ));
  (*in) = (apr_file_t *)memset(tmp___1, 0, sizeof(apr_file_t ));
  ((*in))->pool = pool;
  ((*in))->filedes = filedes[0];
  ((*in))->is_pipe = 1;
  ((*in))->fname = (char *)((void *)0);
  ((*in))->buffered = 0;
  ((*in))->blocking = (enum __anonenum_blocking_60 )2;
  ((*in))->timeout = -1LL;
  ((*in))->ungetchar = -1;
  ((*in))->thlock = (struct apr_thread_mutex_t *)((void *)0);
  tmp___3 = apr_palloc(pool, sizeof(apr_file_t ));
  (*out) = (apr_file_t *)memset(tmp___3, 0, sizeof(apr_file_t ));
  ((*out))->pool = pool;
  ((*out))->filedes = filedes[1];
  ((*out))->is_pipe = 1;
  ((*out))->fname = (char *)((void *)0);
  ((*out))->buffered = 0;
  ((*out))->blocking = (enum __anonenum_blocking_60 )2;
  ((*out))->timeout = -1LL;
  ((*out))->thlock = (struct apr_thread_mutex_t *)((void *)0);
  apr_pool_cleanup_register(((*in))->pool, (void const   *)((void *)(*in)), & apr_unix_file_cleanup,
                            & apr_pool_cleanup_null);
  apr_pool_cleanup_register(((*out))->pool, (void const   *)((void *)(*out)), & apr_unix_file_cleanup,
                            & apr_pool_cleanup_null);
  return (0);
}
}

void apr_proc_other_child_register(apr_proc_t *pid , void (*maintenance)(int reason ,
                                                                         void * ,
                                                                         int status ) ,
                                   void *data , apr_file_t *write_fd , apr_pool_t *p ) 
{ apr_other_child_rec_t *ocr ;

  {
  ocr = (apr_other_child_rec_t *)apr_palloc(p, sizeof((*ocr)));
  ocr->p = p;
  ocr->proc = pid;
  ocr->maintenance = maintenance;
  ocr->data = data;
  if ((unsigned int )write_fd == (unsigned int )((void *)0)) {
    ocr->write_fd = -1;
  } else {
    ocr->write_fd = write_fd->filedes;
  }
  ocr->next = other_children;
  other_children = ocr;
  apr_pool_cleanup_register(p, (void const   *)ocr->data, & other_child_cleanup, & apr_pool_cleanup_null);
  return;
}
}


void *apr_bucket_alloc(apr_size_t size , apr_bucket_alloc_t *list ) 
{ node_header_t *node ;
  apr_memnode_t *active ;
  char *endp ;
  apr_memnode_t *memnode ;
  apr_memnode_t *tmp ;

  {
  active = list->blocks;
  size += (sizeof(node_header_t ) + 7U) & 4294967288U;
  if (size <= ((2U * sizeof(apr_bucket_structs ) + 7U) & 4294967288U) + ((sizeof(node_header_t ) +
                                                                          7U) & 4294967288U)) {
    if (list->freelist) {
      node = list->freelist;
      list->freelist = node->next;
    } else {
      endp = active->first_avail + (((2U * sizeof(apr_bucket_structs ) + 7U) & 4294967288U) +
                                    ((sizeof(node_header_t ) + 7U) & 4294967288U));
      if ((unsigned int )endp >= (unsigned int )active->endp) {
        list->blocks = apr_allocator_alloc(list->allocator, 8192U - ((sizeof(apr_memnode_t ) +
                                                                      7U) & 4294967288U));
        (list->blocks)->next = active;
        active = list->blocks;
        endp = active->first_avail + (((2U * sizeof(apr_bucket_structs ) + 7U) & 4294967288U) +
                                      ((sizeof(node_header_t ) + 7U) & 4294967288U));
      }
      node = (node_header_t *)active->first_avail;
      node->alloc = list;
      node->memnode = active;
      node->size = ((2U * sizeof(apr_bucket_structs ) + 7U) & 4294967288U) + ((sizeof(node_header_t ) +
                                                                               7U) &
                                                                              4294967288U);
      active->first_avail = endp;
    }
  } else {
    tmp = apr_allocator_alloc(list->allocator, size);
    memnode = tmp;
    node = (node_header_t *)memnode->first_avail;
    node->alloc = list;
    node->memnode = memnode;
    node->size = size;
  }
  return ((void *)((char *)node + ((sizeof(node_header_t ) + 7U) & 4294967288U)));
}
}

apr_status_t apr_bucket_simple_copy(apr_bucket *a , apr_bucket **b ) 
{ 

  {
  (*b) = (apr_bucket *)apr_bucket_alloc(sizeof((*((*b)))), a->list);
  (*((*b))) = (*a);
  return (0);
}
}


apr_bucket *apr_bucket_pool_create(char const   *buf , apr_size_t length , apr_pool_t *pool ,
                                   apr_bucket_alloc_t *list ) 
{ apr_bucket *b ;
  apr_bucket *tmp ;
  apr_bucket *tmp___0 ;

  {
  tmp = (apr_bucket *)apr_bucket_alloc(sizeof((*b)), list);
  b = tmp;
  while (1) {
    b->link.next = b;
    b->link.prev = b;
    break;
  }
  b->free = 0;//& apr_bucket_free;
  b->list = list;
  tmp___0 = 0;//apr_bucket_pool_make(b, buf, length, pool);
  return (tmp___0);
}
}

apr_bucket *ap_bucket_error_create(int error , char const   *buf , apr_pool_t *p ,
                                   apr_bucket_alloc_t *list ) 
{ apr_bucket *b ;
  apr_bucket *tmp ;
  apr_bucket *tmp___0 ;

  {
  tmp = (apr_bucket *)apr_bucket_alloc(sizeof((*b)), list);
  b = tmp;
  while (1) {
    b->link.next = b;
    b->link.prev = b;
    break;
  }
  b->free = 0;//& apr_bucket_free;
  b->list = list;
  tmp___0 = 0;//ap_bucket_error_make(b, error, buf, p);
  return (tmp___0);
}
}

apr_bucket *apr_bucket_eos_make(apr_bucket *b ) 
{ 

  {
  b->length = 0U;
  b->start = 0L;
  b->data = (void *)0;
  b->type = & apr_bucket_type_eos;
  return (b);
}
}
apr_bucket *apr_bucket_eos_create(apr_bucket_alloc_t *list ) 
{ apr_bucket *b ;
  apr_bucket *tmp ;
  apr_bucket *tmp___0 ;

  {
  tmp = (apr_bucket *)apr_bucket_alloc(sizeof((*b)), list);
  b = tmp;
  while (1) {
    b->link.next = b;
    b->link.prev = b;
    break;
  }
  b->free = 0;//& apr_bucket_free;
  b->list = list;
  tmp___0 = apr_bucket_eos_make(b);
  return (tmp___0);
}
}

//////// IN BIG SCC

static apr_pool_t *global_pool  =    (apr_pool_t *)((void *)0);


static void run_child_cleanups(cleanup_t **cref ) 
{ cleanup_t *c ;

  {
  c = (*cref);
  while (c) {
    (*cref) = c->next;
    ((*(c->child_cleanup_fn)))((void *)c->data);
    // could be: { socket_cleanup, apr_pool_cleanup_null, 
    // apr_unix_file_cleanup, close_listeners_on_exec, regex_cleanup,
    // piped_log_cleanup_for_exec, flush_all_logs }
    c = (*cref);
  }
  return;
}
}
static void cleanup_pool_for_exec(apr_pool_t *p ) 
{ 

  {
  run_child_cleanups(& p->cleanups);
  p = p->child;
  while (p) {
    cleanup_pool_for_exec(p);
    p = p->sibling;
  }
  return;
}
}
void apr_pool_cleanup_for_exec(void) 
{ 

  {
  cleanup_pool_for_exec(global_pool);
  return;
}
}


apr_status_t apr_pool_cleanup_run(apr_pool_t *p , void *data , apr_status_t (*cleanup_fn)(void * ) ) 
{ apr_status_t tmp ;

  {
  apr_pool_cleanup_kill(p, (void const   *)data, cleanup_fn);
  tmp = ((*cleanup_fn))(data);
  return (tmp);
}
}


apr_status_t apr_file_close(apr_file_t *file ) 
{ apr_status_t tmp ;

  {
  tmp = apr_pool_cleanup_run(file->pool, (void *)file, & apr_unix_file_cleanup);
  return (tmp);
}
}


apr_status_t apr_proc_create(apr_proc_t *new, char const *progname, 
                             char const   * const  *args ,
                             char const   * const  *env , 
                             apr_procattr_t *attr , apr_pool_t *pool ) 
{ int i ;
  char const   **newargs ;
  int *tmp ;
  int status ;
  apr_pool_t *tmp___0 ;
  apr_pool_t *tmp___1 ;
  apr_pool_t *tmp___2 ;
  int tmp___3 ;

  new->in = attr->parent_in;
  new->err = attr->parent_err;
  new->out = attr->parent_out;
  new->pid = fork();

  if (new->pid < 0) {
    tmp = __errno_location();
    return ((*tmp));
  } else {
    if (new->pid == 0) {
      if (attr->child_in) {
        tmp___0 = get_pool_ptr();//apr_file_pool_get((apr_file_t const   *)attr->child_in);
        apr_pool_cleanup_kill(tmp___0, (void const   *)attr->child_in, 
                              & apr_unix_file_cleanup);
      }

      if (attr->child_out) {
        tmp___1 = get_pool_ptr();//apr_file_pool_get((apr_file_t const   *)attr->child_out);
        apr_pool_cleanup_kill(tmp___1, (void const   *)attr->child_out, 
                              & apr_unix_file_cleanup);
      }

      if (attr->child_err) {
        tmp___2 = get_pool_ptr();//apr_file_pool_get((apr_file_t const   *)attr->child_err);
        apr_pool_cleanup_kill(tmp___2, (void const   *)attr->child_err, 
                              & apr_unix_file_cleanup);
      }

      apr_pool_cleanup_for_exec();

      if (attr->child_in) {
        apr_file_close(attr->parent_in);
        dup2((attr->child_in)->filedes, 0);
        apr_file_close(attr->child_in);
      }

      if (attr->child_out) {
        apr_file_close(attr->parent_out);
        dup2((attr->child_out)->filedes, 1);
        apr_file_close(attr->child_out);
      }

      if (attr->child_err) {
        apr_file_close(attr->parent_err);
        dup2((attr->child_err)->filedes, 2);
        apr_file_close(attr->child_err);
      }

      //apr_signal(17, (void (*)(int  ))0);

      if ((unsigned int )attr->currdir != (unsigned int )((void *)0)) {
        tmp___3 = chdir((char const   *)attr->currdir);
        if (tmp___3 == -1) {
          exit(-1);
        }
      }

      status = limit_proc(attr);

      if (status != 0) {
        return (status);
      }

      if (attr->cmdtype == 0) {
        i = 0;
        while ((*(args + i))) {
          i ++;
        }

        newargs = (char const **) apr_palloc(pool, sizeof(char *) * 
                                             (unsigned int )(i + 3));
        (*(newargs + 0)) = "/bin/sh";
        (*(newargs + 1)) = "-c";

        i = 0;
        while ((*(args + i))) {
          (*(newargs + (i + 2))) = (char const   *)(*(args + i));
          i ++;
        }

        (*(newargs + (i + 2))) = (char const   *)((void *)0);

        if (attr->detached) {
          apr_proc_detach(1);
        }

        execve("/bin/sh", (char * const  *)newargs, (char * const  *)env);



      } else {

        if (attr->cmdtype == 1) {

          if (attr->detached) {
            apr_proc_detach(1);
          }

          execve(progname, (char * const  *)args, (char * const  *)env);


        } else {

          if (attr->cmdtype == 2) {

            if (attr->detached) {
              apr_proc_detach(1);
            }

            execv(progname, (char * const  *)args);

          } else {

            if (attr->detached) {
              apr_proc_detach(1);
            }

            execvp(progname, (char * const  *)args);
          }

        }
      }
      exit(-1);
    }
  }
  if (attr->child_in) {
    apr_file_close(attr->child_in);
  }
  if (attr->child_out) {
    apr_file_close(attr->child_out);
  }
  if (attr->child_err) {
    apr_file_close(attr->child_err);
  }
  return (0);

}



static void piped_log_maintenance(int reason , void *data , int status ) 
{ piped_log *pl ;
  apr_status_t stats ;
  char buf[120] ;
  char *tmp ;

  {
  pl = (piped_log *)data;
  switch (reason) {
  case 0: 
  pl->pid = (apr_proc_t *)((void *)0);
  apr_proc_other_child_unregister((void *)pl);
  if ((unsigned int )pl->program == (unsigned int )((void *)0)) {
    break;
  }
  break;
  case 4: 
  pl->pid = (apr_proc_t *)((void *)0);
  apr_proc_other_child_unregister((void *)pl);
  if ((unsigned int )pl->program == (unsigned int )((void *)0)) {
    break;
  }
  stats = piped_log_spawn(pl);
  if (stats != 0) {
    tmp = apr_strerror(stats, buf, sizeof(buf));
    ap_log_error("log.c", 796, 32, 0, (server_rec const   *)((void *)0), "piped_log_maintenance: unable to respawn \'%s\': %s",
                 pl->program, tmp);
  }
  break;
  case 1: ;
  break;
  case 2: 
  pl->program = (char *)((void *)0);
  if ((unsigned int )pl->pid != (unsigned int )((void *)0)) {
    apr_proc_kill(pl->pid, 15);
  }
  break;
  case 3: ;
  break;
  }
  return;
}
}


static int piped_log_spawn(piped_log *pl ) 
{ int rc ;
  apr_procattr_t *procattr ;
  apr_proc_t *procnew ;
  apr_status_t status ;
  char buf[120] ;
  char *tmp ;
  char **args ;
  char const   *pname ;
  void *tmp___0 ;

  
  procnew = (apr_proc_t *)((void *)0);
  status = apr_procattr_create(& procattr, pl->p);
  if (status != 0) {
    goto _L;
  } else {
    status = apr_procattr_child_in_set(procattr, pl->fds[0], pl->fds[1]);
    if (status != 0) {
      _L: 
      tmp = apr_strerror(status, buf, sizeof(buf));
      ap_log_error("log.c", 740, 32, 0, (server_rec const   *)((void *)0), "piped_log_spawn: unable to setup child process \'%s\': %s",
                   pl->program, tmp);
      rc = -1;
    } else {
      apr_tokenize_to_argv((char const   *)pl->program, & args, pl->p);
      pname = apr_pstrdup(pl->p, (char const   *)(*(args + 0)));
      tmp___0 = apr_palloc(pl->p, sizeof(apr_proc_t ));
      procnew = (apr_proc_t *)memset(tmp___0, 0, sizeof(apr_proc_t ));
      rc = apr_proc_create(procnew, pname, (char const   * const  *)args, (char const   * const  *)((void *)0),
                           procattr, pl->p);
      if (rc == 0) {
        pl->pid = procnew;
        pl->fds[1] = procnew->in;
        apr_proc_other_child_register(procnew, & piped_log_maintenance, (void *)pl,
                                      pl->fds[1], pl->p);
      }
    }
  }
  return (0);

}

static void Encode___0(unsigned char *output , apr_uint32_t const   *input , unsigned int len ) 
{ unsigned int i ;
  unsigned int j ;
  apr_uint32_t k ;

  {
  i = 0U;
  j = 0U;
  while (j < len) {
    k = (*(input + i));
    (*(output + j)) = (unsigned char )(k & 255U);
    (*(output + (j + 1U))) = (unsigned char )((k >> 8) & 255U);
    (*(output + (j + 2U))) = (unsigned char )((k >> 16) & 255U);
    (*(output + (j + 3U))) = (unsigned char )((k >> 24) & 255U);
    i ++;
    j += 4U;
  }
  return;
}
}

struct apr_bucket_type_t  const  apr_bucket_type_eos ;
int server_limit  ;
int thread_limit  ;
int ap_extended_status  =    0;
int volatile   ap_my_generation ;
scoreboard *ap_scoreboard_image ;
static char status_flags[11]  ;
static struct stat_opt  const  status_options[4]  = {      
  {0, "refresh", "Refresh"}, 
  {1, "notable", (char const   *)((void *)0)}, 
  {2, "auto", (char const   *)((void *)0)}, 
  {-1, (char const   *)((void *)0), (char const   *)((void *)0)}
};

apr_status_t apr_brigade_cleanup(void *data ) 
{ apr_bucket_brigade *b ;
  apr_bucket *e ;

  {
  b = (apr_bucket_brigade *)data;
  while (! ((unsigned int )b->list.next == (unsigned int )((struct apr_bucket *)((char *)(& b->list) -
                                                                                 (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                         (char *)((void *)0)))))) {
    e = b->list.next;
    while (1) {
      while (1) {
        (e->link.prev)->link.next = e->link.next;
        (e->link.next)->link.prev = e->link.prev;
        break;
      }
      while (1) {
        ((*((e->type)->destroy)))(e->data);
        ((*(e->free)))((void *)e);
        break;
      }
      break;
    }
  }
  return (0);
}
}


static apr_status_t brigade_cleanup(void *data ) 
{ apr_status_t tmp ;

  {
  tmp = apr_brigade_cleanup(data);
  return (tmp);
}
}

apr_status_t apr_brigade_destroy(apr_bucket_brigade *b ) 
{ apr_status_t tmp ;

  {
  apr_pool_cleanup_kill(b->p, (void const   *)b, & brigade_cleanup);
  tmp = apr_brigade_cleanup((void *)b);
  return (tmp);
}
}


apr_bucket_brigade *apr_brigade_create(apr_pool_t *p , apr_bucket_alloc_t *list ) 
{ apr_bucket_brigade *b ;

  {
  b = (apr_bucket_brigade *)apr_palloc(p, sizeof((*b)));
  b->p = p;
  b->bucket_alloc = list;
  while (1) {
    b->list.next = (struct apr_bucket *)((char *)(& b->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                       (char *)((void *)0)));
    b->list.prev = (struct apr_bucket *)((char *)(& b->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                       (char *)((void *)0)));
    break;
  }
  apr_pool_cleanup_register(b->p, (void const   *)b, & brigade_cleanup, & apr_pool_cleanup_null);
  return (b);
}
}


apr_status_t ap_get_brigade(ap_filter_t *next , apr_bucket_brigade *bb , 
                            ap_input_mode_t mode ,
                            apr_read_type_e block , apr_off_t readbytes ) 
{ apr_status_t tmp ;

  {
  if (next) {
    tmp = ((*((next->frec)->filter_func.in_func)))(next, bb, mode, block, readbytes);
    return (tmp);
  }
  return (-2);
}
}



int ap_discard_request_body(request_rec *r ) 
{ apr_bucket_brigade *bb ;
  int rv ;
  int seen_eos ;
  apr_bucket *bucket ;
  char const   *data ;
  apr_size_t len ;

  {
  if (r->main) {
    goto _L___7;
  } else {
    if ((int )(r->connection)->keepalive == 1) {
      _L___7: 
      goto _L___0;
    } else {
      if (r->status == 400) {
        goto _L___6;
      } else {
        if (r->status == 408) {
          _L___6: 
          goto _L___5;
        } else {
          if (r->status == 411) {
            _L___5: 
            goto _L___4;
          } else {
            if (r->status == 413) {
              _L___4: 
              goto _L___3;
            } else {
              if (r->status == 414) {
                _L___3: 
                goto _L___2;
              } else {
                if (r->status == 500) {
                  _L___2: 
                  goto _L___1;
                } else {
                  if (r->status == 503) {
                    _L___1: 
                    goto _L___0;
                  } else {
                    if (r->status == 501) {
                      _L___0: 
                      return (0);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  bb = apr_brigade_create(r->pool, (r->connection)->bucket_alloc);
  seen_eos = 0;
  while (1) {
    rv = ap_get_brigade(r->input_filters, bb, (enum __anonenum_ap_input_mode_t_70 )0,
                        (enum __anonenum_apr_read_type_e_60 )0, 8192L);
    if (rv != 0) {
      if (rv == -3) {
        apr_brigade_destroy(bb);
        return (rv);
      } else {
        apr_brigade_destroy(bb);
        return (400);
      }
    }
    bucket = bb->list.next;
    while ((unsigned int )bucket != (unsigned int )((struct apr_bucket *)((char *)(& bb->list) -
                                                                          (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                  (char *)((void *)0))))) {
      if ((unsigned int )bucket->type == (unsigned int )(& apr_bucket_type_eos)) {
        seen_eos = 1;
        break;
      }
      if (bucket->length == 0U) {
        goto __Cont;
      }
      rv = ((*((bucket->type)->read)))(bucket, & data, & len, (enum __anonenum_apr_read_type_e_60 )0);
      if (rv != 0) {
        apr_brigade_destroy(bb);
        return (400);
      }
      __Cont: 
      bucket = bucket->link.next;
    }
    apr_brigade_cleanup((void *)bb);
    if (! (! seen_eos)) {
      break;
    }
  }
  return (0);
}
}

static int status_handler(request_rec *r ) 
{ char const   *loc ;
  apr_time_t nowtime ;
  apr_interval_time_t up_time ;
  int j ;
  int i ;
  int res ;
  int ready ;
  int busy ;
  unsigned long count ;
  unsigned long lres ;
  unsigned long my_lres ;
  unsigned long conn_lres ;
  apr_off_t bytes ;
  apr_off_t my_bytes ;
  apr_off_t conn_bytes ;
  apr_off_t bcount ;
  apr_off_t kbcount ;
  long req_time ;
  float tick ;
  int short_report ;
  int no_table_report ;
  worker_score *ws_record ;
  process_score *ps_record ;
  char *stat_buffer ;
  pid_t *pid_buffer ;
  clock_t tu ;
  clock_t ts ;
  clock_t tcu ;
  clock_t tcs ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  size_t tmp___2 ;
  size_t tmp___3 ;
  size_t tmp___4 ;
  long tmp___5 ;
  int indx ;
  char const   *tmp___6 ;
  char const   *tmp___7 ;
  char const   *tmp___8 ;
  char *tmp___9 ;
  char *tmp___10 ;
  int indx___0 ;
  int j___0 ;
  int k ;
  int indx___1 ;
  char *tmp___11 ;
  char *tmp___12 ;
  char *tmp___13 ;
  char *tmp___14 ;
  char *tmp___15 ;
  char *tmp___16 ;
  char const   *tmp___17 ;

  {
  tmp = strcmp(r->handler, "application/x-httpd-status");
  if (tmp) {
    tmp___0 = strcmp(r->handler, "server-status");
    if (tmp___0) {
      return (-1);
    } else {
      goto _L;
    }
  } else {
    _L: /* CIL Label */ ;
  }
  tick = (float )sysconf(2);
  ready = 0;
  busy = 0;
  count = 0UL;
  bcount = 0L;
  kbcount = 0L;
  short_report = 0;
  no_table_report = 0;
  pid_buffer = (pid_t *)apr_palloc(r->pool, (unsigned int )server_limit * sizeof(pid_t ));
  stat_buffer = (char *)apr_palloc(r->pool, (unsigned int )(server_limit * thread_limit) *
                                            sizeof(char ));
  nowtime = 0;//apr_time_now();
  tcs = 0L;
  tcu = tcs;
  ts = tcu;
  tu = ts;
  tmp___1 = ap_exists_scoreboard_image();
  if (! tmp___1) {
    //ap_log_rerror("mod_status.c", 289, 3, 0, (request_rec const   *)r, "Server status unavailable in inetd mode");
    return (500);
  }
  r->allowed = 1LL;
  if (r->method_number != 0) {
    return (-1);
  }
  //ap_set_content_type(r, "text/html");
  if (r->args) {
    i = 0;
    while (status_options[i].id != -1) {
      loc = strstr((char const   *)r->args, status_options[i].form_data_str);
      if ((unsigned int )loc != (unsigned int )((void *)0)) {
        switch (status_options[i].id) {
        case 0: 
        tmp___3 = strlen(status_options[i].form_data_str);
        if ((int const   )(*(loc + tmp___3)) == 61) {
          tmp___4 = strlen(status_options[i].form_data_str);
          tmp___5 = atol((loc + tmp___4) + 1);
          if (tmp___5 > 0L) {
            tmp___2 = strlen(status_options[i].hdr_out_str);
            //apr_table_set(r->headers_out, status_options[i].hdr_out_str, (loc + tmp___2) + 1);
          } else {
            goto _L___0;
          }
        } else {
          _L___0: /* CIL Label */ 
          //apr_table_set(r->headers_out, status_options[i].hdr_out_str, "1");
        }
        break;
        case 1: 
        no_table_report = 1;
        break;
        case 2: 
          //ap_set_content_type(r, "text/plain");
        short_report = 1;
        break;
        }
      }
      i ++;
    }
  }
  i = 0;
  while (i < server_limit) {
    ps_record = ap_get_scoreboard_process(i);
    j = 0;
    while (j < thread_limit) {
      indx = i * thread_limit + j;
      ws_record = ap_get_scoreboard_worker(i, j);
      res = (int )ws_record->status;
      (*(stat_buffer + indx)) = status_flags[res];
      if (! ps_record->quiescing) {
        if (ps_record->pid) {
          if (res == 2) {
            if (ps_record->generation == (ap_generation_t )ap_my_generation) {
              ready ++;
            } else {
              goto _L___3;
            }
          } else {
            _L___3: /* CIL Label */ 
            if (res != 0) {
              if (res != 1) {
                if (res != 10) {
                  busy ++;
                } else {
                  goto _L___2;
                }
              } else {
                goto _L___2;
              }
            } else {
              _L___2: /* CIL Label */ ;
            }
          }
        } else {
          goto _L___4;
        }
      } else {
        _L___4: /* CIL Label */ ;
      }
      if (ap_extended_status) {
        lres = ws_record->access_count;
        bytes = ws_record->bytes_served;
        if (lres != 0UL) {
          goto _L___5;
        } else {
          if (res != 2) {
            if (res != 0) {
              _L___5: /* CIL Label */ 
              tu += ws_record->times.tms_utime;
              ts += ws_record->times.tms_stime;
              tcu += ws_record->times.tms_cutime;
              tcs += ws_record->times.tms_cstime;
              count += lres;
              bcount += bytes;
              if (bcount >= 1024L) {
                kbcount += bcount >> 10;
                bcount = bcount & 1023L;
              }
            } else {
              goto _L___6;
            }
          } else {
            _L___6: /* CIL Label */ ;
          }
        }
      }
      j ++;
    }
    (*(pid_buffer + i)) = ps_record->pid;
    i ++;
  }
  up_time = (long long )((unsigned int )((nowtime - (ap_scoreboard_image->global)->restart_time) /
                                         1000000LL));
  if (! short_report) {
    ap_rputs("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n<html><head>\n<title>Apache Status</title>\n</head><body>\n",
             r);
    ap_rputs("<h1>Apache Server Status for ", r);
    tmp___6 = 0;//ap_get_server_name(r);
    ap_rvputs(r, tmp___6, "</h1>\n\n", (void *)0);
    tmp___7 = ap_get_server_version();
    ap_rvputs(r, "<dl><dt>Server Version: ", tmp___7, "</dt>\n", (void *)0);
    tmp___8 = ap_get_server_built();
    ap_rvputs(r, "<dt>Server Built: ", tmp___8, "\n</dt></dl><hr /><dl>\n", (void *)0);
    tmp___9 = 0;//ap_ht_time(r->pool, nowtime, "%A, %d-%b-%Y %H:%M:%S %Z", 0);
    ap_rvputs(r, "<dt>Current Time: ", tmp___9, "</dt>\n", (void *)0);
    tmp___10 = 0;//ap_ht_time(r->pool, (ap_scoreboard_image->global)->restart_time, "%A, %d-%b-%Y %H:%M:%S %Z", 0);
    ap_rvputs(r, "<dt>Restart Time: ", tmp___10, "</dt>\n", (void *)0);
    ap_rprintf(r, "<dt>Parent Server Generation: %d</dt>\n", ap_my_generation);
    ap_rputs("<dt>Server uptime: ", r);
    //show_time(r, up_time);
    ap_rputs("</dt>\n", r);
  }
  if (ap_extended_status) {
    if (short_report) {
      ap_rprintf(r, "Total Accesses: %lu\nTotal kBytes: %ld\n", count, kbcount);
      if (ts) {
        goto _L___9;
      } else {
        if (tu) {
          _L___9: /* CIL Label */ 
          goto _L___8;
        } else {
          if (tcu) {
            _L___8: /* CIL Label */ 
            goto _L___7;
          } else {
            if (tcs) {
              _L___7: /* CIL Label */ 
              ap_rprintf(r, "CPULoad: %g\n", (double )(((float )(((tu + ts) + tcu) +
                                                                 tcs) / tick) / (float )up_time) *
                                             100.);
            }
          }
        }
      }
      ap_rprintf(r, "Uptime: %ld\n", (long )up_time);
      if (up_time > 0LL) {
        ap_rprintf(r, "ReqPerSec: %g\n", (float )count / (float )up_time);
      }
      if (up_time > 0LL) {
        ap_rprintf(r, "BytesPerSec: %g\n", ((float )1024 * (float )kbcount) / (float )up_time);
      }
      if (count > 0UL) {
        ap_rprintf(r, "BytesPerReq: %g\n", ((float )1024 * (float )kbcount) / (float )count);
      }
    } else {
      ap_rprintf(r, "<dt>Total accesses: %lu - Total Traffic: ", count);
      //format_kbyte_out(r, kbcount);
      ap_rputs("</dt>\n", r);
      ap_rprintf(r, "<dt>CPU Usage: u%g s%g cu%g cs%g", (float )tu / tick, (float )ts /
                                                                           tick, (float )tcu /
                                                                                 tick,
                 (float )tcs / tick);
      if (ts) {
        goto _L___12;
      } else {
        if (tu) {
          _L___12: /* CIL Label */ 
          goto _L___11;
        } else {
          if (tcu) {
            _L___11: /* CIL Label */ 
            goto _L___10;
          } else {
            if (tcs) {
              _L___10: /* CIL Label */ 
              ap_rprintf(r, " - %.3g%% CPU load</dt>\n", (double )(((float )(((tu +
                                                                               ts) +
                                                                              tcu) +
                                                                             tcs) /
                                                                    tick) / (float )up_time) *
                                                         100.);
            }
          }
        }
      }
      if (up_time > 0LL) {
        ap_rprintf(r, "<dt>%.3g requests/sec - ", (float )count / (float )up_time);
      }
      if (up_time > 0LL) {
        format_byte_out(r, (long )((unsigned long )(((float )1024 * (float )kbcount) /
                                                    (float )up_time)));
        ap_rputs("/second - ", r);
      }
      if (count > 0UL) {
        format_byte_out(r, (long )((unsigned long )(((float )1024 * (float )kbcount) /
                                                    (float )count)));
        ap_rputs("/request", r);
      }
      ap_rputs("</dt>\n", r);
    }
  }
  if (! short_report) {
    ap_rprintf(r, "<dt>%d requests currently being processed, %d idle workers</dt>\n",
               busy, ready);
  } else {
    ap_rprintf(r, "BusyWorkers: %d\nIdleWorkers: %d\n", busy, ready);
  }
  if (! short_report) {
    ap_rputs("</dl><pre>", r);
  } else {
    ap_rputs("Scoreboard: ", r);
  }
  i = 0;
  while (i < server_limit) {
    j = 0;
    while (j < thread_limit) {
      indx___0 = i * thread_limit + j;
      ap_rputc((int )(*(stat_buffer + indx___0)), r);
      if (indx___0 % 64 == 63) {
        if (! short_report) {
          ap_rputs("\n", r);
        } else {
          goto _L___13;
        }
      } else {
        _L___13: /* CIL Label */ ;
      }
      j ++;
    }
    i ++;
  }
  if (short_report) {
    ap_rputs("\n", r);
  } else {
    ap_rputs("</pre>\n", r);
    ap_rputs("<p>Scoreboard Key:<br />\n", r);
    ap_rputs("\"<b><code>_</code></b>\" Waiting for Connection, \n", r);
    ap_rputs("\"<b><code>S</code></b>\" Starting up, \n", r);
    ap_rputs("\"<b><code>R</code></b>\" Reading Request,<br />\n", r);
    ap_rputs("\"<b><code>W</code></b>\" Sending Reply, \n", r);
    ap_rputs("\"<b><code>K</code></b>\" Keepalive (read), \n", r);
    ap_rputs("\"<b><code>D</code></b>\" DNS Lookup,<br />\n", r);
    ap_rputs("\"<b><code>C</code></b>\" Closing connection, \n", r);
    ap_rputs("\"<b><code>L</code></b>\" Logging, \n", r);
    ap_rputs("\"<b><code>G</code></b>\" Gracefully finishing,<br /> \n", r);
    ap_rputs("\"<b><code>I</code></b>\" Idle cleanup of worker, \n", r);
    ap_rputs("\"<b><code>.</code></b>\" Open slot with no current process</p>\n",
             r);
    ap_rputs("<p />\n", r);
    if (! ap_extended_status) {
      k = 0;
      ap_rputs("PID Key: <br />\n", r);
      ap_rputs("<pre>\n", r);
      i = 0;
      while (i < server_limit) {
        j___0 = 0;
        while (j___0 < thread_limit) {
          indx___1 = i * thread_limit + j___0;
          if ((int )(*(stat_buffer + indx___1)) != 46) {
            ap_rprintf(r, "   %d in state: %c ", (*(pid_buffer + i)), (*(stat_buffer +
                                                                         indx___1)));
            k ++;
            if (k >= 3) {
              ap_rputs("\n", r);
              k = 0;
            } else {
              ap_rputs(",", r);
            }
          }
          j___0 ++;
        }
        i ++;
      }
      ap_rputs("\n", r);
      ap_rputs("</pre>\n", r);
    }
  }
  if (ap_extended_status) {
    if (! short_report) {
      if (no_table_report) {
        ap_rputs("<hr /><h2>Server Details</h2>\n\n", r);
      } else {
        ap_rputs("\n\n<table border=\"0\"><tr><th>Srv</th><th>PID</th><th>Acc</th><th>M</th><th>CPU\n</th><th>SS</th><th>Req</th><th>Conn</th><th>Child</th><th>Slot</th><th>Client</th><th>VHost</th><th>Request</th></tr>\n\n",
                 r);
      }
      i = 0;
      while (i < server_limit) {
        j = 0;
        while (j < thread_limit) {
          ws_record = ap_get_scoreboard_worker(i, j);
          if (ws_record->access_count == 0UL) {
            if ((int )ws_record->status == 2) {
              goto _L___15;
            } else {
              if ((int )ws_record->status == 0) {
                _L___15: /* CIL Label */ 
                goto __Cont;
              } else {
                goto _L___14;
              }
            }
          } else {
            _L___14: /* CIL Label */ ;
          }
          ps_record = ap_get_scoreboard_process(i);
          if (ws_record->start_time == 0LL) {
            req_time = 0L;
          } else {
            req_time = (long )((ws_record->stop_time - ws_record->start_time) / 1000LL);
          }
          if (req_time < 0L) {
            req_time = 0L;
          }
          lres = ws_record->access_count;
          my_lres = ws_record->my_access_count;
          conn_lres = (unsigned long )ws_record->conn_count;
          bytes = ws_record->bytes_served;
          my_bytes = ws_record->my_bytes_served;
          conn_bytes = ws_record->conn_bytes;
          if (no_table_report) {
            if ((int )ws_record->status == 0) {
              ap_rprintf(r, "<b>Server %d-%d</b> (-): %d|%lu|%lu [", i, ps_record->generation,
                         (int )conn_lres, my_lres, lres);
            } else {
              ap_rprintf(r, "<b>Server %d-%d</b> (%d): %d|%lu|%lu [", i, ps_record->generation,
                         ps_record->pid, (int )conn_lres, my_lres, lres);
            }
            switch ((int )ws_record->status) {
            case 2: 
            ap_rputs("Ready", r);
            break;
            case 1: 
            ap_rputs("Starting", r);
            break;
            case 3: 
            ap_rputs("<b>Read</b>", r);
            break;
            case 4: 
            ap_rputs("<b>Write</b>", r);
            break;
            case 5: 
            ap_rputs("<b>Keepalive</b>", r);
            break;
            case 6: 
            ap_rputs("<b>Logging</b>", r);
            break;
            case 7: 
            ap_rputs("<b>DNS lookup</b>", r);
            break;
            case 8: 
            ap_rputs("<b>Closing</b>", r);
            break;
            case 0: 
            ap_rputs("Dead", r);
            break;
            case 9: 
            ap_rputs("Graceful", r);
            break;
            case 10: 
            ap_rputs("Dying", r);
            break;
            default: 
            ap_rputs("?STATE?", r);
            break;
            }
            ap_rprintf(r, "] u%g s%g cu%g cs%g\n %ld %ld (", (float )ws_record->times.tms_utime /
                                                             tick, (float )ws_record->times.tms_stime /
                                                                   tick, (float )ws_record->times.tms_cutime /
                                                                         tick, (float )ws_record->times.tms_cstime /
                                                                               tick,
                       (long )((nowtime - ws_record->last_used) / 1000000LL), req_time);
            format_byte_out(r, conn_bytes);
            ap_rputs("|", r);
            format_byte_out(r, my_bytes);
            ap_rputs("|", r);
            format_byte_out(r, bytes);
            ap_rputs(")\n", r);
            tmp___11 = ap_escape_html(r->pool, (char const   *)(ws_record->vhost));
            tmp___12 = ap_escape_html(r->pool, (char const   *)(ws_record->request));
            tmp___13 = ap_escape_html(r->pool, (char const   *)(ws_record->client));
            ap_rprintf(r, " <i>%s {%s}</i> <b>[%s]</b><br />\n\n", tmp___13, tmp___12,
                       tmp___11);
          } else {
            if ((int )ws_record->status == 0) {
              ap_rprintf(r, "<tr><td><b>%d-%d</b></td><td>-</td><td>%d/%lu/%lu", i,
                         ps_record->generation, (int )conn_lres, my_lres, lres);
            } else {
              ap_rprintf(r, "<tr><td><b>%d-%d</b></td><td>%d</td><td>%d/%lu/%lu",
                         i, ps_record->generation, ps_record->pid, (int )conn_lres,
                         my_lres, lres);
            }
            switch ((int )ws_record->status) {
            case 2: 
            ap_rputs("</td><td>_", r);
            break;
            case 1: 
            ap_rputs("</td><td><b>S</b>", r);
            break;
            case 3: 
            ap_rputs("</td><td><b>R</b>", r);
            break;
            case 4: 
            ap_rputs("</td><td><b>W</b>", r);
            break;
            case 5: 
            ap_rputs("</td><td><b>K</b>", r);
            break;
            case 6: 
            ap_rputs("</td><td><b>L</b>", r);
            break;
            case 7: 
            ap_rputs("</td><td><b>D</b>", r);
            break;
            case 8: 
            ap_rputs("</td><td><b>C</b>", r);
            break;
            case 0: 
            ap_rputs("</td><td>.", r);
            break;
            case 9: 
            ap_rputs("</td><td>G", r);
            break;
            case 10: 
            ap_rputs("</td><td>I", r);
            break;
            default: 
            ap_rputs("</td><td>?", r);
            break;
            }
            ap_rprintf(r, "\n</td><td>%.2f</td><td>%ld</td><td>%ld", (float )(((ws_record->times.tms_utime +
                                                                                ws_record->times.tms_stime) +
                                                                               ws_record->times.tms_cutime) +
                                                                              ws_record->times.tms_cstime) /
                                                                     tick, (long )((nowtime -
                                                                                    ws_record->last_used) /
                                                                                   1000000LL),
                       req_time);
            ap_rprintf(r, "</td><td>%-1.1f</td><td>%-2.2f</td><td>%-2.2f\n", (float )conn_bytes /
                                                                             (float )1024,
                       (float )my_bytes / (float )1048576L, (float )bytes / (float )1048576L);
            if ((int )ws_record->status == 3) {
              ap_rprintf(r, "</td><td>?</td><td nowrap>?</td><td nowrap>..reading.. </td></tr>\n\n");
            } else {
              tmp___14 = ap_escape_html(r->pool, (char const   *)(ws_record->request));
              tmp___15 = ap_escape_html(r->pool, (char const   *)(ws_record->vhost));
              tmp___16 = ap_escape_html(r->pool, (char const   *)(ws_record->client));
              ap_rprintf(r, "</td><td>%s</td><td nowrap>%s</td><td nowrap>%s</td></tr>\n\n",
                         tmp___16, tmp___15, tmp___14);
            }
          }
          __Cont: /* CIL Label */ 
          j ++;
        }
        i ++;
      }
      if (! no_table_report) {
        ap_rputs("</table>\n <hr /> <table>\n <tr><th>Srv</th><td>Child Server number - generation</td></tr>\n <tr><th>PID</th><td>OS process ID</td></tr>\n <tr><th>Acc</th><td>Number of accesses this connection / this child / this slot</td></tr>\n <tr><th>M</th><td>Mode of operation</td></tr>\n<tr><th>CPU</th><td>CPU usage, number of seconds</td></tr>\n<tr><th>SS</th><td>Seconds since beginning of most recent request</td></tr>\n <tr><th>Req</th><td>Milliseconds required to process most recent request</td></tr>\n <tr><th>Conn</th><td>Kilobytes transferred this connection</td></tr>\n <tr><th>Child</th><td>Megabytes transferred this child</td></tr>\n <tr><th>Slot</th><td>Total megabytes transferred this slot</td></tr>\n </table>\n",
                 r);
      }
    } else {
      goto _L___16;
    }
  } else {
    _L___16: /* CIL Label */ 
    if (! short_report) {
      ap_rputs("<hr />To obtain a full report with current status information you need to use the <code>ExtendedStatus On</code> directive.\n",
               r);
    }
  }
  if (! short_report) {
    tmp___17 = //ap_psignature("<hr />\n", r);
    ap_rputs(tmp___17, r);
    ap_rputs("</body></html>\n", r);
  }
  return (0);
}
}


////////////////////////////////////////////////////////////
// MORE HTTPD

char *ap_getword(apr_pool_t *atrans , char const   **line , char stop ) 
{ char const   *pos ;
  int len ;
  char *res ;

  {
  pos = (*line);
  while (1) {
    if ((int const   )(*pos) != (int const   )stop) {
      if (! (*pos)) {
        goto _L;
      }
    } else {
      _L: 
      break;
    }
    pos ++;
  }
  len = pos - (*line);
  res = (char *)apr_palloc(atrans, (unsigned int )(len + 1));
  memcpy((void * __restrict  )res, (void const   * __restrict  )(*line), (unsigned int )len);
  (*(res + len)) = (char)0;
  if (stop) {
    while ((int const   )(*pos) == (int const   )stop) {
      pos ++;
    }
  }
  (*line) = pos;
  return (res);
}
}


static void remove_any_filter(ap_filter_t *f , ap_filter_t **r_filt , ap_filter_t **p_filt ,
                              ap_filter_t **c_filt ) 
{ ap_filter_t **curr ;
  ap_filter_t **tmp ;
  ap_filter_t *fscan ;

  {
  if (r_filt) {
    tmp = r_filt;
  } else {
    tmp = c_filt;
  }
  curr = tmp;
  fscan = (*curr);
  if (p_filt) {
    if ((unsigned int )(*p_filt) == (unsigned int )f) {
      (*p_filt) = ((*p_filt))->next;
    } else {
      goto _L;
    }
  } else {
    _L: ;
  }
  if ((unsigned int )(*curr) == (unsigned int )f) {
    (*curr) = ((*curr))->next;
    return;
  }
  while ((unsigned int )fscan->next != (unsigned int )f) {
    fscan = fscan->next;
    if (! fscan) {
      return;
    }
  }
  fscan->next = f->next;
  return;
}
}


void ap_remove_output_filter(ap_filter_t *f ) 
{ struct ap_filter_t **tmp ;
  struct ap_filter_t **tmp___0 ;

  {
  if (f->r) {
    tmp = & (f->r)->proto_output_filters;
  } else {
    tmp = (struct ap_filter_t **)((void *)0);
  }
  if (f->r) {
    tmp___0 = & (f->r)->output_filters;
  } else {
    tmp___0 = (struct ap_filter_t **)((void *)0);
  }
  remove_any_filter(f, tmp___0, tmp, & (f->c)->output_filters);
  return;
}
}


apr_status_t ap_byterange_filter(ap_filter_t *f , apr_bucket_brigade *bb ) 
{ request_rec *r ;
  conn_rec *c ;
  byterange_ctx *ctx ;
  apr_bucket *e ;
  apr_bucket_brigade *bsend ;
  apr_off_t range_start ;
  apr_off_t range_end ;
  char *current ;
  apr_off_t bb_length ;
  apr_off_t clength ;
  apr_status_t rv ;
  int found ;
  int num_ranges ;
  int tmp ;
  apr_status_t tmp___0 ;
  void *tmp___1 ;
  char const   *orig_ct ;
  char const   *tmp___2 ;
  __pid_t tmp___3 ;
  char const   *tmp___5 ;
  int tmp___6 ;
  char const   *tmp___7 ;
  apr_bucket *e2 ;
  apr_bucket *ec ;
  char const   *tmp___8 ;
  char *ts ;
  apr_size_t tmp___9 ;
  apr_bucket *ap__b ;
  apr_size_t tmp___10 ;
  apr_bucket *ap__b___0 ;
  apr_bucket *foo ;
  char const   *str ;
  apr_size_t len ;
  apr_status_t tmp___11 ;
  apr_bucket *ap__b___1 ;
  apr_bucket *ap__b___2 ;
  apr_bucket *ap__b___3 ;
  apr_status_t tmp___12 ;
  char *end ;
  apr_size_t tmp___13 ;
  apr_bucket *ap__b___4 ;
  apr_bucket *ap__b___5 ;
  apr_status_t tmp___14 ;

  {
  r = f->r;
  c = r->connection;
  ctx = (byterange_ctx *)f->ctx;
  clength = 0L;
  found = 0;
  if (! ctx) {
    tmp = 0;//ap_set_byterange(r);
    num_ranges = tmp;
    if (num_ranges == 0) {
      ap_remove_output_filter(f);
      tmp___0 = ap_pass_brigade(f->next, bb);
      return (tmp___0);
    }
    tmp___1 = apr_palloc(r->pool, sizeof((*ctx)));
    f->ctx = memset(tmp___1, 0, sizeof((*ctx)));
    ctx = (byterange_ctx *)f->ctx;
    ctx->num_ranges = num_ranges;
    ctx->bb = apr_brigade_create(r->pool, c->bucket_alloc);
    if (ctx->num_ranges > 1) {
      tmp___2 = //ap_make_content_type(r, r->content_type);
      orig_ct = tmp___2;
      tmp___3 = getpid();
      ctx->boundary = 0;//apr_psprintf(r->pool, "%qx%lx", r->request_time, (long )tmp___3);
      tmp___6 = use_range_x(r);
      if (tmp___6) {
        tmp___5 = "/x-";
      } else {
        tmp___5 = "/";
      }
      tmp___7 = 0;//apr_pstrcat(r->pool, "multipart", tmp___5, "byteranges; boundary=", ctx->boundary, (void *)0);
      //ap_set_content_type(r, tmp___7);
      ctx->bound_head = 0;//apr_pstrcat(r->pool, "\r\n--", ctx->boundary, "\r\nContent-type: ", orig_ct, "\r\nContent-range: bytes ", (void *)0);
    }
  }
  if (! ((unsigned int )(bb->list.prev)->type == (unsigned int )(& apr_bucket_type_eos))) {
    //ap_save_brigade(f, & ctx->bb, & bb, r->pool);
    return (0);
  }
  while (1) {
    if (! ((unsigned int )(ctx->bb)->list.next == (unsigned int )((struct apr_bucket *)((char *)(& (ctx->bb)->list) -
                                                                                        (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                                (char *)((void *)0)))))) {
      while (1) {
        ((ctx->bb)->list.next)->link.prev = (struct apr_bucket *)((char *)(& bb->list) -
                                                                  (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                          (char *)((void *)0)));
        ((ctx->bb)->list.prev)->link.next = ((struct apr_bucket *)((char *)(& bb->list) -
                                                                   (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                           (char *)((void *)0))))->link.next;
        (((struct apr_bucket *)((char *)(& bb->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                               (char *)((void *)0))))->link.next)->link.prev = (ctx->bb)->list.prev;
        ((struct apr_bucket *)((char *)(& bb->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                              (char *)((void *)0))))->link.next = (ctx->bb)->list.next;
        break;
      }
      while (1) {
        (ctx->bb)->list.next = (struct apr_bucket *)((char *)(& (ctx->bb)->list) -
                                                     (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                             (char *)((void *)0)));
        (ctx->bb)->list.prev = (struct apr_bucket *)((char *)(& (ctx->bb)->list) -
                                                     (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                             (char *)((void *)0)));
        break;
      }
    }
    break;
  }
  //apr_brigade_length(bb, 1, & bb_length);
  clength = bb_length;
  bsend = apr_brigade_create(r->pool, c->bucket_alloc);
  while (1) {
    current = ap_getword(r->pool, & r->range, (char )',');
    if (current) {
      rv = 0;//parse_byterange(current, clength, & range_start, & range_end);
      if (! rv) {
        goto _L;
      }
    } else {
      _L: 
      break;
    }
    if (rv == -1) {
      continue;
    }
    rv = getpid();//apr_brigade_partition(bb, range_start, & ec);
    if (rv != 0) {
      //ap_log_rerror("http_protocol.c", 2951, 3, rv, (request_rec const   *)r, "apr_brigade_partition() failed [%ld,%ld]", range_start, clength);
      continue;
    }
    rv = getpid();//apr_brigade_partition(bb, range_end + 1L, & e2);
    if (rv != 0) {
      //ap_log_rerror("http_protocol.c", 2956, 3, rv, (request_rec const   *)r, "apr_brigade_partition() failed [%ld,%ld]", range_end + 1L, clength);
      continue;
    }
    found = 1;
    if (ctx->num_ranges == 1) {
      tmp___8 = 0;//apr_psprintf(r->pool, "bytes %ld-%ld/%ld", range_start, range_end, clength);
      apr_table_setn(r->headers_out, "Content-Range", tmp___8);
    } else {
      tmp___9 = strlen((char const   *)ctx->bound_head);
      e = apr_bucket_pool_create((char const   *)ctx->bound_head, tmp___9, r->pool,
                                 c->bucket_alloc);
      while (1) {
        ap__b = e;
        while (1) {
          ap__b->link.next = (struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                     (char *)((void *)0)));
          ap__b->link.prev = ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                      (char *)((void *)0))))->link.prev;
          (((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                    (char *)((void *)0))))->link.prev)->link.next = ap__b;
          ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                   (char *)((void *)0))))->link.prev = ap__b;
          break;
        }
        break;
      }
      ts = 0;//apr_psprintf(r->pool, "%ld-%ld/%ld\r\n\r\n", range_start, range_end, clength);
      tmp___10 = strlen((char const   *)ts);
      e = apr_bucket_pool_create((char const   *)ts, tmp___10, r->pool, c->bucket_alloc);
      while (1) {
        ap__b___0 = e;
        while (1) {
          ap__b___0->link.next = (struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                         (char *)((void *)0)));
          ap__b___0->link.prev = ((struct apr_bucket *)((char *)(& bsend->list) -
                                                        (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                (char *)((void *)0))))->link.prev;
          (((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                    (char *)((void *)0))))->link.prev)->link.next = ap__b___0;
          ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                   (char *)((void *)0))))->link.prev = ap__b___0;
          break;
        }
        break;
      }
    }
    while (1) {
      tmp___11 = ((*((ec->type)->copy)))(ec, & foo);
      if (tmp___11 != 0) {
        ((*((ec->type)->read)))(ec, & str, & len, (enum __anonenum_apr_read_type_e_60 )0);
        ((*((ec->type)->copy)))(ec, & foo);
      }
      while (1) {
        ap__b___1 = foo;
        while (1) {
          ap__b___1->link.next = (struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                         (char *)((void *)0)));
          ap__b___1->link.prev = ((struct apr_bucket *)((char *)(& bsend->list) -
                                                        (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                (char *)((void *)0))))->link.prev;
          (((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                    (char *)((void *)0))))->link.prev)->link.next = ap__b___1;
          ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                   (char *)((void *)0))))->link.prev = ap__b___1;
          break;
        }
        break;
      }
      ec = ec->link.next;
      if (! ((unsigned int )ec != (unsigned int )e2)) {
        break;
      }
    }
  }
  if (found == 0) {
    ap_remove_output_filter(f);
    r->status = 200;
    e = ap_bucket_error_create(416, (char const   *)((void *)0), r->pool, c->bucket_alloc);
    while (1) {
      ap__b___2 = e;
      while (1) {
        ap__b___2->link.next = (struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                       (char *)((void *)0)));
        ap__b___2->link.prev = ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                        (char *)((void *)0))))->link.prev;
        (((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                  (char *)((void *)0))))->link.prev)->link.next = ap__b___2;
        ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                 (char *)((void *)0))))->link.prev = ap__b___2;
        break;
      }
      break;
    }
    e = apr_bucket_eos_create(c->bucket_alloc);
    while (1) {
      ap__b___3 = e;
      while (1) {
        ap__b___3->link.next = (struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                       (char *)((void *)0)));
        ap__b___3->link.prev = ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                        (char *)((void *)0))))->link.prev;
        (((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                  (char *)((void *)0))))->link.prev)->link.next = ap__b___3;
        ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                 (char *)((void *)0))))->link.prev = ap__b___3;
        break;
      }
      break;
    }
    tmp___12 = ap_pass_brigade(f->next, bsend);
    return (tmp___12);
  }
  if (ctx->num_ranges > 1) {
    end = 0;//apr_pstrcat(r->pool, "\r\n--", ctx->boundary, "--\r\n", (void *)0);
    tmp___13 = strlen((char const   *)end);
    e = apr_bucket_pool_create((char const   *)end, tmp___13, r->pool, c->bucket_alloc);
    while (1) {
      ap__b___4 = e;
      while (1) {
        ap__b___4->link.next = (struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                       (char *)((void *)0)));
        ap__b___4->link.prev = ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                        (char *)((void *)0))))->link.prev;
        (((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                  (char *)((void *)0))))->link.prev)->link.next = ap__b___4;
        ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                 (char *)((void *)0))))->link.prev = ap__b___4;
        break;
      }
      break;
    }
  }
  e = apr_bucket_eos_create(c->bucket_alloc);
  while (1) {
    ap__b___5 = e;
    while (1) {
      ap__b___5->link.next = (struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                     (char *)((void *)0)));
      ap__b___5->link.prev = ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                                      (char *)((void *)0))))->link.prev;
      (((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                                (char *)((void *)0))))->link.prev)->link.next = ap__b___5;
      ((struct apr_bucket *)((char *)(& bsend->list) - (long )((char *)(& ((struct apr_bucket *)((void *)0))->link) -
                                                               (char *)((void *)0))))->link.prev = ap__b___5;
      break;
    }
    break;
  }
  apr_brigade_destroy(bb);
  tmp___14 = ap_pass_brigade(f->next, bsend);
  return (tmp___14);
}
}



////////////////////////////////////////////////////////////


int main (int argc, char *argv[]) {

  unsigned char buff[80];
  apr_uint32_t val;
  request_rec rr;

  val = 42;

  piped_log_spawn ((piped_log*) 0);
  Encode___0((unsigned char*) buff, &val, 79);
  status_handler(&rr);
  ap_discard_request_body(&rr); 

  return 0;
}
