typedef int __int32_t;
typedef long long __quad_t;
typedef unsigned long long __u_quad_t;
typedef __u_quad_t __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long __ino_t;
typedef unsigned int __mode_t;
typedef unsigned int __nlink_t;
typedef long __off_t;
typedef __quad_t __off64_t;
typedef long __time_t;
typedef long __blksize_t;
typedef long __blkcnt_t;
typedef int __ssize_t;
typedef unsigned int __socklen_t;
typedef unsigned int size_t;
struct __anonstruct___sigset_t_3
{
  unsigned long __val[1024U / (8U * sizeof (unsigned long))];
};
typedef struct __anonstruct___sigset_t_3 __sigset_t;
typedef unsigned long pthread_t;
typedef struct _IO_FILE FILE;
typedef __builtin_va_list __gnuc_va_list;
typedef void _IO_lock_t;
typedef struct _IO_FILE _IO_FILE;
struct stat
{
  __dev_t st_dev;
  unsigned short __pad1;
  __ino_t st_ino;
  __mode_t st_mode;
  __nlink_t st_nlink;
};
typedef struct disk_file DISK_FILE;
typedef int __pid_t;
typedef __time_t time_t;
typedef __sigset_t sigset_t;
typedef int __jmp_buf[6];
struct __jmp_buf_tag;
struct __jmp_buf_tag
{
  __jmp_buf __jmpbuf;
  int __mask_was_saved;
  __sigset_t __saved_mask;
};
typedef struct __jmp_buf_tag jmp_buf[1];
typedef unsigned short u16;
struct pollfd
{
  int fd;
  short events;
  short revents;
};
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned short sa_family_t;
struct sockaddr
{
  sa_family_t sa_family;
  char sa_data[14];
};
typedef uint16_t in_port_t;
typedef uint32_t in_addr_t;
struct in_addr
{
  in_addr_t s_addr;
};
union __anonunion_in6_addr_in6_u_66
{
  uint8_t u6_addr8[16];
  uint16_t u6_addr16[8];
  uint32_t u6_addr32[4];
};
struct in6_addr
{
  union __anonunion_in6_addr_in6_u_66 in6_u;
};
struct sockaddr_in
{
  sa_family_t sin_family;
  in_port_t sin_port;
  struct in_addr sin_addr;
  unsigned char
    sin_zero[((sizeof (struct sockaddr) - sizeof (unsigned short)) -
	      sizeof (in_port_t)) - sizeof (struct in_addr)];
};
struct sockaddr_in6
{
  sa_family_t sin6_family;
  int s_port;
  char *s_proto;
};
struct stack_st
{
  int num;
  void **data;
  int sorted;
  int num_alloc;
  int (*comp) (void const *const *, void const *const *);
};
typedef struct stack_st STACK;
typedef struct asn1_string_st ASN1_BIT_STRING;
typedef struct asn1_string_st ASN1_OCTET_STRING;
typedef struct asn1_string_st ASN1_PRINTABLESTRING;
typedef struct asn1_string_st ASN1_T61STRING;
typedef struct asn1_string_st ASN1_VISIBLESTRING;
typedef struct asn1_string_st ASN1_UTF8STRING;
typedef int ASN1_BOOLEAN;
struct bignum_st;
typedef struct bignum_st BIGNUM;
struct bignum_ctx;
typedef struct bignum_ctx BN_CTX;
struct bn_blinding_st;
typedef struct bn_blinding_st BN_BLINDING;
struct bn_mont_ctx_st;
typedef struct bn_mont_ctx_st BN_MONT_CTX;
struct bn_gencb_st;
typedef struct bn_gencb_st BN_GENCB;
struct buf_mem_st;
typedef struct buf_mem_st BUF_MEM;
struct evp_cipher_st;
typedef struct evp_cipher_st EVP_CIPHER;
struct evp_cipher_ctx_st;
typedef struct evp_cipher_ctx_st EVP_CIPHER_CTX;
struct env_md_st;
typedef struct env_md_st EVP_MD;
struct env_md_ctx_st;
typedef struct env_md_ctx_st EVP_MD_CTX;
struct evp_pkey_st;
typedef struct evp_pkey_st EVP_PKEY;
struct dh_st;
typedef struct dh_st DH;
struct dh_method;
typedef struct dh_method DH_METHOD;
struct dsa_st;
typedef struct dsa_st DSA;
struct dsa_method;
typedef struct dsa_method DSA_METHOD;
struct rsa_st;
typedef struct rsa_st RSA;
struct rsa_meth_st;
typedef struct rsa_meth_st RSA_METHOD;
struct x509_st;
typedef struct x509_st X509;
struct X509_algor_st;
typedef struct X509_algor_st X509_ALGOR;
struct X509_crl_st;
typedef struct X509_crl_st X509_CRL;
struct X509_name_st;
typedef struct X509_name_st X509_NAME;
struct x509_store_st;
typedef struct x509_store_st X509_STORE;
struct x509_store_ctx_st;
typedef struct x509_store_ctx_st X509_STORE_CTX;
struct engine_st;
typedef struct engine_st ENGINE;
struct X509_POLICY_TREE_st;
typedef struct X509_POLICY_TREE_st X509_POLICY_TREE;
typedef struct crypto_ex_data_st CRYPTO_EX_DATA;
struct bio_st;
struct crypto_ex_data_st
{
  STACK *sk;
  int dummy;
};
typedef struct bio_st BIO;
typedef void bio_info_cb (struct bio_st *, int, char const *, int, long,
			  long);
struct bio_method_st
{
  int type;
  char const *name;
  int (*bwrite) (BIO *, char const *, int);
  int (*bread) (BIO *, char *, int);
  int (*bputs) (BIO *, char const *);
  int (*bgets) (BIO *, char *, int);
  long (*ctrl) (BIO *, int, long, void *);
  int (*create) (BIO *);
  int (*destroy) (BIO *);
  long (*callback_ctrl) (BIO *, int, bio_info_cb *);
};
typedef struct bio_method_st BIO_METHOD;
struct bio_st
{
  BIO_METHOD *method;
  long (*callback) (struct bio_st *, int, char const *, int, long, long);
  char *cb_arg;
  int init;
  int shutdown;
  int flags;
  int retry_reason;
  int num;
  void *ptr;
  struct bio_st *next_bio;
  struct bio_st *prev_bio;
  int references;
  unsigned long num_read;
  unsigned long num_write;
  CRYPTO_EX_DATA ex_data;
};
struct lhash_node_st
{
  void *data;
  struct lhash_node_st *next;
  unsigned long hash;
};
typedef struct lhash_node_st LHASH_NODE;
struct lhash_st
{
  unsigned long up_load;
  unsigned long down_load;
  unsigned long num_items;
  unsigned long num_expands;
  unsigned long num_hash_comps;
  int error;
};
struct comp_ctx_st;
typedef struct comp_ctx_st COMP_CTX;
struct comp_method_st
{
  int type;
  int (*expand) (COMP_CTX * ctx, unsigned char *out, unsigned int olen,
		 unsigned char *in, unsigned int ilen);
  long (*ctrl) (void);
  long (*callback_ctrl) (void);
};
typedef struct comp_method_st COMP_METHOD;
struct comp_ctx_st
{
  COMP_METHOD *meth;
  unsigned long compress_in;
  CRYPTO_EX_DATA ex_data;
};
struct buf_mem_st
{
  int length;
  char *data;
  int max;
};
struct bignum_st
{
  unsigned long *d;
  int top;
  int dmax;
  int neg;
  unsigned long n0;
  int flags;
};
union __anonunion_bn_gencb_st_cb_69
{
  void (*cb_1) (int, int, void *);
  int (*cb_2) (int, int, BN_GENCB *);
};
struct bn_gencb_st
{
  unsigned int ver;
  void *arg;
  union __anonunion_bn_gencb_st_cb_69 cb;
};
struct asn1_object_st
{
  char const *sn;
  char const *ln;
  int nid;
  int length;
  unsigned char *data;
  int flags;
};
typedef struct asn1_object_st ASN1_OBJECT;
struct asn1_string_st
{
  int length;
  int type;
  unsigned char *data;
  long flags;
};
typedef struct asn1_string_st ASN1_STRING;
struct ASN1_ENCODING_st
{
  unsigned char *enc;
  long len;
  int modified;
};
typedef struct ASN1_ENCODING_st ASN1_ENCODING;
union __anonunion_asn1_type_st_value_70
{
  char *ptr;
  ASN1_BOOLEAN boolean;
  ASN1_STRING *asn1_string;
  ASN1_STRING *set;
  ASN1_STRING *sequence;
};
struct asn1_type_st
{
  int type;
  union __anonunion_asn1_type_st_value_70 value;
};
typedef struct asn1_type_st ASN1_TYPE;
struct ec_key_st;
union __anonunion_evp_pkey_st_pkey_71
{
  char *ptr;
  struct rsa_st *rsa;
  struct dsa_st *dsa;
  struct dh_st *dh;
  struct ec_key_st *ec;
};
struct evp_pkey_st
{
  int type;
  int save_type;
  int references;
  union __anonunion_evp_pkey_st_pkey_71 pkey;
  int save_parameters;
  STACK *attributes;
};
struct env_md_st
{
  int type;
  int pkey_type;
  int md_size;
  unsigned long flags;
  int (*init) (EVP_MD_CTX * ctx);
  int (*update) (EVP_MD_CTX * ctx, void const *data, size_t count);
  int (*final) (EVP_MD_CTX * ctx, unsigned char *md);
  int (*copy) (EVP_MD_CTX * to, EVP_MD_CTX const *from);
  int (*cleanup) (EVP_MD_CTX * ctx);
  int (*sign) (int type, unsigned char const *m, unsigned int m_length,
	       unsigned char *sigret, unsigned int *siglen, void *key);
  int (*verify) (int type, unsigned char const *m, unsigned int m_length,
		 unsigned char const *sigbuf, unsigned int siglen, void *key);
  int required_pkey_type[5];
  int block_size;
  int ctx_size;
};
struct env_md_ctx_st
{
  EVP_MD const *digest;
  ENGINE *engine;
  unsigned long flags;
  void *md_data;
};
struct evp_cipher_st
{
  int nid;
  int block_size;
  int key_len;
  int iv_len;
  unsigned long flags;
  int (*init) (EVP_CIPHER_CTX * ctx, unsigned char const *key,
	       unsigned char const *iv, int enc);
  int (*do_cipher) (EVP_CIPHER_CTX * ctx, unsigned char *out,
		    unsigned char const *in, unsigned int inl);
  int (*cleanup) (EVP_CIPHER_CTX *);
  int ctx_size;
};
struct evp_cipher_ctx_st
{
  EVP_CIPHER const *cipher;
  ENGINE *engine;
  int encrypt;
  int buf_len;
  unsigned char oiv[16];
  unsigned char iv[16];
  unsigned char buf[32];
  int num;
  void *app_data;
  int key_len;
  unsigned long flags;
  void *cipher_data;
  int final_used;
  int block_mask;
  unsigned char final[32];
};
typedef struct ec_key_st EC_KEY;
struct rsa_meth_st
{
  char const *name;
  int (*rsa_pub_enc) (int flen, unsigned char const *from, unsigned char *to,
		      RSA * rsa, int padding);
  int (*rsa_pub_dec) (int flen, unsigned char const *from, unsigned char *to,
		      RSA * rsa, int padding);
  int (*rsa_priv_enc) (int flen, unsigned char const *from, unsigned char *to,
		       RSA * rsa, int padding);
  int (*rsa_priv_dec) (int flen, unsigned char const *from, unsigned char *to,
		       RSA * rsa, int padding);
  int (*rsa_mod_exp) (BIGNUM * r0, BIGNUM const *I, RSA * rsa, BN_CTX * ctx);
  int (*bn_mod_exp) (BIGNUM * r, BIGNUM const *a, BIGNUM const *p,
		     BIGNUM const *m, BN_CTX * ctx, BN_MONT_CTX * m_ctx);
  int (*init) (RSA * rsa);
  int (*finish) (RSA * rsa);
  int flags;
  char *app_data;
  int (*rsa_sign) (int type, unsigned char const *m, unsigned int m_length,
		   unsigned char *sigret, unsigned int *siglen,
		   RSA const *rsa);
  int (*rsa_verify) (int dtype, unsigned char const *m, unsigned int m_length,
		     unsigned char *sigbuf, unsigned int siglen,
		     RSA const *rsa);
  int (*rsa_keygen) (RSA * rsa, int bits, BIGNUM * e, BN_GENCB * cb);
};
struct rsa_st
{
  int pad;
  long version;
  RSA_METHOD const *meth;
  ENGINE *engine;
  BIGNUM *n;
  BIGNUM *e;
  BIGNUM *d;
  BIGNUM *p;
  BIGNUM *q;
  BIGNUM *dmp1;
  BIGNUM *dmq1;
  BIGNUM *iqmp;
  CRYPTO_EX_DATA ex_data;
  int references;
  int flags;
  BN_MONT_CTX *_method_mod_n;
  BN_MONT_CTX *_method_mod_p;
  BN_MONT_CTX *_method_mod_q;
  char *bignum_data;
  BN_BLINDING *blinding;
  BN_BLINDING *mt_blinding;
};
struct dh_method
{
  char const *name;
  int (*init) (DH * dh);
  int (*finish) (DH * dh);
  int flags;
  char *app_data;
  int (*generate_params) (DH * dh, int prime_len, int generator,
			  BN_GENCB * cb);
};
struct dh_st
{
  int pad;
  int version;
  BIGNUM *p;
  BIGNUM *g;
  long length;
  BIGNUM *pub_key;
  BIGNUM *priv_key;
  int flags;
  BN_MONT_CTX *method_mont_p;
  BIGNUM *q;
  BIGNUM *j;
  unsigned char *seed;
  int seedlen;
  BIGNUM *counter;
  int references;
  CRYPTO_EX_DATA ex_data;
  DH_METHOD const *meth;
  ENGINE *engine;
};
struct dsa_st
{
  int pad;
  long version;
  DSA_METHOD const *meth;
  ENGINE *engine;
};
struct X509_algor_st
{
  ASN1_OBJECT *algorithm;
  ASN1_TYPE *parameter;
};
typedef struct X509_val_st X509_VAL;
struct X509_pubkey_st
{
  X509_ALGOR *algor;
  ASN1_BIT_STRING *public_key;
  EVP_PKEY *pkey;
};
typedef struct X509_pubkey_st X509_PUBKEY;
struct X509_name_st
{
  STACK *entries;
  int modified;
  BUF_MEM *bytes;
  ASN1_BIT_STRING *subjectUID;
  STACK *extensions;
};
typedef struct x509_cinf_st X509_CINF;
struct AUTHORITY_KEYID_st;
struct x509_st
{
  X509_CINF *cert_info;
  X509_ALGOR *sig_alg;
  ASN1_BIT_STRING *signature;
  int valid;
  int references;
  unsigned long ex_flags;
  unsigned long ex_kusage;
  unsigned long ex_xkusage;
  unsigned long ex_nscert;
};
typedef struct X509_VERIFY_PARAM_st X509_VERIFY_PARAM;
struct x509_store_st
{
  int cache;
  int (*cleanup) (X509_STORE_CTX * ctx);
  CRYPTO_EX_DATA ex_data;
  int references;
};
struct x509_store_ctx_st
{
  X509_STORE *ctx;
  int current_method;
  X509 *current_issuer;
  X509_CRL *current_crl;
  CRYPTO_EX_DATA ex_data;
};
typedef int pem_password_cb (char *buf, int size, int rwflag, void *userdata);
struct ssl_st;
struct ssl_cipher_st
{
  int valid;
  char const *name;
  unsigned long id;
  unsigned long algorithms;
  unsigned long algo_strength;
  unsigned long algorithm2;
  int strength_bits;
  int alg_bits;
  unsigned long mask;
  unsigned long mask_strength;
};
typedef struct ssl_cipher_st SSL_CIPHER;
typedef struct ssl_st SSL;
struct ssl_ctx_st;
typedef struct ssl_ctx_st SSL_CTX;
struct ssl3_enc_method;
struct ssl_method_st
{
  int version;
  int (*ssl_new) (SSL * s);
  void (*ssl_clear) (SSL * s);
  void (*ssl_free) (SSL * s);
  int (*ssl_accept) (SSL * s);
  int (*ssl_connect) (SSL * s);
  int (*ssl_read) (SSL * s, void *buf, int len);
  int (*ssl_peek) (SSL * s, void *buf, int len);
  int (*ssl_write) (SSL * s, void const *buf, int len);
  int (*ssl_shutdown) (SSL * s);
  int (*ssl_renegotiate) (SSL * s);
  int (*ssl_renegotiate_check) (SSL * s);
  long (*ssl_get_message) (SSL * s, int st1, int stn, int mt, long max,
			   int *ok);
  int (*ssl_read_bytes) (SSL * s, int type, unsigned char *buf, int len,
			 int peek);
  int (*ssl_write_bytes) (SSL * s, int type, void const *buf_, int len);
  int (*ssl_dispatch_alert) (SSL * s);
  long (*ssl_ctrl) (SSL * s, int cmd, long larg, void *parg);
  long (*ssl_ctx_ctrl) (SSL_CTX * ctx, int cmd, long larg, void *parg);
  SSL_CIPHER *(*get_cipher_by_char) (unsigned char const *ptr);
  int (*put_cipher_by_char) (SSL_CIPHER const *cipher, unsigned char *ptr);
  int (*ssl_pending) (SSL const *s);
  int (*num_ciphers) (void);
  SSL_CIPHER *(*get_cipher) (unsigned int ncipher);
  struct ssl_method_st *(*get_ssl_method) (int version);
  long (*get_timeout) (void);
  struct ssl3_enc_method *ssl3_enc;
  int (*ssl_version) (void);
  long (*ssl_callback_ctrl) (SSL * s, int cb_id, void (*fp) (void));
  long (*ssl_ctx_callback_ctrl) (SSL_CTX * s, int cb_id, void (*fp) (void));
};
typedef struct ssl_method_st SSL_METHOD;
struct sess_cert_st;
struct ssl_session_st
{
  int ssl_version;
  unsigned int key_arg_length;
  unsigned char key_arg[8];
  int master_key_length;
  unsigned char master_key[48];
  unsigned int session_id_length;
  unsigned char session_id[32];
  unsigned int sid_ctx_length;
  unsigned char sid_ctx[32];
  int not_resumable;
  struct sess_cert_st *sess_cert;
  X509 *peer;
  long verify_result;
  int references;
  long timeout;
  long time;
  int compress_meth;
  SSL_CIPHER *cipher;
  unsigned long cipher_id;
  STACK *ciphers;
  CRYPTO_EX_DATA ex_data;
  struct ssl_session_st *prev;
  struct ssl_session_st *next;
};
typedef struct ssl_session_st SSL_SESSION;
struct ssl_comp_st
{
  int id;
  char const *name;
  COMP_METHOD *method;
};
typedef struct ssl_comp_st SSL_COMP;
struct __anonstruct_ssl_ctx_st_stats_80
{
  int sess_connect;
  int sess_connect_renegotiate;
  int sess_connect_good;
  int sess_accept;
  int sess_accept_renegotiate;
  int sess_accept_good;
  int sess_miss;
  int sess_timeout;
  int sess_cache_full;
  int sess_hit;
  int sess_cb_hit;
};
struct cert_st;
struct ssl_ctx_st
{
  SSL_METHOD *method;
  STACK *cipher_list;
  STACK *cipher_list_by_id;
  struct x509_store_st *cert_store;
  struct lhash_st *sessions;
  unsigned long session_cache_size;
  struct ssl_session_st *session_cache_head;
  struct ssl_session_st *session_cache_tail;
  int session_cache_mode;
  long session_timeout;
  int (*new_session_cb) (struct ssl_st * ssl, SSL_SESSION * sess);
  void (*remove_session_cb) (struct ssl_ctx_st * ctx, SSL_SESSION * sess);
  SSL_SESSION *(*get_session_cb) (struct ssl_st * ssl, unsigned char *data,
				  int len, int *copy);
  struct __anonstruct_ssl_ctx_st_stats_80 stats;
  int references;
  int (*app_verify_callback) (X509_STORE_CTX *, void *);
  void *app_verify_arg;
  pem_password_cb *default_passwd_callback;
  void *default_passwd_callback_userdata;
  int (*client_cert_cb) (SSL * ssl, X509 ** x509, EVP_PKEY ** pkey);
  int (*app_gen_cookie_cb) (SSL * ssl, unsigned char *cookie,
			    unsigned int *cookie_len);
  int (*app_verify_cookie_cb) (SSL * ssl, unsigned char *cookie,
			       unsigned int cookie_len);
  CRYPTO_EX_DATA ex_data;
  EVP_MD const *rsa_md5;
  EVP_MD const *md5;
  EVP_MD const *sha1;
  STACK *extra_certs;
  STACK *comp_methods;
  void (*info_callback) (SSL const *ssl, int type, int val);
  STACK *client_CA;
  unsigned long options;
  unsigned long mode;
  long max_cert_list;
  struct cert_st *cert;
  int read_ahead;
  void (*msg_callback) (int write_p, int version, int content_type,
			void const *buf, size_t len, SSL * ssl, void *arg);
  void *msg_callback_arg;
  int verify_mode;
  unsigned int sid_ctx_length;
  unsigned char sid_ctx[32];
  int (*default_verify_callback) (int ok, X509_STORE_CTX * ctx);
  int (*generate_session_id) (SSL const *ssl, unsigned char *id,
			      unsigned int *id_len);
  X509_VERIFY_PARAM *param;
  int quiet_shutdown;
};
struct dtls1_state_st;
struct ssl_st
{
  int version;
  int type;
  SSL_METHOD *method;
  BIO *rbio;
  BIO *wbio;
  BIO *bbio;
  int rwstate;
  int in_handshake;
  int (*handshake_func) (SSL *);
  int server;
  int new_session;
  int quiet_shutdown;
  int shutdown;
  int state;
  int rstate;
  BUF_MEM *init_buf;
  void *init_msg;
  int init_num;
  int init_off;
  unsigned char *packet;
  unsigned int packet_length;
  struct ssl2_state_st *s2;
  struct ssl3_state_st *s3;
  struct dtls1_state_st *d1;
  int read_ahead;
  void (*msg_callback) (int write_p, int version, int content_type,
			void const *buf, size_t len, SSL * ssl, void *arg);
  void *msg_callback_arg;
  int hit;
  X509_VERIFY_PARAM *param;
  STACK *cipher_list;
  STACK *cipher_list_by_id;
  EVP_CIPHER_CTX *enc_read_ctx;
  EVP_MD const *read_hash;
  COMP_CTX *expand;
  EVP_CIPHER_CTX *enc_write_ctx;
  EVP_MD const *write_hash;
  COMP_CTX *compress;
  struct cert_st *cert;
  unsigned int sid_ctx_length;
  unsigned char sid_ctx[32];
  SSL_SESSION *session;
  int (*generate_session_id) (SSL const *ssl, unsigned char *id,
			      unsigned int *id_len);
  int verify_mode;
  int (*verify_callback) (int ok, X509_STORE_CTX * ctx);
  void (*info_callback) (SSL const *ssl, int type, int val);
  int error;
  int error_code;
  SSL_CTX *ctx;
  int debug;
  long verify_result;
  CRYPTO_EX_DATA ex_data;
  STACK *client_CA;
  int references;
  unsigned long options;
  unsigned long mode;
  long max_cert_list;
  int first_packet;
  int client_version;
};
struct __anonstruct_ssl2_state_st_tmp_81
{
  unsigned int conn_id_length;
  unsigned int cert_type;
  unsigned int cert_length;
  unsigned int rlen;
};
struct ssl2_state_st
{
  int three_byte_header;
  int clear_text;
  int escape;
  int ssl2_rollback;
  unsigned int wnum;
  int wpend_tot;
  unsigned char const *wpend_buf;
  int wpend_off;
  int wpend_len;
  int wpend_ret;
  int rbuf_left;
  int rbuf_offs;
  unsigned char *rbuf;
  unsigned char *wbuf;
  unsigned char *write_ptr;
  unsigned int padding;
  unsigned int rlength;
  int ract_data_length;
  unsigned int wlength;
  int wact_data_length;
  unsigned char *ract_data;
  unsigned char *wact_data;
  unsigned char *mac_data;
  unsigned char *read_key;
  unsigned char *write_key;
  unsigned int challenge_length;
  unsigned char challenge[32];
  unsigned int conn_id_length;
  unsigned char conn_id[16];
  unsigned int key_material_length;
  unsigned char key_material[48];
  unsigned long read_sequence;
  unsigned long write_sequence;
  struct __anonstruct_ssl2_state_st_tmp_81 tmp;
};
struct ssl3_record_st
{
  int type;
  unsigned int length;
  unsigned int off;
  unsigned char *data;
  unsigned char *input;
  unsigned char *comp;
  unsigned long epoch;
  unsigned long long seq_num;
};
typedef struct ssl3_record_st SSL3_RECORD;
struct ssl3_buffer_st
{
  unsigned char *buf;
  size_t len;
  int offset;
  int left;
};
typedef struct ssl3_buffer_st SSL3_BUFFER;
struct __anonstruct_ssl3_state_st_tmp_82
{
  unsigned char cert_verify_md[128];
  unsigned char finish_md[128];
  int finish_md_len;
  unsigned char peer_finish_md[128];
  int peer_finish_md_len;
  unsigned long message_size;
  int message_type;
  SSL_CIPHER *new_cipher;
  DH *dh;
  EC_KEY *ecdh;
  int next_state;
  int reuse_message;
  int cert_req;
  int ctype_num;
  char ctype[7];
  STACK *ca_names;
  int use_rsa_tmp;
  int key_block_length;
  unsigned char *key_block;
  EVP_CIPHER const *new_sym_enc;
  EVP_MD const *new_hash;
  SSL_COMP const *new_compression;
  int cert_request;
};
struct ssl3_state_st
{
  long flags;
  int delay_buf_pop_ret;
  unsigned char read_sequence[8];
  unsigned char read_mac_secret[64];
  unsigned char write_sequence[8];
  unsigned char write_mac_secret[64];
  unsigned char server_random[32];
  unsigned char client_random[32];
  int need_empty_fragments;
  int empty_fragment_done;
  SSL3_BUFFER rbuf;
  SSL3_BUFFER wbuf;
  SSL3_RECORD rrec;
  SSL3_RECORD wrec;
  unsigned char alert_fragment[2];
  unsigned int alert_fragment_len;
  unsigned char handshake_fragment[4];
  unsigned int handshake_fragment_len;
  unsigned int wnum;
  int wpend_tot;
  int wpend_type;
  int wpend_ret;
  unsigned char const *wpend_buf;
  EVP_MD_CTX finish_dgst1;
  EVP_MD_CTX finish_dgst2;
  int change_cipher_spec;
  int warn_alert;
  int fatal_alert;
  int alert_dispatch;
  unsigned char send_alert[2];
  int renegotiate;
  int total_renegotiations;
  int num_renegotiations;
  int in_read_app_data;
  struct __anonstruct_ssl3_state_st_tmp_82 tmp;
};
struct _pqueue;
typedef struct _pqueue *pqueue;
struct dtls1_bitmap_st
{
  unsigned long long map;
  unsigned long length;
  unsigned long long max_seq_num;
};
typedef struct dtls1_bitmap_st DTLS1_BITMAP;
struct hm_header_st
{
  unsigned char type;
  unsigned long msg_len;
  unsigned short seq;
  unsigned long frag_off;
  unsigned long frag_len;
  unsigned int is_ccs;
};
struct dtls1_timeout_st
{
  unsigned int read_timeouts;
  unsigned int write_timeouts;
  unsigned int num_alerts;
};
struct record_pqueue_st
{
  unsigned short epoch;
  pqueue q;
};
typedef struct record_pqueue_st record_pqueue;
struct dtls1_state_st
{
  unsigned int send_cookie;
  unsigned char cookie[32];
  unsigned char rcvd_cookie[32];
  unsigned int cookie_len;
  unsigned short r_epoch;
  unsigned short w_epoch;
  DTLS1_BITMAP bitmap;
  DTLS1_BITMAP next_bitmap;
  unsigned short handshake_write_seq;
  unsigned short next_handshake_write_seq;
  unsigned short handshake_read_seq;
  record_pqueue unprocessed_rcds;
  unsigned int mtu;
  struct hm_header_st w_msg_hdr;
  struct hm_header_st r_msg_hdr;
  struct dtls1_timeout_st timeout;
  unsigned char alert_fragment[7];
  unsigned int alert_fragment_len;
  unsigned char handshake_fragment[12];
  unsigned int handshake_fragment_len;
  unsigned int retransmitting;
};
typedef STACK GENERAL_NAMES;
struct AUTHORITY_KEYID_st
{
};
union sockaddr_union
{
  struct sockaddr sa;
  struct sockaddr_in in;
  struct sockaddr_in6 in6;
};
typedef union sockaddr_union SOCKADDR_UNION;
struct sockaddr_list
{
  SOCKADDR_UNION addr[16];
  u16 cur;
  u16 num;
};
typedef struct sockaddr_list SOCKADDR_LIST;
enum __anonenum_COMP_TYPE_93
{
  COMP_NONE = 0, COMP_ZLIB = 1, COMP_RLE = 2
}
 ;
typedef enum __anonenum_COMP_TYPE_93 COMP_TYPE;
struct __anonstruct___anonstruct_GLOBAL_OPTIONS_94_option_95
{
  unsigned int rand_write:1;
  unsigned int foreground:1;
  unsigned int syslog:1;
};
struct __anonstruct_GLOBAL_OPTIONS_94
{
  COMP_TYPE compression;
  char *egd_sock;
  char *rand_file;
  int random_bytes;
  char *chroot_dir;
  unsigned long dpid;
  char *pidfile;
  int uid;
  int gid;
  int debug_level;
  int facility;
  char *output_file;
  struct __anonstruct___anonstruct_GLOBAL_OPTIONS_94_option_95 option;
};
typedef struct __anonstruct_GLOBAL_OPTIONS_94 GLOBAL_OPTIONS;
enum __anonenum_local_options_failover_96
{
  FAILOVER_RR = 0, FAILOVER_PRIO = 1
}
 ;
struct __anonstruct_local_options_option_97
{
  unsigned int cert:1;
  unsigned int client:1;
  unsigned int delayed_lookup:1;
  unsigned int accept:1;
  unsigned int remote:1;
  unsigned int retry:1;
  unsigned int program:1;
  unsigned int pty:1;
  unsigned int transparent:1;
  unsigned int ocsp:1;
};
struct local_options
{
  SSL_CTX *ctx;
  X509_STORE *revocation_store;
  ENGINE *engine;
  struct local_options *next;
  char *servname;
  SSL_SESSION *session;
  char local_address[128];
  int stack_size;
  char *ca_dir;
  char *ca_file;
  char *crl_dir;
  char *crl_file;
  char *cipher_list;
  char *cert;
  char *key;
  long session_timeout;
  int verify_level;
  int verify_use_only_my;
  long ssl_options;
  SOCKADDR_LIST ocsp_addr;
  char *ocsp_path;
  unsigned long ocsp_flags;
  SSL_METHOD *client_method;
  SSL_METHOD *server_method;
  int fd;
  char *execname;
  char **execargs;
  SOCKADDR_LIST local_addr;
  SOCKADDR_LIST remote_addr;
  SOCKADDR_LIST source_addr;
  char *username;
  char *remote_address;
  int timeout_busy;
  int timeout_close;
  int timeout_connect;
  int timeout_idle;
  enum __anonenum_local_options_failover_96 failover;
  char *protocol;
  char *protocol_host;
  char *protocol_username;
  char *protocol_password;
  char *protocol_authentication;
  struct __anonstruct_local_options_option_97 option;
};
typedef struct local_options LOCAL_OPTIONS;
typedef struct __anonstruct_CLI_103 CLI;
enum __anonenum_SECTION_CODE_104
{
  CRIT_KEYGEN = 0, CRIT_INET = 1, CRIT_CLIENTS = 2, CRIT_WIN_LOG =
    3, CRIT_SESSION = 4, CRIT_LIBWRAP = 5, CRIT_SSL = 6, CRIT_SECTIONS = 7
}
 ;
typedef enum __anonenum_SECTION_CODE_104 SECTION_CODE;
struct tm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
  __gid_t gr_gid;
  char **gr_mem;
};
enum __anonenum_VAL_TYPE_98
{
  TYPE_NONE = 0, TYPE_FLAG = 1, TYPE_INT = 2, TYPE_LINGER = 3, TYPE_TIMEVAL =
    4, TYPE_STRING = 5
}
 ;
typedef enum __anonenum_VAL_TYPE_98 VAL_TYPE;
typedef struct __anonstruct_SOCK_OPT_100 SOCK_OPT;
enum __anonenum_CMD_106
{
  CMD_INIT = 0, CMD_EXEC = 1, CMD_DEFAULT = 2, CMD_HELP = 3
}
 ;
typedef enum __anonenum_CMD_106 CMD;
typedef int CRYPTO_EX_new (void *parent, void *ptr, CRYPTO_EX_DATA * ad,
			   int idx, long argl, void *argp);
typedef void CRYPTO_EX_free (void *parent, void *ptr, CRYPTO_EX_DATA * ad,
			     int idx, long argl, void *argp);
typedef int CRYPTO_EX_dup (CRYPTO_EX_DATA * to, CRYPTO_EX_DATA * from,
			   void *from_d, int idx, long argl, void *argp);
struct ui_method_st;
typedef struct ui_method_st UI_METHOD;
struct __anonstruct_UI_DATA_105
{
  LOCAL_OPTIONS *section;
  char pass[1024];
};
typedef struct __anonstruct_UI_DATA_105 UI_DATA;
struct keytabstruct
{
  RSA *key;
  time_t timeout;
};
struct X509_revoked_st
{
  int sequence;
};
typedef struct X509_revoked_st X509_REVOKED;
union __anonunion_x509_object_st_data_76
{
  char *ptr;
  X509 *x509;
  X509_CRL *crl;
  EVP_PKEY *pkey;
};
struct x509_object_st
{
  int type;
  union __anonunion_x509_object_st_data_76 data;
};
typedef struct x509_object_st X509_OBJECT;
struct cert_pkey_st
{
  X509 *x509;
  EVP_PKEY *privatekey;
};
typedef struct cert_pkey_st CERT_PKEY;
struct cert_st
{
  CERT_PKEY *key;
  int valid;
  unsigned long mask;
  unsigned long export_mask;
  RSA *rsa_tmp;
  RSA *(*rsa_tmp_cb) (SSL * ssl, int is_export, int keysize);
  DH *dh_tmp;
  DH *(*dh_tmp_cb) (SSL * ssl, int is_export, int keysize);
  EC_KEY *ecdh_tmp;
  EC_KEY *(*ecdh_tmp_cb) (SSL * ssl, int is_export, int keysize);
  CERT_PKEY pkeys[6];
  int references;
};
struct sess_cert_st
{
  STACK *cert_chain;
  int peer_cert_type;
  CERT_PKEY *peer_key;
  CERT_PKEY peer_pkeys[6];
  RSA *peer_rsa_tmp;
  DH *peer_dh_tmp;
  EC_KEY *peer_ecdh_tmp;
  int references;
};
struct ssl3_enc_method
{
  int (*enc) (SSL *, int);
  int (*mac) (SSL *, unsigned char *, int);
  int (*setup_key_block) (SSL *);
  int (*generate_master_secret) (SSL *, unsigned char *, unsigned char *,
				 int);
  int (*change_cipher_state) (SSL *, int);
  int (*final_finish_mac) (SSL *, EVP_MD_CTX *, EVP_MD_CTX *, char const *,
			   int, unsigned char *);
  int finish_mac_length;
  int (*cert_verify_mac) (SSL *, EVP_MD_CTX *, unsigned char *);
  char const *client_finished_label;
  int client_finished_label_len;
  char const *server_finished_label;
  int server_finished_label_len;
  int (*alert_value) (int);
};
typedef struct ssl3_enc_method SSL3_ENC_METHOD;
typedef int rwlock_t;
typedef struct cert_st CERT;
typedef struct sess_cert_st SESS_CERT;
typedef struct ssl2_state_st SSL2_STATE;
typedef void *d2i_of_void (void **, unsigned char const **, long);
typedef int i2d_of_void (void *, unsigned char **);
enum __anonenum_point_conversion_form_t_29
{
  POINT_CONVERSION_COMPRESSED = 2, POINT_CONVERSION_UNCOMPRESSED =
    4, POINT_CONVERSION_HYBRID = 6
}
 ;
typedef enum __anonenum_point_conversion_form_t_29 point_conversion_form_t;
struct ec_group_st;
typedef struct ec_group_st EC_GROUP;
struct ec_point_st;
typedef struct ec_point_st EC_POINT;
typedef struct ssl3_state_st SSL3_STATE;
struct hmac_ctx_st
{
  EVP_MD const *md;
  EVP_MD_CTX md_ctx;
  EVP_MD_CTX i_ctx;
  EVP_MD_CTX o_ctx;
  unsigned int key_length;
  unsigned char key[128];
};
typedef struct hmac_ctx_st HMAC_CTX;
struct _pitem
{
  unsigned long long priority;
  void *data;
  struct _pitem *next;
  struct hm_header_st msg_header;
  unsigned char *fragment;
};
typedef struct hm_fragment_st hm_fragment;
typedef struct lhash_st LHASH;
struct OPENSSL_dir_context_st;
typedef struct OPENSSL_dir_context_st OPENSSL_DIR_CTX;
struct timeout_param_st
{
  SSL_CTX *ctx;
  long time;
  LHASH *cache;
};
typedef struct timeout_param_st TIMEOUT_PARAM;
struct cipher_order_st
{
  SSL_CIPHER *cipher;
};
typedef struct ssl_session_asn1_st SSL_SESSION_ASN1;
struct bio_ssl_st
{
  SSL *ssl;
  int num_renegotiates;
  unsigned long renegotiate_count;
  unsigned long byte_count;
  unsigned long renegotiate_timeout;
  unsigned long last_time;
};
typedef struct bio_ssl_st BIO_SSL;
struct crypto_ex_data_func_st
{
  long argl;
  void *argp;
  CRYPTO_EX_new *new_func;
  CRYPTO_EX_free *free_func;
  CRYPTO_EX_dup *dup_func;
};
typedef struct crypto_ex_data_func_st CRYPTO_EX_DATA_FUNCS;
struct st_CRYPTO_EX_DATA_IMPL;
typedef struct st_CRYPTO_EX_DATA_IMPL CRYPTO_EX_DATA_IMPL;
struct st_CRYPTO_EX_DATA_IMPL
{
  int (*cb_new_class) (void);
  void (*cb_cleanup) (void);
  int (*cb_get_new_index) (int class_index, long argl, void *argp,
			   CRYPTO_EX_new * new_func, CRYPTO_EX_dup * dup_func,
			   CRYPTO_EX_free * free_func);
  int (*cb_new_ex_data) (int class_index, void *obj, CRYPTO_EX_DATA * ad);
  int (*cb_dup_ex_data) (int class_index, CRYPTO_EX_DATA * to,
			 CRYPTO_EX_DATA * from);
  void (*cb_free_ex_data) (int class_index, void *obj, CRYPTO_EX_DATA * ad);
};
struct st_ex_class_item
{
  int class_index;
  STACK *meth;
  int meth_num;
};
typedef struct st_ex_class_item EX_CLASS_ITEM;
typedef long __clock_t;
struct obj_name_st
{
  int type;
};
typedef struct obj_name_st OBJ_NAME;
struct doall_sorted
{
  int type;
  int n;
  OBJ_NAME const **names;
};
struct added_obj_st
{
  int type;
  ASN1_OBJECT *obj;
};
typedef struct added_obj_st ADDED_OBJ;
typedef struct cast_key_st CAST_KEY;
struct ec_method_st;
typedef struct ec_method_st EC_METHOD;
struct ec_method_st
{
  int (*field_set_to_one) (EC_GROUP const *, BIGNUM * r, BN_CTX *);
};
struct ec_extra_data_st
{
  struct ec_extra_data_st *next;
  void *data;
  void *(*dup_func) (void *);
  void (*free_func) (void *);
  void (*clear_free_func) (void *);
};
typedef struct ec_extra_data_st EC_EXTRA_DATA;
typedef struct ecpk_parameters_st ECPKPARAMETERS;
struct ec_key_st
{
  int version;
  EC_GROUP *group;
  EC_POINT *pub_key;
  BIGNUM *priv_key;
  unsigned int enc_flag;
  point_conversion_form_t conv_form;
  int references;
  EC_EXTRA_DATA *method_data;
};
typedef struct ec_privatekey_st EC_PRIVATEKEY;
struct X509_sig_st
{
  X509_ALGOR *algor;
  ASN1_OCTET_STRING *digest;
};
typedef struct X509_sig_st X509_SIG;
struct asn1_method_st
{
  i2d_of_void *i2d;
  d2i_of_void *d2i;
  void *(*create) (void);
  void (*destroy) (void *);
};
typedef struct asn1_method_st ASN1_METHOD;
struct ecdsa_method;
typedef struct ecdsa_method ECDSA_METHOD;
struct ECDSA_SIG_st
{
  BIGNUM *r;
  BIGNUM *s;
};
typedef struct ECDSA_SIG_st ECDSA_SIG;
struct ecdsa_method
{
  char const *name;
  ECDSA_SIG *(*ecdsa_do_sign) (unsigned char const *dgst, int dgst_len,
			       BIGNUM const *inv, BIGNUM const *rp,
			       EC_KEY * eckey);
  int (*ecdsa_sign_setup) (EC_KEY * eckey, BN_CTX * ctx, BIGNUM ** kinv,
			   BIGNUM ** r);
  int (*ecdsa_do_verify) (unsigned char const *dgst, int dgst_len,
			  ECDSA_SIG const *sig, EC_KEY * eckey);
  int flags;
  char *app_data;
};
struct ecdsa_data_st
{
  int (*init) (EC_KEY *);
  ENGINE *engine;
  int flags;
  ECDSA_METHOD const *meth;
  CRYPTO_EX_DATA ex_data;
};
typedef struct ecdsa_data_st ECDSA_DATA;
struct ecdh_method;
typedef struct ecdh_method ECDH_METHOD;
struct ecdh_method
{
  char const *name;
  int (*compute_key) (void *key, size_t outlen, EC_POINT const *pub_key,
		      EC_KEY * ecdh, void *(*KDF) (void const *in,
						   size_t inlen, void *out,
						   size_t * outlen));
  int flags;
  char *app_data;
};
struct ecdh_data_st
{
  int (*init) (EC_KEY *);
  ENGINE *engine;
  int flags;
  ECDH_METHOD const *meth;
  CRYPTO_EX_DATA ex_data;
};
typedef struct ecdh_data_st ECDH_DATA;
typedef void (*DSO_FUNC_TYPE) (void);
struct dso_st;
typedef struct dso_st DSO;
struct dso_meth_st
{
  char const *name;
  int (*init) (DSO * dso);
  int (*finish) (DSO * dso);
};
typedef struct dso_meth_st DSO_METHOD;
struct rand_meth_st;
typedef struct rand_meth_st RAND_METHOD;
struct store_method_st;
typedef struct store_method_st STORE_METHOD;
struct rand_meth_st
{
  void (*seed) (void const *buf, int num);
  int (*bytes) (unsigned char *buf, int num);
  void (*cleanup) (void);
  void (*add) (void const *buf, int num, double entropy);
  int (*pseudorand) (unsigned char *buf, int num);
  int (*status) (void);
};
struct ENGINE_CMD_DEFN_st
{
  unsigned int cmd_num;
  char const *cmd_name;
  char const *cmd_desc;
  unsigned int cmd_flags;
};
typedef struct ENGINE_CMD_DEFN_st ENGINE_CMD_DEFN;
typedef void ENGINE_CLEANUP_CB (void);
struct st_engine_cleanup_item
{
  ENGINE_CLEANUP_CB *cb;
};
typedef struct st_engine_cleanup_item ENGINE_CLEANUP_ITEM;
struct engine_st
{
  char const *id;
  char const *name;
  RSA_METHOD const *rsa_meth;
  DSA_METHOD const *dsa_meth;
  DH_METHOD const *dh_meth;
  ECDH_METHOD const *ecdh_meth;
  ECDSA_METHOD const *ecdsa_meth;
  RAND_METHOD const *rand_meth;
  STORE_METHOD const *store_meth;
  int (*ciphers) (ENGINE *, EVP_CIPHER const **, int const **, int);
  int (*digests) (ENGINE *, EVP_MD const **, int const **, int);
  int (*destroy) (ENGINE *);
  int (*init) (ENGINE *);
  int (*finish) (ENGINE *);
  int (*ctrl) (ENGINE *, int, long, void *, void (*f) (void));
  EVP_PKEY *(*load_privkey) (ENGINE *, char const *, UI_METHOD * ui_method,
			     void *callback_data);
  EVP_PKEY *(*load_pubkey) (ENGINE *, char const *, UI_METHOD * ui_method,
			    void *callback_data);
  ENGINE_CMD_DEFN const *cmd_defns;
  int flags;
  int struct_ref;
  int funct_ref;
  CRYPTO_EX_DATA ex_data;
  struct engine_st *prev;
  struct engine_st *next;
};
struct st_engine_table;
typedef struct st_engine_table ENGINE_TABLE;
struct st_engine_pile
{
  int nid;
  STACK *sk;
  ENGINE *funct;
  int uptodate;
};
typedef struct st_engine_pile ENGINE_PILE;
struct st_engine_table
{
  LHASH piles;
};
typedef EVP_PKEY *(*ENGINE_LOAD_KEY_PTR) (ENGINE *, char const *,
					  UI_METHOD * ui_method,
					  void *callback_data);
typedef struct st_ERR_FNS ERR_FNS;
struct st_dynamic_MEM_fns
{
  void *(*malloc_cb) (size_t);
  void *(*realloc_cb) (void *, size_t);
  void (*free_cb) (void *);
};
typedef struct st_dynamic_MEM_fns dynamic_MEM_fns;
struct st_dynamic_LOCK_fns
{
  void (*dynlock_destroy_cb) (struct CRYPTO_dynlock_value *, char const *,
			      int);
};
typedef struct st_dynamic_LOCK_fns dynamic_LOCK_fns;
struct st_dynamic_fns
{
  dynamic_LOCK_fns lock_fns;
};
typedef struct st_dynamic_fns dynamic_fns;
struct st_dynamic_data_ctx;
typedef struct st_dynamic_data_ctx dynamic_data_ctx;
struct st_dynamic_data_ctx
{
  DSO *dynamic_dso;
  unsigned long (*v_check) (unsigned long ossl_version);
  int (*bind_engine) (ENGINE * e, char const *id, dynamic_fns const *fns);
  char const *DYNAMIC_LIBNAME;
  int no_vcheck;
  char const *engine_id;
  int list_add_value;
  char const *DYNAMIC_F1;
  char const *DYNAMIC_F2;
  int dir_load;
  STACK *dirs;
};
struct __anonstruct___anonunion_padlock_cipher_data_cword_34_b_35
{
  int accept_nbio;
  char *addr;
  int nbio;
  int bind_mode;
  BIO *bio_chain;
};
typedef struct bio_accept_st BIO_ACCEPT;
struct __anonstruct_mapping_26
{
  int strl;
  unsigned int _errno;
  unsigned int mtu;
};
typedef struct bio_dgram_data_st bio_dgram_data;
typedef __pid_t pid_t;
struct sockaddr_un
{
  sa_family_t sun_family;
  char sun_path[108];
};
struct err_state_st
{
  unsigned long pid;
};
typedef struct err_state_st ERR_STATE;
extern struct servent *getservbyname (char const *__name,
				      char const *__proto);
void X509_free (X509 * a);
char *X509_NAME_oneline (X509_NAME * a, char *buf___5, int len);
int SSL_write (SSL * s, void const *buf___5, int num);
long SSL_ctrl (SSL * s, int cmd, long larg, void *parg);
int SSL_get_error (SSL const *s, int i);
extern __attribute__ ((__nothrow__))
     struct tm *
     localtime_r (time_t const *__restrict __timer,
		  struct tm *__restrict __tp);
     extern struct _IO_FILE *
       stderr;
     extern __attribute__ ((__nothrow__))
     void *
     malloc (size_t __size) __attribute__ ((__malloc__));
     extern __attribute__ ((__nothrow__))
     char *
     strerror (int __errnum);
     SSL_METHOD *
     SSLv2_client_method (void);
     SSL_METHOD *
     SSLv3_server_method (void);
     SSL_METHOD *
     SSLv3_client_method (void);
     SSL_METHOD *
     SSLv23_server_method (void);
     SSL_METHOD *
     SSLv23_client_method (void);
     SSL_METHOD *
     TLSv1_server_method (void);
     SSL_METHOD *
     TLSv1_client_method (void);
     ENGINE *
     get_engine (int i);
     GLOBAL_OPTIONS
       options;
     LOCAL_OPTIONS
       local_options;
     int
     hostport2addrlist (SOCKADDR_LIST * addr_list, char *hostname,
			char *portname);
     static void
     section_validate (char *filename, int line_number,
		       LOCAL_OPTIONS * section, int final___9);
     static void
     config_error (char *name, int num, char *str);
     static char *
       option_not_found = (char *) "Specified option name is not valid here";
     static char *
     global_options (CMD cmd, char *opt, char *arg)
{
  char *tmpstr;
  struct group *gr;
  struct passwd *pw;
  int tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  int tmp___4;
  int tmp___5;
  int tmp___6;
  int tmp___7;
  char *tmp___9;
  int tmp___27;
  int tmp___28;
  int tmp___29;
  int tmp___30;
  {
    if ((unsigned int) cmd == 2U)
      {
	s_log (-1, "Global options");
      }
    else
      {
	if ((unsigned int) cmd == 3U)
	  {
	    s_log (-1, "Global options");
	  }
      }
    switch ((int) cmd)
      {
      case 0:
	options.chroot_dir = (char *) ((void *) 0);
	break;
      case 1:
	tmp = strcasecmp ((char const *) opt, "chroot");
	if (tmp)
	  {
	    break;
	  }
	options.chroot_dir = stralloc (arg);
	return ((char *) ((void *) 0));
      case 2:
	break;
      case 3:
	s_log (-1, "%-15s = directory to chroot stunnel process", "chroot");
	break;
      }
    switch ((int) cmd)
      {
      case 0:
	options.compression = (enum __anonenum_COMP_TYPE_93) 0;
	break;
      case 1:
	tmp___0 = strcasecmp ((char const *) opt, "compression");
	if (tmp___0)
	  {
	    break;
	  }
	tmp___2 = strcasecmp ((char const *) arg, "zlib");
	if (tmp___2)
	  {
	    tmp___1 = strcasecmp ((char const *) arg, "rle");
	    if (tmp___1)
	      {
		return ((char *)
			"Compression type should be either \'zlib\' or \'rle\'");
	      }
	    else
	      {
		options.compression = (enum __anonenum_COMP_TYPE_93) 2;
	  }}
	else
	  {
	    options.compression = (enum __anonenum_COMP_TYPE_93) 1;
	  } return ((char *) ((void *) 0));
      case 2:
	break;
      case 3:
	s_log (-1, "%-15s = zlib|rle compression type", "compression");
	break;
      }
  }
}

static char *
service_options (CMD cmd, LOCAL_OPTIONS * section, char *opt, char *arg)
{
  int tmpnum;
  int tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  int tmp___4;
  int tmp___5;
  int tmp___6;
  int tmp___7;
  int tmp___8;
  int tmp___9;
  int tmp___10;
  int tmp___11;
  int tmp___38;
  int tmp___39;
  int tmp___40;
  int tmp___41;
  int tmp___42;
  int tmp___43;
  int tmp___44;
  int tmp___45;
  int tmp___46;
  SSL_METHOD *tmp___47;
  SSL_METHOD *tmp___48;
  int tmp___49;
  SSL_METHOD *tmp___50;
  SSL_METHOD *tmp___51;
  SSL_METHOD *tmp___52;
  SSL_METHOD *tmp___53;
  SSL_METHOD *tmp___54;
  SSL_METHOD *tmp___55;
  SSL_METHOD *tmp___56;
  SSL_METHOD *tmp___57;
  int tmp___58;
  int tmp___59;
  int tmp___60;
  int tmp___61;
  int tmp___62;
  int tmp___63;
  int tmp___64;
  int tmp___65;
  size_t __s1_len;
  size_t __s2_len;
  int tmp___67;
  int tmp___70;
  int tmp___71;
  int tmp___72;
  int tmp___73;
  int tmp___74;
  size_t __s1_len___0;
  size_t __s2_len___0;
  int tmp___76;
  int tmp___79;
  int tmp___80;
  int tmp___81;
  int tmp___82;
  int tmp___83;
  int tmp___84;
  int tmp___85;
  int tmp___86;
  int tmp___87;
  int tmp___88;
  {
    if ((unsigned int) cmd == 2U)
      {
	s_log (-1, " ");
	s_log (-1, "Service-level options");
      }
    else
      {
	if ((unsigned int) cmd == 3U)
	  {
	    s_log (-1, " ");
	    s_log (-1, "Service-level options");
	  }
      }
    switch ((int) cmd)
      {
      case 0:
	section->crl_file = (char *) ((void *) 0);
	break;
      case 1:
	tmp___11 = strcasecmp ((char const *) opt, "CRLfile");
	if (tmp___11)
	  {
	    break;
	  }
	if (*(arg + 0))
	  {
	    section->crl_file = stralloc (arg);
	  }
	tmp___44 = atoi ((char const *) arg);
	if (tmp___44 > 0)
	  {
	    tmp___43 = atoi ((char const *) arg);
	    section->session_timeout = (long) tmp___43;
	  }
	else
	  {
	    return ((char *) "Illegal session timeout");
	  } return ((char *) ((void *) 0));
      case 2:
	s_log (-1, "%-15s = %ld seconds", "session",
	       section->session_timeout);
	break;
      case 3:
	s_log (-1, "%-15s = session cache timeout (in seconds)", "session");
	break;
      }
    switch ((int) cmd)
      {
      case 0:
	section->stack_size = 65536;
	break;
      case 1:
	tmp___45 = strcasecmp ((char const *) opt, "stack");
	if (tmp___45)
	  {
	    break;
	  }
	tmp___46 = atoi ((char const *) arg);
	if (tmp___46 > 0)
	  {
	    section->stack_size = atoi ((char const *) arg);
	  }
	else
	  {
	    return ((char *) "Illegal thread stack size");
	  } return ((char *) ((void *) 0));
      case 2:
	s_log (-1, "%-15s = %d bytes", "stack", section->stack_size);
	break;
      case 3:
	s_log (-1, "%-15s = thread stack size (in bytes)", "stack");
	break;
      }
    switch ((int) cmd)
      {
      case 0:
	tmp___47 = SSLv3_client_method ();
	section->client_method = tmp___47;
	tmp___48 = SSLv23_server_method ();
	section->server_method = tmp___48;
	break;
      case 1:
	tmp___49 = strcasecmp ((char const *) opt, "sslVersion");
	if (tmp___49)
	  {
	    break;
	  }
	tmp___61 = strcasecmp ((char const *) arg, "all");
	if (tmp___61)
	  {
	    tmp___60 = strcasecmp ((char const *) arg, "SSLv2");
	    if (tmp___60)
	      {
		tmp___59 = strcasecmp ((char const *) arg, "SSLv3");
		if (tmp___59)
		  {
		    tmp___58 = strcasecmp ((char const *) arg, "TLSv1");
		    if (tmp___58)
		      {
			return ((char *) "Incorrect version of SSL protocol");
		      }
		    else
		      {
			tmp___56 = TLSv1_client_method ();
			section->client_method = tmp___56;
			tmp___57 = TLSv1_server_method ();
			section->server_method = tmp___57;
		      }
		  }
		else
		  {
		    tmp___54 = SSLv3_client_method ();
		    section->client_method = tmp___54;
		    tmp___55 = SSLv3_server_method ();
		    section->server_method = tmp___55;
		  }
	      }
	    else
	      {
		tmp___52 = SSLv2_client_method ();
		section->client_method = tmp___52;
		tmp___53 = SSLv2_server_method ();
		section->server_method = tmp___53;
	      }
	  }
	else
	  {
	    tmp___50 = SSLv23_client_method ();
	    section->client_method = tmp___50;
	    tmp___51 = SSLv23_server_method ();
	    section->server_method = tmp___51;
	  }
	return ((char *) ((void *) 0));
      case 2:
	s_log (-1, "%-15s = SSLv3 for client, all for server", "sslVersion");
	break;
      case 3:
	s_log (-1, "%-15s = all|SSLv2|SSLv3|TLSv1 SSL method", "sslVersion");
	break;
      }
    switch ((int) cmd)
      {
      case 0:
	section->timeout_busy = 300;
	break;
      case 1:
	tmp___62 = strcasecmp ((char const *) opt, "TIMEOUTbusy");
	if (tmp___62)
	  {
	    break;
	  }
	tmp___63 = atoi ((char const *) arg);
	if (tmp___63 > 0)
	  {
	    section->timeout_busy = atoi ((char const *) arg);
	  }
	else
	  {
	    return ((char *) "Illegal busy timeout");
	  } return ((char *) ((void *) 0));
      case 2:
	s_log (-1, "%-15s = %d seconds", "TIMEOUTbusy",
	       section->timeout_busy);
	break;
      case 3:
	s_log (-1, "%-15s = seconds to wait for expected data",
	       "TIMEOUTbusy");
	break;
      }
    switch ((int) cmd)
      {
      case 0:
	section->timeout_close = 60;
	break;
      case 1:
	tmp___64 = strcasecmp ((char const *) opt, "TIMEOUTclose");
	if (tmp___64)
	  {
	    break;
	  }
	tmp___65 = atoi ((char const *) arg);
	if (tmp___65 > 0)
	  {
	    section->timeout_close = atoi ((char const *) arg);
	  }
	else
	  {
	    if (0)
	      {
		__s1_len = strlen ((char const *) arg);
		__s2_len = strlen ("0");
		if (!
		    ((unsigned int) ((void const *) (arg + 1)) -
		     (unsigned int) ((void const *) arg) == 1U))
		  {
		    goto _L___0;
		  }
		else
		  {
		    if (__s1_len >= 4U)
		      {
		      _L___0:	/* CIL Label */
			  if (!
		      ((unsigned int) ((void const *) ("0" + 1)) -
	       (unsigned int) ((void const *) "0") == 1U))
			  {
			    tmp___72 = 1;
			  }
			else
			  {
			    if (__s2_len >= 4U)
			      {
				tmp___72 = 1;
			      }
			    else
			      {
				tmp___72 = 0;
			      }
			  }
		      }
		    else
		      {
			tmp___72 = 0;
		      }
		  }
		if (tmp___72)
		  {
		    tmp___67 = __builtin_strcmp ((char const *) arg, "0");
		    tmp___71 = tmp___67;
		  }
		else
		  {
		    tmp___70 = __builtin_strcmp ((char const *) arg, "0");
		    tmp___71 = tmp___70;
	      }}
	    else
	      {
		tmp___70 = __builtin_strcmp ((char const *) arg, "0");
		tmp___71 = tmp___70;
	    } if (tmp___71)
	      {
		return ((char *) "Illegal close timeout");
	      }
	    else
	      {
		section->timeout_close = atoi ((char const *) arg);
	  }} return ((char *) ((void *) 0));
      case 2:
	s_log (-1, "%-15s = %d seconds", "TIMEOUTclose",
	       section->timeout_close);
	break;
      case 3:
	s_log (-1,
	       "%-15s = seconds to wait for close_notify (set to 0 for buggy MSIE)",
	       "TIMEOUTclose");
	break;
      }
    switch ((int) cmd)
      {
      case 0:
	section->timeout_connect = 10;
	break;
      case 1:
	tmp___73 = strcasecmp ((char const *) opt, "TIMEOUTconnect");
	if (tmp___73)
	  {
	    break;
	  }
	tmp___74 = atoi ((char const *) arg);
	if (tmp___74 > 0)
	  {
	    section->timeout_connect = atoi ((char const *) arg);
	  }
	else
	  {
	    if (0)
	      {
		__s1_len___0 = strlen ((char const *) arg);
		__s2_len___0 = strlen ("0");
		if (!
		    ((unsigned int) ((void const *) (arg + 1)) -
		     (unsigned int) ((void const *) arg) == 1U))
		  {
		    goto _L___2;
		  }
		else
		  {
		    if (__s1_len___0 >= 4U)
		      {
		      _L___2:	/* CIL Label */
			  if (!
		      ((unsigned int) ((void const *) ("0" + 1)) -
	       (unsigned int) ((void const *) "0") == 1U))
			  {
			    tmp___81 = 1;
			  }
			else
			  {
			    if (__s2_len___0 >= 4U)
			      {
				tmp___81 = 1;
			      }
			    else
			      {
				tmp___81 = 0;
			      }
			  }
		      }
		    else
		      {
			tmp___81 = 0;
		      }
		  }
		if (tmp___81)
		  {
		    tmp___76 = __builtin_strcmp ((char const *) arg, "0");
		    tmp___80 = tmp___76;
		  }
		else
		  {
		    tmp___79 = __builtin_strcmp ((char const *) arg, "0");
		    tmp___80 = tmp___79;
	      }}
	    else
	      {
		tmp___79 = __builtin_strcmp ((char const *) arg, "0");
		tmp___80 = tmp___79;
	    } if (tmp___80)
	      {
		return ((char *) "Illegal connect timeout");
	      }
	    else
	      {
		section->timeout_connect = atoi ((char const *) arg);
	  }} return ((char *) ((void *) 0));
      case 2:
	s_log (-1, "%-15s = %d seconds", "TIMEOUTconnect",
	       section->timeout_connect);
	break;
      case 3:
	s_log (-1, "%-15s = seconds to connect remote host",
	       "TIMEOUTconnect");
	break;
      }
    switch ((int) cmd)
      {
      case 0:
	section->timeout_idle = 43200;
	break;
      case 1:
	tmp___82 = strcasecmp ((char const *) opt, "TIMEOUTidle");
	if (tmp___82)
	  {
	    break;
	  }
	tmp___83 = atoi ((char const *) arg);
	if (tmp___83 > 0)
	  {
	    section->timeout_idle = atoi ((char const *) arg);
	  }
	else
	  {
	    return ((char *) "Illegal idle timeout");
	  } return ((char *) ((void *) 0));
      case 2:
	s_log (-1, "%-15s = %d seconds", "TIMEOUTidle",
	       section->timeout_idle);
	break;
      case 3:
	s_log (-1, "%-15s = seconds to keep an idle connection",
	       "TIMEOUTidle");
	break;
      }
    switch ((int) cmd)
      {
      case 0:
	section->option.transparent = 0U;
	break;
      case 1:
	tmp___84 = strcasecmp ((char const *) opt, "transparent");
	if (tmp___84)
	  {
	    break;
	  }
	tmp___86 = strcasecmp ((char const *) arg, "yes");
	if (tmp___86)
	  {
	    tmp___85 = strcasecmp ((char const *) arg, "no");
	    if (tmp___85)
	      {
		return ((char *)
			"Argument should be either \'yes\' or \'no\'");
	      }
	    else
	      {
		section->option.transparent = 0U;
	      }
	  }
	else
	  {
	    section->option.transparent = 1U;
	  }
	return ((char *) ((void *) 0));
      case 2:
	break;
      case 3:
	s_log (-1, "%-15s = yes|no transparent proxy mode", "transparent");
	break;
      }
    switch ((int) cmd)
      {
      case 0:
	section->verify_level = -1;
	section->verify_use_only_my = 0;
	break;
      case 1:
	tmp___87 = strcasecmp ((char const *) opt, "verify");
	if (tmp___87)
	  {
	    break;
	  }
	section->verify_level = 0;
	tmp___88 = atoi ((char const *) arg);
	switch (tmp___88)
	  {
	  case 3:
	    section->verify_use_only_my = 1;
	  case 2:
	    section->verify_level |= 2;
	  case 1:
	    section->verify_level |= 1;
	  case 0:
	    return ((char *) ((void *) 0));
	  default:;
	    return ((char *) "Bad verify level");
      } case 2:
	s_log (-1, "%-15s = none", "verify");
	break;
      case 3:
	s_log (-1, "%-15s = level of peer certificate verification",
	       "verify");
	s_log (-1, "%18slevel 1 - verify peer certificate if present", "");
	s_log (-1, "%18slevel 2 - require valid peer certificate always", "");
	s_log (-1,
	       "%18slevel 3 - verify peer with locally installed certificate",
	       "");
	break;
      }
    if ((unsigned int) cmd == 1U)
      {
	return (option_not_found);
      }
    return ((char *) ((void *) 0));
  }
}

static void
syntax (char *confname)
{
  {
    s_log (-1, " ");
    s_log (-1, "Syntax:");
    s_log (-1,
	   "stunnel [<filename>] ] -fd <n> | -help | -version | -sockets");
    s_log (-1, "    <filename>  - use specified config file instead of %s",
	   confname);
    s_log (-1,
	   "    -fd <n>     - read the config file from a file descriptor");
    s_log (-1, "    -help       - get config file help");
    s_log (-1, "    -version    - display version and defaults");
    s_log (-1, "    -sockets    - display default socket options");
    die (1);
    return;
  }
}

void
parse_config (char *name, char *parameter)
{
  char *default_config_file;
  DISK_FILE *df;
  char confline[16384];
  char *arg;
  char *opt;
  char *errstr;
  char *filename;
  int line_number;
  int i;
  int sections;
  LOCAL_OPTIONS *section;
  LOCAL_OPTIONS *new_section;
  int tmp;
  int tmp___0;
  int tmp___1;
  char *tmp___11;
  size_t tmp___12;
  unsigned short const **tmp___13;
  unsigned short const **tmp___14;
  int tmp___15;
  {
    default_config_file = (char *) "/usr/local/etc/stunnel/stunnel.conf";
    sections = 0;
    memset ((void *) (&options), 0, sizeof (GLOBAL_OPTIONS));
    memset ((void *) (&local_options), 0, sizeof (LOCAL_OPTIONS));
    local_options.next = (struct local_options *) ((void *) 0);
    section = &local_options;
    global_options ((enum __anonenum_CMD_106) 0, (char *) ((void *) 0),
		    (char *) ((void *) 0));
    service_options ((enum __anonenum_CMD_106) 0, section,
		     (char *) ((void *) 0), (char *) ((void *) 0));
    if (!name)
      {
	name = default_config_file;
      }
    tmp = strcasecmp ((char const *) name, "-help");
    if (!tmp)
      {
	global_options ((enum __anonenum_CMD_106) 3, (char *) ((void *) 0),
			(char *) ((void *) 0));
	service_options ((enum __anonenum_CMD_106) 3, section,
			 (char *) ((void *) 0), (char *) ((void *) 0));
	die (1);
      }
    if (!section->option.client)
      {
	section->option.cert = 1U;
      }
    context_init (section);
    if ((unsigned int) section == (unsigned int) (&local_options))
      {
	if (section->option.accept)
	  {
	    config_error (filename, line_number,
			  (char *) "accept is not allowed in inetd mode");
	  }
	return;
      }
    if ((section->option.accept + section->option.program) +
	section->option.remote != 2U)
      {
	config_error (filename, line_number,
		      (char *)
		      "Each service section must define exactly two endpoints");
      }
    return;
  }
}

void BIO_set_flags (BIO * b, int flags);
BIO *BIO_new (BIO_METHOD * method___0);
long BIO_ctrl (BIO * b, int cmd, long larg, void *parg);
BIO *BIO_push (BIO * b, BIO * bio);
BIO *BIO_pop (BIO * b);
void BIO_free_all (BIO * bio);

#pragma merger(0,"/tmp/cil-9M3xoTgi.i","-g,-O2,-Wall,-Wshadow,-Wcast-align,-Wpointer-arith")
RSA *RSA_generate_key (int bits___0, unsigned long e_value,
		       void (*callback) (int, int, void *), void *cb_arg);
void RSA_free (RSA * r);
long SSL_CTX_ctrl (SSL_CTX * ctx, int cmd, long larg, void *parg);
char const *SSL_alert_type_string_long (int value);
static RSA *tmp_rsa_cb (SSL * s, int export, int keylen);
static RSA *make_temp_key (int keylen);
static void load_certificate (LOCAL_OPTIONS * section);
static int cache_cb (char *buf___5, int size, int rwflag, void *userdata);
static void info_callback (SSL const *s, int where, int ret___3);
static void print_stats (SSL_CTX * ctx);
static void sslerror_stack (void);
void
context_init (LOCAL_OPTIONS * section)
{
  struct stat st;
  int tmp;
  long tmp___0;
  int tmp___1;
  {
    if (!section->key)
      {
	section->key = section->cert;
      }
    if (!section->engine)
      {
	if (section->option.cert)
	  {
	    tmp =
	      stat ((char const *__restrict) section->key,
		    (struct stat * __restrict) (&st));
	    if (tmp)
	      {
		ioerror ((char const *) section->key);
		die (1);
	      }
	    if (st.st_mode & 7U)
	      {
		s_log (4, "Wrong permissions on %s", section->key);
	      }
	  }
      }
    if (section->option.client)
      {
	section->ctx = SSL_CTX_new (section->client_method);
      }
    else
      {
	section->ctx = SSL_CTX_new (section->server_method);
	SSL_CTX_set_tmp_rsa_callback (section->ctx, &tmp_rsa_cb);
      }
    if (section->ssl_options)
      {
	s_log (7, "Configuration SSL options: 0x%08lX", section->ssl_options);
	tmp___0 =
	  SSL_CTX_ctrl (section->ctx, 32, section->ssl_options, (void *) 0);
	s_log (7, "SSL options set: 0x%08lX", tmp___0);
      }
    if (section->cipher_list)
      {
	tmp___1 =
	  SSL_CTX_set_cipher_list (section->ctx,
				   (char const *) section->cipher_list);
	if (!tmp___1)
	  {
	    sslerror ((char *) "SSL_CTX_set_cipher_list");
	    die (1);
	  }
      }
    SSL_CTX_ctrl (section->ctx, 33, 3L, (void *) 0);
    SSL_CTX_ctrl (section->ctx, 44, 3L, (void *) 0);
    SSL_CTX_set_timeout (section->ctx, section->session_timeout);
    if (section->option.cert)
      {
	load_certificate (section);
      }
    verify_init (section);
    SSL_CTX_set_info_callback (section->ctx, &info_callback);
    s_log (7, "SSL context initialized for service %s", section->servname);
    return;
  }
}

static int initialized = 0;
static struct keytabstruct keytable[2049];
static RSA *longkey = (RSA *) ((void *) 0);
static int longlen = 0;
static time_t longtime = (time_t) 0;
static RSA *
tmp_rsa_cb (SSL * s, int export, int keylen)
{
  RSA *oldkey;
  RSA *retval;
  time_t now;
  int i;
  {
    enter_critical_section ((enum __anonenum_SECTION_CODE_104) 0);
    if (!initialized)
      {
	i = 0;
	while (i < 2049)
	  {
	    keytable[i].key = (RSA *) ((void *) 0);
	    keytable[i].timeout = 0L;
	    i++;
	  } initialized = 1;
      }
    time (&now);
    if (keylen < 2049)
      {
	if (keytable[keylen].timeout < now)
	  {
	    oldkey = keytable[keylen].key;
	    keytable[keylen].key = make_temp_key (keylen);
	    keytable[keylen].timeout = now + 3600L;
	    if (oldkey)
	      {
		RSA_free (oldkey);
	      }
	  }
	retval = keytable[keylen].key;
      }
    else
      {
	if (longtime < now)
	  {
	    goto _L;
	  }
	else
	  {
	    if (longlen != keylen)
	      {
	      _L:		/* CIL Label */ oldkey = longkey;
		longkey = make_temp_key (keylen);
		longtime = now + 3600L;
		longlen = keylen;
		if (oldkey)
		  {
		    RSA_free (oldkey);
		  }
	      }
	  }
	retval = longkey;
      }
    leave_critical_section ((enum __anonenum_SECTION_CODE_104) 0);
    return (retval);
  }
}

static RSA *
make_temp_key (int keylen)
{
  RSA *result;
  {
    s_log (7, "Generating %d bit temporary RSA key...", keylen);
    result =
      RSA_generate_key (keylen, 65537UL,
			(void (*)(int, int, void *)) ((void *) 0),
			(void *) 0);
    s_log (7, "Temporary RSA key created");
    return (result);
  }
}

static int cache_initialized = 0;
static void
load_certificate (LOCAL_OPTIONS * section)
{
  int i;
  int reason;
  UI_DATA ui_data;
  EVP_PKEY *pkey;
  UI_METHOD *uim;
  int tmp;
  unsigned long tmp___0;
  int tmp___1;
  UI_DATA *tmp___2;
  int tmp___3;
  unsigned long tmp___4;
  int tmp___5;
  {
    ui_data.section = section;
    s_log (7, "Certificate: %s", section->cert);
    tmp___5 = SSL_CTX_check_private_key ((SSL_CTX const *) section->ctx);
    if (!tmp___5)
      {
	sslerror ((char *) "Private key does not match the certificate");
	die (1);
      }
    s_log (7, "Private key loaded");
    return;
  }
}

static void
info_callback (SSL const *s, int where, int ret___3)
{
  char const *tmp;
  char const *tmp___0;
  char const *tmp___1;
  char const *tmp___2;
  char const *tmp___3;
  char const *tmp___4;
  {
    if (where & 1)
      {
	tmp = SSL_state_string_long (s);
	if (where & 4096)
	  {
	    tmp___1 = "connect";
	  }
	else
	  {
	    if (where & 8192)
	      {
		tmp___0 = "accept";
	      }
	    else
	      {
		tmp___0 = "undefined";
	      }
	    tmp___1 = tmp___0;
	  }
	s_log (7, "SSL state (%s): %s", tmp___1, tmp);
      }
    else
      {
	if (where & 16384)
	  {
	    tmp___2 = SSL_alert_desc_string_long (ret___3);
	    tmp___3 = SSL_alert_type_string_long (ret___3);
	    if (where & 4)
	      {
		tmp___4 = "read";
	      }
	    else
	      {
		tmp___4 = "write";
	      }
	    s_log (7, "SSL alert (%s): %s: %s", tmp___4, tmp___3, tmp___2);
	  }
	else
	  {
	    if (where == 32)
	      {
		print_stats ((SSL_CTX *) s->ctx);
	      }
	  }
      }
    return;
  }
}

static void
print_stats (SSL_CTX * ctx)
{
  long tmp;
  long tmp___0;
  long tmp___1;
  long tmp___2;
  long tmp___3;
  long tmp___4;
  long tmp___5;
  long tmp___6;
  long tmp___7;
  long tmp___8;
  {
    tmp = SSL_CTX_ctrl (ctx, 20, 0L, (void *) 0);
    s_log (7, "%4ld items in the session cache", tmp);
    tmp___0 = SSL_CTX_ctrl (ctx, 21, 0L, (void *) 0);
    s_log (7, "%4ld client connects (SSL_connect())", tmp___0);
  }
}

void
sslerror (char *txt)
{
  unsigned long err;
  char string[120];
  {
    err = ERR_get_error ();
    if (!err)
      {
	s_log (3, "%s: Peer suddenly disconnected", txt);
	return;
      }
    sslerror_stack ();
    ERR_error_string (err, string);
    s_log (3, "%s: %lX: %s", txt, err, string);
    return;
  }
}

#pragma merger(0,"/tmp/cil-uaFQwsLb.i","-g,-O2,-Wall,-Wshadow,-Wcast-align,-Wpointer-arith")
int sk_num (STACK const *st);
void *sk_value (STACK const *st, int i);
SSL3_ENC_METHOD ssl3_undef_enc_method;
int ssl_undefined_void_function (void);
SSL_CIPHER *ssl2_get_cipher_by_char (unsigned char const *p);
int ssl_ok (SSL * s);
void sk_free (STACK * st);
void *sk_delete (STACK * st, int loc);
STACK *sk_dup (STACK * sk);
rwlock_t lock3;

#define CRYPTO_malloc(size,file,line)           \
  malloc(size)
int RSA_private_decrypt (int flen, unsigned char const *from___0,
			 unsigned char *to, RSA * rsa, int padding);
void ERR_put_error (int lib, int func, int reason, char const *file,
		    int line);
int SSL_state (SSL const *ssl);
SESS_CERT *ssl_sess_cert_new (void);
void ssl_sess_cert_free (SESS_CERT * sc);
STACK *ssl_bytes_to_cipher_list (SSL * s, unsigned char *p, int num,
				 STACK ** skp);
int ssl_cipher_list_to_bytes (SSL * s, STACK * sk, unsigned char *p,
			      int (*put_cb) (SSL_CIPHER const *,
					     unsigned char *));
int ssl_verify_cert_chain (SSL * s, STACK * sk);
int ssl_undefined_function (SSL * s);
int ssl2_enc_init (SSL * s, int client___0);
void RAND_add (void const *buf___5, int num, double entropy___0);
static int request_certificate (SSL * s);
static int ssl_rsa_private_decrypt (CERT * c, int len,
				    unsigned char *from___0,
				    unsigned char *to, int padding);
int
ssl2_accept (SSL * s)
{
}

static int
server_verify (SSL * s)
{
  unsigned char *p;
  unsigned char *tmp;
  int tmp___0;
  {
    if (s->state == 8256)
      {
	p = (unsigned char *) (s->init_buf)->data;
	tmp = p;
	p++;
	*tmp = (unsigned char) 5;
	if ((s->s2)->challenge_length > sizeof ((s->s2)->challenge))
	  {
	    ERR_put_error (20, 240, 68, "s2_srvr.c", 881);
	    return (-1);
	  }
	memcpy ((void *__restrict) p,
		(void const *__restrict) ((s->s2)->challenge),
		(s->s2)->challenge_length);
	s->state = 8257;
	s->init_num = (int) ((s->s2)->challenge_length + 1U);
	s->init_off = 0;
      }
    tmp___0 = ssl2_do_write (s);
    tmp___0 = ssl2_do_write (s);
    return (tmp___0);
  }
}

static int
request_certificate (SSL * s)
{
  unsigned char const *cp;
  unsigned char *p;
  unsigned char *p2;
  unsigned char *buf2;
  unsigned char *ccd;
  int i;
  int n;
  int len;
  int tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  {
    p = (unsigned char *) (s->init_buf)->data;
    if (s->state == 4192)
      {
	i =
	  ssl2_read (s, (void *) ((char *) (p + s->init_num)),
		     1 - s->init_num);
	if (i < 1 - s->init_num)
	  {
	    tmp = ssl2_part_read (s, 110UL, i);
	    return (tmp);
	  }
	s->init_num += i;
	s->state = 4193;
	if ((int) *p != 5)
	  {
	    if ((int) *(p + 0) != 0)
	      {
		ssl2_return_error (s, 0);
		ERR_put_error (20, 110, 212, "s2_clnt.c", 915);
	      }
	    else
	      {
		ERR_put_error (20, 110, 200, "s2_clnt.c", 919);
		i =
		  ssl2_read (s, (void *) ((char *) (p + s->init_num)),
			     3 - s->init_num);
		tmp___0 = ssl2_part_read (s, 110UL, i);
		return (tmp___0);
	      }
	    return (-1);
	  }
      }
    p = (unsigned char *) (s->init_buf)->data;
    len = (int) (1U + (s->s2)->challenge_length);
    n = len - s->init_num;
    i = ssl2_read (s, (void *) ((char *) (p + s->init_num)), n);
    if (i < n)
      {
	tmp___1 = ssl2_part_read (s, 110UL, i);
	return (tmp___1);
      }
    if (s->msg_callback)
      {
	(*(s->msg_callback)) (0, s->version, 0, (void const *) p,
			      (unsigned int) len, s, s->msg_callback_arg);
      }
    p++;
    tmp___2 =
      memcmp ((void const *) p, (void const *) ((s->s2)->challenge),
	      (s->s2)->challenge_length);
    if (tmp___2 != 0)
      {
	ssl2_return_error (s, 0);
	ERR_put_error (20, 110, 136, "s2_clnt.c", 941);
	return (-1);
      }
    return (1);
  }
}

static int
get_server_finished (SSL * s)
{
  unsigned char *buf___5;
  unsigned char *p;
  int i;
  int n;
  int len;
  int tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  {
    buf___5 = (unsigned char *) (s->init_buf)->data;
    p = buf___5;
    if (s->state == 4208)
      {
	i =
	  ssl2_read (s, (void *) ((char *) (buf___5 + s->init_num)),
		     1 - s->init_num);
	if (i < 1 - s->init_num)
	  {
	    tmp = ssl2_part_read (s, 108UL, i);
	    return (tmp);
	  }
	s->init_num += i;
	if ((int) *p == 7)
	  {
	    s->state = 4176;
	    return (1);
	  }
	else
	  {
	    if ((int) *p != 6)
	      {
		if ((int) *(p + 0) != 0)
		  {
		    ssl2_return_error (s, 0);
		    ERR_put_error (20, 108, 212, "s2_clnt.c", 972);
		  }
		else
		  {
		    ERR_put_error (20, 108, 200, "s2_clnt.c", 976);
		    i =
		      ssl2_read (s, (void *) ((char *) (p + s->init_num)),
				 3 - s->init_num);
		    tmp___0 = ssl2_part_read (s, 110UL, i);
		    return (tmp___0);
		  }
		return (-1);
	      }
	  }
	s->state = 4209;
      }
    len = 17;
    n = len - s->init_num;
    i = ssl2_read (s, (void *) ((char *) (buf___5 + s->init_num)), n);
    if (i < n)
      {
	tmp___1 = ssl2_part_read (s, 108UL, i);
	return (tmp___1);
      }
    s->init_num += i;
    if (s->msg_callback)
      {
	(*(s->msg_callback)) (0, s->version, 0, (void const *) buf___5,
			      (unsigned int) s->init_num, s,
			      s->msg_callback_arg);
      }
    if (!s->hit)
      {
	(s->session)->session_id_length = 16U;
	memcpy ((void *__restrict) ((s->session)->session_id),
		(void const *__restrict) (p + 1), 16U);
      }
    else
      {
	if (!(s->options & 1UL))
	  {
	    if ((s->session)->session_id_length >
		sizeof ((s->session)->session_id))
	      {
		ssl2_return_error (s, 0);
		ERR_put_error (20, 108, 231, "s2_clnt.c", 1013);
		return (-1);
	      }
	    else
	      {
		tmp___2 =
		  memcmp ((void const *) (buf___5 + 1),
			  (void const *) ((s->session)->session_id),
			  (s->session)->session_id_length);
		if (0 != tmp___2)
		  {
		    ssl2_return_error (s, 0);
		    ERR_put_error (20, 108, 231, "s2_clnt.c", 1013);
		    return (-1);
		  }
	      }
	  }
      }
    s->state = 3;
    return (1);
  }
}

int
ssl2_set_certificate (SSL * s, int type, int len,
		      unsigned char const *data___0)
{
  STACK *sk;
  EVP_PKEY *pkey;
  SESS_CERT *sc;
  int i;
  X509 *x509;
  int ret___3;
  int tmp;
  int tmp___0;
  {
    sk = (STACK *) ((void *) 0);
    ret___3 = 1;
  err:sk_free (sk);
    X509_free (x509);
    EVP_PKEY_free (pkey);
    return (ret___3);
  }
}

static int
ssl_rsa_public_encrypt (SESS_CERT * sc, int len, unsigned char *from___0,
			unsigned char *to, int padding)
{
  EVP_PKEY *pkey;
  int i;
  {
    pkey = (EVP_PKEY *) ((void *) 0);
    i = -1;
    if ((unsigned int) sc == (unsigned int) ((void *) 0))
      {
	ERR_put_error (20, 188, 192, "s2_clnt.c", 1100);
	return (-1);
      }
    else
      {
	if ((unsigned int) (sc->peer_key)->x509 ==
	    (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 188, 192, "s2_clnt.c", 1100);
	    return (-1);
	  }
	else
	  {
	    pkey = X509_get_pubkey ((sc->peer_key)->x509);
	    if ((unsigned int) pkey == (unsigned int) ((void *) 0))
	      {
		ERR_put_error (20, 188, 192, "s2_clnt.c", 1100);
		return (-1);
	      }
	  }
      }
    if (pkey->type != 6)
      {
	ERR_put_error (20, 188, 209, "s2_clnt.c", 1105);
	goto end;
      }
    i =
      RSA_public_encrypt (len, (unsigned char const *) from___0, to,
			  pkey->pkey.rsa, padding);
    if (i < 0)
      {
	ERR_put_error (20, 188, 4, "s2_clnt.c", 1112);
      }
  end:EVP_PKEY_free (pkey);
    return (i);
  }
}

char const *OBJ_bsearch (char const *key, char const *base, int num, int size,
			 int (*cmp) (void const *, void const *));
int EVP_MD_size (EVP_MD const *md___0);
SSL_CIPHER ssl2_ciphers[7];
SSL_METHOD *ssl_bad_method (int ver);
SSL_METHOD *sslv2_base_method (void);
int ssl_cipher_id_cmp (SSL_CIPHER const *a, SSL_CIPHER const *b);
int ssl2_generate_key_material (SSL * s);
SSL_CIPHER *
ssl2_get_cipher_by_char (unsigned char const *p)
{
  SSL_CIPHER c;
  SSL_CIPHER *cp;
  unsigned long id;
  char const *tmp;
  {
    id =
      ((33554432UL | ((unsigned long) *(p + 0) << 16L)) |
       ((unsigned long) *(p + 1) << 8L)) | (unsigned long) *(p + 2);
    c.id = id;
    tmp =
      OBJ_bsearch ((char const *) ((char *) (&c)),
		   (char const *) ((char *) (ssl2_ciphers)),
		   (int) (sizeof (ssl2_ciphers) / sizeof (SSL_CIPHER)),
		   (int) sizeof (SSL_CIPHER),
		   (int (*)(void const *, void const *))
		   (&ssl_cipher_id_cmp));
    cp = (SSL_CIPHER *) tmp;
    if ((unsigned int) cp == (unsigned int) ((void *) 0))
      {
	return ((SSL_CIPHER *) ((void *) 0));
      }
    else
      {
	if (cp->valid == 0)
	  {
	    return ((SSL_CIPHER *) ((void *) 0));
	  }
	else
	  {
	    return (cp);
	  }
      }
  }
}

int
ssl2_put_cipher_by_char (SSL_CIPHER const *c, unsigned char *p)
{
  long l;
  {
    if ((unsigned int) p != (unsigned int) ((void *) 0))
      {
	l = (long) c->id;
	if (((unsigned long) l & 4278190080UL) != 33554432UL)
	  {
	    return (0);
	  }
	*(p + 0) = (unsigned char) ((int) ((unsigned char) (l >> 16L)) & 255);
	*(p + 1) = (unsigned char) ((int) ((unsigned char) (l >> 8L)) & 255);
	*(p + 2) = (unsigned char) ((int) ((unsigned char) l) & 255);
      }
    return (3);
  }
}

int
ssl2_generate_key_material (SSL * s)
{
  unsigned int i;
  EVP_MD_CTX ctx;
  unsigned char *km;
  unsigned char c;
  EVP_MD const *md5;
  int tmp;
  int tmp___0;
  int tmp___1;
  {
    EVP_MD_CTX_cleanup (&ctx);
    return (1);
  }
}

void
ssl2_return_error (SSL * s, int err)
{
  {
    if (!s->error)
      {
	s->error = 3;
	s->error_code = err;
	ssl2_write_error (s);
      }
    return;
  }
}

void
ssl2_write_error (SSL * s)
{
  unsigned char buf___5[3];
  int i;
  int error;
  {
    buf___5[0] = (unsigned char) 0;
    buf___5[1] = (unsigned char) ((s->error_code >> 8) & 255);
  }
}

void
ssl2_mac (SSL * s, unsigned char *md___0, int send___0)
{
  EVP_MD_CTX c;
  unsigned char sequence[4];
  unsigned char *p;
  unsigned char *sec;
  unsigned char *act;
  unsigned long seq;
  unsigned int len;
  unsigned char *tmp;
  unsigned char *tmp___0;
  unsigned char *tmp___1;
  unsigned char *tmp___2;
  int tmp___3;
  {
    if (send___0)
      {
	seq = (s->s2)->write_sequence;
	sec = (s->s2)->write_key;
	len = (unsigned int) (s->s2)->wact_data_length;
	act = (s->s2)->wact_data;
      }
    EVP_DigestUpdate (&c, (void const *) sec, (unsigned int) tmp___3);
    EVP_DigestUpdate (&c, (void const *) act, len);
    EVP_DigestUpdate (&c, (void const *) (sequence), 4U);
    EVP_DigestFinal_ex (&c, md___0, (unsigned int *) ((void *) 0));
    EVP_MD_CTX_cleanup (&c);
    return;
  }
}

static int
ssl2_read_internal (SSL * s, void *buf___5, int len, int peek)
{
  int n;
  unsigned char mac[20];
  unsigned char *p;
}

static int
do_ssl_write (SSL * s, unsigned char const *buf___5, unsigned int len)
{
  unsigned int j;
  unsigned int k;
  unsigned int olen;
  unsigned int p;
  unsigned int mac_size;
  unsigned int bs;
  register unsigned char *pp;
  int tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  {
    (s->s2)->write_sequence = ((s->s2)->write_sequence + 1UL) & 4294967295UL;
    (s->s2)->wpend_tot = (int) olen;
    (s->s2)->wpend_buf = buf___5;
    (s->s2)->wpend_ret = (int) len;
    (s->s2)->wpend_off = 0;
    tmp___2 = write_pending (s, buf___5, olen);
    return (tmp___2);
  }
}

int
ssl2_part_read (SSL * s, unsigned long f, int i)
{
  unsigned char *p;
  int j;
  int ret___3;
  {
    ret___3 =
      ssl2_write (s, (void const *) ((s->init_buf)->data + s->init_off),
		  s->init_num);
    if (ret___3 == s->init_num)
      {
	if (s->msg_callback)
	  {
	    (*(s->msg_callback)) (1, s->version, 0,
				  (void const *) (s->init_buf)->data,
				  (unsigned int) (s->init_off + s->init_num),
				  s, s->msg_callback_arg);
	  }
	return (1);
      }
    if (ret___3 < 0)
      {
	return (-1);
      }
    return (ret___3);
  }
}

SSL3_ENC_METHOD SSLv3_enc_data;
SSL_CIPHER *ssl3_get_cipher_by_char (unsigned char const *p);
int ssl3_put_cipher_by_char (SSL_CIPHER const *c, unsigned char *p);
long ssl3_get_message (SSL * s, int st1, int stn, int mt, long max___1,
		       int *ok);
int ssl3_num_ciphers (void);
SSL_CIPHER *ssl3_get_cipher (unsigned int u);
int ssl3_renegotiate (SSL * s);
int ssl3_renegotiate_check (SSL * s);
int ssl3_dispatch_alert (SSL * s);
int ssl3_read_bytes (SSL * s, int type, unsigned char *buf___5, int len,
		     int peek);
int ssl3_write_bytes (SSL * s, int type, void const *buf_, int len);
int ssl3_new (SSL * s);
void ssl3_free (SSL * s);
int ssl3_accept (SSL * s);
int ssl3_connect (SSL * s);
int ssl3_read (SSL * s, void *buf___5, int len);
int ssl3_peek (SSL * s, void *buf___5, int len);
int ssl3_write (SSL * s, void const *buf___5, int len);
int ssl3_shutdown (SSL * s);
void ssl3_clear (SSL * s);
long ssl3_ctrl (SSL * s, int cmd, long larg, void *parg);
long ssl3_ctx_ctrl (SSL_CTX * ctx, int cmd, long larg, void *parg);
long ssl3_callback_ctrl (SSL * s, int cmd, void (*fp) (void));
long ssl3_ctx_callback_ctrl (SSL_CTX * ctx, int cmd, void (*fp) (void));
int ssl3_pending (SSL const *s);
long ssl3_default_timeout (void);
int DH_generate_key (DH * dh);
int DH_compute_key (unsigned char *key, BIGNUM const *pub_key, DH * dh);
DH *d2i_DHparams (DH ** a, unsigned char const **in, long len);
int i2d_DHparams (DH const *a, unsigned char **out);
int DSA_verify (int type, unsigned char const *dgst, int dgst_len,
		unsigned char const *sigbuf, int siglen, DSA * dsa);
X509 *ssl_get_server_send_cert (SSL * s);
EVP_PKEY *ssl_get_sign_pkey (SSL * s, SSL_CIPHER * cipher);
int ssl_verify_alarm_type (long type);
unsigned long ssl3_output_cert_chain (SSL * s, X509 * x);
SSL_CIPHER *ssl3_choose_cipher (SSL * s, STACK * clnt, STACK * srvr);
static SSL_METHOD *
ssl3_get_server_method (int ver)
{
  SSL_METHOD *tmp;
  {
    if (ver == 768)
      {
	tmp = SSLv3_server_method ();
	return (tmp);
      }
    else
      {
	return ((SSL_METHOD *) ((void *) 0));
      }
  }
}

static SSL_METHOD SSLv3_server_method_data = {
  768, &ssl3_new, &ssl3_clear, &ssl3_free, &ssl3_accept,
    &ssl_undefined_function, &ssl3_read, &ssl3_peek, &ssl3_write,
    &ssl3_shutdown, &ssl3_renegotiate, &ssl3_renegotiate_check,
    &ssl3_get_message, &ssl3_read_bytes, &ssl3_write_bytes,
    &ssl3_dispatch_alert, &ssl3_ctrl, &ssl3_ctx_ctrl,
    &ssl3_get_cipher_by_char, &ssl3_put_cipher_by_char, &ssl3_pending,
    &ssl3_num_ciphers, &ssl3_get_cipher, &ssl3_get_server_method,
    &ssl3_default_timeout, &SSLv3_enc_data, &ssl_undefined_void_function,
    &ssl3_callback_ctrl, &ssl3_ctx_callback_ctrl
};

SSL_METHOD *
SSLv3_server_method (void)
{
  {
    return (&SSLv3_server_method_data);
  }
}

int
ssl3_accept (SSL * s)
{
  BUF_MEM *buf___5;
  unsigned long l;
  unsigned long Time;
  time_t tmp;
  void (*cb) (SSL const *ssl, int type, int val);
  long num1;
  int ret___3;
  int new_state;
  int state___0;
  int skip;
  int *tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  int tmp___4;
  int tmp___5;
  int tmp___6;
  int tmp___7;
  long tmp___8;
  int tmp___9;
  int tmp___10;
  long tmp___11;
  {
    if ((unsigned int) s->info_callback != (unsigned int) ((void *) 0))
      {
	cb = s->info_callback;
      }
    else
      {
	if ((unsigned int) (s->ctx)->info_callback !=
	    (unsigned int) ((void *) 0))
	  {
	    cb = (s->ctx)->info_callback;
	  }
      }
    (s->in_handshake)++;
    tmp___1 = SSL_state ((SSL const *) s);
    if (tmp___1 & 12288)
      {
	tmp___2 = SSL_state ((SSL const *) s);
	if (tmp___2 & 16384)
	  {
	    SSL_clear (s);
	  }
      }
    else
      {
	SSL_clear (s);
      }
    if ((unsigned int) s->cert == (unsigned int) ((void *) 0))
      {
	ERR_put_error (20, 128, 179, "s3_srvr.c", 188);
	return (-1);
      }
    while (1)
      {
	state___0 = s->state;
	switch (s->state)
	  {
	  case 12292:
	    s->new_session = 1;
	  case 16384:
	  case 8192:
	  case 24576:
	  case 8195:
	    s->server = 1;
	    if ((unsigned int) cb != (unsigned int) ((void *) 0))
	      {
		(*cb) ((SSL const *) s, 16, 1);
	      }
	    if (s->version >> 8 != 3)
	      {
		ERR_put_error (20, 128, 68, "s3_srvr.c", 212);
		return (-1);
	      }
	    s->type = 8192;
	    if ((unsigned int) s->init_buf == (unsigned int) ((void *) 0))
	      {
		buf___5 = BUF_MEM_new ();
		if ((unsigned int) buf___5 == (unsigned int) ((void *) 0))
		  {
		    ret___3 = -1;
		    goto end;
		  }
		tmp___3 = BUF_MEM_grow (buf___5, 16384);
		if (!tmp___3)
		  {
		    ret___3 = -1;
		    goto end;
		  }
		s->init_buf = buf___5;
	      }
	    tmp___4 = ssl3_setup_buffers (s);
	    if (!tmp___4)
	      {
		ret___3 = -1;
		goto end;
	      }
	    s->init_num = 0;
	    if (s->state != 12292)
	      {
		tmp___5 = ssl_init_wbio_buffer (s, 1);
		if (!tmp___5)
		  {
		    ret___3 = -1;
		    goto end;
		  }
		ssl3_init_finished_mac (s);
		s->state = 8464;
		((s->ctx)->stats.sess_accept)++;
	      }
	    else
	      {
		((s->ctx)->stats.sess_accept_renegotiate)++;
		s->state = 8480;
	      }
	    break;
	  case 8480:
	  case 8481:
	    s->shutdown = 0;
	    ret___3 = ssl3_send_hello_request (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    (s->s3)->tmp.next_state = 8482;
	    s->state = 8448;
	    s->init_num = 0;
	    ssl3_init_finished_mac (s);
	    break;
	  case 8482:
	    s->state = 3;
	    break;
	  case 8464:
	  case 8465:
	  case 8466:
	    s->shutdown = 0;
	    ret___3 = ssl3_get_client_hello (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->new_session = 2;
	    s->state = 8496;
	    s->init_num = 0;
	    break;
	  case 8496:
	  case 8497:
	    ret___3 = ssl3_send_server_hello (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    if (s->hit)
	      {
		s->state = 8656;
	      }
	    else
	      {
		s->state = 8512;
	      }
	    s->init_num = 0;
	    break;
	  case 8512:
	  case 8513:
	    if (!(((s->s3)->tmp.new_cipher)->algorithms & 2048UL))
	      {
		if (!(((s->s3)->tmp.new_cipher)->algorithms & 8192UL))
		  {
		    ret___3 = ssl3_send_server_certificate (s);
		    if (ret___3 <= 0)
		      {
			goto end;
		      }
		  }
		else
		  {
		    skip = 1;
		  }
	      }
	    else
	      {
		skip = 1;
	      }
	    s->state = 8528;
	    s->init_num = 0;
	    break;
	  case 8528:
	  case 8529:
	    l = ((s->s3)->tmp.new_cipher)->algorithms;
	    if (s->options & 2097152UL)
	      {
		(s->s3)->tmp.use_rsa_tmp = 1;
	      }
	    else
	      {
		(s->s3)->tmp.use_rsa_tmp = 0;
	      }
	    if ((s->s3)->tmp.use_rsa_tmp)
	      {
		goto _L___0;
	      }
	    else
	      {
		if (l & 128UL)
		  {
		    goto _L___0;
		  }
		else
		  {
		    if (l & 30UL)
		      {
			goto _L___0;
		      }
		    else
		      {
			if (l & 1UL)
			  {
			    if ((unsigned int) (s->cert)->pkeys[0].
				privatekey == (unsigned int) ((void *) 0))
			      {
				goto _L___0;
			      }
			    else
			      {
				if (((s->s3)->tmp.new_cipher)->
				    algo_strength & 2UL)
				  {
				    tmp___6 =
				      EVP_PKEY_size ((s->cert)->pkeys[0].
						     privatekey);
				    if (((s->s3)->tmp.new_cipher)->
					algo_strength & 8UL)
				      {
					tmp___7 = 512;
				      }
				    else
				      {
					tmp___7 = 1024;
				      }
				    if (tmp___6 * 8 > tmp___7)
				      {
				      _L___0:ret___3 =
					  ssl3_send_server_key_exchange
					  (s);
					if (ret___3 <= 0)
					  {
					    goto end;
					  }
				      }
				    else
				      {
					skip = 1;
				      }
				  }
				else
				  {
				    skip = 1;
				  }
			      }
			  }
			else
			  {
			    skip = 1;
			  }
		      }
		  }
	      }
	    s->state = 8544;
	    s->init_num = 0;
	    break;
	  case 8544:
	  case 8545:
	    if (!(s->verify_mode & 1))
	      {
		skip = 1;
		(s->s3)->tmp.cert_request = 0;
		s->state = 8560;
	      }
	    else
	      {
		if ((unsigned int) (s->session)->peer !=
		    (unsigned int) ((void *) 0))
		  {
		    if (s->verify_mode & 4)
		      {
			skip = 1;
			(s->s3)->tmp.cert_request = 0;
			s->state = 8560;
		      }
		    else
		      {
			goto _L___2;
		      }
		  }
		else
		  {
		  _L___2:if (((s->s3)->tmp.new_cipher)->
			algorithms & 2048UL)
		      {
			if (!(s->verify_mode & 2))
			  {
			    skip = 1;
			    (s->s3)->tmp.cert_request = 0;
			    s->state = 8560;
			  }
			else
			  {
			    goto _L___1;
			  }
		      }
		    else
		      {
		      _L___1:if (((s->s3)->tmp.new_cipher)->
			    algorithms & 8192UL)
			  {
			    skip = 1;
			    (s->s3)->tmp.cert_request = 0;
			    s->state = 8560;
			  }
			else
			  {
			    (s->s3)->tmp.cert_request = 1;
			    ret___3 = ssl3_send_certificate_request (s);
			    if (ret___3 <= 0)
			      {
				goto end;
			      }
			    s->state = 8448;
			    (s->s3)->tmp.next_state = 8576;
			    s->init_num = 0;
			  }
		      }
		  }
	      }
	    break;
	  case 8560:
	  case 8561:
	    ret___3 = ssl3_send_server_done (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    (s->s3)->tmp.next_state = 8576;
	    s->state = 8448;
	    s->init_num = 0;
	    break;
	  case 8448:
	    num1 = BIO_ctrl (s->wbio, 3, 0L, (void *) 0);
	    if (num1 > 0L)
	      {
		s->rwstate = 2;
		tmp___8 = BIO_ctrl (s->wbio, 11, 0L, (void *) 0);
		num1 = (long) ((int) tmp___8);
		if (num1 <= 0L)
		  {
		    ret___3 = -1;
		    goto end;
		  }
		s->rwstate = 1;
	      }
	    s->state = (s->s3)->tmp.next_state;
	    break;
	  case 8576:
	  case 8577:
	    ret___3 = ssl3_check_client_hello (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    if (ret___3 == 2)
	      {
		s->state = 8466;
	      }
	    else
	      {
		if ((s->s3)->tmp.cert_request)
		  {
		    ret___3 = ssl3_get_client_certificate (s);
		    if (ret___3 <= 0)
		      {
			goto end;
		      }
		  }
		s->init_num = 0;
		s->state = 8592;
	      }
	    break;
	  case 8592:
	  case 8593:
	    ret___3 = ssl3_get_client_key_exchange (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    if (ret___3 == 2)
	      {
		s->state = 8640;
		s->init_num = 0;
	      }
	    else
	      {
		s->state = 8608;
		s->init_num = 0;
		(*(((s->method)->ssl3_enc)->cert_verify_mac)) (s,
							       &(s->s3)->
							       finish_dgst1,
							       &(s->s3)->tmp.
							       cert_verify_md
							       [0]);
		(*(((s->method)->ssl3_enc)->cert_verify_mac)) (s,
							       &(s->s3)->
							       finish_dgst2,
							       &(s->s3)->tmp.
							       cert_verify_md
							       [16]);
	      }
	    break;
	  case 8608:
	  case 8609:
	    ret___3 = ssl3_get_cert_verify (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 8640;
	    s->init_num = 0;
	    break;
	  case 8640:
	  case 8641:
	    ret___3 = ssl3_get_finished (s, 8640, 8641);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    if (s->hit)
	      {
		s->state = 3;
	      }
	    else
	      {
		s->state = 8656;
	      }
	    s->init_num = 0;
	    break;
	  case 8656:
	  case 8657:
	    (s->session)->cipher = (s->s3)->tmp.new_cipher;
	    tmp___9 = (*(((s->method)->ssl3_enc)->setup_key_block)) (s);
	    if (!tmp___9)
	      {
		ret___3 = -1;
		goto end;
	      }
	    ret___3 = ssl3_send_change_cipher_spec (s, 8656, 8657);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 8672;
	    s->init_num = 0;
	    tmp___10 =
	      (*(((s->method)->ssl3_enc)->change_cipher_state)) (s, 34);
	    if (!tmp___10)
	      {
		ret___3 = -1;
		goto end;
	      }
	    break;
	  case 8672:
	  case 8673:
	    ret___3 =
	      ssl3_send_finished (s, 8672, 8673,
				  ((s->method)->ssl3_enc)->
				  server_finished_label,
				  ((s->method)->ssl3_enc)->
				  server_finished_label_len);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 8448;
	    if (s->hit)
	      {
		(s->s3)->tmp.next_state = 8640;
	      }
	    else
	      {
		(s->s3)->tmp.next_state = 3;
	      }
	    s->init_num = 0;
	    break;
	  case 3:
	    ssl3_cleanup_key_block (s);
	    BUF_MEM_free (s->init_buf);
	    s->init_buf = (BUF_MEM *) ((void *) 0);
	    ssl_free_wbio_buffer (s);
	    s->init_num = 0;
	    if (s->new_session == 2)
	      {
		s->new_session = 0;
		ssl_update_cache (s, 2);
		((s->ctx)->stats.sess_accept_good)++;
		s->handshake_func = &ssl3_accept;
		if ((unsigned int) cb != (unsigned int) ((void *) 0))
		  {
		    (*cb) ((SSL const *) s, 32, 1);
		  }
	      }
	    ret___3 = 1;
	    goto end;
	  default:
	    ERR_put_error (20, 128, 255, "s3_srvr.c", 588);
	    ret___3 = -1;
	    goto end;
	  }
	if (!(s->s3)->tmp.reuse_message)
	  {
	    if (!skip)
	      {
		if (s->debug)
		  {
		    tmp___11 = BIO_ctrl (s->wbio, 11, 0L, (void *) 0);
		    ret___3 = (int) tmp___11;
		    if (ret___3 <= 0)
		      {
			goto end;
		      }
		  }
		if ((unsigned int) cb != (unsigned int) ((void *) 0))
		  {
		    if (s->state != state___0)
		      {
			new_state = s->state;
			s->state = state___0;
			(*cb) ((SSL const *) s, 8193, 1);
			s->state = new_state;
		      }
		  }
	      }
	  }
	skip = 0;
      }
  end:(s->in_handshake)--;
    if ((unsigned int) cb != (unsigned int) ((void *) 0))
      {
	(*cb) ((SSL const *) s, 8194, ret___3);
      }
    return (ret___3);
  }
}

int
ssl3_send_hello_request (SSL * s)
{
  unsigned char *p;
  unsigned char *tmp;
  unsigned char *tmp___0;
  unsigned char *tmp___1;
  unsigned char *tmp___2;
  int tmp___3;
  {
    if (s->state == 8480)
      {
	p = (unsigned char *) (s->init_buf)->data;
	tmp = p;
	p++;
	*tmp = (unsigned char) 0;
	tmp___0 = p;
	p++;
	*tmp___0 = (unsigned char) 0;
	tmp___1 = p;
	p++;
	*tmp___1 = (unsigned char) 0;
	tmp___2 = p;
	p++;
	*tmp___2 = (unsigned char) 0;
	s->state = 8481;
	s->init_num = 4;
	s->init_off = 0;
      }
    tmp___3 = ssl3_do_write (s, 22);
    return (tmp___3);
  }
}

int
ssl3_check_client_hello (SSL * s)
{
  int ok;
  long n;
  {
    n =
      (*((s->method)->ssl_get_message)) (s, 8576, 8577, -1, s->max_cert_list,
					 &ok);
    if (!ok)
      {
	return ((int) n);
      }
    (s->s3)->tmp.reuse_message = 1;
    if ((s->s3)->tmp.message_type == 1)
      {
	if ((unsigned int) (s->s3)->tmp.dh != (unsigned int) ((void *) 0))
	  {
	    DH_free ((s->s3)->tmp.dh);
	    (s->s3)->tmp.dh = (DH *) ((void *) 0);
	  }
	return (2);
      }
    return (1);
  }
}

int
ssl3_get_client_hello (SSL * s)
{
  int i;
  int j;
  int ok;
  int al;
  int ret___3;
  unsigned int cookie_len;
  long n;
  unsigned long id;
  unsigned char *p;
  unsigned char *d;
  unsigned char *q;
  SSL_CIPHER *c;
  SSL_COMP *comp;
  STACK *ciphers;
  unsigned char *tmp;
  int tmp___0;
  int tmp___1;
  unsigned char *tmp___2;
  long tmp___3;
  int tmp___4;
  int tmp___5;
  int tmp___16;
  {
    ret___3 = -1;
    comp = (SSL_COMP *) ((void *) 0);
    ciphers = (STACK *) ((void *) 0);
    if (s->state == 8464)
      {
	s->state = 8465;
      }
    s->first_packet = 1;
    n = (*((s->method)->ssl_get_message)) (s, 8465, 8466, 1, 16384L, &ok);
    if (!ok)
      {
	return ((int) n);
      }
  }
}

int
ssl3_send_server_done (SSL * s)
{
  unsigned char *p;
  unsigned char *tmp;
  unsigned char *tmp___0;
  unsigned char *tmp___1;
  unsigned char *tmp___2;
  int tmp___3;
  {
    if (s->state == 8560)
      {
	p = (unsigned char *) (s->init_buf)->data;
	tmp = p;
	p++;
	*tmp = (unsigned char) 14;
	tmp___0 = p;
	p++;
	*tmp___0 = (unsigned char) 0;
	tmp___1 = p;
	p++;
	*tmp___1 = (unsigned char) 0;
	tmp___2 = p;
	p++;
	*tmp___2 = (unsigned char) 0;
	s->state = 8561;
	s->init_num = 4;
	s->init_off = 0;
      }
    tmp___3 = ssl3_do_write (s, 22);
    return (tmp___3);
  }
}

int
ssl3_send_server_key_exchange (SSL * s)
{
  unsigned char *q;
  int j;
  int num;
  RSA *rsa;
  unsigned char md_buf[36];
  unsigned int u;
  DH *dh;
  DH *dhp;
  EC_KEY *ecdh;
  EC_KEY *ecdhp;
  unsigned char *encodedPoint;
  int encodedlen;
  int curve_id;
  BN_CTX *bn_ctx;
  EVP_PKEY *pkey;
  unsigned char *p;
  unsigned char *d;
  int al;
  EVP_MD_CTX md_ctx;
  int tmp;
  int tmp___0;
  void *tmp___1;
  int tmp___2;
  EC_GROUP const *group;
  int tmp___3;
  int tmp___4;
  int tmp___5;
  int tmp___24;
  unsigned char *tmp___25;
  int tmp___26;
  {
    dh = (DH *) ((void *) 0);
    ecdh = (EC_KEY *) ((void *) 0);
    encodedPoint = (unsigned char *) ((void *) 0);
    encodedlen = 0;
  err:if ((unsigned int) encodedPoint != (unsigned int) ((void *) 0))
      {
	CRYPTO_free ((void *) encodedPoint);
      }
    BN_CTX_free (bn_ctx);
    EVP_MD_CTX_cleanup (&md_ctx);
    return (-1);
  }
}

int
ssl3_send_certificate_request (SSL * s)
{
  unsigned char *p;
  unsigned char *d;
  int i;
  int j;
  int nl;
  int off;
  int n;
  STACK *sk;
  X509_NAME *name;
  BUF_MEM *buf___5;
  void *tmp;
  int tmp___0;
  int tmp___1;
  unsigned char *tmp___2;
  unsigned char *tmp___3;
  unsigned char *tmp___4;
  unsigned char *tmp___5;
  unsigned char *tmp___6;
  int tmp___7;
  {
    sk = (STACK *) ((void *) 0);
    if (s->state == 8544)
      {
	buf___5 = s->init_buf;
	p = (unsigned char *) (buf___5->data + 4);
	d = p;
	p++;
	n = ssl3_get_req_cert_type (s, p);
	*(d + 0) = (unsigned char) n;
	p += n;
	n++;
	off = n;
	p += 2;
	n += 2;
	sk = SSL_get_client_CA_list ((SSL const *) s);
	nl = 0;
	if ((unsigned int) sk != (unsigned int) ((void *) 0))
	  {
	    i = 0;
	    while (1)
	      {
		tmp___1 = sk_num ((STACK const *) sk);
		if (!(i < tmp___1))
		  {
		    break;
		  }
		tmp = sk_value ((STACK const *) sk, i);
		name = (X509_NAME *) tmp;
		j = i2d_X509_NAME (name, (unsigned char **) ((void *) 0));
		tmp___0 = BUF_MEM_grow_clean (buf___5, ((4 + n) + j) + 2);
		if (!tmp___0)
		  {
		    ERR_put_error (20, 150, 7, "s3_srvr.c", 1629);
		    goto err;
		  }
		p = (unsigned char *) (buf___5->data + (4 + n));
		if (!(s->options & 536870912UL))
		  {
		    *(p + 0) = (unsigned char) ((j >> 8) & 255);
		    *(p + 1) = (unsigned char) (j & 255);
		    p += 2;
		    i2d_X509_NAME (name, &p);
		    n += 2 + j;
		    nl += 2 + j;
		  }
		else
		  {
		    d = p;
		    i2d_X509_NAME (name, &p);
		    j -= 2;
		    *(d + 0) = (unsigned char) ((j >> 8) & 255);
		    *(d + 1) = (unsigned char) (j & 255);
		    d += 2;
		    j += 2;
		    n += j;
		    nl += j;
		  } i++;
	  }}
	p = (unsigned char *) (buf___5->data + (4 + off));
	*(p + 0) = (unsigned char) ((nl >> 8) & 255);
	*(p + 1) = (unsigned char) (nl & 255);
	p += 2;
	d = (unsigned char *) buf___5->data;
	tmp___2 = d;
	d++;
	*tmp___2 = (unsigned char) 13;
	*(d + 0) = (unsigned char) ((n >> 16) & 255);
	*(d + 1) = (unsigned char) ((n >> 8) & 255);
	*(d + 2) = (unsigned char) (n & 255);
	d += 3;
	s->init_num = n + 4;
	s->init_off = 0;
	p = (unsigned char *) (s->init_buf)->data + s->init_num;
	tmp___3 = p;
	p++;
	*tmp___3 = (unsigned char) 14;
	tmp___4 = p;
	p++;
	*tmp___4 = (unsigned char) 0;
	tmp___5 = p;
	p++;
	*tmp___5 = (unsigned char) 0;
	tmp___6 = p;
	p++;
	*tmp___6 = (unsigned char) 0;
	s->init_num += 4;
	s->state = 8545;
      }
    tmp___7 = ssl3_do_write (s, 22);
    return (tmp___7);
  err:return (-1);
  }
}

int
ssl3_get_client_key_exchange (SSL * s)
{
  int i;
  int al;
  int ok;
  long n;
  unsigned long l;
  unsigned char *p;
  RSA *rsa;
  EVP_PKEY *pkey;
  BIGNUM *pub;
  DH *dh_srvr;
  EC_KEY *srvr_ecdh;
  EVP_PKEY *clnt_pub_pkey;
  EC_POINT *clnt_ecpoint;
  BN_CTX *bn_ctx;
  int tmp;
  int ret___3;
  int field_size;
  EC_KEY const *tkey;
  EC_GROUP const *group;
  BIGNUM const *priv_key;
  int tmp___0;
  int tmp___1;
  EC_POINT const *tmp___2;
  int tmp___3;
  int tmp___4;
  {
    rsa = (RSA *) ((void *) 0);
    pkey = (EVP_PKEY *) ((void *) 0);
    pub = (BIGNUM *) ((void *) 0);
    srvr_ecdh = (EC_KEY *) ((void *) 0);
    clnt_pub_pkey = (EVP_PKEY *) ((void *) 0);
    clnt_ecpoint = (EC_POINT *) ((void *) 0);
    bn_ctx = (BN_CTX *) ((void *) 0);
    n = (*((s->method)->ssl_get_message)) (s, 8592, 8593, 16, 2048L, &ok);
    if (!ok)
      {
	return ((int) n);
      }
    p = (unsigned char *) s->init_msg;
    l = ((s->s3)->tmp.new_cipher)->algorithms;
    if (l & 1UL)
      {
	if ((s->s3)->tmp.use_rsa_tmp)
	  {
	    if ((unsigned int) s->cert != (unsigned int) ((void *) 0))
	      {
		if ((unsigned int) (s->cert)->rsa_tmp !=
		    (unsigned int) ((void *) 0))
		  {
		    rsa = (s->cert)->rsa_tmp;
		  }
	      }
	    if ((unsigned int) rsa == (unsigned int) ((void *) 0))
	      {
		al = 40;
		ERR_put_error (20, 139, 173, "s3_srvr.c", 1733);
		goto f_err;
	      }
	  }
	else
	  {
	    pkey = (s->cert)->pkeys[0].privatekey;
	    if ((unsigned int) pkey == (unsigned int) ((void *) 0))
	      {
		al = 40;
		ERR_put_error (20, 139, 168, "s3_srvr.c", 1746);
		goto f_err;
	      }
	    else
	      {
		if (pkey->type != 6)
		  {
		    al = 40;
		    ERR_put_error (20, 139, 168, "s3_srvr.c", 1746);
		    goto f_err;
		  }
		else
		  {
		    if ((unsigned int) pkey->pkey.rsa ==
			(unsigned int) ((void *) 0))
		      {
			al = 40;
			ERR_put_error (20, 139, 168, "s3_srvr.c", 1746);
			goto f_err;
		      }
		  }
	      }
	    rsa = pkey->pkey.rsa;
	  }
	if (s->version > 768)
	  {
	    if (s->client_version != 256)
	      {
		i =
		  (int) (((unsigned int) *(p + 0) << 8) | (unsigned int)
			 *(p + 1));
		p += 2;
		if (n != (long) (i + 2))
		  {
		    if (!(s->options & 256UL))
		      {
			ERR_put_error (20, 139, 234, "s3_srvr.c", 1761);
			goto err;
		      }
		    else
		      {
			p -= 2;
		      }
		  }
		else
		  {
		    n = (long) i;
	      }}
	  }
	i =
	  RSA_private_decrypt ((int) n, (unsigned char const *) p, p, rsa, 1);
	al = -1;
	if (i != 48)
	  {
	    al = 50;
	  }
	if (al == -1)
	  {
	    if ((int) *(p + 0) == s->client_version >> 8)
	      {
		if (!((int) *(p + 1) == (s->client_version & 255)))
		  {
		    goto _L;
		  }
	      }
	    else
	      {
	      _L:if (s->options & 8388608UL)
		  {
		    if ((int) *(p + 0) == s->version >> 8)
		      {
			if (!((int) *(p + 1) == (s->version & 255)))
			  {
			    al = 50;
			  }
		      }
		    else
		      {
			al = 50;
		      }
		  }
		else
		  {
		    al = 50;
		  }
	      }
	  }
	if (al != -1)
	  {
	    ERR_clear_error ();
	    i = 48;
	    *(p + 0) = (unsigned char) (s->client_version >> 8);
	    *(p + 1) = (unsigned char) (s->client_version & 255);
	    tmp = RAND_pseudo_bytes (p + 2, i - 2);
	    if (tmp <= 0)
	      {
		goto err;
	      }
	  }
	(s->session)->master_key_length =
	  (*(((s->method)->ssl3_enc)->generate_master_secret)) (s,
								(s->session)->
								master_key, p,
								i);
	OPENSSL_cleanse ((void *) p, (unsigned int) i);
      }
    else
      {
	if (l & 22UL)
	  {
	    i =
	      (int) (((unsigned int) *(p + 0) << 8) | (unsigned int)
		     *(p + 1));
	    p += 2;
	    if (n != (long) (i + 2))
	      {
		if (!(s->options & 128UL))
		  {
		    ERR_put_error (20, 139, 148, "s3_srvr.c", 1835);
		    goto err;
		  }
		else
		  {
		    p -= 2;
		    i = (int) n;
	      }}
	    if (n == 0L)
	      {
		al = 40;
		ERR_put_error (20, 139, 236, "s3_srvr.c", 1848);
		goto f_err;
	      }
	    else
	      {
		if ((unsigned int) (s->s3)->tmp.dh ==
		    (unsigned int) ((void *) 0))
		  {
		    al = 40;
		    ERR_put_error (20, 139, 171, "s3_srvr.c", 1856);
		    goto f_err;
		  }
		else
		  {
		    dh_srvr = (s->s3)->tmp.dh;
		  }
	      }
	    pub =
	      BN_bin2bn ((unsigned char const *) p, i,
			 (BIGNUM *) ((void *) 0));
	    if ((unsigned int) pub == (unsigned int) ((void *) 0))
	      {
		ERR_put_error (20, 139, 130, "s3_srvr.c", 1866);
		goto err;
	      }
	    i = DH_compute_key (p, (BIGNUM const *) pub, dh_srvr);
	    if (i <= 0)
	      {
		ERR_put_error (20, 139, 5, "s3_srvr.c", 1874);
		goto err;
	      }
	    DH_free ((s->s3)->tmp.dh);
	    (s->s3)->tmp.dh = (DH *) ((void *) 0);
	    BN_clear_free (pub);
	    pub = (BIGNUM *) ((void *) 0);
	    (s->session)->master_key_length =
	      (*(((s->method)->ssl3_enc)->generate_master_secret)) (s,
								    (s->
								     session)->
								    master_key,
								    p, i);
	    OPENSSL_cleanse ((void *) p, (unsigned int) i);
	  }
	else
	  {
	    if (l & 64UL)
	      {
		goto _L___0;
	      }
	    else
	      {
		if (l & 128UL)
		  {
		  _L___0:ret___3 = 1;
		    field_size = 0;
		    srvr_ecdh = EC_KEY_new ();
		    if ((unsigned int) srvr_ecdh ==
			(unsigned int) ((void *) 0))
		      {
			ERR_put_error (20, 139, 65, "s3_srvr.c", 2096);
			goto err;
		      }
		    if (l & 64UL)
		      {
			tkey =
			  (EC_KEY const *) ((s->cert)->pkeys[5].privatekey)->
			  pkey.ec;
		      }
		    else
		      {
			tkey = (EC_KEY const *) (s->s3)->tmp.ecdh;
		      } group = EC_KEY_get0_group (tkey);
		    priv_key = EC_KEY_get0_private_key (tkey);
		    tmp___0 = EC_KEY_set_group (srvr_ecdh, group);
		    if (tmp___0)
		      {
			tmp___1 =
			  EC_KEY_set_private_key (srvr_ecdh, priv_key);
			if (!tmp___1)
			  {
			    ERR_put_error (20, 139, 16, "s3_srvr.c", 2121);
			    goto err;
			  }
		      }
		    else
		      {
			ERR_put_error (20, 139, 16, "s3_srvr.c", 2121);
			goto err;
		      }
		    clnt_ecpoint = EC_POINT_new (group);
		    if ((unsigned int) clnt_ecpoint ==
			(unsigned int) ((void *) 0))
		      {
			ERR_put_error (20, 139, 65, "s3_srvr.c", 2129);
			goto err;
		      }
		    if (n == 0L)
		      {
			if (l & 128UL)
			  {
			    al = 40;
			    ERR_put_error (20, 139, 311, "s3_srvr.c", 2140);
			    goto f_err;
			  }
			clnt_pub_pkey = X509_get_pubkey ((s->session)->peer);
			if ((unsigned int) clnt_pub_pkey ==
			    (unsigned int) ((void *) 0))
			  {
			    al = 40;
			    ERR_put_error (20, 139, 313, "s3_srvr.c", 2160);
			    goto f_err;
			  }
			else
			  {
			    if (clnt_pub_pkey->type != 408)
			      {
				al = 40;
				ERR_put_error (20, 139, 313, "s3_srvr.c",
					       2160);
				goto f_err;
			      }
			  }
			tmp___2 =
			  EC_KEY_get0_public_key ((EC_KEY const *)
						  clnt_pub_pkey->pkey.ec);
			tmp___3 = EC_POINT_copy (clnt_ecpoint, tmp___2);
			if (tmp___3 == 0)
			  {
			    ERR_put_error (20, 139, 16, "s3_srvr.c", 2168);
			    goto err;
			  }
			ret___3 = 2;
		      }
		    else
		      {
			bn_ctx = BN_CTX_new ();
			if ((unsigned int) bn_ctx ==
			    (unsigned int) ((void *) 0))
			  {
			    ERR_put_error (20, 139, 65, "s3_srvr.c", 2181);
			    goto err;
			  }
			i = (int) *p;
			p++;
			tmp___4 =
			  EC_POINT_oct2point (group, clnt_ecpoint,
					      (unsigned char const *) p,
					      (unsigned int) i, bn_ctx);
			if (tmp___4 == 0)
			  {
			    ERR_put_error (20, 139, 16, "s3_srvr.c", 2192);
			    goto err;
			  }
			p = (unsigned char *) (s->init_buf)->data;
		      } field_size = EC_GROUP_get_degree (group);
		    if (field_size <= 0)
		      {
			ERR_put_error (20, 139, 43, "s3_srvr.c", 2206);
			goto err;
		      }
		    i =
		      ECDH_compute_key ((void *) p,
					(unsigned int) ((field_size + 7) / 8),
					(EC_POINT const *) clnt_ecpoint,
					srvr_ecdh,
					(void
					 *(*)(void const *in, size_t inlen,
					      void *out,
					      size_t * outlen)) ((void *) 0));
		    if (i <= 0)
		      {
			ERR_put_error (20, 139, 43, "s3_srvr.c", 2213);
			goto err;
		      }
		    EVP_PKEY_free (clnt_pub_pkey);
		    EC_POINT_free (clnt_ecpoint);
		    if ((unsigned int) srvr_ecdh !=
			(unsigned int) ((void *) 0))
		      {
			EC_KEY_free (srvr_ecdh);
		      }
		    BN_CTX_free (bn_ctx);
		    (s->session)->master_key_length =
		      (*(((s->method)->ssl3_enc)->generate_master_secret)) (s,
									    (s->
									     session)->
									    master_key,
									    p,
									    i);
		    OPENSSL_cleanse ((void *) p, (unsigned int) i);
		    return (ret___3);
		  }
		else
		  {
		    al = 40;
		    ERR_put_error (20, 139, 249, "s3_srvr.c", 2235);
		    goto f_err;
		  }
	      }
	  }
      }
    return (1);
  f_err:ssl3_send_alert (s, 2, al);
  err:EVP_PKEY_free (clnt_pub_pkey);
    EC_POINT_free (clnt_ecpoint);
    if ((unsigned int) srvr_ecdh != (unsigned int) ((void *) 0))
      {
	EC_KEY_free (srvr_ecdh);
      }
    BN_CTX_free (bn_ctx);
    return (-1);
  }
}

int
ssl3_get_cert_verify (SSL * s)
{
  EVP_PKEY *pkey;
  unsigned char *p;
  int al;
  int ok;
  int ret___3;
  long n;
  int type;
  int i;
  int j;
  X509 *peer;
  {
    pkey = (EVP_PKEY *) ((void *) 0);
    ret___3 = 0;
    type = 0;
    n = (*((s->method)->ssl_get_message)) (s, 8608, 8609, -1, 514L, &ok);
    if (!ok)
      {
	return ((int) n);
      }
    if ((unsigned int) (s->session)->peer != (unsigned int) ((void *) 0))
      {
	peer = (s->session)->peer;
	pkey = X509_get_pubkey (peer);
	type = X509_certificate_type (peer, pkey);
      }
    else
      {
	peer = (X509 *) ((void *) 0);
	pkey = (EVP_PKEY *) ((void *) 0);
      }
    j = EVP_PKEY_size (pkey);
    if (i > j)
      {
	ERR_put_error (20, 136, 265, "s3_srvr.c", 2333);
	al = 50;
	goto f_err;
      }
    else
      {
	if (n > (long) j)
	  {
	    ERR_put_error (20, 136, 265, "s3_srvr.c", 2333);
	    al = 50;
	    goto f_err;
	  }
	else
	  {
	    if (n <= 0L)
	      {
		ERR_put_error (20, 136, 265, "s3_srvr.c", 2333);
		al = 50;
		goto f_err;
	      }
	  }
      }
    if (pkey->type == 6)
      {
	i =
	  RSA_verify (114,
		      (unsigned char const *) ((s->s3)->tmp.cert_verify_md),
		      36U, p, (unsigned int) i, pkey->pkey.rsa);
	if (i < 0)
	  {
	    al = 51;
	    ERR_put_error (20, 136, 118, "s3_srvr.c", 2347);
	    goto f_err;
	  }
	if (i == 0)
	  {
	    al = 51;
	    ERR_put_error (20, 136, 122, "s3_srvr.c", 2353);
	    goto f_err;
	  }
      }
    else
      {
	if (pkey->type == 116)
	  {
	    j =
	      DSA_verify (pkey->save_type,
			  (unsigned char const *) (&(s->s3)->tmp.
						   cert_verify_md[16]), 20,
			  (unsigned char const *) p, i, pkey->pkey.dsa);
	    if (j <= 0)
	      {
		al = 51;
		ERR_put_error (20, 136, 112, "s3_srvr.c", 2369);
		goto f_err;
	      }
	  }
	else
	  {
	    if (pkey->type == 408)
	      {
		j =
		  ECDSA_verify (pkey->save_type,
				(unsigned char const *) (&(s->s3)->tmp.
							 cert_verify_md[16]),
				20, (unsigned char const *) p, i,
				pkey->pkey.ec);
		if (j <= 0)
		  {
		    al = 51;
		    ERR_put_error (20, 136, 305, "s3_srvr.c", 2386);
		    goto f_err;
		  }
	      }
	    else
	      {
		ERR_put_error (20, 136, 68, "s3_srvr.c", 2393);
		al = 43;
		goto f_err;
	      }
	  }
      }
    ret___3 = 1;
    if (0)
      {
      f_err:ssl3_send_alert (s, 2, al);
      }
  end:EVP_PKEY_free (pkey);
    return (ret___3);
  }
}

int
ssl3_get_client_certificate (SSL * s)
{
  int i;
  int ok;
  int al;
  int ret___3;
  X509 *x;
  unsigned long l;
  unsigned long nc;
  unsigned long llen;
  unsigned long n;
  unsigned char const *p;
  unsigned char const *q;
  unsigned char *d;
  STACK *sk;
  long tmp;
  int tmp___0;
  int tmp___1;
  void *tmp___2;
  {
    ret___3 = -1;
    x = (X509 *) ((void *) 0);
    sk = (STACK *) ((void *) 0);
    tmp =
      (*((s->method)->ssl_get_message)) (s, 8576, 8577, -1, s->max_cert_list,
					 &ok);
    tmp___2 = sk_shift (sk);
    (s->session)->peer = (X509 *) tmp___2;
    (s->session)->verify_result = s->verify_result;
    if ((unsigned int) (s->session)->sess_cert == (unsigned int) ((void *) 0))
      {
	(s->session)->sess_cert = ssl_sess_cert_new ();
	if ((unsigned int) (s->session)->sess_cert ==
	    (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 137, 65, "s3_srvr.c", 2542);
	    goto err;
	  }
      }
    if ((unsigned int) ((s->session)->sess_cert)->cert_chain !=
	(unsigned int) ((void *) 0))
      {
	sk_pop_free (((s->session)->sess_cert)->cert_chain,
		     (void (*)(void *)) (&X509_free));
      }
    ((s->session)->sess_cert)->cert_chain = sk;
    sk = (STACK *) ((void *) 0);
    ret___3 = 1;
    if (0)
      {
      f_err:ssl3_send_alert (s, 2, al);
      }
  err:if ((unsigned int) x != (unsigned int) ((void *) 0))
      {
	X509_free (x);
      }
    if ((unsigned int) sk != (unsigned int) ((void *) 0))
      {
	sk_pop_free (sk, (void (*)(void *)) (&X509_free));
      }
    return (ret___3);
  }
}

int
ssl3_send_server_certificate (SSL * s)
{
  unsigned long l;
  X509 *x;
  int tmp;
  {
    if (s->state == 8512)
      {
	x = ssl_get_server_send_cert (s);
	if ((unsigned int) x == (unsigned int) ((void *) 0))
	  {
	    if ((((s->s3)->tmp.new_cipher)->algorithms & 32767UL) != 8224UL)
	      {
		ERR_put_error (20, 154, 68, "s3_srvr.c", 2580);
		return (0);
	      }
	  }
	l = ssl3_output_cert_chain (s, x);
	s->state = 8513;
	s->init_num = (int) l;
	s->init_off = 0;
      }
    tmp = ssl3_do_write (s, 22);
    return (tmp);
  }
}

RSA *RSA_new (void);
int RSA_size (RSA const *r);
DH *DH_new (void);
void X509_NAME_free (X509_NAME * a);
X509_NAME *d2i_X509_NAME (X509_NAME ** a, unsigned char const **in, long len);
int X509_NAME_cmp (X509_NAME const *a, X509_NAME const *b);
static int ca_dn_cmp (X509_NAME const *const *a, X509_NAME const *const *b);
static int curve_id2nid (int curve_id);
static SSL_METHOD *
ssl3_get_client_method (int ver)
{
  SSL_METHOD *tmp;
  {
    if (ver == 768)
      {
	tmp = SSLv3_client_method ();
	return (tmp);
      }
    else
      {
	return ((SSL_METHOD *) ((void *) 0));
      }
  }
}

int
ssl3_connect (SSL * s)
{
  BUF_MEM *buf___5;
  unsigned long Time;
  time_t tmp;
  unsigned long l;
  long num1;
  void (*cb) (SSL const *ssl, int type, int val);
  int ret___3;
  int new_state;
  int state___0;
  int skip;
  int *tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  int tmp___4;
  int tmp___5;
  int tmp___6;
  int tmp___7;
  int tmp___8;
  long tmp___9;
  long tmp___10;
  {
    buf___5 = (BUF_MEM *) ((void *) 0);
    tmp = time ((time_t *) ((void *) 0));
    Time = (unsigned long) tmp;
    cb = (void (*)(SSL const *ssl, int type, int val)) ((void *) 0);
    ret___3 = -1;
    skip = 0;
    RAND_add ((void const *) (&Time), (int) sizeof (Time), (double) 0);
    ERR_clear_error ();
    tmp___0 = __errno_location ();
    *tmp___0 = 0;
    if ((unsigned int) s->info_callback != (unsigned int) ((void *) 0))
      {
	cb = s->info_callback;
      }
    else
      {
	if ((unsigned int) (s->ctx)->info_callback !=
	    (unsigned int) ((void *) 0))
	  {
	    cb = (s->ctx)->info_callback;
	  }
      }
    (s->in_handshake)++;
    tmp___1 = SSL_state ((SSL const *) s);
    if (tmp___1 & 12288)
      {
	tmp___2 = SSL_state ((SSL const *) s);
	if (tmp___2 & 16384)
	  {
	    SSL_clear (s);
	  }
      }
    else
      {
	SSL_clear (s);
      }
    while (1)
      {
	state___0 = s->state;
	switch (s->state)
	  {
	  case 12292:
	    s->new_session = 1;
	    s->state = 4096;
	    ((s->ctx)->stats.sess_connect_renegotiate)++;
	  case 16384:
	  case 4096:
	  case 20480:
	  case 4099:
	    s->server = 0;
	    if ((unsigned int) cb != (unsigned int) ((void *) 0))
	      {
		(*cb) ((SSL const *) s, 16, 1);
	      }
	    if ((s->version & 65280) != 768)
	      {
		ERR_put_error (20, 132, 68, "s3_clnt.c", 204);
		ret___3 = -1;
		goto end;
	      }
	    s->type = 4096;
	    if ((unsigned int) s->init_buf == (unsigned int) ((void *) 0))
	      {
		buf___5 = BUF_MEM_new ();
		if ((unsigned int) buf___5 == (unsigned int) ((void *) 0))
		  {
		    ret___3 = -1;
		    goto end;
		  }
		tmp___3 = BUF_MEM_grow (buf___5, 16384);
		if (!tmp___3)
		  {
		    ret___3 = -1;
		    goto end;
		  }
		s->init_buf = buf___5;
		buf___5 = (BUF_MEM *) ((void *) 0);
	      }
	    tmp___4 = ssl3_setup_buffers (s);
	    if (!tmp___4)
	      {
		ret___3 = -1;
		goto end;
	      }
	    tmp___5 = ssl_init_wbio_buffer (s, 0);
	    if (!tmp___5)
	      {
		ret___3 = -1;
		goto end;
	      }
	    ssl3_init_finished_mac (s);
	    s->state = 4368;
	    ((s->ctx)->stats.sess_connect)++;
	    s->init_num = 0;
	    break;
	  case 4368:
	  case 4369:
	    s->shutdown = 0;
	    ret___3 = ssl3_client_hello (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 4384;
	    s->init_num = 0;
	    if ((unsigned int) s->bbio != (unsigned int) s->wbio)
	      {
		s->wbio = BIO_push (s->bbio, s->wbio);
	      }
	    break;
	  case 4384:
	  case 4385:
	    ret___3 = ssl3_get_server_hello (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    if (s->hit)
	      {
		s->state = 4560;
	      }
	    else
	      {
		s->state = 4400;
	      }
	    s->init_num = 0;
	    break;
	  case 4400:
	  case 4401:
	    if (!(((s->s3)->tmp.new_cipher)->algorithms & 2048UL))
	      {
		ret___3 = ssl3_get_server_certificate (s);
		if (ret___3 <= 0)
		  {
		    goto end;
		  }
	      }
	    else
	      {
		skip = 1;
	      }
	    s->state = 4416;
	    s->init_num = 0;
	    break;
	  case 4416:
	  case 4417:
	    ret___3 = ssl3_get_key_exchange (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 4432;
	    s->init_num = 0;
	    tmp___6 = ssl3_check_cert_and_algorithm (s);
	    if (!tmp___6)
	      {
		ret___3 = -1;
		goto end;
	      }
	    break;
	  case 4432:
	  case 4433:
	    ret___3 = ssl3_get_certificate_request (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 4448;
	    s->init_num = 0;
	    break;
	  case 4448:
	  case 4449:
	    ret___3 = ssl3_get_server_done (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    if ((s->s3)->tmp.cert_req)
	      {
		s->state = 4464;
	      }
	    else
	      {
		s->state = 4480;
	      }
	    s->init_num = 0;
	    break;
	  case 4464:
	  case 4465:
	  case 4466:
	  case 4467:
	    ret___3 = ssl3_send_client_certificate (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 4480;
	    s->init_num = 0;
	    break;
	  case 4480:
	  case 4481:
	    ret___3 = ssl3_send_client_key_exchange (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    l = ((s->s3)->tmp.new_cipher)->algorithms;
	    if ((s->s3)->tmp.cert_req == 1)
	      {
		s->state = 4496;
	      }
	    else
	      {
		s->state = 4512;
		(s->s3)->change_cipher_spec = 0;
	      }
	    s->init_num = 0;
	    break;
	  case 4496:
	  case 4497:
	    ret___3 = ssl3_send_client_verify (s);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 4512;
	    s->init_num = 0;
	    (s->s3)->change_cipher_spec = 0;
	    break;
	  case 4512:
	  case 4513:
	    ret___3 = ssl3_send_change_cipher_spec (s, 4512, 4513);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 4528;
	    s->init_num = 0;
	    (s->session)->cipher = (s->s3)->tmp.new_cipher;
	    if ((unsigned int) (s->s3)->tmp.new_compression ==
		(unsigned int) ((void *) 0))
	      {
		(s->session)->compress_meth = 0;
	      }
	    else
	      {
		(s->session)->compress_meth =
		  (int) ((s->s3)->tmp.new_compression)->id;
	      } tmp___7 = (*(((s->method)->ssl3_enc)->setup_key_block)) (s);
	    if (!tmp___7)
	      {
		ret___3 = -1;
		goto end;
	      }
	    tmp___8 =
	      (*(((s->method)->ssl3_enc)->change_cipher_state)) (s, 18);
	    if (!tmp___8)
	      {
		ret___3 = -1;
		goto end;
	      }
	    break;
	  case 4528:
	  case 4529:
	    ret___3 =
	      ssl3_send_finished (s, 4528, 4529,
				  ((s->method)->ssl3_enc)->
				  client_finished_label,
				  ((s->method)->ssl3_enc)->
				  client_finished_label_len);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    s->state = 4352;
	    (s->s3)->flags &= -5L;
	    if (s->hit)
	      {
		(s->s3)->tmp.next_state = 3;
		if ((s->s3)->flags & 2L)
		  {
		    s->state = 3;
		    (s->s3)->flags |= 4L;
		    (s->s3)->delay_buf_pop_ret = 0;
		  }
	      }
	    else
	      {
		(s->s3)->tmp.next_state = 4560;
	      }
	    s->init_num = 0;
	    break;
	  case 4560:
	  case 4561:
	    ret___3 = ssl3_get_finished (s, 4560, 4561);
	    if (ret___3 <= 0)
	      {
		goto end;
	      }
	    if (s->hit)
	      {
		s->state = 4512;
	      }
	    else
	      {
		s->state = 3;
	      }
	    s->init_num = 0;
	    break;
	  case 4352:
	    num1 = BIO_ctrl (s->wbio, 3, 0L, (void *) 0);
	    if (num1 > 0L)
	      {
		s->rwstate = 2;
		tmp___9 = BIO_ctrl (s->wbio, 11, 0L, (void *) 0);
		num1 = (long) ((int) tmp___9);
		if (num1 <= 0L)
		  {
		    ret___3 = -1;
		    goto end;
		  }
		s->rwstate = 1;
	      }
	    s->state = (s->s3)->tmp.next_state;
	    break;
	  case 3:
	    ssl3_cleanup_key_block (s);
	    if ((unsigned int) s->init_buf != (unsigned int) ((void *) 0))
	      {
		BUF_MEM_free (s->init_buf);
		s->init_buf = (BUF_MEM *) ((void *) 0);
	      }
	    if (!((s->s3)->flags & 4L))
	      {
		ssl_free_wbio_buffer (s);
	      }
	    s->init_num = 0;
	    s->new_session = 0;
	    ssl_update_cache (s, 1);
	    if (s->hit)
	      {
		((s->ctx)->stats.sess_hit)++;
	      }
	    ret___3 = 1;
	    s->handshake_func = &ssl3_connect;
	    ((s->ctx)->stats.sess_connect_good)++;
	    if ((unsigned int) cb != (unsigned int) ((void *) 0))
	      {
		(*cb) ((SSL const *) s, 32, 1);
	      }
	    goto end;
	  default:
	    ERR_put_error (20, 132, 255, "s3_clnt.c", 516);
	    ret___3 = -1;
	    goto end;
	  }
	if (!(s->s3)->tmp.reuse_message)
	  {
	    if (!skip)
	      {
		if (s->debug)
		  {
		    tmp___10 = BIO_ctrl (s->wbio, 11, 0L, (void *) 0);
		    ret___3 = (int) tmp___10;
		    if (ret___3 <= 0)
		      {
			goto end;
		      }
		  }
		if ((unsigned int) cb != (unsigned int) ((void *) 0))
		  {
		    if (s->state != state___0)
		      {
			new_state = s->state;
			s->state = state___0;
			(*cb) ((SSL const *) s, 4097, 1);
			s->state = new_state;
		      }
		  }
	      }
	  }
	skip = 0;
      }
  end:(s->in_handshake)--;
    if ((unsigned int) buf___5 != (unsigned int) ((void *) 0))
      {
	BUF_MEM_free (buf___5);
      }
    if ((unsigned int) cb != (unsigned int) ((void *) 0))
      {
	(*cb) ((SSL const *) s, 4098, ret___3);
      }
    return (ret___3);
  }
}

int
ssl3_client_hello (SSL * s)
{
  unsigned char *buf___5;
  unsigned char *p;
  unsigned char *d;
  int i;
  unsigned long Time;
  unsigned long l;
  int j;
  SSL_COMP *comp;
  int tmp;
  time_t tmp___0;
  unsigned char *tmp___1;
  unsigned char *tmp___2;
  unsigned char *tmp___3;
  unsigned char *tmp___4;
  int tmp___5;
  unsigned char *tmp___6;
  unsigned char *tmp___7;
  unsigned char *tmp___8;
  STACK *tmp___9;
  unsigned char *tmp___10;
  void *tmp___11;
  unsigned char *tmp___12;
  unsigned char *tmp___13;
  unsigned char *tmp___14;
  int tmp___15;
  {
    buf___5 = (unsigned char *) (s->init_buf)->data;
    if (s->state == 4368)
      {
	if ((unsigned int) s->session == (unsigned int) ((void *) 0))
	  {
	    goto _L;
	  }
	else
	  {
	    if ((s->session)->ssl_version != s->version)
	      {
		goto _L;
	      }
	    else
	      {
		if ((s->session)->not_resumable)
		  {
		  _L:tmp = ssl_get_new_session (s, 0);
		    if (!tmp)
		      {
			goto err;
		      }
		  }
	      }
	  }
	p = (s->s3)->client_random;
	tmp___0 = time ((time_t *) ((void *) 0));
	Time = (unsigned long) tmp___0;
	tmp___1 = p;
	p++;
	*tmp___1 = (unsigned char) ((Time >> 24) & 255UL);
	tmp___2 = p;
	p++;
	*tmp___2 = (unsigned char) ((Time >> 16) & 255UL);
	tmp___3 = p;
	p++;
	*tmp___3 = (unsigned char) ((Time >> 8) & 255UL);
	tmp___4 = p;
	p++;
	*tmp___4 = (unsigned char) (Time & 255UL);
	tmp___5 = RAND_pseudo_bytes (p, 28);
	if (tmp___5 <= 0)
	  {
	    goto err;
	  }
	p = buf___5 + 4;
	d = p;
	tmp___6 = p;
	p++;
	*tmp___6 = (unsigned char) (s->version >> 8);
	tmp___7 = p;
	p++;
	*tmp___7 = (unsigned char) (s->version & 255);
	s->client_version = s->version;
	memcpy ((void *__restrict) p,
		(void const *__restrict) ((s->s3)->client_random), 32U);
	p += 32;
	if (s->new_session)
	  {
	    i = 0;
	  }
	else
	  {
	    i = (int) (s->session)->session_id_length;
	  } tmp___8 = p;
	p++;
	*tmp___8 = (unsigned char) i;
	if (i != 0)
	  {
	    if (i > (int) sizeof ((s->session)->session_id))
	      {
		ERR_put_error (20, 131, 68, "s3_clnt.c", 601);
		goto err;
	      }
	    memcpy ((void *__restrict) p,
		    (void const *__restrict) ((s->session)->session_id),
		    (unsigned int) i);
	    p += i;
	  }
	tmp___9 = SSL_get_ciphers ((SSL const *) s);
	i =
	  ssl_cipher_list_to_bytes (s, tmp___9, p + 2,
				    (int (*)
				     (SSL_CIPHER const *,
				      unsigned char *)) 0);
	if (i == 0)
	  {
	    ERR_put_error (20, 131, 181, "s3_clnt.c", 612);
	    goto err;
	  }
	*(p + 0) = (unsigned char) ((i >> 8) & 255);
	*(p + 1) = (unsigned char) (i & 255);
	p += 2;
	p += i;
	if ((unsigned int) (s->ctx)->comp_methods ==
	    (unsigned int) ((void *) 0))
	  {
	    j = 0;
	  }
	else
	  {
	    j = sk_num ((STACK const *) (s->ctx)->comp_methods);
	  } tmp___10 = p;
	p++;
	*tmp___10 = (unsigned char) (1 + j);
	i = 0;
	while (i < j)
	  {
	    tmp___11 = sk_value ((STACK const *) (s->ctx)->comp_methods, i);
	    comp = (SSL_COMP *) tmp___11;
	    tmp___12 = p;
	    p++;
	    *tmp___12 = (unsigned char) comp->id;
	    i++;
	  } tmp___13 = p;
	p++;
	*tmp___13 = (unsigned char) 0;
	l = (unsigned long) (p - d);
	d = buf___5;
	tmp___14 = d;
	d++;
	*tmp___14 = (unsigned char) 1;
	*(d + 0) = (unsigned char) ((l >> 16) & 255UL);
	*(d + 1) = (unsigned char) ((l >> 8) & 255UL);
	*(d + 2) = (unsigned char) (l & 255UL);
	d += 3;
	s->state = 4369;
	s->init_num = p - buf___5;
	s->init_off = 0;
      }
    tmp___15 = ssl3_do_write (s, 22);
    return (tmp___15);
  err:return (-1);
  }
}

int
ssl3_get_server_hello (SSL * s)
{
  STACK *sk;
  SSL_CIPHER *c;
  unsigned char *p;
  unsigned char *d;
  int i;
  int al;
  int ok;
  unsigned int j;
  long n;
  SSL_COMP *comp;
  int tmp;
  unsigned char *tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  int tmp___4;
  unsigned char *tmp___5;
  {
    n = (*((s->method)->ssl_get_message)) (s, 4384, 4385, -1, 20000L, &ok);
    if (!ok)
      {
	return ((int) n);
      }
    tmp = SSL_version ((SSL const *) s);
    if (tmp == 65279)
      {
	if ((s->s3)->tmp.message_type == 3)
	  {
	    if ((s->d1)->send_cookie == 0U)
	      {
		(s->s3)->tmp.reuse_message = 1;
		return (1);
	      }
	    else
	      {
		al = 10;
		ERR_put_error (20, 146, 114, "s3_clnt.c", 691);
		goto f_err;
	      }
	  }
      }
    if ((s->s3)->tmp.message_type != 2)
      {
	al = 10;
	ERR_put_error (20, 146, 114, "s3_clnt.c", 700);
	goto f_err;
      }
    p = (unsigned char *) s->init_msg;
    d = p;
    if ((int) *(p + 0) != s->version >> 8)
      {
	ERR_put_error (20, 146, 266, "s3_clnt.c", 708);
	s->version = (s->version & 65280) | (int) *(p + 1);
	al = 70;
	goto f_err;
      }
    if (j == 0U)
      {
	comp = (SSL_COMP *) ((void *) 0);
      }
    else
      {
	comp = ssl3_comp_find ((s->ctx)->comp_methods, (int) j);
      }
    if (j != 0U)
      {
	if ((unsigned int) comp == (unsigned int) ((void *) 0))
	  {
	    al = 47;
	    ERR_put_error (20, 146, 257, "s3_clnt.c", 816);
	    goto f_err;
	  }
	else
	  {
	    (s->s3)->tmp.new_compression = (SSL_COMP const *) comp;
      }}
    else
      {
	(s->s3)->tmp.new_compression = (SSL_COMP const *) comp;
      }
    if ((unsigned int) p != (unsigned int) (d + n))
      {
	al = 50;
	ERR_put_error (20, 146, 115, "s3_clnt.c", 847);
	goto err;
      }
    return (1);
  f_err:ssl3_send_alert (s, 2, al);
  err:return (-1);
  }
}

int
ssl3_get_server_certificate (SSL * s)
{
  int al;
  int i;
  int ok;
  int ret___3;
  unsigned long n;
  unsigned long nc;
  unsigned long llen;
  unsigned long l;
  X509 *x;
  unsigned char const *q;
  unsigned char const *p;
  unsigned char *d;
  STACK *sk;
  SESS_CERT *sc;
  EVP_PKEY *pkey;
  int need_cert;
  long tmp;
  int tmp___0;
  void *tmp___1;
  int tmp___2;
  {
    ret___3 = -1;
    x = (X509 *) ((void *) 0);
    if (need_cert)
      {
	sc->peer_cert_type = i;
	CRYPTO_add_lock (&x->references, 1, lock3, "s3_clnt.c", 1005);
	if ((unsigned int) sc->peer_pkeys[i].x509 !=
	    (unsigned int) ((void *) 0))
	  {
	    X509_free (sc->peer_pkeys[i].x509);
	  }
	sc->peer_pkeys[i].x509 = x;
	sc->peer_key = &sc->peer_pkeys[i];
	if ((unsigned int) (s->session)->peer != (unsigned int) ((void *) 0))
	  {
	    X509_free ((s->session)->peer);
	  }
	CRYPTO_add_lock (&x->references, 1, lock3, "s3_clnt.c", 1015);
	(s->session)->peer = x;
      }
    else
      {
	sc->peer_cert_type = i;
	sc->peer_key = (CERT_PKEY *) ((void *) 0);
	if ((unsigned int) (s->session)->peer != (unsigned int) ((void *) 0))
	  {
	    X509_free ((s->session)->peer);
	  }
	(s->session)->peer = (X509 *) ((void *) 0);
      }
    (s->session)->verify_result = s->verify_result;
    x = (X509 *) ((void *) 0);
    ret___3 = 1;
    if (0)
      {
      f_err:ssl3_send_alert (s, 2, al);
      }
  err:EVP_PKEY_free (pkey);
    X509_free (x);
    sk_pop_free (sk, (void (*)(void *)) (&X509_free));
    return (ret___3);
  }
}

int
ssl3_get_key_exchange (SSL * s)
{
  unsigned char *q;
  unsigned char md_buf[128];
  EVP_MD_CTX md_ctx;
  unsigned char *param;
  unsigned char *p;
  int al;
  int i;
  int j;
  int param_len;
  int ok;
  long n;
  long alg;
  EVP_PKEY *pkey;
  RSA *rsa;
  DH *dh;
  EC_KEY *ecdh;
  BN_CTX *bn_ctx;
  EC_POINT *srvr_ecpoint;
  int curve_nid;
  int encoded_pt_len;
  BIGNUM *tmp;
  BIGNUM *tmp___0;
  BIGNUM *tmp___1;
  BIGNUM *tmp___2;
  BIGNUM *tmp___3;
  EC_GROUP *ngroup;
  EC_GROUP const *group;
  int tmp___4;
  int tmp___5;
  int tmp___6;
  int num;
  EVP_MD const *tmp___7;
  EVP_MD const *tmp___8;
  int tmp___9;
  EVP_MD const *tmp___10;
  int tmp___11;
  {
    pkey = (EVP_PKEY *) ((void *) 0);
    if (alg & 1L)
      {
	rsa = RSA_new ();
	if ((unsigned int) rsa == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 141, 65, "s3_clnt.c", 1125);
	    goto err;
	  }
	i = (int) (((unsigned int) *(p + 0) << 8) | (unsigned int) *(p + 1));
	p += 2;
	param_len = i + 2;
	if ((long) param_len > n)
	  {
	    al = 50;
	    ERR_put_error (20, 141, 121, "s3_clnt.c", 1133);
	    goto f_err;
	  }
	tmp = BN_bin2bn ((unsigned char const *) p, i, rsa->n);
	rsa->n = tmp;
	if (!tmp)
	  {
	    ERR_put_error (20, 141, 3, "s3_clnt.c", 1138);
	    goto err;
	  }
	p += i;
	i = (int) (((unsigned int) *(p + 0) << 8) | (unsigned int) *(p + 1));
	p += 2;
	param_len += i + 2;
	if ((long) param_len > n)
	  {
	    al = 50;
	    ERR_put_error (20, 141, 120, "s3_clnt.c", 1148);
	    goto f_err;
	  }
	tmp___0 = BN_bin2bn ((unsigned char const *) p, i, rsa->e);
	rsa->e = tmp___0;
	if (!tmp___0)
	  {
	    ERR_put_error (20, 141, 3, "s3_clnt.c", 1153);
	    goto err;
	  }
	p += i;
	n -= (long) param_len;
	if (alg & 256L)
	  {
	    pkey =
	      X509_get_pubkey (((s->session)->sess_cert)->peer_pkeys[0].x509);
	  }
	else
	  {
	    ERR_put_error (20, 141, 68, "s3_clnt.c", 1164);
	    goto err;
	  }
	((s->session)->sess_cert)->peer_rsa_tmp = rsa;
	rsa = (RSA *) ((void *) 0);
      }
    else
      {
	if (alg & 16L)
	  {
	    dh = DH_new ();
	    if ((unsigned int) dh == (unsigned int) ((void *) 0))
	      {
		ERR_put_error (20, 141, 5, "s3_clnt.c", 1179);
		goto err;
	      }
	    i =
	      (int) (((unsigned int) *(p + 0) << 8) | (unsigned int)
		     *(p + 1));
	    p += 2;
	    param_len = i + 2;
	    if ((long) param_len > n)
	      {
		al = 50;
		ERR_put_error (20, 141, 110, "s3_clnt.c", 1187);
		goto f_err;
	      }
	    tmp___1 =
	      BN_bin2bn ((unsigned char const *) p, i,
			 (BIGNUM *) ((void *) 0));
	    dh->p = tmp___1;
	    if (!tmp___1)
	      {
		ERR_put_error (20, 141, 3, "s3_clnt.c", 1192);
		goto err;
	      }
	    p += i;
	    i =
	      (int) (((unsigned int) *(p + 0) << 8) | (unsigned int)
		     *(p + 1));
	    p += 2;
	    param_len += i + 2;
	    if ((long) param_len > n)
	      {
		al = 50;
		ERR_put_error (20, 141, 108, "s3_clnt.c", 1202);
		goto f_err;
	      }
	    tmp___2 =
	      BN_bin2bn ((unsigned char const *) p, i,
			 (BIGNUM *) ((void *) 0));
	    dh->g = tmp___2;
	    if (!tmp___2)
	      {
		ERR_put_error (20, 141, 3, "s3_clnt.c", 1207);
		goto err;
	      }
	    p += i;
	    i =
	      (int) (((unsigned int) *(p + 0) << 8) | (unsigned int)
		     *(p + 1));
	    p += 2;
	    param_len += i + 2;
	    if ((long) param_len > n)
	      {
		al = 50;
		ERR_put_error (20, 141, 109, "s3_clnt.c", 1217);
		goto f_err;
	      }
	    tmp___3 =
	      BN_bin2bn ((unsigned char const *) p, i,
			 (BIGNUM *) ((void *) 0));
	    dh->pub_key = tmp___3;
	    if (!tmp___3)
	      {
		ERR_put_error (20, 141, 3, "s3_clnt.c", 1222);
		goto err;
	      }
	    p += i;
	    n -= (long) param_len;
	    if (alg & 256L)
	      {
		pkey =
		  X509_get_pubkey (((s->session)->sess_cert)->peer_pkeys[0].
				   x509);
	      }
	    else
	      {
		if (alg & 512L)
		  {
		    pkey =
		      X509_get_pubkey (((s->session)->sess_cert)->
				       peer_pkeys[2].x509);
		  }
	      }
	    ((s->session)->sess_cert)->peer_dh_tmp = dh;
	    dh = (DH *) ((void *) 0);
	  }
	else
	  {
	    if (alg & 2L)
	      {
		al = 47;
		ERR_put_error (20, 141, 235, "s3_clnt.c", 1247);
		goto f_err;
	      }
	    else
	      {
		if (alg & 4L)
		  {
		    al = 47;
		    ERR_put_error (20, 141, 235, "s3_clnt.c", 1247);
		    goto f_err;
		  }
		else
		  {
		    if (alg & 128L)
		      {
			ecdh = EC_KEY_new ();
			if ((unsigned int) ecdh ==
			    (unsigned int) ((void *) 0))
			  {
			    ERR_put_error (20, 141, 65, "s3_clnt.c", 1260);
			    goto err;
			  }
			param_len = 3;
			if ((long) param_len > n)
			  {
			    al = 80;
			    ERR_put_error (20, 141, 314, "s3_clnt.c", 1279);
			    goto f_err;
			  }
			else
			  {
			    if ((int) *p != 3)
			      {
				al = 80;
				ERR_put_error (20, 141, 314, "s3_clnt.c",
					       1279);
				goto f_err;
			      }
			    else
			      {
				curve_nid = curve_id2nid ((int) *(p + 2));
				if (curve_nid == 0)
				  {
				    al = 80;
				    ERR_put_error (20, 141, 314, "s3_clnt.c",
						   1279);
				    goto f_err;
				  }
			      }
			  }
			ngroup = EC_GROUP_new_by_curve_name (curve_nid);
			if ((unsigned int) ngroup ==
			    (unsigned int) ((void *) 0))
			  {
			    ERR_put_error (20, 141, 16, "s3_clnt.c", 1286);
			    goto err;
			  }
			tmp___4 =
			  EC_KEY_set_group (ecdh, (EC_GROUP const *) ngroup);
			if (tmp___4 == 0)
			  {
			    ERR_put_error (20, 141, 16, "s3_clnt.c", 1291);
			    goto err;
			  }
			EC_GROUP_free (ngroup);
			group = EC_KEY_get0_group ((EC_KEY const *) ecdh);
			if (((s->s3)->tmp.new_cipher)->algo_strength & 2UL)
			  {
			    tmp___5 = EC_GROUP_get_degree (group);
			    if (tmp___5 > 163)
			      {
				al = 60;
				ERR_put_error (20, 141, 310, "s3_clnt.c",
					       1302);
				goto f_err;
			      }
			  }
			p += 3;
			srvr_ecpoint = EC_POINT_new (group);
			if ((unsigned int) srvr_ecpoint ==
			    (unsigned int) ((void *) 0))
			  {
			    ERR_put_error (20, 141, 65, "s3_clnt.c", 1312);
			    goto err;
			  }
			else
			  {
			    bn_ctx = BN_CTX_new ();
			    if ((unsigned int) bn_ctx ==
				(unsigned int) ((void *) 0))
			      {
				ERR_put_error (20, 141, 65, "s3_clnt.c",
					       1312);
				goto err;
			      }
			  }
			encoded_pt_len = (int) *p;
			p++;
			param_len += 1 + encoded_pt_len;
			if ((long) param_len > n)
			  {
			    al = 50;
			    ERR_put_error (20, 141, 306, "s3_clnt.c", 1324);
			    goto f_err;
			  }
			else
			  {
			    tmp___6 =
			      EC_POINT_oct2point (group, srvr_ecpoint,
						  (unsigned char const *) p,
						  (unsigned int)
						  encoded_pt_len, bn_ctx);
			    if (tmp___6 == 0)
			      {
				al = 50;
				ERR_put_error (20, 141, 306, "s3_clnt.c",
					       1324);
				goto f_err;
			      }
			  }
			n -= (long) param_len;
			p += encoded_pt_len;
			if (alg & 256L)
			  {
			    pkey =
			      X509_get_pubkey (((s->session)->sess_cert)->
					       peer_pkeys[0].x509);
			  }
			else
			  {
			    if (alg & 16384L)
			      {
				pkey =
				  X509_get_pubkey (((s->session)->sess_cert)->
						   peer_pkeys[5].x509);
			      }
			  }
			EC_KEY_set_public_key (ecdh,
					       (EC_POINT const *)
					       srvr_ecpoint);
			((s->session)->sess_cert)->peer_ecdh_tmp = ecdh;
			ecdh = (EC_KEY *) ((void *) 0);
			BN_CTX_free (bn_ctx);
			EC_POINT_free (srvr_ecpoint);
			srvr_ecpoint = (EC_POINT *) ((void *) 0);
		      }
		    else
		      {
			if (alg & 64L)
			  {
			    al = 10;
			    ERR_put_error (20, 141, 244, "s3_clnt.c", 1355);
			    goto f_err;
			  }
		      }
		  }
	      }
	  }
      }
    if (alg & 1024L)
      {
	al = 40;
	ERR_put_error (20, 141, 235, "s3_clnt.c", 1362);
	goto f_err;
      }
    if ((unsigned int) pkey != (unsigned int) ((void *) 0))
      {
	i = (int) (((unsigned int) *(p + 0) << 8) | (unsigned int) *(p + 1));
	p += 2;
	n -= 2L;
	j = EVP_PKEY_size (pkey);
	if ((long) i != n)
	  {
	    al = 50;
	    ERR_put_error (20, 141, 264, "s3_clnt.c", 1380);
	    goto f_err;
	  }
	else
	  {
	    if (n > (long) j)
	      {
		al = 50;
		ERR_put_error (20, 141, 264, "s3_clnt.c", 1380);
		goto f_err;
	      }
	    else
	      {
		if (n <= 0L)
		  {
		    al = 50;
		    ERR_put_error (20, 141, 264, "s3_clnt.c", 1380);
		    goto f_err;
		  }
	      }
	  }
	if (pkey->type == 6)
	  {
	    j = 0;
	    q = md_buf;
	    num = 2;
	    while (num > 0)
	      {
		if (num == 2)
		  {
		    tmp___7 = (s->ctx)->md5;
		  }
		else
		  {
		    tmp___7 = (s->ctx)->sha1;
		  }
		EVP_DigestInit_ex (&md_ctx, tmp___7, (ENGINE *) ((void *) 0));
		EVP_DigestUpdate (&md_ctx,
				  (void const *) (&(s->s3)->client_random[0]),
				  32U);
		EVP_DigestUpdate (&md_ctx,
				  (void const *) (&(s->s3)->server_random[0]),
				  32U);
		EVP_DigestUpdate (&md_ctx, (void const *) param,
				  (unsigned int) param_len);
		EVP_DigestFinal_ex (&md_ctx, q, (unsigned int *) (&i));
		q += i;
		j += i;
		num--;
	      } i =
	      RSA_verify (114, (unsigned char const *) (md_buf),
			  (unsigned int) j, p, (unsigned int) n,
			  pkey->pkey.rsa);
	    if (i < 0)
	      {
		al = 51;
		ERR_put_error (20, 141, 118, "s3_clnt.c", 1407);
		goto f_err;
	      }
	    if (i == 0)
	      {
		al = 51;
		ERR_put_error (20, 141, 123, "s3_clnt.c", 1414);
		goto f_err;
	      }
	  }
	else
	  {
	    if (pkey->type == 116)
	      {
		tmp___8 = EVP_dss1 ();
		EVP_DigestInit_ex (&md_ctx, tmp___8, (ENGINE *) ((void *) 0));
		EVP_DigestUpdate (&md_ctx,
				  (void const *) (&(s->s3)->client_random[0]),
				  32U);
		EVP_DigestUpdate (&md_ctx,
				  (void const *) (&(s->s3)->server_random[0]),
				  32U);
		EVP_DigestUpdate (&md_ctx, (void const *) param,
				  (unsigned int) param_len);
		tmp___9 =
		  EVP_VerifyFinal (&md_ctx, (unsigned char const *) p,
				   (unsigned int) ((int) n), pkey);
		if (!tmp___9)
		  {
		    al = 51;
		    ERR_put_error (20, 141, 123, "s3_clnt.c", 1432);
		    goto f_err;
		  }
	      }
	    else
	      {
		if (pkey->type == 408)
		  {
		    tmp___10 = EVP_ecdsa ();
		    EVP_DigestInit_ex (&md_ctx, tmp___10,
				       (ENGINE *) ((void *) 0));
		    EVP_DigestUpdate (&md_ctx,
				      (void const *) (&(s->s3)->
						      client_random[0]), 32U);
		    EVP_DigestUpdate (&md_ctx,
				      (void const *) (&(s->s3)->
						      server_random[0]), 32U);
		    EVP_DigestUpdate (&md_ctx, (void const *) param,
				      (unsigned int) param_len);
		    tmp___11 =
		      EVP_VerifyFinal (&md_ctx, (unsigned char const *) p,
				       (unsigned int) ((int) n), pkey);
		    if (!tmp___11)
		      {
			al = 51;
			ERR_put_error (20, 141, 123, "s3_clnt.c", 1450);
			goto f_err;
		      }
		  }
		else
		  {
		    ERR_put_error (20, 141, 68, "s3_clnt.c", 1457);
		    goto err;
		  }
	      }
	  }
      }
    else
      {
	if (!(alg & 2048L))
	  {
	    ERR_put_error (20, 141, 68, "s3_clnt.c", 1466);
	    goto err;
	  }
	if (n != 0L)
	  {
	    al = 50;
	    ERR_put_error (20, 141, 153, "s3_clnt.c", 1472);
	    goto f_err;
	  }
      }
    EVP_PKEY_free (pkey);
    EVP_MD_CTX_cleanup (&md_ctx);
    return (1);
  f_err:ssl3_send_alert (s, 2, al);
  err:EVP_PKEY_free (pkey);
    if ((unsigned int) rsa != (unsigned int) ((void *) 0))
      {
	RSA_free (rsa);
      }
    if ((unsigned int) dh != (unsigned int) ((void *) 0))
      {
	DH_free (dh);
      }
    BN_CTX_free (bn_ctx);
    EC_POINT_free (srvr_ecpoint);
    if ((unsigned int) ecdh != (unsigned int) ((void *) 0))
      {
	EC_KEY_free (ecdh);
      }
    EVP_MD_CTX_cleanup (&md_ctx);
    return (-1);
  }
}

int
ssl3_get_certificate_request (SSL * s)
{
  int ok;
  int ret___3;
  unsigned long n;
  unsigned long nc;
  unsigned long l;
  unsigned int llen;
  unsigned int ctype_num;
  unsigned int i;
  X509_NAME *xn;
  unsigned char const *p;
  unsigned char const *q;
  unsigned char *d;
  STACK *ca_sk;
  long tmp;
  unsigned char const *tmp___0;
  int tmp___1;
  {
    ret___3 = 0;
    xn = (X509_NAME *) ((void *) 0);
    ca_sk = (STACK *) ((void *) 0);
    tmp =
      (*((s->method)->ssl_get_message)) (s, 4432, 4433, -1, s->max_cert_list,
					 &ok);
    n = (unsigned long) tmp;
    if (!ok)
      {
	return ((int) n);
      }
    ret___3 = 1;
  err:if ((unsigned int) ca_sk != (unsigned int) ((void *) 0))
      {
	sk_pop_free (ca_sk, (void (*)(void *)) (&X509_NAME_free));
      }
    return (ret___3);
  }
}

int
ssl3_get_server_done (SSL * s)
{
  int ok;
  int ret___3;
  long n;
  {
    ret___3 = 0;
    n = (*((s->method)->ssl_get_message)) (s, 4448, 4449, 14, 30L, &ok);
    if (!ok)
      {
	return ((int) n);
      }
    if (n > 0L)
      {
	ssl3_send_alert (s, 2, 50);
	ERR_put_error (20, 145, 159, "s3_clnt.c", 1734);
	return (-1);
      }
    ret___3 = 1;
    return (ret___3);
  }
}

int
ssl3_send_client_key_exchange (SSL * s)
{
  unsigned char *p;
  unsigned char *d;
  int n;
  unsigned long l;
  unsigned char *q;
  EVP_PKEY *pkey;
  EC_KEY *clnt_ecdh;
  EC_POINT const *srvr_ecpoint;
  EVP_PKEY *srvr_pub_pkey;
  unsigned char *encodedPoint;
  int encoded_pt_len;
  BN_CTX *bn_ctx;
  RSA *rsa;
  unsigned char tmp_buf[48];
  int tmp;
  DH *dh_srvr;
  DH *dh_clnt;
  void *tmp___0;
  int tmp___1;
  int tmp___2;
  EC_GROUP const *srvr_group;
  EC_KEY *tkey;
  int ecdh_clnt_cert;
  int field_size;
  int tmp___3;
  BIGNUM const *priv_key;
  int tmp___4;
  int tmp___5;
  EC_POINT const *tmp___6;
  size_t tmp___7;
  void *tmp___8;
  EC_POINT const *tmp___9;
  size_t tmp___10;
  unsigned char *tmp___11;
  int tmp___12;
  {
    pkey = (EVP_PKEY *) ((void *) 0);
    clnt_ecdh = (EC_KEY *) ((void *) 0);
    srvr_ecpoint = (EC_POINT const *) ((void *) 0);
    srvr_pub_pkey = (EVP_PKEY *) ((void *) 0);
    encodedPoint = (unsigned char *) ((void *) 0);
    encoded_pt_len = 0;
    bn_ctx = (BN_CTX *) ((void *) 0);
    if (s->state == 4480)
      {
	d = (unsigned char *) (s->init_buf)->data;
	p = d + 4;
	l = ((s->s3)->tmp.new_cipher)->algorithms;
	if (l & 1UL)
	  {
	    if ((unsigned int) ((s->session)->sess_cert)->peer_rsa_tmp !=
		(unsigned int) ((void *) 0))
	      {
		rsa = ((s->session)->sess_cert)->peer_rsa_tmp;
	      }
	    else
	      {
		pkey =
		  X509_get_pubkey (((s->session)->sess_cert)->peer_pkeys[0].
				   x509);
		if ((unsigned int) pkey == (unsigned int) ((void *) 0))
		  {
		    ERR_put_error (20, 152, 68, "s3_clnt.c", 1787);
		    goto err;
		  }
		else
		  {
		    if (pkey->type != 6)
		      {
			ERR_put_error (20, 152, 68, "s3_clnt.c", 1787);
			goto err;
		      }
		    else
		      {
			if ((unsigned int) pkey->pkey.rsa ==
			    (unsigned int) ((void *) 0))
			  {
			    ERR_put_error (20, 152, 68, "s3_clnt.c", 1787);
			    goto err;
			  }
		      }
		  }
		rsa = pkey->pkey.rsa;
		EVP_PKEY_free (pkey);
	      }
	    tmp_buf[0] = (unsigned char) (s->client_version >> 8);
	    tmp_buf[1] = (unsigned char) (s->client_version & 255);
	    tmp = RAND_bytes (&tmp_buf[2], (int) (sizeof (tmp_buf) - 2U));
	    if (tmp <= 0)
	      {
		goto err;
	      }
	    (s->session)->master_key_length = (int) sizeof (tmp_buf);
	    q = p;
	    if (s->version > 768)
	      {
		p += 2;
	      }
	    n =
	      RSA_public_encrypt ((int) sizeof (tmp_buf),
				  (unsigned char const *) (tmp_buf), p, rsa,
				  1);
	    if (s->options & 134217728UL)
	      {
		*(p + 1) = (unsigned char) ((int) *(p + 1) + 1);
	      }
	    if (s->options & 268435456UL)
	      {
		tmp_buf[0] = (unsigned char) 112;
	      }
	    if (n <= 0)
	      {
		ERR_put_error (20, 152, 119, "s3_clnt.c", 1813);
		goto err;
	      }
	    if (s->version > 768)
	      {
		*(q + 0) = (unsigned char) ((n >> 8) & 255);
		*(q + 1) = (unsigned char) (n & 255);
		q += 2;
		n += 2;
	      }
	    (s->session)->master_key_length =
	      (*(((s->method)->ssl3_enc)->generate_master_secret)) (s,
								    (s->
								     session)->
								    master_key,
								    tmp_buf,
								    (int)
								    sizeof
								    (tmp_buf));
	    OPENSSL_cleanse ((void *) (tmp_buf), sizeof (tmp_buf));
	  }
	else
	  {
	    if (l & 22UL)
	      {
		if ((unsigned int) ((s->session)->sess_cert)->peer_dh_tmp !=
		    (unsigned int) ((void *) 0))
		  {
		    dh_srvr = ((s->session)->sess_cert)->peer_dh_tmp;
		  }
		else
		  {
		    ssl3_send_alert (s, 2, 40);
		    ERR_put_error (20, 152, 238, "s3_clnt.c", 1976);
		    goto err;
		  }
		tmp___0 =
		  ASN1_dup ((i2d_of_void *) (&i2d_DHparams),
			    (d2i_of_void *) (&d2i_DHparams),
			    (char *) ((void *) ((DH const *) dh_srvr)));
		dh_clnt = (DH *) tmp___0;
		if ((unsigned int) dh_clnt == (unsigned int) ((void *) 0))
		  {
		    ERR_put_error (20, 152, 5, "s3_clnt.c", 1983);
		    goto err;
		  }
		tmp___1 = DH_generate_key (dh_clnt);
		if (!tmp___1)
		  {
		    ERR_put_error (20, 152, 5, "s3_clnt.c", 1988);
		    goto err;
		  }
		n =
		  DH_compute_key (p, (BIGNUM const *) dh_srvr->pub_key,
				  dh_clnt);
		if (n <= 0)
		  {
		    ERR_put_error (20, 152, 5, "s3_clnt.c", 1999);
		    goto err;
		  }
		(s->session)->master_key_length =
		  (*(((s->method)->ssl3_enc)->generate_master_secret)) (s,
									(s->
									 session)->
									master_key,
									p, n);
		memset ((void *) p, 0, (unsigned int) n);
		tmp___2 = BN_num_bits ((BIGNUM const *) dh_clnt->pub_key);
		n = (tmp___2 + 7) / 8;
		*(p + 0) = (unsigned char) ((n >> 8) & 255);
		*(p + 1) = (unsigned char) (n & 255);
		p += 2;
		BN_bn2bin ((BIGNUM const *) dh_clnt->pub_key, p);
		n += 2;
		DH_free (dh_clnt);
	      }
	    else
	      {
		if (l & 64UL)
		  {
		    goto _L;
		  }
		else
		  {
		    if (l & 128UL)
		      {
		      _L:srvr_group =
			  (EC_GROUP const *) ((void *)
					      0);
			ecdh_clnt_cert = 0;
			field_size = 0;
			if (l & 64UL)
			  {
			    if ((unsigned int) s->cert !=
				(unsigned int) ((void *) 0))
			      {
			      }
			  }
			if ((unsigned int) ((s->session)->sess_cert)->
			    peer_ecdh_tmp != (unsigned int) ((void *) 0))
			  {
			    tkey = ((s->session)->sess_cert)->peer_ecdh_tmp;
			  }
			else
			  {
			    srvr_pub_pkey =
			      X509_get_pubkey (((s->session)->sess_cert)->
					       peer_pkeys[5].x509);
			    if ((unsigned int) srvr_pub_pkey ==
				(unsigned int) ((void *) 0))
			      {
				ERR_put_error (20, 152, 68, "s3_clnt.c",
					       2072);
				goto err;
			      }
			    else
			      {
				if (srvr_pub_pkey->type != 408)
				  {
				    ERR_put_error (20, 152, 68, "s3_clnt.c",
						   2072);
				    goto err;
				  }
				else
				  {
				    if ((unsigned int) srvr_pub_pkey->pkey.
					ec == (unsigned int) ((void *) 0))
				      {
					ERR_put_error (20, 152, 68,
						       "s3_clnt.c", 2072);
					goto err;
				      }
				  }
			      }
			    tkey = srvr_pub_pkey->pkey.ec;
			  }
			srvr_group =
			  EC_KEY_get0_group ((EC_KEY const *) tkey);
			srvr_ecpoint =
			  EC_KEY_get0_public_key ((EC_KEY const *) tkey);
			if ((unsigned int) srvr_group ==
			    (unsigned int) ((void *) 0))
			  {
			    ERR_put_error (20, 152, 68, "s3_clnt.c", 2085);
			    goto err;
			  }
			else
			  {
			    if ((unsigned int) srvr_ecpoint ==
				(unsigned int) ((void *) 0))
			      {
				ERR_put_error (20, 152, 68, "s3_clnt.c",
					       2085);
				goto err;
			      }
			  }
			clnt_ecdh = EC_KEY_new ();
			if ((unsigned int) clnt_ecdh ==
			    (unsigned int) ((void *) 0))
			  {
			    ERR_put_error (20, 152, 65, "s3_clnt.c", 2091);
			    goto err;
			  }
			tmp___3 = EC_KEY_set_group (clnt_ecdh, srvr_group);
			if (!tmp___3)
			  {
			    ERR_put_error (20, 152, 16, "s3_clnt.c", 2097);
			    goto err;
			  }
			if (ecdh_clnt_cert)
			  {
			    tkey = (((s->cert)->key)->privatekey)->pkey.ec;
			    priv_key =
			      EC_KEY_get0_private_key ((EC_KEY const *) tkey);
			    if ((unsigned int) priv_key ==
				(unsigned int) ((void *) 0))
			      {
				ERR_put_error (20, 152, 65, "s3_clnt.c",
					       2111);
				goto err;
			      }
			    tmp___4 =
			      EC_KEY_set_private_key (clnt_ecdh, priv_key);
			    if (!tmp___4)
			      {
				ERR_put_error (20, 152, 16, "s3_clnt.c",
					       2116);
				goto err;
			      }
			  }
			else
			  {
			    tmp___5 = EC_KEY_generate_key (clnt_ecdh);
			    if (!tmp___5)
			      {
				ERR_put_error (20, 152, 43, "s3_clnt.c",
					       2125);
				goto err;
			      }
			  }
			field_size = EC_GROUP_get_degree (srvr_group);
			if (field_size <= 0)
			  {
			    ERR_put_error (20, 152, 43, "s3_clnt.c", 2138);
			    goto err;
			  }
			n =
			  ECDH_compute_key ((void *) p,
					    (unsigned int) ((field_size + 7) /
							    8), srvr_ecpoint,
					    clnt_ecdh,
					    (void
					     *(*)(void const *in,
						  size_t inlen, void *out,
						  size_t *
						  outlen)) ((void *) 0));
			if (n <= 0)
			  {
			    ERR_put_error (20, 152, 43, "s3_clnt.c", 2145);
			    goto err;
			  }
			(s->session)->master_key_length =
			  (*(((s->method)->ssl3_enc)->generate_master_secret))
			  (s, (s->session)->master_key, p, n);
			memset ((void *) p, 0, (unsigned int) n);
			if (ecdh_clnt_cert)
			  {
			    n = 0;
			  }
			else
			  {
			    tmp___6 =
			      EC_KEY_get0_public_key ((EC_KEY const *)
						      clnt_ecdh);
			    tmp___7 =
			      EC_POINT_point2oct (srvr_group, tmp___6,
						  (enum
						   __anonenum_point_conversion_form_t_29)
						  4,
						  (unsigned char *) ((void *)
								     0), 0U,
						  (BN_CTX *) ((void *) 0));
			    encoded_pt_len = (int) tmp___7;
			    tmp___8 =
			      CRYPTO_malloc ((int)
					     ((unsigned int) encoded_pt_len *
					      sizeof (unsigned char)),
					     "s3_clnt.c", 2175);
			    encodedPoint = (unsigned char *) tmp___8;
			    bn_ctx = BN_CTX_new ();
			    if ((unsigned int) encodedPoint ==
				(unsigned int) ((void *) 0))
			      {
				ERR_put_error (20, 152, 65, "s3_clnt.c",
					       2180);
				goto err;
			      }
			    else
			      {
				if ((unsigned int) bn_ctx ==
				    (unsigned int) ((void *) 0))
				  {
				    ERR_put_error (20, 152, 65, "s3_clnt.c",
						   2180);
				    goto err;
				  }
			      }
			    tmp___9 =
			      EC_KEY_get0_public_key ((EC_KEY const *)
						      clnt_ecdh);
			    tmp___10 =
			      EC_POINT_point2oct (srvr_group, tmp___9,
						  (enum
						   __anonenum_point_conversion_form_t_29)
						  4, encodedPoint,
						  (unsigned int)
						  encoded_pt_len, bn_ctx);
			    n = (int) tmp___10;
			    *p = (unsigned char) n;
			    p++;
			    memcpy ((void *__restrict) p,
				    (void const *__restrict) encodedPoint,
				    (unsigned int) n);
			    n++;
			  } BN_CTX_free (bn_ctx);
			if ((unsigned int) encodedPoint !=
			    (unsigned int) ((void *) 0))
			  {
			    CRYPTO_free ((void *) encodedPoint);
			  }
			if ((unsigned int) clnt_ecdh !=
			    (unsigned int) ((void *) 0))
			  {
			    EC_KEY_free (clnt_ecdh);
			  }
			EVP_PKEY_free (srvr_pub_pkey);
		      }
		    else
		      {
			ssl3_send_alert (s, 2, 40);
			ERR_put_error (20, 152, 68, "s3_clnt.c", 2212);
			goto err;
		      }
		  }
	      }
	  }
	tmp___11 = d;
	d++;
	*tmp___11 = (unsigned char) 16;
	*(d + 0) = (unsigned char) ((n >> 16) & 255);
	*(d + 1) = (unsigned char) ((n >> 8) & 255);
	*(d + 2) = (unsigned char) (n & 255);
	d += 3;
	s->state = 4481;
	s->init_num = n + 4;
	s->init_off = 0;
      }
    tmp___12 = ssl3_do_write (s, 22);
    return (tmp___12);
  err:BN_CTX_free (bn_ctx);
    if ((unsigned int) encodedPoint != (unsigned int) ((void *) 0))
      {
	CRYPTO_free ((void *) encodedPoint);
      }
    if ((unsigned int) clnt_ecdh != (unsigned int) ((void *) 0))
      {
	EC_KEY_free (clnt_ecdh);
      }
    EVP_PKEY_free (srvr_pub_pkey);
    return (-1);
  }
}

int
ssl3_send_client_verify (SSL * s)
{
  unsigned char *p;
  unsigned char *d;
  unsigned char data___0[36];
  EVP_PKEY *pkey;
  unsigned int u;
  unsigned long n;
  int j;
  int tmp;
  int tmp___0;
  int tmp___1;
  unsigned char *tmp___2;
  int tmp___3;
  {
    u = 0U;
    if (s->state == 4496)
      {
	d = (unsigned char *) (s->init_buf)->data;
	p = d + 4;
	pkey = ((s->cert)->key)->privatekey;
	(*(((s->method)->ssl3_enc)->cert_verify_mac)) (s,
						       &(s->s3)->finish_dgst2,
						       &data___0[16]);
	if (pkey->type == 6)
	  {
	    (*(((s->method)->ssl3_enc)->cert_verify_mac)) (s,
							   &(s->s3)->
							   finish_dgst1,
							   &data___0[0]);
	    tmp =
	      RSA_sign (114, (unsigned char const *) (data___0), 36U, p + 2,
			&u, pkey->pkey.rsa);
	    if (tmp <= 0)
	      {
		ERR_put_error (20, 153, 4, "s3_clnt.c", 2269);
		goto err;
	      }
	    *(p + 0) = (unsigned char) ((u >> 8) & 255U);
	    *(p + 1) = (unsigned char) (u & 255U);
	    p += 2;
	    n = (unsigned long) (u + 2U);
	  }
	else
	  {
	    if (pkey->type == 116)
	      {
		tmp___0 =
		  DSA_sign (pkey->save_type,
			    (unsigned char const *) (&data___0[16]), 20,
			    p + 2, (unsigned int *) (&j), pkey->pkey.dsa);
		if (!tmp___0)
		  {
		    ERR_put_error (20, 153, 10, "s3_clnt.c", 2285);
		    goto err;
		  }
		*(p + 0) = (unsigned char) ((j >> 8) & 255);
		*(p + 1) = (unsigned char) (j & 255);
		p += 2;
		n = (unsigned long) (j + 2);
	      }
	    else
	      {
		if (pkey->type == 408)
		  {
		    tmp___1 =
		      ECDSA_sign (pkey->save_type,
				  (unsigned char const *) (&data___0[16]), 20,
				  p + 2, (unsigned int *) (&j),
				  pkey->pkey.ec);
		    if (!tmp___1)
		      {
			ERR_put_error (20, 153, 42, "s3_clnt.c", 2302);
			goto err;
		      }
		    *(p + 0) = (unsigned char) ((j >> 8) & 255);
		    *(p + 1) = (unsigned char) (j & 255);
		    p += 2;
		    n = (unsigned long) (j + 2);
		  }
		else
		  {
		    ERR_put_error (20, 153, 68, "s3_clnt.c", 2311);
		    goto err;
		  }
	      }
	  }
	tmp___2 = d;
	d++;
	*tmp___2 = (unsigned char) 15;
	*(d + 0) = (unsigned char) ((n >> 16) & 255UL);
	*(d + 1) = (unsigned char) ((n >> 8) & 255UL);
	*(d + 2) = (unsigned char) (n & 255UL);
	d += 3;
	s->state = 4497;
	s->init_num = (int) n + 4;
	s->init_off = 0;
      }
    tmp___3 = ssl3_do_write (s, 22);
    return (tmp___3);
  err:return (-1);
  }
}

int
ssl3_send_client_certificate (SSL * s)
{
  X509 *x509;
  EVP_PKEY *pkey;
  int i;
  unsigned long l;
  int tmp;
  int tmp___0;
  X509 *tmp___1;
  int tmp___2;
  {
    x509 = (X509 *) ((void *) 0);
    pkey = (EVP_PKEY *) ((void *) 0);
    if (s->state == 4464)
      {
	if ((unsigned int) s->cert == (unsigned int) ((void *) 0))
	  {
	    s->state = 4465;
	  }
	else
	  {
	    if ((unsigned int) ((s->cert)->key)->x509 ==
		(unsigned int) ((void *) 0))
	      {
		s->state = 4465;
	      }
	    else
	      {
		if ((unsigned int) ((s->cert)->key)->privatekey ==
		    (unsigned int) ((void *) 0))
		  {
		    s->state = 4465;
		  }
		else
		  {
		    s->state = 4466;
		  }
	      }
	  }
      }
    if (s->state == 4465)
      {
	i = 0;
	if ((unsigned int) (s->ctx)->client_cert_cb !=
	    (unsigned int) ((void *) 0))
	  {
	    i = (*((s->ctx)->client_cert_cb)) (s, &x509, &pkey);
	  }
	if (i < 0)
	  {
	    s->rwstate = 4;
	    return (-1);
	  }
	s->rwstate = 1;
	if (i == 1)
	  {
	    if ((unsigned int) pkey != (unsigned int) ((void *) 0))
	      {
		if ((unsigned int) x509 != (unsigned int) ((void *) 0))
		  {
		    s->state = 4465;
		    tmp = SSL_use_certificate (s, x509);
		    if (tmp)
		      {
			tmp___0 = SSL_use_PrivateKey (s, pkey);
			if (!tmp___0)
			  {
			    i = 0;
			  }
		      }
		    else
		      {
			i = 0;
		      }
		  }
		else
		  {
		    goto _L___0;
		  }
	      }
	    else
	      {
		goto _L___0;
	      }
	  }
	else
	  {
	  _L___0:if (i == 1)
	      {
		i = 0;
		ERR_put_error (20, 151, 106, "s3_clnt.c", 2368);
	      }
	  }
	if ((unsigned int) x509 != (unsigned int) ((void *) 0))
	  {
	    X509_free (x509);
	  }
	if ((unsigned int) pkey != (unsigned int) ((void *) 0))
	  {
	    EVP_PKEY_free (pkey);
	  }
	if (i == 0)
	  {
	    if (s->version == 768)
	      {
		(s->s3)->tmp.cert_req = 0;
		ssl3_send_alert (s, 1, 41);
		return (1);
	      }
	    else
	      {
		(s->s3)->tmp.cert_req = 2;
	      }
	  }
	s->state = 4466;
      }
    if (s->state == 4466)
      {
	s->state = 4467;
	if ((s->s3)->tmp.cert_req == 2)
	  {
	    tmp___1 = (X509 *) ((void *) 0);
	  }
	else
	  {
	    tmp___1 = ((s->cert)->key)->x509;
	  }
	l = ssl3_output_cert_chain (s, tmp___1);
	s->init_num = (int) l;
	s->init_off = 0;
      }
    tmp___2 = ssl3_do_write (s, 22);
    return (tmp___2);
  }
}

int
ssl3_check_cert_and_algorithm (SSL * s)
{
  int i;
  int idx;
  long algs;
  EVP_PKEY *pkey;
  SESS_CERT *sc;
  RSA *rsa;
  DH *dh;
  int tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  {
    pkey = (EVP_PKEY *) ((void *) 0);
    sc = (s->session)->sess_cert;
    algs = (long) ((s->s3)->tmp.new_cipher)->algorithms;
    EVP_PKEY_free (pkey);
    if (algs & 256L)
      {
	if (!((i & 17) == 17))
	  {
	    ERR_put_error (20, 130, 170, "s3_clnt.c", 2465);
	    goto f_err;
	  }
	else
	  {
	    goto _L;
	  }
      }
    else
      {
      _L:if (algs & 512L)
	  {
	    if (!((i & 18) == 18))
	      {
		ERR_put_error (20, 130, 165, "s3_clnt.c", 2471);
		goto f_err;
	      }
	  }
      }
    if (algs & 1L)
      {
	if (!((i & 33) == 33))
	  {
	    if (!((unsigned int) rsa != (unsigned int) ((void *) 0)))
	      {
		ERR_put_error (20, 130, 169, "s3_clnt.c", 2479);
		goto f_err;
	      }
	  }
      }
    if (algs & 16L)
      {
	if ((i & 68) == 68)
	  {
	    goto _L___1;
	  }
	else
	  {
	    if ((unsigned int) dh != (unsigned int) ((void *) 0))
	      {
		goto _L___1;
	      }
	    else
	      {
		ERR_put_error (20, 130, 163, "s3_clnt.c", 2487);
		goto f_err;
	      }
	  }
      }
    else
      {
      _L___1:if (algs & 2L)
	  {
	    if (!((i & 260) == 260))
	      {
		ERR_put_error (20, 130, 164, "s3_clnt.c", 2492);
		goto f_err;
	      }
	    else
	      {
		goto _L___0;
	      }
	  }
	else
	  {
	  _L___0:if (algs & 4L)
	      {
		if (!((i & 516) == 516))
		  {
		    ERR_put_error (20, 130, 162, "s3_clnt.c", 2498);
		    goto f_err;
		  }
	      }
	  }
      }
    if (((s->s3)->tmp.new_cipher)->algo_strength & 2UL)
      {
	if (!((i & 4096) == 4096))
	  {
	    if (algs & 1L)
	      {
		if ((unsigned int) rsa == (unsigned int) ((void *) 0))
		  {
		    ERR_put_error (20, 130, 167, "s3_clnt.c", 2512);
		    goto f_err;
		  }
		else
		  {
		    tmp___0 = RSA_size ((RSA const *) rsa);
		    if (((s->s3)->tmp.new_cipher)->algo_strength & 8UL)
		      {
			tmp___1 = 512;
		      }
		    else
		      {
			tmp___1 = 1024;
		      }
		    if (tmp___0 * 8 > tmp___1)
		      {
			ERR_put_error (20, 130, 167, "s3_clnt.c", 2512);
			goto f_err;
		      }
		  }
	      }
	    else
	      {
		if (algs & 22L)
		  {
		    if ((unsigned int) dh == (unsigned int) ((void *) 0))
		      {
			ERR_put_error (20, 130, 166, "s3_clnt.c", 2524);
			goto f_err;
		      }
		    else
		      {
			tmp___2 = DH_size ((DH const *) dh);
			if (((s->s3)->tmp.new_cipher)->algo_strength & 8UL)
			  {
			    tmp___3 = 512;
			  }
			else
			  {
			    tmp___3 = 1024;
			  }
			if (tmp___2 * 8 > tmp___3)
			  {
			    ERR_put_error (20, 130, 166, "s3_clnt.c", 2524);
			    goto f_err;
			  }
		      }
		  }
		else
		  {
		    ERR_put_error (20, 130, 250, "s3_clnt.c", 2531);
		    goto f_err;
		  }
	      }
	  }
      }
    return (1);
  f_err:ssl3_send_alert (s, 2, 40);
  err:return (0);
  }
}

EC_KEY *EC_KEY_dup (EC_KEY const *ec_key);
SSL_CIPHER ssl3_ciphers[66];
void ssl_set_cert_masks (CERT * c, SSL_CIPHER * cipher);
int ssl3_setup_key_block (SSL * s);
int ssl3_change_cipher_state (SSL * s, int which);
int ssl3_generate_master_secret (SSL * s, unsigned char *out,
				 unsigned char *p, int len);
int ssl3_final_finish_mac (SSL * s, EVP_MD_CTX * ctx1, EVP_MD_CTX * ctx2,
			   char const *sender, int len, unsigned char *p);
int ssl3_cert_verify_mac (SSL * s, EVP_MD_CTX * ctx, unsigned char *p);
int ssl3_enc (SSL * s, int send___0);
int ssl3_mac (SSL * ssl, unsigned char *md___0, int send___0);
int ssl3_alert_code (int code);
char const ssl3_version_str[41] = {
  (char const) 'S', (char const) 'S', (char const) 'L', (char const) 'v',
    (char const) '3', (char const) ' ', (char const) 'p', (char const) 'a',
    (char const) 'r', (char const) 't', (char const) ' ', (char const) 'o',
    (char const) 'f', (char const) ' ', (char const) 'O', (char const) 'p',
    (char const) 'e', (char const) 'n', (char const) 'S', (char const) 'S',
    (char const) 'L', (char const) ' ', (char const) '0', (char const) '.',
    (char const) '9', (char const) '.', (char const) '8', (char const) 'g',
    (char const) ' ', (char const) '1', (char const) '9', (char const) ' ',
    (char const) 'O', (char const) 'c', (char const) 't', (char const) ' ',
    (char const) '2', (char const) '0', (char const) '0', (char const) '7',
    (char const) '\000'
};

SSL_CIPHER ssl3_ciphers[66] = {
  {
   1, "NULL-MD5", 50331649UL, 39846145UL, 5UL, 0UL, 0, 0, 486539263UL, 255UL}
  , {
     1, "NULL-SHA", 50331650UL, 44040449UL, 5UL, 0UL, 0, 0, 486539263UL, 255UL}
  , {
     1, "EXP-RC4-MD5", 50331651UL, 37880065UL, 10UL, 0UL, 40, 128,
     486539263UL, 255UL}
  , {
     1, "RC4-MD5", 50331652UL, 37880065UL, 65UL, 0UL, 128, 128, 486539263UL,
     255UL}
  , {
     1, "AECDH-DES-CBC3-SHA", 50380823UL, 42010752UL, 129UL, 0UL, 168, 168,
     486539263UL, 255UL}
  , {
     1, "AECDH-AES128-SHA", 50380824UL, 109054080UL, 129UL, 0UL, 128, 128,
     486539263UL, 255UL}
  , {
     1, "AECDH-AES256-SHA", 50380825UL, 109054080UL, 129UL, 0UL, 256, 256,
     486539263UL, 255UL}
};

SSL3_ENC_METHOD SSLv3_enc_data = {
  &ssl3_enc, &ssl3_mac, &ssl3_setup_key_block, &ssl3_generate_master_secret,
    &ssl3_change_cipher_state, &ssl3_final_finish_mac, 36,
    &ssl3_cert_verify_mac, "CLNT", 4, "SRVR", 4, &ssl3_alert_code
};

int
ssl3_new (SSL * s)
{
  SSL3_STATE *s3;
  void *tmp;
  {
    tmp = CRYPTO_malloc ((int) sizeof (*s3), "s3_lib.c", 1661);
    s3 = (SSL3_STATE *) tmp;
    if ((unsigned int) s3 == (unsigned int) ((void *) 0))
      {
	goto err;
      }
    memset ((void *) s3, 0, sizeof (*s3));
    EVP_MD_CTX_init (&s3->finish_dgst1);
    EVP_MD_CTX_init (&s3->finish_dgst2);
    s->s3 = s3;
    (*((s->method)->ssl_clear)) (s);
    return (1);
  err:return (0);
  }
}

void
ssl3_free (SSL * s)
{
  {
    if ((unsigned int) s == (unsigned int) ((void *) 0))
      {
	return;
      }
    ssl3_cleanup_key_block (s);
    if ((unsigned int) (s->s3)->rbuf.buf != (unsigned int) ((void *) 0))
      {
	CRYPTO_free ((void *) (s->s3)->rbuf.buf);
      }
    if ((unsigned int) (s->s3)->wbuf.buf != (unsigned int) ((void *) 0))
      {
	CRYPTO_free ((void *) (s->s3)->wbuf.buf);
      }
    if ((unsigned int) (s->s3)->rrec.comp != (unsigned int) ((void *) 0))
      {
	CRYPTO_free ((void *) (s->s3)->rrec.comp);
      }
    if ((unsigned int) (s->s3)->tmp.dh != (unsigned int) ((void *) 0))
      {
	DH_free ((s->s3)->tmp.dh);
      }
    if ((unsigned int) (s->s3)->tmp.ecdh != (unsigned int) ((void *) 0))
      {
	EC_KEY_free ((s->s3)->tmp.ecdh);
      }
    if ((unsigned int) (s->s3)->tmp.ca_names != (unsigned int) ((void *) 0))
      {
	sk_pop_free ((s->s3)->tmp.ca_names,
		     (void (*)(void *)) (&X509_NAME_free));
      }
    EVP_MD_CTX_cleanup (&(s->s3)->finish_dgst1);
    EVP_MD_CTX_cleanup (&(s->s3)->finish_dgst2);
    OPENSSL_cleanse ((void *) s->s3, sizeof (*(s->s3)));
    CRYPTO_free ((void *) s->s3);
    s->s3 = (struct ssl3_state_st *) ((void *) 0);
    return;
  }
}

void
ssl3_clear (SSL * s)
{
  unsigned char *rp;
  unsigned char *wp;
  size_t rlen;
  size_t wlen;
  {
    ssl3_cleanup_key_block (s);
    if ((unsigned int) (s->s3)->tmp.ca_names != (unsigned int) ((void *) 0))
      {
	sk_pop_free ((s->s3)->tmp.ca_names,
		     (void (*)(void *)) (&X509_NAME_free));
      }
    if ((unsigned int) (s->s3)->rrec.comp != (unsigned int) ((void *) 0))
      {
	CRYPTO_free ((void *) (s->s3)->rrec.comp);
	(s->s3)->rrec.comp = (unsigned char *) ((void *) 0);
      }
    (s->s3)->num_renegotiations = 0;
    (s->s3)->in_read_app_data = 0;
    s->version = 768;
    return;
  }
}

long
ssl3_ctrl (SSL * s, int cmd, long larg, void *parg)
{
  int ret___3;
  int tmp;
  int tmp___0;
  RSA *rsa;
  DH *dh;
  void *tmp___1;
  int tmp___2;
  EC_KEY *ecdh;
  int tmp___3;
  int tmp___4;
  {
    ret___3 = 0;
    if (cmd == 2)
      {
	goto _L;
      }
    else
      {
	if (cmd == 5)
	  {
	    goto _L;
	  }
	else
	  {
	    if (cmd == 3)
	      {
		goto _L;
	      }
	    else
	      {
		if (cmd == 6)
		  {
		    goto _L;
		  }
		else
		  {
		    if (0)
		      {
		      _L:tmp =
			  ssl_cert_inst (&s->cert);
			if (!tmp)
			  {
			    ERR_put_error (20, 213, 65, "s3_lib.c", 1774);
			    return (0L);
			  }
		      }
		  }
	      }
	  }
      }
    switch (cmd)
      {
      case 8:
	ret___3 = s->hit;
	break;
      case 9:
	break;
      case 10:
	ret___3 = (s->s3)->num_renegotiations;
	break;
      case 11:
	ret___3 = (s->s3)->num_renegotiations;
	(s->s3)->num_renegotiations = 0;
	break;
      case 12:
	ret___3 = (s->s3)->total_renegotiations;
	break;
      case 13:
	ret___3 = (int) (s->s3)->flags;
	break;
      case 1:
	if ((unsigned int) s->cert != (unsigned int) ((void *) 0))
	  {
	    if ((unsigned int) (s->cert)->rsa_tmp ==
		(unsigned int) ((void *) 0))
	      {
		if ((unsigned int) (s->cert)->pkeys[0].privatekey ==
		    (unsigned int) ((void *) 0))
		  {
		    ret___3 = 1;
		  }
		else
		  {
		    tmp___0 = EVP_PKEY_size ((s->cert)->pkeys[0].privatekey);
		    if (tmp___0 > 64)
		      {
			ret___3 = 1;
		      }
		  }
	      }
	  }
	break;
      case 2:
	rsa = (RSA *) parg;
	if ((unsigned int) rsa == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 213, 67, "s3_lib.c", 1812);
	    return ((long) ret___3);
	  }
	rsa = RSAPrivateKey_dup (rsa);
	if ((unsigned int) rsa == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 213, 4, "s3_lib.c", 1817);
	    return ((long) ret___3);
	  }
	if ((unsigned int) (s->cert)->rsa_tmp != (unsigned int) ((void *) 0))
	  {
	    RSA_free ((s->cert)->rsa_tmp);
	  }
	(s->cert)->rsa_tmp = rsa;
	ret___3 = 1;
	break;
      case 5:
	ERR_put_error (20, 213, 66, "s3_lib.c", 1828);
	return ((long) ret___3);
	break;
      case 3:
	dh = (DH *) parg;
	if ((unsigned int) dh == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 213, 67, "s3_lib.c", 1839);
	    return ((long) ret___3);
	  }
	tmp___1 =
	  ASN1_dup ((i2d_of_void *) (&i2d_DHparams),
		    (d2i_of_void *) (&d2i_DHparams),
		    (char *) ((void *) ((DH const *) dh)));
	dh = (DH *) tmp___1;
	if ((unsigned int) dh == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 213, 5, "s3_lib.c", 1844);
	    return ((long) ret___3);
	  }
	if (!(s->options & 1048576UL))
	  {
	    tmp___2 = DH_generate_key (dh);
	    if (!tmp___2)
	      {
		DH_free (dh);
		ERR_put_error (20, 213, 5, "s3_lib.c", 1852);
		return ((long) ret___3);
	      }
	  }
	if ((unsigned int) (s->cert)->dh_tmp != (unsigned int) ((void *) 0))
	  {
	    DH_free ((s->cert)->dh_tmp);
	  }
	(s->cert)->dh_tmp = dh;
	ret___3 = 1;
	break;
      case 6:
	ERR_put_error (20, 213, 66, "s3_lib.c", 1864);
	return ((long) ret___3);
	break;
      case 4:
	ecdh = (EC_KEY *) ((void *) 0);
	if ((unsigned int) parg == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 213, 67, "s3_lib.c", 1876);
	    return ((long) ret___3);
	  }
	tmp___3 = EC_KEY_up_ref ((EC_KEY *) parg);
	if (!tmp___3)
	  {
	    ERR_put_error (20, 213, 43, "s3_lib.c", 1881);
	    return ((long) ret___3);
	  }
	ecdh = (EC_KEY *) parg;
	if (!(s->options & 524288UL))
	  {
	    tmp___4 = EC_KEY_generate_key (ecdh);
	    if (!tmp___4)
	      {
		EC_KEY_free (ecdh);
		ERR_put_error (20, 213, 43, "s3_lib.c", 1890);
		return ((long) ret___3);
	      }
	  }
	if ((unsigned int) (s->cert)->ecdh_tmp != (unsigned int) ((void *) 0))
	  {
	    EC_KEY_free ((s->cert)->ecdh_tmp);
	  }
	(s->cert)->ecdh_tmp = ecdh;
	ret___3 = 1;
	break;
      case 7:
	ERR_put_error (20, 213, 66, "s3_lib.c", 1902);
	return ((long) ret___3);
	break;
      default:;
	break;
      }
    return ((long) ret___3);
  }
}

long
ssl3_callback_ctrl (SSL * s, int cmd, void (*fp) (void))
{
}

long
ssl3_ctx_ctrl (SSL_CTX * ctx, int cmd, long larg, void *parg)
{
  CERT *cert;
  int tmp;
  RSA *rsa;
  int i;
  DH *new;
  DH *dh;
  void *tmp___0;
  int tmp___1;
  EC_KEY *ecdh;
  int tmp___2;
  STACK *tmp___3;
  {
    cert = ctx->cert;
    switch (cmd)
      {
      case 1:
	if ((unsigned int) cert->rsa_tmp == (unsigned int) ((void *) 0))
	  {
	    if ((unsigned int) cert->pkeys[0].privatekey ==
		(unsigned int) ((void *) 0))
	      {
		return (1L);
	      }
	    else
	      {
		tmp = EVP_PKEY_size (cert->pkeys[0].privatekey);
		if (tmp > 64)
		  {
		    return (1L);
		  }
		else
		  {
		    return (0L);
		  }
	      }
	  }
	else
	  {
	    return (0L);
	  }
      case 2:
	rsa = (RSA *) parg;
	i = 1;
	if ((unsigned int) rsa == (unsigned int) ((void *) 0))
	  {
	    i = 0;
	  }
	else
	  {
	    rsa = RSAPrivateKey_dup (rsa);
	    if ((unsigned int) rsa == (unsigned int) ((void *) 0))
	      {
		i = 0;
	      }
	} if (!i)
	  {
	    ERR_put_error (20, 133, 4, "s3_lib.c", 2037);
	    return (0L);
	  }
	else
	  {
	    if ((unsigned int) cert->rsa_tmp != (unsigned int) ((void *) 0))
	      {
		RSA_free (cert->rsa_tmp);
	      }
	    cert->rsa_tmp = rsa;
	    return (1L);
	  }
      case 5:
	ERR_put_error (20, 133, 66, "s3_lib.c", 2051);
	return (0L);
	break;
      case 3:
	new = (DH *) ((void *) 0);
	dh = (DH *) parg;
	tmp___0 =
	  ASN1_dup ((i2d_of_void *) (&i2d_DHparams),
		    (d2i_of_void *) (&d2i_DHparams),
		    (char *) ((void *) ((DH const *) dh)));
	new = (DH *) tmp___0;
	if ((unsigned int) new == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 133, 5, "s3_lib.c", 2064);
	    return (0L);
	  }
	if (!(ctx->options & 1048576UL))
	  {
	    tmp___1 = DH_generate_key (new);
	    if (!tmp___1)
	      {
		ERR_put_error (20, 133, 5, "s3_lib.c", 2071);
		DH_free (new);
		return (0L);
	      }
	  }
	if ((unsigned int) cert->dh_tmp != (unsigned int) ((void *) 0))
	  {
	    DH_free (cert->dh_tmp);
	  }
	cert->dh_tmp = new;
	return (1L);
      case 6:
	ERR_put_error (20, 133, 66, "s3_lib.c", 2084);
	return (0L);
	break;
      case 4:
	ecdh = (EC_KEY *) ((void *) 0);
	if ((unsigned int) parg == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 133, 43, "s3_lib.c", 2096);
	    return (0L);
	  }
	ecdh = EC_KEY_dup ((EC_KEY const *) ((EC_KEY *) parg));
	if ((unsigned int) ecdh == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 133, 16, "s3_lib.c", 2102);
	    return (0L);
	  }
	if (!(ctx->options & 524288UL))
	  {
	    tmp___2 = EC_KEY_generate_key (ecdh);
	    if (!tmp___2)
	      {
		EC_KEY_free (ecdh);
		ERR_put_error (20, 133, 43, "s3_lib.c", 2110);
		return (0L);
	      }
	  }
	if ((unsigned int) cert->ecdh_tmp != (unsigned int) ((void *) 0))
	  {
	    EC_KEY_free (cert->ecdh_tmp);
	  }
	cert->ecdh_tmp = ecdh;
	return (1L);
      case 7:
	ERR_put_error (20, 133, 66, "s3_lib.c", 2125);
	return (0L);
	break;
      case 14:
	if ((unsigned int) ctx->extra_certs == (unsigned int) ((void *) 0))
	  {
	    tmp___3 = sk_new_null ();
	    ctx->extra_certs = tmp___3;
	    if ((unsigned int) tmp___3 == (unsigned int) ((void *) 0))
	      {
		return (0L);
	      }
	  }
	sk_push (ctx->extra_certs, (void *) ((char *) ((X509 *) parg)));
	break;
      default:;
	return (0L);
      }
    return (1L);
  }
}

SSL_CIPHER *
ssl3_get_cipher_by_char (unsigned char const *p)
{
  SSL_CIPHER c;
  SSL_CIPHER *cp;
  unsigned long id;
  char const *tmp;
  {
    id =
      (50331648UL | ((unsigned long) *(p + 0) << 8L)) | (unsigned long) *(p +
									  1);
    c.id = id;
    tmp =
      OBJ_bsearch ((char const *) ((char *) (&c)),
		   (char const *) ((char *) (ssl3_ciphers)),
		   (int) (sizeof (ssl3_ciphers) / sizeof (SSL_CIPHER)),
		   (int) sizeof (SSL_CIPHER),
		   (int (*)(void const *, void const *))
		   (&ssl_cipher_id_cmp));
    cp = (SSL_CIPHER *) tmp;
    if ((unsigned int) cp == (unsigned int) ((void *) 0))
      {
	return ((SSL_CIPHER *) ((void *) 0));
      }
    else
      {
	if (cp->valid == 0)
	  {
	    return ((SSL_CIPHER *) ((void *) 0));
	  }
	else
	  {
	    return (cp);
	  }
      }
  }
}

SSL_CIPHER *
ssl3_choose_cipher (SSL * s, STACK * clnt, STACK * srvr)
{
  SSL_CIPHER *c;
  SSL_CIPHER *ret___3;
  STACK *prio;
  STACK *allow;
  int i;
  int j;
  int ok;
  CERT *cert;
  unsigned long alg;
  unsigned long mask;
  unsigned long emask;
  void *tmp;
  void *tmp___0;
  int tmp___1;
  {
    ret___3 = (SSL_CIPHER *) ((void *) 0);
    cert = s->cert;
    if (s->options & 4194304UL)
      {
	prio = srvr;
	allow = clnt;
      }
    else
      {
	prio = clnt;
	allow = srvr;
      }
    i = 0;
    while (1)
      {
	tmp___1 = sk_num ((STACK const *) prio);
	if (!(i < tmp___1))
	  {
	    break;
	  }
	tmp = sk_value ((STACK const *) prio, i);
	c = (SSL_CIPHER *) tmp;
	ssl_set_cert_masks (cert, c);
	mask = cert->mask;
	emask = cert->export_mask;
	alg = c->algorithms & 32767UL;
	if (c->algo_strength & 2UL)
	  {
	    if ((alg & emask) == alg)
	      {
		ok = 1;
	      }
	    else
	      {
		ok = 0;
	      }
	  }
	else
	  {
	    if ((alg & mask) == alg)
	      {
		ok = 1;
	      }
	    else
	      {
		ok = 0;
	      }
	  }
	if (!ok)
	  {
	    goto __Cont;
	  }
	j = sk_find (allow, (void *) ((char *) c));
	if (j >= 0)
	  {
	    tmp___0 = sk_value ((STACK const *) allow, j);
	    ret___3 = (SSL_CIPHER *) tmp___0;
	    break;
	  }
      __Cont:i++;
      }
    return (ret___3);
  }
}

int
ssl3_get_req_cert_type (SSL * s, unsigned char *p)
{
}

int
ssl3_shutdown (SSL * s)
{
  {
    if (s->quiet_shutdown)
      {
	s->shutdown = 3;
	return (1);
      }
    else
      {
	if (s->state == 16384)
	  {
	    s->shutdown = 3;
	    return (1);
	  }
      }
    if (!(s->shutdown & 1))
      {
	s->shutdown |= 1;
	ssl3_send_alert (s, 1, 0);
      }
    else
      {
	if ((s->s3)->alert_dispatch)
	  {
	    (*((s->method)->ssl_dispatch_alert)) (s);
	  }
	else
	  {
	    if (!(s->shutdown & 2))
	      {
		(*((s->method)->ssl_read_bytes)) (s, 0,
						  (unsigned char *) ((void *)
								     0), 0,
						  0);
	      }
      }}
    if (s->shutdown == 3)
      {
	if (!(s->s3)->alert_dispatch)
	  {
	    return (1);
	  }
	else
	  {
	    return (0);
	  }
      }
    else
      {
	return (0);
      }
  }
}

int
ssl3_write (SSL * s, void const *buf___5, int len)
{
  int ret___3;
  int n;
  int *tmp;
  long tmp___0;
  {
    tmp = __errno_location ();
    *tmp = 0;
    if ((s->s3)->renegotiate)
      {
	ssl3_renegotiate_check (s);
      }
    if ((s->s3)->flags & 4L)
      {
	if ((unsigned int) s->wbio == (unsigned int) s->bbio)
	  {
	    if ((s->s3)->delay_buf_pop_ret == 0)
	      {
		ret___3 = ssl3_write_bytes (s, 23, buf___5, len);
		if (ret___3 <= 0)
		  {
		    return (ret___3);
		  }
		(s->s3)->delay_buf_pop_ret = ret___3;
	      }
	    s->rwstate = 2;
	    tmp___0 = BIO_ctrl (s->wbio, 11, 0L, (void *) 0);
	    n = (int) tmp___0;
	    if (n <= 0)
	      {
		return (n);
	      }
	    s->rwstate = 1;
	    ssl_free_wbio_buffer (s);
	    (s->s3)->flags &= -5L;
	    ret___3 = (s->s3)->delay_buf_pop_ret;
	    (s->s3)->delay_buf_pop_ret = 0;
	  }
	else
	  {
	    goto _L;
	  }
      }
    else
      {
      _L:ret___3 =
	  (*((s->method)->ssl_write_bytes)) (s, 23, buf___5, len);
	if (ret___3 <= 0)
	  {
	    return (ret___3);
	  }
      }
    return (ret___3);
  }
}

static int
ssl3_read_internal (SSL * s, void *buf___5, int len, int peek)
{
  int ret___3;
  int *tmp;
  {
    tmp = __errno_location ();
    *tmp = 0;
    if ((s->s3)->renegotiate)
      {
	ssl3_renegotiate_check (s);
      }
    (s->s3)->in_read_app_data = 1;
    ret___3 =
      (*((s->method)->ssl_read_bytes)) (s, 23, (unsigned char *) buf___5, len,
					peek);
    if (ret___3 == -1)
      {
	if ((s->s3)->in_read_app_data == 2)
	  {
	    (s->in_handshake)++;
	    ret___3 =
	      (*((s->method)->ssl_read_bytes)) (s, 23,
						(unsigned char *) buf___5,
						len, peek);
	    (s->in_handshake)--;
	  }
	else
	  {
	    (s->s3)->in_read_app_data = 0;
	  }
      }
    else
      {
	(s->s3)->in_read_app_data = 0;
      }
    return (ret___3);
  }
}

int
ssl3_read (SSL * s, void *buf___5, int len)
{
  int tmp;
  {
    tmp = ssl3_read_internal (s, buf___5, len, 0);
    return (tmp);
  }
}

int
ssl3_peek (SSL * s, void *buf___5, int len)
{
  int tmp;
  {
    tmp = ssl3_read_internal (s, buf___5, len, 1);
    return (tmp);
  }
}

void ssl3_record_sequence_update (unsigned char *seq);
static unsigned char ssl3_pad_1[48] = {
  (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54,
    (unsigned char) 54, (unsigned char) 54, (unsigned char) 54
};

static unsigned char ssl3_pad_2[48] = {
  (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92,
    (unsigned char) 92, (unsigned char) 92, (unsigned char) 92
};

static int ssl3_handshake_mac (SSL * s, EVP_MD_CTX * in_ctx,
			       char const *sender, int len, unsigned char *p);
static int
ssl3_generate_key_block (SSL * s, unsigned char *km, int num)
{
}

void
ssl3_cleanup_key_block (SSL * s)
{
  {
    if ((unsigned int) (s->s3)->tmp.key_block != (unsigned int) ((void *) 0))
      {
	OPENSSL_cleanse ((void *) (s->s3)->tmp.key_block,
			 (unsigned int) (s->s3)->tmp.key_block_length);
	CRYPTO_free ((void *) (s->s3)->tmp.key_block);
	(s->s3)->tmp.key_block = (unsigned char *) ((void *) 0);
      }
    (s->s3)->tmp.key_block_length = 0;
    return;
  }
}

int
ssl3_enc (SSL * s, int send___0)
{
  SSL3_RECORD *rec;
  EVP_CIPHER_CTX *ds;
  unsigned long l;
  int bs;
  int i;
  EVP_CIPHER const *enc;
  {
    if (send___0)
      {
	ds = s->enc_write_ctx;
	rec = &(s->s3)->wrec;
	if ((unsigned int) s->enc_write_ctx == (unsigned int) ((void *) 0))
	  {
	    enc = (EVP_CIPHER const *) ((void *) 0);
	  }
	else
	  {
	    enc =
	      EVP_CIPHER_CTX_cipher ((EVP_CIPHER_CTX const *) s->
				     enc_write_ctx);
      }}
    else
      {
	ds = s->enc_read_ctx;
	rec = &(s->s3)->rrec;
	if ((unsigned int) s->enc_read_ctx == (unsigned int) ((void *) 0))
	  {
	    enc = (EVP_CIPHER const *) ((void *) 0);
	  }
	else
	  {
	    enc =
	      EVP_CIPHER_CTX_cipher ((EVP_CIPHER_CTX const *) s->
				     enc_read_ctx);
      }}
    if ((unsigned int) s->session == (unsigned int) ((void *) 0))
      {
	memmove ((void *) rec->data, (void const *) rec->input, rec->length);
	rec->input = rec->data;
      }
    else
      {
	if ((unsigned int) ds == (unsigned int) ((void *) 0))
	  {
	    memmove ((void *) rec->data, (void const *) rec->input,
		     rec->length);
	    rec->input = rec->data;
	  }
	else
	  {
	    if ((unsigned int) enc == (unsigned int) ((void *) 0))
	      {
		memmove ((void *) rec->data, (void const *) rec->input,
			 rec->length);
		rec->input = rec->data;
	      }
	    else
	      {
		l = (unsigned long) rec->length;
		bs = EVP_CIPHER_block_size (ds->cipher);
		if (bs != 1)
		  {
		    if (send___0)
		      {
			i = bs - (int) l % bs;
			l += (unsigned long) i;
			rec->length += (unsigned int) i;
			*(rec->input + (l - 1UL)) = (unsigned char) (i - 1);
		      }
		  }
		if (!send___0)
		  {
		    if (l == 0UL)
		      {
			ERR_put_error (20, 134, 129, "s3_enc.c", 490);
			ssl3_send_alert (s, 2, 21);
			return (0);
		      }
		    else
		      {
			if (l % (unsigned long) bs != 0UL)
			  {
			    ERR_put_error (20, 134, 129, "s3_enc.c", 490);
			    ssl3_send_alert (s, 2, 21);
			    return (0);
			  }
		      }
		  }
		EVP_Cipher (ds, rec->data, (unsigned char const *) rec->input,
			    (unsigned int) l);
		if (bs != 1)
		  {
		    if (!send___0)
		      {
			i = (int) *(rec->data + (l - 1UL)) + 1;
			if (i > bs)
			  {
			    return (-1);
			  }
			rec->length -= (unsigned int) i;
		      }
		  }
      }}}
    return (1);
  }
}

void
ssl3_init_finished_mac (SSL * s)
{
  {
    EVP_DigestInit_ex (&(s->s3)->finish_dgst1, (s->ctx)->md5,
		       (ENGINE *) ((void *) 0));
    EVP_DigestInit_ex (&(s->s3)->finish_dgst2, (s->ctx)->sha1,
		       (ENGINE *) ((void *) 0));
    return;
  }
}

void
ssl3_finish_mac (SSL * s, unsigned char const *buf___5, int len)
{
  {
    EVP_DigestUpdate (&(s->s3)->finish_dgst1, (void const *) buf___5,
		      (unsigned int) len);
    EVP_DigestUpdate (&(s->s3)->finish_dgst2, (void const *) buf___5,
		      (unsigned int) len);
    return;
  }
}

int
ssl3_cert_verify_mac (SSL * s, EVP_MD_CTX * ctx, unsigned char *p)
{
  int tmp;
  {
    tmp = ssl3_handshake_mac (s, ctx, (char const *) ((void *) 0), 0, p);
    return (tmp);
  }
}

int
ssl3_final_finish_mac (SSL * s, EVP_MD_CTX * ctx1, EVP_MD_CTX * ctx2,
		       char const *sender, int len, unsigned char *p)
{
  int ret___3;
  int tmp;
  {
    ret___3 = ssl3_handshake_mac (s, ctx1, sender, len, p);
    p += ret___3;
    tmp = ssl3_handshake_mac (s, ctx2, sender, len, p);
    ret___3 += tmp;
    return (ret___3);
  }
}

static int
ssl3_handshake_mac (SSL * s, EVP_MD_CTX * in_ctx, char const *sender, int len,
		    unsigned char *p)
{
  unsigned int ret___3;
  int npad;
  int n;
  unsigned int i;
  unsigned char md_buf[64];
  EVP_MD_CTX ctx;
  EVP_MD const *tmp;
  EVP_MD const *tmp___0;
  {
    EVP_MD_CTX_init (&ctx);
    EVP_MD_CTX_copy_ex (&ctx, (EVP_MD_CTX const *) in_ctx);
    tmp = EVP_MD_CTX_md ((EVP_MD_CTX const *) (&ctx));
    EVP_DigestUpdate (&ctx, (void const *) ((s->session)->master_key),
		      (unsigned int) (s->session)->master_key_length);
    EVP_DigestUpdate (&ctx, (void const *) (ssl3_pad_2), (unsigned int) npad);
    EVP_DigestUpdate (&ctx, (void const *) (md_buf), i);
    EVP_DigestFinal_ex (&ctx, p, &ret___3);
    EVP_MD_CTX_cleanup (&ctx);
    return ((int) ret___3);
  }
}

int
ssl3_mac (SSL * ssl, unsigned char *md___0, int send___0)
{
  SSL3_RECORD *rec;
  unsigned char *mac_sec;
  unsigned char *seq;
  EVP_MD_CTX md_ctx;
  EVP_MD const *hash___0;
  unsigned char *p;
  unsigned char rec_char;
  unsigned int md_size;
  int npad;
  int tmp;
  {
    if (send___0)
      {
	rec = &(ssl->s3)->wrec;
	mac_sec = &(ssl->s3)->write_mac_secret[0];
	seq = &(ssl->s3)->write_sequence[0];
	hash___0 = ssl->write_hash;
      }
    else
      {
	rec = &(ssl->s3)->rrec;
	mac_sec = &(ssl->s3)->read_mac_secret[0];
	seq = &(ssl->s3)->read_sequence[0];
	hash___0 = ssl->read_hash;
      }
    tmp = EVP_MD_size (hash___0);
    md_size = (unsigned int) tmp;
    npad = (int) ((48U / md_size) * md_size);
    EVP_MD_CTX_init (&md_ctx);
    EVP_DigestFinal_ex (&md_ctx, md___0, &md_size);
    EVP_MD_CTX_cleanup (&md_ctx);
    ssl3_record_sequence_update (seq);
    return ((int) md_size);
  }
}

static unsigned char const *salt[3] = {
  (unsigned char const *) "A", (unsigned char const *) "BB",
    (unsigned char const *) "CCC"
};

int
ssl3_generate_master_secret (SSL * s, unsigned char *out, unsigned char *p,
			     int len)
{
  unsigned char buf___5[64];
  EVP_MD_CTX ctx;
  int i;
  int ret___3;
  unsigned int n;
  size_t tmp;
  {
    ret___3 = 0;
    EVP_MD_CTX_init (&ctx);
    i = 0;
    while (i < 3)
      {
	EVP_DigestInit_ex (&ctx, (s->ctx)->sha1, (ENGINE *) ((void *) 0));
	tmp = strlen ((char const *) salt[i]);
	EVP_DigestUpdate (&ctx, (void const *) salt[i], tmp);
	EVP_DigestUpdate (&ctx, (void const *) p, (unsigned int) len);
	EVP_DigestUpdate (&ctx, (void const *) (&(s->s3)->client_random[0]),
			  32U);
	EVP_DigestUpdate (&ctx, (void const *) (&(s->s3)->server_random[0]),
			  32U);
	EVP_DigestFinal_ex (&ctx, buf___5, &n);
	EVP_DigestInit_ex (&ctx, (s->ctx)->md5, (ENGINE *) ((void *) 0));
	EVP_DigestUpdate (&ctx, (void const *) p, (unsigned int) len);
	EVP_DigestUpdate (&ctx, (void const *) (buf___5), n);
	EVP_DigestFinal_ex (&ctx, out, &n);
	out += n;
	ret___3 = (int) ((unsigned int) ret___3 + n);
	i++;
      }
    EVP_MD_CTX_cleanup (&ctx);
    return (ret___3);
  }
}

int ssl3_write_pending (SSL * s, int type, unsigned char const *buf___5,
			unsigned int len);
static int do_ssl3_write (SSL * s, int type, unsigned char const *buf___5,
			  unsigned int len, int create_empty_fragment);
static int ssl3_get_record (SSL * s);
int
ssl3_read_n (SSL * s, int n, int max___1, int extend)
{
  int i;
  int off;
  int newb;
  int tmp;
  int max_max;
  int *tmp___0;
  {
  }
}

static int
ssl3_get_record (SSL * s)
{
  int ssl_major;
  int ssl_minor;
  int al;
  int enc_err;
  int n;
  int i;
  int ret___3;
  SSL3_RECORD *rr;
  SSL_SESSION *sess;
  unsigned char *p;
  unsigned char md___0[64];
  short version;
  unsigned int mac_size;
  int clear;
  size_t extra;
  int decryption_failed_or_bad_record_mac;
  unsigned char *mac;
  unsigned char *tmp;
  unsigned char *tmp___0;
  unsigned char *tmp___1;
  int tmp___2;
  int tmp___3;
  int tmp___4;
  {
    ret___3 = -1;
    clear = 0;
    decryption_failed_or_bad_record_mac = 0;
    mac = (unsigned char *) ((void *) 0);
    rr = &(s->s3)->rrec;
    sess = s->session;
    if (s->options & 32UL)
      {
	extra = 16384U;
      }
    else
      {
	extra = 0U;
      }
    if (extra != (s->s3)->rbuf.len - 18437U)
      {
	ERR_put_error (20, 143, 68, "s3_pkt.c", 257);
	return (-1);
      }
  again:if (s->rstate != 241)
      {
	goto _L;
      }
    else
      {
	if (s->packet_length < 5U)
	  {
	  _L:n = ssl3_read_n (s, 5, (int) (s->s3)->rbuf.len, 0);
	    if (n <= 0)
	      {
		return (n);
	      }
	    s->rstate = 241;
	    p = s->packet;
	    tmp = p;
	    p++;
	    rr->type = (int) *tmp;
	    tmp___0 = p;
	    p++;
	    ssl_major = (int) *tmp___0;
	    tmp___1 = p;
	    p++;
	    ssl_minor = (int) *tmp___1;
	    version = (short) ((ssl_major << 8) | ssl_minor);
	    rr->length =
	      ((unsigned int) *(p + 0) << 8) | (unsigned int) *(p + 1);
	    p += 2;
	    if (!s->first_packet)
	      {
		if ((int) version != s->version)
		  {
		    ERR_put_error (20, 143, 267, "s3_pkt.c", 284);
		    s->version = (int) version;
		    al = 70;
		    goto f_err;
		  }
	      }
	    if ((int) version >> 8 != 3)
	      {
		ERR_put_error (20, 143, 267, "s3_pkt.c", 295);
		goto err;
	      }
	    if (rr->length > 18432U + extra)
	      {
		al = 22;
		ERR_put_error (20, 143, 198, "s3_pkt.c", 302);
		goto f_err;
	      }
	  }
      }
    if (rr->length > s->packet_length - 5U)
      {
	i = (int) rr->length;
	n = ssl3_read_n (s, i, i, 1);
	if (n <= 0)
	  {
	    return (n);
	  }
      }
    s->rstate = 240;
    rr->input = s->packet + 5;
    if (rr->length > 18432U + extra)
      {
	al = 22;
	ERR_put_error (20, 143, 150, "s3_pkt.c", 342);
	goto f_err;
      }
    rr->data = rr->input;
    enc_err = (*(((s->method)->ssl3_enc)->enc)) (s, 0);
    if (enc_err <= 0)
      {
	if (enc_err == 0)
	  {
	    goto err;
	  }
	decryption_failed_or_bad_record_mac = 1;
      }
    if ((unsigned int) sess == (unsigned int) ((void *) 0))
      {
	clear = 1;
      }
    else
      {
	if ((unsigned int) s->enc_read_ctx == (unsigned int) ((void *) 0))
	  {
	    clear = 1;
	  }
	else
	  {
	    if ((unsigned int) s->read_hash == (unsigned int) ((void *) 0))
	      {
		clear = 1;
	      }
      }}
    if (!clear)
      {
	tmp___2 = EVP_MD_size (s->read_hash);
	mac_size = (unsigned int) tmp___2;
	if (rr->length > (17408U + extra) + mac_size)
	  {
	    decryption_failed_or_bad_record_mac = 1;
	  }
	if (rr->length >= mac_size)
	  {
	    rr->length -= mac_size;
	    mac = rr->data + rr->length;
	  }
	else
	  {
	    decryption_failed_or_bad_record_mac = 1;
	    rr->length = 0U;
	  }
	i = (*(((s->method)->ssl3_enc)->mac)) (s, md___0, 0);
	if ((unsigned int) mac == (unsigned int) ((void *) 0))
	  {
	    decryption_failed_or_bad_record_mac = 1;
	  }
	else
	  {
	    tmp___3 =
	      memcmp ((void const *) (md___0), (void const *) mac, mac_size);
	    if (tmp___3 != 0)
	      {
		decryption_failed_or_bad_record_mac = 1;
	      }
	  }
      }
    if (decryption_failed_or_bad_record_mac)
      {
	al = 20;
	ERR_put_error (20, 143, 281, "s3_pkt.c", 422);
	goto f_err;
      }
    if ((unsigned int) s->expand != (unsigned int) ((void *) 0))
      {
	if (rr->length > 17408U + extra)
	  {
	    al = 22;
	    ERR_put_error (20, 143, 140, "s3_pkt.c", 432);
	    goto f_err;
	  }
	tmp___4 = ssl3_do_uncompress (s);
	if (!tmp___4)
	  {
	    al = 30;
	    ERR_put_error (20, 143, 107, "s3_pkt.c", 438);
	    goto f_err;
	  }
      }
    if (rr->length > 16384U + extra)
      {
	al = 22;
	ERR_put_error (20, 143, 146, "s3_pkt.c", 446);
	goto f_err;
      }
    rr->off = 0U;
    s->packet_length = 0U;
    if (rr->length == 0U)
      {
	goto again;
      }
    return (1);
  f_err:ssl3_send_alert (s, 2, al);
  err:return (ret___3);
  }
}

int
ssl3_write_bytes (SSL * s, int type, void const *buf_, int len)
{
  unsigned char const *buf___5;
  unsigned int tot;
  unsigned int n;
  unsigned int nw;
  int i;
  int tmp;
  {
    buf___5 = (unsigned char const *) buf_;
    s->rwstate = 1;
    tot = (s->s3)->wnum;
    (s->s3)->wnum = 0U;
    tmp = SSL_state ((SSL const *) s);
    if (tmp & 12288)
      {
	if (!s->in_handshake)
	  {
	    i = (*(s->handshake_func)) (s);
	    if (i < 0)
	      {
		return (i);
	      }
	    if (i == 0)
	      {
		ERR_put_error (20, 158, 229, "s3_pkt.c", 530);
		return (-1);
	      }
	  }
      }
    n = (unsigned int) len - tot;
    while (1)
      {
	if (n > 16384U)
	  {
	    nw = 16384U;
	  }
	else
	  {
	    nw = n;
	  }
	i = do_ssl3_write (s, type, buf___5 + tot, nw, 0);
	if (i <= 0)
	  {
	    (s->s3)->wnum = tot;
	    return (i);
	  }
	if (i == (int) n)
	  {
	    (s->s3)->empty_fragment_done = 0;
	    return ((int) (tot + (unsigned int) i));
	  }
	else
	  {
	    if (type == 23)
	      {
		if (s->mode & 1UL)
		  {
		    (s->s3)->empty_fragment_done = 0;
		    return ((int) (tot + (unsigned int) i));
		  }
	      }
	  } n -= (unsigned int) i;
	tot += (unsigned int) i;
      }
  }
}

static int
do_ssl3_write (SSL * s, int type, unsigned char const *buf___5,
	       unsigned int len, int create_empty_fragment)
{
  unsigned char *p;
  unsigned char *plen;
  int i;
  int mac_size;
  int clear;
  int prefix_len;
  SSL3_RECORD *wr;
  SSL3_BUFFER *wb;
  SSL_SESSION *sess;
  int tmp;
  unsigned char *tmp___0;
  unsigned char *tmp___1;
  unsigned char *tmp___2;
  int tmp___3;
  int tmp___4;
  {
    clear = 0;
    prefix_len = 0;
    if ((s->s3)->wbuf.left != 0)
      {
	tmp = ssl3_write_pending (s, type, buf___5, len);
	return (tmp);
      }
    if ((s->s3)->alert_dispatch)
      {
	i = (*((s->method)->ssl_dispatch_alert)) (s);
	if (i <= 0)
	  {
	    return (i);
	  }
      }
    if (len == 0U)
      {
	if (!create_empty_fragment)
	  {
	    return (0);
	  }
      }
    wr = &(s->s3)->wrec;
    wb = &(s->s3)->wbuf;
    sess = s->session;
    if ((unsigned int) sess == (unsigned int) ((void *) 0))
      {
	clear = 1;
      }
    (s->s3)->wpend_type = type;
    (s->s3)->wpend_ret = (int) len;
    tmp___4 = ssl3_write_pending (s, type, buf___5, len);
    return (tmp___4);
  err:return (-1);
  }
}

int
ssl3_write_pending (SSL * s, int type, unsigned char const *buf___5,
		    unsigned int len)
{
  int i;
  int *tmp;
  {
    if ((s->s3)->wpend_tot > (int) len)
      {
	ERR_put_error (20, 159, 127, "s3_pkt.c", 731);
	return (-1);
      }
    else
      {
	if ((unsigned int) (s->s3)->wpend_buf != (unsigned int) buf___5)
	  {
	    if (!(s->mode & 2UL))
	      {
		ERR_put_error (20, 159, 127, "s3_pkt.c", 731);
		return (-1);
	      }
	    else
	      {
		goto _L;
	      }
	  }
	else
	  {
	  _L:if ((s->s3)->wpend_type != type)
	      {
		ERR_put_error (20, 159, 127, "s3_pkt.c", 731);
		return (-1);
	      }
	  }
      }
    while (1)
      {
	tmp = __errno_location ();
	*tmp = 0;
	if ((unsigned int) s->wbio != (unsigned int) ((void *) 0))
	  {
	    s->rwstate = 2;
	    i =
	      BIO_write (s->wbio,
			 (void const
			  *) ((char *) ((s->s3)->wbuf.buf +
					(s->s3)->wbuf.offset)),
			 (int) ((unsigned int) (s->s3)->wbuf.left));
	  }
	else
	  {
	    ERR_put_error (20, 159, 128, "s3_pkt.c", 747);
	    i = -1;
	  }
	if (i == (s->s3)->wbuf.left)
	  {
	    (s->s3)->wbuf.left = 0;
	    s->rwstate = 1;
	    return ((s->s3)->wpend_ret);
	  }
	else
	  {
	    if (i <= 0)
	      {
		return (i);
	      }
	  }
	(s->s3)->wbuf.offset += i;
	(s->s3)->wbuf.left -= i;
      }
  }
}

int
ssl3_read_bytes (SSL * s, int type, unsigned char *buf___5, int len, int peek)
{
  int al;
  int i;
  int j;
  int ret___3;
  unsigned int n;
  SSL3_RECORD *rr;
  void (*cb) (SSL const *ssl, int type2, int val);
  int tmp;
  unsigned char *src;
  unsigned char *dst;
  unsigned int k;
  unsigned char *tmp___0;
  unsigned char *tmp___1;
  unsigned char *tmp___2;
  int tmp___3;
  int tmp___4;
  unsigned int dest_maxlen;
  int alert_level;
  int alert_descr;
  char tmp___10[16];
  int tmp___11;
  BIO *bio___0;
  {
    cb = (void (*)(SSL const *ssl, int type2, int val)) ((void *) 0);
    if ((unsigned int) (s->s3)->rbuf.buf == (unsigned int) ((void *) 0))
      {
	tmp = ssl3_setup_buffers (s);
	if (!tmp)
	  {
	    return (-1);
	  }
      }
    if (type)
      {
	if (type != 23)
	  {
	    if (type != 22)
	      {
		if (type)
		  {
		    ERR_put_error (20, 148, 68, "s3_pkt.c", 804);
		    return (-1);
		  }
		else
		  {
		    goto _L___1;
		  }
	      }
	    else
	      {
		goto _L___1;
	      }
	  }
	else
	  {
	    goto _L___1;
	  }
      }
    else
      {
      _L___1:if (peek)
	  {
	    if (type != 23)
	      {
		ERR_put_error (20, 148, 68, "s3_pkt.c", 804);
		return (-1);
	      }
	  }
      }
    if (type == 22)
      {
	if ((s->s3)->handshake_fragment_len > 0U)
	  {
	    src = (s->s3)->handshake_fragment;
	    dst = buf___5;
	    n = 0U;
	    while (1)
	      {
		if (len > 0)
		  {
		    if (!((s->s3)->handshake_fragment_len > 0U))
		      {
			break;
		      }
		  }
		else
		  {
		    break;
		  }
		tmp___0 = dst;
		dst++;
		tmp___1 = src;
		src++;
		*tmp___0 = *tmp___1;
		len--;
		((s->s3)->handshake_fragment_len)--;
		n++;
	      }
	    k = 0U;
	    while (k < (s->s3)->handshake_fragment_len)
	      {
		tmp___2 = src;
		src++;
		(s->s3)->handshake_fragment[k] = *tmp___2;
		k++;
	      }
	    return ((int) n);
	  }
      }
    if (!s->in_handshake)
      {
	tmp___3 = SSL_state ((SSL const *) s);
	if (tmp___3 & 12288)
	  {
	    i = (*(s->handshake_func)) (s);
	    if (i < 0)
	      {
		return (i);
	      }
	    if (i == 0)
	      {
		ERR_put_error (20, 148, 229, "s3_pkt.c", 838);
		return (-1);
	      }
	  }
      }
  start:s->rwstate = 1;
    rr = &(s->s3)->rrec;
    if (rr->length == 0U)
      {
	goto _L___2;
      }
    else
      {
	if (s->rstate == 241)
	  {
	  _L___2:ret___3 = ssl3_get_record (s);
	    if (ret___3 <= 0)
	      {
		return (ret___3);
	      }
	  }
      }
    if ((s->s3)->change_cipher_spec)
      {
	if (rr->type != 22)
	  {
	    al = 10;
	    ERR_put_error (20, 148, 145, "s3_pkt.c", 865);
	    goto f_err;
	  }
      }
    if ((s->s3)->alert_fragment_len >= 2U)
      {
	alert_level = (int) (s->s3)->alert_fragment[0];
	alert_descr = (int) (s->s3)->alert_fragment[1];
	(s->s3)->alert_fragment_len = 0U;
	if (s->msg_callback)
	  {
	    (*(s->msg_callback)) (0, s->version, 21,
				  (void const *) ((s->s3)->alert_fragment),
				  2U, s, s->msg_callback_arg);
	  }
	if ((unsigned int) s->info_callback != (unsigned int) ((void *) 0))
	  {
	    cb = s->info_callback;
	  }
	else
	  {
	    if ((unsigned int) (s->ctx)->info_callback !=
		(unsigned int) ((void *) 0))
	      {
		cb = (s->ctx)->info_callback;
	      }
	} if ((unsigned int) cb != (unsigned int) ((void *) 0))
	  {
	    j = (alert_level << 8) | alert_descr;
	    (*cb) ((SSL const *) s, 16388, j);
	  }
	if (alert_level == 1)
	  {
	    (s->s3)->warn_alert = alert_descr;
	    if (alert_descr == 0)
	      {
		s->shutdown |= 2;
		return (0);
	      }
	  }
	else
	  {
	    if (alert_level == 2)
	      {
		s->rwstate = 1;
		(s->s3)->fatal_alert = alert_descr;
		ERR_put_error (20, 148, 1000 + alert_descr, "s3_pkt.c", 1053);
		BIO_snprintf (tmp___10, sizeof (tmp___10), "%d", alert_descr);
		ERR_add_error_data (2, "SSL alert number ", tmp___10);
		s->shutdown |= 2;
		SSL_CTX_remove_session (s->ctx, s->session);
		return (0);
	      }
	    else
	      {
		al = 47;
		ERR_put_error (20, 148, 246, "s3_pkt.c", 1063);
		goto f_err;
	      }
	  }
	goto start;
      }
    if (s->shutdown & 1)
      {
	s->rwstate = 1;
	rr->length = 0U;
	return (0);
      }
    if (rr->type == 20)
      {
	if (rr->length != 1U)
	  {
	    al = 47;
	    ERR_put_error (20, 148, 103, "s3_pkt.c", 1085);
	    goto f_err;
	  }
	else
	  {
	    if (rr->off != 0U)
	      {
		al = 47;
		ERR_put_error (20, 148, 103, "s3_pkt.c", 1085);
		goto f_err;
	      }
	    else
	      {
		if ((int) *(rr->data + 0) != 1)
		  {
		    al = 47;
		    ERR_put_error (20, 148, 103, "s3_pkt.c", 1085);
		    goto f_err;
		  }
	      }
	  }
	if ((unsigned int) (s->s3)->tmp.new_cipher ==
	    (unsigned int) ((void *) 0))
	  {
	    al = 10;
	    ERR_put_error (20, 148, 133, "s3_pkt.c", 1093);
	    goto f_err;
	  }
	rr->length = 0U;
	if (s->msg_callback)
	  {
	    (*(s->msg_callback)) (0, s->version, 20, (void const *) rr->data,
				  1U, s, s->msg_callback_arg);
	  }
	(s->s3)->change_cipher_spec = 1;
	tmp___11 = ssl3_do_change_cipher_spec (s);
	if (tmp___11)
	  {
	    goto start;
	  }
	else
	  {
	    goto err;
	  }
      }
    if ((s->s3)->handshake_fragment_len >= 4U)
      {
	if (!s->in_handshake)
	  {
	    if ((s->state & 4095) == 3)
	      {
		if (!((s->s3)->flags & 1L))
		  {
		    if (s->server)
		      {
			s->state = 8192;
		      }
		    else
		      {
			s->state = 4096;
		      }
		    s->new_session = 1;
		  }
	      }
	    i = (*(s->handshake_func)) (s);
	    if (i < 0)
	      {
		return (i);
	      }
	    if (i == 0)
	      {
		ERR_put_error (20, 148, 229, "s3_pkt.c", 1130);
		return (-1);
	      }
	    if (!(s->mode & 4UL))
	      {
		if ((s->s3)->rbuf.left == 0)
		  {
		    s->rwstate = 3;
		    bio___0 = SSL_get_rbio ((SSL const *) s);
		    BIO_clear_flags (bio___0, 15);
		    BIO_set_flags (bio___0, 9);
		    return (-1);
		  }
	      }
	    goto start;
	  }
      }
    switch (rr->type)
      {
      default:;
	if (s->version == 769)
	  {
	    rr->length = 0U;
	    goto start;
	  }
	al = 10;
	ERR_put_error (20, 148, 245, "s3_pkt.c", 1165);
	goto f_err;
      case 20:
      case 21:
      case 22:
	al = 10;
	ERR_put_error (20, 148, 68, "s3_pkt.c", 1174);
	goto f_err;
      case 23:
	if ((s->s3)->in_read_app_data)
	  {
	    if ((s->s3)->total_renegotiations != 0)
	      {
		if (s->state & 4096)
		  {
		    if (s->state >= 4368)
		      {
			if (s->state <= 4384)
			  {
			    (s->s3)->in_read_app_data = 2;
			    return (-1);
			  }
			else
			  {
			    goto _L___4;
			  }
		      }
		    else
		      {
			goto _L___4;
		      }
		  }
		else
		  {
		  _L___4:if (s->state & 8192)
		      {
			if (s->state <= 8480)
			  {
			    if (s->state >= 8464)
			      {
				(s->s3)->in_read_app_data = 2;
				return (-1);
			      }
			    else
			      {
				al = 10;
				ERR_put_error (20, 148, 245, "s3_pkt.c",
					       1203);
				goto f_err;
			      }
			  }
			else
			  {
			    al = 10;
			    ERR_put_error (20, 148, 245, "s3_pkt.c", 1203);
			    goto f_err;
			  }
		      }
		    else
		      {
			al = 10;
			ERR_put_error (20, 148, 245, "s3_pkt.c", 1203);
			goto f_err;
		      }
		  }
	      }
	    else
	      {
		al = 10;
		ERR_put_error (20, 148, 245, "s3_pkt.c", 1203);
		goto f_err;
	      }
	  }
	else
	  {
	    al = 10;
	    ERR_put_error (20, 148, 245, "s3_pkt.c", 1203);
	    goto f_err;
	  }
      }
  f_err:ssl3_send_alert (s, 2, al);
  err:return (-1);
  }
}

int
ssl3_do_change_cipher_spec (SSL * s)
{
  int i;
  char const *sender;
  int slen;
  int tmp;
  int tmp___0;
  {
    if (s->state & 8192)
      {
	i = 33;
      }
    else
      {
	i = 17;
      }
    if ((unsigned int) (s->s3)->tmp.key_block == (unsigned int) ((void *) 0))
      {
	(s->session)->cipher = (s->s3)->tmp.new_cipher;
	tmp = (*(((s->method)->ssl3_enc)->setup_key_block)) (s);
	if (!tmp)
	  {
	    return (0);
	  }
      }
    tmp___0 = (*(((s->method)->ssl3_enc)->change_cipher_state)) (s, i);
    if (!tmp___0)
      {
	return (0);
      }
    if (s->state & 4096)
      {
	sender = ((s->method)->ssl3_enc)->server_finished_label;
	slen = ((s->method)->ssl3_enc)->server_finished_label_len;
      }
    else
      {
	sender = ((s->method)->ssl3_enc)->client_finished_label;
	slen = ((s->method)->ssl3_enc)->client_finished_label_len;
      }
    (s->s3)->tmp.peer_finish_md_len =
      (*(((s->method)->ssl3_enc)->final_finish_mac)) (s,
						      &(s->s3)->finish_dgst1,
						      &(s->s3)->finish_dgst2,
						      sender, slen,
						      (s->s3)->tmp.
						      peer_finish_md);
    return (1);
  }
}

void
ssl3_send_alert (SSL * s, int level, int desc)
{
  {
    desc = (*(((s->method)->ssl3_enc)->alert_value)) (desc);
    if (s->version == 768)
      {
	if (desc == 70)
	  {
	    desc = 40;
	  }
      }
    if (desc < 0)
      {
	return;
      }
    if (level == 2)
      {
	if ((unsigned int) s->session != (unsigned int) ((void *) 0))
	  {
	    SSL_CTX_remove_session (s->ctx, s->session);
	  }
      }
    (s->s3)->alert_dispatch = 1;
    (s->s3)->send_alert[0] = (unsigned char) level;
    (s->s3)->send_alert[1] = (unsigned char) desc;
    if ((s->s3)->wbuf.left == 0)
      {
	(*((s->method)->ssl_dispatch_alert)) (s);
      }
    return;
  }
}

int
ssl3_dispatch_alert (SSL * s)
{
  int i;
  int j;
  void (*cb) (SSL const *ssl, int type, int val);
  {
    cb = (void (*)(SSL const *ssl, int type, int val)) ((void *) 0);
    (s->s3)->alert_dispatch = 0;
    i =
      do_ssl3_write (s, 21, (unsigned char const *) (&(s->s3)->send_alert[0]),
		     2U, 0);
    if (i <= 0)
      {
	(s->s3)->alert_dispatch = 1;
      }
    else
      {
	if ((int) (s->s3)->send_alert[0] == 2)
	  {
	    BIO_ctrl (s->wbio, 11, 0L, (void *) 0);
	  }
	if (s->msg_callback)
	  {
	    (*(s->msg_callback)) (1, s->version, 21,
				  (void const *) ((s->s3)->send_alert), 2U, s,
				  s->msg_callback_arg);
	  }
	if ((unsigned int) s->info_callback != (unsigned int) ((void *) 0))
	  {
	    cb = s->info_callback;
	  }
	else
	  {
	    if ((unsigned int) (s->ctx)->info_callback !=
		(unsigned int) ((void *) 0))
	      {
		cb = (s->ctx)->info_callback;
	      }
	} if ((unsigned int) cb != (unsigned int) ((void *) 0))
	  {
	    j =
	      ((int) (s->s3)->send_alert[0] << 8) | (int) (s->s3)->
	      send_alert[1];
	    (*cb) ((SSL const *) s, 16392, j);
	  }
      }
    return (i);
  }
}

int
ssl3_do_write (SSL * s, int type)
{
  int ret___3;
  {
    ret___3 =
      ssl3_write_bytes (s, type,
			(void const *) ((s->init_buf)->data + s->init_off),
			s->init_num);
    if (ret___3 < 0)
      {
	return (-1);
      }
    if (type == 22)
      {
	ssl3_finish_mac (s,
			 (unsigned char const
			  *) ((unsigned char *) ((s->init_buf)->data +
						 s->init_off)), ret___3);
      }
    if (ret___3 == s->init_num)
      {
	if (s->msg_callback)
	  {
	    (*(s->msg_callback)) (1, s->version, type,
				  (void const *) (s->init_buf)->data,
				  (unsigned int) (s->init_off + s->init_num),
				  s, s->msg_callback_arg);
	  }
	return (1);
      }
    s->init_off += ret___3;
    s->init_num -= ret___3;
    return (0);
  }
}

int
ssl3_send_finished (SSL * s, int a, int b, char const *sender, int slen)
{
  unsigned char *p;
  unsigned char *d;
  int i;
  unsigned long l;
  unsigned char *tmp;
  int tmp___0;
  {
    if (s->state == a)
      {
	d = (unsigned char *) (s->init_buf)->data;
	p = d + 4;
	i =
	  (*(((s->method)->ssl3_enc)->final_finish_mac)) (s,
							  &(s->s3)->
							  finish_dgst1,
							  &(s->s3)->
							  finish_dgst2,
							  sender, slen,
							  (s->s3)->tmp.
							  finish_md);
	(s->s3)->tmp.finish_md_len = i;
	memcpy ((void *__restrict) p,
		(void const *__restrict) ((s->s3)->tmp.finish_md),
		(unsigned int) i);
	p += i;
	l = (unsigned long) i;
	tmp = d;
	d++;
	*tmp = (unsigned char) 20;
	*(d + 0) = (unsigned char) ((l >> 16) & 255UL);
	*(d + 1) = (unsigned char) ((l >> 8) & 255UL);
	*(d + 2) = (unsigned char) (l & 255UL);
	d += 3;
	s->init_num = (int) l + 4;
	s->init_off = 0;
	s->state = b;
      }
    tmp___0 = ssl3_do_write (s, 22);
    return (tmp___0);
  }
}

int
ssl3_get_finished (SSL * s, int a, int b)
{
  int al;
  int i;
  int ok;
  long n;
  unsigned char *p;
  int tmp;
  {
    if (tmp != 0)
      {
	al = 51;
	ERR_put_error (20, 140, 149, "s3_both.c", 231);
	goto f_err;
      }
    return (1);
  f_err:ssl3_send_alert (s, 2, al);
    return (0);
  }
}

int
ssl3_send_change_cipher_spec (SSL * s, int a, int b)
{
  unsigned char *p;
  int tmp;
  {
    if (s->state == a)
      {
	p = (unsigned char *) (s->init_buf)->data;
	*p = (unsigned char) 1;
	s->init_num = 1;
	s->init_off = 0;
	s->state = b;
      }
    tmp = ssl3_do_write (s, 20);
    return (tmp);
  }
}

long
ssl3_get_message (SSL * s, int st1, int stn, int mt, long max___1, int *ok)
{
  unsigned char *p;
  unsigned long l;
  long n;
  int i;
  int al;
  int skip_message;
  unsigned char *tmp;
  int tmp___0;
  {
    if ((s->s3)->tmp.reuse_message)
      {
	(s->s3)->tmp.reuse_message = 0;
	if (mt >= 0)
	  {
	    if ((s->s3)->tmp.message_type != mt)
	      {
		al = 10;
		ERR_put_error (20, 142, 244, "s3_both.c", 375);
		goto f_err;
	      }
	  }
	*ok = 1;
	s->init_msg = (void *) ((s->init_buf)->data + 4);
	s->init_num = (int) (s->s3)->tmp.message_size;
	return ((long) s->init_num);
      }
    return ((long) s->init_num);
  f_err:ssl3_send_alert (s, 2, al);
  err:*ok = 0;
    return (-1L);
  }
}

int
ssl_cert_type (X509 * x, EVP_PKEY * pkey)
{
  EVP_PKEY *pk;
  int ret___3;
  int i;
  {
    ret___3 = -1;
    if ((unsigned int) pkey == (unsigned int) ((void *) 0))
      {
	pk = X509_get_pubkey (x);
      }
    else
      {
	pk = pkey;
      }
    if ((unsigned int) pk == (unsigned int) ((void *) 0))
      {
	goto err;
      }
    i = pk->type;
    if (i == 6)
      {
	ret___3 = 0;
      }
    else
      {
	if (i == 116)
	  {
	    ret___3 = 2;
	  }
	else
	  {
	    if (i == 408)
	      {
		ret___3 = 5;
	      }
	  }
      }
  err:if (!pkey)
      {
	EVP_PKEY_free (pk);
      }
    return (ret___3);
  }
}

int
ssl_verify_alarm_type (long type)
{
  int al;
  {
    switch ((int) type)
      {
      case 2:
      case 3:
      case 33:
	al = 48;
	break;
      case 4:
      case 5:
      case 6:
      case 13:
      case 14:
      case 15:
      case 16:
      case 9:
      case 11:
      case 27:
      case 28:
	al = 42;
	break;
      case 7:
      case 8:
	al = 51;
	break;
      case 10:
      case 12:
	al = 45;
	break;
      case 23:
	al = 44;
	break;
      case 17:
	al = 80;
	break;
      case 18:
      case 19:
      case 20:
      case 21:
      case 22:
      case 25:
      case 24:
	al = 48;
	break;
      case 50:
	al = 40;
	break;
      case 26:
	al = 43;
	break;
      default:
	al = 46;
	break;
      }
    return (al);
  }
}

int ssl_undefined_const_function (SSL const *s);
int ssl23_num_ciphers (void);
SSL_CIPHER *ssl23_get_cipher (unsigned int u);
int ssl23_read (SSL * s, void *buf___5, int len);
int ssl23_peek (SSL * s, void *buf___5, int len);
int ssl23_write (SSL * s, void const *buf___5, int len);
int ssl23_put_cipher_by_char (SSL_CIPHER const *c, unsigned char *p);
SSL_CIPHER *ssl23_get_cipher_by_char (unsigned char const *p);
long ssl23_default_timeout (void);
int ssl23_accept (SSL * s);
int ssl23_connect (SSL * s);
int tls1_new (SSL * s);
void tls1_free (SSL * s);
void tls1_clear (SSL * s);
int ssl23_get_client_hello (SSL * s);
static SSL_METHOD *
ssl23_get_server_method (int ver)
{
  SSL_METHOD *tmp;
  SSL_METHOD *tmp___0;
  SSL_METHOD *tmp___1;
  {
    if (ver == 2)
      {
	tmp = SSLv2_server_method ();
	return (tmp);
      }
    if (ver == 768)
      {
	tmp___0 = SSLv3_server_method ();
	return (tmp___0);
      }
    else
      {
	if (ver == 769)
	  {
	    tmp___1 = TLSv1_server_method ();
	    return (tmp___1);
	  }
	else
	  {
	    return ((SSL_METHOD *) ((void *) 0));
      }}
  }
}

static SSL_METHOD SSLv23_server_method_data = {
  769, &tls1_new, &tls1_clear, &tls1_free, &ssl23_accept,
    &ssl_undefined_function, &ssl23_read, &ssl23_peek, &ssl23_write,
    &ssl_undefined_function, &ssl_undefined_function, &ssl_ok,
    &ssl3_get_message, &ssl3_read_bytes, &ssl3_write_bytes,
    &ssl3_dispatch_alert, &ssl3_ctrl, &ssl3_ctx_ctrl,
    &ssl23_get_cipher_by_char, &ssl23_put_cipher_by_char,
    &ssl_undefined_const_function, &ssl23_num_ciphers, &ssl23_get_cipher,
    &ssl23_get_server_method, &ssl23_default_timeout, &ssl3_undef_enc_method,
    &ssl_undefined_void_function, &ssl3_callback_ctrl, &ssl3_ctx_callback_ctrl
};

SSL_METHOD *
SSLv23_server_method (void)
{
  {
    return (&SSLv23_server_method_data);
  }
}

int
ssl23_accept (SSL * s)
{
  BUF_MEM *buf___5;
  unsigned long Time;
  time_t tmp;
  void (*cb) (SSL const *ssl, int type, int val);
  int ret___3;
  int new_state;
  int state___0;
  int *tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  {
    tmp = time ((time_t *) ((void *) 0));
    Time = (unsigned long) tmp;
    cb = (void (*)(SSL const *ssl, int type, int val)) ((void *) 0);
    ret___3 = -1;
    if (tmp___1 & 12288)
      {
	tmp___2 = SSL_state ((SSL const *) s);
	if (tmp___2 & 16384)
	  {
	    SSL_clear (s);
	  }
      }
    else
      {
	SSL_clear (s);
      }
    while (1)
      {
	state___0 = s->state;
	switch (s->state)
	  {
	  case 16384:
	  case 8192:
	  case 24576:
	  case 8195:
	    s->server = 1;
	    if ((unsigned int) cb != (unsigned int) ((void *) 0))
	      {
		(*cb) ((SSL const *) s, 16, 1);
	      }
	    s->type = 8192;
	    if ((unsigned int) s->init_buf == (unsigned int) ((void *) 0))
	      {
		buf___5 = BUF_MEM_new ();
		if ((unsigned int) buf___5 == (unsigned int) ((void *) 0))
		  {
		    ret___3 = -1;
		    goto end;
		  }
		tmp___3 = BUF_MEM_grow (buf___5, 16384);
		if (!tmp___3)
		  {
		    ret___3 = -1;
		    goto end;
		  }
		s->init_buf = buf___5;
	      }
	    ssl3_init_finished_mac (s);
	    s->state = 8720;
	    ((s->ctx)->stats.sess_accept)++;
	    s->init_num = 0;
	    break;
	  case 8720:
	  case 8721:
	    s->shutdown = 0;
	    ret___3 = ssl23_get_client_hello (s);
	    if (ret___3 >= 0)
	      {
		cb =
		  (void (*)(SSL const *ssl, int type, int val)) ((void *) 0);
	      }
	    goto end;
	  default:
	    ERR_put_error (20, 115, 255, "s23_srvr.c", 209);
	    ret___3 = -1;
	    goto end;
	  }
	if ((unsigned int) cb != (unsigned int) ((void *) 0))
	  {
	    if (s->state != state___0)
	      {
		new_state = s->state;
		s->state = state___0;
		(*cb) ((SSL const *) s, 8193, 1);
		s->state = new_state;
	      }
	  }
      }
  end:(s->in_handshake)--;
    if ((unsigned int) cb != (unsigned int) ((void *) 0))
      {
	(*cb) ((SSL const *) s, 8194, ret___3);
      }
    return (ret___3);
  }
}

int
ssl23_get_client_hello (SSL * s)
{
  char buf_space[11];
  char *buf___5;
  unsigned char *p;
  unsigned char *d;
  unsigned char *d_len;
  unsigned char *dd;
  unsigned int i;
  unsigned int csl;
  unsigned char *tmp___69;
  unsigned char *tmp___70;
  unsigned char *tmp___71;
  unsigned char *tmp___72;
  int tmp___73;
  int tmp___74;
  int tmp___75;
  int tmp___76;
  {
    s->init_num = 0;
    if ((unsigned int) buf___5 != (unsigned int) (buf_space))
      {
	CRYPTO_free ((void *) buf___5);
      }
    tmp___76 = SSL_accept (s);
    return (tmp___76);
  err:if ((unsigned int) buf___5 != (unsigned int) (buf_space))
      {
	CRYPTO_free ((void *) buf___5);
      }
    return (-1);
  }
}

static int ssl23_client_hello (SSL * s);
static int ssl23_get_server_hello (SSL * s);
static SSL_METHOD *
ssl23_get_client_method (int ver)
{
  SSL_METHOD *tmp;
  SSL_METHOD *tmp___0;
  SSL_METHOD *tmp___1;
  {
    if (ver == 2)
      {
	tmp = SSLv2_client_method ();
	return (tmp);
      }
    if (ver == 768)
      {
	tmp___0 = SSLv3_client_method ();
	return (tmp___0);
      }
    else
      {
	if (ver == 769)
	  {
	    tmp___1 = TLSv1_client_method ();
	    return (tmp___1);
	  }
	else
	  {
	    return ((SSL_METHOD *) ((void *) 0));
      }}
  }
}

static SSL_METHOD SSLv23_client_method_data = {
  769, &tls1_new, &tls1_clear, &tls1_free, &ssl_undefined_function,
    &ssl23_connect, &ssl23_read, &ssl23_peek, &ssl23_write,
    &ssl_undefined_function, &ssl_undefined_function, &ssl_ok,
    &ssl3_get_message, &ssl3_read_bytes, &ssl3_write_bytes,
    &ssl3_dispatch_alert, &ssl3_ctrl, &ssl3_ctx_ctrl,
    &ssl23_get_cipher_by_char, &ssl23_put_cipher_by_char,
    &ssl_undefined_const_function, &ssl23_num_ciphers, &ssl23_get_cipher,
    &ssl23_get_client_method, &ssl23_default_timeout, &ssl3_undef_enc_method,
    &ssl_undefined_void_function, &ssl3_callback_ctrl, &ssl3_ctx_callback_ctrl
};

SSL_METHOD *
SSLv23_client_method (void)
{
  {
    return (&SSLv23_client_method_data);
  }
}

int
ssl23_connect (SSL * s)
{
  BUF_MEM *buf___5;
  unsigned long Time;
  time_t tmp;
  void (*cb) (SSL const *ssl, int type, int val);
  int ret___3;
  int new_state;
  int state___0;
  int *tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  int tmp___4;
  {
    buf___5 = (BUF_MEM *) ((void *) 0);
    tmp = time ((time_t *) ((void *) 0));
    Time = (unsigned long) tmp;
    cb = (void (*)(SSL const *ssl, int type, int val)) ((void *) 0);
    ret___3 = -1;
    RAND_add ((void const *) (&Time), (int) sizeof (Time), (double) 0);
    ERR_clear_error ();
    tmp___0 = __errno_location ();
    *tmp___0 = 0;
    if ((unsigned int) s->info_callback != (unsigned int) ((void *) 0))
      {
	cb = s->info_callback;
      }
    return (ret___3);
  }
}

static int
ssl23_client_hello (SSL * s)
{
  unsigned char *buf___5;
  unsigned char *p;
  unsigned char *d;
  int i;
  int j;
}

static long
dtls1_get_message_fragment (SSL * s, int st1, int stn, long max___1, int *ok)
{
  unsigned char wire[12];
  unsigned long l;
  unsigned long frag_off;
  unsigned long frag_len;
  int i;
  int al;
  struct hm_header_st msg_hdr;
  int tmp;
  int tmp___0;
  long tmp___1;
  unsigned char *p;
  {
    tmp = dtls1_retrieve_buffered_fragment (s, max___1, ok);
    frag_len = (unsigned long) tmp;
    if (frag_len)
      {
	goto _L;
      }
    else
      {
	if (*ok)
	  {
	  _L:if (*ok)
	      {
		s->init_num = (int) ((unsigned long) s->init_num + frag_len);
	      }
	    return ((long) frag_len);
	  }
      }
    i = (*((s->method)->ssl_read_bytes)) (s, 22, wire, 12, 0);
    if (i <= 0)
      {
	s->rwstate = 3;
	*ok = 0;
	return ((long) i);
      }
    if (!(i == 12))
      {
	OpenSSLDie ("d1_both.c", 636, "i == DTLS1_HM_HEADER_LENGTH");
      }
    dtls1_get_message_header (wire, &msg_hdr);
    if ((int) msg_hdr.seq != (int) (s->d1)->handshake_read_seq)
      {
	tmp___0 = dtls1_process_out_of_seq_message (s, &msg_hdr, ok);
	return ((long) tmp___0);
      }
    l = msg_hdr.msg_len;
    frag_off = msg_hdr.frag_off;
    s->init_num = (int) ((unsigned long) s->init_num + frag_len);
    return ((long) frag_len);
  f_err:ssl3_send_alert (s, 2, al);
    s->init_num = 0;
    *ok = 0;
    return (-1L);
  }
}

int
dtls1_send_finished (SSL * s, int a, int b, char const *sender, int slen)
{
  unsigned char *p;
  unsigned char *d;
  int i;
  unsigned long l;
  int tmp;
  {
    if (s->state == a)
      {
	d = (unsigned char *) (s->init_buf)->data;
	p = d + 12;
	i =
	  (*(((s->method)->ssl3_enc)->final_finish_mac)) (s,
							  &(s->s3)->
							  finish_dgst1,
							  &(s->s3)->
							  finish_dgst2,
							  sender, slen,
							  (s->s3)->tmp.
							  finish_md);
	(s->s3)->tmp.finish_md_len = i;
	memcpy ((void *__restrict) p,
		(void const *__restrict) ((s->s3)->tmp.finish_md),
		(unsigned int) i);
	p += i;
	l = (unsigned long) i;
	d = dtls1_set_message_header (s, d, (unsigned char) 20, l, 0UL, l);
	s->init_num = (int) l + 12;
	s->init_off = 0;
	dtls1_buffer_message (s, 0);
	s->state = b;
      }
    tmp = dtls1_do_write (s, 22);
    return (tmp);
  }
}

int
dtls1_send_change_cipher_spec (SSL * s, int a, int b)
{
  unsigned char *p;
  unsigned char *tmp;
  int tmp___0;
  {
    if (s->state == a)
      {
	p = (unsigned char *) (s->init_buf)->data;
	tmp = p;
	p++;
	*tmp = (unsigned char) 1;
	(s->d1)->handshake_write_seq = (s->d1)->next_handshake_write_seq;
	s->init_num = 1;
	if (s->client_version == 256)
	  {
	    (s->d1)->next_handshake_write_seq =
	      (unsigned short) ((int) (s->d1)->next_handshake_write_seq + 1);
	    *(p + 0) =
	      (unsigned char) (((int) (s->d1)->handshake_write_seq >> 8) &
			       255);
	    *(p + 1) =
	      (unsigned char) ((int) (s->d1)->handshake_write_seq & 255);
	    p += 2;
	    s->init_num += 2;
	  }
	s->init_off = 0;
	dtls1_set_message_header_int (s, (unsigned char) 1, 0UL,
				      (s->d1)->handshake_write_seq, 0UL, 0UL);
	dtls1_buffer_message (s, 1);
	s->state = b;
      }
    tmp___0 = dtls1_do_write (s, 20);
    return (tmp___0);
  }
}

unsigned long
dtls1_output_cert_chain (SSL * s, X509 * x)
{
  unsigned char *p;
  int n;
  int i;
  unsigned long l;
  BUF_MEM *buf___5;
  X509_STORE_CTX xs_ctx;
  X509_OBJECT obj;
  int tmp;
  int tmp___0;
  int tmp___1;
  X509_NAME *tmp___2;
  X509_NAME *tmp___3;
  int tmp___4;
  X509_NAME *tmp___5;
  void *tmp___6;
  int tmp___7;
  int tmp___8;
  {
    l = 15UL;
    buf___5 = s->init_buf;
    *(p + 2) = (unsigned char) (l & 255UL);
    p += 3;
    l += 3UL;
    p = (unsigned char *) (buf___5->data + 0);
    p = dtls1_set_message_header (s, p, (unsigned char) 11, l, 0UL, l);
    l += 12UL;
    return (l);
  }
}

#pragma merger(0,"/tmp/cil-T0o8e1DG.i","-march=pentium,-O3,-fomit-frame-pointer,-Wall")
void *sk_set (STACK * st, int i, void *value);
void sk_zero (STACK * st);
rwlock_t lock12;
rwlock_t lock13;
rwlock_t lock14;
rwlock_t lock16;
void _write_unlock (rwlock_t * lock);
void _write_lock (rwlock_t * lock);
long BIO_int_ctrl (BIO * b, int cmd, long larg, int iarg);
BIO *BIO_find_type (BIO * bio, int type);
BIO_METHOD *BIO_f_buffer (void);
LHASH *lh_new (unsigned long (*h___2) (void const *),
	       int (*c) (void const *, void const *));
CERT *ssl_cert_dup (CERT * cert);
void ssl_cert_free (CERT * c);
int
SSL_clear (SSL * s)
{
  int tmp;
  int tmp___0;
  int tmp___1;
  {
    if ((unsigned int) s->method == (unsigned int) ((void *) 0))
      {
	ERR_put_error (20, 164, 188, "ssl_lib.c", 157);
	return (0);
      }
    tmp = ssl_clear_bad_session (s);
    if (tmp)
      {
	SSL_SESSION_free (s->session);
	s->session = (SSL_SESSION *) ((void *) 0);
      }
    s->error = 0;
    s->hit = 0;
    return (1);
  }
}

SSL *
SSL_new (SSL_CTX * ctx)
{
  SSL *s;
  void *tmp;
  int tmp___0;
  {
    CRYPTO_new_ex_data (1, (void *) s, &s->ex_data);
    return (s);
  err:if ((unsigned int) s != (unsigned int) ((void *) 0))
      {
	if ((unsigned int) s->cert != (unsigned int) ((void *) 0))
	  {
	    ssl_cert_free (s->cert);
	  }
	if ((unsigned int) s->ctx != (unsigned int) ((void *) 0))
	  {
	    SSL_CTX_free (s->ctx);
	  }
	CRYPTO_free ((void *) s);
      }
    ERR_put_error (20, 186, 65, "ssl_lib.c", 341);
    return ((SSL *) ((void *) 0));
  }
}

void
SSL_free (SSL * s)
{
  int i;
  {
    if ((unsigned int) s == (unsigned int) ((void *) 0))
      {
	return;
      }
    i = CRYPTO_add_lock (&s->references, -1, lock16, "ssl_lib.c", 450);
    if (i > 0)
      {
	return;
      }
    if (s->param)
      {
	X509_VERIFY_PARAM_free (s->param);
      }
    CRYPTO_free_ex_data (1, (void *) s, &s->ex_data);
    if ((unsigned int) s->bbio != (unsigned int) ((void *) 0))
      {
	if ((unsigned int) s->bbio == (unsigned int) s->wbio)
	  {
	    s->wbio = BIO_pop (s->wbio);
	  }
	BIO_free (s->bbio);
	s->bbio = (BIO *) ((void *) 0);
      }
    if ((unsigned int) s->rbio != (unsigned int) ((void *) 0))
      {
	BIO_free_all (s->rbio);
      }
    return ((int) s->read_ahead);
  }
}

int
SSL_pending (SSL const *s)
{
  int tmp;
  {
    tmp = (*((s->method)->ssl_pending)) (s);
    return (tmp);
  }
}

X509 *
SSL_get_peer_certificate (SSL const *s)
{
  STACK *r;
  {
    if ((unsigned int) s == (unsigned int) ((void *) 0))
      {
	r = (STACK *) ((void *) 0);
      }
    else
      {
	if ((unsigned int) s->session == (unsigned int) ((void *) 0))
	  {
	    r = (STACK *) ((void *) 0);
	  }
	else
	  {
	    if ((unsigned int) (s->session)->sess_cert ==
		(unsigned int) ((void *) 0))
	      {
		r = (STACK *) ((void *) 0);
	      }
	    else
	      {
		r = ((s->session)->sess_cert)->cert_chain;
	      }
	  }
      }
    return (r);
  }
}

void
SSL_copy_session_id (SSL * t, SSL const *f)
{
  CERT *tmp;
  SSL_SESSION *tmp___0;
  {
    tmp___0 = SSL_get_session (f);
    SSL_set_session (t, tmp___0);
    if ((unsigned int) t->method != (unsigned int) f->method)
      {
	(*((t->method)->ssl_free)) (t);
	t->method = (SSL_METHOD *) f->method;
	(*((t->method)->ssl_new)) (t);
      }
    tmp = t->cert;
    if ((unsigned int) f->cert != (unsigned int) ((void *) 0))
      {
	CRYPTO_add_lock (&(f->cert)->references, 1, lock13, "ssl_lib.c", 790);
	t->cert = (struct cert_st *) f->cert;
      }
    else
      {
	t->cert = (struct cert_st *) ((void *) 0);
      }
    if ((unsigned int) tmp != (unsigned int) ((void *) 0))
      {
	ssl_cert_free (tmp);
      }
    SSL_set_session_id_context (t, (unsigned char const *) (f->sid_ctx),
				(unsigned int) f->sid_ctx_length);
    return;
  }
}

int
SSL_accept (SSL * s)
{
  int tmp;
  {
    if ((unsigned int) s->handshake_func ==
	(unsigned int) ((int (*)(SSL *)) 0))
      {
	SSL_set_connect_state (s);
      }
    tmp = (*((s->method)->ssl_connect)) (s);
    return (tmp);
  }
}

long
SSL_get_default_timeout (SSL const *s)
{
  long tmp;
  {
    tmp = (*((s->method)->get_timeout)) ();
    return (tmp);
  }
}

int
SSL_read (SSL * s, void *buf___5, int num)
{
  int tmp;
  {
    if ((unsigned int) s->handshake_func ==
	(unsigned int) ((int (*)(SSL *)) 0))
      {
	ERR_put_error (20, 223, 276, "ssl_lib.c", 871);
	return (-1);
      }
  }
}

long
SSL_ctrl (SSL * s, int cmd, long larg, void *parg)
{
  long l;
  int tmp;
  long tmp___0;
  {
    switch (cmd)
      {
      case 40:
	return ((long) s->read_ahead);
      case 41:
	l = (long) s->read_ahead;
	s->read_ahead = (int) larg;
	return (l);
      case 16:
	s->msg_callback_arg = parg;
	return (1L);
      case 32:
	s->options |= (unsigned long) larg;
	return ((long) s->options);
      case 33:
	s->mode |= (unsigned long) larg;
	return ((long) s->mode);
      case 50:
	return (s->max_cert_list);
      case 51:
	l = s->max_cert_list;
	s->max_cert_list = larg;
	return (l);
      case 17:
	tmp = SSL_version ((SSL const *) s);
	if (tmp == 65279)
	  {
	    (s->d1)->mtu = (unsigned int) larg;
	    return (larg);
	  }
	return (0L);
      default:
	tmp___0 = (*((s->method)->ssl_ctrl)) (s, cmd, larg, parg);
	return (tmp___0);
      }
  }
}

long
SSL_CTX_ctrl (SSL_CTX * ctx, int cmd, long larg, void *parg)
{
  long l;
  long tmp;
  {
    switch (cmd)
      {
      case 40:
	return ((long) ctx->read_ahead);
      case 41:
	l = (long) ctx->read_ahead;
	ctx->read_ahead = (int) larg;
	return (l);
      case 16:
	ctx->msg_callback_arg = parg;
	return (1L);
      case 50:
	return (ctx->max_cert_list);
      case 51:
	l = ctx->max_cert_list;
	ctx->max_cert_list = larg;
	return (l);
      case 42:
	l = (long) ctx->session_cache_size;
	ctx->session_cache_size = (unsigned long) larg;
	return (l);
      case 43:
	return ((long) ctx->session_cache_size);
      case 44:
	l = (long) ctx->session_cache_mode;
	ctx->session_cache_mode = (int) larg;
	return (l);
      case 45:
	return ((long) ctx->session_cache_mode);
      case 20:
	return ((long) (ctx->sessions)->num_items);
      case 21:
	return ((long) ctx->stats.sess_connect);
      case 22:
	return ((long) ctx->stats.sess_connect_good);
      case 23:
	return ((long) ctx->stats.sess_connect_renegotiate);
      case 24:
	return ((long) ctx->stats.sess_accept);
      case 25:
	return ((long) ctx->stats.sess_accept_good);
      case 26:
	return ((long) ctx->stats.sess_accept_renegotiate);
      case 27:
	return ((long) ctx->stats.sess_hit);
      case 28:
	return ((long) ctx->stats.sess_cb_hit);
      case 29:
	return ((long) ctx->stats.sess_miss);
      case 30:
	return ((long) ctx->stats.sess_timeout);
      case 31:
	return ((long) ctx->stats.sess_cache_full);
      case 32:
	ctx->options |= (unsigned long) larg;
	return ((long) ctx->options);
      case 33:
	ctx->mode |= (unsigned long) larg;
	return ((long) ctx->mode);
      default:
	tmp = (*((ctx->method)->ssl_ctx_ctrl)) (ctx, cmd, larg, parg);
	return (tmp);
      }
  }
}

long
SSL_CTX_callback_ctrl (SSL_CTX * ctx, int cmd, void (*fp) (void))
{
  long tmp;
  {
    switch (cmd)
      {
      case 15:
	ctx->msg_callback =
	  (void (*)
	   (int write_p, int version, int content_type, void const *buf,
	    size_t len, SSL * ssl, void *arg)) fp;
	return (1L);
      default:
	tmp = (*((ctx->method)->ssl_ctx_callback_ctrl)) (ctx, cmd, fp);
	return (tmp);
      }
  }
}

int
ssl_cipher_id_cmp (SSL_CIPHER const *a, SSL_CIPHER const *b)
{
  long l;
}

void
SSL_CTX_free (SSL_CTX * a)
{
  int i;
  {
    if ((unsigned int) a == (unsigned int) ((void *) 0))
      {
	return;
      }
    i = CRYPTO_add_lock (&a->references, -1, lock12, "ssl_lib.c", 1518);
    if (i > 0)
      {
	return;
      }
    if (a->param)
      {
	X509_VERIFY_PARAM_free (a->param);
      }
    if ((unsigned int) a->sessions != (unsigned int) ((void *) 0))
      {
	SSL_CTX_flush_sessions (a, 0L);
      }
    CRYPTO_free_ex_data (2, (void *) a, &a->ex_data);
    if ((unsigned int) a->sessions != (unsigned int) ((void *) 0))
      {
	lh_free (a->sessions);
      }
    if ((unsigned int) a->cert_store != (unsigned int) ((void *) 0))
      {
	X509_STORE_free (a->cert_store);
      }
    if ((unsigned int) a->cipher_list != (unsigned int) ((void *) 0))
      {
	sk_free (a->cipher_list);
      }
    if ((unsigned int) a->cipher_list_by_id != (unsigned int) ((void *) 0))
      {
	sk_free (a->cipher_list_by_id);
      }
    if ((unsigned int) a->cert != (unsigned int) ((void *) 0))
      {
	ssl_cert_free (a->cert);
      }
    if ((unsigned int) a->client_CA != (unsigned int) ((void *) 0))
      {
	sk_pop_free (a->client_CA, (void (*)(void *)) (&X509_NAME_free));
      }
    if ((unsigned int) a->extra_certs != (unsigned int) ((void *) 0))
      {
	sk_pop_free (a->extra_certs, (void (*)(void *)) (&X509_free));
      }
    a->comp_methods = (STACK *) ((void *) 0);
    CRYPTO_free ((void *) a);
    return;
  }
}

void
SSL_CTX_set_default_passwd_cb (SSL_CTX * ctx, pem_password_cb * cb)
{
}

void
ssl_set_cert_masks (CERT * c, SSL_CIPHER * cipher)
{
  CERT_PKEY *cpk;
  int dh_dsa_export;
  int rsa_tmp_export;
  int dh_tmp_export;
  int kl;
  unsigned long mask;
  unsigned long emask;
  int have_ecc_cert;
  int ecdh_ok;
  int ecdsa_ok;
  int ecc_pkey_size;
  int have_ecdh_tmp;
  X509 *x;
  EVP_PKEY *ecc_pkey;
  int signature_nid;
  int tmp;
  int tmp___0;
  int tmp___1;
  int tmp___10;
  int tmp___11;
  int tmp___12;
  int tmp___13;
  int tmp___14;
  int tmp___15;
  int tmp___16;
  int tmp___17;
  int tmp___18;
  {
    x = (X509 *) ((void *) 0);
    ecc_pkey = (EVP_PKEY *) ((void *) 0);
    signature_nid = 0;
    if ((unsigned int) c == (unsigned int) ((void *) 0))
      {
	return;
      }
    if (cipher->algo_strength & 8UL)
      {
	kl = 512;
      }
    else
      {
	kl = 1024;
      }
    if ((unsigned int) c->rsa_tmp != (unsigned int) ((void *) 0))
      {
	tmp = 1;
      }
    if (have_ecc_cert)
      {
	x = c->pkeys[5].x509;
	X509_check_purpose (x, -1, 0);
	if (x->ex_flags & 2UL)
	  {
	    ecdh_ok = (int) (x->ex_kusage & 8UL);
	  }
	else
	  {
	    ecdh_ok = 1;
	  }
	if (x->ex_flags & 2UL)
	  {
	    ecdsa_ok = (int) (x->ex_kusage & 128UL);
	  }
	else
	  {
	    ecdsa_ok = 1;
	  }
	ecc_pkey = X509_get_pubkey (x);
	if ((unsigned int) ecc_pkey != (unsigned int) ((void *) 0))
	  {
	    tmp___18 = EVP_PKEY_bits (ecc_pkey);
	    ecc_pkey_size = tmp___18;
	  }
	else
	  {
	    ecc_pkey_size = 0;
	  }
	EVP_PKEY_free (ecc_pkey);
	if (x->sig_alg)
	  {
	    if ((x->sig_alg)->algorithm)
	      {
		signature_nid =
		  OBJ_obj2nid ((ASN1_OBJECT const *) (x->sig_alg)->algorithm);
	      }
	  }
	if (ecdh_ok)
	  {
	    if (signature_nid == 8)
	      {
		goto _L;
	      }
	    else
	      {
		if (signature_nid == 396)
		  {
		    goto _L;
		  }
		else
		  {
		    if (signature_nid == 7)
		      {
		      _L:mask |= 320UL;
			if (ecc_pkey_size <= 163)
			  {
			    emask |= 320UL;
			  }
		      }
		  }
	      }
	    if (signature_nid == 416)
	      {
		mask |= 16448UL;
		if (ecc_pkey_size <= 163)
		  {
		    emask |= 16448UL;
		  }
	      }
	  }
	if (ecdsa_ok)
	  {
	    mask |= 16384UL;
	    emask |= 16384UL;
	  }
      }
    if (have_ecdh_tmp)
      {
	mask |= 128UL;
	emask |= 128UL;
      }
    c->mask = mask;
    c->export_mask = emask;
    c->valid = 1;
    return;
  }
}

int
check_srvr_ecc_cert_and_alg (X509 * x, SSL_CIPHER * cs)
{
  unsigned long alg;
  EVP_PKEY *pkey;
  int keysize;
  int signature_nid;
  {
    alg = cs->algorithms;
    pkey = (EVP_PKEY *) ((void *) 0);
    keysize = 0;
    signature_nid = 0;
    if (cs->algo_strength & 2UL)
      {
	pkey = X509_get_pubkey (x);
	if ((unsigned int) pkey == (unsigned int) ((void *) 0))
	  {
	    return (0);
	  }
	keysize = EVP_PKEY_bits (pkey);
	EVP_PKEY_free (pkey);
	if (keysize > 163)
	  {
	    return (0);
	  }
      }
    X509_check_purpose (x, -1, 0);
    if (x->sig_alg)
      {
	if ((x->sig_alg)->algorithm)
	  {
	    signature_nid =
	      OBJ_obj2nid ((ASN1_OBJECT const *) (x->sig_alg)->algorithm);
	  }
      }
    if (alg & 64UL)
      {
	if (x->ex_flags & 2UL)
	  {
	    if (!(x->ex_kusage & 8UL))
	      {
		return (0);
	      }
	  }
	if (alg & 16384UL)
	  {
	    if (signature_nid != 416)
	      {
		return (0);
	      }
	  }
	if (alg & 256UL)
	  {
	    if (signature_nid != 8)
	      {
		if (signature_nid != 396)
		  {
		    if (signature_nid != 7)
		      {
			return (0);
		      }
		  }
	      }
	  }
      }
    else
      {
	if (alg & 16384UL)
	  {
	    if (x->ex_flags & 2UL)
	      {
		if (!(x->ex_kusage & 128UL))
		  {
		    return (0);
		  }
	      }
	  }
      }
    return (1);
  }
}

X509 *
ssl_get_server_send_cert (SSL * s)
{
  unsigned long alg;
  unsigned long mask;
  unsigned long kalg;
  CERT *c;
  int i;
  int is_export;
  {
    s->server = 1;
    s->shutdown = 0;
    s->state = 24576;
    s->handshake_func = (s->method)->ssl_accept;
    ssl_clear_cipher_ctx (s);
    return;
  }
}

void
SSL_set_connect_state (SSL * s)
{
  {
    s->server = 0;
    s->shutdown = 0;
    s->state = 20480;
    s->handshake_func = (s->method)->ssl_connect;
    ssl_clear_cipher_ctx (s);
    return;
  }
}

int
ssl_undefined_function (SSL * s)
{
  {
    ERR_put_error (20, 197, 66, "ssl_lib.c", 2114);
    return (0);
  }
}

int
ssl_undefined_void_function (void)
{
}

SSL *
SSL_dup (SSL * s)
{
  STACK *sk;
  X509_NAME *xn;
  SSL *ret___3;
  int i;
  SSL_CTX *tmp;
  long tmp___0;
  int tmp___1;
  int (*tmp___2) (int, X509_STORE_CTX *);
  int tmp___3;
  int tmp___4;
  void (*tmp___5) (SSL const *ssl, int type, int val);
  int tmp___6;
  long tmp___7;
  long tmp___8;
  STACK *tmp___9;
  STACK *tmp___10;
  void *tmp___11;
  X509_NAME *tmp___12;
  void *tmp___13;
  int tmp___14;
  {
    tmp = SSL_get_SSL_CTX ((SSL const *) s);
    ret___3 = SSL_new (tmp);
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	return ((SSL *) ((void *) 0));
      }
    ret___3->version = s->version;
    ret___3->type = s->type;
    ret___3->method = s->method;
    if ((unsigned int) s->session != (unsigned int) ((void *) 0))
      {
	SSL_copy_session_id (ret___3, (SSL const *) s);
      }
    else
      {
	(*((ret___3->method)->ssl_free)) (ret___3);
	ret___3->method = s->method;
	(*((ret___3->method)->ssl_new)) (ret___3);
	if ((unsigned int) s->cert != (unsigned int) ((void *) 0))
	  {
	    if ((unsigned int) ret___3->cert != (unsigned int) ((void *) 0))
	      {
		ssl_cert_free (ret___3->cert);
	      }
	    ret___3->cert = ssl_cert_dup (s->cert);
	    if ((unsigned int) ret___3->cert == (unsigned int) ((void *) 0))
	      {
		goto err;
	      }
	  }
	SSL_set_session_id_context (ret___3,
				    (unsigned char const *) (s->sid_ctx),
				    s->sid_ctx_length);
      }
    ret___3->options = s->options;
    ret___3->mode = s->mode;
    tmp___0 = SSL_ctrl (s, 50, 0L, (void *) 0);
    SSL_ctrl (ret___3, 51, tmp___0, (void *) 0);
    ret___3->init_num = 0;
    ret___3->hit = s->hit;
    X509_VERIFY_PARAM_inherit (ret___3->param,
			       (X509_VERIFY_PARAM const *) s->param);
    if ((unsigned int) s->cipher_list != (unsigned int) ((void *) 0))
      {
	tmp___9 = sk_dup (s->cipher_list);
	ret___3->cipher_list = tmp___9;
	if ((unsigned int) tmp___9 == (unsigned int) ((void *) 0))
	  {
	    goto err;
	  }
      }
    if ((unsigned int) s->cipher_list_by_id != (unsigned int) ((void *) 0))
      {
	tmp___10 = sk_dup (s->cipher_list_by_id);
	ret___3->cipher_list_by_id = tmp___10;
	if ((unsigned int) tmp___10 == (unsigned int) ((void *) 0))
	  {
	    goto err;
	  }
      }
    if ((unsigned int) s->client_CA != (unsigned int) ((void *) 0))
      {
	sk = sk_dup (s->client_CA);
	if ((unsigned int) sk == (unsigned int) ((void *) 0))
	  {
	    goto err;
	  }
	ret___3->client_CA = sk;
	i = 0;
	while (1)
	  {
	    tmp___14 = sk_num ((STACK const *) sk);
	    if (!(i < tmp___14))
	      {
		break;
	      }
	    tmp___11 = sk_value ((STACK const *) sk, i);
	    xn = (X509_NAME *) tmp___11;
	    tmp___12 = X509_NAME_dup (xn);
	    tmp___13 = sk_set (sk, i, (void *) ((char *) tmp___12));
	    if ((unsigned int) ((X509_NAME *) tmp___13) ==
		(unsigned int) ((void *) 0))
	      {
		X509_NAME_free (xn);
		goto err;
	      }
	    i++;
	  }
      }
    if (0)
      {
      err:if ((unsigned int) ret___3 != (unsigned int) ((void *) 0))
	  {
	    SSL_free (ret___3);
	  }
	ret___3 = (SSL *) ((void *) 0);
      }
    return (ret___3);
  }
}

void
ssl_clear_cipher_ctx (SSL * s)
{
  {
    if ((unsigned int) s->enc_read_ctx != (unsigned int) ((void *) 0))
      {
	EVP_CIPHER_CTX_cleanup (s->enc_read_ctx);
	CRYPTO_free ((void *) s->enc_read_ctx);
	s->enc_read_ctx = (EVP_CIPHER_CTX *) ((void *) 0);
      }
    if ((unsigned int) s->enc_write_ctx != (unsigned int) ((void *) 0))
      {
	EVP_CIPHER_CTX_cleanup (s->enc_write_ctx);
	CRYPTO_free ((void *) s->enc_write_ctx);
	s->enc_write_ctx = (EVP_CIPHER_CTX *) ((void *) 0);
      }
    if ((unsigned int) s->expand != (unsigned int) ((void *) 0))
      {
	COMP_CTX_free (s->expand);
	s->expand = (COMP_CTX *) ((void *) 0);
      }
    if ((unsigned int) s->compress != (unsigned int) ((void *) 0))
      {
	COMP_CTX_free (s->compress);
	s->compress = (COMP_CTX *) ((void *) 0);
      }
    return;
  }
}

int
ssl_init_wbio_buffer (SSL * s, int push)
{
  BIO *bbio;
  BIO_METHOD *tmp;
  long tmp___0;
  {
    if ((unsigned int) s->bbio == (unsigned int) ((void *) 0))
      {
	tmp = BIO_f_buffer ();
	bbio = BIO_new (tmp);
	if ((unsigned int) bbio == (unsigned int) ((void *) 0))
	  {
	    return (0);
	  }
	s->bbio = bbio;
      }
    else
      {
	bbio = s->bbio;
	if ((unsigned int) s->bbio == (unsigned int) s->wbio)
	  {
	    s->wbio = BIO_pop (s->wbio);
	  }
      }
    BIO_ctrl (bbio, 1, 0L, (void *) 0);
    tmp___0 = BIO_int_ctrl (bbio, 117, 1L, 0);
    if (!tmp___0)
      {
	ERR_put_error (20, 184, 7, "ssl_lib.c", 2376);
	return (0);
      }
    if (push)
      {
	if ((unsigned int) s->wbio != (unsigned int) bbio)
	  {
	    s->wbio = BIO_push (bbio, s->wbio);
	  }
      }
    else
      {
	if ((unsigned int) s->wbio == (unsigned int) bbio)
	  {
	    s->wbio = BIO_pop (bbio);
	  }
      }
    return (1);
  }
}

void
ssl_free_wbio_buffer (SSL * s)
{
  {
    if ((unsigned int) s->bbio == (unsigned int) ((void *) 0))
      {
	return;
      }
    if ((unsigned int) s->bbio == (unsigned int) s->wbio)
      {
	s->wbio = BIO_pop (s->wbio);
      }
    BIO_free (s->bbio);
    s->bbio = (BIO *) ((void *) 0);
    return;
  }
}

void
SSL_CTX_set_tmp_rsa_callback (SSL_CTX * ctx,
			      RSA * (*cb) (SSL * ssl, int is_export,
					   int keylength))
{
  {
    SSL_CTX_callback_ctrl (ctx, 5, (void (*)(void)) cb);
    return;
  }
}

void
SSL_set_tmp_rsa_callback (SSL * ssl,
			  RSA * (*cb) (SSL * ssl, int is_export,
				       int keylength))
{
  {
    SSL_callback_ctrl (ssl, 5, (void (*)(void)) cb);
    return;
  }
}

int OPENSSL_DIR_end (OPENSSL_DIR_CTX ** ctx);
rwlock_t lock10;
rwlock_t lock15;
CERT *
ssl_cert_new (void)
{
  CERT *ret___3;
  void *tmp;
  {
    tmp = CRYPTO_malloc ((int) sizeof (CERT), "ssl_cert.c", 167);
    ret___3 = (CERT *) tmp;
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	ERR_put_error (20, 162, 65, "ssl_cert.c", 170);
	return ((CERT *) ((void *) 0));
      }
    memset ((void *) ret___3, 0, sizeof (CERT));
    ret___3->key = &ret___3->pkeys[0];
    ret___3->references = 1;
    return (ret___3);
  }
}

CERT *
ssl_cert_dup (CERT * cert)
{
  CERT *ret___3;
  int i;
  void *tmp;
  void *tmp___0;
  BIGNUM *b;
  BIGNUM *tmp___1;
  BIGNUM *b___0;
  BIGNUM *tmp___2;
  {
    tmp = CRYPTO_malloc ((int) sizeof (CERT), "ssl_cert.c", 186);
    ret___3 = (CERT *) tmp;
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	ERR_put_error (20, 221, 65, "ssl_cert.c", 189);
	return ((CERT *) ((void *) 0));
      }
    memset ((void *) ret___3, 0, sizeof (CERT));
    ret___3->key = &ret___3->pkeys[cert->key - &cert->pkeys[0]];
    ret___3->valid = cert->valid;
    ret___3->mask = cert->mask;
    ret___3->export_mask = cert->export_mask;
    if ((unsigned int) cert->rsa_tmp != (unsigned int) ((void *) 0))
      {
	RSA_up_ref (cert->rsa_tmp);
	ret___3->rsa_tmp = cert->rsa_tmp;
      }
    ret___3->rsa_tmp_cb = cert->rsa_tmp_cb;
    if ((unsigned int) cert->dh_tmp != (unsigned int) ((void *) 0))
      {
	tmp___0 =
	  ASN1_dup ((i2d_of_void *) (&i2d_DHparams),
		    (d2i_of_void *) (&d2i_DHparams),
		    (char *) ((void *) ((DH const *) cert->dh_tmp)));
	ret___3->dh_tmp = (DH *) tmp___0;
	if ((unsigned int) ret___3->dh_tmp == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 221, 5, "ssl_cert.c", 218);
	    goto err;
	  }
	if ((cert->dh_tmp)->priv_key)
	  {
	    tmp___1 = BN_dup ((BIGNUM const *) (cert->dh_tmp)->priv_key);
	    b = tmp___1;
	    if (!b)
	      {
		ERR_put_error (20, 221, 3, "ssl_cert.c", 226);
		goto err;
	      }
	    (ret___3->dh_tmp)->priv_key = b;
	  }
	if ((cert->dh_tmp)->pub_key)
	  {
	    tmp___2 = BN_dup ((BIGNUM const *) (cert->dh_tmp)->pub_key);
	    b___0 = tmp___2;
	    if (!b___0)
	      {
		ERR_put_error (20, 221, 3, "ssl_cert.c", 236);
		goto err;
	      }
	    (ret___3->dh_tmp)->pub_key = b___0;
	  }
      }
    ret___3->dh_tmp_cb = cert->dh_tmp_cb;
    if (cert->ecdh_tmp)
      {
	ret___3->ecdh_tmp = EC_KEY_dup ((EC_KEY const *) cert->ecdh_tmp);
	if ((unsigned int) ret___3->ecdh_tmp == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 221, 16, "ssl_cert.c", 251);
	    goto err;
	  }
      }
    ret___3->ecdh_tmp_cb = cert->ecdh_tmp_cb;
    i = 0;
    while (i < 6)
      {
	if ((unsigned int) cert->pkeys[i].x509 != (unsigned int) ((void *) 0))
	  {
	    ret___3->pkeys[i].x509 = cert->pkeys[i].x509;
	    CRYPTO_add_lock (&(ret___3->pkeys[i].x509)->references, 1, lock3,
			     "ssl_cert.c", 264);
	  }
	if ((unsigned int) cert->pkeys[i].privatekey !=
	    (unsigned int) ((void *) 0))
	  {
	    ret___3->pkeys[i].privatekey = cert->pkeys[i].privatekey;
	    CRYPTO_add_lock (&(ret___3->pkeys[i].privatekey)->references, 1,
			     lock10, "ssl_cert.c", 271);
	    switch (i)
	      {
	      case 0:
	      case 1:
		break;
	      case 2:
		break;
	      case 3:
	      case 4:
		break;
	      case 5:
		break;
	      default:
		ERR_put_error (20, 221, 274, "ssl_cert.c", 299);
	      }
	  }
	i++;
      }
    ret___3->references = 1;
    return (ret___3);
  err:if ((unsigned int) ret___3->rsa_tmp != (unsigned int) ((void *) 0))
      {
	RSA_free (ret___3->rsa_tmp);
      }
    if ((unsigned int) ret___3->dh_tmp != (unsigned int) ((void *) 0))
      {
	DH_free (ret___3->dh_tmp);
      }
    if ((unsigned int) ret___3->ecdh_tmp != (unsigned int) ((void *) 0))
      {
	EC_KEY_free (ret___3->ecdh_tmp);
      }
    i = 0;
    while (i < 6)
      {
	if ((unsigned int) ret___3->pkeys[i].x509 !=
	    (unsigned int) ((void *) 0))
	  {
	    X509_free (ret___3->pkeys[i].x509);
	  }
	if ((unsigned int) ret___3->pkeys[i].privatekey !=
	    (unsigned int) ((void *) 0))
	  {
	    EVP_PKEY_free (ret___3->pkeys[i].privatekey);
	  }
	i++;
      }
    return ((CERT *) ((void *) 0));
    ret___3->references = 1;
    return (ret___3);
  }
}

void
ssl_sess_cert_free (SESS_CERT * sc)
{
  int i;
  {
    if ((unsigned int) sc == (unsigned int) ((void *) 0))
      {
	return;
      }
    i = CRYPTO_add_lock (&sc->references, -1, lock15, "ssl_cert.c", 437);
    if (i > 0)
      {
	return;
      }
    if ((unsigned int) sc->cert_chain != (unsigned int) ((void *) 0))
      {
	sk_pop_free (sc->cert_chain, (void (*)(void *)) (&X509_free));
      }
    i = 0;
    while (i < 6)
      {
	if ((unsigned int) sc->peer_pkeys[i].x509 !=
	    (unsigned int) ((void *) 0))
	  {
	    X509_free (sc->peer_pkeys[i].x509);
	  }
	i++;
      }
    if ((unsigned int) sc->peer_rsa_tmp != (unsigned int) ((void *) 0))
      {
	RSA_free (sc->peer_rsa_tmp);
      }
    if ((unsigned int) sc->peer_dh_tmp != (unsigned int) ((void *) 0))
      {
	DH_free (sc->peer_dh_tmp);
      }
  }
}

void *SSL_SESSION_get_ex_data (SSL_SESSION const *s, int idx);
int SSL_SESSION_get_ex_new_index (long argl, void *argp,
				  CRYPTO_EX_new * new_func,
				  CRYPTO_EX_dup * dup_func,
				  CRYPTO_EX_free * free_func___0);
static void SSL_SESSION_list_remove (SSL_CTX * ctx, SSL_SESSION * s);
static void SSL_SESSION_list_add (SSL_CTX * ctx, SSL_SESSION * s);
static int remove_session_lock (SSL_CTX * ctx, SSL_SESSION * c, int lck);
static int
def_generate_session_id (SSL const *ssl, unsigned char *id,
			 unsigned int *id_len)
{
  unsigned int retry;
  int tmp;
  int tmp___0;
  {
    retry = 0U;
    while (1)
      {
	tmp = RAND_pseudo_bytes (id, (int) *id_len);
	if (tmp <= 0)
	  {
	    return (0);
	  }
	tmp___0 =
	  SSL_has_matching_session_id (ssl, (unsigned char const *) id,
				       *id_len);
	if (tmp___0)
	  {
	    retry++;
	    if (!(retry < 10U))
	      {
		break;
	      }
	  }
	else
	  {
	    break;
	  }
      }
    if (retry < 10U)
      {
	return (1);
      }
    return (0);
  }
}

int
ssl_get_new_session (SSL * s, int session)
{
  unsigned int tmp;
  SSL_SESSION *ss;
  int (*cb) (SSL const *ssl, unsigned char *id, unsigned int *id_len);
  int tmp___0;
  int tmp___1;
  {
    ss = (SSL_SESSION *) ((void *) 0);
    cb = &def_generate_session_id;
    ss = SSL_SESSION_new ();
    if ((unsigned int) ss == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    if ((s->ctx)->session_timeout == 0L)
      {
	ss->timeout = SSL_get_default_timeout ((SSL const *) s);
      }
    else
      {
	ss->timeout = (s->ctx)->session_timeout;
      }
    if ((unsigned int) s->session != (unsigned int) ((void *) 0))
      {
	SSL_SESSION_free (s->session);
	s->session = (SSL_SESSION *) ((void *) 0);
      }
    if (session)
      {
	if (s->version == 2)
	  {
	    ss->ssl_version = 2;
	    ss->session_id_length = 16U;
	  }
	else
	  {
	    if (s->version == 768)
	      {
		ss->ssl_version = 768;
		ss->session_id_length = 32U;
	      }
	    else
	      {
		if (s->version == 769)
		  {
		    ss->ssl_version = 769;
		    ss->session_id_length = 32U;
		  }
		else
		  {
		    if (s->version == 65279)
		      {
			ss->ssl_version = 65279;
			ss->session_id_length = 32U;
		      }
		    else
		      {
			ERR_put_error (20, 181, 259, "ssl_sess.c", 218);
			SSL_SESSION_free (ss);
			return (0);
		      }
		  }
	      }
	  }
	_read_lock (&lock12);
	if (s->generate_session_id)
	  {
	    cb = s->generate_session_id;
	  }
	else
	  {
	    if ((s->ctx)->generate_session_id)
	      {
		cb = (s->ctx)->generate_session_id;
	      }
	  }
	_read_unlock (&lock12);
	tmp = ss->session_id_length;
	tmp___0 = (*cb) ((SSL const *) s, ss->session_id, &tmp);
	if (!tmp___0)
	  {
	    ERR_put_error (20, 181, 301, "ssl_sess.c", 243);
	    SSL_SESSION_free (ss);
	    return (0);
	  }
	if (!tmp)
	  {
	    ERR_put_error (20, 181, 303, "ssl_sess.c", 253);
	    SSL_SESSION_free (ss);
	    return (0);
	  }
	else
	  {
	    if (tmp > ss->session_id_length)
	      {
		ERR_put_error (20, 181, 303, "ssl_sess.c", 253);
		SSL_SESSION_free (ss);
		return (0);
	      }
	  }
	if (tmp < ss->session_id_length)
	  {
	    if (s->version == 2)
	      {
		memset ((void *) (ss->session_id + tmp), 0,
			ss->session_id_length - tmp);
	      }
	    else
	      {
		ss->session_id_length = tmp;
	      }
	  }
	else
	  {
	    ss->session_id_length = tmp;
	  }
	tmp___1 =
	  SSL_has_matching_session_id ((SSL const *) s,
				       (unsigned char const *) (ss->
								session_id),
				       ss->session_id_length);
	if (tmp___1)
	  {
	    ERR_put_error (20, 181, 302, "ssl_sess.c", 267);
	    SSL_SESSION_free (ss);
	    return (0);
	  }
      }
    else
      {
	ss->session_id_length = 0U;
      }
    if (s->sid_ctx_length > sizeof (ss->sid_ctx))
      {
	ERR_put_error (20, 181, 68, "ssl_sess.c", 290);
	SSL_SESSION_free (ss);
	return (0);
      }
    memcpy ((void *__restrict) (ss->sid_ctx),
	    (void const *__restrict) (s->sid_ctx), s->sid_ctx_length);
    ss->sid_ctx_length = s->sid_ctx_length;
    s->session = ss;
    ss->ssl_version = s->version;
    ss->verify_result = 0L;
    return (1);
  }
}

int
ssl_get_prev_session (SSL * s, unsigned char *session_id, int len,
		      unsigned char const *limit)
{
  SSL_SESSION *ret___3;
  int fatal;
  SSL_SESSION data___0;
  void *tmp;
  int copy;
  int tmp___0;
  unsigned char buf___5[5];
  unsigned char *p;
  unsigned long l;
  unsigned char *tmp___1;
  unsigned char *tmp___2;
  unsigned char *tmp___3;
  unsigned char *tmp___4;
  time_t tmp___5;
  {
    ret___3 = (SSL_SESSION *) ((void *) 0);
    tmp___5 = time ((time_t *) ((void *) 0));
    if (ret___3->timeout < tmp___5 - ret___3->time)
      {
	((s->ctx)->stats.sess_timeout)++;
	SSL_CTX_remove_session (s->ctx, ret___3);
	goto err;
      }
    ((s->ctx)->stats.sess_hit)++;
    if ((unsigned int) s->session != (unsigned int) ((void *) 0))
      {
	SSL_SESSION_free (s->session);
      }
    s->session = ret___3;
    s->verify_result = (s->session)->verify_result;
    return (1);
  err:if ((unsigned int) ret___3 != (unsigned int) ((void *) 0))
      {
	SSL_SESSION_free (ret___3);
      }
    if (fatal)
      {
	return (-1);
      }
    else
      {
	return (0);
      }
  }
}

int
SSL_CTX_add_session (SSL_CTX * ctx, SSL_SESSION * c)
{
  int ret___3;
  SSL_SESSION *s;
  void *tmp;
  int tmp___0;
  long tmp___1;
  long tmp___2;
  long tmp___3;
  {
    ret___3 = 0;
    CRYPTO_add_lock (&c->references, 1, lock14, "ssl_sess.c", 473);
    _write_lock (&lock12);
    tmp = lh_insert (ctx->sessions, (void *) c);
    s = (SSL_SESSION *) tmp;
    if ((unsigned int) s != (unsigned int) ((void *) 0))
      {
	if ((unsigned int) s != (unsigned int) c)
	  {
	    SSL_SESSION_list_remove (ctx, s);
	    SSL_SESSION_free (s);
	    s = (SSL_SESSION *) ((void *) 0);
	  }
      }
    if ((unsigned int) s == (unsigned int) ((void *) 0))
      {
	SSL_SESSION_list_add (ctx, c);
      }
    if ((unsigned int) s != (unsigned int) ((void *) 0))
      {
	SSL_SESSION_free (s);
	ret___3 = 0;
      }
    else
      {
	ret___3 = 1;
	tmp___3 = SSL_CTX_ctrl (ctx, 43, 0L, (void *) 0);
	if (tmp___3 > 0L)
	  {
	    while (1)
	      {
		tmp___1 = SSL_CTX_ctrl (ctx, 20, 0L, (void *) 0);
		tmp___2 = SSL_CTX_ctrl (ctx, 43, 0L, (void *) 0);
		if (!(tmp___1 > tmp___2))
		  {
		    break;
		  }
		tmp___0 =
		  remove_session_lock (ctx, ctx->session_cache_tail, 0);
		if (tmp___0)
		  {
		    (ctx->stats.sess_cache_full)++;
		  }
		else
		  {
		    break;
		  }
	      }
	  }
      }
    _write_unlock (&lock12);
    return (ret___3);
  }
}

int
SSL_CTX_remove_session (SSL_CTX * ctx, SSL_SESSION * c)
{
  int tmp;
  {
    tmp = remove_session_lock (ctx, c, 1);
    return (tmp);
  }
}

static int
remove_session_lock (SSL_CTX * ctx, SSL_SESSION * c, int lck)
{
  SSL_SESSION *r;
  int ret___3;
  void *tmp;
  void *tmp___0;
  {
    ret___3 = 0;
    if ((unsigned int) c != (unsigned int) ((void *) 0))
      {
	if (c->session_id_length != 0U)
	  {
	    if (lck)
	      {
		_write_lock (&lock12);
	      }
	    tmp___0 = lh_retrieve (ctx->sessions, (void const *) c);
	    r = (SSL_SESSION *) tmp___0;
	    if ((unsigned int) r == (unsigned int) c)
	      {
		ret___3 = 1;
		tmp = lh_delete (ctx->sessions, (void const *) c);
		r = (SSL_SESSION *) tmp;
		SSL_SESSION_list_remove (ctx, c);
	      }
	    if (lck)
	      {
		_write_unlock (&lock12);
	      }
	    if (ret___3)
	      {
		r->not_resumable = 1;
		if ((unsigned int) ctx->remove_session_cb !=
		    (unsigned int) ((void *) 0))
		  {
		    (*(ctx->remove_session_cb)) (ctx, r);
		  }
		SSL_SESSION_free (r);
	      }
	  }
	else
	  {
	    ret___3 = 0;
	  }
      }
    else
      {
	ret___3 = 0;
      }
    return (ret___3);
  }
}

void
SSL_SESSION_free (SSL_SESSION * ss)
{
  int i;
  {
    if ((unsigned int) ss == (unsigned int) ((void *) 0))
      {
	return;
      }
    i = CRYPTO_add_lock (&ss->references, -1, lock14, "ssl_sess.c", 572);
    if (i > 0)
      {
	return;
      }
    CRYPTO_free_ex_data (3, (void *) ss, &ss->ex_data);
    OPENSSL_cleanse ((void *) (ss->key_arg), sizeof (ss->key_arg));
    OPENSSL_cleanse ((void *) (ss->master_key), sizeof (ss->master_key));
    OPENSSL_cleanse ((void *) (ss->session_id), sizeof (ss->session_id));
    if ((unsigned int) ss->sess_cert != (unsigned int) ((void *) 0))
      {
	ssl_sess_cert_free (ss->sess_cert);
      }
    if ((unsigned int) ss->peer != (unsigned int) ((void *) 0))
      {
	X509_free (ss->peer);
      }
    if ((unsigned int) ss->ciphers != (unsigned int) ((void *) 0))
      {
	sk_free (ss->ciphers);
      }
    OPENSSL_cleanse ((void *) ss, sizeof (*ss));
    CRYPTO_free ((void *) ss);
    return;
  }
}

int
SSL_set_session (SSL * s, SSL_SESSION * session)
{
  int ret___3;
  SSL_METHOD *meth___0;
  int tmp;
  int tmp___0;
  {
    ret___3 = 0;
    if ((unsigned int) session != (unsigned int) ((void *) 0))
      {
	meth___0 =
	  (*(((s->ctx)->method)->get_ssl_method)) (session->ssl_version);
	if ((unsigned int) meth___0 == (unsigned int) ((void *) 0))
	  {
	    meth___0 =
	      (*((s->method)->get_ssl_method)) (session->ssl_version);
	  }
	if ((unsigned int) meth___0 == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (20, 195, 240, "ssl_sess.c", 613);
	    return (0);
	  }
	if ((unsigned int) meth___0 != (unsigned int) s->method)
	  {
	    tmp = SSL_set_ssl_method (s, meth___0);
	    if (!tmp)
	      {
		return (0);
	      }
	    if ((s->ctx)->session_timeout == 0L)
	      {
		session->timeout = SSL_get_default_timeout ((SSL const *) s);
	      }
	    else
	      {
		session->timeout = (s->ctx)->session_timeout;
	      }
	  }
	CRYPTO_add_lock (&session->references, 1, lock14, "ssl_sess.c", 639);
	if ((unsigned int) s->session != (unsigned int) ((void *) 0))
	  {
	    SSL_SESSION_free (s->session);
	  }
	s->session = session;
	s->verify_result = (s->session)->verify_result;
	ret___3 = 1;
      }
    else
      {
	if ((unsigned int) s->session != (unsigned int) ((void *) 0))
	  {
	    SSL_SESSION_free (s->session);
	    s->session = (SSL_SESSION *) ((void *) 0);
	  }
	meth___0 = (s->ctx)->method;
	if ((unsigned int) meth___0 != (unsigned int) s->method)
	  {
	    tmp___0 = SSL_set_ssl_method (s, meth___0);
	    if (!tmp___0)
	      {
		return (0);
	      }
	  }
	ret___3 = 1;
      }
    return (ret___3);
  }
}

long
SSL_SESSION_set_timeout (SSL_SESSION * s, long t)
{
  {
    return ((long) s->timeout);
  }
}

static void
timeout (SSL_SESSION * s, TIMEOUT_PARAM * p)
{
  {
    if (p->time == 0L)
      {
	goto _L;
      }
    else
      {
	if (p->time > s->time + s->timeout)
	  {
	  _L:lh_delete (p->cache, (void const *) s);
	    SSL_SESSION_list_remove (p->ctx, s);
	    s->not_resumable = 1;
	    if ((unsigned int) (p->ctx)->remove_session_cb !=
		(unsigned int) ((void *) 0))
	      {
		(*((p->ctx)->remove_session_cb)) (p->ctx, s);
	      }
	    SSL_SESSION_free (s);
	  }
      }
    return;
  }
}

static void
SSL_SESSION_list_remove (SSL_CTX * ctx, SSL_SESSION * s)
{
  struct ssl_session_st *tmp;
  {
    if ((unsigned int) s->next == (unsigned int) ((void *) 0))
      {
	return;
      }
    else
      {
	if ((unsigned int) s->prev == (unsigned int) ((void *) 0))
	  {
	    return;
	  }
      }
    if ((unsigned int) s->next ==
	(unsigned int) ((SSL_SESSION *) (&ctx->session_cache_tail)))
      {
	if ((unsigned int) s->prev ==
	    (unsigned int) ((SSL_SESSION *) (&ctx->session_cache_head)))
	  {
	    ctx->session_cache_head = (struct ssl_session_st *) ((void *) 0);
	    ctx->session_cache_tail = (struct ssl_session_st *) ((void *) 0);
	  }
	else
	  {
	    ctx->session_cache_tail = s->prev;
	    (s->prev)->next = (SSL_SESSION *) (&ctx->session_cache_tail);
	  }
      }
    else
      {
	if ((unsigned int) s->prev ==
	    (unsigned int) ((SSL_SESSION *) (&ctx->session_cache_head)))
	  {
	    ctx->session_cache_head = s->next;
	    (s->next)->prev = (SSL_SESSION *) (&ctx->session_cache_head);
	  }
	else
	  {
	    (s->next)->prev = s->prev;
	    (s->prev)->next = s->next;
	  }
      }
    tmp = (struct ssl_session_st *) ((void *) 0);
    s->next = tmp;
    s->prev = tmp;
    return;
  }
}

void
SSL_CTX_set_info_callback (SSL_CTX * ctx,
			   void (*cb) (SSL const *ssl, int type, int val))
{
  {
    ctx->info_callback = cb;
    return;
  }
}

void (*SSL_CTX_get_info_callback (SSL_CTX * ctx)) (SSL const *ssl, int type,
						   int val)
{
  {
    return (ctx->info_callback);
  }
}

char const *SSL_COMP_get_name (COMP_METHOD const *comp);
void ssl_load_ciphers (void);
static EVP_CIPHER const *ssl_cipher_methods[12] = {
  (EVP_CIPHER const *) ((void *) 0), (EVP_CIPHER const *) ((void *) 0),
    (EVP_CIPHER const *) ((void *) 0), (EVP_CIPHER const *) ((void *) 0),
    (EVP_CIPHER const *) ((void *) 0), (EVP_CIPHER const *) ((void *) 0),
    (EVP_CIPHER const *) 0, (EVP_CIPHER const *) 0, (EVP_CIPHER const *) 0,
    (EVP_CIPHER const *) 0, (EVP_CIPHER const *) 0, (EVP_CIPHER const *) 0
};

static STACK *ssl_comp_methods = (STACK *) ((void *) 0);
static EVP_MD const *ssl_digest_methods[2] = {
  (EVP_MD const *) ((void *) 0), (EVP_MD const *) ((void *) 0)
};

static SSL_CIPHER const cipher_aliases[47] = {
  {
   0, "LOW", 0UL, 0UL, 32UL, 0UL, 0, 0, 0UL, 252UL}
  , {
     0, "MEDIUM", 0UL, 0UL, 64UL, 0UL, 0, 0, 0UL, 252UL}
  , {
     0, "HIGH", 0UL, 0UL, 128UL, 0UL, 0, 0, 0UL, 252UL}
};

static int
sk_comp_cmp (SSL_COMP const *const *a, SSL_COMP const *const *b)
{
  {
    return ((int) ((*a)->id - (*b)->id));
  }
}

static void
load_builtin_compressions (void)
{
}

int
ssl_cipher_get_evp (SSL_SESSION const *s, EVP_CIPHER const **enc,
		    EVP_MD const **md___0, SSL_COMP ** comp)
{
  int i;
  SSL_CIPHER *c;
  SSL_COMP ctmp;
  void *tmp;
  {
    c = (SSL_CIPHER *) s->cipher;
    if ((unsigned int) c == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    if ((unsigned int) comp != (unsigned int) ((void *) 0))
      {
	load_builtin_compressions ();
	*comp = (SSL_COMP *) ((void *) 0);
	ctmp.id = (int) s->compress_meth;
	if ((unsigned int) ssl_comp_methods != (unsigned int) ((void *) 0))
	  {
	    i = sk_find (ssl_comp_methods, (void *) ((char *) (&ctmp)));
	    if (i >= 0)
	      {
		tmp = sk_value ((STACK const *) ssl_comp_methods, i);
		*comp = (SSL_COMP *) tmp;
	      }
	    else
	      {
		*comp = (SSL_COMP *) ((void *) 0);
	  }}
      }
    if ((unsigned int) enc == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    else
      {
	if ((unsigned int) md___0 == (unsigned int) ((void *) 0))
	  {
	    return (0);
	  }
      }
    switch ((int) (c->algorithms & 473923584UL))
      {
      case 32768L:
	i = 0;
	break;
      case 65536L:
	i = 1;
	break;
      case 131072L:
	i = 2;
	break;
      case 262144L:
	i = 3;
	break;
      case 524288L:
	i = 4;
	break;
      case 2097152L:
	i = 6;
	break;
      case 67108864L:
	switch (c->alg_bits)
	  {
	  case 128:
	    i = 7;
	    break;
	  case 256:
	    i = 8;
	    break;
	  default:
	    i = -1;
	    break;
	  }
	break;
      case 134217728L:
	switch (c->alg_bits)
	  {
	  case 128:
	    i = 9;
	    break;
	  case 256:
	    i = 10;
	    break;
	  default:
	    i = -1;
	    break;
	  }
	break;
      case 268435456L:
	i = 11;
	break;
      default:
	i = -1;
	break;
      }
    if (i < 0)
      {
	*enc = (EVP_CIPHER const *) ((void *) 0);
      }
    else
      {
	if (i > 12)
	  {
	    *enc = (EVP_CIPHER const *) ((void *) 0);
	  }
	else
	  {
	    if (i == 6)
	      {
		*enc = EVP_enc_null ();
	      }
	    else
	      {
		*enc = ssl_cipher_methods[i];
	      }
	  }
      }
    switch ((int) (c->algorithms & 12582912UL))
      {
      case 4194304L:
	i = 0;
	break;
      case 8388608L:
	i = 1;
	break;
      default:
	i = -1;
	break;
      }
    if (i < 0)
      {
	*md___0 = (EVP_MD const *) ((void *) 0);
      }
    else
      {
	if (i > 2)
	  {
	    *md___0 = (EVP_MD const *) ((void *) 0);
	  }
	else
	  {
	    *md___0 = ssl_digest_methods[i];
	  }
      }
    if ((unsigned int) *enc != (unsigned int) ((void *) 0))
      {
	if ((unsigned int) *md___0 != (unsigned int) ((void *) 0))
	  {
	    return (1);
	  }
	else
	  {
	    return (0);
	  }
      }
    else
      {
	return (0);
      }
  }
}

#pragma merger(0,"/tmp/cil-J0KlSqV1.i","-march=pentium,-O3,-fomit-frame-pointer,-Wall")
rwlock_t lock21;
long BIO_callback_ctrl (BIO * b, int cmd,
			void (*fp) (struct bio_st *, int, char const *, int,
				    long, long));
BIO_METHOD *BIO_s_connect (void);
void BIO_copy_next_retry (BIO * b);
BIO_METHOD *BIO_f_ssl (void);
BIO *BIO_new_ssl (SSL_CTX * ctx, int client___0);
BIO *BIO_new_ssl_connect (SSL_CTX * ctx);
BIO *BIO_new_buffer_ssl_connect (SSL_CTX * ctx);
static int ssl_write (BIO * b, char const *out, int outl);
static int ssl_read (BIO * b, char *out, int outl);
static int ssl_puts (BIO * bp, char const *str);
static long ssl_ctrl (BIO * b, int cmd, long num, void *ptr);
static int ssl_new (BIO * bi);
static int ssl_free (BIO * a);
static long ssl_callback_ctrl (BIO * b, int cmd, bio_info_cb * fp);
static BIO_METHOD methods_sslp = {
  519, "ssl", &ssl_write, &ssl_read, &ssl_puts,
    (int (*)(BIO *, char *, int)) ((void *) 0), &ssl_ctrl, &ssl_new,
    &ssl_free, &ssl_callback_ctrl
};

BIO_METHOD *
BIO_f_ssl (void)
{
  {
    return (&methods_sslp);
  }
}

static int
ssl_new (BIO * bi)
{
  BIO_SSL *bs;
  void *tmp;
  {
    tmp = CRYPTO_malloc ((int) sizeof (BIO_SSL), "bio_ssl.c", 108);
    bs = (BIO_SSL *) tmp;
    if ((unsigned int) bs == (unsigned int) ((void *) 0))
      {
	ERR_put_error (32, 118, 65, "bio_ssl.c", 111);
	return (0);
      }
    memset ((void *) bs, 0, sizeof (BIO_SSL));
    bi->init = 0;
    bi->ptr = (void *) ((char *) bs);
    bi->flags = 0;
    return (1);
  }
}

static int
ssl_free (BIO * a)
{
  BIO_SSL *bs;
  {
    if ((unsigned int) a == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    bs = (BIO_SSL *) a->ptr;
    if ((unsigned int) bs->ssl != (unsigned int) ((void *) 0))
      {
	SSL_shutdown (bs->ssl);
      }
    if (a->shutdown)
      {
	if (a->init)
	  {
	    if ((unsigned int) bs->ssl != (unsigned int) ((void *) 0))
	      {
		SSL_free (bs->ssl);
	      }
	  }
	a->init = 0;
	a->flags = 0;
      }
    if ((unsigned int) a->ptr != (unsigned int) ((void *) 0))
      {
	CRYPTO_free (a->ptr);
      }
    return (1);
  }
}

static int
ssl_read (BIO * b, char *out, int outl)
{
  int ret___3;
  BIO_SSL *sb;
  SSL *ssl;
  int retry_reason;
  int r;
  int tmp;
  unsigned long tm;
  time_t tmp___0;
  {
    ret___3 = 1;
    retry_reason = 0;
    b->retry_reason = retry_reason;
    return (ret___3);
  }
}

static int
ssl_write (BIO * b, char const *out, int outl)
{
  int ret___3;
  int r;
  int retry_reason;
  SSL *ssl;
}

static long
ssl_ctrl (BIO * b, int cmd, long num, void *ptr)
{
  SSL **sslp;
  SSL *ssl;
  BIO_SSL *bs;
  BIO *dbio;
  BIO *bio;
  long ret___3;
  time_t tmp;
  int tmp___0;
  long tmp___1;
  int tmp___2;
  int tmp___3;
  void (**fptr) (SSL const *xssl, int type, int val);
  {
    ret___3 = 1L;
    bs = (BIO_SSL *) b->ptr;
    ssl = bs->ssl;
    if ((unsigned int) ssl == (unsigned int) ((void *) 0))
      {
	if (cmd != 109)
	  {
	    return (0L);
	  }
      }
    switch (cmd)
      {
      case 1:
	SSL_shutdown (ssl);
	if ((unsigned int) ssl->handshake_func ==
	    (unsigned int) (ssl->method)->ssl_connect)
	  {
	    SSL_set_connect_state (ssl);
	  }
	else
	  {
	    if ((unsigned int) ssl->handshake_func ==
		(unsigned int) (ssl->method)->ssl_accept)
	      {
		SSL_set_accept_state (ssl);
	      }
	  } SSL_clear (ssl);
	if ((unsigned int) b->next_bio != (unsigned int) ((void *) 0))
	  {
	    ret___3 = BIO_ctrl (b->next_bio, cmd, num, ptr);
	  }
	else
	  {
	    if ((unsigned int) ssl->rbio != (unsigned int) ((void *) 0))
	      {
		ret___3 = BIO_ctrl (ssl->rbio, cmd, num, ptr);
	      }
	    else
	      {
		ret___3 = 1L;
	      }
	  }
	break;
      case 3:
	ret___3 = 0L;
	break;
      case 119:
	if (num)
	  {
	    SSL_set_connect_state (ssl);
	  }
	else
	  {
	    SSL_set_accept_state (ssl);
	  }
	break;
      case 127:
	ret___3 = (long) bs->renegotiate_timeout;
	if (num < 60L)
	  {
	    num = 5L;
	  }
	bs->renegotiate_timeout = (unsigned long) num;
	tmp = time ((time_t *) ((void *) 0));
	bs->last_time = (unsigned long) tmp;
	break;
      case 125:
	ret___3 = (long) bs->renegotiate_count;
	if (num >= 512L)
	  {
	    bs->renegotiate_count = (unsigned long) num;
	  }
	break;
      case 126:
	ret___3 = (long) bs->num_renegotiates;
	break;
      case 109:
	if ((unsigned int) ssl != (unsigned int) ((void *) 0))
	  {
	    ssl_free (b);
	  }
	b->shutdown = (int) num;
	ssl = (SSL *) ptr;
	((BIO_SSL *) b->ptr)->ssl = ssl;
	bio = SSL_get_rbio ((SSL const *) ssl);
	if ((unsigned int) bio != (unsigned int) ((void *) 0))
	  {
	    if ((unsigned int) b->next_bio != (unsigned int) ((void *) 0))
	      {
		BIO_push (bio, b->next_bio);
	      }
	    b->next_bio = bio;
	    CRYPTO_add_lock (&bio->references, 1, lock21, "bio_ssl.c", 361);
	  }
	b->init = 1;
	break;
      case 110:
	if ((unsigned int) ptr != (unsigned int) ((void *) 0))
	  {
	    sslp = (SSL **) ptr;
	    *sslp = ssl;
	  }
	else
	  {
	    ret___3 = 0L;
	  }
	break;
      case 8:
	ret___3 = (long) b->shutdown;
	break;
      case 9:
	b->shutdown = (int) num;
	break;
      case 13:
	ret___3 = BIO_ctrl (ssl->wbio, cmd, num, ptr);
	break;
      case 10:
	tmp___0 = SSL_pending ((SSL const *) ssl);
	ret___3 = (long) tmp___0;
	if (ret___3 == 0L)
	  {
	    tmp___1 = BIO_ctrl (ssl->rbio, 10, 0L, (void *) 0);
	    ret___3 = (long) ((int) tmp___1);
	  }
	break;
      case 11:
	BIO_clear_flags (b, 15);
	ret___3 = BIO_ctrl (ssl->wbio, cmd, num, ptr);
	BIO_copy_next_retry (b);
	break;
      case 6:
	if ((unsigned int) b->next_bio != (unsigned int) ((void *) 0))
	  {
	    if ((unsigned int) b->next_bio != (unsigned int) ssl->rbio)
	      {
		SSL_set_bio (ssl, b->next_bio, b->next_bio);
		CRYPTO_add_lock (&(b->next_bio)->references, 1, lock21,
				 "bio_ssl.c", 397);
	      }
	  }
	break;
      case 7:
	if ((unsigned int) ssl->rbio != (unsigned int) ssl->wbio)
	  {
	    BIO_free_all (ssl->wbio);
	  }
	if ((unsigned int) b->next_bio != (unsigned int) ((void *) 0))
	  {
	    CRYPTO_add_lock (&(b->next_bio)->references, 1, lock21,
			     "bio_ssl.c", 408);
	  }
	ssl->wbio = (BIO *) ((void *) 0);
	ssl->rbio = (BIO *) ((void *) 0);
	break;
      case 101:
	BIO_clear_flags (b, 15);
	b->retry_reason = 0;
	tmp___2 = SSL_do_handshake (ssl);
	ret___3 = (long) tmp___2;
	tmp___3 = SSL_get_error ((SSL const *) ssl, (int) ret___3);
	switch (tmp___3)
	  {
	  case 2:
	    BIO_set_flags (b, 9);
	    break;
	  case 3:
	    BIO_set_flags (b, 10);
	    break;
	  case 7:
	    BIO_set_flags (b, 12);
	    b->retry_reason = (b->next_bio)->retry_reason;
	    break;
	  default:;
	    break;
	  }
	break;
      case 12:
	dbio = (BIO *) ptr;
	if ((unsigned int) ((BIO_SSL *) dbio->ptr)->ssl !=
	    (unsigned int) ((void *) 0))
	  {
	    SSL_free (((BIO_SSL *) dbio->ptr)->ssl);
	  }
	((BIO_SSL *) dbio->ptr)->ssl = SSL_dup (ssl);
	((BIO_SSL *) dbio->ptr)->renegotiate_count =
	  ((BIO_SSL *) b->ptr)->renegotiate_count;
	((BIO_SSL *) dbio->ptr)->byte_count =
	  ((BIO_SSL *) b->ptr)->byte_count;
	((BIO_SSL *) dbio->ptr)->renegotiate_timeout =
	  ((BIO_SSL *) b->ptr)->renegotiate_timeout;
	((BIO_SSL *) dbio->ptr)->last_time = ((BIO_SSL *) b->ptr)->last_time;
	ret___3 =
	  (long) ((unsigned int) ((BIO_SSL *) dbio->ptr)->ssl !=
		  (unsigned int) ((void *) 0));
	break;
      case 105:
	ret___3 = BIO_ctrl (ssl->rbio, cmd, num, ptr);
	break;
      case 14:
	ret___3 = 0L;
	break;
      case 15:
	fptr = (void (**)(SSL const *xssl, int type, int val)) ptr;
	*fptr = SSL_get_info_callback ((SSL const *) ssl);
	break;
      default:
	ret___3 = BIO_ctrl (ssl->rbio, cmd, num, ptr);
	break;
      }
    return (ret___3);
  }
}

static long
ssl_callback_ctrl (BIO * b, int cmd, bio_info_cb * fp)
{
  SSL *ssl;
}

static int
ssl_puts (BIO * bp, char const *str)
{
  int n;
  int ret___3;
  size_t tmp;
  {
    tmp = strlen (str);
    n = (int) tmp;
    ret___3 = BIO_write (bp, (void const *) str, n);
    return (ret___3);
  }
}

BIO *
BIO_new_buffer_ssl_connect (SSL_CTX * ctx)
{
  BIO *ret___3;
  BIO *buf___5;
  BIO *ssl;
  BIO_METHOD *tmp;
  {
    ret___3 = (BIO *) ((void *) 0);
    buf___5 = (BIO *) ((void *) 0);
    ssl = (BIO *) ((void *) 0);
    tmp = BIO_f_buffer ();
    buf___5 = BIO_new (tmp);
    if ((unsigned int) buf___5 == (unsigned int) ((void *) 0))
      {
	return ((BIO *) ((void *) 0));
      }
    ssl = BIO_new_ssl_connect (ctx);
    if ((unsigned int) ssl == (unsigned int) ((void *) 0))
      {
	goto err;
      }
    ret___3 = BIO_push (buf___5, ssl);
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	goto err;
      }
    return (ret___3);
  err:if ((unsigned int) buf___5 != (unsigned int) ((void *) 0))
      {
	BIO_free (buf___5);
      }
    if ((unsigned int) ssl != (unsigned int) ((void *) 0))
      {
	BIO_free (ssl);
      }
    return ((BIO *) ((void *) 0));
  }
}

BIO *
BIO_new_ssl_connect (SSL_CTX * ctx)
{
  BIO *ret___3;
  BIO *con;
  BIO *ssl;
  BIO_METHOD *tmp;
  {
    ret___3 = (BIO *) ((void *) 0);
    con = (BIO *) ((void *) 0);
  err:if ((unsigned int) con != (unsigned int) ((void *) 0))
      {
	BIO_free (con);
      }
    if ((unsigned int) ret___3 != (unsigned int) ((void *) 0))
      {
	BIO_free (ret___3);
      }
    return ((BIO *) ((void *) 0));
  }
}

BIO *
BIO_new_ssl (SSL_CTX * ctx, int client___0)
{
  BIO *ret___3;
  SSL *ssl;
  BIO_METHOD *tmp;
  {
    tmp = BIO_f_ssl ();
    ret___3 = BIO_new (tmp);
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	return ((BIO *) ((void *) 0));
      }
    ssl = SSL_new (ctx);
    if ((unsigned int) ssl == (unsigned int) ((void *) 0))
      {
	BIO_free (ret___3);
	return ((BIO *) ((void *) 0));
      }
    if (client___0)
      {
	SSL_set_connect_state (ssl);
      }
    else
      {
	SSL_set_accept_state (ssl);
      }
    BIO_ctrl (ret___3, 109, 1L, (void *) ((char *) ssl));
    return (ret___3);
  }
}

extern __attribute__ ((__nothrow__))
     char *getenv (char const *__name) __attribute__ ((__nonnull__ (1)));
     rwlock_t lock1;
     rwlock_t lock2;
     rwlock_t lock9;
     rwlock_t lock10;
     rwlock_t lock11;
     rwlock_t lock18;
     rwlock_t lock19;
     rwlock_t lock20;
     rwlock_t lock23;
     rwlock_t lock24;
     rwlock_t lock25;
     rwlock_t lock26;
     rwlock_t lock27;
     rwlock_t lock28;
     rwlock_t lock29;
     rwlock_t lock30;
     static CRYPTO_EX_DATA_IMPL const *impl =
       (CRYPTO_EX_DATA_IMPL const *) ((void *) 0);
     static int int_new_class (void);
     static void int_cleanup (void);
     static int int_get_new_index (int class_index, long argl, void *argp,
				   CRYPTO_EX_new * new_func,
				   CRYPTO_EX_dup * dup_func,
				   CRYPTO_EX_free * free_func___0);
     static int int_new_ex_data (int class_index, void *obj,
				 CRYPTO_EX_DATA * ad);
     static int int_dup_ex_data (int class_index, CRYPTO_EX_DATA * to,
				 CRYPTO_EX_DATA * from___0);
     static void int_free_ex_data (int class_index, void *obj,
				   CRYPTO_EX_DATA * ad);
     static CRYPTO_EX_DATA_IMPL impl_default = {
       &int_new_class, &int_cleanup, &int_get_new_index, &int_new_ex_data,
	 &int_dup_ex_data, &int_free_ex_data
     };

static void
impl_check (void)
{
  {
    _write_lock (&lock2);
    if (!impl)
      {
	impl = (CRYPTO_EX_DATA_IMPL const *) (&impl_default);
      }
    _write_unlock (&lock2);
  }
}

static int ex_class = 100;
static LHASH *ex_data = (LHASH *) ((void *) 0);
static unsigned long
ex_hash_cb (void const *a_void)
{
  {
    return ((unsigned long) ((EX_CLASS_ITEM const *) a_void)->class_index);
  }
}

static int
ex_cmp_cb (void const *a_void, void const *b_void)
{
  {
    return ((int)
	    (((EX_CLASS_ITEM const *) a_void)->class_index -
	     ((EX_CLASS_ITEM const *) b_void)->class_index));
  }
}

static int
ex_data_check (void)
{
  int toret;
  {
    toret = 1;
    _write_lock (&lock2);
    if (!ex_data)
      {
	ex_data = lh_new (&ex_hash_cb, &ex_cmp_cb);
	if ((unsigned int) ex_data == (unsigned int) ((void *) 0))
	  {
	    toret = 0;
	  }
      }
    _write_unlock (&lock2);
    return (toret);
  }
}

static void
def_cleanup_util_cb (CRYPTO_EX_DATA_FUNCS * funcs)
{
  {
    CRYPTO_free ((void *) funcs);
    return;
  }
}

static void
def_cleanup_cb (void *a_void)
{
}

static int
def_add_index (EX_CLASS_ITEM * item, long argl, void *argp,
	       CRYPTO_EX_new * new_func, CRYPTO_EX_dup * dup_func,
	       CRYPTO_EX_free * free_func___0)
{
  int toret;
  CRYPTO_EX_DATA_FUNCS *a;
  void *tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  {
    toret = -1;
    tmp =
      CRYPTO_malloc ((int) sizeof (CRYPTO_EX_DATA_FUNCS), "ex_data.c", 335);
    a = (CRYPTO_EX_DATA_FUNCS *) tmp;
    if (!a)
      {
	ERR_put_error (15, 104, 65, "ex_data.c", 338);
	return (-1);
      }
    a->argl = argl;
    a->argp = argp;
    a->new_func = new_func;
    a->dup_func = dup_func;
    a->free_func = free_func___0;
    _write_lock (&lock2);
    while (1)
      {
	tmp___1 = sk_num ((STACK const *) item->meth);
	if (!(tmp___1 <= item->meth_num))
	  {
	    break;
	  }
	tmp___0 = sk_push (item->meth, (void *) ((char *) ((void *) 0)));
	if (!tmp___0)
	  {
	    ERR_put_error (15, 104, 65, "ex_data.c", 351);
	    CRYPTO_free ((void *) a);
	    goto err;
	  }
      }
    tmp___2 = item->meth_num;
    (item->meth_num)++;
    toret = tmp___2;
    sk_set (item->meth, toret, (void *) ((char *) a));
  err:_write_unlock (&lock2);
    return (toret);
  }
}

static int
int_new_class (void)
{
  int toret;
  int tmp;
  {
    _write_lock (&lock2);
    tmp = ex_class;
    ex_class++;
    toret = tmp;
    _write_unlock (&lock2);
    return (toret);
  }
}

static int
int_get_new_index (int class_index, long argl, void *argp,
		   CRYPTO_EX_new * new_func, CRYPTO_EX_dup * dup_func,
		   CRYPTO_EX_free * free_func___0)
{
  EX_CLASS_ITEM *item;
  EX_CLASS_ITEM *tmp;
  int tmp___0;
  {
    tmp = def_get_class (class_index);
    item = tmp;
    if (!item)
      {
	return (-1);
      }
    tmp___0 =
      def_add_index (item, argl, argp, new_func, dup_func, free_func___0);
    return (tmp___0);
  }
}

static int
int_new_ex_data (int class_index, void *obj, CRYPTO_EX_DATA * ad)
{
  int mx;
  int i;
  void *ptr;
  CRYPTO_EX_DATA_FUNCS **storage;
  EX_CLASS_ITEM *item;
  EX_CLASS_ITEM *tmp;
  void *tmp___0;
  void *tmp___1;
  {
    storage = (CRYPTO_EX_DATA_FUNCS **) ((void *) 0);
    i = 0;
    while (i < mx)
      {
	if (*(storage + i))
	  {
	    if ((*(storage + i))->new_func)
	      {
		ptr = CRYPTO_get_ex_data ((CRYPTO_EX_DATA const *) ad, i);
		(*((*(storage + i))->new_func)) (obj, ptr, ad, i,
						 (*(storage + i))->argl,
						 (*(storage + i))->argp);
	      }
	  }
	i++;
      }
    if (storage)
      {
	CRYPTO_free ((void *) storage);
      }
    return (1);
  }
}

static int
int_dup_ex_data (int class_index, CRYPTO_EX_DATA * to,
		 CRYPTO_EX_DATA * from___0)
{
  int mx;
  int j;
  int i;
  char *ptr;
  CRYPTO_EX_DATA_FUNCS **storage;
  EX_CLASS_ITEM *item;
  void *tmp;
  void *tmp___0;
  void *tmp___1;
  {
    if (mx > 0)
      {
	tmp =
	  CRYPTO_malloc ((int)
			 ((unsigned int) mx *
			  sizeof (CRYPTO_EX_DATA_FUNCS *)), "ex_data.c", 460);
	storage = (CRYPTO_EX_DATA_FUNCS **) tmp;
	if (!storage)
	  {
	    goto skip;
	  }
	i = 0;
	while (i < mx)
	  {
	    tmp___0 = sk_value ((STACK const *) item->meth, i);
	    *(storage + i) = (CRYPTO_EX_DATA_FUNCS *) tmp___0;
	    i++;
      }}
  skip:_read_unlock (&lock2);
    if (mx > 0)
      {
	if (!storage)
	  {
	    ERR_put_error (15, 106, 65, "ex_data.c", 470);
	    return (0);
	  }
      }
    i = 0;
    while (i < mx)
      {
	tmp___1 = CRYPTO_get_ex_data ((CRYPTO_EX_DATA const *) from___0, i);
	ptr = (char *) tmp___1;
	if (*(storage + i))
	  {
	    if ((*(storage + i))->dup_func)
	      {
		(*((*(storage + i))->dup_func)) (to, from___0,
						 (void *) (&ptr), i,
						 (*(storage + i))->argl,
						 (*(storage + i))->argp);
	      }
	  }
	CRYPTO_set_ex_data (to, i, (void *) ptr);
	i++;
      }
    if (storage)
      {
	CRYPTO_free ((void *) storage);
      }
    return (1);
  }
}

static void
int_free_ex_data (int class_index, void *obj, CRYPTO_EX_DATA * ad)
{
  int mx;
  int i;
  EX_CLASS_ITEM *item;
  void *ptr;
  CRYPTO_EX_DATA_FUNCS **storage;
  void *tmp;
  void *tmp___0;
  {
    storage = (CRYPTO_EX_DATA_FUNCS **) ((void *) 0);
    item = def_get_class (class_index);
    if ((unsigned int) item == (unsigned int) ((void *) 0))
      {
	return;
      }
    _read_lock (&lock2);
    mx = sk_num ((STACK const *) item->meth);
    if (mx > 0)
      {
	tmp =
	  CRYPTO_malloc ((int)
			 ((unsigned int) mx *
			  sizeof (CRYPTO_EX_DATA_FUNCS *)), "ex_data.c", 500);
	storage = (CRYPTO_EX_DATA_FUNCS **) tmp;
	if (!storage)
	  {
	    goto skip;
	  }
	i = 0;
	while (i < mx)
	  {
	    tmp___0 = sk_value ((STACK const *) item->meth, i);
	    *(storage + i) = (CRYPTO_EX_DATA_FUNCS *) tmp___0;
	    i++;
      }}
  skip:_read_unlock (&lock2);
    if (mx > 0)
      {
	if (!storage)
	  {
	    ERR_put_error (15, 107, 65, "ex_data.c", 510);
	    return;
	  }
      }
    i = 0;
    while (i < mx)
      {
	if (*(storage + i))
	  {
	    if ((*(storage + i))->free_func)
	      {
		ptr = CRYPTO_get_ex_data ((CRYPTO_EX_DATA const *) ad, i);
		(*((*(storage + i))->free_func)) (obj, ptr, ad, i,
						  (*(storage + i))->argl,
						  (*(storage + i))->argp);
	      }
	  }
	i++;
      }
    if (storage)
      {
	CRYPTO_free ((void *) storage);
      }
    if (ad->sk)
      {
	sk_free (ad->sk);
	ad->sk = (STACK *) ((void *) 0);
      }
    return;
  }
}

int
CRYPTO_get_ex_new_index (int class_index, long argl, void *argp,
			 CRYPTO_EX_new * new_func, CRYPTO_EX_dup * dup_func,
			 CRYPTO_EX_free * free_func___0)
{
  int ret___3;
  {
    ret___3 = -1;
    if (!impl)
      {
	impl_check ();
      }
    ret___3 =
      (*(impl->cb_get_new_index)) (class_index, argl, argp, new_func,
				   dup_func, free_func___0);
    return (ret___3);
  }
}

void
CRYPTO_free_ex_data (int class_index, void *obj, CRYPTO_EX_DATA * ad)
{
  {
    if (!impl)
      {
	impl_check ();
      }
    (*(impl->cb_free_ex_data)) (class_index, obj, ad);
    return;
  }
}

int
CRYPTO_set_ex_data (CRYPTO_EX_DATA * ad, int idx, void *val)
{
  int i;
  STACK *tmp;
  int tmp___0;
  {
    if ((unsigned int) ad->sk == (unsigned int) ((void *) 0))
      {
	tmp = sk_new_null ();
	ad->sk = tmp;
	if ((unsigned int) tmp == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (15, 102, 65, "ex_data.c", 601);
	    return (0);
	  }
      }
    i = sk_num ((STACK const *) ad->sk);
    while (i <= idx)
      {
	tmp___0 = sk_push (ad->sk, (void *) 0);
	if (!tmp___0)
	  {
	    ERR_put_error (15, 102, 65, "ex_data.c", 611);
	    return (0);
	  }
	i++;
      }
    sk_set (ad->sk, idx, val);
    return (1);
  }
}

int OBJ_sn2nid (char const *s);
char const *OBJ_bsearch_ex (char const *key, char const *base, int num,
			    int size, int (*cmp) (void const *, void const *),
			    int flags);

static ASN1_OBJECT nid_objs[780] = {
};

static ASN1_OBJECT *obj_objs[729] = {
  &nid_objs[0], &nid_objs[393], &nid_objs[404], &nid_objs[645],
    &nid_objs[434], &nid_objs[181], &nid_objs[182], &nid_objs[379],
    &nid_objs[676], &nid_objs[646], &nid_objs[11], &nid_objs[647],
    &nid_objs[380], &nid_objs[12], &nid_objs[378], &nid_objs[81],
    &nid_objs[512], &nid_objs[678], &nid_objs[435], &nid_objs[183],
    &nid_objs[381], &nid_objs[677], &nid_objs[394], &nid_objs[13],
    &nid_objs[100], &nid_objs[105], &nid_objs[14], &nid_objs[15],
    &nid_objs[16], &nid_objs[660], &nid_objs[17], &nid_objs[18],
    &nid_objs[106], &nid_objs[107], &nid_objs[661], &nid_objs[173],
    &nid_objs[99], &nid_objs[101], &nid_objs[509], &nid_objs[503],
    &nid_objs[174], &nid_objs[510], &nid_objs[400], &nid_objs[769],
    &nid_objs[82], &nid_objs[83], &nid_objs[84], &nid_objs[85], &nid_objs[86],
    &nid_objs[87], &nid_objs[88], &nid_objs[141], &nid_objs[430],
    &nid_objs[142], &nid_objs[140], &nid_objs[770], &nid_objs[771],
    &nid_objs[666], &nid_objs[103], &nid_objs[89], &nid_objs[747],
    &nid_objs[90], &nid_objs[401], &nid_objs[126], &nid_objs[748],
    &nid_objs[402], &nid_objs[403], &nid_objs[513], &nid_objs[514],
    &nid_objs[515], &nid_objs[516], &nid_objs[517], &nid_objs[518],
    &nid_objs[679], &nid_objs[382], &nid_objs[383], &nid_objs[384],
    &nid_objs[385], &nid_objs[386], &nid_objs[387], &nid_objs[388],
    &nid_objs[376], &nid_objs[395], &nid_objs[19], &nid_objs[96],
    &nid_objs[95], &nid_objs[746], &nid_objs[519], &nid_objs[520],
    &nid_objs[521], &nid_objs[522], &nid_objs[523], &nid_objs[524],
    &nid_objs[525], &nid_objs[526], &nid_objs[527], &nid_objs[528],
    &nid_objs[529], &nid_objs[530], &nid_objs[531], &nid_objs[532],
    &nid_objs[533], &nid_objs[534], &nid_objs[535], &nid_objs[536],
    &nid_objs[537], &nid_objs[538], &nid_objs[539], &nid_objs[540],
    &nid_objs[541], &nid_objs[542], &nid_objs[543], &nid_objs[544],
    &nid_objs[545], &nid_objs[546], &nid_objs[547], &nid_objs[548],
    &nid_objs[549], &nid_objs[550], &nid_objs[551], &nid_objs[552],
    &nid_objs[553], &nid_objs[554], &nid_objs[555], &nid_objs[556],
    &nid_objs[557], &nid_objs[558], &nid_objs[559], &nid_objs[560],
    &nid_objs[561], &nid_objs[562], &nid_objs[563], &nid_objs[564],
    &nid_objs[565], &nid_objs[566], &nid_objs[567], &nid_objs[568],
    &nid_objs[569], &nid_objs[570], &nid_objs[571], &nid_objs[572],
    &nid_objs[573], &nid_objs[574], &nid_objs[575], &nid_objs[576],
    &nid_objs[577], &nid_objs[578], &nid_objs[579], &nid_objs[580],
    &nid_objs[581], &nid_objs[582], &nid_objs[583], &nid_objs[584],
    &nid_objs[585], &nid_objs[586], &nid_objs[587], &nid_objs[588],
    &nid_objs[589], &nid_objs[590], &nid_objs[591], &nid_objs[592],
    &nid_objs[593], &nid_objs[594], &nid_objs[595], &nid_objs[596],
    &nid_objs[597], &nid_objs[598], &nid_objs[599], &nid_objs[600],
    &nid_objs[601], &nid_objs[602], &nid_objs[603], &nid_objs[604],
    &nid_objs[605], &nid_objs[606], &nid_objs[620], &nid_objs[621],
    &nid_objs[622], &nid_objs[623], &nid_objs[607], &nid_objs[608],
    &nid_objs[609], &nid_objs[610], &nid_objs[611], &nid_objs[612],
    &nid_objs[613], &nid_objs[614], &nid_objs[615], &nid_objs[616],
    &nid_objs[617], &nid_objs[618], &nid_objs[619], &nid_objs[636],
    &nid_objs[640], &nid_objs[641], &nid_objs[637], &nid_objs[638],
    &nid_objs[639], &nid_objs[184], &nid_objs[405], &nid_objs[389],
    &nid_objs[504], &nid_objs[104], &nid_objs[29], &nid_objs[31],
    &nid_objs[45], &nid_objs[30], &nid_objs[377], &nid_objs[67],
    &nid_objs[66], &nid_objs[42], &nid_objs[32], &nid_objs[41], &nid_objs[64],
    &nid_objs[70], &nid_objs[115], &nid_objs[117], &nid_objs[143],
    &nid_objs[721], &nid_objs[722], &nid_objs[728], &nid_objs[717],
    &nid_objs[718], &nid_objs[704], &nid_objs[705], &nid_objs[709],
    &nid_objs[708], &nid_objs[714], &nid_objs[723], &nid_objs[729],
    &nid_objs[730], &nid_objs[719], &nid_objs[720], &nid_objs[724],
    &nid_objs[725], &nid_objs[726], &nid_objs[727], &nid_objs[706],
    &nid_objs[707], &nid_objs[710], &nid_objs[711], &nid_objs[712],
    &nid_objs[713], &nid_objs[715], &nid_objs[716], &nid_objs[731],
    &nid_objs[732], &nid_objs[733], &nid_objs[734], &nid_objs[624],
    &nid_objs[625], &nid_objs[626], &nid_objs[627], &nid_objs[628],
    &nid_objs[629], &nid_objs[630], &nid_objs[642], &nid_objs[735],
    &nid_objs[736], &nid_objs[737], &nid_objs[738], &nid_objs[739],
    &nid_objs[740], &nid_objs[741], &nid_objs[742], &nid_objs[743],
    &nid_objs[744], &nid_objs[745], &nid_objs[124], &nid_objs[125],
    &nid_objs[773], &nid_objs[1], &nid_objs[185], &nid_objs[127],
    &nid_objs[505], &nid_objs[506], &nid_objs[119], &nid_objs[631],
    &nid_objs[632], &nid_objs[633], &nid_objs[634], &nid_objs[635],
    &nid_objs[436], &nid_objs[2], &nid_objs[431], &nid_objs[432],
    &nid_objs[433], &nid_objs[116], &nid_objs[113], &nid_objs[406],
    &nid_objs[407], &nid_objs[408], &nid_objs[416], &nid_objs[258],
    &nid_objs[175], &nid_objs[259], &nid_objs[128], &nid_objs[260],
    &nid_objs[261], &nid_objs[262], &nid_objs[263], &nid_objs[264],
    &nid_objs[265], &nid_objs[266], &nid_objs[267], &nid_objs[268],
    &nid_objs[662], &nid_objs[176], &nid_objs[507], &nid_objs[508],
    &nid_objs[57], &nid_objs[754], &nid_objs[766], &nid_objs[757],
    &nid_objs[755], &nid_objs[767], &nid_objs[758], &nid_objs[756],
    &nid_objs[768], &nid_objs[759], &nid_objs[437], &nid_objs[776],
    &nid_objs[777], &nid_objs[779], &nid_objs[778], &nid_objs[186],
    &nid_objs[27], &nid_objs[187], &nid_objs[20], &nid_objs[47], &nid_objs[3],
    &nid_objs[257], &nid_objs[4], &nid_objs[163], &nid_objs[37], &nid_objs[5],
    &nid_objs[44], &nid_objs[120], &nid_objs[643], &nid_objs[680],
    &nid_objs[684], &nid_objs[685], &nid_objs[686], &nid_objs[687],
    &nid_objs[688], &nid_objs[689], &nid_objs[690], &nid_objs[691],
    &nid_objs[692], &nid_objs[693], &nid_objs[694], &nid_objs[695],
    &nid_objs[696], &nid_objs[697], &nid_objs[698], &nid_objs[699],
    &nid_objs[700], &nid_objs[701], &nid_objs[702], &nid_objs[703],
    &nid_objs[409], &nid_objs[410], &nid_objs[411], &nid_objs[412],
    &nid_objs[413], &nid_objs[414], &nid_objs[415], &nid_objs[269],
    &nid_objs[270], &nid_objs[271], &nid_objs[272], &nid_objs[273],
    &nid_objs[274], &nid_objs[275], &nid_objs[276], &nid_objs[277],
    &nid_objs[278], &nid_objs[279], &nid_objs[280], &nid_objs[281],
    &nid_objs[282], &nid_objs[283], &nid_objs[284], &nid_objs[177],
    &nid_objs[285], &nid_objs[286], &nid_objs[287], &nid_objs[288],
    &nid_objs[289], &nid_objs[290], &nid_objs[291], &nid_objs[292],
    &nid_objs[397], &nid_objs[398], &nid_objs[663], &nid_objs[164],
    &nid_objs[165], &nid_objs[293], &nid_objs[129], &nid_objs[130],
    &nid_objs[131], &nid_objs[132], &nid_objs[294], &nid_objs[295],
    &nid_objs[296], &nid_objs[133], &nid_objs[180], &nid_objs[297],
    &nid_objs[298], &nid_objs[299], &nid_objs[300], &nid_objs[301],
    &nid_objs[302], &nid_objs[303], &nid_objs[304], &nid_objs[305],
    &nid_objs[306], &nid_objs[307], &nid_objs[308], &nid_objs[309],
    &nid_objs[310], &nid_objs[311], &nid_objs[312], &nid_objs[313],
    &nid_objs[314], &nid_objs[323], &nid_objs[324], &nid_objs[325],
    &nid_objs[326], &nid_objs[327], &nid_objs[328], &nid_objs[329],
    &nid_objs[330], &nid_objs[331], &nid_objs[332], &nid_objs[333],
    &nid_objs[334], &nid_objs[335], &nid_objs[336], &nid_objs[337],
    &nid_objs[338], &nid_objs[339], &nid_objs[340], &nid_objs[341],
    &nid_objs[342], &nid_objs[343], &nid_objs[344], &nid_objs[345],
    &nid_objs[346], &nid_objs[347], &nid_objs[348], &nid_objs[349],
    &nid_objs[351], &nid_objs[352], &nid_objs[353], &nid_objs[354],
    &nid_objs[355], &nid_objs[356], &nid_objs[357], &nid_objs[358],
    &nid_objs[399], &nid_objs[359], &nid_objs[360], &nid_objs[361],
    &nid_objs[362], &nid_objs[664], &nid_objs[665], &nid_objs[667],
    &nid_objs[178], &nid_objs[179], &nid_objs[363], &nid_objs[364],
    &nid_objs[58], &nid_objs[59], &nid_objs[438], &nid_objs[439],
    &nid_objs[440], &nid_objs[441], &nid_objs[108], &nid_objs[112],
    &nid_objs[6], &nid_objs[7], &nid_objs[396], &nid_objs[8], &nid_objs[65],
    &nid_objs[644], &nid_objs[668], &nid_objs[669], &nid_objs[670],
    &nid_objs[671], &nid_objs[28], &nid_objs[9], &nid_objs[10],
    &nid_objs[168], &nid_objs[169], &nid_objs[170], &nid_objs[68],
    &nid_objs[69], &nid_objs[161], &nid_objs[162], &nid_objs[21],
    &nid_objs[22], &nid_objs[23], &nid_objs[24], &nid_objs[25], &nid_objs[26],
    &nid_objs[48], &nid_objs[49], &nid_objs[50], &nid_objs[51], &nid_objs[52],
    &nid_objs[53], &nid_objs[54], &nid_objs[55], &nid_objs[56],
    &nid_objs[172], &nid_objs[167], &nid_objs[188], &nid_objs[156],
    &nid_objs[157], &nid_objs[681], &nid_objs[682], &nid_objs[683],
    &nid_objs[417], &nid_objs[390], &nid_objs[91], &nid_objs[315],
    &nid_objs[316], &nid_objs[317], &nid_objs[318], &nid_objs[319],
    &nid_objs[320], &nid_objs[321], &nid_objs[322], &nid_objs[365],
    &nid_objs[366], &nid_objs[367], &nid_objs[368], &nid_objs[369],
    &nid_objs[370], &nid_objs[371], &nid_objs[372], &nid_objs[373],
    &nid_objs[374], &nid_objs[375], &nid_objs[418], &nid_objs[419],
    &nid_objs[420], &nid_objs[421], &nid_objs[422], &nid_objs[423],
    &nid_objs[424], &nid_objs[425], &nid_objs[426], &nid_objs[427],
    &nid_objs[428], &nid_objs[429], &nid_objs[672], &nid_objs[673],
    &nid_objs[674], &nid_objs[675], &nid_objs[71], &nid_objs[72],
    &nid_objs[73], &nid_objs[74], &nid_objs[75], &nid_objs[76], &nid_objs[77],
    &nid_objs[78], &nid_objs[79], &nid_objs[139], &nid_objs[458],
    &nid_objs[459], &nid_objs[460], &nid_objs[461], &nid_objs[462],
    &nid_objs[463], &nid_objs[464], &nid_objs[465], &nid_objs[466],
    &nid_objs[467], &nid_objs[468], &nid_objs[469], &nid_objs[470],
    &nid_objs[471], &nid_objs[472], &nid_objs[473], &nid_objs[474],
    &nid_objs[475], &nid_objs[476], &nid_objs[477], &nid_objs[391],
    &nid_objs[478], &nid_objs[479], &nid_objs[480], &nid_objs[481],
    &nid_objs[482], &nid_objs[483], &nid_objs[484], &nid_objs[485],
    &nid_objs[486], &nid_objs[487], &nid_objs[488], &nid_objs[489],
    &nid_objs[490], &nid_objs[491], &nid_objs[492], &nid_objs[493],
    &nid_objs[494], &nid_objs[495], &nid_objs[496], &nid_objs[497],
    &nid_objs[498], &nid_objs[499], &nid_objs[500], &nid_objs[501],
    &nid_objs[502], &nid_objs[442], &nid_objs[443], &nid_objs[444],
    &nid_objs[445], &nid_objs[446], &nid_objs[447], &nid_objs[448],
    &nid_objs[449], &nid_objs[392], &nid_objs[450], &nid_objs[451],
    &nid_objs[452], &nid_objs[453], &nid_objs[454], &nid_objs[455],
    &nid_objs[456], &nid_objs[457], &nid_objs[189], &nid_objs[190],
    &nid_objs[191], &nid_objs[192], &nid_objs[193], &nid_objs[194],
    &nid_objs[195], &nid_objs[158], &nid_objs[159], &nid_objs[160],
    &nid_objs[144], &nid_objs[145], &nid_objs[146], &nid_objs[147],
    &nid_objs[148], &nid_objs[149], &nid_objs[171], &nid_objs[134],
    &nid_objs[135], &nid_objs[136], &nid_objs[137], &nid_objs[138],
    &nid_objs[648], &nid_objs[649], &nid_objs[751], &nid_objs[752],
    &nid_objs[753], &nid_objs[196], &nid_objs[197], &nid_objs[198],
    &nid_objs[199], &nid_objs[200], &nid_objs[201], &nid_objs[202],
    &nid_objs[203], &nid_objs[204], &nid_objs[205], &nid_objs[206],
    &nid_objs[207], &nid_objs[208], &nid_objs[209], &nid_objs[210],
    &nid_objs[211], &nid_objs[212], &nid_objs[213], &nid_objs[214],
    &nid_objs[215], &nid_objs[216], &nid_objs[217], &nid_objs[218],
    &nid_objs[219], &nid_objs[220], &nid_objs[221], &nid_objs[222],
    &nid_objs[223], &nid_objs[224], &nid_objs[225], &nid_objs[226],
    &nid_objs[227], &nid_objs[228], &nid_objs[229], &nid_objs[230],
    &nid_objs[231], &nid_objs[232], &nid_objs[233], &nid_objs[234],
    &nid_objs[235], &nid_objs[236], &nid_objs[237], &nid_objs[238],
    &nid_objs[239], &nid_objs[240], &nid_objs[241], &nid_objs[242],
    &nid_objs[243], &nid_objs[244], &nid_objs[245], &nid_objs[246],
    &nid_objs[247], &nid_objs[248], &nid_objs[249], &nid_objs[250],
    &nid_objs[251], &nid_objs[252], &nid_objs[253], &nid_objs[254],
    &nid_objs[255], &nid_objs[256], &nid_objs[150], &nid_objs[151],
    &nid_objs[152], &nid_objs[153], &nid_objs[154], &nid_objs[155],
    &nid_objs[34]
};

static int obj_cmp (void const *ap, void const *bp);
static int new_nid = 780;
static LHASH *added = (LHASH *) ((void *) 0);
static int
sn_cmp (void const *a, void const *b)
{
  ASN1_OBJECT const *const *ap;
  ASN1_OBJECT const *const *bp;
  size_t __s1_len;
  size_t __s2_len;
  int tmp___0;
  int tmp___3;
  int tmp___4;
  int tmp___5;
  {
    ap = (ASN1_OBJECT const *const *) a;
    bp = (ASN1_OBJECT const *const *) b;
    if (0)
      {
	__s1_len = strlen ((char const *) (*ap)->sn);
	__s2_len = strlen ((char const *) (*bp)->sn);
	if (!
	    ((unsigned int) ((void const *) ((*ap)->sn + 1)) -
	     (unsigned int) ((void const *) (*ap)->sn) == 1U))
	  {
	    goto _L___0;
	  }
	else
	  {
	    if (__s1_len >= 4U)
	      {
	      _L___0:if (!
		    ((unsigned int) ((void const *) ((*bp)->sn + 1)) -
		     (unsigned int) ((void const *) (*bp)->sn) == 1U))
		  {
		    tmp___5 = 1;
		  }
		else
		  {
		    if (__s2_len >= 4U)
		      {
			tmp___5 = 1;
		      }
		    else
		      {
			tmp___5 = 0;
		      }
		  }
	      }
	    else
	      {
		tmp___5 = 0;
	      }
	  }
	if (tmp___5)
	  {
	    tmp___0 =
	      __builtin_strcmp ((char const *) (*ap)->sn,
				(char const *) (*bp)->sn);
	    tmp___4 = tmp___0;
	  }
	else
	  {
	    tmp___3 =
	      __builtin_strcmp ((char const *) (*ap)->sn,
				(char const *) (*bp)->sn);
	    tmp___4 = tmp___3;
      }}
    else
      {
	tmp___3 =
	  __builtin_strcmp ((char const *) (*ap)->sn,
			    (char const *) (*bp)->sn);
	tmp___4 = tmp___3;
      }
    return (tmp___4);
  }
}

static int
ln_cmp (void const *a, void const *b)
{
  ASN1_OBJECT const *const *ap;
  ASN1_OBJECT const *const *bp;
  size_t __s1_len;
  size_t __s2_len;
  int tmp___0;
  int tmp___3;
  int tmp___4;
  int tmp___5;
  {
    ap = (ASN1_OBJECT const *const *) a;
    bp = (ASN1_OBJECT const *const *) b;
    if (0)
      {
	__s1_len = strlen ((char const *) (*ap)->ln);
	__s2_len = strlen ((char const *) (*bp)->ln);
	if (!
	    ((unsigned int) ((void const *) ((*ap)->ln + 1)) -
	     (unsigned int) ((void const *) (*ap)->ln) == 1U))
	  {
	    goto _L___0;
	  }
	else
	  {
	    if (__s1_len >= 4U)
	      {
	      _L___0:if (!
		    ((unsigned int) ((void const *) ((*bp)->ln + 1)) -
		     (unsigned int) ((void const *) (*bp)->ln) == 1U))
		  {
		    tmp___5 = 1;
		  }
		else
		  {
		    if (__s2_len >= 4U)
		      {
			tmp___5 = 1;
		      }
		    else
		      {
			tmp___5 = 0;
		      }
		  }
	      }
	    else
	      {
		tmp___5 = 0;
	      }
	  }
	if (tmp___5)
	  {
	    tmp___0 =
	      __builtin_strcmp ((char const *) (*ap)->ln,
				(char const *) (*bp)->ln);
	    tmp___4 = tmp___0;
	  }
	else
	  {
	    tmp___3 =
	      __builtin_strcmp ((char const *) (*ap)->ln,
				(char const *) (*bp)->ln);
	    tmp___4 = tmp___3;
      }}
    else
      {
	tmp___3 =
	  __builtin_strcmp ((char const *) (*ap)->ln,
			    (char const *) (*bp)->ln);
	tmp___4 = tmp___3;
      }
    return (tmp___4);
  }
}

char const *
OBJ_nid2ln (int n)
{
  ADDED_OBJ ad;
  ADDED_OBJ *adp;
  ASN1_OBJECT ob;
  void *tmp;
  {
    if (n >= 0)
      {
	if (n < 780)
	  {
	    if (n != 0)
	      {
		if (nid_objs[n].nid == 0)
		  {
		    ERR_put_error (8, 102, 101, "obj_dat.c", 346);
		    return ((char const *) ((void *) 0));
		  }
	      }
	    return (nid_objs[n].ln);
	  }
	else
	  {
	    goto _L;
	  }
      }
    else
      {
      _L:if ((unsigned int) added == (unsigned int) ((void *) 0))
	  {
	    return ((char const *) ((void *) 0));
	  }
	else
	  {
	    ad.type = 3;
	    ad.obj = &ob;
	    ob.nid = n;
	    tmp = lh_retrieve (added, (void const *) (&ad));
	    adp = (ADDED_OBJ *) tmp;
	    if ((unsigned int) adp != (unsigned int) ((void *) 0))
	      {
		return ((adp->obj)->ln);
	      }
	    else
	      {
		ERR_put_error (8, 102, 101, "obj_dat.c", 363);
		return ((char const *) ((void *) 0));
      }}}
  }
}

int
OBJ_obj2nid (ASN1_OBJECT const *a)
{
  ASN1_OBJECT **op;
  ADDED_OBJ ad;
  ADDED_OBJ *adp;
  void *tmp;
  char const *tmp___0;
  {
    if ((unsigned int) a == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    if (a->nid != 0)
      {
	return ((int) a->nid);
      }
    if ((unsigned int) added != (unsigned int) ((void *) 0))
      {
	ad.type = 0;
	ad.obj = (ASN1_OBJECT *) a;
	tmp = lh_retrieve (added, (void const *) (&ad));
	adp = (ADDED_OBJ *) tmp;
	if ((unsigned int) adp != (unsigned int) ((void *) 0))
	  {
	    return ((adp->obj)->nid);
	  }
      }
    tmp___0 =
      OBJ_bsearch ((char const *) (&a), (char const *) (obj_objs), 729,
		   (int) sizeof (ASN1_OBJECT *), &obj_cmp);
    op = (ASN1_OBJECT **) tmp___0;
    if ((unsigned int) op == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    return ((*op)->nid);
  }
}

static int
obj_cmp (void const *ap, void const *bp)
{
  int j;
  ASN1_OBJECT const *a;
  ASN1_OBJECT const *b;
  int tmp;
  {
    a = (ASN1_OBJECT const *) *((ASN1_OBJECT * const *) ap);
    b = (ASN1_OBJECT const *) *((ASN1_OBJECT * const *) bp);
    j = (int) (a->length - b->length);
    if (j)
      {
	return (j);
      }
    return (tmp);
  }
}

char const *
OBJ_bsearch_ex (char const *key, char const *base, int num, int size,
		int (*cmp) (void const *, void const *), int flags)
{
  int l;
  int h___2;
  int i;
  int c;
  char const *p;
  int tmp;
  {
    i = 0;
    c = 0;
    p = (char const *) ((void *) 0);
    if (num == 0)
      {
	return ((char const *) ((void *) 0));
      }
    l = 0;
    h___2 = num;
    while (l < h___2)
      {
	i = (l + h___2) / 2;
	p = base + i * size;
	c = (*cmp) ((void const *) key, (void const *) p);
	if (c < 0)
	  {
	    h___2 = i;
	  }
	else
	  {
	    if (c > 0)
	      {
		l = i + 1;
	      }
	    else
	      {
		break;
	      }
	  }
      }
    if (c != 0)
      {
	if (!(flags & 1))
	  {
	    p = (char const *) ((void *) 0);
	  }
	else
	  {
	    goto _L;
	  }
      }
    else
      {
      _L:if (c == 0)
	  {
	    if (flags & 2)
	      {
		while (1)
		  {
		    if (i > 0)
		      {
			tmp =
			  (*cmp) ((void const *) key,
				  (void const *) (base + (i - 1) * size));
			if (!(tmp == 0))
			  {
			    break;
			  }
		      }
		    else
		      {
			break;
		      }
		    i--;
		  }
		p = base + i * size;
	      }
	  }
      }
    return (p);
  }
}

void
HMAC_Init_ex (HMAC_CTX * ctx, void const *key, int len, EVP_MD const *md___0,
	      ENGINE * impl___0)
{
  int i;
  int j;
  {
    EVP_MD_CTX_cleanup (&ctx->i_ctx);
    EVP_MD_CTX_cleanup (&ctx->o_ctx);
    EVP_MD_CTX_cleanup (&ctx->md_ctx);
    memset ((void *) ctx, 0, sizeof (*ctx));
    return;
  }
}

void CAST_ecb_encrypt (unsigned char const *in, unsigned char *out,
		       CAST_KEY * ks, int enc);
int BN_mod_exp_mont (BIGNUM * rr, BIGNUM const *a, BIGNUM const *p,
		     BIGNUM const *m___10, BN_CTX * ctx,
		     BN_MONT_CTX * in_mont);
int BN_mod_exp_mont_consttime (BIGNUM * rr, BIGNUM const *a, BIGNUM const *p,
			       BIGNUM const *m___10, BN_CTX * ctx,
			       BN_MONT_CTX * in_mont);
int BN_mod_exp_mont_word (BIGNUM * rr, unsigned long a, BIGNUM const *p,
			  BIGNUM const *m___10, BN_CTX * ctx,
			  BN_MONT_CTX * in_mont);
EC_KEY *
EC_KEY_copy (EC_KEY * dest, EC_KEY const *src)
{
  EC_EXTRA_DATA *d;
  EC_METHOD const *meth___0;
  EC_METHOD const *tmp;
  int tmp___0;
  int tmp___1;
  BIGNUM *tmp___2;
  void *t;
  void *tmp___3;
  int tmp___4;
  {
    if ((unsigned int) dest == (unsigned int) ((void *) 0))
      {
	ERR_put_error (16, 178, 67, "ec_key.c", 144);
	return ((EC_KEY *) ((void *) 0));
      }
    else
      {
	if ((unsigned int) src == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (16, 178, 67, "ec_key.c", 144);
	    return ((EC_KEY *) ((void *) 0));
	  }
      }
    if (src->group)
      {
	tmp = EC_GROUP_method_of ((EC_GROUP const *) src->group);
	meth___0 = tmp;
	if (dest->group)
	  {
	    EC_GROUP_free (dest->group);
	  }
	dest->group = EC_GROUP_new (meth___0);
	if ((unsigned int) dest->group == (unsigned int) ((void *) 0))
	  {
	    return ((EC_KEY *) ((void *) 0));
	  }
	tmp___0 = EC_GROUP_copy (dest->group, (EC_GROUP const *) src->group);
	if (!tmp___0)
	  {
	    return ((EC_KEY *) ((void *) 0));
	  }
      }
    if (src->pub_key)
      {
	if (src->group)
	  {
	    if (dest->pub_key)
	      {
		EC_POINT_free (dest->pub_key);
	      }
	    dest->pub_key = EC_POINT_new ((EC_GROUP const *) src->group);
	    if ((unsigned int) dest->pub_key == (unsigned int) ((void *) 0))
	      {
		return ((EC_KEY *) ((void *) 0));
	      }
	    tmp___1 =
	      EC_POINT_copy (dest->pub_key, (EC_POINT const *) src->pub_key);
	    if (!tmp___1)
	      {
		return ((EC_KEY *) ((void *) 0));
	      }
	  }
      }
    if (src->priv_key)
      {
	if ((unsigned int) dest->priv_key == (unsigned int) ((void *) 0))
	  {
	    dest->priv_key = BN_new ();
	    if ((unsigned int) dest->priv_key == (unsigned int) ((void *) 0))
	      {
		return ((EC_KEY *) ((void *) 0));
	      }
	  }
	tmp___2 = BN_copy (dest->priv_key, (BIGNUM const *) src->priv_key);
	if (!tmp___2)
	  {
	    return ((EC_KEY *) ((void *) 0));
	  }
      }
    EC_EX_DATA_free_all_data (&dest->method_data);
    d = (EC_EXTRA_DATA *) src->method_data;
    while ((unsigned int) d != (unsigned int) ((void *) 0))
      {
	tmp___3 = (*(d->dup_func)) (d->data);
	t = tmp___3;
	if ((unsigned int) t == (unsigned int) ((void *) 0))
	  {
	    return ((EC_KEY *) 0);
	  }
	tmp___4 =
	  EC_EX_DATA_set_data (&dest->method_data, t, d->dup_func,
			       d->free_func, d->clear_free_func);
	if (!tmp___4)
	  {
	    return ((EC_KEY *) 0);
	  }
	d = d->next;
      }
    dest->enc_flag = (unsigned int) src->enc_flag;
    dest->conv_form =
      (enum __anonenum_point_conversion_form_t_29) src->conv_form;
    dest->version = (int) src->version;
    return (dest);
  }
}

EC_KEY *
EC_KEY_dup (EC_KEY const *ec_key)
{
  EC_KEY *ret___3;
  EC_KEY *tmp;
  EC_KEY *tmp___0;
  {
    tmp = EC_KEY_new ();
    ret___3 = tmp;
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	return ((EC_KEY *) ((void *) 0));
      }
    tmp___0 = EC_KEY_copy (ret___3, ec_key);
    if ((unsigned int) tmp___0 == (unsigned int) ((void *) 0))
      {
	EC_KEY_free (ret___3);
	return ((EC_KEY *) ((void *) 0));
      }
    return (ret___3);
  }
}

BN_BLINDING *RSA_setup_blinding (RSA * rsa, BN_CTX * in_ctx);
int RSA_padding_add_PKCS1_type_1 (unsigned char *to, int tlen,
				  unsigned char const *from___0, int flen);
int RSA_padding_add_X931 (unsigned char *to, int tlen,
			  unsigned char const *from___0, int flen);
int RSA_padding_check_X931 (unsigned char *to, int tlen,
			    unsigned char const *from___0, int flen, int num);
static int RSA_eay_public_encrypt (int flen, unsigned char const *from___0,
				   unsigned char *to, RSA * rsa, int padding);
static int RSA_eay_private_encrypt (int flen, unsigned char const *from___0,
				    unsigned char *to, RSA * rsa,
				    int padding);
static int RSA_eay_public_decrypt (int flen, unsigned char const *from___0,
				   unsigned char *to, RSA * rsa, int padding);
static int RSA_eay_private_decrypt (int flen, unsigned char const *from___0,
				    unsigned char *to, RSA * rsa,
				    int padding);
static int RSA_eay_mod_exp (BIGNUM * r0, BIGNUM const *I, RSA * rsa,
			    BN_CTX * ctx);
static int RSA_eay_init (RSA * rsa);
static int RSA_eay_finish (RSA * rsa);
static RSA_METHOD rsa_pkcs1_eay_meth = {
  "Eric Young\'s PKCS#1 RSA", &RSA_eay_public_encrypt,
    &RSA_eay_public_decrypt, &RSA_eay_private_encrypt,
    &RSA_eay_private_decrypt, &RSA_eay_mod_exp, &BN_mod_exp_mont,
    &RSA_eay_init, &RSA_eay_finish, 0, (char *) ((void *) 0),
    (int (*)
     (int type, unsigned char const *m, unsigned int m_length,
      unsigned char *sigret, unsigned int *siglen, RSA const *rsa)) 0,
    (int (*)
     (int dtype, unsigned char const *m, unsigned int m_length,
      unsigned char *sigbuf, unsigned int siglen, RSA const *rsa)) 0,
    (int (*)(RSA * rsa, int bits, BIGNUM * e, BN_GENCB * cb)) ((void *) 0)
};

RSA_METHOD const *
RSA_PKCS1_SSLeay (void)
{
  {
    return ((RSA_METHOD const *) (&rsa_pkcs1_eay_meth));
  }
}

static int
RSA_eay_public_encrypt (int flen, unsigned char const *from___0,
			unsigned char *to, RSA * rsa, int padding)
{
  BIGNUM *f;
  BIGNUM *ret___3;
  int i;
  int j;
  int k;
  int num;
  int r;
  unsigned char *buf___5;
  BN_CTX *ctx;
  int tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  void *tmp___4;
  BIGNUM *tmp___5;
  int tmp___6;
  BN_MONT_CTX *tmp___7;
  int tmp___8;
  int tmp___9;
  {
    num = 0;
    r = -1;
    buf___5 = (unsigned char *) ((void *) 0);
    ctx = (BN_CTX *) ((void *) 0);
    f = BN_CTX_get (ctx);
    ret___3 = BN_CTX_get (ctx);
    tmp___3 = BN_num_bits ((BIGNUM const *) rsa->n);
    num = (tmp___3 + 7) / 8;
    tmp___4 = CRYPTO_malloc (num, "rsa_eay.c", 198);
    buf___5 = (unsigned char *) tmp___4;
    if (!f)
      {
	ERR_put_error (4, 104, 65, "rsa_eay.c", 201);
	goto err;
      }
    else
      {
	if (!ret___3)
	  {
	    ERR_put_error (4, 104, 65, "rsa_eay.c", 201);
	    goto err;
	  }
	else
	  {
	    if (!buf___5)
	      {
		ERR_put_error (4, 104, 65, "rsa_eay.c", 201);
		goto err;
	      }
	  }
      }
    switch (padding)
      {
      case 1:
	i = RSA_padding_add_PKCS1_type_2 (buf___5, num, from___0, flen);
	break;
      case 4:
	i =
	  RSA_padding_add_PKCS1_OAEP (buf___5, num, from___0, flen,
				      (unsigned char const *) ((void *) 0),
				      0);
	break;
      case 2:
	i = RSA_padding_add_SSLv23 (buf___5, num, from___0, flen);
	break;
      case 3:
	i = RSA_padding_add_none (buf___5, num, from___0, flen);
	break;
      default:
	ERR_put_error (4, 104, 118, "rsa_eay.c", 222);
	goto err;
      }
    j = (tmp___9 + 7) / 8;
    i = BN_bn2bin ((BIGNUM const *) ret___3, to + (num - j));
    k = 0;
    while (k < num - i)
      {
	*(to + k) = (unsigned char) 0;
	k++;
      }
    r = num;
  err:if ((unsigned int) ctx != (unsigned int) ((void *) 0))
      {
	BN_CTX_end (ctx);
	BN_CTX_free (ctx);
      }
    if ((unsigned int) buf___5 != (unsigned int) ((void *) 0))
      {
	OPENSSL_cleanse ((void *) buf___5, (unsigned int) num);
	CRYPTO_free ((void *) buf___5);
      }
    return (r);
  }
}

static BN_BLINDING *
rsa_get_blinding (RSA * rsa, int *local, BN_CTX * ctx)
{
  BN_BLINDING *ret___3;
  int got_write_lock;
  unsigned long tmp;
  unsigned long tmp___0;
  {
    got_write_lock = 0;
    _read_lock (&lock9);
    if ((unsigned int) rsa->blinding == (unsigned int) ((void *) 0))
      {
	_read_unlock (&lock9);
	_write_lock (&lock9);
	got_write_lock = 1;
	if ((unsigned int) rsa->blinding == (unsigned int) ((void *) 0))
	  {
	    rsa->blinding = RSA_setup_blinding (rsa, ctx);
	  }
      }
    ret___3 = rsa->blinding;
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	goto err;
      }
    tmp = BN_BLINDING_get_thread_id ((BN_BLINDING const *) ret___3);
    tmp___0 = CRYPTO_thread_id ();
    if (tmp == tmp___0)
      {
	*local = 1;
      }
    else
      {
	*local = 0;
	if ((unsigned int) rsa->mt_blinding == (unsigned int) ((void *) 0))
	  {
	    if (!got_write_lock)
	      {
		_read_unlock (&lock9);
		_write_lock (&lock9);
		got_write_lock = 1;
	      }
	    if ((unsigned int) rsa->mt_blinding ==
		(unsigned int) ((void *) 0))
	      {
		rsa->mt_blinding = RSA_setup_blinding (rsa, ctx);
	      }
	  }
	ret___3 = rsa->mt_blinding;
      }
  err:if (got_write_lock)
      {
	_write_unlock (&lock9);
      }
    else
      {
	_read_unlock (&lock9);
      }
    return (ret___3);
  }
}

static int
rsa_blinding_convert (BN_BLINDING * b, int local, BIGNUM * f, BIGNUM * r,
		      BN_CTX * ctx)
{
  int tmp;
}

static int
RSA_eay_private_encrypt (int flen, unsigned char const *from___0,
			 unsigned char *to, RSA * rsa, int padding)
{
  BIGNUM *f;
  BIGNUM *ret___3;
  BIGNUM *br;
  BIGNUM *res;
  int i;
  int j;
  int k;
  int num;
  int r;
  unsigned char *buf___5;
  BN_CTX *ctx;
  int local_blinding;
  BN_BLINDING *blinding;
  int tmp;
  void *tmp___0;
  BIGNUM *tmp___1;
  int tmp___2;
  {
    num = 0;
    r = -1;
    buf___5 = (unsigned char *) ((void *) 0);
    ctx = (BN_CTX *) ((void *) 0);
    local_blinding = 0;
    blinding = (BN_BLINDING *) ((void *) 0);
    ctx = BN_CTX_new ();
    if ((unsigned int) ctx == (unsigned int) ((void *) 0))
      {
	goto err;
      }
    if (!f)
      {
	ERR_put_error (4, 102, 65, "rsa_eay.c", 373);
	goto err;
      }
    else
      {
	if (!ret___3)
	  {
	    ERR_put_error (4, 102, 65, "rsa_eay.c", 373);
	    goto err;
	  }
	else
	  {
	    if (!buf___5)
	      {
		ERR_put_error (4, 102, 65, "rsa_eay.c", 373);
		goto err;
	      }
	  }
      }
    switch (padding)
      {
      case 1:
	i = RSA_padding_add_PKCS1_type_1 (buf___5, num, from___0, flen);
	break;
      case 5:
	i = RSA_padding_add_X931 (buf___5, num, from___0, flen);
	break;
      case 3:
	i = RSA_padding_add_none (buf___5, num, from___0, flen);
	break;
      case 2:
      default:
	ERR_put_error (4, 102, 118, "rsa_eay.c", 390);
	goto err;
      }
    if (i <= 0)
      {
	goto err;
      }
    tmp___1 = BN_bin2bn ((unsigned char const *) buf___5, num, f);
    if ((unsigned int) tmp___1 == (unsigned int) ((void *) 0))
      {
	goto err;
      }
    tmp___2 = BN_ucmp ((BIGNUM const *) f, (BIGNUM const *) rsa->n);
    if (tmp___2 >= 0)
      {
	ERR_put_error (4, 102, 132, "rsa_eay.c", 400);
	goto err;
      }
    if (!(rsa->flags & 128))
      {
	blinding = rsa_get_blinding (rsa, &local_blinding, ctx);
	if ((unsigned int) blinding == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (4, 102, 68, "rsa_eay.c", 409);
	    goto err;
	  }
      }
    i = BN_bn2bin ((BIGNUM const *) res, to + (num - j));
    k = 0;
    while (k < num - i)
      {
	*(to + k) = (unsigned char) 0;
	k++;
      }
    r = num;
  err:if ((unsigned int) ctx != (unsigned int) ((void *) 0))
      {
	BN_CTX_end (ctx);
	BN_CTX_free (ctx);
      }
    if ((unsigned int) buf___5 != (unsigned int) ((void *) 0))
      {
	OPENSSL_cleanse ((void *) buf___5, (unsigned int) num);
	CRYPTO_free ((void *) buf___5);
      }
    return (r);
  }
}

static int
RSA_eay_private_decrypt (int flen, unsigned char const *from___0,
			 unsigned char *to, RSA * rsa, int padding)
{
  BIGNUM *f;
  BIGNUM *ret___3;
  BIGNUM *br;
  int j;
  int num;
  int r;
  unsigned char *p;
  unsigned char *buf___5;
  BN_CTX *ctx;
  int local_blinding;
  BN_BLINDING *blinding;
  int tmp;
  void *tmp___0;
  BIGNUM *tmp___1;
  int tmp___2;
  int tmp___3;
  int tmp___4;
  BIGNUM local_d;
  BIGNUM *d;
  BN_MONT_CTX *tmp___5;
  int tmp___6;
  int tmp___7;
  {
    num = 0;
    if (flen > num)
      {
	ERR_put_error (4, 101, 108, "rsa_eay.c", 512);
	goto err;
      }
    tmp___1 = BN_bin2bn (from___0, flen, f);
    if ((unsigned int) tmp___1 == (unsigned int) ((void *) 0))
      {
	goto err;
      }
    tmp___2 = BN_ucmp ((BIGNUM const *) f, (BIGNUM const *) rsa->n);
    if (tmp___2 >= 0)
      {
	ERR_put_error (4, 101, 132, "rsa_eay.c", 521);
	goto err;
      }
    if (!(rsa->flags & 128))
      {
	blinding = rsa_get_blinding (rsa, &local_blinding, ctx);
	if ((unsigned int) blinding == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (4, 101, 68, "rsa_eay.c", 530);
	    goto err;
	  }
      }
    if ((unsigned int) blinding != (unsigned int) ((void *) 0))
      {
	tmp___3 = rsa_blinding_convert (blinding, local_blinding, f, br, ctx);
	if (!tmp___3)
	  {
	    goto err;
	  }
      }
    if (rsa->flags & 32)
      {
	goto _L;
      }
    else
      {
	if ((unsigned int) rsa->p != (unsigned int) ((void *) 0))
	  {
	    if ((unsigned int) rsa->q != (unsigned int) ((void *) 0))
	      {
		if ((unsigned int) rsa->dmp1 != (unsigned int) ((void *) 0))
		  {
		    if ((unsigned int) rsa->dmq1 !=
			(unsigned int) ((void *) 0))
		      {
			if ((unsigned int) rsa->iqmp !=
			    (unsigned int) ((void *) 0))
			  {
			  _L:tmp___4 =
			      (*((rsa->meth)->rsa_mod_exp)) (ret___3,
							     (BIGNUM const *)
							     f, rsa, ctx);
			    if (!tmp___4)
			      {
				goto err;
			      }
			  }
			else
			  {
			    goto _L___3;
			  }
		      }
		    else
		      {
			goto _L___3;
		      }
		  }
		else
		  {
		    goto _L___3;
		  }
	      }
	    else
	      {
		goto _L___3;
	      }
	  }
	else
	  {
	  _L___3:d = (BIGNUM *) ((void *) 0);
	    if (!(rsa->flags & 256))
	      {
		d = &local_d;
		d->d = (rsa->d)->d;
		d->top = (rsa->d)->top;
		d->dmax = (rsa->d)->dmax;
		d->neg = (rsa->d)->neg;
		d->flags =
		  (((d->flags & 1) | ((rsa->d)->flags & -2)) | 2) | 4;
	      }
	    else
	      {
		d = rsa->d;
	      }
	    if (rsa->flags & 2)
	      {
		if ((unsigned int) rsa->_method_mod_n ==
		    (unsigned int) ((void *) 0))
		  {
		    tmp___5 =
		      BN_MONT_CTX_set_locked (&rsa->_method_mod_n, lock9,
					      (BIGNUM const *) rsa->n, ctx);
		    if (!tmp___5)
		      {
			goto err;
		      }
		  }
	      }
	    tmp___6 =
	      (*((rsa->meth)->bn_mod_exp)) (ret___3, (BIGNUM const *) f,
					    (BIGNUM const *) d,
					    (BIGNUM const *) rsa->n, ctx,
					    rsa->_method_mod_n);
	    if (!tmp___6)
	      {
		goto err;
	      }
	  }
      }
    if (blinding)
      {
	tmp___7 =
	  rsa_blinding_invert (blinding, local_blinding, ret___3, br, ctx);
	if (!tmp___7)
	  {
	    goto err;
	  }
      }
    p = buf___5;
    j = BN_bn2bin ((BIGNUM const *) ret___3, p);
    switch (padding)
      {
      case 1:
	r =
	  RSA_padding_check_PKCS1_type_2 (to, num,
					  (unsigned char const *) buf___5, j,
					  num);
	break;
      case 4:
	r =
	  RSA_padding_check_PKCS1_OAEP (to, num,
					(unsigned char const *) buf___5, j,
					num,
					(unsigned char const *) ((void *) 0),
					0);
	break;
      case 2:
	r =
	  RSA_padding_check_SSLv23 (to, num, (unsigned char const *) buf___5,
				    j, num);
	break;
      case 3:
	r =
	  RSA_padding_check_none (to, num, (unsigned char const *) buf___5, j,
				  num);
	break;
      default:
	ERR_put_error (4, 101, 118, "rsa_eay.c", 592);
	goto err;
      }
    if (r < 0)
      {
	ERR_put_error (4, 101, 114, "rsa_eay.c", 596);
      }
  err:if ((unsigned int) ctx != (unsigned int) ((void *) 0))
      {
	BN_CTX_end (ctx);
	BN_CTX_free (ctx);
      }
    if ((unsigned int) buf___5 != (unsigned int) ((void *) 0))
      {
	OPENSSL_cleanse ((void *) buf___5, (unsigned int) num);
	CRYPTO_free ((void *) buf___5);
      }
    return (r);
  }
}

RSA *RSA_new_method (ENGINE * engine);
int RSA_private_encrypt (int flen, unsigned char const *from___0,
			 unsigned char *to, RSA * rsa, int padding);
RSA_METHOD const *ENGINE_get_RSA (ENGINE const *e);
int ENGINE_finish (ENGINE * e);
ENGINE *ENGINE_get_default_RSA (void);
char const RSA_version[39] = {
  (char const) 'R', (char const) 'S', (char const) 'A', (char const) ' ',
    (char const) 'p', (char const) 'a', (char const) 'r', (char const) 't',
    (char const) ' ', (char const) 'o', (char const) 'f', (char const) ' ',
    (char const) 'O', (char const) 'p', (char const) 'e', (char const) 'n',
    (char const) 'S', (char const) 'S', (char const) 'L', (char const) ' ',
    (char const) '0', (char const) '.', (char const) '9', (char const) '.',
    (char const) '8', (char const) 'g', (char const) ' ', (char const) '1',
    (char const) '9', (char const) ' ', (char const) 'O', (char const) 'c',
    (char const) 't', (char const) ' ', (char const) '2', (char const) '0',
    (char const) '0', (char const) '7', (char const) '\000'
};

static RSA_METHOD const *default_RSA_meth = (RSA_METHOD const *) ((void *) 0);
RSA *
RSA_new (void)
{
  RSA *r;
  RSA *tmp;
  {
    tmp = RSA_new_method ((ENGINE *) ((void *) 0));
    r = tmp;
    return (r);
  }
}

RSA_METHOD const *
RSA_get_default_method (void)
{
  {
    if ((unsigned int) default_RSA_meth == (unsigned int) ((void *) 0))
      {
	default_RSA_meth = RSA_PKCS1_SSLeay ();
      }
    return (default_RSA_meth);
  }
}

RSA_METHOD const *
RSA_get_method (RSA const *rsa)
{
  {
    return ((RSA_METHOD const *) rsa->meth);
  }
}

int
RSA_set_method (RSA * rsa, RSA_METHOD const *meth___0)
{
  RSA_METHOD const *mtmp;
  {
    mtmp = rsa->meth;
    if (mtmp->finish)
      {
	(*(mtmp->finish)) (rsa);
      }
    if (rsa->engine)
      {
	ENGINE_finish (rsa->engine);
	rsa->engine = (ENGINE *) ((void *) 0);
      }
    rsa->meth = meth___0;
    if (meth___0->init)
      {
	(*(meth___0->init)) (rsa);
      }
    return (1);
  }
}

RSA *
RSA_new_method (ENGINE * engine)
{
  RSA *ret___3;
  void *tmp;
  int tmp___0;
  int tmp___1;
  {
    tmp = CRYPTO_malloc ((int) sizeof (RSA), "rsa_lib.c", 132);
    ret___3 = (RSA *) tmp;
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	ERR_put_error (4, 106, 65, "rsa_lib.c", 135);
	return ((RSA *) ((void *) 0));
      }
    ret___3->meth = RSA_get_default_method ();
    if (engine)
      {
	tmp___0 = ENGINE_init (engine);
	if (!tmp___0)
	  {
	    ERR_put_error (4, 106, 38, "rsa_lib.c", 145);
	    CRYPTO_free ((void *) ret___3);
	    return ((RSA *) ((void *) 0));
	  }
	ret___3->engine = engine;
      }
    else
      {
	ret___3->engine = ENGINE_get_default_RSA ();
      }
    if (ret___3->engine)
      {
	ret___3->meth = ENGINE_get_RSA ((ENGINE const *) ret___3->engine);
	if (!ret___3->meth)
	  {
	    ERR_put_error (4, 106, 38, "rsa_lib.c", 159);
	    ENGINE_finish (ret___3->engine);
	    CRYPTO_free ((void *) ret___3);
	    return ((RSA *) ((void *) 0));
	  }
      }
    ret___3->pad = 0;
    ret___3->version = 0L;
    ret___3->n = (BIGNUM *) ((void *) 0);
    ret___3->e = (BIGNUM *) ((void *) 0);
    ret___3->d = (BIGNUM *) ((void *) 0);
    ret___3->p = (BIGNUM *) ((void *) 0);
    ret___3->q = (BIGNUM *) ((void *) 0);
    ret___3->mt_blinding = (BN_BLINDING *) ((void *) 0);
    ret___3->bignum_data = (char *) ((void *) 0);
    ret___3->flags = (int) (ret___3->meth)->flags;
    CRYPTO_new_ex_data (6, (void *) ret___3, &ret___3->ex_data);
    if ((unsigned int) (ret___3->meth)->init != (unsigned int) ((void *) 0))
      {
	tmp___1 = (*((ret___3->meth)->init)) (ret___3);
	if (!tmp___1)
	  {
	    if (ret___3->engine)
	      {
		ENGINE_finish (ret___3->engine);
	      }
	    CRYPTO_free_ex_data (6, (void *) ret___3, &ret___3->ex_data);
	    CRYPTO_free ((void *) ret___3);
	    ret___3 = (RSA *) ((void *) 0);
	  }
      }
    return (ret___3);
  }
}

void
RSA_free (RSA * r)
{
  int i;
  {
    if ((unsigned int) r == (unsigned int) ((void *) 0))
      {
	return;
      }
    i = CRYPTO_add_lock (&r->references, -1, lock9, "rsa_lib.c", 205);
    if (i > 0)
      {
	return;
      }
    if ((r->meth)->finish)
      {
	(*((r->meth)->finish)) (r);
      }
    if (r->engine)
      {
	ENGINE_finish (r->engine);
      }
    CRYPTO_free_ex_data (6, (void *) r, &r->ex_data);
    if ((unsigned int) r->mt_blinding != (unsigned int) ((void *) 0))
      {
	BN_BLINDING_free (r->mt_blinding);
      }
    if ((unsigned int) r->bignum_data != (unsigned int) ((void *) 0))
      {
	CRYPTO_free_locked ((void *) r->bignum_data);
      }
    CRYPTO_free ((void *) r);
    return;
  }
}

int
RSA_public_encrypt (int flen, unsigned char const *from___0,
		    unsigned char *to, RSA * rsa, int padding)
{
  int tmp;
  {
    tmp = (*((rsa->meth)->rsa_pub_enc)) (flen, from___0, to, rsa, padding);
    return (tmp);
  }
}

int
RSA_private_encrypt (int flen, unsigned char const *from___0,
		     unsigned char *to, RSA * rsa, int padding)
{
  int tmp;
  {
    tmp = (*((rsa->meth)->rsa_priv_enc)) (flen, from___0, to, rsa, padding);
    return (tmp);
  }
}

int
RSA_private_decrypt (int flen, unsigned char const *from___0,
		     unsigned char *to, RSA * rsa, int padding)
{
  int tmp;
  {
    tmp = (*((rsa->meth)->rsa_priv_dec)) (flen, from___0, to, rsa, padding);
    return (tmp);
  }
}

static BIGNUM *
rsa_get_public_exp (BIGNUM const *d, BIGNUM const *p, BIGNUM const *q,
		    BN_CTX * ctx)
{
  BIGNUM *ret___3;
  BIGNUM *r0;
}

BN_BLINDING *
RSA_setup_blinding (RSA * rsa, BN_CTX * in_ctx)
{
  BIGNUM local_n;
  BIGNUM *e;
  BIGNUM *n;
  BN_CTX *ctx;
  BN_BLINDING *ret___3;
  int tmp;
  unsigned long tmp___0;
  {
    ret___3 = (BN_BLINDING *) ((void *) 0);
    if ((unsigned int) in_ctx == (unsigned int) ((void *) 0))
      {
	ctx = BN_CTX_new ();
	if ((unsigned int) ctx == (unsigned int) ((void *) 0))
	  {
	    return ((BN_BLINDING *) 0);
	  }
      }
    else
      {
	ctx = in_ctx;
      }
    BN_CTX_start (ctx);
    e = BN_CTX_get (ctx);
    if ((unsigned int) e == (unsigned int) ((void *) 0))
      {
	ERR_put_error (4, 136, 65, "rsa_lib.c", 380);
	goto err;
      }
    if ((unsigned int) rsa->e == (unsigned int) ((void *) 0))
      {
	e =
	  rsa_get_public_exp ((BIGNUM const *) rsa->d,
			      (BIGNUM const *) rsa->p,
			      (BIGNUM const *) rsa->q, ctx);
	if ((unsigned int) e == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (4, 136, 140, "rsa_lib.c", 389);
	    goto err;
	  }
      }
    else
      {
	e = rsa->e;
      }
    tmp = RAND_status ();
    if (tmp == 0)
      {
	if ((unsigned int) rsa->d != (unsigned int) ((void *) 0))
	  {
	    if ((unsigned int) (rsa->d)->d != (unsigned int) ((void *) 0))
	      {
		RAND_add ((void const *) (rsa->d)->d,
			  (int) ((unsigned int) (rsa->d)->dmax *
				 sizeof (*((rsa->d)->d + 0))), 0.0);
	      }
	  }
      }
    if (!(rsa->flags & 256))
      {
	n = &local_n;
	n->d = (rsa->n)->d;
	n->top = (rsa->n)->top;
	n->dmax = (rsa->n)->dmax;
	n->neg = (rsa->n)->neg;
	n->flags = (((n->flags & 1) | ((rsa->n)->flags & -2)) | 2) | 4;
      }
    else
      {
	n = rsa->n;
      }
    ret___3 =
      BN_BLINDING_create_param ((BN_BLINDING *) ((void *) 0),
				(BIGNUM const *) e, n, ctx,
				(int (*)
				 (BIGNUM * r, BIGNUM const *a,
				  BIGNUM const *p, BIGNUM const *m,
				  BN_CTX * ctx,
				  BN_MONT_CTX *
				  m_ctx)) (rsa->meth)->bn_mod_exp,
				rsa->_method_mod_n);
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	ERR_put_error (4, 136, 3, "rsa_lib.c", 417);
	goto err;
      }
    tmp___0 = CRYPTO_thread_id ();
    BN_BLINDING_set_thread_id (ret___3, tmp___0);
  err:BN_CTX_end (ctx);
    if ((unsigned int) in_ctx == (unsigned int) ((void *) 0))
      {
	BN_CTX_free (ctx);
      }
    if ((unsigned int) rsa->e == (unsigned int) ((void *) 0))
      {
	BN_free (e);
      }
    return (ret___3);
  }
}

int
RSA_sign (int type, unsigned char const *m___10, unsigned int m_len,
	  unsigned char *sigret, unsigned int *siglen, RSA * rsa)
{
  X509_SIG sig;
  ASN1_TYPE parameter;
  int i;
  int j;
  int ret___3;
  unsigned char *p;
  unsigned char *tmps;
  unsigned char const *s;
  X509_ALGOR algor;
  ASN1_OCTET_STRING digest;
  int tmp;
  void *tmp___0;
  {
    ret___3 = 1;
    tmps = (unsigned char *) ((void *) 0);
    s = (unsigned char const *) ((void *) 0);
    if (rsa->flags & 64)
      {
	if ((rsa->meth)->rsa_sign)
	  {
	    tmp =
	      (*((rsa->meth)->rsa_sign)) (type, m___10, m_len, sigret, siglen,
					  (RSA const *) rsa);
	    return (tmp);
	  }
      }
    if (type == 114)
      {
	if (m_len != 36U)
	  {
	    ERR_put_error (4, 117, 131, "rsa_sign.c", 87);
	    return (0);
	  }
	i = 36;
	s = m___10;
      }
    else
      {
	sig.algor = &algor;
	(sig.algor)->algorithm = OBJ_nid2obj (type);
	if ((unsigned int) (sig.algor)->algorithm ==
	    (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (4, 117, 117, "rsa_sign.c", 97);
	    return (0);
	  }
	if (((sig.algor)->algorithm)->length == 0)
	  {
	    ERR_put_error (4, 117, 116, "rsa_sign.c", 102);
	    return (0);
	  }
	parameter.type = 5;
	parameter.value.ptr = (char *) ((void *) 0);
	(sig.algor)->parameter = &parameter;
	sig.digest = &digest;
	(sig.digest)->data = (unsigned char *) m___10;
	(sig.digest)->length = (int) m_len;
	i = i2d_X509_SIG (&sig, (unsigned char **) ((void *) 0));
      }
    j = RSA_size ((RSA const *) rsa);
    if (i > j - 11)
      {
	ERR_put_error (4, 117, 112, "rsa_sign.c", 118);
	return (0);
      }
    if (type != 114)
      {
	tmp___0 =
	  CRYPTO_malloc ((int) ((unsigned int) j) + 1, "rsa_sign.c", 122);
	tmps = (unsigned char *) tmp___0;
	if ((unsigned int) tmps == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (4, 117, 65, "rsa_sign.c", 125);
	    return (0);
	  }
	p = tmps;
	i2d_X509_SIG (&sig, &p);
	s = (unsigned char const *) tmps;
      }
    i = RSA_private_encrypt (i, s, sigret, rsa, 1);
    if ((unsigned int) s != (unsigned int) ((void *) 0))
      {
	OPENSSL_cleanse ((void *) s, siglen);
	CRYPTO_free ((void *) s);
      }
    return (ret___3);
  }
}

int
RSA_padding_add_PKCS1_type_2 (unsigned char *to, int tlen,
			      unsigned char const *from___0, int flen)
{
  int i;
  int j;
  unsigned char *p;
  unsigned char *tmp;
  unsigned char *tmp___0;
  int tmp___1;
  int tmp___2;
  unsigned char *tmp___3;
  {
    if (flen > tlen - 11)
      {
	ERR_put_error (4, 109, 110, "rsa_pk1.c", 151);
	return (0);
      }
    p = to;
    tmp = p;
    i = 0;
    while (i < j)
      {
	if ((int) *p == 0)
	  {
	    while (1)
	      {
		tmp___2 = RAND_bytes (p, 1);
		if (tmp___2 <= 0)
		  {
		    return (0);
		  }
		if (!((int) *p == 0))
		  {
		    break;
		  }
	      }
	  }
	p++;
	i++;
      }
    tmp___3 = p;
    p++;
    *tmp___3 = (unsigned char) '\000';
    memcpy ((void *__restrict) p, (void const *__restrict) from___0,
	    (unsigned int) flen);
    return (1);
  }
}

int
RSA_padding_check_PKCS1_type_2 (unsigned char *to, int tlen,
				unsigned char const *from___0, int flen,
				int num)
{
  int i;
  int j;
  unsigned char const *p;
  unsigned char const *tmp;
  unsigned char const *tmp___0;
  {
    p = from___0;
    return (tmp___0);
  }
}

#pragma merger(0,"/tmp/cil-2iV4vvGF.i","-march=pentium,-O3,-fomit-frame-pointer,-Wall")
RSA *
RSA_generate_key (int bits___0, unsigned long e_value,
		  void (*callback) (int, int, void *), void *cb_arg)
{
  BN_GENCB cb;
  int i;
  RSA *rsa;
  RSA *tmp;
  BIGNUM *e;
  BIGNUM *tmp___0;
  int tmp___1;
  BN_GENCB *tmp_gencb;
  int tmp___2;
  {
    tmp = RSA_new ();
    tmp_gencb->arg = cb_arg;
    tmp_gencb->cb.cb_1 = callback;
    tmp___2 = RSA_generate_key_ex (rsa, bits___0, e, &cb);
    if (tmp___2)
      {
	BN_free (e);
	return (rsa);
      }
  err:if (e)
      {
	BN_free (e);
      }
    if (rsa)
      {
	RSA_free (rsa);
      }
    return ((RSA *) 0);
  }
}

ECDSA_METHOD const *ECDSA_OpenSSL (void);
void ECDSA_set_default_method (ECDSA_METHOD const *meth___0);
ECDSA_METHOD const *ECDSA_get_default_method (void);
ECDSA_DATA *ecdsa_check (EC_KEY * key);
ECDSA_METHOD const *ENGINE_get_ECDSA (ENGINE const *e);
ENGINE *ENGINE_get_default_ECDSA (void);
char const ECDSA_version[41] = {
  (char const) 'E', (char const) 'C', (char const) 'D', (char const) 'S',
    (char const) 'A', (char const) ' ', (char const) 'p', (char const) 'a',
    (char const) 'r', (char const) 't', (char const) ' ', (char const) 'o',
    (char const) 'f', (char const) ' ', (char const) 'O', (char const) 'p',
    (char const) 'e', (char const) 'n', (char const) 'S', (char const) 'S',
    (char const) 'L', (char const) ' ', (char const) '0', (char const) '.',
    (char const) '9', (char const) '.', (char const) '8', (char const) 'g',
    (char const) ' ', (char const) '1', (char const) '9', (char const) ' ',
    (char const) 'O', (char const) 'c', (char const) 't', (char const) ' ',
    (char const) '2', (char const) '0', (char const) '0', (char const) '7',
    (char const) '\000'
};

static ECDSA_METHOD const *default_ECDSA_method =
  (ECDSA_METHOD const *) ((void *) 0);
ECDSA_METHOD const *
ECDSA_get_default_method (void)
{
  {
    if (!default_ECDSA_method)
      {
	default_ECDSA_method = ECDSA_OpenSSL ();
      }
    return (default_ECDSA_method);
  }
}

int
ECDSA_set_method (EC_KEY * eckey, ECDSA_METHOD const *meth___0)
{
  ECDSA_METHOD const *mtmp;
}

static ECDSA_DATA *
ECDSA_DATA_new_method (ENGINE * engine)
{
  ECDSA_DATA *ret___3;
  void *tmp;
  {
    tmp = CRYPTO_malloc ((int) sizeof (ECDSA_DATA), "ecs_lib.c", 111);
    ret___3 = (ECDSA_DATA *) tmp;
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	ERR_put_error (42, 100, 65, "ecs_lib.c", 114);
	return ((ECDSA_DATA *) ((void *) 0));
      }
    ret___3->init = (int (*)(EC_KEY *)) ((void *) 0);
    ret___3->meth = ECDSA_get_default_method ();
    ret___3->engine = engine;
    if (!ret___3->engine)
      {
	ret___3->engine = ENGINE_get_default_ECDSA ();
      }
    if (ret___3->engine)
      {
	ret___3->meth = ENGINE_get_ECDSA ((ENGINE const *) ret___3->engine);
	if (!ret___3->meth)
	  {
	    ERR_put_error (42, 100, 38, "ecs_lib.c", 130);
	    ENGINE_finish (ret___3->engine);
	    CRYPTO_free ((void *) ret___3);
	    return ((ECDSA_DATA *) ((void *) 0));
	  }
      }
    ret___3->flags = (int) (ret___3->meth)->flags;
    CRYPTO_new_ex_data (12, (void *) ret___3, &ret___3->ex_data);
    return (ret___3);
  }
}

static void *
ecdsa_data_new (void)
{
  ECDSA_DATA *tmp;
  {
    tmp = ECDSA_DATA_new_method ((ENGINE *) ((void *) 0));
    return ((void *) tmp);
  }
}

static void *
ecdsa_data_dup (void *data___0)
{
  ECDSA_DATA *r;
  void *tmp;
  {
    r = (ECDSA_DATA *) data___0;
    if ((unsigned int) r == (unsigned int) ((void *) 0))
      {
	return ((void *) 0);
      }
    tmp = ecdsa_data_new ();
    return (tmp);
  }
}

static void
ecdsa_data_free (void *data___0)
{
  ECDSA_DATA *r;
  {
    r = (ECDSA_DATA *) data___0;
    if (r->engine)
      {
	ENGINE_finish (r->engine);
      }
    CRYPTO_free_ex_data (12, (void *) r, &r->ex_data);
    OPENSSL_cleanse ((void *) r, sizeof (ECDSA_DATA));
    CRYPTO_free ((void *) r);
    return;
  }
}

ECDSA_DATA *
ecdsa_check (EC_KEY * key)
{
  ECDSA_DATA *ecdsa_data;
  void *data___0;
  void *tmp;
  void *tmp___0;
  {
    tmp =
      EC_KEY_get_key_method_data (key, &ecdsa_data_dup, &ecdsa_data_free,
				  &ecdsa_data_free);
    data___0 = tmp;
    if ((unsigned int) data___0 == (unsigned int) ((void *) 0))
      {
	tmp___0 = ecdsa_data_new ();
	ecdsa_data = (ECDSA_DATA *) tmp___0;
	if ((unsigned int) ecdsa_data == (unsigned int) ((void *) 0))
	  {
	    return ((ECDSA_DATA *) ((void *) 0));
	  }
	EC_KEY_insert_key_method_data (key, (void *) ecdsa_data,
				       &ecdsa_data_dup, &ecdsa_data_free,
				       &ecdsa_data_free);
      }
    else
      {
	ecdsa_data = (ECDSA_DATA *) data___0;
      }
    return (ecdsa_data);
  }
}

static ECDSA_SIG *ecdsa_do_sign (unsigned char const *dgst, int dgst_len,
				 BIGNUM const *in_kinv, BIGNUM const *in_r,
				 EC_KEY * eckey);
static int ecdsa_sign_setup (EC_KEY * eckey, BN_CTX * ctx_in, BIGNUM ** kinvp,
			     BIGNUM ** rp);
static int ecdsa_do_verify (unsigned char const *dgst, int dgst_len,
			    ECDSA_SIG const *sig, EC_KEY * eckey);
static ECDSA_METHOD openssl_ecdsa_meth = {
  "OpenSSL ECDSA method", &ecdsa_do_sign, &ecdsa_sign_setup, &ecdsa_do_verify,
    0, (char *) ((void *) 0)
};

ECDSA_METHOD const *
ECDSA_OpenSSL (void)
{
  {
    return ((ECDSA_METHOD const *) (&openssl_ecdsa_meth));
  }
}

static int
ecdsa_sign_setup (EC_KEY * eckey, BN_CTX * ctx_in, BIGNUM ** kinvp,
		  BIGNUM ** rp)
{
  BN_CTX *ctx;
  BIGNUM *k;
  BIGNUM *r;
  BIGNUM *order___0;
  BIGNUM *X;
  EC_POINT *tmp_point;
  int tmp___6;
  BIGNUM *tmp___7;
  {
    ctx = (BN_CTX *) ((void *) 0);
    k = (BIGNUM *) ((void *) 0);
    r = (BIGNUM *) ((void *) 0);
    order___0 = (BIGNUM *) ((void *) 0);
    X = (BIGNUM *) ((void *) 0);
  }
}

static ECDSA_SIG *
ecdsa_do_sign (unsigned char const *dgst, int dgst_len, BIGNUM const *in_kinv,
	       BIGNUM const *in_r, EC_KEY * eckey)
{
  int ok;
  BIGNUM *kinv;
  BIGNUM *s;
  BIGNUM *m___10;
  BIGNUM *tmp;
  BIGNUM *order___0;
  BIGNUM const *ckinv;
  BN_CTX *ctx;
  EC_GROUP const *group;
  ECDSA_SIG *ret___3;
  ECDSA_DATA *ecdsa;
  BIGNUM const *priv_key;
  int tmp___0;
  int tmp___1;
  BIGNUM *tmp___2;
  int tmp___3;
  BIGNUM *tmp___4;
  int tmp___5;
  int tmp___6;
  int tmp___7;
  {
    ok = 0;
    kinv = (BIGNUM *) ((void *) 0);
    m___10 = (BIGNUM *) ((void *) 0);
    tmp = (BIGNUM *) ((void *) 0);
    order___0 = (BIGNUM *) ((void *) 0);
    ctx = (BN_CTX *) ((void *) 0);
    ecdsa = ecdsa_check (eckey);
    group = EC_KEY_get0_group ((EC_KEY const *) eckey);
    priv_key = EC_KEY_get0_private_key ((EC_KEY const *) eckey);
    if ((unsigned int) group == (unsigned int) ((void *) 0))
      {
	ERR_put_error (42, 101, 67, "ecs_ossl.c", 230);
	return ((ECDSA_SIG *) ((void *) 0));
      }
    else
      {
	if ((unsigned int) priv_key == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (42, 101, 67, "ecs_ossl.c", 230);
	    return ((ECDSA_SIG *) ((void *) 0));
	  }
	else
	  {
	    if ((unsigned int) ecdsa == (unsigned int) ((void *) 0))
	      {
		ERR_put_error (42, 101, 67, "ecs_ossl.c", 230);
		return ((ECDSA_SIG *) ((void *) 0));
	      }
      }}
    ret___3 = ECDSA_SIG_new ();
    if (!ret___3)
      {
	BN_clear_free (kinv);
      }
    return (ret___3);
  }
}

ECDSA_SIG *ECDSA_do_sign_ex (unsigned char const *dgst, int dlen,
			     BIGNUM const *kinv, BIGNUM const *rp,
			     EC_KEY * eckey);
int ECDSA_sign_ex (int type, unsigned char const *dgst, int dlen,
		   unsigned char *sig, unsigned int *siglen,
		   BIGNUM const *kinv, BIGNUM const *r, EC_KEY * eckey);
ECDSA_SIG *
ECDSA_do_sign (unsigned char const *dgst, int dlen, EC_KEY * eckey)
{
  ECDSA_SIG *tmp;
  {
    tmp =
      ECDSA_do_sign_ex (dgst, dlen, (BIGNUM const *) ((void *) 0),
			(BIGNUM const *) ((void *) 0), eckey);
    return (tmp);
  }
}

ECDSA_SIG *
ECDSA_do_sign_ex (unsigned char const *dgst, int dlen, BIGNUM const *kinv,
		  BIGNUM const *rp, EC_KEY * eckey)
{
  ECDSA_DATA *ecdsa;
  ECDSA_DATA *tmp;
  ECDSA_SIG *tmp___0;
  {
    tmp = ecdsa_check (eckey);
    ecdsa = tmp;
    if ((unsigned int) ecdsa == (unsigned int) ((void *) 0))
      {
	return ((ECDSA_SIG *) ((void *) 0));
      }
    tmp___0 = (*((ecdsa->meth)->ecdsa_do_sign)) (dgst, dlen, kinv, rp, eckey);
    return (tmp___0);
  }
}

int
ECDSA_sign (int type, unsigned char const *dgst, int dlen, unsigned char *sig,
	    unsigned int *siglen, EC_KEY * eckey)
{
  int tmp;
  {
    tmp =
      ECDSA_sign_ex (type, dgst, dlen, sig, siglen,
		     (BIGNUM const *) ((void *) 0),
		     (BIGNUM const *) ((void *) 0), eckey);
    return (tmp);
  }
}

int
ECDSA_sign_ex (int type, unsigned char const *dgst, int dlen,
	       unsigned char *sig, unsigned int *siglen, BIGNUM const *kinv,
	       BIGNUM const *r, EC_KEY * eckey)
{
  ECDSA_SIG *s;
  int tmp;
  {
    s = ECDSA_do_sign_ex (dgst, dlen, kinv, r, eckey);
    if ((unsigned int) s == (unsigned int) ((void *) 0))
      {
	*siglen = 0U;
	return (0);
      }
    tmp = i2d_ECDSA_SIG ((ECDSA_SIG const *) s, &sig);
    *siglen = (unsigned int) tmp;
    ECDSA_SIG_free (s);
    return (1);
  }
}

int
ECDSA_sign_setup (EC_KEY * eckey, BN_CTX * ctx_in, BIGNUM ** kinvp,
		  BIGNUM ** rp)
{
  ECDSA_DATA *ecdsa;
  ECDSA_DATA *tmp;
  int tmp___0;
  {
    tmp = ecdsa_check (eckey);
    ecdsa = tmp;
    if ((unsigned int) ecdsa == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    tmp___0 = (*((ecdsa->meth)->ecdsa_sign_setup)) (eckey, ctx_in, kinvp, rp);
    return (tmp___0);
  }
}

int
ECDSA_do_verify (unsigned char const *dgst, int dgst_len,
		 ECDSA_SIG const *sig, EC_KEY * eckey)
{
  ECDSA_DATA *ecdsa;
  ECDSA_DATA *tmp;
  int tmp___0;
  {
    tmp = ecdsa_check (eckey);
    ecdsa = tmp;
    if ((unsigned int) ecdsa == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    tmp___0 =
      (*((ecdsa->meth)->ecdsa_do_verify)) (dgst, dgst_len, sig, eckey);
    return (tmp___0);
  }
}

int
ECDSA_verify (int type, unsigned char const *dgst, int dgst_len,
	      unsigned char const *sigbuf, int sig_len, EC_KEY * eckey)
{
  ECDSA_SIG *s;
  int ret___3;
  ECDSA_SIG *tmp;
  {
    ret___3 = -1;
    s = ECDSA_SIG_new ();
    if ((unsigned int) s == (unsigned int) ((void *) 0))
      {
	return (ret___3);
      }
    tmp = d2i_ECDSA_SIG (&s, &sigbuf, (long) sig_len);
    if ((unsigned int) tmp == (unsigned int) ((void *) 0))
      {
	goto err;
      }
    ret___3 = ECDSA_do_verify (dgst, dgst_len, (ECDSA_SIG const *) s, eckey);
  err:ECDSA_SIG_free (s);
    return (ret___3);
  }
}

int DH_set_method (DH * dh, DH_METHOD const *meth___0);
DH *DH_new_method (ENGINE * engine);
ENGINE *ENGINE_get_default_DH (void);
char const DH_version[50] = {
  (char const) 'D', (char const) 'i', (char const) 'f', (char const) 'f',
    (char const) 'i', (char const) 'e', (char const) '-', (char const) 'H',
    (char const) 'e', (char const) 'l', (char const) 'l', (char const) 'm',
    (char const) 'a', (char const) 'n', (char const) ' ', (char const) 'p',
    (char const) 'a', (char const) 'r', (char const) 't', (char const) ' ',
    (char const) 'o', (char const) 'f', (char const) ' ', (char const) 'O',
    (char const) 'p', (char const) 'e', (char const) 'n', (char const) 'S',
    (char const) 'S', (char const) 'L', (char const) ' ', (char const) '0',
    (char const) '.', (char const) '9', (char const) '.', (char const) '8',
    (char const) 'g', (char const) ' ', (char const) '1', (char const) '9',
    (char const) ' ', (char const) 'O', (char const) 'c', (char const) 't',
    (char const) ' ', (char const) '2', (char const) '0', (char const) '0',
    (char const) '7', (char const) '\000'
};

DH *
DH_new (void)
{
  DH *tmp;
  {
    tmp = DH_new_method ((ENGINE *) ((void *) 0));
    return (tmp);
  }
}

DH *
DH_new_method (ENGINE * engine)
{
  DH *ret___3;
  void *tmp;
  int tmp___0;
  int tmp___1;
  {
    tmp = CRYPTO_malloc ((int) sizeof (DH), "dh_lib.c", 111);
    ret___3 = (DH *) tmp;
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	ERR_put_error (5, 105, 65, "dh_lib.c", 114);
	return ((DH *) ((void *) 0));
      }
    ret___3->meth = DH_get_default_method ();
    if (engine)
      {
	tmp___0 = ENGINE_init (engine);
	if (!tmp___0)
	  {
	    ERR_put_error (5, 105, 38, "dh_lib.c", 124);
	    CRYPTO_free ((void *) ret___3);
	    return ((DH *) ((void *) 0));
	  }
	ret___3->engine = engine;
      }
    else
      {
	ret___3->engine = ENGINE_get_default_DH ();
      }
    if (ret___3->engine)
      {
	ret___3->meth = ENGINE_get_DH ((ENGINE const *) ret___3->engine);
	if (!ret___3->meth)
	  {
	    ERR_put_error (5, 105, 38, "dh_lib.c", 137);
	    ENGINE_finish (ret___3->engine);
	    CRYPTO_free ((void *) ret___3);
	    return ((DH *) ((void *) 0));
	  }
      }
    ret___3->pad = 0;
    ret___3->version = 0;
    ret___3->counter = (BIGNUM *) ((void *) 0);
    ret___3->method_mont_p = (BN_MONT_CTX *) ((void *) 0);
    ret___3->references = 1;
    ret___3->flags = (int) (ret___3->meth)->flags;
    CRYPTO_new_ex_data (8, (void *) ret___3, &ret___3->ex_data);
    if ((unsigned int) (ret___3->meth)->init != (unsigned int) ((void *) 0))
      {
	tmp___1 = (*((ret___3->meth)->init)) (ret___3);
	if (!tmp___1)
	  {
	    if (ret___3->engine)
	      {
		ENGINE_finish (ret___3->engine);
	      }
	    CRYPTO_free_ex_data (8, (void *) ret___3, &ret___3->ex_data);
	    CRYPTO_free ((void *) ret___3);
	    ret___3 = (DH *) ((void *) 0);
	  }
      }
    return (ret___3);
  }
}

void
DH_free (DH * r)
{
  int i;
  {
    if ((unsigned int) r == (unsigned int) ((void *) 0))
      {
	return;
      }
    i = CRYPTO_add_lock (&r->references, -1, lock26, "dh_lib.c", 178);
    if (i > 0)
      {
	return;
      }
    if ((r->meth)->finish)
      {
	(*((r->meth)->finish)) (r);
      }
    if (r->engine)
      {
	ENGINE_finish (r->engine);
      }
    if ((unsigned int) r->priv_key != (unsigned int) ((void *) 0))
      {
	BN_clear_free (r->priv_key);
      }
    CRYPTO_free ((void *) r);
    return;
  }
}

ECDH_METHOD const *ENGINE_get_ECDH (ENGINE const *e);
ENGINE *ENGINE_get_default_ECDH (void);
char const ECDH_version[40] = {
  (char const) 'E', (char const) 'C', (char const) 'D', (char const) 'H',
    (char const) ' ', (char const) 'p', (char const) 'a', (char const) 'r',
    (char const) 't', (char const) ' ', (char const) 'o', (char const) 'f',
    (char const) ' ', (char const) 'O', (char const) 'p', (char const) 'e',
    (char const) 'n', (char const) 'S', (char const) 'S', (char const) 'L',
    (char const) ' ', (char const) '0', (char const) '.', (char const) '9',
    (char const) '.', (char const) '8', (char const) 'g', (char const) ' ',
    (char const) '1', (char const) '9', (char const) ' ', (char const) 'O',
    (char const) 'c', (char const) 't', (char const) ' ', (char const) '2',
    (char const) '0', (char const) '0', (char const) '7', (char const) '\000'
};

static ECDH_METHOD const *default_ECDH_method =
  (ECDH_METHOD const *) ((void *) 0);
static void *ecdh_data_new (void);
static void *ecdh_data_dup (void *data___0);
static void ecdh_data_free (void *data___0);
void
ECDH_set_default_method (ECDH_METHOD const *meth___0)
{
  {
    default_ECDH_method = meth___0;
    return;
  }
}

static ECDH_DATA *
ECDH_DATA_new_method (ENGINE * engine)
{
  ECDH_DATA *ret___3;
  void *tmp;
  {
    tmp = CRYPTO_malloc ((int) sizeof (ECDH_DATA), "ech_lib.c", 131);
    ret___3 = (ECDH_DATA *) tmp;
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	ERR_put_error (43, 101, 65, "ech_lib.c", 134);
	return ((ECDH_DATA *) ((void *) 0));
      }
    ret___3->init = (int (*)(EC_KEY *)) ((void *) 0);
    ret___3->meth = ECDH_get_default_method ();
    ret___3->engine = engine;
    if (!ret___3->engine)
      {
	ret___3->engine = ENGINE_get_default_ECDH ();
      }
    if (ret___3->engine)
      {
	ret___3->meth = ENGINE_get_ECDH ((ENGINE const *) ret___3->engine);
	if (!ret___3->meth)
	  {
	    ERR_put_error (43, 101, 38, "ech_lib.c", 150);
	    ENGINE_finish (ret___3->engine);
	    CRYPTO_free ((void *) ret___3);
	    return ((ECDH_DATA *) ((void *) 0));
	  }
      }
    ret___3->flags = (int) (ret___3->meth)->flags;
    CRYPTO_new_ex_data (13, (void *) ret___3, &ret___3->ex_data);
    return (ret___3);
  }
}

static void *
ecdh_data_new (void)
{
  ECDH_DATA *tmp;
  {
    tmp = ECDH_DATA_new_method ((ENGINE *) ((void *) 0));
    return ((void *) tmp);
  }
}

ECDH_DATA *
ecdh_check (EC_KEY * key)
{
  ECDH_DATA *ecdh_data;
  void *data___0;
  void *tmp;
  void *tmp___0;
  {
    tmp =
      EC_KEY_get_key_method_data (key, &ecdh_data_dup, &ecdh_data_free,
				  &ecdh_data_free);
    data___0 = tmp;
    if ((unsigned int) data___0 == (unsigned int) ((void *) 0))
      {
	tmp___0 = ecdh_data_new ();
	ecdh_data = (ECDH_DATA *) tmp___0;
	if ((unsigned int) ecdh_data == (unsigned int) ((void *) 0))
	  {
	    return ((ECDH_DATA *) ((void *) 0));
	  }
	EC_KEY_insert_key_method_data (key, (void *) ecdh_data,
				       &ecdh_data_dup, &ecdh_data_free,
				       &ecdh_data_free);
      }
    else
      {
	ecdh_data = (ECDH_DATA *) data___0;
      }
    return (ecdh_data);
  }
}

int
ECDH_get_ex_new_index (long argl, void *argp, CRYPTO_EX_new * new_func,
		       CRYPTO_EX_dup * dup_func,
		       CRYPTO_EX_free * free_func___0)
{
  int tmp;
  {
    tmp =
      CRYPTO_get_ex_new_index (13, argl, argp, new_func, dup_func,
			       free_func___0);
    return (tmp);
  }
}

int
ECDH_set_ex_data (EC_KEY * d, int idx, void *arg)
{
  ECDH_DATA *ecdh;
  int tmp;
  {
    ecdh = ecdh_check (d);
    if ((unsigned int) ecdh == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    tmp = CRYPTO_set_ex_data (&ecdh->ex_data, idx, arg);
    return (tmp);
  }
}

#pragma merger(0,"/tmp/cil-uHVXhIv5.i","-march=pentium,-O3,-fomit-frame-pointer,-Wall")
int
ECDH_compute_key (void *out, size_t outlen, EC_POINT const *pub_key,
		  EC_KEY * eckey, void *(*KDF) (void const *in, size_t inlen,
						void *out, size_t * outlen))
{
  ECDH_DATA *ecdh;
  ECDH_DATA *tmp;
  int tmp___0;
  {
    tmp = ecdh_check (eckey);
    ecdh = tmp;
    if ((unsigned int) ecdh == (unsigned int) ((void *) 0))
      {
	return (0);
      }
    tmp___0 =
      (*((ecdh->meth)->compute_key)) (out, outlen, pub_key, eckey, KDF);
    return (tmp___0);
  }
}

int
engine_free_util (ENGINE * e, int locked)
{
  int i;
  {
    if ((unsigned int) e == (unsigned int) ((void *) 0))
      {
	ERR_put_error (38, 108, 67, "eng_lib.c", 112);
	return (0);
      }
    if (locked)
      {
	i = CRYPTO_add_lock (&e->struct_ref, -1, lock30, "eng_lib.c", 116);
      }
    else
      {
	(e->struct_ref)--;
	i = e->struct_ref;
      }
    if (i > 0)
      {
	return (1);
      }
    if (e->destroy)
      {
	(*(e->destroy)) (e);
      }
    CRYPTO_free_ex_data (9, (void *) e, &e->ex_data);
    CRYPTO_free ((void *) e);
    return (1);
  }
}

static STACK *cleanup_stack = (STACK *) ((void *) 0);
static int
int_cleanup_check (int create)
{
}

void
engine_cleanup_add_last (ENGINE_CLEANUP_CB * cb)
{
  ENGINE_CLEANUP_ITEM *item;
}

static void
engine_cleanup_cb_free (ENGINE_CLEANUP_ITEM * item)
{
  {
    (*(item->cb)) ();
    CRYPTO_free ((void *) item);
    return;
  }
}

void
ENGINE_cleanup (void)
{
  int tmp;
  {
    tmp = int_cleanup_check (0);
    if (tmp)
      {
	sk_pop_free (cleanup_stack,
		     (void (*)(void *)) (&engine_cleanup_cb_free));
	cleanup_stack = (STACK *) ((void *) 0);
      }
    RAND_set_rand_method ((RAND_METHOD const *) ((void *) 0));
    return;
  }
}

int
ENGINE_get_ex_new_index (long argl, void *argp, CRYPTO_EX_new * new_func,
			 CRYPTO_EX_dup * dup_func,
			 CRYPTO_EX_free * free_func___0)
{
  int tmp;
  {
    tmp =
      CRYPTO_get_ex_new_index (9, argl, argp, new_func, dup_func,
			       free_func___0);
    return (tmp);
  }
}

int ENGINE_up_ref (ENGINE * e);
static void
engine_list_cleanup (void)
{
  ENGINE *iterator;
}

static int
engine_list_add (ENGINE * e)
{
  int to_return;
  int tmp;
  {
    if (to_return)
      {
	(e->struct_ref)++;
	(e->funct_ref)++;
      }
    return (to_return);
  }
}

int
engine_unlocked_finish (ENGINE * e, int unlock_for_handlers)
{
  int to_return;
  int tmp;
  {
    to_return = 1;
    (e->funct_ref)--;
    if (e->funct_ref == 0)
      {
	if (e->finish)
	  {
	    if (unlock_for_handlers)
	      {
		_write_unlock (&lock30);
	      }
	    to_return = (*(e->finish)) (e);
	    if (unlock_for_handlers)
	      {
		_write_lock (&lock30);
	      }
	    if (!to_return)
	      {
		return (0);
	      }
	  }
      }
    tmp = engine_free_util (e, 0);
    if (!tmp)
      {
	ERR_put_error (38, 191, 106, "eng_init.c", 114);
	return (0);
      }
    return (to_return);
  }
  int ret___3;
  {
    if ((unsigned int) e == (unsigned int) ((void *) 0))
      {
	ERR_put_error (38, 119, 67, "eng_init.c", 126);
	return (0);
      }
    _write_lock (&lock30);
    ret___3 = engine_unlocked_init (e);
    _write_unlock (&lock30);
    return (ret___3);
  }
}

int
ENGINE_finish (ENGINE * e)
{
  int to_return;
  {
    to_return = 1;
    if ((unsigned int) e == (unsigned int) ((void *) 0))
      {
	ERR_put_error (38, 107, 67, "eng_init.c", 142);
	return (0);
      }
    _write_lock (&lock30);
    to_return = engine_unlocked_finish (e, 1);
    _write_unlock (&lock30);
    if (!to_return)
      {
	ERR_put_error (38, 107, 106, "eng_init.c", 150);
	return (0);
      }
    return (to_return);
  }
}

static unsigned int table_flags = 0U;
ENGINE *
engine_table_select (ENGINE_TABLE ** table, int nid)
{
  ENGINE *ret___3;
  ENGINE_PILE tmplate;
  ENGINE_PILE *fnd;
  int initres;
  int loop;
  int tmp;
  void *tmp___0;
  int tmp___1;
  int tmp___2;
  void *tmp___3;
  int tmp___4;
  {
    ret___3 = (ENGINE *) ((void *) 0);
    fnd = (ENGINE_PILE *) ((void *) 0);
    if (fnd->funct)
      {
	tmp___1 = engine_unlocked_init (fnd->funct);
	if (tmp___1)
	  {
	    ret___3 = fnd->funct;
	    goto end;
	  }
      }
    if (fnd->uptodate)
      {
	ret___3 = fnd->funct;
	goto end;
      }
  trynext:tmp___2 = loop;
    loop++;
    tmp___3 = sk_value ((STACK const *) fnd->sk, tmp___2);
    ret___3 = (ENGINE *) tmp___3;
    if (!ret___3)
      {
	goto end;
      }
    if (ret___3->funct_ref > 0)
      {
	initres = engine_unlocked_init (ret___3);
      }
    else
      {
	if (!(table_flags & 1U))
	  {
	    initres = engine_unlocked_init (ret___3);
	  }
	else
	  {
	    initres = 0;
	  }
      }
    if (initres)
      {
	if ((unsigned int) fnd->funct != (unsigned int) ret___3)
	  {
	    tmp___4 = engine_unlocked_init (ret___3);
	    if (tmp___4)
	      {
		if (fnd->funct)
		  {
		    engine_unlocked_finish (fnd->funct, 0);
		  }
		fnd->funct = ret___3;
	      }
	  }
	goto end;
      }
    goto trynext;
  end:if (fnd)
      {
	fnd->uptodate = 1;
      }
    _write_unlock (&lock30);
    ERR_clear_error ();
    return (ret___3);
  }
}

static ENGINE_TABLE *rsa_table = (ENGINE_TABLE *) ((void *) 0);
static int const dummy_nid = (int const) 1;
static void
engine_unregister_all_RSA (void)
{
  {
    engine_table_cleanup (&rsa_table);
    return;
  }
}

int
ENGINE_register_RSA (ENGINE * e)
{
  int tmp;
  {
    if (e->rsa_meth)
      {
	tmp =
	  engine_table_register (&rsa_table, &engine_unregister_all_RSA, e,
				 &dummy_nid, 1, 1);
	return (tmp);
      }
    return (1);
  }
}

ENGINE *
ENGINE_get_default_RSA (void)
{
  ENGINE *tmp;
  {
    tmp = engine_table_select (&rsa_table, (int) dummy_nid);
    return (tmp);
  }
}

static ENGINE_TABLE *ecdsa_table = (ENGINE_TABLE *) ((void *) 0);
static int const dummy_nid___1 = (int const) 1;
void
ENGINE_unregister_ECDSA (ENGINE * e)
{
  {
    engine_table_unregister (&ecdsa_table, e);
    return;
  }
  {
    engine_table_cleanup (&ecdsa_table);
    return;
  }
}

int
ENGINE_set_default_ECDSA (ENGINE * e)
{
}

ENGINE *
ENGINE_get_default_ECDSA (void)
{
  ENGINE *tmp;
  {
    tmp = engine_table_select (&ecdsa_table, (int) dummy_nid___1);
    return (tmp);
  }
}

ECDSA_METHOD const *
ENGINE_get_ECDSA (ENGINE const *e)
{
  {
    return ((ECDSA_METHOD const *) e->ecdsa_meth);
  }
}

int ENGINE_set_DH (ENGINE * e, DH_METHOD const *dh_meth);
static ENGINE_TABLE *dh_table = (ENGINE_TABLE *) ((void *) 0);
static int const dummy_nid___2 = (int const) 1;
void
ENGINE_unregister_DH (ENGINE * e)
{
}

static void
engine_unregister_all_DH (void)
{
  {
    engine_table_cleanup (&dh_table);
    return;
  }
}

int
ENGINE_set_default_DH (ENGINE * e)
{
  int tmp;
  {
    if (e->dh_meth)
      {
	tmp =
	  engine_table_register (&dh_table, &engine_unregister_all_DH, e,
				 &dummy_nid___2, 1, 1);
	return (tmp);
      }
    return (1);
  }
}

ENGINE *
ENGINE_get_default_DH (void)
{
  ENGINE *tmp;
  {
    tmp = engine_table_select (&dh_table, (int) dummy_nid___2);
    return (tmp);
  }
}

static ENGINE_TABLE *ecdh_table = (ENGINE_TABLE *) ((void *) 0);
static int const dummy_nid___3 = (int const) 1;
static void
engine_unregister_all_ECDH (void)
{
  {
  }
}

ENGINE *
ENGINE_get_default_ECDH (void)
{
  ENGINE *tmp;
  {
    tmp = engine_table_select (&ecdh_table, (int) dummy_nid___3);
    return (tmp);
  }
}

static ENGINE_TABLE *rand_table = (ENGINE_TABLE *) ((void *) 0);
static int const dummy_nid___4 = (int const) 1;
ENGINE *
ENGINE_get_default_RAND (void)
{
  ENGINE *tmp;
  {
    tmp = engine_table_select (&rand_table, (int) dummy_nid___4);
    return (tmp);
  }
}

RAND_METHOD const *
ENGINE_get_RAND (ENGINE const *e)
{
  {
    return ((RAND_METHOD const *) e->rand_meth);
  }
}

static ENGINE_TABLE *cipher_table = (ENGINE_TABLE *) ((void *) 0);
void
ENGINE_unregister_ciphers (ENGINE * e)
{
  {
    engine_table_unregister (&cipher_table, e);
    return;
  }
}

static void
engine_unregister_all_ciphers (void)
{
  {
    engine_table_cleanup (&cipher_table);
    return;
  }
}

static ENGINE_TABLE *digest_table = (ENGINE_TABLE *) ((void *) 0);
void
ENGINE_unregister_digests (ENGINE * e)
{
  {
    engine_table_unregister (&digest_table, e);
    return;
  }
}

static void
engine_unregister_all_digests (void)
{
  {
    engine_table_cleanup (&digest_table);
    return;
  }
}

int
ENGINE_register_digests (ENGINE * e)
{
  int const *nids;
  int num_nids;
  int tmp;
  int tmp___0;
  {
    if (e->digests)
      {
	tmp = (*(e->digests)) (e, (EVP_MD const **) ((void *) 0), &nids, 0);
	num_nids = tmp;
	if (num_nids > 0)
	  {
	    tmp___0 =
	      engine_table_register (&digest_table,
				     &engine_unregister_all_digests, e, nids,
				     num_nids, 0);
	    return (tmp___0);
	  }
      }
    return (1);
  }
}

int
ENGINE_set_default_digests (ENGINE * e)
{
  int const *nids;
}

ENGINE *
ENGINE_get_digest_engine (int nid)
{
  ENGINE *tmp;
  {
    tmp = engine_table_select (&digest_table, nid);
    return (tmp);
  }
}

ERR_FNS const *ERR_get_implementation (void);
static int dynamic_init (ENGINE * e);
static int dynamic_finish (ENGINE * e);
static int dynamic_ctrl (ENGINE * e, int cmd, long i, void *p,
			 void (*f) (void));
static int dynamic_load (ENGINE * e, dynamic_data_ctx * ctx);
static char const *engine_dynamic_id = "dynamic";
static char const *engine_dynamic_name = "Dynamic engine loading support";
static ENGINE_CMD_DEFN const dynamic_cmd_defns[8] = {
  {
   200U, "SO_PATH", "Specifies the path to the new ENGINE shared library", 2U}
  , {
     201U, "NO_VCHECK",
     "Specifies to continue even if version checking fails (boolean)", 1U}
  , {
     0U, (char const *) ((void *) 0), (char const *) ((void *) 0), 0U}
};

static int dynamic_ex_data_idx = -1;
static void
int_free_str (void *s)
{
  {
    CRYPTO_free (s);
    return;
  }
}

static void
dynamic_data_ctx_free_func (void *parent, void *ptr, CRYPTO_EX_DATA * ad,
			    int idx, long argl, void *argp)
{
  dynamic_data_ctx *ctx;
  {
    if (ptr)
      {
	ctx = (dynamic_data_ctx *) ptr;
	if (ctx->dynamic_dso)
	  {
	    DSO_free (ctx->dynamic_dso);
	  }
	if (ctx->DYNAMIC_LIBNAME)
	  {
	    CRYPTO_free ((void *) ctx->DYNAMIC_LIBNAME);
	  }
	if (ctx->engine_id)
	  {
	    CRYPTO_free ((void *) ctx->engine_id);
	  }
	if (ctx->dirs)
	  {
	    sk_pop_free (ctx->dirs, &int_free_str);
	  }
	CRYPTO_free ((void *) ctx);
      }
    return;
  }
}

static int
dynamic_set_data_ctx (ENGINE * e, dynamic_data_ctx ** ctx)
{
  dynamic_data_ctx *c;
  void *tmp;
  dynamic_data_ctx *tmp___0;
  void *tmp___1;
  {
    tmp = CRYPTO_malloc ((int) sizeof (dynamic_data_ctx), "eng_dyn.c", 189);
    c = (dynamic_data_ctx *) tmp;
    if (!c)
      {
	ERR_put_error (38, 183, 65, "eng_dyn.c", 192);
	return (0);
      }
    memset ((void *) c, 0, sizeof (dynamic_data_ctx));
    c->dynamic_dso = (DSO *) ((void *) 0);
    c->v_check = (unsigned long (*)(unsigned long ossl_version)) ((void *) 0);
    c->bind_engine =
      (int (*)(ENGINE * e, char const *id, dynamic_fns const *fns)) ((void *)
								     0);
  }
}

static dynamic_data_ctx *
dynamic_get_data_ctx (ENGINE * e)
{
  dynamic_data_ctx *ctx;
  int new_idx;
  int tmp;
  void *tmp___0;
  int tmp___1;
  {
    if (dynamic_ex_data_idx < 0)
      {
	tmp =
	  ENGINE_get_ex_new_index (0L, (void *) 0,
				   (CRYPTO_EX_new *) ((void *) 0),
				   (CRYPTO_EX_dup *) ((void *) 0),
				   &dynamic_data_ctx_free_func);
	new_idx = tmp;
	if (new_idx == -1)
	  {
	    ERR_put_error (38, 181, 144, "eng_dyn.c", 244);
	    return ((dynamic_data_ctx *) ((void *) 0));
	  }
	_write_lock (&lock30);
	if (dynamic_ex_data_idx < 0)
	  {
	    dynamic_ex_data_idx = new_idx;
	    new_idx = -1;
	  }
	_write_unlock (&lock30);
      }
    tmp___0 = ENGINE_get_ex_data ((ENGINE const *) e, dynamic_ex_data_idx);
    ctx = (dynamic_data_ctx *) tmp___0;
    if ((unsigned int) ctx == (unsigned int) ((void *) 0))
      {
	tmp___1 = dynamic_set_data_ctx (e, &ctx);
	if (!tmp___1)
	  {
	    return ((dynamic_data_ctx *) ((void *) 0));
	  }
      }
    return (ctx);
  }
}

static ENGINE *
engine_dynamic (void)
{
  ENGINE *ret___3;
  ENGINE *tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  int tmp___3;
  int tmp___4;
  int tmp___5;
  int tmp___6;
  {
    tmp = ENGINE_new ();
    ret___3 = tmp;
    if (!ret___3)
      {
	return ((ENGINE *) ((void *) 0));
      }
    tmp___0 = ENGINE_set_id (ret___3, engine_dynamic_id);
    if (tmp___0)
      {
	tmp___1 = ENGINE_set_name (ret___3, engine_dynamic_name);
	if (tmp___1)
	  {
	    tmp___2 = ENGINE_set_init_function (ret___3, &dynamic_init);
	    if (tmp___2)
	      {
		tmp___3 =
		  ENGINE_set_finish_function (ret___3, &dynamic_finish);
		if (tmp___3)
		  {
		    tmp___4 =
		      ENGINE_set_ctrl_function (ret___3, &dynamic_ctrl);
		    if (tmp___4)
		      {
			tmp___5 = ENGINE_set_flags (ret___3, 4);
			if (tmp___5)
			  {
			    tmp___6 =
			      ENGINE_set_cmd_defns (ret___3,
						    dynamic_cmd_defns);
			    if (!tmp___6)
			      {
				ENGINE_free (ret___3);
				return ((ENGINE *) ((void *) 0));
			      }
			  }
			else
			  {
			    ENGINE_free (ret___3);
			    return ((ENGINE *) ((void *) 0));
		      }}
		    else
		      {
			ENGINE_free (ret___3);
			return ((ENGINE *) ((void *) 0));
		  }}
		else
		  {
		    ENGINE_free (ret___3);
		    return ((ENGINE *) ((void *) 0));
	      }}
	    else
	      {
		ENGINE_free (ret___3);
		return ((ENGINE *) ((void *) 0));
	  }}
	else
	  {
	    ENGINE_free (ret___3);
	    return ((ENGINE *) ((void *) 0));
      }}
    else
      {
	ENGINE_free (ret___3);
	return ((ENGINE *) ((void *) 0));
      }
    return (ret___3);
  }
}

void
ENGINE_load_dynamic (void)
{
  ENGINE *toadd;
  ENGINE *tmp;
  {
    tmp = engine_dynamic ();
    toadd = tmp;
    if (!toadd)
      {
	return;
      }
  }
}

static int
dynamic_ctrl (ENGINE * e, int cmd, long i, void *p, void (*f) (void))
{
  dynamic_data_ctx *ctx;
  dynamic_data_ctx *tmp;
  int initialised;
  size_t tmp___0;
  char *tmp___1;
  int tmp___2;
  size_t tmp___3;
  char *tmp___4;
  int tmp___5;
  int tmp___6;
  size_t tmp___7;
  char *tmp_str;
  char *tmp___8;
  {
    tmp = dynamic_get_data_ctx (e);
    ctx = tmp;
    if (!ctx)
      {
	ERR_put_error (38, 180, 112, "eng_dyn.c", 322);
	return (0);
      }
    if ((unsigned int) ctx->dynamic_dso == (unsigned int) ((void *) 0))
      {
	initialised = 0;
      }
    else
      {
	initialised = 1;
      }
    if (initialised)
      {
	ERR_put_error (38, 180, 100, "eng_dyn.c", 330);
	return (0);
      }
    switch (cmd)
      {
      case 200:
	if (p)
	  {
	    tmp___0 = strlen ((char const *) p);
	    if (tmp___0 < 1U)
	      {
		p = (void *) 0;
	      }
	  }
	if (ctx->DYNAMIC_LIBNAME)
	  {
	    CRYPTO_free ((void *) ctx->DYNAMIC_LIBNAME);
	  }
	if (p)
	  {
	    tmp___1 = BUF_strdup ((char const *) p);
	    ctx->DYNAMIC_LIBNAME = (char const *) tmp___1;
	  }
	else
	  {
	    ctx->DYNAMIC_LIBNAME = (char const *) ((void *) 0);
	} if (ctx->DYNAMIC_LIBNAME)
	  {
	    tmp___2 = 1;
	  }
	else
	  {
	    tmp___2 = 0;
	  }
	return (tmp___2);
      case 201:
	if (i == 0L)
	  {
	    ctx->no_vcheck = 0;
	  }
	else
	  {
	    ctx->no_vcheck = 1;
	  }
	return (1);
      case 202:
	if (p)
	  {
	    tmp___3 = strlen ((char const *) p);
	    if (tmp___3 < 1U)
	      {
		p = (void *) 0;
	      }
	  }
	if (ctx->engine_id)
	  {
	    CRYPTO_free ((void *) ctx->engine_id);
	  }
	if (p)
	  {
	    tmp___4 = BUF_strdup ((char const *) p);
	    ctx->engine_id = (char const *) tmp___4;
	  }
	else
	  {
	    ctx->engine_id = (char const *) ((void *) 0);
	} if (ctx->engine_id)
	  {
	    tmp___5 = 1;
	  }
	else
	  {
	    tmp___5 = 0;
	  }
	return (tmp___5);
      case 203:
	if (i < 0L)
	  {
	    ERR_put_error (38, 180, 143, "eng_dyn.c", 364);
	    return (0);
	  }
	else
	  {
	    if (i > 2L)
	      {
		ERR_put_error (38, 180, 143, "eng_dyn.c", 364);
		return (0);
	      }
	  }
	ctx->list_add_value = (int) i;
	return (1);
      case 206:
	tmp___6 = dynamic_load (e, ctx);
	return (tmp___6);
      case 204:
	if (i < 0L)
	  {
	    ERR_put_error (38, 180, 143, "eng_dyn.c", 375);
	    return (0);
	  }
	else
	  {
	    if (i > 2L)
	      {
		ERR_put_error (38, 180, 143, "eng_dyn.c", 375);
		return (0);
	      }
	  }
	ctx->dir_load = (int) i;
	return (1);
      case 205:
	if (!p)
	  {
	    ERR_put_error (38, 180, 143, "eng_dyn.c", 385);
	    return (0);
	  }
	else
	  {
	    tmp___7 = strlen ((char const *) p);
	    if (tmp___7 < 1U)
	      {
		ERR_put_error (38, 180, 143, "eng_dyn.c", 385);
		return (0);
	      }
	  }
	tmp___8 = BUF_strdup ((char const *) p);
	tmp_str = tmp___8;
	if (!tmp_str)
	  {
	    ERR_put_error (38, 180, 65, "eng_dyn.c", 393);
	    return (0);
	  }
	sk_insert (ctx->dirs, (void *) tmp_str, -1);
	return (1);
      default:;
	break;
      }
    ERR_put_error (38, 180, 119, "eng_dyn.c", 402);
    return (0);
  }
}

BIO *
BIO_new (BIO_METHOD * method___0)
{
  BIO *ret___3;
  void *tmp;
  int tmp___0;
  {
    ret___3 = (BIO *) ((void *) 0);
    tmp = CRYPTO_malloc ((int) sizeof (BIO), "bio_lib.c", 70);
    ret___3 = (BIO *) tmp;
    if ((unsigned int) ret___3 == (unsigned int) ((void *) 0))
      {
	ERR_put_error (32, 108, 65, "bio_lib.c", 73);
	return ((BIO *) ((void *) 0));
      }
    tmp___0 = BIO_set (ret___3, method___0);
    if (!tmp___0)
      {
	CRYPTO_free ((void *) ret___3);
	ret___3 = (BIO *) ((void *) 0);
      }
    return (ret___3);
  }
}

int
BIO_set (BIO * bio, BIO_METHOD * method___0)
{
  int tmp;
  {
    bio->method = method___0;
    bio->callback =
      (long (*)(struct bio_st *, int, char const *, int, long, long)) ((void
									*) 0);
    bio->cb_arg = (char *) ((void *) 0);
    bio->init = 0;
    bio->shutdown = 1;
    bio->flags = 0;
    if ((unsigned int) method___0->create != (unsigned int) ((void *) 0))
      {
	tmp = (*(method___0->create)) (bio);
	if (!tmp)
	  {
	    CRYPTO_free_ex_data (0, (void *) bio, &bio->ex_data);
	    return (0);
	  }
      }
    return (1);
  }
}

int
BIO_free (BIO * a)
{
  int ret___3;
  int i;
  long tmp;
}

long
BIO_ctrl (BIO * b, int cmd, long larg, void *parg)
{
  long ret___3;
  long (*cb) (BIO *, int, char const *, int, long, long);
  {
    if ((unsigned int) b == (unsigned int) ((void *) 0))
      {
	return (0L);
      }
    if ((unsigned int) b->method == (unsigned int) ((void *) 0))
      {
	ERR_put_error (32, 103, 121, "bio_lib.c", 360);
	return (-2L);
      }
    else
      {
	if ((unsigned int) (b->method)->ctrl == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (32, 103, 121, "bio_lib.c", 360);
	    return (-2L);
	  }
      }
    cb = b->callback;
    if ((unsigned int) cb != (unsigned int) ((void *) 0))
      {
	ret___3 = (*cb) (b, 6, (char const *) parg, cmd, larg, 1L);
	if (ret___3 <= 0L)
	  {
	    return (ret___3);
	  }
      }
    ret___3 = (*((b->method)->ctrl)) (b, cmd, larg, parg);
    if ((unsigned int) cb != (unsigned int) ((void *) 0))
      {
	ret___3 = (*cb) (b, 134, (char const *) parg, cmd, larg, ret___3);
      }
    return (ret___3);
  }
}

BIO *
BIO_push (BIO * b, BIO * bio)
{
  BIO *lb;
  {
    if ((unsigned int) b == (unsigned int) ((void *) 0))
      {
	return (bio);
      }
    lb = b;
    while ((unsigned int) lb->next_bio != (unsigned int) ((void *) 0))
      {
	lb = lb->next_bio;
      }
    lb->next_bio = bio;
    if ((unsigned int) bio != (unsigned int) ((void *) 0))
      {
	bio->prev_bio = lb;
      }
    BIO_ctrl (b, 6, 0L, (void *) 0);
    return (b);
  }
}

BIO *
BIO_pop (BIO * b)
{
  BIO *ret___3;
  {
    if ((unsigned int) b == (unsigned int) ((void *) 0))
      {
	return ((BIO *) ((void *) 0));
      }
    ret___3 = b->next_bio;
    BIO_ctrl (b, 7, 0L, (void *) 0);
    if ((unsigned int) b->prev_bio != (unsigned int) ((void *) 0))
      {
	(b->prev_bio)->next_bio = b->next_bio;
      }
  }
}

void
BIO_free_all (BIO * bio)
{
  BIO *b;
  int ref;
  {
    while ((unsigned int) bio != (unsigned int) ((void *) 0))
      {
	b = bio;
	ref = b->references;
	bio = bio->next_bio;
	BIO_free (b);
	if (ref > 1)
	  {
	    break;
	  }
      }
    return;
  }
}

void *
sk_delete_ptr (STACK * st, void *p)
{
  int i;
  void *tmp;
  {
    i = 0;
    while (i < st->num)
      {
	if ((unsigned int) *(st->data + i) == (unsigned int) p)
	  {
	    tmp = sk_delete (st, i);
	    return (tmp);
	  }
	i++;
      }
    return ((void *) 0);
  }
}

void *
sk_delete (STACK * st, int loc)
{
  void *ret___3;
  int i;
  int j;
  {
    if (!st)
      {
	return ((void *) 0);
      }
    else
      {
	if (loc < 0)
	  {
	    return ((void *) 0);
	  }
	else
	  {
	    if (loc >= st->num)
	      {
		return ((void *) 0);
	      }
      }}
    ret___3 = *(st->data + loc);
    if (loc != st->num - 1)
      {
	j = st->num - 1;
	i = loc;
	while (i < j)
	  {
	    *(st->data + i) = *(st->data + (i + 1));
	    i++;
	  }
      }
    (st->num)--;
    return (ret___3);
  }
}

static int
internal_find (STACK * st, void *data___0, int ret_val_options)
{
  void **r;
  int i;
  int (*comp_func) (void const *, void const *);
  char const *tmp;
  {
    if ((unsigned int) st == (unsigned int) ((void *) 0))
      {
	return (-1);
      }
    if ((unsigned int) st->comp == (unsigned int) ((void *) 0))
      {
	i = 0;
	while (i < st->num)
	  {
	    if ((unsigned int) *(st->data + i) == (unsigned int) data___0)
	      {
		return (i);
	      }
	    i++;
	  }
	return (-1);
      }
    sk_sort (st);
    if ((unsigned int) data___0 == (unsigned int) ((void *) 0))
      {
	return (-1);
      }
    comp_func = (int (*)(void const *, void const *)) st->comp;
    tmp =
      OBJ_bsearch_ex ((char const *) ((void *) (&data___0)),
		      (char const *) ((void *) st->data), st->num,
		      (int) sizeof (void *), comp_func, ret_val_options);
    r = (void **) tmp;
    if ((unsigned int) r == (unsigned int) ((void *) 0))
      {
	return (-1);
      }
    return (r - st->data);
  }
}

int
sk_find (STACK * st, void *data___0)
{
  int tmp;
  {
    tmp = internal_find (st, data___0, 2);
    return (tmp);
  }
}

void
sk_pop_free (STACK * st, void (*func) (void *))
{
  int i;
  {
    if ((unsigned int) st == (unsigned int) ((void *) 0))
      {
	return;
      }
    i = 0;
    while (i < st->num)
      {
	if ((unsigned int) *(st->data + i) != (unsigned int) ((void *) 0))
	  {
	    (*func) (*(st->data + i));
	  }
	i++;
      }
    sk_free (st);
    return;
  }
}

void
sk_free (STACK * st)
{
  {
    if ((unsigned int) st == (unsigned int) ((void *) 0))
      {
	return;
      }
    if ((unsigned int) st->data != (unsigned int) ((void *) 0))
      {
	CRYPTO_free ((void *) st->data);
      }
    CRYPTO_free ((void *) st);
    return;
  }
}

void *
sk_value (STACK const *st, int i)
{
  {
    if (!st)
      {
	return ((void *) 0);
      }
    else
      {
	if (i < 0)
	  {
	    return ((void *) 0);
	  }
	else
	  {
	    if (i >= (int) st->num)
	      {
		return ((void *) 0);
	      }
      }}
    return (*(st->data + i));
  }
}

void *
sk_set (STACK * st, int i, void *value)
{
  void *tmp;
  {
    if (!st)
      {
	return ((void *) 0);
      }
    else
      {
	if (i < 0)
	  {
	    return ((void *) 0);
	  }
	else
	  {
	    if (i >= st->num)
	      {
		return ((void *) 0);
	      }
      }}
    tmp = value;
    *(st->data + i) = tmp;
    return (tmp);
  }
}

static int state_num = 0;
static int state_index = 0;
static unsigned char state[1043];
static unsigned char md[20];
static long md_count[2] = {
  0L, 0L
};

static double entropy = (double) 0;
static int initialized___0 = 0;
static unsigned int crypto_lock_rand = 0U;
static unsigned long locking_thread = 0UL;
char const RAND_version[40] = {
  (char const) 'R', (char const) 'A', (char const) 'N', (char const) 'D',
    (char const) ' ', (char const) 'p', (char const) 'a', (char const) 'r',
    (char const) 't', (char const) ' ', (char const) 'o', (char const) 'f',
    (char const) ' ', (char const) 'O', (char const) 'p', (char const) 'e',
    (char const) 'n', (char const) 'S', (char const) 'S', (char const) 'L',
    (char const) ' ', (char const) '0', (char const) '.', (char const) '9',
    (char const) '.', (char const) '8', (char const) 'g', (char const) ' ',
    (char const) '1', (char const) '9', (char const) ' ', (char const) 'O',
    (char const) 'c', (char const) 't', (char const) ' ', (char const) '2',
    (char const) '0', (char const) '0', (char const) '7', (char const) '\000'
};

static void ssleay_rand_cleanup (void);
static void ssleay_rand_seed (void const *buf___5, int num);
static void ssleay_rand_add (void const *buf___5, int num, double add);
static int ssleay_rand_bytes (unsigned char *buf___5, int num);
static int ssleay_rand_pseudo_bytes (unsigned char *buf___5, int num);
static int ssleay_rand_status (void);
RAND_METHOD rand_ssleay_meth = {
  &ssleay_rand_seed, &ssleay_rand_bytes, &ssleay_rand_cleanup,
    &ssleay_rand_add, &ssleay_rand_pseudo_bytes, &ssleay_rand_status
};

RAND_METHOD *
RAND_SSLeay (void)
{
  {
    return (&rand_ssleay_meth);
  }
  {
    OPENSSL_cleanse ((void *) (state), sizeof (state));
    state_num = 0;
    state_index = 0;
    OPENSSL_cleanse ((void *) (md), 20U);
    md_count[0] = 0L;
    md_count[1] = 0L;
    entropy = (double) 0;
    initialized___0 = 0;
    return;
  }
}

static void
ssleay_rand_add (void const *buf___5, int num, double add)
{
  int i;
  int j;
  int k;
  int st_idx;
  long md_c[2];
  unsigned char local_md[20];
  EVP_MD_CTX m___10;
  int do_not_lock;
  unsigned long tmp;
  EVP_MD const *tmp___0;
  int tmp___1;
  {
    if (crypto_lock_rand)
      {
	_read_lock (&lock19);
	tmp = CRYPTO_thread_id ();
	do_not_lock = locking_thread == tmp;
	_read_unlock (&lock19);
      }
    else
      {
	do_not_lock = 0;
      }
    if (!do_not_lock)
      {
	_write_lock (&lock18);
      }
    st_idx = state_index;
    md_c[0] = md_count[0];
    md_c[1] = md_count[1];
    while (i < num)
      {
	j = num - i;
	if (j > 20)
	  {
	    j = 20;
	  }
	else
	  {
	    j = j;
	  }
	tmp___0 = EVP_sha1 ();
	EVP_DigestInit_ex (&m___10, tmp___0, (ENGINE *) ((void *) 0));
	EVP_DigestUpdate (&m___10, (void const *) (local_md), 20U);
	k = (st_idx + j) - 1023;
	if (k > 0)
	  {
	    EVP_DigestUpdate (&m___10, (void const *) (&state[st_idx]),
			      (unsigned int) (j - k));
	    EVP_DigestUpdate (&m___10, (void const *) (&state[0]),
			      (unsigned int) k);
	  }
	else
	  {
	    EVP_DigestUpdate (&m___10, (void const *) (&state[st_idx]),
			      (unsigned int) j);
	  } EVP_DigestUpdate (&m___10, buf___5, (unsigned int) j);
	EVP_DigestUpdate (&m___10,
			  (void const *) ((unsigned char *) (&md_c[0])),
			  sizeof (md_c));
	EVP_DigestFinal_ex (&m___10, local_md, (unsigned int *) ((void *) 0));
	(md_c[1])++;
	buf___5 = (void const *) ((char const *) buf___5 + j);
	k = 0;
	while (k < j)
	  {
	    tmp___1 = st_idx;
	    st_idx++;
	    state[tmp___1] =
	      (unsigned char) ((int) state[tmp___1] ^ (int) local_md[k]);
	    if (st_idx >= 1023)
	      {
		st_idx = 0;
	      }
	    k++;
	  }
	i += 20;
      }
    EVP_MD_CTX_cleanup (&m___10);
    if (!do_not_lock)
      {
	_write_lock (&lock18);
      }
    k = 0;
    while (k < (int) sizeof (md))
      {
	md[k] = (unsigned char) ((int) md[k] ^ (int) local_md[k]);
	k++;
      }
    if (entropy < (double) 32)
      {
	entropy += add;
      }
    if (!do_not_lock)
      {
	_write_unlock (&lock18);
      }
    return;
  }
}

static void
ssleay_rand_seed (void const *buf___5, int num)
{
  {
    ssleay_rand_add (buf___5, num, (double) num);
    return;
  }
}

static int volatile stirred_pool = (int volatile) 0;
static int
ssleay_rand_bytes (unsigned char *buf___5, int num)
{
  int i;
  int j;
  int k;
  int st_num;
  int st_idx;
  int num_ceil;
  int ok;
  long md_c[2];
  unsigned char local_md[20];
  EVP_MD_CTX m___10;
  pid_t curr_pid;
  __pid_t tmp;
  int do_stir_pool;
  int n;
  EVP_MD const *tmp___0;
  int tmp___1;
  unsigned char *tmp___2;
  EVP_MD const *tmp___3;
  {
    tmp = getpid ();
    curr_pid = tmp;
    do_stir_pool = 0;
    if (num <= 0)
      {
	return (1);
      }
    EVP_MD_CTX_init (&m___10);
    num_ceil = (1 + (num - 1) / 10) * 10;
    _write_lock (&lock18);
    EVP_DigestUpdate (&m___10, (void const *) (md), 20U);
    EVP_DigestFinal_ex (&m___10, md, (unsigned int *) ((void *) 0));
    _write_unlock (&lock18);
    EVP_MD_CTX_cleanup (&m___10);
    if (ok)
      {
	return (1);
      }
    else
      {
	ERR_put_error (36, 100, 100, "md_rand.c", 503);
	ERR_add_error_data (1,
			    "You need to read the OpenSSL FAQ, http://www.openssl.org/support/faq.html");
	return (0);
      }
  }
}

static int
ssleay_rand_pseudo_bytes (unsigned char *buf___5, int num)
{
  int ret___3;
  unsigned long err;
  {
    ret___3 = RAND_bytes (buf___5, num);
    if (ret___3 == 0)
      {
	err = ERR_peek_error ();
	if ((int) ((err >> 24L) & 255UL) == 36)
	  {
	    if ((int) (err & 4095UL) == 100)
	      {
		ERR_clear_error ();
	      }
	  }
      }
    return (ret___3);
  }
}

static int
ssleay_rand_status (void)
{
  int ret___3;
  int do_not_lock;
  unsigned long tmp;
  {
    if (crypto_lock_rand)
      {
	_read_lock (&lock19);
	tmp = CRYPTO_thread_id ();
	do_not_lock = locking_thread == tmp;
	_read_unlock (&lock19);
      }
    else
      {
	do_not_lock = 0;
      }
    if (!do_not_lock)
      {
	_write_lock (&lock18);
	_write_lock (&lock19);
	locking_thread = CRYPTO_thread_id ();
	_write_unlock (&lock19);
	crypto_lock_rand = 1U;
      }
    if (!initialized___0)
      {
	RAND_poll ();
	initialized___0 = 1;
      }
    ret___3 = entropy >= (double) 32;
    if (!do_not_lock)
      {
	crypto_lock_rand = 0U;
	_write_unlock (&lock18);
      }
    return (ret___3);
  }
}

void RAND_cleanup (void);
static ENGINE *funct_ref = (ENGINE *) ((void *) 0);
static RAND_METHOD const *default_RAND_meth =
  (RAND_METHOD const *) ((void *) 0);
int
RAND_set_rand_method (RAND_METHOD const *meth___0)
{
  {
    if (funct_ref)
      {
	ENGINE_finish (funct_ref);
	funct_ref = (ENGINE *) ((void *) 0);
      }
    default_RAND_meth = meth___0;
    return (1);
  }
}

RAND_METHOD const *
RAND_get_rand_method (void)
{
  ENGINE *e;
  ENGINE *tmp;
  RAND_METHOD *tmp___0;
  {
    if (!default_RAND_meth)
      {
	tmp = ENGINE_get_default_RAND ();
	e = tmp;
	if (e)
	  {
	    default_RAND_meth = ENGINE_get_RAND ((ENGINE const *) e);
	    if (!default_RAND_meth)
	      {
		ENGINE_finish (e);
		e = (ENGINE *) ((void *) 0);
	      }
	  }
	if (e)
	  {
	    funct_ref = e;
	  }
	else
	  {
	    tmp___0 = RAND_SSLeay ();
	    default_RAND_meth = (RAND_METHOD const *) tmp___0;
      }}
    return (default_RAND_meth);
  }
}

void
RAND_seed (void const *buf___5, int num)
{
  RAND_METHOD const *meth___0;
  RAND_METHOD const *tmp;
  {
    tmp = RAND_get_rand_method ();
    meth___0 = tmp;
    if (meth___0)
      {
	if (meth___0->seed)
	  {
	    (*(meth___0->seed)) (buf___5, num);
	  }
      }
    return;
  }
}

void
RAND_add (void const *buf___5, int num, double entropy___0)
{
  RAND_METHOD const *meth___0;
  RAND_METHOD const *tmp;
  {
    tmp = RAND_get_rand_method ();
    meth___0 = tmp;
    if (meth___0)
      {
	if (meth___0->add)
	  {
	    (*(meth___0->add)) (buf___5, num, entropy___0);
	  }
      }
    return;
  }
}

int
RAND_bytes (unsigned char *buf___5, int num)
{
  RAND_METHOD const *meth___0;
  RAND_METHOD const *tmp;
  int tmp___0;
  {
    tmp = RAND_get_rand_method ();
    meth___0 = tmp;
    if (meth___0)
      {
	if (meth___0->bytes)
	  {
	    tmp___0 = (*(meth___0->bytes)) (buf___5, num);
	    return (tmp___0);
	  }
      }
    return (-1);
  }
}

int
RAND_pseudo_bytes (unsigned char *buf___5, int num)
{
  RAND_METHOD const *meth___0;
  RAND_METHOD const *tmp;
  int tmp___0;
  {
    tmp = RAND_get_rand_method ();
    meth___0 = tmp;
    if (meth___0)
      {
	if (meth___0->pseudorand)
	  {
	    tmp___0 = (*(meth___0->pseudorand)) (buf___5, num);
	    return (tmp___0);
	  }
      }
    return (-1);
  }
}

int
RAND_status (void)
{
  RAND_METHOD const *meth___0;
  RAND_METHOD const *tmp;
  int tmp___0;
  {
    tmp = RAND_get_rand_method ();
    meth___0 = tmp;
    if (meth___0)
      {
	if (meth___0->status)
	  {
	    tmp___0 = (*(meth___0->status)) ();
	    return (tmp___0);
	  }
      }
    return (0);
  }
}

int RAND_egd_bytes (char const *path, int bytes);
int
RAND_query_egd_bytes (char const *path, unsigned char *buf___5, int bytes)
{
  int ret___3;
  struct sockaddr_un addr;
  int len;
}

int
RAND_egd_bytes (char const *path, int bytes)
{
  int num;
  int ret___3;
  int tmp;
  {
    ret___3 = 0;
    num = RAND_query_egd_bytes (path, (unsigned char *) ((void *) 0), bytes);
    if (num < 1)
      {
	goto err;
      }
    tmp = RAND_status ();
    if (tmp == 1)
      {
	ret___3 = num;
      }
  err:return (ret___3);
  }
}

static char const *randomfiles[3] = {
  "/dev/urandom", "/dev/random", "/dev/srandom"
};

static char const *egdsockets[5] = {
  "/var/run/egd-pool", "/dev/egd-pool", "/etc/egd-pool", "/etc/entropy",
    (char const *) ((void *) 0)
};

int
RAND_poll (void)
{
  unsigned long l;
  pid_t curr_pid;
  __pid_t tmp;
  unsigned char tmpbuf___0[32];
  int n;
  struct stat randomstats[sizeof (randomfiles) / sizeof (randomfiles[0])];
  int fd;
  size_t i;
  char const **egdsocket;
  int usec;
  int r;
  size_t j;
  struct stat *st;
  int tmp___0;
  int try_read;
  struct pollfd pset;
  int tmp___1;
  int *tmp___2;
  int *tmp___3;
  int r___0;
  __uid_t tmp___4;
  time_t tmp___5;
  {
    tmp = getpid ();
    curr_pid = tmp;
    n = 0;
    egdsocket = (char const **) ((void *) 0);
    memset ((void *) (randomstats), 0, sizeof (randomstats));
    i = 0U;
    while (1)
      {
	if (i < sizeof (randomfiles) / sizeof (randomfiles[0]))
	  {
	    if (!(n < 32))
	      {
		break;
	      }
	  }
	else
	  {
	    break;
	  }
	fd = open (randomfiles[i], 2304);
	if (fd >= 0)
	  {
	    usec = 10000;
	    st = &randomstats[i];
	    tmp___0 = fstat (fd, st);
	    if (tmp___0 != 0)
	      {
		close (fd);
		goto __Cont;
	      }
	    j = 0U;
	    while (j < i)
	      {
		if (randomstats[j].st_ino == st->st_ino)
		  {
		    if (randomstats[j].st_dev == st->st_dev)
		      {
			break;
		      }
		  }
		j++;
	      }
	    if (j < i)
	      {
		close (fd);
		goto __Cont;
	      }
	    while (1)
	      {
		try_read = 0;
		pset.fd = fd;
		pset.events = (short) 1;
		pset.revents = (short) 0;
		tmp___1 = poll (&pset, 1UL, usec / 1000);
		if (tmp___1 < 0)
		  {
		    usec = 0;
		  }
		else
		  {
		    try_read = ((int) pset.revents & 1) != 0;
		} if (try_read)
		  {
		    r =
		      read (fd, (void *) (tmpbuf___0 + n),
			    (unsigned int) (32 - n));
		    if (r > 0)
		      {
			n += r;
		      }
		  }
		else
		  {
		    r = -1;
		  }
		if (usec == 10000)
		  {
		    usec = 0;
		  }
		if (r > 0)
		  {
		    goto _L___0;
		  }
		else
		  {
		    tmp___2 = __errno_location ();
		    if (*tmp___2 == 4)
		      {
			goto _L___0;
		      }
		    else
		      {
			tmp___3 = __errno_location ();
			if (*tmp___3 == 11)
			  {
			  _L___0:if (usec != 0)
			      {
				if (!(n < 32))
				  {
				    break;
				  }
			      }
			    else
			      {
				break;
			      }
			  }
			else
			  {
			    break;
			  }
		      }
		  }
	      }
	    close (fd);
	  }
      __Cont:i++;
      }
    egdsocket = egdsockets;
    while (1)
      {
	if (*egdsocket)
	  {
	    if (!(n < 32))
	      {
		break;
	      }
	  }
	else
	  {
	    break;
	  }
	r___0 = RAND_query_egd_bytes (*egdsocket, tmpbuf___0 + n, 32 - n);
	if (r___0 > 0)
	  {
	    n += r___0;
	  }
	egdsocket++;
      }
    if (n > 0)
      {
	RAND_add ((void const *) (tmpbuf___0), (int) sizeof (tmpbuf___0),
		  (double) n);
	OPENSSL_cleanse ((void *) (tmpbuf___0), (unsigned int) n);
      }
    l = (unsigned long) curr_pid;
    RAND_add ((void const *) (&l), (int) sizeof (l), 0.0);
  }
}

int
EVP_DigestInit (EVP_MD_CTX * ctx, EVP_MD const *type)
{
  int tmp;
  {
    EVP_MD_CTX_init (ctx);
    tmp = EVP_DigestInit_ex (ctx, type, (ENGINE *) ((void *) 0));
    return (tmp);
  }
}

int
EVP_DigestInit_ex (EVP_MD_CTX * ctx, EVP_MD const *type, ENGINE * impl___0)
{
  int tmp;
  EVP_MD const *d;
  EVP_MD const *tmp___0;
  int tmp___1;
  {
    EVP_MD_CTX_clear_flags (ctx, 2);
    if (ctx->engine)
      {
	if (ctx->digest)
	  {
	    if (!type)
	      {
		goto skip_to_init;
	      }
	    else
	      {
		if (type)
		  {
		    if (type->type == (ctx->digest)->type)
		      {
			goto skip_to_init;
		      }
		  }
	      }
	  }
      }
    if (type)
      {
	if (ctx->engine)
	  {
	    ENGINE_finish (ctx->engine);
	  }
	if (impl___0)
	  {
	    tmp = ENGINE_init (impl___0);
	    if (!tmp)
	      {
		ERR_put_error (6, 128, 134, "digest.c", 162);
		return (0);
	      }
	  }
	else
	  {
	    impl___0 = ENGINE_get_digest_engine ((int) type->type);
	} if (impl___0)
	  {
	    tmp___0 = ENGINE_get_digest (impl___0, (int) type->type);
	    d = tmp___0;
	    if (!d)
	      {
		ERR_put_error (6, 128, 134, "digest.c", 176);
		return (0);
	      }
	    type = d;
	    ctx->engine = impl___0;
	  }
	else
	  {
	    ctx->engine = (ENGINE *) ((void *) 0);
      }}
    else
      {
	if (!ctx->digest)
	  {
	    ERR_put_error (6, 128, 139, "digest.c", 192);
	    return (0);
	  }
      }
    if ((unsigned int) ctx->digest != (unsigned int) type)
      {
	if (ctx->digest)
	  {
	    if ((ctx->digest)->ctx_size)
	      {
		CRYPTO_free (ctx->md_data);
	      }
	  }
	ctx->digest = type;
	if (type->ctx_size)
	  {
	    ctx->md_data =
	      CRYPTO_malloc ((int) type->ctx_size, "digest.c", 202);
	  }
      }
  skip_to_init:tmp___1 = (*((ctx->digest)->init)) (ctx);
    return (tmp___1);
  }
}

int
EVP_MD_CTX_copy_ex (EVP_MD_CTX * out, EVP_MD_CTX const *in)
{
  unsigned char *tmp_buf;
  int tmp;
  int tmp___0;
  {
    if ((unsigned int) in == (unsigned int) ((void *) 0))
      {
	ERR_put_error (6, 110, 111, "digest.c", 254);
	return (0);
      }
    else
      {
	if ((unsigned int) in->digest == (unsigned int) ((void *) 0))
	  {
	    ERR_put_error (6, 110, 111, "digest.c", 254);
	    return (0);
	  }
      }
    if (in->engine)
      {
	tmp = ENGINE_init ((ENGINE *) in->engine);
	if (!tmp)
	  {
	    ERR_put_error (6, 110, 38, "digest.c", 261);
	    return (0);
	  }
      }
    if ((unsigned int) out->digest == (unsigned int) in->digest)
      {
	tmp_buf = (unsigned char *) out->md_data;
	EVP_MD_CTX_set_flags (out, 4);
      }
    else
      {
	tmp_buf = (unsigned char *) ((void *) 0);
      }
    EVP_MD_CTX_cleanup (out);
    memcpy ((void *__restrict) out, (void const *__restrict) in,
	    sizeof (*out));
    if ((out->digest)->ctx_size)
      {
	if (tmp_buf)
	  {
	    out->md_data = (void *) tmp_buf;
	  }
	else
	  {
	    out->md_data =
	      CRYPTO_malloc ((int) (out->digest)->ctx_size, "digest.c", 278);
	  } memcpy ((void *__restrict) out->md_data,
		    (void const *__restrict) in->md_data,
		    (unsigned int) (out->digest)->ctx_size);
      }
    if ((out->digest)->copy)
      {
	tmp___0 = (*((out->digest)->copy)) (out, in);
	return (tmp___0);
      }
    return (1);
  }
}

int
EVP_Digest (void const *data___0, size_t count, unsigned char *md___0,
	    unsigned int *size, EVP_MD const *type, ENGINE * impl___0)
{
  EVP_MD_CTX ctx;
  int ret___3;
  int tmp;
  int tmp___0;
  int tmp___1;
  int tmp___2;
  {
    EVP_MD_CTX_init (&ctx);
    EVP_MD_CTX_set_flags (&ctx, 1);
    tmp = EVP_DigestInit_ex (&ctx, type, impl___0);
    if (tmp)
      {
	tmp___0 = EVP_DigestUpdate (&ctx, data___0, count);
	if (tmp___0)
	  {
	    tmp___1 = EVP_DigestFinal_ex (&ctx, md___0, size);
	    if (tmp___1)
	      {
		tmp___2 = 1;
	      }
	    else
	      {
		tmp___2 = 0;
	      }
	  }
	else
	  {
	    tmp___2 = 0;
	  }
      }
    else
      {
	tmp___2 = 0;
      }
    ret___3 = tmp___2;
    EVP_MD_CTX_cleanup (&ctx);
    return (ret___3);
  }
}

void
EVP_MD_CTX_destroy (EVP_MD_CTX * ctx)
{
  {
    EVP_MD_CTX_cleanup (ctx);
    CRYPTO_free ((void *) ctx);
    return;
  }
}

int
EVP_MD_CTX_cleanup (EVP_MD_CTX * ctx)
{
  int tmp;
  int tmp___0;
  {
    if (ctx->digest)
      {
	if ((ctx->digest)->cleanup)
	  {
	    tmp = EVP_MD_CTX_test_flags ((EVP_MD_CTX const *) ctx, 2);
	    if (!tmp)
	      {
		(*((ctx->digest)->cleanup)) (ctx);
	      }
	  }
      }
    if (ctx->digest)
      {
	if ((ctx->digest)->ctx_size)
	  {
	    if (ctx->md_data)
	      {
		tmp___0 = EVP_MD_CTX_test_flags ((EVP_MD_CTX const *) ctx, 4);
		if (!tmp___0)
		  {
		    OPENSSL_cleanse (ctx->md_data,
				     (unsigned int) (ctx->digest)->ctx_size);
		    CRYPTO_free (ctx->md_data);
		  }
	      }
	  }
      }
    if (ctx->engine)
      {
	ENGINE_finish (ctx->engine);
      }
    memset ((void *) ctx, '\000', sizeof (*ctx));
    return (1);
  }
}

int
EVP_CipherInit_ex (EVP_CIPHER_CTX * ctx, EVP_CIPHER const *cipher,
		   ENGINE * impl___0, unsigned char const *key,
		   unsigned char const *iv, int enc)
{
  int tmp;
  EVP_CIPHER const *c;
  EVP_CIPHER const *tmp___0;
  int tmp___1;
  unsigned long tmp___2;
  int tmp___3;
  int tmp___4;
  int tmp___5;
  unsigned long tmp___6;
  int tmp___7;
  {
    if (enc == -1)
      {
	enc = ctx->encrypt;
      }
    else
      {
	if (enc)
	  {
	    enc = 1;
	  }
	ctx->encrypt = enc;
      }
    if (ctx->engine)
      {
	if (ctx->cipher)
	  {
	    if (!cipher)
	      {
		goto skip_to_init;
	      }
	    else
	      {
		if (cipher)
		  {
		    if (cipher->nid == (ctx->cipher)->nid)
		      {
			goto skip_to_init;
		      }
		  }
	      }
	  }
      }
    if (cipher)
      {
	EVP_CIPHER_CTX_cleanup (ctx);
	ctx->encrypt = enc;
	if (impl___0)
	  {
	    tmp = ENGINE_init (impl___0);
	    if (!tmp)
	      {
		ERR_put_error (6, 123, 134, "evp_enc.c", 127);
		return (0);
	      }
	  }
	else
	  {
	    impl___0 = ENGINE_get_cipher_engine ((int) cipher->nid);
	} if (impl___0)
	  {
	    tmp___0 = ENGINE_get_cipher (impl___0, (int) cipher->nid);
	    c = tmp___0;
	    if (!c)
	      {
		ERR_put_error (6, 123, 134, "evp_enc.c", 144);
		return (0);
	      }
	    cipher = c;
	    ctx->engine = impl___0;
	  }
	else
	  {
	    ctx->engine = (ENGINE *) ((void *) 0);
	  } ctx->cipher = cipher;
	if ((ctx->cipher)->ctx_size)
	  {
	    ctx->cipher_data =
	      CRYPTO_malloc ((int) (ctx->cipher)->ctx_size, "evp_enc.c", 161);
	    if (!ctx->cipher_data)
	      {
		ERR_put_error (6, 123, 65, "evp_enc.c", 164);
		return (0);
	      }
	  }
	else
	  {
	    ctx->cipher_data = (void *) 0;
	  } ctx->key_len = (int) cipher->key_len;
	ctx->flags = 0UL;
	if ((ctx->cipher)->flags & 64UL)
	  {
	    tmp___1 = EVP_CIPHER_CTX_ctrl (ctx, 0, 0, (void *) 0);
	    if (!tmp___1)
	      {
		ERR_put_error (6, 123, 134, "evp_enc.c", 178);
		return (0);
	      }
	  }
      }
    else
      {
	if (!ctx->cipher)
	  {
	    ERR_put_error (6, 123, 131, "evp_enc.c", 185);
	    return (0);
	  }
      }
  skip_to_init:if (!((ctx->cipher)->block_size == 1))
      {
	if (!((ctx->cipher)->block_size == 8))
	  {
	    if (!((ctx->cipher)->block_size == 16))
	      {
		OpenSSLDie ("evp_enc.c", 194,
			    "ctx->cipher->block_size == 1 || ctx->cipher->block_size == 8 || ctx->cipher->block_size == 16");
	      }
	  }
      }
    tmp___6 = EVP_CIPHER_CTX_flags ((EVP_CIPHER_CTX const *) ctx);
    if (!(tmp___6 & 16UL))
      {
	tmp___2 = EVP_CIPHER_CTX_flags ((EVP_CIPHER_CTX const *) ctx);
	switch ((int) (tmp___2 & 7UL))
	  {
	  case 0:
	  case 1:
	    break;
	  case 3:
	  case 4:
	    ctx->num = 0;
	  case 2:
	    tmp___3 = EVP_CIPHER_CTX_iv_length ((EVP_CIPHER_CTX const *) ctx);
	    if (!(tmp___3 <= (int) sizeof (ctx->iv)))
	      {
		OpenSSLDie ("evp_enc.c", 211,
			    "EVP_CIPHER_CTX_iv_length(ctx) <= (int)sizeof(ctx->iv)");
	      }
	    if (iv)
	      {
		tmp___4 =
		  EVP_CIPHER_CTX_iv_length ((EVP_CIPHER_CTX const *) ctx);
		memcpy ((void *__restrict) (ctx->oiv),
			(void const *__restrict) iv, (unsigned int) tmp___4);
	      }
	    tmp___5 = EVP_CIPHER_CTX_iv_length ((EVP_CIPHER_CTX const *) ctx);
	    memcpy ((void *__restrict) (ctx->iv),
		    (void const *__restrict) (ctx->oiv),
		    (unsigned int) tmp___5);
	    break;
	  default:;
	    return (0);
	    break;
	  }
      }
    if (key)
      {
	goto _L;
      }
    else
      {
	if ((ctx->cipher)->flags & 32UL)
	  {
	  _L:tmp___7 =
	      (*((ctx->cipher)->init)) (ctx, key, iv, enc);
	    if (!tmp___7)
	      {
		return (0);
	      }
	  }
      }
    ctx->buf_len = 0;
    ctx->final_used = 0;
    ctx->block_mask = (int) ((ctx->cipher)->block_size - 1);
    return (1);
    tmp = EVP_CipherInit_ex (ctx, cipher, impl___0, key, iv, 0);
    return (tmp);
  }
}

int
EVP_EncryptUpdate (EVP_CIPHER_CTX * ctx, unsigned char *out, int *outl,
		   unsigned char const *in, int inl)
{
  int i;
  int j;
  int bl;
  int tmp;
}

void
EVP_CIPHER_CTX_free (EVP_CIPHER_CTX * ctx)
{
  {
    if (ctx)
      {
	EVP_CIPHER_CTX_cleanup (ctx);
	CRYPTO_free ((void *) ctx);
      }
    return;
  }
}

int
EVP_CIPHER_CTX_cleanup (EVP_CIPHER_CTX * c)
{
  int tmp;
  {
    if ((unsigned int) c->cipher != (unsigned int) ((void *) 0))
      {
	if ((c->cipher)->cleanup)
	  {
	    tmp = (*((c->cipher)->cleanup)) (c);
	    if (!tmp)
	      {
		return (0);
	      }
	  }
	if (c->cipher_data)
	  {
	    OPENSSL_cleanse (c->cipher_data,
			     (unsigned int) (c->cipher)->ctx_size);
	  }
      }
    if (c->cipher_data)
      {
	CRYPTO_free (c->cipher_data);
      }
    if (c->engine)
      {
	ENGINE_finish (c->engine);
      }
    memset ((void *) c, 0, sizeof (EVP_CIPHER_CTX));
    return (1);
  }
}
