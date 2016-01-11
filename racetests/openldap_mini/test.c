
typedef unsigned int size_t;
typedef long long __quad_t;
typedef long __off_t;
typedef __quad_t __off64_t;
struct _IO_FILE;
typedef struct _IO_FILE FILE;
typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
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
   void *__pad3 ;
   void *__pad4 ;
   size_t __pad5 ;
   int _mode ;
   char _unused2[(15U * sizeof(int ) - 4U * sizeof(void *)) - sizeof(size_t )] ;
};
typedef struct _IO_FILE _IO_FILE;



typedef unsigned long ber_len_t;
typedef long ber_slen_t;
typedef int ber_int_t;
typedef unsigned long ber_tag_t;
typedef long __time_t;
typedef long __suseconds_t;
typedef unsigned int __socklen_t;
typedef __time_t time_t;
typedef int ber_socket_t;

typedef unsigned long long ldap_pvt_mp_t;
typedef unsigned long slap_mask_t;
typedef unsigned int slap_ssf_t;
struct slap_ssf_set {
   slap_ssf_t sss_ssf ;
   slap_ssf_t sss_transport ;
   slap_ssf_t sss_tls ;
   slap_ssf_t sss_sasl ;
   slap_ssf_t sss_update_ssf ;
   slap_ssf_t sss_update_transport ;
   slap_ssf_t sss_update_tls ;
   slap_ssf_t sss_update_sasl ;
   slap_ssf_t sss_simple_bind ;
};
typedef struct slap_ssf_set slap_ssf_set_t;

struct timeval {
   __time_t tv_sec ;
   __suseconds_t tv_usec ;
};

struct berval {
   ber_len_t bv_len ;
   char *bv_val ;
};
struct lber_options {
   short lbo_valid ;
   unsigned short lbo_options ;
   int lbo_debug ;
};
struct berelement {
   struct lber_options ber_opts ;
   ber_tag_t ber_tag ;
   ber_len_t ber_len ;
   ber_tag_t ber_usertag ;
   char *ber_buf ;
   char *ber_ptr ;
   char *ber_end ;
   char *ber_sos_ptr ;
   char *ber_rwptr ;
   void *ber_memctx ;
};
typedef struct berval BerValue;
typedef BerValue *BerVarray;

union lber_berelement_u {
   char buffer[256] ;
   int ialign ;
   long lalign ;
   float falign ;
   double dalign ;
   char *palign ;
};
typedef union lber_berelement_u BerElementBuffer;
struct berelement;
typedef struct berelement BerElement;

struct sockbuf;
typedef struct sockbuf Sockbuf;
struct sockbuf_io;
typedef struct sockbuf_io Sockbuf_IO;
struct sockbuf_io_desc {
   int sbiod_level ;
   Sockbuf *sbiod_sb ;
   Sockbuf_IO *sbiod_io ;
   void *sbiod_pvt ;
   struct sockbuf_io_desc *sbiod_next ;
};
typedef struct sockbuf_io_desc Sockbuf_IO_Desc;
struct sockbuf_io {
   int (*sbi_setup)(Sockbuf_IO_Desc *sbiod , void *arg ) ;
   int (*sbi_remove)(Sockbuf_IO_Desc *sbiod ) ;
   int (*sbi_ctrl)(Sockbuf_IO_Desc *sbiod , int opt , void *arg ) ;
   ber_slen_t (*sbi_read)(Sockbuf_IO_Desc *sbiod , void *buf , ber_len_t len ) ;
   ber_slen_t (*sbi_write)(Sockbuf_IO_Desc *sbiod , void *buf , ber_len_t len ) ;
   int (*sbi_close)(Sockbuf_IO_Desc *sbiod ) ;
};

struct sockbuf {
   struct lber_options sb_opts ;
   Sockbuf_IO_Desc *sb_iod ;
   ber_socket_t sb_fd ;
   ber_len_t sb_max_incoming ;
   unsigned int sb_trans_needs_read : 1 ;
   unsigned int sb_trans_needs_write : 1 ;
};


typedef void *BER_MEMALLOC_FN(ber_len_t size , void *ctx );
typedef void *BER_MEMCALLOC_FN(ber_len_t n , ber_len_t size , void *ctx );
typedef void *BER_MEMREALLOC_FN(void *p , ber_len_t size , void *ctx );
typedef void BER_MEMFREE_FN(void *p , void *ctx );
struct lber_memory_fns {
   BER_MEMALLOC_FN *bmf_malloc ;
   BER_MEMCALLOC_FN *bmf_calloc ;
   BER_MEMREALLOC_FN *bmf_realloc ;
   BER_MEMFREE_FN *bmf_free ;
};
typedef struct lber_memory_fns BerMemoryFunctions;


struct slog_entry {
   struct slog_entry *se_next ;
   struct berval se_uuid ;
   struct berval se_csn ;
   int se_sid ;
   ber_tag_t se_tag ;
};
typedef struct slog_entry slog_entry;


struct __anonstruct_sync_cookie_sc_next_87 {
   struct sync_cookie *stqe_next ;
};
struct sync_cookie {
   struct berval *ctxcsn ;
   struct berval octet_str ;
   int rid ;
   int sid ;
   int numcsns ;
   int *sids ;
   struct __anonstruct_sync_cookie_sc_next_87 sc_next ;
};

struct SlapReply;
typedef struct SlapReply SlapReply;

struct __pthread_internal_slist {
   struct __pthread_internal_slist *__next ;
};
typedef struct __pthread_internal_slist __pthread_slist_t;
union __anonunion___pthread_mutex_s____missing_field_name_8 {
   int __spins ;
   __pthread_slist_t __list ;
};
struct __pthread_mutex_s {
   int __lock ;
   unsigned int __count ;
   int __owner ;
   int __kind ;
   unsigned int __nusers ;
   union __anonunion___pthread_mutex_s____missing_field_name_8 __annonCompField1 ;
};
union __anonunion_pthread_mutex_t_7 {
   struct __pthread_mutex_s __data ;
   char __size[24] ;
   long __align ;
};
typedef union __anonunion_pthread_mutex_t_7 pthread_mutex_t;

typedef unsigned long pthread_t;
typedef pthread_t ldap_int_thread_t;
typedef ldap_int_thread_t ldap_pvt_thread_t;
typedef pthread_mutex_t ldap_int_thread_mutex_t;
typedef ldap_int_thread_mutex_t ldap_pvt_thread_mutex_t;

struct sessionlog {
   struct berval sl_mincsn ;
   int sl_num ;
   int sl_size ;
   slog_entry *sl_head ;
   slog_entry *sl_tail ;
   ldap_pvt_thread_mutex_t sl_mutex ;
};
typedef struct sessionlog sessionlog;

struct sync_control {
   struct sync_cookie sr_state ;
   int sr_rhint ;
};
typedef struct sync_control sync_control;

enum slap_access_t {
    ACL_INVALID_ACCESS = -1,
    ACL_NONE = 0,
    ACL_DISCLOSE = 1,
    ACL_AUTH = 2,
    ACL_COMPARE = 3,
    ACL_SEARCH = 4,
    ACL_READ = 5,
    ACL_WRITE_ = 6,
    ACL_MANAGE = 7,
    ACL_LAST = 8,
    ACL_LEVEL_MASK = 15,
    ACL_QUALIFIER1 = 256,
    ACL_QUALIFIER2 = 512,
    ACL_QUALIFIER3 = 1024,
    ACL_QUALIFIER4 = 2048,
    ACL_QUALIFIER_MASK = 3840,
    ACL_WADD = 262,
    ACL_WDEL = 518,
    ACL_WRITE = 774
} ;
typedef enum slap_access_t slap_access_t;
enum slap_control_e {
    ACL_INVALID_CONTROL = 0,
    ACL_STOP = 1,
    ACL_CONTINUE = 2,
    ACL_BREAK = 3
} ;
typedef enum slap_control_e slap_control_t;
enum slap_style_e {
    ACL_STYLE_REGEX = 0,
    ACL_STYLE_EXPAND = 1,
    ACL_STYLE_BASE = 2,
    ACL_STYLE_ONE = 3,
    ACL_STYLE_SUBTREE = 4,
    ACL_STYLE_CHILDREN = 5,
    ACL_STYLE_LEVEL = 6,
    ACL_STYLE_ATTROF = 7,
    ACL_STYLE_ANONYMOUS = 8,
    ACL_STYLE_USERS = 9,
    ACL_STYLE_SELF = 10,
    ACL_STYLE_IP = 11,
    ACL_STYLE_IPV6 = 12,
    ACL_STYLE_PATH = 13
} ;
typedef enum slap_style_e slap_style_t;


struct ObjectClass;
typedef struct ObjectClass ObjectClass;
struct AttributeType;
typedef struct AttributeType AttributeType;
struct AttributeDescription;
typedef struct AttributeDescription AttributeDescription;
struct AttributeName;
typedef struct AttributeName AttributeName;
struct AttributeAssertion;
typedef struct AttributeAssertion AttributeAssertion;
struct Attribute;
typedef struct Attribute Attribute;
struct Entry;
typedef struct Entry Entry;
struct Filter;
typedef struct Filter Filter;
struct Modification;
typedef struct Modification Modification;
struct Modifications;
typedef struct Modifications Modifications;
struct BackendInfo;
typedef struct BackendInfo BackendInfo;
struct BackendDB;
typedef struct BackendDB BackendDB;
typedef BackendDB Backend;
struct Connection;
typedef struct Connection Connection;
struct Operation;
typedef struct Operation Operation;

struct Modification {
   AttributeDescription *sm_desc ;
   BerVarray sm_values ;
   BerVarray sm_nvalues ;
   unsigned int sm_numvals ;
   short sm_op ;
   short sm_flags ;
   struct berval sm_type ;
};
struct Modifications {
   Modification sml_mod ;
   Modifications *sml_next ;
};

struct ldapcontrol {
   char *ldctl_oid ;
   struct berval ldctl_value ;
   char ldctl_iscritical ;
};
typedef struct ldapcontrol LDAPControl;

struct AttributeDescription {
   AttributeDescription *ad_next ;
  //   AttributeType *ad_type ;
   struct berval ad_cname ;
   struct berval ad_tags ;
   unsigned int ad_flags ;
};
struct AttributeName {
   struct berval an_name ;
   AttributeDescription *an_desc ;
   int an_flags ;
  //   ObjectClass *an_oc ;
};
struct AttributeAssertion {
   AttributeDescription *aa_desc ;
   struct berval aa_value ;
};


enum slap_reply_e {
    REP_RESULT = 0,
    REP_SASL = 1,
    REP_EXTENDED = 2,
    REP_SEARCH = 3,
    REP_SEARCHREF = 4,
    REP_INTERMEDIATE = 5,
    REP_GLUE_RESULT = 6
} ;
typedef enum slap_reply_e slap_reply_t;
struct rep_sasl_s {
   struct berval *r_sasldata ;
};
typedef struct rep_sasl_s rep_sasl_s;
struct rep_extended_s {
   char const   *r_rspoid ;
   struct berval *r_rspdata ;
};
typedef struct rep_extended_s rep_extended_s;
struct rep_search_s {
   Entry *r_entry ;
   slap_mask_t r_attr_flags ;
   Attribute *r_operational_attrs ;
   AttributeName *r_attrs ;
   int r_nentries ;
   BerVarray r_v2ref ;
};
typedef struct rep_search_s rep_search_s;
union sr_u {
   rep_search_s sru_search ;
   rep_sasl_s sru_sasl ;
   rep_extended_s sru_extended ;
};
struct SlapReply {
   slap_reply_t sr_type ;
   ber_tag_t sr_tag ;
   ber_int_t sr_msgid ;
   ber_int_t sr_err ;
   char const   *sr_matched ;
   char const   *sr_text ;
   BerVarray sr_ref ;
   LDAPControl **sr_ctrls ;
   union sr_u sr_un ;
   slap_mask_t sr_flags ;
};

typedef int slap_response(Operation * , SlapReply * );
struct slap_callback {
   struct slap_callback *sc_next ;
   slap_response *sc_response ;
   slap_response *sc_cleanup ;
   void *sc_private ;
};
typedef struct slap_callback slap_callback;

typedef int BI_op_func(Operation *op , SlapReply *rs );
typedef BI_op_func BI_op_bind;
typedef BI_op_func BI_op_unbind;
typedef BI_op_func BI_op_search;
typedef BI_op_func BI_op_compare;
typedef BI_op_func BI_op_modify;
typedef BI_op_func BI_op_modrdn;
typedef BI_op_func BI_op_add;
typedef BI_op_func BI_op_delete;
typedef BI_op_func BI_op_abandon;
typedef BI_op_func BI_op_extended;
typedef BI_op_func BI_op_cancel;
typedef BI_op_func BI_chk_referrals;
typedef BI_op_func BI_chk_controls;

struct BackendInfo {
     char *bi_type ;

   BI_op_bind *bi_op_bind ;
   BI_op_unbind *bi_op_unbind ;
   BI_op_search *bi_op_search ;
   BI_op_compare *bi_op_compare ;
   BI_op_modify *bi_op_modify ;
   BI_op_modrdn *bi_op_modrdn ;
   BI_op_add *bi_op_add ;
   BI_op_delete *bi_op_delete ;
   BI_op_abandon *bi_op_abandon ;
   BI_op_extended *bi_extended ;
   BI_op_cancel *bi_op_cancel ;

};



union f_un_u {
   ber_int_t f_un_result ;
   AttributeDescription *f_un_desc ;
   AttributeAssertion *f_un_ava ;
   Filter *f_un_complex ;
};
struct Filter {
   ber_tag_t f_choice ;
   union f_un_u f_un ;
   AttributeAssertion *f_un_ava ;
   Filter *f_next ;
};

typedef void SEND_LDAP_RESULT(Operation *op , SlapReply *rs );
typedef int SEND_SEARCH_ENTRY(Operation *op , SlapReply *rs );
typedef int SEND_SEARCH_REFERENCE(Operation *op , SlapReply *rs );
typedef void SEND_LDAP_EXTENDED(Operation *op , SlapReply *rs );
typedef void SEND_LDAP_INTERMEDIATE(Operation *op , SlapReply *rs );

struct Opheader {
   unsigned long oh_opid ;
   unsigned long oh_connid ;
   Connection *oh_conn ;
   ber_int_t oh_msgid ;
   ber_int_t oh_protocol ;
   ldap_pvt_thread_t oh_tid ;
   void *oh_threadctx ;
   void *oh_tmpmemctx ;
   BerMemoryFunctions *oh_tmpmfuncs ;
  //slap_counters_t *oh_counters ;
   char oh_log_prefix[256] ;
};
typedef struct Opheader Opheader;
struct c_o {
   struct Operation *stqh_first ;
   struct Operation **stqh_last ;
};
struct c_po {
   struct Operation *stqh_first ;
   struct Operation **stqh_last ;
};
struct Connection {
   int c_struct_state ;
   int c_conn_state ;
   int c_conn_idx ;
   ber_socket_t c_sd ;
   char const   *c_close_reason ;
   ldap_pvt_thread_mutex_t c_mutex ;
   Sockbuf *c_sb ;
   time_t c_starttime ;
   time_t c_activitytime ;
   unsigned long c_connid ;
   struct berval c_peer_domain ;
   struct berval c_peer_name ;
  //Listener *c_listener ;
   struct berval c_sasl_bind_mech ;
   struct berval c_sasl_dn ;
   struct berval c_sasl_authz_dn ;
   Backend *c_authz_backend ;
   void *c_authz_cookie ;
  //AuthorizationInformation c_authz ;
   ber_int_t c_protocol ;
   struct c_o c_ops ;
   struct c_po c_pending_ops ;
   ldap_pvt_thread_mutex_t c_write1_mutex ;
  //ldap_pvt_thread_cond_t c_write1_cv ;
   ldap_pvt_thread_mutex_t c_write2_mutex ;
  //ldap_pvt_thread_cond_t c_write2_cv ;
   BerElement *c_currentber ;
   int c_writers ;
   char c_writing ;
   char c_sasl_bind_in_progress ;
   char c_writewaiter ;
   char c_is_tls ;
   char c_needs_tls_accept ;
   char c_sasl_layers ;
   char c_sasl_done ;
   void *c_sasl_authctx ;
   void *c_sasl_sockctx ;
   void *c_sasl_extra ;
   Operation *c_sasl_bindop ;
  //PagedResultsState c_pagedresults_state ;
   long c_n_ops_received ;
   long c_n_ops_executing ;
   long c_n_ops_pending ;
   long c_n_ops_completed ;
   long c_n_get ;
   long c_n_read ;
   long c_n_write ;
   void *c_extensions ;
  //ldap_pvt_thread_start_t *c_clientfunc ;
   void *c_clientarg ;
   SEND_LDAP_RESULT *c_send_ldap_result ;
   SEND_SEARCH_ENTRY *c_send_search_entry ;
   SEND_SEARCH_REFERENCE *c_send_search_reference ;
   SEND_LDAP_EXTENDED *c_send_ldap_extended ;
   SEND_LDAP_INTERMEDIATE *c_send_ldap_intermediate ;
};


struct req_bind_s {
   int rb_method ;
   struct berval rb_cred ;
   struct berval rb_edn ;
   slap_ssf_t rb_ssf ;
   struct berval rb_mech ;
};
typedef struct req_bind_s req_bind_s;
struct req_search_s {
   int rs_scope ;
   int rs_deref ;
   int rs_slimit ;
   int rs_tlimit ;
   struct slap_limits_set *rs_limit ;
   int rs_attrsonly ;
   AttributeName *rs_attrs ;
   Filter *rs_filter ;
   struct berval rs_filterstr ;
};
typedef struct req_search_s req_search_s;
struct req_compare_s {
   AttributeAssertion *rs_ava ;
};
typedef struct req_compare_s req_compare_s;
struct req_modifications_s {
   Modifications *rs_modlist ;
   char rs_no_opattrs ;
};
typedef struct req_modifications_s req_modifications_s;
struct req_modify_s {
   req_modifications_s rs_mods ;
   int rs_increment ;
};
typedef struct req_modify_s req_modify_s;
struct req_modrdn_s {
   req_modifications_s rs_mods ;
   int rs_deleteoldrdn ;
   struct berval rs_newrdn ;
   struct berval rs_nnewrdn ;
   struct berval *rs_newSup ;
   struct berval *rs_nnewSup ;
};
typedef struct req_modrdn_s req_modrdn_s;
struct req_add_s {
   Modifications *rs_modlist ;
   Entry *rs_e ;
};
typedef struct req_add_s req_add_s;
struct req_abandon_s {
   ber_int_t rs_msgid ;
};
typedef struct req_abandon_s req_abandon_s;
struct req_extended_s {
   struct berval rs_reqoid ;
   int rs_flags ;
   struct berval *rs_reqdata ;
};
typedef struct req_extended_s req_extended_s;
struct req_pwdexop_s {
   struct req_extended_s rs_extended ;
   struct berval rs_old ;
   struct berval rs_new ;
   Modifications *rs_mods ;
   Modifications **rs_modtail ;
};
typedef struct req_pwdexop_s req_pwdexop_s;

union OpRequest {
   req_add_s oq_add ;
   req_bind_s oq_bind ;
   req_compare_s oq_compare ;
   req_modify_s oq_modify ;
   req_modrdn_s oq_modrdn ;
   req_search_s oq_search ;
   req_abandon_s oq_abandon ;
   req_abandon_s oq_cancel ;
   req_extended_s oq_extended ;
   req_pwdexop_s oq_pwdexop ;
};
typedef union OpRequest OpRequest;


struct __anonstruct_OpExtra_oe_next_92 {
   struct OpExtra *sle_next ;
};
struct OpExtra {
   struct __anonstruct_OpExtra_oe_next_92 oe_next ;
   void *oe_key ;
};
struct o_e {
   struct OpExtra *slh_first ;
};
struct __anonstruct_Operation_o_next_93 {
   struct Operation *stqe_next ;
};
struct Operation {
   Opheader *o_hdr ;
   ber_tag_t o_tag ;
   time_t o_time ;
   int o_tincr ;
   BackendDB *o_bd ;
   struct berval o_req_dn ;
   struct berval o_req_ndn ;
   OpRequest o_request ;

   char o_do_not_cache ;
   char o_is_auth_check ;
   char o_dont_replicate ;
   slap_access_t o_acl_priv ;
   char o_nocaching ;
   char o_delete_glue_parent ;
   char o_no_schema_check ;
   char o_no_subordinate_glue ;
   char o_ctrlflag[32] ;
   void **o_controls ;

   slap_callback *o_callback ;
   struct berval o_csn ;
   void *o_private ;
   struct __anonstruct_Operation_o_next_93 o_next ;
};


struct slap_overinfo;
struct slap_overinst {
   BackendInfo on_bi ;
   slap_response *on_response ;
   struct slap_overinfo *on_info ;
   struct slap_overinst *on_next ;
};
typedef struct slap_overinst slap_overinst;
struct slap_overinfo {
   BackendInfo oi_bi ;
   BackendInfo *oi_orig ;
   BackendDB *oi_origdb ;
   struct slap_overinst *oi_list ;
};
typedef struct slap_overinfo slap_overinfo;
struct slap_control_ids {
   int sc_LDAPsync ;
   int sc_assert ;
   int sc_domainScope ;
   int sc_dontUseCopy ;
   int sc_manageDSAit ;
   int sc_modifyIncrement ;
   int sc_noOp ;
   int sc_pagedResults ;
   int sc_permissiveModify ;
   int sc_postRead ;
   int sc_preRead ;
   int sc_proxyAuthz ;
   int sc_relax ;
   int sc_searchOptions ;
   int sc_subentries ;
   int sc_treeDelete ;
   int sc_valuesReturnFilter ;
};

struct slap_limits_set {
   int lms_t_soft ;
   int lms_t_hard ;
   int lms_s_soft ;
   int lms_s_hard ;
   int lms_s_unchecked ;
   int lms_s_pr ;
   int lms_s_pr_hide ;
   int lms_s_pr_total ;
};

struct BackendDB {
   BackendInfo *bd_info ;
   BackendDB *bd_self ;
   char be_ctrls[33] ;
   slap_mask_t be_flags ;
   slap_mask_t be_restrictops ;
   slap_mask_t be_requires ;
   slap_ssf_set_t be_ssf_set ;
   BerVarray be_suffix ;
   BerVarray be_nsuffix ;
   struct berval be_schemadn ;
   struct berval be_schemandn ;
   struct berval be_rootdn ;
   struct berval be_rootndn ;
   struct berval be_rootpw ;
   unsigned int be_max_deref_depth ;
   struct slap_limits_set be_def_limit ;
   struct slap_limits **be_limits ;
  /*
   AccessControl *be_acl ;
   slap_access_t be_dfltaccess ;
   struct berval be_update_ndn ;
   BerVarray be_update_refs ;
   struct be_pcl *be_pending_csn_list ;
   ldap_pvt_thread_mutex_t be_pcl_mutex ;
   struct syncinfo_s *be_syncinfo ;
   void *be_pb ;
   struct ConfigOCs *be_cf_ocs ;
   void *be_private ;
   struct __anonstruct_BackendDB_be_next_88 be_next ;
  */
};
struct slap_internal_schema {
   ObjectClass *si_oc_top ;
   ObjectClass *si_oc_extensibleObject ;
   ObjectClass *si_oc_alias ;
   ObjectClass *si_oc_referral ;
   ObjectClass *si_oc_rootdse ;
   ObjectClass *si_oc_subentry ;
   ObjectClass *si_oc_subschema ;
   ObjectClass *si_oc_collectiveAttributeSubentry ;
   ObjectClass *si_oc_dynamicObject ;
   ObjectClass *si_oc_glue ;
   ObjectClass *si_oc_syncConsumerSubentry ;
   ObjectClass *si_oc_syncProviderSubentry ;
   AttributeDescription *si_ad_objectClass ;
   AttributeDescription *si_ad_structuralObjectClass ;
   AttributeDescription *si_ad_creatorsName ;
   AttributeDescription *si_ad_createTimestamp ;
   AttributeDescription *si_ad_modifiersName ;
   AttributeDescription *si_ad_modifyTimestamp ;
   AttributeDescription *si_ad_hasSubordinates ;
   AttributeDescription *si_ad_subschemaSubentry ;
   AttributeDescription *si_ad_collectiveSubentries ;
   AttributeDescription *si_ad_collectiveExclusions ;
   AttributeDescription *si_ad_entryDN ;
   AttributeDescription *si_ad_entryUUID ;
   AttributeDescription *si_ad_entryCSN ;
   AttributeDescription *si_ad_namingCSN ;
   AttributeDescription *si_ad_dseType ;
   AttributeDescription *si_ad_syncreplCookie ;
   AttributeDescription *si_ad_syncTimestamp ;
   AttributeDescription *si_ad_contextCSN ;
   AttributeDescription *si_ad_altServer ;
   AttributeDescription *si_ad_namingContexts ;
   AttributeDescription *si_ad_supportedControl ;
   AttributeDescription *si_ad_supportedExtension ;
   AttributeDescription *si_ad_supportedLDAPVersion ;
   AttributeDescription *si_ad_supportedSASLMechanisms ;
   AttributeDescription *si_ad_supportedFeatures ;
   AttributeDescription *si_ad_monitorContext ;
   AttributeDescription *si_ad_vendorName ;
   AttributeDescription *si_ad_vendorVersion ;
   AttributeDescription *si_ad_configContext ;
   AttributeDescription *si_ad_administrativeRole ;
   AttributeDescription *si_ad_subtreeSpecification ;
   AttributeDescription *si_ad_attributeTypes ;
   AttributeDescription *si_ad_ditContentRules ;
   AttributeDescription *si_ad_ditStructureRules ;
   AttributeDescription *si_ad_ldapSyntaxes ;
   AttributeDescription *si_ad_matchingRules ;
   AttributeDescription *si_ad_matchingRuleUse ;
   AttributeDescription *si_ad_nameForms ;
   AttributeDescription *si_ad_objectClasses ;
   AttributeDescription *si_ad_aliasedObjectName ;
   AttributeDescription *si_ad_ref ;
   AttributeDescription *si_ad_entry ;
   AttributeDescription *si_ad_children ;
   AttributeDescription *si_ad_saslAuthzTo ;
   AttributeDescription *si_ad_saslAuthzFrom ;
   AttributeDescription *si_ad_entryTtl ;
   AttributeDescription *si_ad_dynamicSubtrees ;
   AttributeDescription *si_ad_distinguishedName ;
   AttributeDescription *si_ad_name ;
   AttributeDescription *si_ad_cn ;
   AttributeDescription *si_ad_uid ;
   AttributeDescription *si_ad_uidNumber ;
   AttributeDescription *si_ad_gidNumber ;
   AttributeDescription *si_ad_userPassword ;
   AttributeDescription *si_ad_labeledURI ;
   AttributeDescription *si_ad_description ;
   AttributeDescription *si_ad_seeAlso ;
   AttributeType *si_at_undefined ;
   AttributeType *si_at_proxied ;

};

/*************************************************************/


extern  __attribute__((__nothrow__)) void *malloc(size_t __size )  __attribute__((__malloc__)) ;
extern  __attribute__((__nothrow__)) void *calloc(size_t __nmemb , size_t __size )  __attribute__((__malloc__)) ;
extern  __attribute__((__nothrow__)) void *realloc(void *__ptr , size_t __size )  __attribute__((__warn_unused_result__)) ;
extern  __attribute__((__nothrow__)) void free(void *__ptr ) ;
extern  __attribute__((__nothrow__)) void *memmove(void *__dest , void const   *__src ,
                                                   size_t __n )  __attribute__((__nonnull__(1,2))) ;
extern  __attribute__((__nothrow__)) void *memset(void *__s , int __c , size_t __n )  __attribute__((__nonnull__(1))) ;
extern  __attribute__((__nothrow__)) size_t strlen(char const   *__s )  __attribute__((__pure__,
__nonnull__(1))) ;


#define ber_memalloc_x(len,ctx)\
  malloc(len)

#define slap_sl_malloc(s, ctx)\
  malloc(s)


int slap_debug ;
int ldap_syslog ;
int ldap_syslog_level ;
struct slap_internal_schema slap_schema ;

BerMemoryFunctions slap_sl_mfuncs ;

void * slap_sl_malloc_wrap (ber_len_t s, void * ctx) {
  return malloc(s);
}
void * slap_sl_calloc_wrap (ber_len_t n, ber_len_t s, void * ctx) {
  return calloc(n, s);
}
void * slap_sl_realloc_wrap (void * p, ber_len_t s, void * ctx) {
  return realloc(p, s);
}
void slap_sl_free(void *ptr , void *ctx ) {
  free (ptr);
}

BerMemoryFunctions slap_sl_mfuncs  = {& slap_sl_malloc_wrap, & slap_sl_calloc_wrap, & slap_sl_realloc_wrap, & slap_sl_free};


// JAN add a wrapper so that the type signature matches...
void * ch_malloc_wrapper(ber_len_t size, void *ign) {
  return malloc (size);
}

void * ch_calloc_wrapper(ber_len_t n, ber_len_t s, void *ign) {
  return calloc (n, s);
}
void * ch_realloc_wrapper(void * p, ber_len_t size, void *ign) {
  return realloc (p, size);
}
void ch_free_wrapper(void * p, void *ign) {
  free (p);
}

BerMemoryFunctions ch_mfuncs  =  {(& ch_malloc_wrapper), 
                                  (& ch_calloc_wrapper),
                                  (& ch_realloc_wrapper),
                                  (& ch_free_wrapper)};

BerMemoryFunctions *ber_int_memory_fns ;
BerMemoryFunctions *ber_int_memory_fns  =    (BerMemoryFunctions *)((void *)0);
static BerMemoryFunctions ber_int_memory_fns_datum  ;


struct slap_control_ids slap_cids ;
AttributeName *slap_anlist_all_attributes ;
int slap_serverID ;
struct lber_options ber_int_options ;
void (*ber_pvt_log_print)(char const   *buf ) ;
void (*ber_int_log_proc)(FILE *file , char const   *subsys , int level , char const   *fmt 
                         , ...) ;
FILE *ber_pvt_err_file ;
void (*ber_int_log_proc)(FILE *file , char const   *subsys , int level , char const   *fmt 
                         , ...)  =    (void (*)(FILE *file , char const   *subsys , int level , char const   *fmt  , ...))((void *)0);
FILE *ber_pvt_err_file  =    (FILE *)((void *)0);

extern struct _IO_FILE *stderr ;
static FILE *log_file  =    (FILE *)((void *)0);


/************************************************************/


void ( /* format attribute */  lutil_debug)(int debug , int level , char const   *fmt 
                                            , ...) 
{ char buffer[4096] ;
  va_list vl ;

  {
  if (! (level & debug)) {
    return;
  }
  __builtin_va_start(vl, fmt);
  vsnprintf((char * __restrict  )(buffer), sizeof(buffer), (char const   * __restrict  )fmt,
            vl);
  buffer[sizeof(buffer) - 1U] = (char )'\000';
  if ((unsigned int )log_file != (unsigned int )((void *)0)) {
    fputs((char const   * __restrict  )(buffer), (FILE * __restrict  )log_file);
    fflush(log_file);
  }
  fputs((char const   * __restrict  )(buffer), (FILE * __restrict  )stderr);
  __builtin_va_end(vl);
  return;
}
}

static int playlog_cb(Operation *op , SlapReply *rs ) 
{ 

  {
  if ((unsigned int )rs->sr_type == 3U) {
    (op->o_callback)->sc_private = (void *)1;
  }
  return (rs->sr_err);
}
}

struct berval *ber_str2bv_x(char const   *s , ber_len_t len , int dup , struct berval *bv ,
                            void *ctx ) 
{ struct berval *new ;
  int *tmp ;
  int *tmp___0 ;
  void *tmp___1 ;
  size_t tmp___2 ;
  int *tmp___3 ;
  char *tmp___4 ;
  void *tmp___5 ;

  {
  if ((unsigned int )s == (unsigned int )((void *)0)) {
    tmp = ber_errno_addr();
    *tmp = 1;
    return ((struct berval *)((void *)0));
  }
  if (bv) {
    new = bv;
  } else {
    tmp___1 = ber_memalloc_x((unsigned long )sizeof(struct berval ), ctx);
    new = (struct berval *)tmp___1;
    if ((unsigned int )new == (unsigned int )((void *)0)) {
      tmp___0 = ber_errno_addr();
      *tmp___0 = 2;
      return ((struct berval *)((void *)0));
    }
  }
  if (len) {
    new->bv_len = len;
  } else {
    tmp___2 = strlen(s);
    new->bv_len = (unsigned long )tmp___2;
  }
  if (dup) {
    tmp___5 = ber_memalloc_x(new->bv_len + 1UL, ctx);
    tmp___4 = (char *)tmp___5;
    new->bv_val = tmp___4;
    if ((unsigned int )tmp___4 == (unsigned int )((void *)0)) {
      tmp___3 = ber_errno_addr();
      *tmp___3 = 2;
      if (! bv) {
        ber_memfree_x((void *)new, ctx);
      }
      return ((struct berval *)((void *)0));
    }
    memmove((void *)new->bv_val, (void const   *)s, (unsigned int )new->bv_len);
    *(new->bv_val + new->bv_len) = (char )'\000';
  } else {
    new->bv_val = (char *)s;
  }
  return (new);
}
}


struct berval *ber_str2bv(char const   *s , ber_len_t len , int dup , struct berval *bv ) 
{ struct berval *tmp ;

  {
  tmp = ber_str2bv_x(s, len, dup, bv, (void *)0);
  return (tmp);
}
}

void slap_compose_sync_cookie(Operation *op , struct berval *cookie , BerVarray csn ,
                              int rid , int sid ) 
{ int len ;
  int numcsn ;
  char cookiestr[84] ;
  int tmp ;
  void *tmp___0 ;
  char *ptr ;
  int i ;
  void *tmp___1 ;
  void *tmp___2 ;
  int tmp___3 ;
  char *tmp___4 ;

  {
  numcsn = 0;
  if (csn) {
    while (! ((unsigned int )(csn + numcsn)->bv_val == (unsigned int )((void *)0))) {
      numcsn ++;
    }
  }
  if (numcsn == 0) {
    goto _L;
  } else {
    if (rid == -1) {
      _L: /* CIL Label */ 
      if (rid == -1) {
        cookiestr[0] = (char )'\000';
        len = 0;
      } else {
        len = snprintf((char * __restrict  )(cookiestr), sizeof(cookiestr), (char const   * __restrict  )"rid=%03d",
                       rid);
        if (sid >= 0) {
          tmp = sprintf((char * __restrict  )(cookiestr + len), (char const   * __restrict  )",sid=%03x",
                        sid);
          len += tmp;
        }
      }
      if (op) {
        tmp___0 = (op->o_hdr)->oh_tmpmemctx;
      } else {
        tmp___0 = (void *)0;
      }
      ber_str2bv_x((char const   *)(cookiestr), (unsigned long )len, 1, cookie, tmp___0);
    } else {
      len = 0;
      i = 0;
      while (i < numcsn) {
        len = (int )((ber_len_t )len + ((csn + i)->bv_len + 1UL));
        i ++;
      }
      len = (int )((unsigned int )len + (sizeof("rid=123,csn=") - 1U));
      if (sid >= 0) {
        len = (int )((unsigned int )len + (sizeof("sid=xxx,") - 1U));
      }
      if (op) {
        tmp___1 = (op->o_hdr)->oh_tmpmemctx;
      } else {
        tmp___1 = (void *)0;
      }
      tmp___2 = slap_sl_malloc((unsigned long )len, tmp___1);
      cookie->bv_val = (char *)tmp___2;
      len = sprintf((char * __restrict  )cookie->bv_val, (char const   * __restrict  )"rid=%03d,",
                    rid);
      ptr = cookie->bv_val + len;
      if (sid >= 0) {
        tmp___3 = sprintf((char * __restrict  )ptr, (char const   * __restrict  )"sid=%03x,",
                          sid);
        ptr += tmp___3;
      }
      ptr = lutil_strcopy(ptr, "csn=");
      i = 0;
      while (i < numcsn) {
        ptr = lutil_strncopy(ptr, (char const   *)(csn + i)->bv_val, (unsigned int )(csn + i)->bv_len);
        tmp___4 = ptr;
        ptr ++;
        *tmp___4 = (char )';';
        i ++;
      }
      ptr --;
      *ptr = (char )'\000';
      cookie->bv_len = (unsigned long )(ptr - cookie->bv_val);
    }
  }
  return;
}
}

int ber_set_option(void *item , int option , void const   *invalue ) 
{ BerElement *ber ;
  Sockbuf *sb ;
  int *tmp ;
  BerMemoryFunctions const   *f ;
  int *tmp___0 ;
  int *tmp___1 ;
  int *tmp___2 ;

  {
  if ((unsigned int )invalue == (unsigned int )((void *)0)) {
    tmp = ber_errno_addr();
    *tmp = 1;
    return (-1);
  }
  if ((unsigned int )item == (unsigned int )((void *)0)) {
    switch (option) {
    case 2: 
    ber_int_options.lbo_debug = (int )*((int const   *)invalue);
    return (0);
    case 32769: 
    ber_pvt_log_print = (void (*)(char const   *buf ))invalue;
    return (0);
    case 32772: 
    ber_pvt_err_file = (FILE *)((void *)invalue);
    return (0);
    case 32773: 
    return (-1);
    case 32770: 
    if ((unsigned int )ber_int_memory_fns == (unsigned int )((void *)0)) {
      f = (BerMemoryFunctions const   *)invalue;
      if (f->bmf_malloc) {
        if (f->bmf_calloc) {
          if (f->bmf_realloc) {
            if (! f->bmf_free) {
              tmp___0 = ber_errno_addr();
              *tmp___0 = 1;
              return (-1);
            }
          } else {
            tmp___0 = ber_errno_addr();
            *tmp___0 = 1;
            return (-1);
          }
        } else {
          tmp___0 = ber_errno_addr();
          *tmp___0 = 1;
          return (-1);
        }
      } else {
        tmp___0 = ber_errno_addr();
        *tmp___0 = 1;
        return (-1);
      }
      ber_int_memory_fns = & ber_int_memory_fns_datum;
      memmove((void *)ber_int_memory_fns, (void const   *)f, sizeof(BerMemoryFunctions ));
      return (0);
    }
    break;
    case 32774: 
    ber_int_log_proc = (void (*)(FILE *file , char const   *subsys , int level , char const   *fmt 
                                 , ...))invalue;
    return (0);
    }
    tmp___1 = ber_errno_addr();
    *tmp___1 = 1;
    return (-1);
  }
  ber = (BerElement *)item;
  sb = (Sockbuf *)item;
  switch (option) {
  case 1: 
  if (! ((int )ber->ber_opts.lbo_valid == 2)) {
    __assert_fail("((ber)->ber_opts.lbo_valid==0x2)", "options.c", 197U, "ber_set_option");
  }
  ber->ber_opts.lbo_options = (unsigned short )*((int const   *)invalue);
  return (0);
  case 2: 
  if (! ((int )ber->ber_opts.lbo_valid == 2)) {
    __assert_fail("((ber)->ber_opts.lbo_valid==0x2)", "options.c", 202U, "ber_set_option");
  }
  ber->ber_opts.lbo_debug = (int )*((int const   *)invalue);
  return (0);
  case 3: 
  if (! ((int )ber->ber_opts.lbo_valid == 2)) {
    __assert_fail("((ber)->ber_opts.lbo_valid==0x2)", "options.c", 207U, "ber_set_option");
  }
  ber->ber_end = ber->ber_ptr + *((ber_len_t const   *)invalue);
  return (0);
  case 4: 
  if (! ((int )ber->ber_opts.lbo_valid == 2)) {
    __assert_fail("((ber)->ber_opts.lbo_valid==0x2)", "options.c", 212U, "ber_set_option");
  }
  ber->ber_end = ber->ber_buf + *((ber_len_t const   *)invalue);
  return (0);
  case 5: 
  if (! ((int )ber->ber_opts.lbo_valid == 2)) {
    __assert_fail("((ber)->ber_opts.lbo_valid==0x2)", "options.c", 217U, "ber_set_option");
  }
  ber->ber_ptr = ber->ber_buf + *((ber_len_t const   *)invalue);
  return (0);
  case 6: 
  if (! ((int )ber->ber_opts.lbo_valid == 2)) {
    __assert_fail("((ber)->ber_opts.lbo_valid==0x2)", "options.c", 222U, "ber_set_option");
  }
  ber->ber_memctx = *((void **)invalue);
  return (0);
  default: 
  tmp___2 = ber_errno_addr();
  *tmp___2 = 1;
  break;
  }
  return (-1);
}
}

void ber_init2(BerElement *ber , struct berval *bv , int options___0 ) 
{ 

  {
  if (! ((unsigned int )ber != (unsigned int )((void *)0))) {
    __assert_fail("ber != ((void *)0)", "io.c", 315U, "ber_init2");
  }
  memset((void *)((char *)ber), '\000', sizeof(BerElement ));
  ber->ber_opts.lbo_valid = (short)2;
  ber->ber_tag = 4294967295UL;
  ber->ber_opts.lbo_options = (unsigned short )((char )options___0);
  ber->ber_opts.lbo_debug = ber_int_options.lbo_debug;
  if ((unsigned int )bv != (unsigned int )((void *)0)) {
    ber->ber_buf = bv->bv_val;
    ber->ber_ptr = ber->ber_buf;
    ber->ber_end = ber->ber_buf + bv->bv_len;
  }
  if (! ((int )ber->ber_opts.lbo_valid == 2)) {
    __assert_fail("((ber)->ber_opts.lbo_valid==0x2)", "io.c", 329U, "ber_init2");
  }
  return;
}
}

int ber_flatten2(BerElement *ber , struct berval *bv , int alloc ) 
{ ber_len_t len ;
  void *tmp ;

  {
  if (! ((unsigned int )bv != (unsigned int )((void *)0))) {
    __assert_fail("bv != ((void *)0)", "io.c", 387U, "ber_flatten2");
  }
  if ((unsigned int )bv == (unsigned int )((void *)0)) {
    return (-1);
  }
  if ((unsigned int )ber == (unsigned int )((void *)0)) {
    bv->bv_val = (char *)((void *)0);
    bv->bv_len = 0UL;
  } else {
    len = (unsigned long )(ber->ber_ptr - ber->ber_buf);
    if (alloc) {
      tmp = ber_memalloc_x(len + 1UL, ber->ber_memctx);
      bv->bv_val = (char *)tmp;
      if ((unsigned int )bv->bv_val == (unsigned int )((void *)0)) {
        return (-1);
      }
      memmove((void *)bv->bv_val, (void const   *)ber->ber_buf, (unsigned int )len);
      *(bv->bv_val + len) = (char )'\000';
    } else {
      if ((unsigned int )ber->ber_buf != (unsigned int )((void *)0)) {
        bv->bv_val = ber->ber_buf;
        *(bv->bv_val + len) = (char )'\000';
      } else {
        bv->bv_val = (char *)"";
      }
    }
    bv->bv_len = len;
  }
  return (0);
}
}


static int syncprov_sendinfo(Operation *op , SlapReply *rs , int type , struct berval *cookie ,
                             int refreshDone , BerVarray syncUUIDs , int refreshDeletes ) 
{ BerElementBuffer berbuf ;
  BerElement *ber ;
  struct berval rspdata ;
  int ret ;

  {
  ber = (BerElement *)(& berbuf);
  ber_init2(ber, (struct berval *)((void *)0), 1);
  ber_set_option((void *)ber, 6, (void const   *)(& (op->o_hdr)->oh_tmpmemctx));
  if (type) {
    switch (type) {
    case 128UL: 
    ber_printf(ber, "tO", type, cookie);
    break;
    case 161UL: 
    case 162UL: 
    ber_printf(ber, "t{", type);
    if (cookie) {
      ber_printf(ber, "O", cookie);
    }
    if (refreshDone == 0) {
      ber_printf(ber, "b", refreshDone);
    }
    ber_printf(ber, "N}");
    break;
    case 163UL: 
    ber_printf(ber, "t{", type);
    if (cookie) {
      ber_printf(ber, "O", cookie);
    }
    if (refreshDeletes == 1) {
      ber_printf(ber, "b", refreshDeletes);
    }
    ber_printf(ber, "[W]", syncUUIDs);
    ber_printf(ber, "N}");
    break;
    default: ;
    while (1) {
      if (slap_debug & 1) {
        lutil_debug(slap_debug, 1, "syncprov_sendinfo: invalid syncinfo type (%d)\n",
                    type, 0, 0);
      }
      if (ldap_syslog & 1) {
        syslog(ldap_syslog_level, "syncprov_sendinfo: invalid syncinfo type (%d)\n",
               type, 0, 0);
      }
      break;
    }
    return (80);
    }
  }
  ret = ber_flatten2(ber, & rspdata, 0);
  if (ret < 0) {
    while (1) {
      if (slap_debug & 1) {
        lutil_debug(slap_debug, 1, "syncprov_sendinfo: ber_flatten2 failed\n", 0,
                    0, 0);
      }
      if (ldap_syslog & 1) {
        syslog(ldap_syslog_level, "syncprov_sendinfo: ber_flatten2 failed\n", 0, 0,
               0);
      }
      break;
    }
    while (1) {
      rs->sr_err = 80;
      rs->sr_text = "internal error";
      (*(((op->o_hdr)->oh_conn)->c_send_ldap_result))(op, rs);
      break;
    }
    return (ret);
  }
  rs->sr_un.sru_extended.r_rspoid = "1.3.6.1.4.1.4203.1.9.1.4";
  rs->sr_un.sru_extended.r_rspdata = & rspdata;
  (*(((op->o_hdr)->oh_conn)->c_send_ldap_intermediate))(op, rs);
  rs->sr_un.sru_extended.r_rspdata = (struct berval *)((void *)0);
  ber_free_buf(ber);
  return (0);
}
}


static void syncprov_playlog(Operation *op , SlapReply *rs , sessionlog *sl , 
                             sync_control *srs ,
                             BerVarray ctxcsn , int numcsns , int *sids ) 
{ slap_overinst *on ;
  slog_entry *se ;
  int i ;
  int j ;
  int ndel ;
  int num ;
  int nmods ;
  int mmods ;
  char cbuf[64] ;
  BerVarray uuids ;
  struct berval delcsn[2] ;
  void *tmp ;
  int k ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;
  Operation fop ;
  SlapReply frs ;
  int rc ;
  Filter mf ;
  Filter af ;
  AttributeAssertion eq ;
  slap_callback cb ;
  int tmp___6 ;
  struct berval cookie ;
  int tmp___7 ;
  struct berval *tmp___8 ;

  {
  on = (slap_overinst *)(op->o_bd)->bd_info;
  if (! sl->sl_num) {
    ldap_pvt_thread_mutex_unlock(& sl->sl_mutex);
    return;
  }
  num = sl->sl_num;
  i = 0;
  nmods = 0;
  tmp = (*(((op->o_hdr)->oh_tmpmfuncs)->bmf_malloc))((unsigned long )((unsigned int )(num + 1) * sizeof(struct berval ) + (unsigned int )(num * 16)),
                                                     (op->o_hdr)->oh_tmpmemctx);
  uuids = (BerValue *)tmp;
  (uuids + 0)->bv_val = (char *)((uuids + num) + 1);
  delcsn[0].bv_len = 0UL;
  delcsn[0].bv_val = cbuf;
  while (1) {
    delcsn[1].bv_len = 0UL;
    delcsn[1].bv_val = (char *)((void *)0);
    break;
  }
  while (1) {
    if (slap_debug & 16384) {
      lutil_debug(slap_debug, 16384, "srs csn %s\n", (srs->sr_state.ctxcsn + 0)->bv_val,
                  0, 0);
    }
    if (ldap_syslog & 16384) {
      syslog(ldap_syslog_level, "srs csn %s\n", (srs->sr_state.ctxcsn + 0)->bv_val,
             0, 0);
    }
    break;
  }
  se = sl->sl_head;
  while (se) {
    while (1) {
      if (slap_debug & 16384) {
        lutil_debug(slap_debug, 16384, "log csn %s\n", se->se_csn.bv_val, 0, 0);
      }
      if (ldap_syslog & 16384) {
        syslog(ldap_syslog_level, "log csn %s\n", se->se_csn.bv_val, 0, 0);
      }
      break;
    }
    ndel = 1;
    k = 0;
    while (k < srs->sr_state.numcsns) {
      if (se->se_sid == *(srs->sr_state.sids + k)) {
        if (se->se_csn.bv_len < (srs->sr_state.ctxcsn + k)->bv_len) {
          ndel = -1;
        } else {
          if (se->se_csn.bv_len > (srs->sr_state.ctxcsn + k)->bv_len) {
            tmp___1 = 1;
          } else {
            tmp___0 = memcmp((void const   *)se->se_csn.bv_val, (void const   *)(srs->sr_state.ctxcsn + k)->bv_val,
                             (unsigned int )se->se_csn.bv_len);
            tmp___1 = tmp___0;
          }
          ndel = tmp___1;
        }
        break;
      }
      k ++;
    }
    if (ndel <= 0) {
      while (1) {
        if (slap_debug & 16384) {
          lutil_debug(slap_debug, 16384, "cmp %d, too old\n", ndel, 0, 0);
        }
        if (ldap_syslog & 16384) {
          syslog(ldap_syslog_level, "cmp %d, too old\n", ndel, 0, 0);
        }
        break;
      }
      goto __Cont;
    }
    ndel = 0;
    k = 0;
    while (k < numcsns) {
      if (se->se_sid == *(sids + k)) {
        if (se->se_csn.bv_len < (ctxcsn + k)->bv_len) {
          ndel = -1;
        } else {
          if (se->se_csn.bv_len > (ctxcsn + k)->bv_len) {
            tmp___3 = 1;
          } else {
            tmp___2 = memcmp((void const   *)se->se_csn.bv_val, (void const   *)(ctxcsn + k)->bv_val,
                             (unsigned int )se->se_csn.bv_len);
            tmp___3 = tmp___2;
          }
          ndel = tmp___3;
        }
        break;
      }
      k ++;
    }
    if (ndel > 0) {
      while (1) {
        if (slap_debug & 16384) {
          lutil_debug(slap_debug, 16384, "cmp %d, too new\n", ndel, 0, 0);
        }
        if (ldap_syslog & 16384) {
          syslog(ldap_syslog_level, "cmp %d, too new\n", ndel, 0, 0);
        }
        break;
      }
      break;
    }
    if (se->se_tag == 74UL) {
      j = i;
      i ++;
      memmove((void *)(cbuf), (void const   *)se->se_csn.bv_val, (unsigned int )se->se_csn.bv_len);
      delcsn[0].bv_len = se->se_csn.bv_len;
      *(delcsn[0].bv_val + delcsn[0].bv_len) = (char )'\000';
    } else {
      nmods ++;
      j = num - nmods;
    }
    (uuids + j)->bv_val = (uuids + 0)->bv_val + j * 16;
    memmove((void *)(uuids + j)->bv_val, (void const   *)se->se_uuid.bv_val, 16U);
    (uuids + j)->bv_len = 16UL;
    __Cont: 
    se = se->se_next;
  }
  ldap_pvt_thread_mutex_unlock(& sl->sl_mutex);
  ndel = i;
  i = ndel;
  while (i < num - nmods) {
    (uuids + i)->bv_len = 0UL;
    i ++;
  }
  mmods = nmods;
  i = 0;
  while (i < nmods) {
    j = 0;
    while (j < ndel) {
      if ((uuids + j)->bv_len == (uuids + ((num - 1) - i))->bv_len) {
        tmp___4 = memcmp((void const   *)(uuids + j)->bv_val, (void const   *)(uuids + ((num - 1) - i))->bv_val,
                         (unsigned int )(uuids + j)->bv_len);
        if (tmp___4 == 0) {
          (uuids + ((num - 1) - i))->bv_len = 0UL;
          mmods --;
          break;
        }
      }
      j ++;
    }
    if ((uuids + ((num - 1) - i))->bv_len == 0UL) {
      goto __Cont___0;
    }
    j = 0;
    while (j < i) {
      if ((uuids + ((num - 1) - j))->bv_len == (uuids + ((num - 1) - i))->bv_len) {
        tmp___5 = memcmp((void const   *)(uuids + ((num - 1) - j))->bv_val, (void const   *)(uuids + ((num - 1) - i))->bv_val,
                         (unsigned int )(uuids + ((num - 1) - j))->bv_len);
        if (tmp___5 == 0) {
          (uuids + ((num - 1) - i))->bv_len = 0UL;
          mmods --;
          break;
        }
      }
      j ++;
    }
    __Cont___0: 
    i ++;
  }
  if (mmods) {
    frs.sr_type = (enum slap_reply_e )0;
    frs.sr_tag = 0UL;
    frs.sr_msgid = 0;
    frs.sr_err = 0;
    frs.sr_matched = (char const   *)0;
    frs.sr_text = (char const   *)0;
    frs.sr_ref = (BerValue *)0;
    frs.sr_ctrls = (LDAPControl **)0;
    frs.sr_un.sru_search.r_entry = (Entry *)0;
    frs.sr_un.sru_search.r_attr_flags = 0UL;
    frs.sr_un.sru_search.r_operational_attrs = (Attribute *)0;
    frs.sr_un.sru_search.r_attrs = (AttributeName *)0;
    frs.sr_un.sru_search.r_nentries = 0;
    frs.sr_un.sru_search.r_v2ref = (BerValue *)0;
    frs.sr_flags = 0UL;
    eq.aa_desc = (AttributeDescription *)((void *)0);
    eq.aa_value.bv_len = 0UL;
    eq.aa_value.bv_val = (char *)((void *)0);
    cb.sc_next = (struct slap_callback *)0;
    cb.sc_response = (slap_response *)0;
    cb.sc_cleanup = (slap_response *)0;
    cb.sc_private = (void *)0;
    fop = *op;
    fop.o_ctrlflag[slap_cids.sc_LDAPsync] = (char)0;
    fop.o_callback = & cb;
    fop.o_request.oq_search.rs_limit = (struct slap_limits_set *)((void *)0);
    fop.o_request.oq_search.rs_tlimit = -1;
    fop.o_request.oq_search.rs_attrs = slap_anlist_all_attributes;
    fop.o_request.oq_search.rs_attrsonly = 0;
    fop.o_ctrlflag[slap_cids.sc_manageDSAit] = (char)3;
    af.f_choice = 160UL;
    af.f_next = (Filter *)((void *)0);
    af.f_un.f_un_complex = & mf;
    mf.f_choice = 163UL;
    mf.f_un.f_un_ava = & eq;
    (mf.f_un.f_un_ava)->aa_desc = slap_schema.si_ad_entryUUID;
    mf.f_next = fop.o_request.oq_search.rs_filter;
    fop.o_request.oq_search.rs_filter = & af;
    cb.sc_response = & playlog_cb;
    (fop.o_bd)->bd_info = (BackendInfo *)on->on_info;
    i = ndel;
    while (i < num) {
      if ((uuids + i)->bv_len == 0UL) {
        goto __Cont___1;
      }
      (mf.f_un.f_un_ava)->aa_value = *(uuids + i);
      cb.sc_private = (void *)0;
      fop.o_request.oq_search.rs_slimit = 1;
      frs.sr_un.sru_search.r_nentries = 0;
      rc = (*(((fop.o_bd)->bd_info)->bi_op_search))(& fop, & frs);
      if (! cb.sc_private) {
        tmp___6 = ndel;
        ndel ++;
        *(uuids + tmp___6) = *(uuids + i);
      }
      __Cont___1: 
      i ++;
    }
    (fop.o_bd)->bd_info = (BackendInfo *)on;
  }
  if (ndel) {
    if (delcsn[0].bv_len) {
      if (slap_serverID) {
        tmp___7 = slap_serverID;
      } else {
        tmp___7 = -1;
      }
      slap_compose_sync_cookie(op, & cookie, delcsn, srs->sr_state.rid, tmp___7);
      while (1) {
        if (slap_debug & 16384) {
          lutil_debug(slap_debug, 16384, "syncprov_playlog: cookie=%s\n", cookie.bv_val,
                      0, 0);
        }
        if (ldap_syslog & 16384) {
          syslog(ldap_syslog_level, "syncprov_playlog: cookie=%s\n", cookie.bv_val,
                 0, 0);
        }
        break;
      }
    }
    (uuids + ndel)->bv_val = (char *)((void *)0);
    if (delcsn[0].bv_len) {
      tmp___8 = & cookie;
    } else {
      tmp___8 = (struct berval *)((void *)0);
    }
    syncprov_sendinfo(op, rs, 163, tmp___8, 0, uuids, 1);
    if (delcsn[0].bv_len) {
      (*(((op->o_hdr)->oh_tmpmfuncs)->bmf_free))((void *)cookie.bv_val, (op->o_hdr)->oh_tmpmemctx);
    }
  }

  (*(((op->o_hdr)->oh_tmpmfuncs)->bmf_free))((void *)uuids, (op->o_hdr)->oh_tmpmemctx);
  return;
}
}


