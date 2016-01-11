# 1 "ieee80211_rx.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "ieee80211_rx.c"


typedef unsigned char u8;
typedef signed char s8;
typedef unsigned short u16;
typedef unsigned int u32;

typedef unsigned char __u8;
typedef unsigned short __u16;
typedef unsigned int __u32;


typedef __u16 __le16;
typedef __u16 __be16;
typedef __u32 __le32;
typedef __u32 __be32;


typedef unsigned int __kernel_size_t;
typedef __kernel_size_t size_t;


struct list_head {
 struct list_head *next, *prev;
};
# 75 "ieee80211_rx.c"
struct ieee80211_info_element {
 u8 id;
 u8 len;
 u8 data[0];
} __attribute__ ((packed));




struct ieee80211_rx_stats {
 u32 mac_time;
 s8 rssi;
 u8 signal;
 u8 noise;
 u16 rate;
 u8 received_channel;
 u8 control;
 u8 mask;
 u8 freq;
 u16 len;
};


struct ieee80211_qos_parameters {
 __le16 cw_min[4];
 __le16 cw_max[4];
 u8 aifs[4];
 u8 flag[4];
 __le16 tx_op_limit[4];
} __attribute__ ((packed));


struct ieee80211_qos_data {
 struct ieee80211_qos_parameters parameters;
 int active;
 int supported;
 u8 param_count;
 u8 old_param_count;
};

struct ieee80211_tim_parameters {
 u8 tim_count;
 u8 tim_period;
} __attribute__ ((packed));

struct ieee80211_network {

 u8 bssid[6];
 u8 channel;

 u8 ssid[32 + 1];
 u8 ssid_len;

 struct ieee80211_qos_data qos_data;


 struct ieee80211_rx_stats stats;
 u16 capability;
 u8 rates[((u8)12)];
 u8 rates_len;
 u8 rates_ex[((u8)16)];
 u8 rates_ex_len;
 unsigned long last_scanned;
 u8 mode;
 u8 flags;
 u32 last_associate;
 u32 time_stamp[2];
 u16 beacon_interval;
 u16 listen_interval;
 u16 atim_window;
 u8 erp_value;
 u8 wpa_ie[64];
 size_t wpa_ie_len;
 u8 rsn_ie[64];
 size_t rsn_ie_len;
 struct ieee80211_tim_parameters tim;
 struct list_head list;
};
# 197 "ieee80211_rx.c"
enum ieee80211_mfie {
 MFIE_TYPE_SSID = 0,
 MFIE_TYPE_RATES = 1,
 MFIE_TYPE_FH_SET = 2,
 MFIE_TYPE_DS_SET = 3,
 MFIE_TYPE_CF_SET = 4,
 MFIE_TYPE_TIM = 5,
 MFIE_TYPE_IBSS_SET = 6,
 MFIE_TYPE_COUNTRY = 7,
 MFIE_TYPE_HOP_PARAMS = 8,
 MFIE_TYPE_HOP_TABLE = 9,
 MFIE_TYPE_REQUEST = 10,
 MFIE_TYPE_CHALLENGE = 16,
 MFIE_TYPE_POWER_CONSTRAINT = 32,
 MFIE_TYPE_POWER_CAPABILITY = 33,
 MFIE_TYPE_TPC_REQUEST = 34,
 MFIE_TYPE_TPC_REPORT = 35,
 MFIE_TYPE_SUPP_CHANNELS = 36,
 MFIE_TYPE_CSA = 37,
 MFIE_TYPE_MEASURE_REQUEST = 38,
 MFIE_TYPE_MEASURE_REPORT = 39,
 MFIE_TYPE_QUIET = 40,
 MFIE_TYPE_IBSS_DFS = 41,
 MFIE_TYPE_ERP_INFO = 42,
 MFIE_TYPE_RSN = 48,
 MFIE_TYPE_RATES_EX = 50,
 MFIE_TYPE_GENERIC = 221,
 MFIE_TYPE_QOS_PARAMETER = 222,
};





static int ieee80211_parse_info_param(struct ieee80211_info_element
          *info_element, u16 length,
          struct ieee80211_network *network)
{
 u8 i;
 char rates_str[64];
 char *p;

 while (length >= sizeof(*info_element)) {
  if (sizeof(*info_element) + info_element->len > length) {
   IEEE80211_DEBUG((1<<4), "Info elem: parse failed: " "info_element->len + 2 > left : " "info_element->len+2=%zd left=%d, id=%d.\n", info_element->len + sizeof(*info_element), length, info_element->id);





   return 1;
  }

  switch (info_element->id) {
  case MFIE_TYPE_SSID:
   if (ieee80211_is_empty_essid(info_element->data,
           info_element->len)) {
    network->flags |= (1<<0);
    break;
   }

   network->ssid_len = min(info_element->len,
      (u8) 32);
   memcpy(network->ssid, info_element->data,
          network->ssid_len);
   if (network->ssid_len < 32)
    memset(network->ssid + network->ssid_len, 0,
           32 - network->ssid_len);

   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_SSID: '%s' len=%d.\n", network->ssid, network->ssid_len);

   break;

  case MFIE_TYPE_RATES:
   p = rates_str;
   network->rates_len = min(info_element->len,
       ((u8)12));
   for (i = 0; i < network->rates_len; i++) {
    network->rates[i] = info_element->data[i];
    p += snprintf(p, sizeof(rates_str) -
           (p - rates_str), "%02X ",
           network->rates[i]);
    if (ieee80211_is_ofdm_rate
        (info_element->data[i])) {
     network->flags |= (1<<1);
     if (info_element->data[i] &
         0x80)
      network->flags &=
          ~(1<<2);
    }
   }

   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_RATES: '%s' (%d)\n", rates_str, network->rates_len);

   break;

  case MFIE_TYPE_RATES_EX:
   p = rates_str;
   network->rates_ex_len = min(info_element->len,
          ((u8)16));
   for (i = 0; i < network->rates_ex_len; i++) {
    network->rates_ex[i] = info_element->data[i];
    p += snprintf(p, sizeof(rates_str) -
           (p - rates_str), "%02X ",
           network->rates[i]);
    if (ieee80211_is_ofdm_rate
        (info_element->data[i])) {
     network->flags |= (1<<1);
     if (info_element->data[i] &
         0x80)
      network->flags &=
          ~(1<<2);
    }
   }

   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_RATES_EX: '%s' (%d)\n", rates_str, network->rates_ex_len);

   break;

  case MFIE_TYPE_DS_SET:
   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_DS_SET: %d\n", info_element->data[0]);

   network->channel = info_element->data[0];
   break;

  case MFIE_TYPE_FH_SET:
   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_FH_SET: ignored\n");
   break;

  case MFIE_TYPE_CF_SET:
   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_CF_SET: ignored\n");
   break;

  case MFIE_TYPE_TIM:
   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_TIM: ignored\n");
   break;

  case MFIE_TYPE_ERP_INFO:
   network->erp_value = info_element->data[0];
   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_ERP_SET: %d\n", network->erp_value);

   break;

  case MFIE_TYPE_IBSS_SET:
   network->atim_window = info_element->data[0];
   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_IBSS_SET: %d\n", network->atim_window);

   break;

  case MFIE_TYPE_CHALLENGE:
   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_CHALLENGE: ignored\n");
   break;

  case MFIE_TYPE_GENERIC:
   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_GENERIC: %d bytes\n", info_element->len);

   if (!ieee80211_parse_qos_info_param_IE(info_element,
              network))
    break;

   if (info_element->len >= 4 &&
       info_element->data[0] == 0x00 &&
       info_element->data[1] == 0x50 &&
       info_element->data[2] == 0xf2 &&
       info_element->data[3] == 0x01) {
    network->wpa_ie_len = min(info_element->len + 2,
         64);
    memcpy(network->wpa_ie, info_element,
           network->wpa_ie_len);
   }
   break;

  case MFIE_TYPE_RSN:
   IEEE80211_DEBUG((1<<4), "MFIE_TYPE_RSN: %d bytes\n", info_element->len);

   network->rsn_ie_len = min(info_element->len + 2,
        64);
   memcpy(network->rsn_ie, info_element,
          network->rsn_ie_len);
   break;

  case MFIE_TYPE_QOS_PARAMETER:



   break;

  default:
   IEEE80211_DEBUG((1<<4), "unsupported IE %d\n", info_element->id);

   break;
  }

  length -= sizeof(*info_element) + info_element->len;

  info_element =
      (struct ieee80211_info_element *)&info_element->
          data[info_element->len];
 }

 return 0;
}


int main (int argc, char *argv[]) {

  int i;

  i = ieee80211_parse_info_param((struct ieee80211_info_element *) 0, 13,
                                 (struct ieee80211_network *)0);
  return 0;
}
