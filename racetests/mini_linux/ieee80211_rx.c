#include <string.h>
#include <stdio.h>
#include "types.h"
#include "list.h"



/*
 *	IEEE 802.3 Ethernet magic constants.  The frame sizes omit the preamble
 *	and FCS/CRC (frame check sequence). 
 */

#define ETH_ALEN	6		/* Octets in one ethernet addr	 */
#define ETH_HLEN	14		/* Total octets in header.	 */
#define ETH_ZLEN	60		/* Min. octets in frame sans FCS */
#define ETH_DATA_LEN	1500		/* Max. octets in payload	 */
#define ETH_FRAME_LEN	1514		/* Max. octets in frame sans FCS */


/* Maximum size of the ESSID and NICKN strings */
#define IW_ESSID_MAX_SIZE	32



/////////////////////////////////////
// from include/net/ieee80211.h

/* MAX_RATES_LENGTH needs to be 12.  The spec says 8, and many APs
 * only use 8, and then use extended rates for the remaining supported
 * rates.  Other APs, however, stick all of their supported rates on the
 * main rates information element... */
#define MAX_RATES_LENGTH                  ((u8)12)
#define MAX_RATES_EX_LENGTH               ((u8)16)
#define MAX_NETWORK_COUNT                  128

#define CRC_LENGTH                 4U

#define MAX_WPA_IE_LEN 64

#define NETWORK_EMPTY_ESSID    (1<<0)
#define NETWORK_HAS_OFDM       (1<<1)
#define NETWORK_HAS_CCK        (1<<2)


#define QOS_QUEUE_NUM                   4
#define QOS_OUI_LEN                     3
#define QOS_OUI_TYPE                    2
#define QOS_ELEMENT_ID                  221
#define QOS_OUI_INFO_SUB_TYPE           0
#define QOS_OUI_PARAM_SUB_TYPE          1
#define QOS_VERSION_1                   1
#define QOS_AIFSN_MIN_VALUE             2


struct ieee80211_info_element {
	u8 id;
	u8 len;
	u8 data[0];
} __attribute__ ((packed));

/* NOTE: This data is for statistical purposes; not all hardware provides this
 *       information for frames received.  Not setting these will not cause
 *       any adverse affects. */
struct ieee80211_rx_stats {
	u32 mac_time;
	s8 rssi;
	u8 signal;
	u8 noise;
	u16 rate;		/* in 100 kbps */
	u8 received_channel;
	u8 control;
	u8 mask;
	u8 freq;
	u16 len;
};


struct ieee80211_qos_parameters {
	__le16 cw_min[QOS_QUEUE_NUM];
	__le16 cw_max[QOS_QUEUE_NUM];
	u8 aifs[QOS_QUEUE_NUM];
	u8 flag[QOS_QUEUE_NUM];
	__le16 tx_op_limit[QOS_QUEUE_NUM];
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
	/* These entries are used to identify a unique network */
	u8 bssid[ETH_ALEN];
	u8 channel;
	/* Ensure null-terminated for any debug msgs */
	u8 ssid[IW_ESSID_MAX_SIZE + 1];
	u8 ssid_len;

	struct ieee80211_qos_data qos_data;

	/* These are network statistics */
	struct ieee80211_rx_stats stats;
	u16 capability;
	u8 rates[MAX_RATES_LENGTH];
	u8 rates_len;
	u8 rates_ex[MAX_RATES_EX_LENGTH];
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
	u8 wpa_ie[MAX_WPA_IE_LEN];
	size_t wpa_ie_len;
	u8 rsn_ie[MAX_WPA_IE_LEN];
	size_t rsn_ie_len;
	struct ieee80211_tim_parameters tim;
	struct list_head list;
};

#define IEEE80211_CCK_RATE_1MB		        0x02
#define IEEE80211_CCK_RATE_2MB		        0x04
#define IEEE80211_CCK_RATE_5MB		        0x0B
#define IEEE80211_CCK_RATE_11MB		        0x16
#define IEEE80211_OFDM_RATE_6MB		        0x0C
#define IEEE80211_OFDM_RATE_9MB		        0x12
#define IEEE80211_OFDM_RATE_12MB		0x18
#define IEEE80211_OFDM_RATE_18MB		0x24
#define IEEE80211_OFDM_RATE_24MB		0x30
#define IEEE80211_OFDM_RATE_36MB		0x48
#define IEEE80211_OFDM_RATE_48MB		0x60
#define IEEE80211_OFDM_RATE_54MB		0x6C
#define IEEE80211_BASIC_RATE_MASK		0x80


#define IEEE80211_DL_INFO          (1<<0)
#define IEEE80211_DL_WX            (1<<1)
#define IEEE80211_DL_SCAN          (1<<2)
#define IEEE80211_DL_STATE         (1<<3)
#define IEEE80211_DL_MGMT          (1<<4)
#define IEEE80211_DL_FRAG          (1<<5)
#define IEEE80211_DL_DROP          (1<<7)

#define IEEE80211_DL_TX            (1<<8)
#define IEEE80211_DL_RX            (1<<9)
#define IEEE80211_DL_QOS           (1<<31)


#define IEEE80211_ERROR(f, a...) printk(KERN_ERR "ieee80211: " f, ## a)
#define IEEE80211_WARNING(f, a...) printk(KERN_WARNING "ieee80211: " f, ## a)
#define IEEE80211_DEBUG_INFO(f, a...)   IEEE80211_DEBUG(IEEE80211_DL_INFO, f, ## a)

#define IEEE80211_DEBUG_WX(f, a...)     IEEE80211_DEBUG(IEEE80211_DL_WX, f, ## a)
#define IEEE80211_DEBUG_SCAN(f, a...)   IEEE80211_DEBUG(IEEE80211_DL_SCAN, f, ## a)
#define IEEE80211_DEBUG_STATE(f, a...)  IEEE80211_DEBUG(IEEE80211_DL_STATE, f, ## a)
#define IEEE80211_DEBUG_MGMT(f, a...)  IEEE80211_DEBUG(IEEE80211_DL_MGMT, f, ## a)
#define IEEE80211_DEBUG_FRAG(f, a...)  IEEE80211_DEBUG(IEEE80211_DL_FRAG, f, ## a)
#define IEEE80211_DEBUG_DROP(f, a...)  IEEE80211_DEBUG(IEEE80211_DL_DROP, f, ## a)
#define IEEE80211_DEBUG_TX(f, a...)  IEEE80211_DEBUG(IEEE80211_DL_TX, f, ## a)
#define IEEE80211_DEBUG_RX(f, a...)  IEEE80211_DEBUG(IEEE80211_DL_RX, f, ## a)
#define IEEE80211_DEBUG_QOS(f, a...)  IEEE80211_DEBUG(IEEE80211_DL_QOS, f, ## a)

/* Management Frame Information Element Types */
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


/////////////////////////////////////
// from /net/ieee80211/ieee80211_rx.c

int ieee80211_parse_info_param(struct ieee80211_info_element
				      *info_element, u16 length,
				      struct ieee80211_network *network)
{
	u8 i;
	char rates_str[64];
	char *p;

	while (length >= sizeof(*info_element)) {
		if (sizeof(*info_element) + info_element->len > length) {
			IEEE80211_DEBUG_MGMT("Info elem: parse failed: "
					     "info_element->len + 2 > left : "
					     "info_element->len+2=%zd left=%d, id=%d.\n",
					     info_element->len +
					     sizeof(*info_element),
					     length, info_element->id);
			return 1;
		}

		switch (info_element->id) {
		case MFIE_TYPE_SSID:
			if (ieee80211_is_empty_essid(info_element->data,
						     info_element->len)) {
				network->flags |= NETWORK_EMPTY_ESSID;
				break;
			}

			network->ssid_len = min(info_element->len,
						(u8) IW_ESSID_MAX_SIZE);
			memcpy(network->ssid, info_element->data,
			       network->ssid_len);
			if (network->ssid_len < IW_ESSID_MAX_SIZE)
				memset(network->ssid + network->ssid_len, 0,
				       IW_ESSID_MAX_SIZE - network->ssid_len);

			IEEE80211_DEBUG_MGMT("MFIE_TYPE_SSID: '%s' len=%d.\n",
					     network->ssid, network->ssid_len);
			break;

		case MFIE_TYPE_RATES:
			p = rates_str;
			network->rates_len = min(info_element->len,
						 MAX_RATES_LENGTH);
			for (i = 0; i < network->rates_len; i++) {
				network->rates[i] = info_element->data[i];
				p += snprintf(p, sizeof(rates_str) -
					      (p - rates_str), "%02X ",
					      network->rates[i]);
				if (ieee80211_is_ofdm_rate
				    (info_element->data[i])) {
					network->flags |= NETWORK_HAS_OFDM;
					if (info_element->data[i] &
					    IEEE80211_BASIC_RATE_MASK)
						network->flags &=
						    ~NETWORK_HAS_CCK;
				}
			}

			IEEE80211_DEBUG_MGMT("MFIE_TYPE_RATES: '%s' (%d)\n",
					     rates_str, network->rates_len);
			break;

		case MFIE_TYPE_RATES_EX:
			p = rates_str;
			network->rates_ex_len = min(info_element->len,
						    MAX_RATES_EX_LENGTH);
			for (i = 0; i < network->rates_ex_len; i++) {
				network->rates_ex[i] = info_element->data[i];
				p += snprintf(p, sizeof(rates_str) -
					      (p - rates_str), "%02X ",
					      network->rates[i]);
				if (ieee80211_is_ofdm_rate
				    (info_element->data[i])) {
					network->flags |= NETWORK_HAS_OFDM;
					if (info_element->data[i] &
					    IEEE80211_BASIC_RATE_MASK)
						network->flags &=
						    ~NETWORK_HAS_CCK;
				}
			}

			IEEE80211_DEBUG_MGMT("MFIE_TYPE_RATES_EX: '%s' (%d)\n",
					     rates_str, network->rates_ex_len);
			break;

		case MFIE_TYPE_DS_SET:
			IEEE80211_DEBUG_MGMT("MFIE_TYPE_DS_SET: %d\n",
					     info_element->data[0]);
			network->channel = info_element->data[0];
			break;

		case MFIE_TYPE_FH_SET:
			IEEE80211_DEBUG_MGMT("MFIE_TYPE_FH_SET: ignored\n");
			break;

		case MFIE_TYPE_CF_SET:
			IEEE80211_DEBUG_MGMT("MFIE_TYPE_CF_SET: ignored\n");
			break;

		case MFIE_TYPE_TIM:
			IEEE80211_DEBUG_MGMT("MFIE_TYPE_TIM: ignored\n");
			break;

		case MFIE_TYPE_ERP_INFO:
			network->erp_value = info_element->data[0];
			IEEE80211_DEBUG_MGMT("MFIE_TYPE_ERP_SET: %d\n",
					     network->erp_value);
			break;

		case MFIE_TYPE_IBSS_SET:
			network->atim_window = info_element->data[0];
			IEEE80211_DEBUG_MGMT("MFIE_TYPE_IBSS_SET: %d\n",
					     network->atim_window);
			break;

		case MFIE_TYPE_CHALLENGE:
			IEEE80211_DEBUG_MGMT("MFIE_TYPE_CHALLENGE: ignored\n");
			break;

		case MFIE_TYPE_GENERIC:
			IEEE80211_DEBUG_MGMT("MFIE_TYPE_GENERIC: %d bytes\n",
					     info_element->len);
			if (!ieee80211_parse_qos_info_param_IE(info_element,
							       network))
				break;

			if (info_element->len >= 4 &&
			    info_element->data[0] == 0x00 &&
			    info_element->data[1] == 0x50 &&
			    info_element->data[2] == 0xf2 &&
			    info_element->data[3] == 0x01) {
				network->wpa_ie_len = min(info_element->len + 2,
							  MAX_WPA_IE_LEN);
				memcpy(network->wpa_ie, info_element,
				       network->wpa_ie_len);
			}
			break;

		case MFIE_TYPE_RSN:
			IEEE80211_DEBUG_MGMT("MFIE_TYPE_RSN: %d bytes\n",
					     info_element->len);
			network->rsn_ie_len = min(info_element->len + 2,
						  MAX_WPA_IE_LEN);
			memcpy(network->rsn_ie, info_element,
			       network->rsn_ie_len);
			break;

		case MFIE_TYPE_QOS_PARAMETER:
          /*			printk(KERN_ERR
			       "QoS Error need to parse QOS_PARAMETER IE\n");
          */
			break;

		default:
			IEEE80211_DEBUG_MGMT("unsupported IE %d\n",
					     info_element->id);
			break;
		}

		length -= sizeof(*info_element) + info_element->len;
        //current handling of this instr leads to non-termination 
		info_element =
		    (struct ieee80211_info_element *)&info_element->
          data[info_element->len]; // supposed to make it go to the next element in the array (if info_element was packed as an array)... by purposely going out of the array bounds
	}

	return 0;
}

