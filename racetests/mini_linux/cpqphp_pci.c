#include "types.h"
#include "semaphore.h"


struct controller {
  	struct controller *next;
  //	u32 ctrl_int_comp;
    struct semaphore crit_sect;	/* critical section semaphore */
  //	void __iomem *hpc_reg;		/* cookie for our pci controller location */
  //	struct pci_resource *mem_head;
  //	struct pci_resource *p_mem_head;
  //	struct pci_resource *io_head;
  //	struct pci_resource *bus_head;
  //	struct pci_dev *pci_dev;
  //	struct pci_bus *pci_bus;
  //	struct event_info event_queue[10];
  //	struct slot *slot;
	u8 next_event;
	u8 interrupt;
	u8 cfgspc_irq;
	u8 bus;				/* bus number for the pci hotplug controller */
	u8 rev;
	u8 slot_device_offset;
	u8 first_slot;
	u8 add_support;
	u8 push_flag;
  //	enum pci_bus_speed speed;
  //	enum pci_bus_speed speed_capability;
	u8 push_button;			/* 0 = no pushbutton, 1 = pushbutton present */
	u8 slot_switch_type;	/* 0 = no switch, 1 = switch present */
	u8 defeature_PHP;		/* 0 = PHP not supported, 1 = PHP supported */
	u8 alternate_base_address;	/* 0 = not supported, 1 = supported */
	u8 pci_config_space;		/* Index/data access to working registers 0 = not supported, 1 = supported */
	u8 pcix_speed_capability;	/* PCI-X */
	u8 pcix_support;		/* PCI-X */
	u16 vendor_id;
  //	struct work_struct int_task_event;
  //	wait_queue_head_t queue;	/* sleep & wake process */
};

struct pci_func {
  	struct pci_func *next;
	u8 bus;
	u8 device;
	u8 function;
	u8 is_a_board;
	u16 status;
	u8 configured;
	u8 switch_save;
	u8 presence_save;
	u32 base_length[0x06];
	u8 base_type[0x06];
	u16 reserved2;
	u32 config_space[0x20];
  //	struct pci_resource *mem_head;
  //	struct pci_resource *p_mem_head;
  //	struct pci_resource *io_head;
  //	struct pci_resource *bus_head;
  u32 bus_head;
  //	struct timer_list *p_task_event;
  //	struct pci_dev* pci_dev;
};



static struct semaphore event_semaphore;	
/* mutex for process loop (up if something to process) */
static struct semaphore event_exit;		
/* guard ensure thread has exited before calling it quits */

static int event_finished;
static unsigned long pushbutton_pending;	/* = 0 */

/* things needed for the long_delay function */
static struct semaphore		delay_sem;

struct controller *cpqhp_ctrl_list;	/* = NULL */

struct pci_func *cpqhp_slot_list[256];


/* this is the main worker thread */
int event_thread(void* data)
{
	struct controller *ctrl;

	while (1) {

      down_interruptible (&event_semaphore);

      if (event_finished) break;
      /* Do stuff here */
      if (pushbutton_pending)
        cpqhp_slot_list[0]->bus_head = 0;//cpqhp_pushbutton_thread(pushbutton_pending); // HERE
      else
        for (ctrl = cpqhp_ctrl_list; ctrl; ctrl=ctrl->next)
          ;//interrupt_event_handler(ctrl);
       
	}

	up(&event_exit);
	return 0;
}


// cpqhp_pushbutton_thread >>> cpqhp_process_SS >>> remove_board >>> cpqhp_save_used_resources

