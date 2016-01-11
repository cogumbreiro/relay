#include "list.h"
#include "spinlock_types.h"





/////////////////////////////////////////////////


struct resource {
  const char *name;
  unsigned long start, end;
  unsigned long flags;
  struct resource *parent, *sibling, *child;
};



/////////////////////////////////////////////////

struct pci_bus ;

#define DEVICE_COUNT_COMPATIBLE	4
#define DEVICE_COUNT_RESOURCE	12

/*
 * The pci_dev structure is used to describe PCI devices.
 */
struct pci_dev {
	struct list_head global_list;	/* node in list of all PCI devices */
	struct list_head bus_list;	/* node in per-bus list */
	struct pci_bus	*bus;		/* bus this device is on */
	struct pci_bus	*subordinate;	/* bus this device bridges to */

	void		*sysdata;	/* hook for sys-specific extension */
  //	struct proc_dir_entry *procent;	/* device entry in /proc/bus/pci */

	unsigned int	devfn;		/* encoded device & function index */
	unsigned short	vendor;
	unsigned short	device;
	unsigned short	subsystem_vendor;
	unsigned short	subsystem_device;
	unsigned int	class;		/* 3 bytes: (base,sub,prog-if) */
	u8		hdr_type;	/* PCI header type (`multi' flag masked out) */
	u8		rom_base_reg;	/* which config register controls the ROM */

  //	struct pci_driver *driver;	/* which driver has allocated this device */
	u64		dma_mask;	/* Mask of the bits of bus address this
					   device implements.  Normally this is
					   0xffffffff.  You only need to change
					   this if your device has broken DMA
					   or supports 64-bit transfers.  */

  //	pci_power_t     current_state;  /* Current operating state. In ACPI-speak, this is D0-D3, D0 being fully functional, and D3 being off. */

  //	struct	device	dev;		/* Generic device interface */

	/* device is compatible with these IDs */
	unsigned short vendor_compatible[DEVICE_COUNT_COMPATIBLE];
	unsigned short device_compatible[DEVICE_COUNT_COMPATIBLE];

	int		cfg_size;	/* Size of configuration space */

	/*
	 * Instead of touching interrupt line and base address registers
	 * directly, use the values stored here. They might be different!
	 */
	unsigned int	irq;
	struct resource resource[DEVICE_COUNT_RESOURCE]; /* I/O and memory regions + expansion ROMs */

	/* These fields are used by common fixups */
	unsigned int	transparent:1;	/* Transparent PCI bridge */
	unsigned int	multifunction:1;/* Part of multi-function device */
	/* keep track of device state */
	unsigned int	is_enabled:1;	/* pci_enable_device has been called */
	unsigned int	is_busmaster:1; /* device is busmaster */
	unsigned int	no_msi:1;	/* device may not use msi */
	unsigned int	block_ucfg_access:1;	/* userspace config space access is blocked */

  u32		saved_config_space[16]; /* config space saved at suspend time */
  //	struct bin_attribute *rom_attr; /* attribute descriptor for sysfs ROM entry */
	int rom_attr_enabled;		/* has display of the rom attribute been enabled? */
  //	struct bin_attribute *res_attr[DEVICE_COUNT_RESOURCE]; /* sysfs file for resources */
};



/*
 *  For PCI devices, the region numbers are assigned this way:
 *
 *	0-5	standard PCI regions
 *	6	expansion ROM
 *	7-10	bridges: address space assigned to buses behind the bridge
 */

#define PCI_ROM_RESOURCE	6
#define PCI_BRIDGE_RESOURCES	7
#define PCI_NUM_RESOURCES	11

#ifndef PCI_BUS_NUM_RESOURCES
#define PCI_BUS_NUM_RESOURCES	8
#endif


struct pci_bus {
  struct list_head node;          /* node in list of buses */
  struct pci_bus  *parent;        /* parent bus this bridge is on */
  struct list_head children;      /* list of child buses */
  struct list_head devices;       /* list of devices on this bus */
  struct pci_dev  *self;          /* bridge device as seen by parent */
  struct resource *resource[PCI_BUS_NUM_RESOURCES];
                                  /* address space routed to this bus */
  //  struct pci_ops  *ops;           /* configuration access functions */
  void            *sysdata;       /* hook for sys-specific extension */
  //  struct proc_dir_entry *procdir; /* directory entry in /proc/bus/pci */

  unsigned char   number;         /* bus number */
  unsigned char   primary;        /* number of primary bridge */
  unsigned char   secondary;      /* number of secondary bridge */
  unsigned char   subordinate;    /* max number of subordinate buses */

  char            name[48];

  unsigned short  bridge_ctl;     /* manage NO_ISA/FBB/et al behaviors */
  unsigned short  pad2;
  //  struct device           *bridge;
  //  struct class_device     class_dev;
  //  struct bin_attribute    *legacy_io; /* legacy I/O for this bus */
  //  struct bin_attribute    *legacy_mem; /* legacy mem */
};


// from /kernel/resource.c

static DEFINE_RWLOCK(resource_lock);


static struct resource * __request_resource(struct resource *root, struct resource *new)
{
  unsigned long start = new->start;
  unsigned long end = new->end;
  struct resource *tmp, **p;

  if (end < start)
    return root;
  if (start < root->start)
    return root;
  if (end > root->end)
    return root;
  p = &root->child;
  for (;;) {
    tmp = *p;
    if (!tmp || tmp->start > end) {
      new->sibling = tmp;
      *p = new;
      new->parent = root;
      return NULL;
    }
    p = &tmp->sibling;
    if (tmp->end < start)
      continue;
    return tmp;
  }
}

int request_resource(struct resource *root, struct resource *new)
{
  struct resource *conflict;

  write_lock(&resource_lock);
  conflict = __request_resource(root, new);
  write_unlock(&resource_lock);
  return conflict ? -EBUSY : 0;
}

// from init.h

#define __init          __attribute__ ((__section__ (".init.text")))

// from /arch/i386/pic/i386.c

void /*__init*/ pcibios_allocate_bus_resources(struct list_head *bus_list)
{
	struct pci_bus *bus;
	struct pci_dev *dev;
	int idx;
	struct resource *r, *pr;

	/* Depth-First Search on bus tree */
	list_for_each_entry(bus, bus_list, node) {
		if ((dev = bus->self)) {
			for (idx = PCI_BRIDGE_RESOURCES; idx < PCI_NUM_RESOURCES; idx++) {
				r = &dev->resource[idx];
				if (!r->flags)
					continue;
				pr = pci_find_parent_resource(dev, r);
				if (!r->start || !pr || request_resource(pr, r) < 0) {
                  //printk(KERN_ERR "PCI: Cannot allocate resource region %d of bridge %s\n", idx, pci_name(dev));
					/* Something is wrong with the region.
					   Invalidate the resource to prevent child
					   resource allocations in this range. */
					r->flags = 0;
				}
			}
		}
		pcibios_allocate_bus_resources(&bus->children);
	}
}
