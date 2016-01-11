


NORET_TYPE void panic(const char * fmt, ...)
{
	long i;
	static char buf[1024];
	va_list args;

	/*
	 * It's possible to come here directly from a panic-assertion and not
	 * have preempt disabled. Some functions called from here want
	 * preempt to be disabled. No point enabling it later though...
	 */

    /*
	preempt_disable();

	bust_spinlocks(1);
	va_start(args, fmt);
	vsnprintf(buf, sizeof(buf), fmt, args);
	va_end(args);
	printk(KERN_EMERG "Kernel panic - not syncing: %s\n",buf);
	bust_spinlocks(0);

    */

	/*
	 * If we have crashed and we have a crash kernel loaded let it handle
	 * everything else.
	 * Do we want to call this before we try to display a message?
	 */

    /*
	crash_kexec(NULL);
    */

#ifdef CONFIG_SMP
	/*
	 * Note smp_send_stop is the usual smp shutdown function, which
	 * unfortunately means it may not be hardened to work in a panic
	 * situation.
	 */

    /*
	smp_send_stop();
    */
#endif

    /*
	notifier_call_chain(&panic_notifier_list, 0, buf);

	if (!panic_blink)
		panic_blink = no_blink;

	if (panic_timeout > 0) {
		printk(KERN_EMERG "Rebooting in %d seconds..",panic_timeout);
		for (i = 0; i < panic_timeout*1000; ) {
			touch_nmi_watchdog();
			i += panic_blink(i);
			mdelay(1);
			i++;
		}
		emergency_restart();
	}
	local_irq_enable();
	for (i = 0;;) {
		i += panic_blink(i);
		mdelay(1);
		i++;
	}
    */
}
