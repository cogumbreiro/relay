#ifndef __ARCH_I386_ATOMIC__
#define __ARCH_I386_ATOMIC__

///

typedef struct { volatile int counter; } atomic_t;
typedef atomic_t mm_counter_t;

#define LOCK "lock ; " //ifdef CONFIG_SMP


#define ATOMIC_INIT(i)  { (i) 


#define atomic_read(v)                ((v)->counter)

#define atomic_set(v,i)               (((v)->counter) = (i))

static __inline__ void atomic_add(int i, atomic_t *v)
{
        __asm__ __volatile__(
                LOCK "addl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}



static __inline__ void atomic_inc(atomic_t *v)
{
        __asm__ __volatile__(
                LOCK "incl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}


static __inline__ void atomic_dec(atomic_t *v)
{
        __asm__ __volatile__(
                LOCK "decl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}


#endif
