// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include <unistd.h>
#include <ucontext.h>
#include <assert.h>

class coroutine_stack {
public:
	void *base;
	size_t size;

	coroutine_stack(size_t size=4096*1024)
		: size(size)
	{
		base = aligned_alloc(sysconf(_SC_PAGESIZE), size);

		if (!base)
			throw std::bad_alloc();
	}

	~coroutine_stack()
	{
		free(base);
	}
};

class coroutine {
private:
	ucontext_t others, ours;
	void cleanup();
	coroutine_stack &stack;
protected:
	void yield();
public:
	coroutine(coroutine_stack &stack)
		: stack(stack), running(false) {};
	virtual ~coroutine();

	virtual void entry() = 0;
	volatile bool running;
	void start();
	void resume();
};

coroutine::~coroutine()
{
}

typedef coroutine * coroutine_p;
volatile coroutine_p _run_coroutine_p;

extern "C"
void _run_coroutine()
{
	coroutine *r = _run_coroutine_p;

	r->running = true;
	r->entry();
	r->running = false;
}

void coroutine::start() {
	assert(!running);
	assert(getcontext(&ours) >= 0);
	ours.uc_stack.ss_sp = stack.base;
	ours.uc_stack.ss_size = stack.size;
	ours.uc_link = &others;

	// HACK: not thread safe!
	_run_coroutine_p = this;
	makecontext(&ours, _run_coroutine, 0);
	swapcontext(&others, &ours);
}

void coroutine::yield() {
	swapcontext(&ours, &others);
}

void coroutine::resume() {
	if (!running)
		return;
	swapcontext(&others, &ours);
}
