// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/register.h"
#include "kernel/rtlil.h"

USING_YOSYS_NAMESPACE

struct FoldSynthPass : Pass {
	FoldSynthPass() : Pass("fold_synth", "synthesize a Fold circuit") {}

	bool did_something(Design *d, bool &first)
	{
		bool ret;

		if (first) {
			first = false;
			ret = true;
		} else {
			ret = d->scratchpad_get_bool("seer.did_something", false)
				|| d->scratchpad_get_bool("opt.did_something", false);
		}

		d->scratchpad_unset("seer.did_something");
		d->scratchpad_unset("opt.did_something");

		return ret;
	}

	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing FOLD_SYNTH pass. (Fold synthesize)\n");

		size_t argidx;
		std::string initplan_args;
		std::string debug;
		bool no_lower = false;
		bool no_resolve = false;
		bool no_timetravel = false;
		bool no_initplan = false;
		bool no_imports = false;
		bool resolve2 = false;
		bool new_ = false;
		bool execid = false;

		if (getenv("FOLD_SYNTH_ARGS")) {
			std::istringstream is(getenv("FOLD_SYNTH_ARGS"));
			std::string arg;
			while (getline(is, arg, ' '))
				args.push_back(arg);
		}

		for (argidx = 1; argidx < args.size(); argidx++) {
			if (args[argidx] == "-select_offenders" || args[argidx] == "-offenders")
				initplan_args += " -select_offenders";
			else if (args[argidx] == "-no_lower")
				no_lower = true;
			else if (args[argidx] == "-no_timetravel")
				no_timetravel = true;
			else if (args[argidx] == "-no_resolve")
				no_resolve = true;
			else if (args[argidx] == "-no_initplan")
				no_initplan = true;
			else if (args[argidx] == "-resolve2")
				resolve2 = true;
			else if (args[argidx] == "-new")
				new_ = true;
			else if (args[argidx] == "-execid")
				execid = true;
			else if (args[argidx] == "-no_imports")
				no_imports = true;
			else if (args[argidx] == "-debug" && argidx + 1 < args.size())
				debug = args[++argidx];
			else
				break;
		}
		(void) new_; // maybe unused
		extra_args(args, argidx, d);
		log_push();

		if (!debug.empty())
			Pass::call(d, stringf("write_rtlil %s/01_frontend.il", debug.c_str()));

		log_header(d, "Performing pre-imports synthesis.\n");
		log_push();

		Pass::call(d, "immutlinks -writeback");

		Pass::call(d, "read_verilog -sv " CONFIG_SUPPORT_ROOT "/lib.v");

		Pass::call(d, "imselect");
		Pass::call(d, "setattr -set keep 1 @immutlinks_ctl");
		Pass::call(d, "setattr -set keep 1 t:VAR_SET");
		Pass::call(d, "connbgval");

		if (!execid)
			Pass::call(d, "delete a:execid_assert");

		bool first = true;
		while (did_something(d, first)) {
			Pass::call(d, "portal_const");
			Pass::call(d, "opt_expr");
			Pass::call(d, "opt_clean");
		}

		Pass::call(d, "opt_merge -share_all");

		Pass::call(d, "immutvars_clean");
		Pass::call(d, "prune_mutexasserts");
		Pass::call(d, "delete t:MUTEX_ASSERT r:WIDTH<=1 %i");

		Pass::call(d, "delete w:crank %ci* c:* %i");
		Pass::call(d, "setattr -set seer.background_value 1'b0 w:crank");
		Pass::call(d, "expose -input top/w:crank");
		Pass::call(d, "connbgval");
		Pass::call(d, "opt_clean");
		Pass::call(d, "opt_merge -share_all");

		Pass::call(d, "setattr -unset keep t:VAR_SET");

		log_pop();

		if (!debug.empty())
			Pass::call(d, stringf("write_rtlil %s/02_preimports.il", debug.c_str()));

		if (no_imports) {
			log_pop();
			return;
		}

		log_header(d, "Synthesizing imports.\n");
		log_push();

		Pass::call(d, "immutvars_clean");
		Pass::call(d, "immutvars");
		Pass::call(d, "immutvars_clean");
		Pass::call(d, "imports");
		Pass::call(d, "setattr -unset keep @immutlinks_ctl");
		Pass::call(d, "scratchpad -unset immutlinks");
		Pass::call(d, "connbgval");
		Pass::call(d, "opt_clean");

		log_pop();

		if (!debug.empty())
			Pass::call(d, stringf("write_rtlil %s/03_imports.il", debug.c_str()));

		log_header(d, "Synthesizing background.\n");
		log_push();

		//Pass::call(d, "check -allow-loops -assert");
		Pass::call(d, "opt_merge -share_all");
		Pass::call(d, "connbgval");
		Pass::call(d, "opt_clean");

		Pass::call(d, "delete -input top/w:crank");
		// TODO: what to do with -assert_consistence?
		Pass::call(d, "seer_spreadbg -writeback -assert_consistence");
		Pass::call(d, "expose -input top/w:crank");
		Pass::call(d, "seer_reachability -connect_const");

		log_pop();

		//Pass::call(d, "check -assert -allow-loops");
		Pass::call(d, "connbgval");
		Pass::call(d, "opt_clean");

		if (!debug.empty())
			Pass::call(d, stringf("write_rtlil %s/04_background.il", debug.c_str()));

		first = true;
		while (did_something(d, first)) {
			Pass::call(d, "prop_constmuxes");
			Pass::call(d, "portal_const");
			Pass::call(d, "opt_expr");
			Pass::call(d, "portal_const");
			Pass::call(d, "opt_muxtree");
			Pass::call(d, "connbgval");
			Pass::call(d, "opt_clean");
			Pass::call(d, "prune_mutexasserts");
			Pass::call(d, "delete t:MUTEX_ASSERT r:WIDTH<=1 %i");
			Pass::call(d, "static_assert -any -fully_disabled -always_passing -remove");
		}

		Pass::call(d, "seer_spreadbg -writeback");

		if (no_initplan) {
			log_pop();
			return;
		}

		if (!debug.empty())
			Pass::call(d, stringf("write_rtlil %s/05_pre_initplan.il", debug.c_str()));

		d->scratchpad_unset("seer.insatiable");
		Pass::call(d, "seer_initplan -best_effort" + initplan_args + " top");
		bool insatiable = d->scratchpad_get_bool("seer.insatiable", false);

		if (insatiable) {
			log_pop();
			log_error("Design inherently violates data causality. See the log for details.\n");
		}

		if (no_resolve) {
			log_pop();
			return;
		}

		if (!resolve2)
			Pass::call(d, "seer_resolve -mask_undef_mux");
		else
			Pass::call(d, "seer_resolve2 -mask_undef_mux");

		if (!debug.empty())
			Pass::call(d, stringf("write_rtlil %s/06_timeplanned.il", debug.c_str()));

		if (!no_timetravel) {
			Pass::call(d, "seer_timetravel");
			Pass::call(d, "seer_spreadbg -writeback");
			Pass::call(d, "seer_merge");
			Pass::call(d, "timeportal_drop");
		}

		Pass::call(d, "check -assert");
		Pass::call(d, "backedge_lower -select_port A -bypass");
		Pass::call(d, "check -assert");
		Pass::call(d, "portal_const");
		Pass::call(d, "check -assert");
		Pass::call(d, "connbgval");
		Pass::call(d, "opt_clean");
		Pass::call(d, "check -assert");

		if (!no_timetravel)
			Pass::call(d, "seer_stat -mask_undef_mux -assert-no-magic");

		if (no_lower || no_timetravel) {
			log_pop();
			return;
		}

		// Assert there are no failing asserts the enable of which is connected
		// to the SEER tree rooted at the crank wire
		Pass::call(d, "static_assert -assert-none -always_failing w:crank %x*:+SEER %co:+$assert[EN]");

		Pass::call(d, "opt_clean");
		Pass::call(d, "opt_expr");
		Pass::call(d, "opt_expr -mux_undef t:SEER r:OFFSET>0 %i %co3");
		Pass::call(d, "opt_expr");
		Pass::call(d, "connbgval");
		Pass::call(d, "opt_clean");
		Pass::call(d, "add -input clk 1 top");
		Pass::call(d, "add -input rst 1 top");

		Pass::call(d, "patch_crank top");
		Pass::call(d, "check top");
		Pass::call(d, "seer_lower top");

		Pass::call(d, "opt_dff");
		Pass::call(d, "connbgval");
		Pass::call(d, "opt_clean");
		Pass::call(d, "opt_merge");

		Pass::call(d, "select -assert-none top/t:SEER top/t:ORACLE");
		Pass::call(d, "static_assert -assert-none -undef_enable -undef_input top");
		Pass::call(d, "check -assert top");
	}
} FoldSynthPass;
