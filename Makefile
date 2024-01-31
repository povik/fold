YOSYS := $(YOSYS_PREFIX)yosys
YOSYS_CONFIG := $(YOSYS_PREFIX)yosys-config
FOLD_ROOT := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SUPPORT_ROOT = $(FOLD_ROOT)/support
PASSES_SOURCES = $(wildcard passes/*.cc) \
				 $(wildcard passes/graphic/*.cc)
PASSES_OBJS = $(PASSES_SOURCES:.cc=.o)
TARGET_PLUGIN_LIB = build/fold.so

main: $(TARGET_PLUGIN_LIB)

-include $(PASSES_OBJS:.o=.d)
%.o: %.cc
	@echo "    CXX $@"
	@# -Wno-deprecated-declarations for std::iterator usage in Yosys headers
	@$(YOSYS_CONFIG) --exec --cxx --cxxflags -O3 -g -I . -MD \
		-DCONFIG_SUPPORT_ROOT=\"$(SUPPORT_ROOT)\" -c -o $@ $< \
		-D _XOPEN_SOURCE \
		-Wno-deprecated-declarations -std=c++17

build/fold.so: $(PASSES_OBJS)
	@echo "   LINK $@"
	@$(YOSYS_CONFIG) --exec --cxx --cxxflags --ldflags -g -o $@ \
		-shared $^ --ldlibs

test: build/fold.so
	@code=0; \
	for testcase in passes/tests/*.ys tests/*.ys; \
	do \
		TEXT=`head -n 1 $${testcase} | sed 's/^#\ //'`; \
		echo -n "$${TEXT}... "; \
		if ! $(YOSYS) -m $(TARGET_PLUGIN_LIB) -m fold.logic.frontend.py -s $${testcase} 1>/dev/null 2>&1; then \
			echo -e "\e[31mFAIL\e[0m"; \
			echo -e "Testcase \e[1m$${testcase}\e[0m failed"; \
			$(YOSYS) -g -Q -m $(TARGET_PLUGIN_LIB) -m fold.logic.frontend.py -p "script' $${testcase}"; \
			echo; \
			code=1; \
		else \
			echo -e "\e[32mOK\e[0m"; \
		fi \
	done; \
	echo "Target: machine code"; \
	for testcase in tests/programs/*.fold; \
	do \
		echo -n "$${testcase}... "; \
		if ! python3 -m fold.machinecode --jit-exec $${testcase} 1>/dev/null 2>&1; then \
			echo -e "\e[31mFAIL\e[0m"; \
			echo -e "Testcase \e[1m$${testcase}\e[0m failed"; \
			python3 -m fold.machinecode --jit-exec $${testcase}; \
			code=1; \
		else \
			echo -e "\e[32mOK\e[0m"; \
		fi \
	done; \
	echo "Target: logic"; \
	for testcase in tests/programs/*.fold tests/programs/*.fold.disabled_mcode; \
	do \
		echo -n "$${testcase}... "; \
		if ! $(YOSYS) -m $(TARGET_PLUGIN_LIB) -m fold.logic.frontend.py -p "read_fold $${testcase}; fold_synth; read_verilog -sv support/mutex_assert.sv; hierarchy -top top; proc; memory_nordff; sim -n 100 -assert -assert-cover -clock clk -reset rst" 1>/dev/null 2>&1; then \
			echo -e "\e[31mFAIL\e[0m"; \
			echo -e "Testcase \e[1m$${testcase}\e[0m failed"; \
			$(YOSYS) -m $(TARGET_PLUGIN_LIB) -m fold.logic.frontend.py -p "read_fold $${testcase}; fold_synth; read_verilog -sv support/mutex_assert.sv; hierarchy -top top; proc; memory_nordff; sim -n 100 -assert -assert-cover -clock clk -reset rst"; \
			echo; \
			code=1; \
		else \
			echo -e "\e[32mOK\e[0m"; \
		fi \
	done; \
	echo "Target: frontend constant evaluator"; \
	for testcase in tests/programs/ops*.fold; \
	do \
		echo -n "$${testcase}... "; \
		if ! python3 -m fold.eval $${testcase} 1>/dev/null 2>&1; then \
			echo -e "\e[31mFAIL\e[0m"; \
			echo -e "Testcase \e[1m$${testcase}\e[0m failed"; \
			python3 -m fold.eval $${testcase}; \
			code=1; \
		else \
			echo -e "\e[32mOK\e[0m"; \
		fi \
	done; \
	echo "Import feasibility test"; \
	for testcase in tests/import_feasibility/*.fold; \
	do \
		echo -n "$${testcase}... "; \
		if ! $(YOSYS) -m $(TARGET_PLUGIN_LIB) -m fold.logic.frontend.py -p "read_fold $${testcase}; fold_synth;" 1>/dev/null 2>&1; then \
			echo -e "\e[31mFAIL\e[0m"; \
			echo -e "Testcase \e[1m$${testcase}\e[0m failed"; \
			$(YOSYS) -m $(TARGET_PLUGIN_LIB) -m fold.logic.frontend.py -p "read_fold $${testcase}; fold_synth;"; \
			code=1; \
		else \
			echo -e "\e[32mOK\e[0m"; \
		fi \
	done; \
	exit $${code}


COVERAGE_ARGS = --rcfile=coverage.rc --branch --append --data-file build/python.coverage
frontend_coverage:
	rm build/python.coverage
	@for testcase in tests/programs/*.fold; \
	do \
		echo "$${testcase}..."; \
		coverage run $(COVERAGE_ARGS) -m fold.machinecode $${testcase}; \
		coverage run $(COVERAGE_ARGS) -m fold.logic \
			-o /dev/null --top top $${testcase} > /dev/null; \
	done
	coverage run $(COVERAGE_ARGS) -m fold.eval tests/selftest_programs/ops1.fold; \
	coverage report --rcfile=coverage.rc --data-file build/python.coverage --include "fold/*"
	coverage html --rcfile=coverage.rc --directory build/coverage/ --data-file build/python.coverage --include "fold/*"

clean:
	@echo "  CLEAN"
	@rm -f $(PASSES_OBJS) $(TARGET_PLUGIN_LIB)
