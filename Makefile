CM_VERBOSE := false
export CM_VERBOSE

files = $(wildcard ch*.sml)
targets = $(patsubst %.sml,%,$(files))

test:
	@time sml $(files) exit.sml

.PHONY: $(targets) all

$(targets):
	@time sml $@.sml exit.sml

all: $(targets)
