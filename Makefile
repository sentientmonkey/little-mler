CM_VERBOSE := false
export CM_VERBOSE

files = $(wildcard ch*.sml)
targets = $(patsubst %.sml,%,$(files))

test:
	@time sml $(files) < /dev/null

.PHONY: $(targets) all

$(targets):
	@time sml $@.sml < /dev/null

all: $(targets)
