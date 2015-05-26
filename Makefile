CM_VERBOSE := false
export CM_VERBOSE

files = $(wildcard ch*.sml)
targets = $(patsubst %.sml,%,$(files))

test:
	@time sml $(files)

.PHONY: $(targets) all

$(targets):
	@time sml $@.sml

all: $(targets)
