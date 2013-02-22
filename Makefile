
PREFIX=/usr
PROG=slacker

all: $(PROG)

$(PROG): $(PROG).rkt
	raco exe $(PROG).rkt

.PHONY: clean
clean:
	rm -f $(PROG)

install: $(PROG)
	cp $(PROG) $(PREFIX)/bin

