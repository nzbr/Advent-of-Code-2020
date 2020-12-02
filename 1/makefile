.PHONY: run
run: main.elf
	./main.elf

main.elf: $(patsubst %.hs,%.o,$(wildcard *.hs))
	ghc -o $@ $<

%.o : %.hs
	ghc -c $<

clean:
	find \( -name "*.o" -or -name "*.hi" \) -printf '%p\n' -delete
