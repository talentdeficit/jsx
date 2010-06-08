compile:
	./priv/jsx compile
    
test: compile
	./priv/jsx test

examples: force
	./priv/jsx examples    

clean:
	./priv/jsx clean
	
install: clean compile
	./priv/jsx install
	
force: