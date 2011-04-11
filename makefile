SRC = src
OBJ = obj
HC = ghc
BIN = bin
OUT = $(BIN)/hmo
OPTS = -XDeriveDataTypeable -XScopedTypeVariables -package hslua
SRCS = $(wildcard $(SRC)/*.hs)

$(OUT): $(BIN) $(OBJ) $(SRCS) 
	$(HC) $(OPTS) --make -i$(SRC) -hidir $(OBJ) -odir $(OBJ) -o $(OUT) $(SRCS)

run: $(OUT)
	cp config.lua $(BIN)
	$(OUT)

$(BIN):
	mkdir $(BIN)

$(OBJ): 
	mkdir $(OBJ)

clean:
	rm obj/*.o
	rm bin/*
