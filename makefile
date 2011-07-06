SRC = src
OBJ = obj
HC = ghc
BIN = bin
OUT = $(BIN)/hmo
OPTS = -threaded
SRCS = $(wildcard $(SRC)/*.hs)
SBOOT = $(wildcard $(SRC)/*.hs-boot)

OBJS = $(patsubst src/%.hs, obj/%.o, $(SRCS))

#$(OUT): $(BIN) $(OBJ) $(SRCS)
#	$(HC) $(OPTS) --make -i$(SRC) -hidir $(OBJ) -odir $(OBJ) -o $(OUT) $(SBOOT) $(SRCS)

default : $(OUT)
	@:
	
$(OUT): $(OBJS) $(BIN)
	$(HC) --make $(OPTS) -o $(OUT) \
-package base          \
-package base64-string \
-package bimap         \
-package binary        \
-package binary-strict \
-package bytestring    \
-package containers    \
-package ghc-binary    \
-package haskell98     \
-package hslogger      \
-package hslua         \
-package HUnit         \
-package MaybeT        \
-package MissingH      \
-package mtl           \
-package network       \
-package parsec        \
-package stm           \
-package stringsearch  \
-package unix          \
-package utf8-string $(OBJS)

obj/%.hi : obj/%.o
	@:

obj/%.o : src/%.hs
	$(HC) $(OPTS) -c $< -i$(OBJ) -hidir $(OBJ) -odir $(OBJ)

obj/%.o-boot obj/%.hi-boot : src/%.hs-boot $(OBJ)
	$(HC) $(OPTS) -c $< -i$(OBJ) -hidir $(OBJ) -odir $(OBJ)

depends: 
	$(HC) $(OPTS) -i$(SRC) -odir $(OBJ) -hidir $(OBJ) -M $(SRCS) 

run: $(OUT)
	cp *.lua $(BIN)
	$(OUT)

$(BIN):
	mkdir $(BIN)

$(OBJ): 
	mkdir $(OBJ)

clean:
	rm -r $(OBJ)
	rm -r $(BIN)

# DO NOT DELETE: Beginning of Haskell dependencies
obj/WorkerTypes.o : src/WorkerTypes.hs
obj/WorkerPool.o : src/WorkerPool.hs
obj/WorkerPool.o : obj/WorkerTypes.hi
obj/Service.o : src/Service.hs
obj/Service.o : obj/WorkerTypes.hi
obj/Parsec.o : src/Parsec.hs
obj/Signals.o : src/Signals.hs
obj/Multimap.o : src/Multimap.hs
obj/Flags.o : src/Flags.hs
obj/FileSystem.o : src/FileSystem.hs
obj/Logger.o : src/Logger.hs
obj/ThreadManager.o : src/ThreadManager.hs
obj/ThreadManager.o : obj/Logger.hi
obj/ThreadManager.o : obj/WorkerTypes.hi
obj/TcpListener.o : src/TcpListener.hs
obj/TcpListener.o : obj/ThreadManager.hi
obj/TcpListener.o : obj/Logger.hi
obj/LuaUtils.o : src/LuaUtils.hs
obj/Config.o : src/Config.hs
obj/Config.o : obj/Logger.hi
obj/Config.o : obj/LuaUtils.hi
obj/CaseInsensitiveString.o : src/CaseInsensitiveString.hs
obj/Headers.o : src/Headers.hs
obj/Headers.o : obj/CaseInsensitiveString.hi
obj/Headers.o : obj/Multimap.hi
obj/Rtsp.o : src/Rtsp.hs
obj/Rtsp.o : obj/Parsec.hi
obj/Rtsp.o : obj/Headers.hi
obj/Rtsp.o : obj/Multimap.hi
obj/CommonTypes.o : src/CommonTypes.hs
obj/ScriptExecutor.o : src/ScriptExecutor.hs
obj/ScriptExecutor.o : obj/Service.hi
obj/ScriptExecutor.o : obj/Flags.hi
obj/ScriptExecutor.o : obj/LuaUtils.hi
obj/ScriptExecutor.o : obj/Logger.hi
obj/ScriptExecutor.o : obj/CommonTypes.hi
obj/Sdp.o : src/Sdp.hs
obj/Sdp.o : obj/CommonTypes.hi
obj/Sdp.o : obj/Parsec.hi
obj/Session.o : src/Session.hs
obj/Session.o : obj/Sdp.hi
obj/Session.o : obj/Service.hi
obj/Session.o : obj/CommonTypes.hi
obj/SessionManager.o : src/SessionManager.hs
obj/SessionManager.o : obj/ScriptExecutor.hi
obj/SessionManager.o : obj/Sdp.hi
obj/SessionManager.o : obj/Session.hi
obj/SessionManager.o : obj/Logger.hi
obj/SessionManager.o : obj/FileSystem.hi
obj/SessionManager.o : obj/CommonTypes.hi
obj/Authentication.o : src/Authentication.hs
obj/Authentication.o : obj/CommonTypes.hi
obj/RtspConnection.o : src/RtspConnection.hs
obj/RtspConnection.o : obj/Service.hi
obj/RtspConnection.o : obj/ScriptExecutor.hi
obj/RtspConnection.o : obj/Logger.hi
obj/RtspConnection.o : obj/Headers.hi
obj/RtspConnection.o : obj/Rtsp.hi
obj/RtspConnection.o : obj/CommonTypes.hi
obj/RtspConnection.o : obj/Authentication.hi
obj/Main.o : src/Main.hs
obj/Main.o : obj/Logger.hi
obj/Main.o : obj/Signals.hi
obj/Main.o : obj/SessionManager.hi
obj/Main.o : obj/RtspConnection.hi
obj/Main.o : obj/TcpListener.hi
obj/Main.o : obj/ScriptExecutor.hi
obj/Main.o : obj/Config.hi
obj/UnitTests.o : src/UnitTests.hs
obj/UnitTests.o : obj/SessionManager.hi
obj/UnitTests.o : obj/Sdp.hi
obj/UnitTests.o : obj/RtspConnection.hi
obj/UnitTests.o : obj/Rtsp.hi
obj/UnitTests.o : obj/Multimap.hi
obj/UnitTests.o : obj/LuaUtils.hi
obj/UnitTests.o : obj/Headers.hi
obj/UnitTests.o : obj/CaseInsensitiveString.hi
# DO NOT DELETE: End of Haskell dependencies
