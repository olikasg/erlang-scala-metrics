include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

EI_ROOT = $(ERL_TOP)/lib/erl_interface
EI_INCLUDE = -I$(EI_ROOT)/include -I$(ERL_TOP)/erts/emulator/beam
EI_LIB = -L$(EI_ROOT)/obj/$(TARGET) -lei

PQ_LIB = -lpq

OUR_C_FLAGS =  -g -Wall -fpic $(EI_INCLUDE)
CFLAGS += $(OUR_C_FLAGS)
CXXFLAGS += $(OUR_C_FLAGS)

TARGETS = pg_sync.beam pg_async.beam pg_sync.so pg_async.so \
next_perm.so next_perm.beam

all: $(TARGETS)

clean:
	rm -f $(TARGETS) *.o
	rm -f pg_async2.so pg_encode2.beam pg_async2.beam
	rm -f core erl_crash.dump
	rm -f *~

pg_async2.o  pg_encode2.o: pg_encode2.h

pg_sync.o pg_async.o pg_encode.o: pg_encode.h


pg_async2.so: pg_encode2.o

pg_sync.so pg_async.so: pg_encode.o

pg_async2.so: pg_async2.o
	$(CC) $(CFLAGS) pg_encode2.o -shared $< $(EI_LIB) $(PQ_LIB) -o $@

%.so: %.cc
	$(CXX) $(CXXFLAGS) $< -shared -o $@

%.so: %.o
	$(CC) $(CFLAGS) pg_encode.o -shared $< $(EI_LIB) $(PQ_LIB) -o $@

%: %.cc
	$(CXX) $(CXXFLAGS) $< -o $@
