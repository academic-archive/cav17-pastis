LLC = llvm-config

BIN = llvm-reader

CXXFLAGS += `${LLC} --cxxflags`
LDFLAGS += `${LLC} --ldflags --system-libs --libs bitreader core support`

all: ${BIN}

${BIN}: ${BIN}.o
	${CXX} $^ ${LDFLAGS} -o $@

%.o: %.cpp
	${CXX} ${CXXFLAGS} -c $^

clean:
	rm *.o ${BIN}

.PHONY: all clean
