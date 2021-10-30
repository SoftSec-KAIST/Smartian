BUILDDIR=$(shell pwd)/build

all: EVMAnalysis

EVMAnalysis:
		@mkdir -p $(BUILDDIR)
		@dotnet build -c Release -o $(BUILDDIR) src/EVMAnalysis.fsproj

clean:
		@dotnet clean -c Debug
		@dotnet clean -c Release
		@rm -rf $(BUILDDIR)

.PHONY: EVMAnalysis all clean

