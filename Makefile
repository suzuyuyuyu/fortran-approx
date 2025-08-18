####################################################
#> OUT:
#    $(BUILD_DIR)/approx.o, $(INC_DIR)/approx.mod, $(BUILD_DIR)/libapprox.a
####################################################
SRC_DIR?=src
INC_DIR?=include
BUILD_DIR?=build

OBJS:=$(BUILD_DIR)/approx.o
LIB:=$(BUILD_DIR)/libapprox.a
MOD:=$(INC_DIR)/approx.mod

FC?=gfortran
FFLAGS:=-O3
LD:=ar
LDFLAGS:=rcs

ifeq ($(filter $(FC),mpiifort ifort mpifort),$(FC))
	POINT_INC_DIR:=-module $(INC_DIR)
else
	POINT_INC_DIR:=-J$(INC_DIR)
endif


.PHONY: all clean initialize

all: $(OBJS)
	@$(LD) $(LDFLAGS) $(LIB) $(OBJS)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.F90 | initialize
	@$(FC) $(FFLAGS) $(POINT_INC_DIR) -c $< -o $@

initialize:
	@mkdir -p $(INC_DIR) $(BUILD_DIR)

clean:
	@rm -f $(OBJS) $(MOD) $(LIB)
	@rmdir $(INC_DIR) 2>/dev/null || true
	@rmdir $(BUILD_DIR) 2>/dev/null || true
