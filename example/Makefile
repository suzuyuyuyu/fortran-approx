# environment
FC:=ifort
FLINK:=$(FC)
FFLAGS:=-O3 -xHost
.PHONY: clean

TARGET:=main
SRC:=main.f90
OBJS:=$(SRC:.f90=.o)

####################################################
# module information
# MODULE_DIR - BUILD_DIR should be changed according to your environment
MODULE_DIR:=../../../fortran-approx
INC_DIR:=.
BUILD_DIR:=.
MODULE_NAME:=approx
LIB_FILE:=$(BUILD_DIR)/lib$(MODULE_NAME).a
ABS_BUILD_DIR=$(abspath $(BUILD_DIR))
ABS_INC_DIR=$(abspath $(INC_DIR))
ABS_MODULE_DIR=$(abspath $(MODULE_DIR))
MODULE_LIB_PATH:=-L$(ABS_BUILD_DIR)
MODULE_LIB_FLAG:=-l$(MODULE_NAME)
PASS_ARGS:=FC="$(FC)" FFLAGS="$(FFLAGS)" INC_DIR="$(ABS_INC_DIR)" BUILD_DIR="$(ABS_BUILD_DIR)" MODULE_DIR="$(ABS_MODULE_DIR)"
####################################################


####################################################
# Add $(MODULE_LIB_PATH) and $(MODULE_LIB_FLAG) to the linking command
#   and $(LIB_FILE) to the requirements.
$(TARGET): $(LIB_FILE) $(OBJS)
	@$(FLINK) $(FFLAGS) $(MODULE_LIB_PATH) -o $@ $(OBJS) $(MODULE_LIB_FLAG)
####################################################

%.o: %.f90
	@$(FC) $(FFLAGS) -I$(INC_DIR) -c $< -o $@

####################################################
# Module compilation
LIB_SRCS := $(wildcard $(ABS_MODULE_DIR)/src/*.f90)
$(LIB_FILE): $(LIB_SRCS) $(ABS_MODULE_DIR)/Makefile
	@$(MAKE) -C $(ABS_MODULE_DIR) $(PASS_ARGS)
####################################################

.PHONY: clean
clean: clean-module
	@rm -f *.o $(TARGET)

######################################################
# Add target `clean-module` in the requirements of `clean`
clean-module:
	@cd $(ABS_MODULE_DIR) && $(MAKE) clean $(PASS_ARGS)
####################################################
