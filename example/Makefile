# environment
FC:=ifort
FLINK:=$(FC)
FFLAGS:=-O3 -xHost
.PHONY: clean

TARGET:=run
SRC:=main.f90

INC_DIR:=./include
LIB_DIR:=./lib
OBJ_DIR:=./build
MODULE_NAME:=approx
LIB_FILE:=$(BUILD_DIR)/lib$(MODULE_NAME).a
MODULE_LIB_PATH:=-L$(LIB_DIR)
MODULE_LIB_FLAG:=-l$(MODULE_NAME)

OBJS:=$(OBJ_DIR)/$(SRC:.f90=.o)

####################################################
# Add $(MODULE_LIB_PATH) and $(MODULE_LIB_FLAG) to the linking command
#   and $(LIB_FILE) to the requirements.
$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) $(MODULE_LIB_PATH) -o $@ $(OBJS) $(MODULE_LIB_FLAG)
####################################################

$(OBJ_DIR)/%.o: %.f90 | _MKDIR
	$(FC) $(FFLAGS) -I$(INC_DIR) -c $< -o $@

_MKDIR:
	@mkdir -p $(OBJ_DIR)

.PHONY: clean

clean:
	@rm -f *.o $(TARGET)
