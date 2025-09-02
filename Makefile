####################################################
#> OUT:
#>   $(STATIC_OBJ_DIR)/approx.o
#>   $(SHARED_OBJ_DIR)/approx.o
#>   $(INC_DIR)/approx.mod
#>   $(BUILD_DIR)/libapprox.a
#>   $(BUILD_DIR)/libapprox.so
####################################################
SRC_DIR:=./src
INC_DIR:=./example/include
LIB_DIR:=./example/lib
STATIC_OBJ_DIR:=./build
SHARED_OBJ_DIR:=./build
LIB_NAME:=approx

FC:=ifort
FFLAGS:=-O3
LD:=ar
LDFLAGS:=rcs

DO_GENERATE_STATIC:=1
DO_GENERATE_SHARED:=0

STATIC_BUILD_DIR:=$(LIB_DIR)
STATIC_OBJS:=$(STATIC_OBJ_DIR)/$(LIB_NAME).o
STATIC_LIB:=$(STATIC_BUILD_DIR)/lib$(LIB_NAME).a
SHARED_BUILD_DIR:=$(LIB_DIR)
SHARED_OBJS:=$(SHARED_OBJ_DIR)/$(LIB_NAME).o
SHARED_LIB:=$(SHARED_BUILD_DIR)/lib$(LIB_NAME).so
MOD:=$(INC_DIR)/$(LIB_NAME).mod

TARGET:=
DO_NOTICE_MESSAGE:=0
ifeq ($(DO_GENERATE_STATIC),1)
	TARGET+=BUILD_STATIC_LIB
	ifeq ($(DO_GENERATE_SHARED),1)
		ifeq ($(STATIC_BUILD_DIR),$(SHARED_BUILD_DIR))
			DO_NOTICE_MESSAGE:=1
		endif
	endif
endif
ifeq ($(DO_GENERATE_SHARED),1)
	TARGET+=BUILD_SHARED_LIB
endif

ifeq ($(TARGET),)
	$(error "No target specified")
endif

ifeq ($(filter $(FC),mpiifort ifort),$(FC))
	POINT_INC_DIR:=-module $(INC_DIR)
else
	POINT_INC_DIR:=-J$(INC_DIR)
endif

.PHONY: all clean _MKDIR

all: BUILD_LIB _OUTPUT_LOG

BUILD_LIB: $(TARGET)

BUILD_STATIC_LIB: $(STATIC_OBJS)
	$(LD) $(LDFLAGS) $(STATIC_LIB) $(STATIC_OBJS)

BUILD_SHARED_LIB: $(SHARED_OBJS)
	$(FC) -shared -o $(SHARED_LIB) $(SHARED_OBJS)

$(STATIC_OBJ_DIR)/%.o: $(SRC_DIR)/%.F90 | _MKDIR
	$(FC) $(FFLAGS) $(POINT_INC_DIR) -c $< -o $@

$(SHARED_OBJ_DIR)/%.o: $(SRC_DIR)/%.F90 | _MKDIR
	$(FC) -fPIC $(FFLAGS) $(POINT_INC_DIR) -c $< -o $@

_OUTPUT_LOG:
	@echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
	@echo "Build completed successfully"
	@echo "---"
	@echo -e ":- CONFIG"
	@echo -e "    - FC      = \e[35m$(FC)\e[0m"
	@echo -e "    - FFLAGS  = \e[35m$(FFLAGS)\e[0m"
	@echo -e "    - INC_DIR = \e[35m$(abspath $(INC_DIR))\e[0m"
	@echo -e "    - LIB_DIR = \e[35m$(abspath $(LIB_DIR))\e[0m"
	@echo -e ":- CREATED"
	@if [ "$(DO_GENERATE_STATIC)" = "1" ] && [ -f "$(STATIC_LIB)" ]; then echo -e "    - \e[36m$(abspath $(STATIC_LIB))\e[0m"; fi
	@if [ "$(DO_GENERATE_SHARED)" = "1" ] && [ -f "$(SHARED_LIB)" ]; then echo -e "    - \e[36m$(abspath $(SHARED_LIB))\e[0m"; fi
	@if [ -f "$(MOD)" ]; then                                             echo -e "    - \e[36m$(abspath $(MOD))\e[0m"; fi
	@if [ "$(DO_NOTICE_MESSAGE)" = "1" ]; then echo -e ":- \e[31mNOTICE\e[0m"; fi
	@if [ "$(DO_NOTICE_MESSAGE)" = "1" ]; then echo -e "    Both Archive Files and Shared Object Libraries Have Been Created."; fi


_MKDIR:
	@mkdir -p $(INC_DIR) $(STATIC_BUILD_DIR) $(SHARED_BUILD_DIR) $(STATIC_OBJ_DIR) $(SHARED_OBJ_DIR)

clean:
	@rm -f $(STATIC_OBJS) $(SHARED_OBJS) $(MOD) $(STATIC_LIB) $(SHARED_LIB)
	@$(call DELETE_EMPTY_DIR, $(INC_DIR))
	@$(call DELETE_EMPTY_DIR, $(LIB_DIR))
	@$(call DELETE_EMPTY_DIR, $(STATIC_OBJ_DIR))
	@$(call DELETE_EMPTY_DIR, $(SHARED_OBJ_DIR))

define DELETE_EMPTY_DIR
	[ -d $(1) ] && [ -z "$$(find $(1) -type f -print -quit)" ] && rm -rf $(1) 2>/dev/null || true
endef
