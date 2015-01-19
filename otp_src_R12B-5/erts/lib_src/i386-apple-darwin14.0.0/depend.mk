# Generated dependency rules
# 
# ethread lib objects...
$(r_OBJ_DIR)/ethread.o: common/ethread.c \
  /Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/$(TARGET)/config.h \
  ../include/internal/ethread.h ../include/internal/x86_64/ethread.h \
  ../include/internal/x86_64/../i386/ethread.h \
  ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
  ../include/internal/i386/rwlock.h
# erts_internal_r lib objects...
$(r_OBJ_DIR)/erl_printf_format.o: common/erl_printf_format.c \
  /Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/$(TARGET)/config.h \
  ../include/internal/erl_printf_format.h ../emulator/beam/sys.h \
  ../emulator/sys/unix/erl_unix_sys.h ../emulator/beam/erl_lock_check.h \
  ../emulator/beam/erl_smp.h ../emulator/beam/erl_threads.h \
  ../include/internal/ethread.h ../include/internal/x86_64/ethread.h \
  ../include/internal/x86_64/../i386/ethread.h \
  ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
  ../include/internal/i386/rwlock.h ../emulator/beam/erl_lock_count.h \
  ../emulator/beam/erl_term.h ../include/internal/erl_printf.h
$(r_OBJ_DIR)/erl_printf.o: common/erl_printf.c \
  /Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/$(TARGET)/config.h \
  ../include/internal/erl_printf.h \
  ../include/internal/erl_printf_format.h
$(r_OBJ_DIR)/erl_misc_utils.o: common/erl_misc_utils.c \
  /Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/$(TARGET)/config.h \
  ../include/internal/erl_misc_utils.h
# erts_internal lib objects...
$(OBJ_DIR)/erl_printf_format.o: common/erl_printf_format.c \
  /Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/$(TARGET)/config.h \
  ../include/internal/erl_printf_format.h ../emulator/beam/sys.h \
  ../emulator/sys/unix/erl_unix_sys.h ../emulator/beam/erl_lock_check.h \
  ../emulator/beam/erl_smp.h ../emulator/beam/erl_threads.h \
  ../include/internal/erl_printf.h
$(OBJ_DIR)/erl_printf.o: common/erl_printf.c \
  /Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/$(TARGET)/config.h \
  ../include/internal/erl_printf.h \
  ../include/internal/erl_printf_format.h
$(OBJ_DIR)/erl_misc_utils.o: common/erl_misc_utils.c \
  /Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/erts/$(TARGET)/config.h \
  ../include/internal/erl_misc_utils.h
# erts_r lib objects...
$(r_OBJ_DIR)/erl_memory_trace_parser.o: common/erl_memory_trace_parser.c \
  ../include/erl_memory_trace_parser.h \
  ../include/erl_fixed_size_int_types.h \
  ../include/$(TARGET)/erl_int_sizes_config.h \
  ../include/internal/erl_memory_trace_protocol.h
# erts lib objects...
$(OBJ_DIR)/erl_memory_trace_parser.o: common/erl_memory_trace_parser.c \
  ../include/erl_memory_trace_parser.h \
  ../include/erl_fixed_size_int_types.h \
  ../include/$(TARGET)/erl_int_sizes_config.h \
  ../include/internal/erl_memory_trace_protocol.h
# EOF
