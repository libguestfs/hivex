# Tests not to run as part of "make distcheck".
local-checks-to-skip =			\
  sc_prohibit_strcmp			\
  sc_two_space_separator_in_usage	\
  sc_prohibit_magic_number_exit		\
  sc_file_system			\
  sc_GPL_version			\
  sc_bindtextdomain			\
  sc_makefile_path_separator_check	\
  sc_program_name

gnulib_dir = .gnulib

exclude_file_name_regexp--sc_trailing_blank = \
  ^sh/hivexsh\.pod$$

exclude_file_name_regexp--sc_prohibit_empty_lines_at_EOF = \
  ^images/minimal$$

exclude_file_name_regexp--sc_useless_cpp_parens = \
  ^lib/gettext\.h$$
